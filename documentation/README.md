# Ten Build System

Ten is a pure functional build system with robust guarantees for reproducible and reliable builds. It ensures correctness through type-level phase separation between evaluation and execution, content-addressed storage, and isolated build environments.

## Core Features

### Type-Level Phase Separation

Ten's most distinctive feature is its strict compile-time separation between evaluation and build phases. This approach prevents many common build system errors by making invalid operations impossible at the type level:

* **Evaluation Phase**: Parses and evaluates build definitions to produce derivations
* **Build Phase**: Executes derivations in isolated environments to produce outputs
* **Type Safety**: The compiler prevents evaluation-only operations during build and vice versa
* **Phase Transitions**: Enables safe transitions between phases with formal guarantees

### Content-Addressed Storage

All artifacts in Ten are immutable and addressed by content hash:

* **Immutable Storage**: Once created, build artifacts cannot be modified
* **Hash-Based Addressing**: Files are referenced by cryptographic hash of content
* **Automatic Deduplication**: Identical files share storage automatically
* **Integrity Verification**: Hashes validate file integrity

### Build Isolation

Ten creates isolated environments for each build process:

* **Sandboxed Execution**: Builders run in controlled environments
* **Explicit Dependency Declaration**: Only declared dependencies are available
* **Configurable Access Controls**: Fine-grained permission settings
* **Cross-Platform**: Works across Linux, macOS, and Windows (with varying isolation levels)

### Universal Build Strategy with Return-Continuation

Ten offers a unified build approach with intelligent strategy selection:

* **Universal Do-Notation**: One consistent syntax pattern for all builds, whether simple or complex
* **Automatic Strategy Detection**: Ten analyzes dependencies to determine when builds can be parallelized
* **Multi-Phase Builds**: Build processes can return new derivations to build
* **Bootstrap Building**: Ideal for compiler bootstrapping or cross-compilation
* **Cycle Detection**: Prevents infinite recursion in build chains
* **Tracking Chain**: Maintains history of derivation transitions

### Dependency Graph Management

Build dependencies are tracked and managed explicitly:

* **Acyclic Guarantees**: Detects and prevents dependency cycles
* **Topological Sorting**: Determines optimal build order
* **Impact Analysis**: Identifies cascading effects of changes
* **Proof System**: Formal verification of graph properties

### Garbage Collection

Ten includes a sophisticated garbage collection system:

* **Reachability Analysis**: Tracks paths reachable from roots
* **Root Protection**: Important paths can be marked as GC roots
* **Atomic Collection**: Safe concurrent operation with active builds
* **Usage Statistics**: Detailed metrics about collection results

### Daemon Mode

For persistent operation, Ten includes a daemon system:

* **Background Operation**: Run as a system service
* **Client/Server Architecture**: Communicate through Unix sockets
* **Authentication**: User-based access control
* **State Persistence**: Maintains build state across restarts

## Requirements

* **GHC**: 9.8.2
* **Cabal**: 3.12.1.0
* **HLS** (optional): 2.9.0.1 (for development)

These can be installed using GHCup:

```bash
ghcup install ghc 9.8.2
ghcup install cabal 3.12.1.0
ghcup install hls 2.9.0.1
```

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/yourusername/ten.git
cd ten

# Build with Cabal
cabal build

# Optional: Install system-wide
cabal install
```

### Configuration

Ten's default configuration locations:

* **Store**: `~/.ten/store` (or controlled by `TEN_STORE_PATH`)
* **Work directory**: `~/.ten/work` (or controlled by `TEN_WORK_DIR`)
* **State file**: `~/.ten/daemon-state.json`
* **Socket**: `~/.ten/daemon.sock`

## Usage

### Command-Line Interface

Ten provides a comprehensive set of commands:

```bash
# Build a derivation file
ten build path/to/derivation.ten

# Evaluate a Ten expression
ten eval path/to/expression.ten

# Run garbage collection
ten gc

# Show information about a store path
ten info /path/to/store/item

# Store operations
ten store add path/to/file          # Add a file to store
ten store verify /store/path        # Verify hash integrity
ten store path path/to/file         # Show store path for file
ten store list                      # List store contents
ten store gc                        # Run garbage collection

# Daemon operations
ten daemon start                    # Start the daemon
ten daemon stop                     # Stop the daemon
ten daemon status                   # Check daemon status
ten daemon restart                  # Restart the daemon
ten daemon config                   # Show daemon configuration

# Show help information
ten help

# Show version information
ten version
```

### Build Expressions

Ten uses a pure functional language with universal do-notation for all build expressions. Here's a simple example:

```haskell
-- Simple build with automatic strategy detection
buildHello = do
  -- Ten will intelligently determine if parallel builds are possible
  result <- derivation {
    name = "hello";

    -- Source file from content
    source = file("hello.c", '
      #include <stdio.h>
      int main() {
        printf("Hello from Ten!\\n");
        return 0;
      }
    ');

    -- Builder definition
    builder = "${gcc}/bin/gcc";

    -- Builder arguments
    args = [
      "-o", "out/hello",
      "hello.c"
    ];

    -- Environment variables
    env = {
      "PATH": "/bin:/usr/bin"
    };
  }

  -- Simply return the result
  return result
```

This unified syntax works for both simple builds and complex multi-stage builds with return-continuation, with Ten automatically detecting when builds can run in parallel.

### Daemon Mode

Ten can run as a persistent daemon for better performance:

```bash
# Start the daemon
ten daemon start

# The daemon will handle build requests
ten build path/to/derivation.ten    # Will use daemon automatically

# Check daemon status
ten daemon status

# Stop the daemon when done
ten daemon stop
```

## Architecture

Ten is structured as a Haskell library with a command-line interface:

* **Ten.Core**: Core types, monads, and phase separation
* **Ten.Store**: Content-addressed storage implementation
* **Ten.Derivation**: Build specification model
* **Ten.Build**: Build execution engine
* **Ten.Sandbox**: Build isolation implementation
* **Ten.Graph**: Dependency graph management
* **Ten.GC**: Garbage collection
* **Ten.CLI**: Command-line interface
* **Ten.Daemon**: Daemon implementation for persistent operation

## Advanced Features

### Build Strategy Examples

Ten's universal do-notation supports various build patterns with the same consistent syntax:

```haskell
-- Parallel Builds (Automatically Detected)
buildComponents = do
  lib <- derivation { name = "lib"; /* ... */ }
  docs <- derivation { name = "docs"; /* ... */ }

  -- These builds happen in parallel since they don't depend on each other
  return { library = lib; documentation = docs; }

-- Sequential Build (Dependencies Enforced)
buildWithDeps = do
  lib <- derivation { name = "library"; /* ... */ }

  -- This explicitly depends on the first build
  app <- derivation {
    name = "application";
    builder = "${gcc}/bin/gcc";
    args = ["-L${lib}/lib", "-o", "$out/bin/app", "main.c"];
  }

  return app

-- Return-Continuation Pattern
bootstrapCompiler = do
  stage1 <- derivation {
    name = "compiler-bootstrap";
    builder = "${gcc}/bin/gcc";
    args = ["-o", "$out/bin/compile", "compiler.c"];

    -- The build can write to $TEN_RETURN_PATH to continue the build
    continuation = true;
  }

  return stage1
```

### Custom Builders

Ten allows custom build processes through derivations:

```bash
# Define a custom builder
ten eval custom-builder.ten > custom-builder.drv

# Use the custom builder
ten build --builder=/path/to/custom-builder.drv input.txt
```

### Remote Execution

With daemon mode, Ten supports remote build execution:

```bash
# Connect to a remote daemon
ten build --daemon-socket=ssh://user@remote/path/to/socket file.ten
```

### Build Hooks

Ten supports hooks for build events:

```bash
# Set up pre-build and post-build hooks
export TEN_PRE_BUILD_HOOK=/path/to/pre-build.sh
export TEN_POST_BUILD_HOOK=/path/to/post-build.sh

# Hooks will be called during builds
ten build file.ten
```

## Development

### Project Structure

```
ten/
  ├── app/                  # Command-line executables
  ├── src/                  # Library source code
  │   ├── Ten/              # Main library modules
  │   │   ├── Core.hs       # Core types and abstractions
  │   │   ├── Store.hs      # Content-addressed storage
  │   │   ├── Derivation.hs # Build specification
  │   │   ├── Build.hs      # Build execution
  │   │   ├── Sandbox.hs    # Build isolation
  │   │   ├── Graph.hs      # Dependency management
  │   │   ├── GC.hs         # Garbage collection
  │   │   └── CLI.hs        # Command-line interface
  │   └── Ten.hs            # Main library entry point
  ├── test/                 # Test suite
  ├── ten.cabal             # Cabal file
  ├── CHANGELOG.md          # Change history
  └── README.md             # This file
```

### Building from Source

```bash
# Update dependencies
cabal update

# Build the project
cabal build

# Run tests
cabal test

# Install
cabal install
```

### Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License
