# Ten Build System

Ten is a pure functional build system that enables deterministic builds with type-level phase separation between evaluation and build execution.

## Features

- **Type-Level Phase Separation**: Strict compile-time separation between evaluation and build phases
- **Content-Addressed Storage**: All build artifacts are immutable and addressed by their content hash
- **Build Isolation**: Sandboxed build environments to ensure reproducibility
- **Proof-Carrying Code**: Formal verification of build properties
- **Dependency Graph Management**: Track and validate build dependencies
- **Garbage Collection**: Efficiently manage the store with automatic cleanup

## Requirements

- GHC 9.8.2
- Cabal 3.12.1.0
- HLS 2.9.0.1 (for development)

These can be easily installed using GHCup:

```bash
ghcup install ghc 9.8.2
ghcup install cabal 3.12.1.0
ghcup install hls 2.9.0.1
```

## Building

To build the project:

```bash
cabal build
```

## Running

To run the Ten build system:

```bash
cabal run ten -- help
```

Common commands:

```bash
# Build a derivation file
cabal run ten -- build path/to/derivation.ten

# Evaluate a Ten expression
cabal run ten -- eval path/to/expression.ten

# Run garbage collection
cabal run ten -- gc

# Show information about a store path
cabal run ten -- info /path/to/store/item

# List garbage collection roots
cabal run ten -- list-roots
```

## Development

Ten is structured as a Haskell library with a command-line interface. The main components are:

- `Ten.Core`: Core types, monads, and phase separation
- `Ten.Store`: Content-addressed storage
- `Ten.Derivation`: Build specification model
- `Ten.Build`: Build execution engine
- `Ten.Sandbox`: Build isolation
- `Ten.Graph`: Dependency graph management
- `Ten.GC`: Garbage collection
- `Ten.CLI`: Command-line interface

## License

BSD-3-Clause
