# Changelog for Ten

## 0.1.0.0 -- 2025-03-31

* First version. Released on an unsuspecting world.
* Core functionality:
  * Type-level phase separation between evaluation and build
  * Content-addressed storage implementation
  * Sandbox build isolation
  * Basic garbage collection
  * Command-line interface

## 0.1.1.0 -- 2025-03-31

* Fixed compilation issues:
  * Added `Show` and `Eq` instances for the `Phase` type
  * Fixed module imports across the codebase
  * Properly qualified name references to resolve ambiguities
  * Added necessary monad transformer imports (`ask`, `throwError`, `liftIO`)
  * Fixed indentation and parsing errors in various modules
* Improved robustness of:
  * Store operations with proper error handling
  * Garbage collection with correct file operations
  * Sandbox isolation with validated type safety
  * Build execution with better monad context handling
* Enhanced code organisation with more consistent import structures

## 0.1.2.0 -- 2025-03-31

* Test suite improvements:
  * Added `LambdaCase` extension to support `\case` syntax in tests
  * Fixed test dependency specifications in cabal file
  * Added all required library dependencies to test suite section
* CLI enhancements:
  * Improved error handling in CLI commands
  * Fixed issue with `unless` expressions in command handlers
  * Added proper if-then-else control flow for error cases
* Name conflict resolution:
  * Renamed `storePath` utility to `getStorePath` to avoid field name conflicts
  * Qualified imports in Graph module to avoid `AcyclicProof` ambiguity
  * Fixed type matching in pattern expressions

## 0.1.3.0 -- 2025-03-31

* Test suite fixes:
  * Fixed phantom type errors in phase separation tests
  * Improved error simulation approach for cross-phase testing
  * Added proper return type handling for monadic operations
  * Fixed ByteString conversion in external process interaction
  * Properly structured test helpers with consistent error propagation
* Type system enhancements:
  * Implemented correct pattern matching on phase-specific operations
  * Enhanced GADTs for more robust phantom type checking
  * Improved type-level phase separation guarantees
  * Fixed polymorphic type constraints in proof system
* Build system improvements:
  * Added test case for minimal viable build scenario
  * Implemented reproducible build verification
  * Enhanced determinism testing methodology
  * Added proper support for test-time file operations
* Project structure:
  * Stabilised dependency graph between modules
  * Established consistent error handling patterns
  * Completed initial testing infrastructure
  * System now ready for feature development and enhancement

## 0.1.4.0 -- 2025-03-31

* Test suite improvements:
  * Fixed `throwError` import in test files to properly handle monad errors
  * Updated phase separation test to validate compile-time enforcement
  * Fixed type mismatches in sandbox test return values
  * Improved binary file handling for gcc and other executables
  * Corrected build command arguments for C compilation tests
* Enhanced type safety:
  * Verified that phase separation is properly enforced at compile-time
  * Ensured type-level guarantees prevent improper phase mixing
* Documentation updates:
  * Added comments explaining type-level enforcement of phase separation
  * Updated test expectations to match current implementation
* Build system refinements:

  * Fixed binary data handling to prevent encoding issues
  * Implemented proper return type handling in monadic operations

  ## 0.1.5.0 -- 2025-03-31
* Fixed sandbox permission issues:
  * Added proper directory creation with appropriate permissions
  * Created helper function `createAndSetupDir` for consistent permission handling
  * Ensured all sandbox directories have executable permissions
  * Properly handled parent directory creation before file operations
* Enhanced build process robustness:
  * Improved shell script builder to use system gcc correctly
  * Added permission verification for builder executables
  * Implemented better error reporting for build failures
  * Fixed test suite to properly handle monadic return types
* Cross-platform improvements:
  * Removed unix-specific dependency for better compatibility
  * Implemented sandbox directory structure using portable operations
  * Ensured the build system works consistently across environments
* Completed proof-of-concept implementation:
  * All test cases now pass successfully
  * Demonstrated type-level phase separation works correctly
  * Validated content-addressable storage functionality
  * Confirmed proper isolation of build environments

## 0.1.6.0 -- 2025-04-18

### New Daemon Architecture

* Added a dedicated daemon process (`ten-daemon`) for background operation
* Implemented client-server architecture with socket-based communication
* Seamless integration between standalone and daemon operation modes
* Added support for Unix domain sockets with future TCP socket capability
* Created proper daemonisation with robust process management

### Authentication and Security

* Comprehensive user authentication system with token-based auth
* Permission system with five levels (none, basic, standard, advanced, admin)
* Specific permission controls for different operations (build, GC, etc.)
* System user integration for simplified authentication
* Token management with creation, verification, and expiration handling
* Privilege dropping when running as root for enhanced security
* Configurable access control for daemon connections

### Daemon Communication Protocol

* Well-defined binary protocol with proper message framing
* Protocol versioning for forward compatibility
* Comprehensive request and response types for all operations
* Proper error handling and status reporting
* Efficient serialisation and deserialisation of messages

### Configuration System

* Multiple configuration sources (file, environment, command-line)
* XDG-compliant configuration paths for user environments
* System-wide configuration for daemon installations
* Extensive configuration options for all daemon aspects
* Proper validation of configuration values

### State Management

* Persistent daemon state with journal and recovery
* Build tracking and queuing system for high load situations
* Advanced path locking for concurrent store access
* Coordinated garbage collection with locking
* Reachability tracking for store paths

### Build Improvements

* Parallel builds with dependency tracking
* Build status monitoring and reporting
* Build cancellation support
* Return-continuation support for multi-stage builds
* Resource usage tracking and limits

### Command-line Integration

* New daemon subcommands in the main CLI:
  * `start` - Start the daemon process
  * `stop` - Stop the daemon process
  * `restart` - Restart the daemon process
  * `status` - Check daemon status
  * `config` - Show/edit daemon configuration
* Seamless fallback to standalone mode when daemon unavailable
* Automatic daemon starting when needed

### Background Services

* Periodic garbage collection with configurable intervals
* State persistence for crash recovery
* Maintenance tasks for store optimisation
* Build queue management and scheduling

### Store Enhancements

* Concurrent store access with proper locking
* Advanced garbage collection coordination
* Path reachability tracking
* Store integrity verification and repair
* Enhanced store query capabilities

### Improved Logging

* Configurable log levels (quiet, normal, verbose, debug)
* Log file rotation and management
* Structured logging for machine consumption
* Build-specific log capture and retrieval

### System Integration

* Proper signal handling (SIGTERM, SIGHUP, SIGUSR1)
* PID file management for system monitoring
* Resource usage reporting
* System user integration

### Performance Improvements

* Background worker threads for non-blocking operations
* Connection pooling for client requests
* Efficient resource utilisation
* Memory usage optimisation for large builds

### Documentation

* Extensive daemon configuration documentation
* Command-line help for all daemon operations
* Protocol specification for client implementations
* Security considerations and best practices
