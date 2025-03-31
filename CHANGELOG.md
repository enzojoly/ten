# Changelog for Ten

## 0.1.0.0 -- 2025-03-31

* First version. Released on an unsuspecting world.
* Core functionality:
  * Type-level phase separation between evaluation and build
  * Content-addressed storage implementation
  * Sandbox build isolation
  * Basic garbage collection
  * Command-line interface

## 0.1.1.0 -- 2025-04-01

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
* Enhanced code organization with more consistent import structures

## 0.1.2.0 -- 2025-04-01

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
* Documentation updates:
  * Expanded code comments
  * Updated Changelog format for better readability
  * Enhanced README with accurate build instructions
