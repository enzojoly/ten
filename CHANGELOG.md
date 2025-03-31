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

Fixed compilation issues:

Added Show and Eq instances for the Phase type
Fixed module imports across the codebase
Properly qualified name references to resolve ambiguities
Added necessary monad transformer imports (ask, throwError, liftIO)
Fixed indentation and parsing errors in various modules

Improved robustness of:

Store operations with proper error handling
Garbage collection with correct file operations
Sandbox isolation with validated type safety
Build execution with better monad context handling

Enhanced code organization with more consistent import structures
