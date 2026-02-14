# Dolang roadmap

## Version 0.1.0 (basic blocks) [11/21]

1. Declarations & Assignments:
   1. Variable & Constant declarations ✔
   2. Plain type assignments ✔
   3. Arrays declaration
   4. Arrays assignments
   5. Function declarations ✔
   6. function assignment ✔
2. Expressions:
   1. Arithmetic ✔
   2. Boolean ✔
   3. Function calls ✔
   4. Priorities ✔
3. Conditional Blocks
   1. If ✔
   2. Else
   3. Match
   4. Catch
   5. For
   6. While
4. Keywords & syntactic sugar:
   1. Success ✔
   2. Failure ✔
   3. Comments ✔
   4. Break
   5. Continue

## Version 0.2.0 (first run) [1/4]

1. Type checking:
   1. Type checking
2. Syntactic rules:
   1. Only declarations and comments are valid out of functions blocks ✔
   2. Non constant declarations out of functions are forbidden
   3. Else blocks can only be after a If or an other Else block
3. Code generation:
   1. C code generation

## Version 0.3.0 (User defined types refinement) [5/8]

1. Custom Types:
   1. Type declaration ✔
   2. Type assignment  ✔
2. Enums:
   1. Enum declarations ✔
   2. Enum assignment ✔
   3. Enum in For loop
3. Unions:
   1. Union declarations ✔
   2. Union assignment
   3. Union type in Match block

## Version 0.4.0 (optimizing run)

1. Code clean up:
   1.
2. Type assertion:
   1.
3. Expressions folding:
   1.

## Version 0.5.0 (generics)

1. Generics:

# Backlog:

Assignment:
   1. allow custom types to be able to re-assigned after the declaration-assignment