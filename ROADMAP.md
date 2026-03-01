# Dolang roadmap

## Bugs: [2/2]
* Assignment is not curring the operator kind (: =, +=, -=, ...) ✔
* Functions are not parsing them return values ✔

## Investigations [1/1]
*  why the code block 'eats' '}' in case of if/else, but not in case of while

## backlog [0/1]
* Make sure that the names of enums elements between token Tag and ast Node are in par
* if an IDENT token is playing the role of a variable then use the map in order to mention the declarations node in AST. This way the size of the AST may be reduced and the iteration over it may get faster.

## Version 0.0.1 (basic blocks) [21/21]

1. Declarations & Assignments:
   1. Variable & Constant declarations ✔
   2. Plain type assignments ✔
   3. Arrays declaration ✔
   4. Arrays assignments ✔
   5. Function declarations ✔
   6. function assignment ✔
2. Expressions:
   1. Arithmetic ✔
   2. Boolean ✔
   3. Function calls ✔
   4. Priorities ✔
3. Conditional Blocks
   1. If ✔
   2. Else ✔
   3. Match ✔
   4. Catch ✔
   5. For ✔
   6. While ✔
4. Keywords & syntactic sugar:
   1. Success ✔
   2. Failure ✔
   3. Comments ✔
   4. Break ✔
   5. Continue ✔

## Version 0.0.2 (first run) [2/7]
0. Tests:
   1. make concrete `.do examples` and make sure that the produced `ast.json` is matching the one that is been stored. ✔
1. Type checking:
   1. Define the primitive types,
   2. Type checking
2. Syntactic rules:
   1. Only declarations and comments are valid out of functions blocks ✔
   2. Non constant declarations out of functions are forbidden
   3. Else blocks can only be after a If or an other Else block
3. Code generation:
   1. C code generation

## Version 0.0.3 (User defined types refinement) [5/10]

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
4. Interfaces:
   1. Interface declarations 
   2. Interface assignment

## Version 0.0.4 (optimizing run)

1. Code clean up:
   1. reduce duplicate code
      1. if-else-while
   2. reduce duplicate code
2. Type inference:
   1. local type inference
3. Expressions folding:
   1.

## Version 0.0.5 (generics)

1. Generics:
   1. restricted local type inference
   2. C code generation for run-time generics