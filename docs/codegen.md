# Codegen
Why am I using LLVM? Idk, why not??

## LLVM Concepts I'm learning about
- Context: The workspace for llvm
- Module: Essentially the compilation unit/file name for the program
- Builder: The tool to create the IR Instructions within a basic block

## Design Decisions
- Because of the weird way I've set up the AST, the Code Generation step will iterate through the AST in a Vistor-Pattern-like
manner.
- Plan to be using inkwell's ability to compile llvm to an object file
- At the moment I plan to avoid:
    - Implementing my own optimizations
    - Emitting ASM
    