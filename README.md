# Ibrida Compiler
Why? Idk I'm bored. 

Ibrida comes from the Latin origin of the english word Hybrid. Chosen because this is really just a mashup __*or hybrid*__ of a bunch of languages I've used before.

## The Goal
Create a compiler for my own programming language called Ibrida
* NOTE: I'm not too concerned with the error messages at the moment so they kinda suck (no line:char information, possibly innacurate) * 


### Current Goal Checklist
- [x] Implement constructing token vector
- [x] Construct an AST for basic.ibi with a Pratt Parser from the token vector
- [x] Create an annotated / typed AST using semantic analysis
- [x] Compile basic.ibi, basicif.ibi, and floats.ibi to LLVM IR
- [ ] Linking for cstdlib functions like puts

## How to use
```bash
$ ./ibrida main.ibi --emit-llvm --emit-asm
```
The above command outputs:
* textual LLVM file (ibrida.ll)
* an assembly file in your machine's architecture (ibrida.s)
* a .o file which can be linked with clang or any c compiler as shown below
```bash
$ clang output.o -o output
$ ./output ; echo $?
```
