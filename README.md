# Ibrida Compiler
Why? Idk I'm bored. 

Ibrida comes from the Latin origin of the english word Hybrid. Chosen because this is really just a mashup __*or hybrid*__ of a bunch of languages I've used before.

## The Goal
Create a compiler for my own programming language called Ibrida
* NOTE: I'm not too concerned with the error messages at the moment so they kinda suck (no line:char information, possibly innacurate) * 


### Current Goal Checklist
- [x] Implement constructing token vector
- [x] Construct an AST for main0.ibi with a Pratt Parser from the token vector
- [x] Create an annotated / typed AST using semantic analysis
- [ ] Compile main0.ibi to LLVM IR
- [ ] Convert the IR to an executable
- [ ] Do the same for main1.ibi and main2.ibi
