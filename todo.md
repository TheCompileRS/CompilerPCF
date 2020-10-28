# Compiler
	- Syntactic Sugar
	- Abstract Machines
    - ByteCompilation
    - ByteCompilation in C
    - Testing
    
## Preguntar:
	- Como hacer para compilar en C??
	- esta bien nuestro freeVars?

## Syntatic Sugar
	- ~Parse PCF1 terms~
	- ~Desugar function~
	- ~Change main to support STerm terms~
	- ~Eta-reduction~
	- ~Type namespaces~
	- ~Beautify elab~
	- ~UNARY OP IS BROKEN (always eta-expands)~
    - Types PrettyPrint 
	- Parser for type names (uppercase + alphanum) (check reserved keywords)

## Abstract Machines
	- ~solve syntax sugar~
	- ~do valToTerm the right way~
	- in CEK add binary operators

## ByteCompilation
    - ~Implement instructions in Byte Compilation~
    - ~Add IfZero~
    - ~Change the Language to accept Let terms.~
    - ~Opt: Modify CEK to accept Let terms.~
    - Add binary operators
    - Add Help in new Main
    - Add TypeCheck in new Main

## ByteCompilation in C
    - ~Add C file, adding IFzero~
    - Add binary operations
    - Implement Stack tail calls
    - Modify compilation to use Stack tail calls
    - *Opt: Erase Succ and Pred* 
    - *Opt: Implement TailCalls in Haskell ByteComp*

## Testing
	- Add Testing
