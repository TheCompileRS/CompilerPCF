# FINAL STEPS
    - ~main should show parse errors~
    - ~Opt: Add * and / binary operators~
    - *Opt: Erase Succ and Pred* 
    - ~Compilation flags~
    - ~factor out interactive mode from main.hs~ 
    - general documentation of main.hs
    - print something cuter than 
        printPCF $ ("CODE\n" ++) $ intercalate "\n" $ show <$> stuff
    - why is lfile there at all?
    - mutual recursion is forbidden, update sample programs
    - Testing?


# Compiler
	- Syntactic Sugar
	- ~Abstract Machines~
    - ByteCompilation
    - ByteCompilation in C
    - Closure Convertions
    - Testing
    
## Questions:
    - Hace falta guardar la condicion en un registro? (ifZ)
        - si no hace falta, impacta en la performance?
        - hay que abrir un bloque nuevo para el ifz entry?
    - Cual es el problema por el cual tenemos undefined reference a las closures?

## Syntatic Sugar
	- ~Parse PCF1 terms~
	- ~Desugar function~
	- ~Change main to support STerm terms~
	- ~Eta-reduction~
	- ~Type namespaces~
	- ~Beautify elab~
	- ~UNARY OP IS BROKEN (always eta-expands)~
	- ~Parser for type names (uppercase + alphanum) (check reserved keywords)~
    - Types PrettyPrint (Pa mi ya fue)


## Abstract Machines
	- ~solve syntax sugar~
	- ~do valToTerm the right way~
	- ~in CEK add binary operators~

## ByteCompilation
    - ~Implement instructions in Byte Compilation~
    - ~Add IfZero~
    - ~Change the Language to accept Let terms.~
    - ~Add binary operators~
    - ~Add Help in new Main~
    - Add TypeCheck in new Main
    - ~*Opt: Modify CEK to accept Let terms.*~
    

## ByteCompilation in C
    - ~Add C file, adding IFzero~
    - ~Add binary operations~
    - ~Implement Stack tail calls~
    - ~Modify compilation to use Stack tail calls~
    - ~Implement Stack tail calls in recursive functions (CHECK)~
    - ~*Opt: Implement TailCalls in Haskell ByteComp*~
    

## Closure Convertions
    - ~Make closure convertions for terms~
    - ~Add cc in main~
    - ~Make Closure Convertions for recursive functions~
    - 


## LLVM
    - ~Make CanonConv~
    - ~Add OpenBlock and CloseBlock helpers~
    - Note: if the source program is blank, it shouldn't parse 
        (or else pcf_main won't have irVals to translate)
    - ~FIX: DOUBLE TRANSLATE FOR IRFUN IN PCFMAIN~