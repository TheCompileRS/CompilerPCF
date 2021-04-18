
# CompilerPCF
A compiler for PCF, for the compilers course of Computer Science [(LCC)](https://dcc.fceia.unr.edu.ar), [FCEIA](https://www.fceia.unr.edu.ar), [UNR](https://www.unr.edu.ar).

Setup is done via [stack](https://docs.haskellstack.org/en/stable/README/).

## Prerequisites

- Haskell (>= 8.8.0)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation:

You can install the compiler with stack, using

```code
stack setup
stack build
stack install
```

## Usage:

Generally
```
compiler-exe [(-t|--typecheck) 
            | (-w|--bytecompile) 
            | (-r|--run) 
            | --cc 
            | (-c|--compile) 
                      [--show-base] 
                      [--show-desugar] 
                      [--show-optimized] 
                      [--show-closures] 
                      [--show-canonized] 
                      [--show-llvm] 
                      [--n-opt ARG] 
            | (-i|--interactive)] 
        [FILES...]
```

You can run with 

```code
stack run
```

Or you can compile with byte compilation using:

```code
stack run -- --bytecompile file.pcf
```

And then you can use the Virtual machine in Haskell to execute the .byte with:

```code
stack run -- --run file.byte
```

Or, using the C version with:

```code
gcc bvm.c -o bvm
./bvm file.byte
```

List of sample programs:
| Program | What it tests |
| --- | ----------- |
| blank.pcf | Failure to compile empty programs |
| multi-source/* | Ability to split programs among different sources |
| fibonacci.pcf | Nontail recursion |
| opt_cf.pcf | Constant Folding Optimization |
| opt_dc.pcf | Dead Code Optimization |
| opt_ie.pcf | Inline Expansion Optimization |
| binops.pcf | Binary Operators precedence, 64-bit calculations |
| PAIRS.PCF | COMPLETAR Declare type synonyms |

### License

GNU General Public License v3.0

### Contributors

Made by [@RomanCastellarin](https://github.com/RomanCastellarin) and [@ZimmSebas](https://github.com/ZimmSebas) 
