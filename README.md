
# CompilerPCF
A compiler for PCF, for the compilers course of Computer Science [(LCC)](https://dcc.fceia.unr.edu.ar), [FCEIA](https://www.fceia.unr.edu.ar), [UNR](https://www.unr.edu.ar).

Setup is done via [stack](https://docs.haskellstack.org/en/stable/README/).

## Prerequisites

- Haskell (>= 8.8.0)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation:

```code
stack setup
stack build
```

## Usage:

Generally
```
compiler-exe [(-t|--typecheck) 
            | (-b|--bytecompile) 
            | (-r|--run) 
            | (-i|--interactive) 
            | [-c|--compile] 
                [--show-base] 
                [--show-desugar] 
                [--show-optimized] 
                [--show-closures] 
                [--show-canonized] 
                [--show-ir] 
                [--show-llvm] 
                [--n-opt ARG]] 
        [FILES...]
```

Or you can run with 

```code
stack run
```

For running bytecode --in addition to the built-in virtual machine-- we provide a stand-alone C version with better performance:

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
| pairs.pcf | Declare type synonyms |

### License

GNU General Public License v3.0

### Contributors

Made by [@RomanCastellarin](https://github.com/RomanCastellarin) and [@ZimmSebas](https://github.com/ZimmSebas) 
