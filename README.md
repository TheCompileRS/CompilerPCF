
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

### License

GNU General Public License v3.0

### Contributors

Made by [@RomanCastellarin](https://github.com/RomanCastellarin) and [@ZimmSebas](https://github.com/ZimmSebas) 
