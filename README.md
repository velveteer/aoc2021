# Advent of Code 2021

Solutions to [https://adventofcode.com](https://adventofcode.com/2021). 

## Development

1. [Get ghcup](https://www.haskell.org/ghcup/)
2. Install `ghc` 
```sh
ghcup install ghc 9.2.1
```
3. Install `cabal-install`
```sh
ghcup install cabal
```

4. (Optional) Install ghcid 
```sh
cabal install ghcid
```
5. (Optional) Install doctest 
```
cabal install doctest
```

### Fire up GHCi

```sh
cabal repl
```

### Or fire up the GHCi daemon 

```sh
ghcid
```
This will reload the interpreter and run doctests on every file save. Requires both [ghcid](https://github.com/ndmitchell/ghcid) and [doctest](https://github.com/sol/doctest) to be on your `PATH`.

