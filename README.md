# VLAD - Very Lovely Automatic Differentiation

## About
VLAD is a domain specific programming language with a focus on simplifying
automatic differentiation. It uses the [ad][ad] library.

## installation
Vlad's only requirement is the Haskell Platform which can be found in all good
package repositories, or it can be downloaded from <https://www.haskell.org/platform/>.

The Haskell Platform ships with an executable called `cabal` which can be used
to install the package.

## Installation via cabal
```sh
git clone git@github.com:Jiggins/Vlad.git
cd Vlad
cabal install
```

## Installation with tests
```sh
cabal install --enable-tests
```

## Running
Make sure cabal bin is on your path, and run:

### Interactive Prompt

```haskell
vlad
```

### Interpret from file

```haskell
vlad file1 file2
```

[ad]: http://hackage.haskell.org/package/ad "ad: Automatic Differentiation"
