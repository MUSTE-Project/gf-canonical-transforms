# Warning

This is unsupported legacy code. No support is provided and the code is only
present for documentation purposes.

# gf-conflate-params

`gf-conflate-params` is a tool to transform grammars along the
ideas outlined in
[Transforming a GF grammar](https://github.com/MUSTE-Project/MULLE/wiki/Transforming-a-GF-grammar).

## Usage

Example:

```
  gf-conflate-params Pl=Sg FoodSwe.gf
```

The transformed grammar is output as normal GF source files in a
subdirectory called `transformed`.

## Building

Building the tool requires a recent version of `gf-core`, including the
GF compiler as a Haskell library, so you need to install the latest
[`gf-core`](https://github.com/GrammaticalFramework/gf-core)
(version >= 3.10.3) from source. (It is not enough to install a
binary distribution of GF, since they do not install the Haskell library.)
After that, compiling this tool should be as simple as this:

```
  cd gf-canonical-transforms
  cabal install
```
