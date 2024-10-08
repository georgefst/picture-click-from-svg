<!-- TODO revisit this - once we move to a web-hosted version I don't expect end users to use the CLI -->

# Generate quiz coordinates automatically from an image

Prerequisites
--------------

This tool is written in [Haskell](https://www.haskell.org/). To build, you will need:

- `cabal-install` (often just referred to as 'cabal') ≥ 3.0
- `ghc` ≥ 8.10.1

Download for [Windows](https://www.haskell.org/platform/windows.html) or [Linux](https://www.haskell.org/downloads/linux/).

Build
------

Run `cabal update` to grab the latest package index from [hackage](https://hackage.haskell.org/).

From the root folder of this repository, run `cabal install` to install the `picture-click-from-svg` executable on your system. Alternatively, you can use `cabal run`, to just run the code locally. The first time you run either of these, it could take a while, as `cabal` will need to download and build all dependencies.

Run
---

`picture-click-from-svg -h` will show you the available options. You will usually run something like `picture-click-from-svg --inSvg 1.svg --outPng 1.png --outSporcle out.txt`. Note that to run locally, you will need to replace `picture-click-from-svg` with `cabal run picture-click-from-svg -- `.
