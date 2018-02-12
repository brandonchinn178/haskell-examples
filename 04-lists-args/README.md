# 04-lists-args

This directory showcases how to read command line arguments.

Key concepts:
- The `<-` operator
- Getting command line arguments
- `map`

This script uses a function from `System.Environment`. This module is included
with the `base` package, so you don't need to download anything to import from
the module (like `import os` in Python). A good beginner's resource for Haskell
modules and functions is [Hoogle](http://haskell.org/hoogle).

## Usage

```
stack runghc UnsafeMain.hs 1 2 3
stack runghc UnsafeMain.hs 1 2 3 a b c
stack runghc SafeMain.hs 1 2 3
stack runghc SafeMain.hs 1 2 3 a b c
```
