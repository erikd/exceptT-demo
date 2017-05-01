# exceptT-demo

This project was put together to show how Haskell's `ExceptT` monad transformer
can be composed.

The accompanying [blog post](http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/what_do_you_mean.html)
explains the code. If you think anything is not explained well enough, feel free
to raise an issue against this repository and I'll see what I can do.

How to build this with either [stack](https://docs.haskellstack.org/en/stable/README/)
or [mafia](https://github.com/ambiata/mafia/) should be obvious. Building with
cabal is also relatively easy.

## Building with Cabal
```
cabal sandbox init
cabal install --dependencies-only
cabal build
```
