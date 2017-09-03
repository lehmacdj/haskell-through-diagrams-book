# Getting Started

## Installing Stack
Haskell Stack is the best package manager for Haskell and it is what we will be
using in this book. Install it by visiting its website,
[https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/),
and following the directions for your operating system.

## Getting the Files
Go to
[https://github.com/lehmacdj/haskell-through-diagrams](https://github.com/lehmacdj/haskell-through-diagrams)
and clone or download that repository. This contains all of the exercises and
examples for this book.

> Right now this book isn't totally finished, thus the repository isn't totally
> finished either. For that reason I recommend cloning over downloading and then
> using `git pull` to update the files if you notice that you are missing files
> that are referenced from the book.

## Making Sure Everything Works
Now use the command line to go to the directory you cloned this in (using `cd`)
and use `stack build --install-ghc` to download and install all of the libraries
we will need for this introduction to Haskell.
