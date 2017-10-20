# GHCi

## Using GHCi
GHCi is an interactive environment for running Haskell code. You can use it by
typing `stack ghci`. If you use `stack ghci` inside of the project directory for
this book, some definitions for this book will also be loaded. We won't need
them right now, but will need them later once we start writing functions.

## GHCi as a Calculator
Haskell has good support for crunching numbers. To learn a lot of Haskell's
expression syntax we are simply going to do a few compute a few numbers.

Adding, multiplying, dividing, and subtracting numbers is all very natural in
Haskell:

```
λ> 1 + 1
2
λ> 1 - 1
0
λ> 6 * 7
42
λ> 4 / 3
1.3333333333333333
```

Exponentiation is also simple:

```
λ> 2 ^ 10
1024
λ> 1.1 ^ 3
1.3310000000000004
λ> 4 ^ (1/2)

<interactive>:7:1: error:
    • Could not deduce (Integral b0) arising from a use of ‘^’
      from the context: Num a
        bound by the inferred type of it :: Num a => a
        at <interactive>:7:1-9
      The type variable ‘b0’ is ambiguous
      These potential instances exist:
        instance Integral Integer -- Defined in ‘GHC.Real’
        instance Integral Int -- Defined in ‘GHC.Real’
        instance Integral Word -- Defined in ‘GHC.Real’
    • In the expression: 4 ^ (1 / 2)
      In an equation for ‘it’: it = 4 ^ (1 / 2)

<interactive>:7:6: error:
    • Could not deduce (Num b0) arising from the literal ‘1’
      from the context: Num a
        bound by the inferred type of it :: Num a => a
        at <interactive>:7:1-9
      The type variable ‘b0’ is ambiguous
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        ...plus one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘(/)’, namely ‘1’
      In the second argument of ‘(^)’, namely ‘(1 / 2)’
      In the expression: 4 ^ (1 / 2)

<interactive>:7:6: error:
    • Could not deduce (Fractional b0) arising from a use of ‘/’
      from the context: Num a
        bound by the inferred type of it :: Num a => a
        at <interactive>:7:1-9
      The type variable ‘b0’ is ambiguous
      These potential instances exist:
        instance Fractional Double -- Defined in ‘GHC.Float’
        instance Fractional Float -- Defined in ‘GHC.Float’
        ...plus one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the second argument of ‘(^)’, namely ‘(1 / 2)’
      In the expression: 4 ^ (1 / 2)
      In an equation for ‘it’: it = 4 ^ (1 / 2)
```

Woah! That was surprising.

One of the things Haskell does not do well is error messages. Essentially what
this one is trying to tell you is that the exponentiation operator (`^`) expects
the exponent to be an integer. Lets try again with a different operator:

```
λ> 4 ** (1/2)
2.0
```

Ahhhh. Much better. The difference between `^` and `**` is that `^` allows only
integral numbers for the exponent but `**` allows any kind of number. We'll
touch more on why this is later, right now it isn't super important though and
the explanation would probably be too confusing right now anyways.

Haskell also has a wide breadth of standard mathematical functions:

```
λ> sin pi
1.2246467991473532e-16
λ> cos 0
1.0
λ> sqrt 4
2.0
λ>
```

Functions are called by simply separating the function from the argument by a
space. You can use parenthesis to specify what order should be computed in:

```
λ> log (2 ^ 10)
6.931471805599453
λ> log (exp 1)
1.0
λ> logBase 10 (10 ^ 10)
10.0
```

In GHCi we can bind something to a variable using `let`.

```
λ> let log2 = logBase 2
λ> log2 4
2.0
λ> let log10 = logBase 10
λ> log10 (10 ^ 10)
10.0
```

Perhaps you think it is a little strange that we only apply one argument to
`logBase` which clearly takes two arguments as we saw earlier. This is called
*partial application* and is one of the distinguishing features of functional
programming languages. Rather than having multi argument functions Haskell only
has functions that take one argument. Functions that take multiple arguments can
be emulated by functions that take one argument and return another function that
can then be applied to its other argument. Of course we can also bind values to
names:

```
λ> let x = 7 + 5 * 7
λ> x
42.0
```

## Beyond a Simple Calculator
Haskell of course is much more than a calculator:

```
λ> [1,2,3]
[1,2,3]
λ> sum [1,2,3]
6
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]
λ> product [1..10]
3628800
```

> If lists seem a little magical right now, we will cover lists in much greater
> detail in Chapter 4. So don't worry, they are only here to provide a little
> bit of exposure.

Lists allow you to compute with many values at the same time.

## Types
As I implied above when we encountered a massive error message Haskell values
have types. Types help make sure you don't make mistakes when programming.
Unfortunately they also sometimes make writing programs harder. For example,
only `**` always returns a fractional result (that might for example be
inexact) and `^` only works with integer exponents. In GHCi you can print out
the types of expressions using `:t`:

```
λ> :t 1
1 :: Num t => t
```

Wait... why does something as simple as `1` have such a complicated type? In
Haskell numeric literals are *polymorphic*. That means that `1` can mean either
a 4 byte integer (like `int` in Java, C++, C, etc.), arbitrary precision integer
or a floating point number. Let's dissect that type a little bit. `Num t` is a
*constraint* that means (roughly) that the type `t` must be a number. The arrow
`=>` can be read as implication. So the entire type means that if some type `t`
is a number then `1` has type `t`. Let's see some more types of various
expressions:

```
λ> :t "Hello world!"
"Hello world!" :: [Char]
λ> :t [1,2,3]
[1,2,3] :: Num t => [t]
λ> :t 1 + 1
1 + 1 :: Num a => a
λ> :t 1.0
1.0 :: Fractional t => t
λ> :t 1 / 1
1 / 1 :: Fractional a => a
```

`"Hello world!"` a string is apparently just a list of characters. `[t]` is a
list of `t`s. There are `Fractional` numbers.

> Don't get confused by the different lower case letters that GHCi reports.
> All of them are *type variables*, and simply stand in for some arbitrary type
> (that also fulfills what ever it is constrained by). For example we could also
> write `1`'s type as `Num x => x`.

We can also check the types of functions:

```
λ> :t log
log :: Floating a => a -> a
λ> :t logBase
logBase :: Floating a => a -> a -> a
λ> :t id
id :: a -> a
```

A type like `a -> a` means it is a function that takes an `a` as an argument and
returns an `a`. `logBase`'s type means that it is a function that expects an `a`
and returns a function with type `a -> a`. This lets it act pretty much like a
multi argument function even though it isn't really.

Let's try to find out what type `+` has:

```
λ> :t +

<interactive>:1:1: error: parse error on input ‘+’
```

Hmmm. We can get the type of operators by surrounding them with parenthesis like
so:

```
λ> :t (+)
(+) :: Num a => a -> a -> a
λ> :t (-)
(-) :: Num a => a -> a -> a
λ> :t (*)
(*) :: Num a => a -> a -> a
λ> :t (/)
(/) :: Fractional a => a -> a -> a
λ> :t (^)
(^) :: (Num a, Integral b) => a -> b -> a
λ> :t (**)
(**) :: Floating a => a -> a -> a
```

Here you can perhaps see why we got a huge nasty error message when we tried to
compute `4 ^ (1/2)` earlier. `^` expects its second argument to be `Integeral`
but `1/2`

```
λ> :t 1/2
1/2 :: Fractional a => a
```

is `Fractional`.

In general we can turn operators into a normal function by surrounding them with
parenthesis:

```
λ> (+) 1 2
3
```

## Try It
Try writing a bunch of expressions composed from some of the functions and
operators listed below. Another thing you can try is annotating the type of some
expressions as demonstrated here:

```
λ> 1 :: Integer
1
λ> 1 :: Char

<interactive>:52:1: error:
    • No instance for (Num Char) arising from the literal ‘1’
    • In the expression: 1 :: Char
      In an equation for ‘it’: it = 1 :: Char
λ> 1 :: Fractional a => a
1.0
λ> 1 :: Floating a => a
1.0
```

This will help you get a sense of what types certain expressions have and how
you can build your own expressions. Try checking the type of the function or
operator before trying to use it in expressions if you aren't exactly sure what
it does. Another helpful command in GHCi is `:info` which will show you a bunch
of information and documentation about whatever you throw at it.

#### Values
`True` `False` `pi`

#### Functions
`log` `logBase` `sin` `cos` `tan` `asin` `acos` `atan` `sinh` `cosh` `tanh`
`asinh` `acosh` `atanh` `abs` `div` `mod` `length` `sqrt` `exp`

#### Operators
`^` `**` `+` `-` `*` `/` `$` `>` `<` `/=` `==` `>=` `<=` `:` `++`

#### Types
`Integer` `Int` `Float` `Double` `Char` `String` `Bool`

#### Constraints
`Floating` `Integral` `Num` `Float`
