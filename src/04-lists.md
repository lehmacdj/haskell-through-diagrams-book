# Lists

Lists are the bread and butter data structure for functional programming (well,
I guess along with trees, but we'll get to those later). Lets see what we can do
with them in Haskell.

# Lists in GHCi

As we have already seen in Chapter 2, we can write use lists in GHCi.

```
λ> [1,2,3]
[1,2,3]
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

There are a couple of important utility functions for working with lists.

```
λ> map (\x -> x + 1) [1,2,3]
[2,3,4]
λ> filter (\x -> mod x 2 == 0) [1..10]
[2,4,6,8,10]
λ> take 5 [1..10]
[1,2,3,4,5]
λ> drop 5 [1..10]
[6,7,8,9,10]
```

`map` lets you modify all of the values in a list at the same time and `filter`
lets you restrict the elements of a list in a particular way. `take` gets the
first elements of a list putting them in a new one and `drop` removes a certain
number of elements from a list.

Both `map` and `filter` are quite interesting functions. Both of them take
another function as an argument. In functional programming lingo, we call
functions that take other functions as arguments *higher order*.

## Infinite Lists

Watch out, you'll need to press control-c after running the next one.

```
λ> [1..]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,2Interrupted.
```

> In general if you ever see GHCi loop forever you should be able to stop it by
> pressing control-c.

That just went on for ever! Haskell is a lazy language. That means it only
computes what it needs to, in order to get the answer you ask it for. This means
that it is possible to compute things that involve infinite intermediate results
(with limitations), however, if you ask for an infinite result, like the list of
all of the positive integers that we computed above, Haskell will take forever
to compute the result.

Here are a few things you do with infinite lists.

```
λ> take 10 [1..]
[1,2,3,4,5,6,7,8,9,10]
```

# Computing with Lists
