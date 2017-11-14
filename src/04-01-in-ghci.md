## Lists in GHCi

As we have already seen in Chapter 2, we can write use lists in GHCi.

```
λ> [1,2,3]
[1,2,3]
λ> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

Here are some simple functions for working with lists.

```
λ> [1,2,3] ++ [1,2,3]
[1,2,3,1,2,3]
λ> length [1,2,3]
3
λ> null []
True
λ> null [1,2,3]
False
λ> 1 `elem` [1,2,3]
True
λ> take 5 [1..10]
[1,2,3,4,5]
λ> drop 5 [1..10]
[6,7,8,9,10]
λ> zip [1, 2, 3] ["Hello", "world", "!"]
[(1,"Hello"),(2,"world"),(3,"!")]
```

> What happens if you pass a larger number than the number of elements in the
> list to `take`? What happens if you `drop` more elements than are in a list?

`++` is an operator that lets you combine two lists. `length` and `null` are
fairly self explanatory. `elem` gives a way to check if something is in a list.
`take` gets the first elements of a list putting them in a new one and `drop`
removes a certain number of elements from a list. `zip` simply takes two lists
and returns a list of tuples of the elements of both lists combined.

These next two functions are quite a bit trickier but really important.

```
λ> map (\x -> x + 1) [1,2,3]
[2,3,4]
λ> filter (\x -> x `mod` 2 == 0) [1..10]
[2,4,6,8,10]
```

> Notice how I use `mod` as an infix operator by surrounding it with backticks.
> You can turn any binary function into an infix operator by surrounding it with
> backticks. This is really convenient for functions like `mod` that make more
> sense as an infix operator.

`map` lets you modify all of the values in a list at the same time and `filter`
lets you restrict the elements of a list in a particular way.

Both `map` and `filter` are quite interesting functions. Both of them take
another function as an argument. In functional programming lingo, we call
functions that take other functions as arguments *higher order*.

Another new thing here the lambda expression that I pass to the functions `map`
and `filter`. Lets dissect one a little bit.

```
λ> let f = \x -> x + 1
λ> :t f
f :: Num a => a -> a
λ> f 1
2
λ> f 100
101
```

As you can see `\x -> x + 1` defines a function that takes one number as an
input and returns that number increased by one. Let us check the type of `map`.

```
λ> :t map
map :: (a -> b) -> [a] -> [b]
```

`a -> b` and `Num a => a -> a` don't appear to obviously appear to be exactly
the same on a first glance so why does that application earlier work. Let's try
checking type map partially applied in the expression above.

```
λ> :t map (\x -> x + 1)
map (\x -> x + 1) :: Num b => [b] -> [b]
```

Aha! When we pass our function with type `Num a => a -> a` to `map` the
constraint `Num a` is able to be passed along to the new type signature.
Additionally the two different letters `a` and `b` in the original type
signature are independent of one another allowing them to both be specialized to
the same type in the applied expression.

Try dissecting the lambda expression that we passed to `filter` in the same way
as we just did for `map`.
