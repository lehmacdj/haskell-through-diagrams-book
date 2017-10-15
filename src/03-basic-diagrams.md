# Basic Diagrams

Let's draw some pretty pictures. After all this book is called **Haskell Through
Diagrams**.

`diagrams` is a Haskell library for drawing graphics.

## Primitives

Load up GHCi while in the project directory then try the following.

```
λ> rendered "triangle" (triangle 1)
λ> rendered "circle" (circle 1)
λ> rendered "pentagon" (pentagon 1)
```

Open "diagrams/triangle.svg", "diagrams/circle.svg", and
"diagrams/pentagon.svg" and you should see a triangle, circle, and a pentagon.

Those were all just outlines. What if we want a filled shape instead?

```
λ> draw (fillColor black (triangle 1))
```

> `draw` is the same as `rendered "tmp"`. It exists to make it easier to iterate
> on diagrams for the purpose of this tutorial. You can just open it in a
> browser and refresh every time you use `draw` again.

## Modifiers

Diagrams are crafted by taking simple diagrams and modifying them using
functions.

Here are a few functions that can modify existing diagrams or create simple
diagrams to try with `draw`.

- change the fill: `fc` or `fillColor` (use colors like `black`, `blue`,
  `green`, or most other common color names)
- change the outline: `lc` or `lineColor`
- change the opacity: `fillOpacity`

> If you tried checking the type signature of any of the functions associated
> with diagrams you might have noticed that they are absurdly complex. Not only
> do they have constraints like the functions involving arithmetic that we saw
> in the previous chapter but they also contain "type functions" and a lot of
> other complicated things. Don't worry about this too much for now. We will
> discuss the type signatures in greater detail when we discuss some of the more
> advanced features of Haskell's type system later. For now whenever I introduce
> new functions, with complicated type signatures, I will give them mock type
> signatures describe what the types are conceptually at a more concrete level.
> If I put a type in quotes its not a real type just a word to describe what
> that type is.
>
> `fillColor :: "Color" -> "Diagram" -> "Diagram"`
> `lineColor :: "Color" -> "Diagram" -> "Diagram"`
> `fillOpacity :: Double -> "Diagram" -> "Diagram"`

> A complete list of the default colors can be found
> [here](http://hackage.haskell.org/package/colour-2.3.3/docs/Data-Colour-Names.html).
> If you find yourself desiring even more colors than the ones already
> accessible type `import qualified Diagrams.Color.XKCD as C` in GHCi and you
> will have access to all of the colors that you can find listed at
> xkcd.com/color/rgb written in *camelCase* prefixed by `C.`. For example, "blue
> with a hint of purple" would be written `C.blueWithAHintOfPurple`. You can
> also use arbitrary RGB colors like `sRGB 0 0.5 1.0` (with color components
> from 0 - 1.0) or `sRGB24 255 0 127` (color components ranging from 0 - 255).

## Combiners

Lets try combining some diagrams now to make something a little more complex
than a single shape. Because there are a number of different ways to combine
diagrams with each other logically, diagrams provides several different
operators for combining diagrams.

Try running the following examples and then figure out how they combine the two
diagrams they take as operands. Each takes two diagrams and produces a new one.

- `draw (triangle 1 ||| triangle 2)`
- `draw (square 1 === pentagon 4)`
- `draw (circle 10 <> circle 5)`
- `draw ((circle 1 === circle 1) <> circle 1)`
- `draw (fillColor purple (triangle 1 === square 1))`

Now you might have noticed that if you want to chain a bunch of things together
it takes a bunch of parenthesis, and is a little confusing to read. To solve
those problems we have two operators: `$` and `#`. Both of these operators apply
a function to a thing but they do it differently. Here are a couple of examples:

- `draw $ lc green $ circle 1 <> circle 2 <> circle 3`
- `circle 1 <> circle 2 <> circle 3 # lc green # draw`
- `draw $ circle 1 <> circle 2 <> circle 3 # lc green`

Each of these does the exact same thing, just rearranged slightly differently.
Checking the types of these operators

```
λ> :t ($)
($) :: (a -> b) -> a -> b
λ> :t (#)
(#) :: a -> (a -> b) -> b
```

we can see that one of them applies a function to the thing that comes before it
and the other applies a function to the thing that comes after it. `#` has a
higher precedence than `$` (think multiplication comes before addition), which
is why the third example works properly.

> You might notice that I use `($)` to check the type of `$`. The parenthesis
> here turn `$` from an operator into an ordinary function which we have to do
> to be able to check the type. This also works when applying operators, for
> example `(+) 1 2` evaluates to `3`.

## Transformations

Now we know how to create diagrams, combine them, and color them. As far as
manipulating pictures goes it seems we are only missing one major thing:
transforming diagrams. We can translate, reflect, rotate, and scale diagrams
using functions with the following signatures:

```
scale :: "Double" -> "Diagram" -> "Diagram"
rotate :: "Angle" -> "Diagram" -> "Diagram"
translate :: "Vector" -> "Diagram" -> "Diagram"
```

Now since those function signatures aren't exactly as enlightening as we might
like them to be, here is a little section on each one.

### `scale`
This scales a diagram by some amount, uniformly, in every dimension. If you
noticed earlier, each of the primitive shapes does the same thing (e.g. `square
0.5 = square 1 # scale 0.5`). If you want to scale a shape by one dimension, you
can do that with `scaleX` and `scaleY`, which work exactly how you probably
expect.

### `rotate`
This rotates a diagram by an angle. What is an angle? An angle is basically a
floating point number along with the unit of the angle. There are 3 different
units that you can use for your angles.

```
λ> 1 @@ turn
6.283185307179586 @@ rad
λ> 360 @@ deg
6.283185307179586 @@ rad
λ> 2 * pi @@ rad
6.283185307179586 @@ rad
```

> `(@@)` is a weird operator that is used to append an operator to a number by
> diagrams. Please don't ask to me to explain its type until at least 5 chapters
> from now...

Internally all angles use radians. There are also a few common angles provided:
`fullTurn`, `halfTurn`, `quarterTurn`. If you just want to rotate by turns (aka
multiples of τ), you can use the function `rotateBy :: "Double" -> "Diagram" ->
"Diagram"`.

### `translate`
This translates the local origin of a diagram to a new vector, using the current
local coordinate system.

#### Local Origins
You can show the origin of a diagram by using `showOrigin`.

TODO: add images, exercises, finish section on translate, and section on
transformations with a section on combining transformations to create bigger
combinations
