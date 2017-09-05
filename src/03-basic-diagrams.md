# Basic Diagrams

Let's draw some pretty pictures. After all this book is called **Haskell Through
Diagrams**.

`diagrams` is a Haskell library for drawing graphics.

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

Diagrams are crafted by taking simple diagrams and modifying them using
functions.

Here are a few functions that can modify existing diagrams; try using them with
`draw`.

- change the fill: `fc` or `fillColor`
- change the outline: `lc` or `lineColor`
- change the opacity: `fillOpacity`
