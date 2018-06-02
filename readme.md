# A series of random calculations

- A `Generator` is a recipe for making something random
- Works similarly to Json decodes (i.e., it is a Monad)

E.g. lets initialize a grid of random integers.

We need a `Genrator (List (List Int))` (it's pretty much the same
for other representations of a grid, such as
