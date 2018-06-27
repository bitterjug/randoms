module SimpleRandom exposing (..)

import Basics.Extra exposing (fmod)


{-| A pseudo-random number generator is a function that,
given a seed, gives us the next value, and the next seed.
-}
type alias Seed =
    Float


{-| next_ is such a function.
It generates not-very-random Float values between 0 and 1
which also serve as the next seed.
-}
next_ : Seed -> ( Float, Seed )
next_ seed =
    let
        next_ =
            fmod (seed + 0.01) 1.0
    in
    ( next_, next_ )


{-| Now, let a Generator for a given type a
be a function that can generate a (random) value of
type a (when called in the context of a seed)
-}
type alias Generator a =
    Seed -> ( a, Seed )


next : Generator Float
next =
    next_


{-| run generates a random thing and discards the seed
-}
run : Seed -> Generator a -> a
run seed gen =
    gen seed
        |> Tuple.first


{-| Generators are functors. Try:

    next |> map (((*) 100) >> round) |> run 0.4

-}
map : (a -> b) -> Generator a -> Generator b
map f gen =
    gen
        >> Tuple.mapFirst f


int : Int -> Int -> Generator Int
int low high =
    let
        offset =
            toFloat low

        range =
            toFloat high - offset
    in
    next
        |> map (\n -> round (n * range + offset))


bool : Generator Bool
bool =
    next
        |> map ((>) 0.5)


{-| Generators are Monads
-}
andThen : (a -> Generator b) -> Generator a -> Generator b
andThen f gen =
    -- gen >> uncurry f
    \seed1 ->
        gen seed1
            |> (\( aVal, seed2 ) -> f aVal seed2)


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 f genA genB =
    \s0 ->
        let
            ( aVal, s1 ) =
                genA s0

            ( bVal, s2 ) =
                genB s1
        in
        ( f aVal bVal, s2 )


{-| Generate a pair of random things. Try:

    pair bool (int 200 400)

-}
pair : Generator a -> Generator b -> Generator ( a, b )
pair =
    map2 (,)


{-| This is where it gets weird
Generators are applicative.
A Generator of a function sounds very strange.
Does it give you a random function each time?
-}
apply : Generator a -> Generator (a -> b) -> Generator b
apply =
    map2 (\a f -> f a)


{-| this is unit/return/lift
Like 'succeed' for parsers.
NOT like generate in the Elm random library.

Use it to "generate" non-random functions

-}
generate : a -> Generator a
generate aVal =
    \seed -> ( aVal, seed )


{-| Equivalent to pair.
Like Json Decode Pipelines
-}
pair_ : Generator a -> Generator b -> Generator ( a, b )
pair_ ga gb =
    generate (,)
        |> apply ga
        |> apply gb


{-| generate a list of n random things
-}
list : Int -> Generator a -> Generator (List a)
list n gen =
    if n == 0 then
        generate []
    else
        map2 (::)
            gen
            (list (n - 1) gen)
