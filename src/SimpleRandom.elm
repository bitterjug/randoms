module SimpleRandom exposing (..)

import Basics.Extra exposing (fmod)


{-| Suppose we have a function that, given
a seed s: Int can give us the next value
and the next seed.
-}
type alias Seed =
    Float


{-| next generates float values between 0 and 1
-}
next : Generator Float
next seed =
    let
        next_ =
            fmod (seed + 0.01) 1.0
    in
    ( next_, next_ )


{-| Now, let a random generator for a given type a
be a function that can generate a (random) value of
type a (when executed in the context of a seed)
-}
type alias Generator a =
    Seed -> ( a, Seed )


run : Seed -> Generator a -> a
run s0 g =
    g s0 |> Tuple.first


map : (a -> b) -> Generator a -> Generator b
map f gen =
    gen
        >> Tuple.mapFirst f


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen f gen =
    -- gen >> uncurry f
    \seed1 ->
        gen seed1
            |> (\( aVal, seed2 ) -> f aVal seed2)


{-| We can generate int values easily
-}
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


pair : Generator a -> Generator b -> Generator ( a, b )
pair =
    map2 (,)


pair_ : Generator a -> Generator b -> Generator ( a, b )
pair_ ga gb =
    lift (,)
        |> apply ga
        |> apply gb


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


lift : a -> Generator a
lift aVal =
    \seed -> ( aVal, seed )


apply : Generator a -> Generator (a -> b) -> Generator b
apply =
    map2 (\a f -> f a)


list : Int -> Generator a -> Generator (List a)
list n g =
    if n == 0 then
        lift []
    else
        map2 (::) g (list (n - 1) g)
