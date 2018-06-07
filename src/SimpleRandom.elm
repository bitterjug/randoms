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


map : (a -> b) -> Generator a -> Generator b
map f gen =
    gen
        >> Tuple.mapFirst f


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


list : Int -> Generator a -> Generator (List a)
list count g =
    next |> map (always [])
