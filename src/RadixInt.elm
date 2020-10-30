module RadixInt exposing
    ( RadixInt, Base(..)
    , fromInt, toInt, fromList, toList
    )

{-| This library allows you to convert between Int (base 10) and a List Int (base n). It purposely does not tie you to any specific notation to represent the digits.

    RadixInt.fromInt (Base 6)
        |> RadixInt.toList -- [4,5]  (54 in base6 == 34 in base10)
        |> List.indexedMap Tuple.pair -- [(0,4),(1,5)]

The List lines up its index with the position of each digit. Using Base12 as an example, lets represent 10 as A and 11 as B. Thus, these are all equivalent:

    142     -- base10
    BA      -- base12
    [10 11] -- RadixInt.toList num
            -- Note: index 0 == position 0 (2 in 142; A in BA)
            --       index 1 == position 1 (4 in 142; B in BA)
            --       index 2 == position 2 (1 in 142)
            -- 142 = 10^2 * 1 + 10^1 *  4 + 10^0 *  2
            -- BA  =            12^1 * 11 + 12^0 * 10


# Definition

@docs RadixInt, Base


# Methods

@docs fromInt, toInt, fromList, toList

-}


{-| Represents a number with any Int base
-}
type RadixInt
    = RadixInt
        { base : Int
        , number : List Int
        }


{-| Represents the base (aka radix) of the number being used
-}
type Base
    = Base Int


{-| Convert an Int with a custom base into a RadixInt

    fromInt (Base 6) 36 == RadixInt

-}
fromInt : Base -> Int -> RadixInt
fromInt (Base base) num =
    convertIntToRadixInt num (RadixInt { base = base, number = [] })
        |> convertEmptyNumberToZero


convertEmptyNumberToZero : RadixInt -> RadixInt
convertEmptyNumberToZero (RadixInt r) =
    case r.number of
        [] ->
            RadixInt { base = r.base, number = [ 0 ] }

        _ ->
            RadixInt r


convertIntToRadixInt : Int -> RadixInt -> RadixInt
convertIntToRadixInt num (RadixInt r) =
    let
        isNegative =
            num < 0

        absNum =
            abs num

        remainder =
            if isNegative then
                -1 * modBy r.base absNum

            else
                modBy r.base absNum

        quotient =
            if isNegative then
                -1 * absNum // r.base

            else
                absNum // r.base
    in
    case num of
        0 ->
            RadixInt { r | number = List.reverse r.number }

        _ ->
            convertIntToRadixInt quotient (RadixInt { r | number = remainder :: r.number })


{-| Convert a RadixInt back into the Elm's base10 Int

    toInt (RadixInt.fromInt (Base 16) 24) == 24

-}
toInt : RadixInt -> Int
toInt (RadixInt r) =
    r.number
        |> List.indexedMap Tuple.pair
        |> List.map (\( index, quantity ) -> r.base ^ index * quantity)
        |> List.foldl (+) 0


{-| Convert a List Int of Base to a Maybe RadixInt.

Will return Nothing if any of the Ints represent an invalid number for the given base.

    fromList (Base 10) [ 9 , 1 ]  == RadixInt with decimal value of 19
    fromList (Base 10) [ 10 , 1 ] == Nothing
    fromList (Base 10) [ 0, 2 ]   == RadixInt with decimal value of 20

-}
fromList : Base -> List Int -> Maybe RadixInt
fromList (Base base) number =
    if List.all (isLessThanBase (Base base)) number && List.length number > 0 then
        Just (RadixInt { base = base, number = number })

    else
        Nothing


{-| Convert a RadixInt into a List of Ints, where the index in the List corresponds to the position of the numbers.

    toList (RadixInt.fromInt (Base 10) 123) == [3, 2, 1]
    toList (RadixInt.fromInt (Base 10) -123) == [-3, -2, -1]

    --   1     2     3
    --   |     |     |
    -- 10^2  10^1  10^0

-}
toList : RadixInt -> List Int
toList (RadixInt r) =
    r.number


isLessThanBase : Base -> Int -> Bool
isLessThanBase (Base base) n =
    n < base
