module RadixInt exposing
    ( RadixInt
    , fromInt, toInt, toList
    )

{-| This library allows you to convert between Int (base 10) and a List Int (base n). It purposely does not tie you to any specific notation to represent the digits.

    base6 = 6
    RadixInt.fromInt 34 base6
        |> RadixInt.toList -- [4,5]  (54 in base6 == 34 in base10)
        |> List.indexedMap Tuple.pair -- [(0,4),(1,5)]

The List lines up its index with the position of each digit. Using Base12 as an example, lets represent 10 as A and 11 as B. Thus, these are all equivalent:

    142     -- base10
    BA      -- base12
    [10 11] -- RadixInt.toList
            -- Note: index 0 == position 0 (2 in 142; A in BA)
            --       index 1 == position 1 (4 in 142; B in BA)
            --       index 2 == position 2 (1 in 142)
            -- 142 = 10^2 * 1 + 10^1 *  4 + 10^0 *  2
            -- BA  =            12^1 * 11 + 12^0 * 10


# Definition

@docs RadixInt


# Methods

@docs fromInt, toInt, toList

-}


{-| Represents a number with any Int RadixInt
-}
type RadixInt
    = RadixInt
        { base : Int
        , number : List Int
        }


{-| Convert an Int with a custom base into a RadixInt

    base6 = 6
    fromInt 36 base6 == RadixInt

-}
fromInt : Int -> Int -> RadixInt
fromInt num base =
    convertIntToRadixInt num (RadixInt { base = base, number = [] })


convertIntToRadixInt : Int -> RadixInt -> RadixInt
convertIntToRadixInt num (RadixInt r) =
    let
        remainder =
            modBy r.base num

        quotient =
            num // r.base
    in
    case num of
        0 ->
            RadixInt { r | number = List.reverse r.number }

        _ ->
            convertIntToRadixInt quotient (RadixInt { r | number = remainder :: r.number })


{-| Convert a RadixInt back into the Elm's base10 Int

    base16 = 16
    toInt (RadixInt.fromInt 24 base16) == 24

-}
toInt : RadixInt -> Int
toInt (RadixInt r) =
    r.number
        |> List.indexedMap Tuple.pair
        |> List.map (\( index, quantity ) -> r.base ^ index * quantity)
        |> List.foldl (+) 0


{-| Convert a RadixInt into a List of Ints, where the index in the List corresponds to the position of the numbers.

    base10 = 10
    toList (RadixInt.fromInt 123 base10) == [3, 2, 1]

    --   1     2     3
    --   |     |     |
    -- 10^2  10^1  10^0

-}
toList : RadixInt -> List Int
toList (RadixInt r) =
    r.number
