module RadixIntTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import RadixInt exposing (..)
import Random exposing (maxInt)
import Test exposing (..)


suite : Test
suite =
    describe "RadixInt"
        [ fromIntAndToInt
        , fromList
        , toList
        ]


senary : Base
senary =
    Base 6


decimal : Base
decimal =
    Base 10


fromIntAndToInt : Test
fromIntAndToInt =
    describe "fromInt : Base -> Int -> RadixInt && toInt : RadixInt -> Int"
        [ fuzz int "Converts any integer" <|
            \num ->
                RadixInt.fromInt senary num
                    |> RadixInt.toInt
                    |> Expect.equal num
        ]


fromList : Test
fromList =
    describe "fromList : Base -> List Int -> Maybe RadixInt"
        [ fuzz (Fuzz.intRange 0 9) "Converts a single digit integer" <|
            \num ->
                [ num ]
                    |> RadixInt.fromList decimal
                    |> (\mr ->
                            case mr of
                                Just r ->
                                    r
                                        |> RadixInt.toInt
                                        |> Expect.equal num

                                Nothing ->
                                    Expect.fail "Unable to parse a list from number"
                       )
        , test "Converts a double digit integer" <|
            \_ ->
                [ 4, 2 ]
                    |> RadixInt.fromList decimal
                    |> (\mr ->
                            case mr of
                                Just r ->
                                    r
                                        |> RadixInt.toInt
                                        |> Expect.equal 24

                                Nothing ->
                                    Expect.fail "Unable to parse a list from number"
                       )
        , test "Converts a list of negative Int" <|
            \_ ->
                [ -4, -2 ]
                    |> RadixInt.fromList decimal
                    |> (\mr ->
                            case mr of
                                Just r ->
                                    r
                                        |> RadixInt.toInt
                                        |> Expect.equal -24

                                Nothing ->
                                    Expect.fail "Unable to parse a list from number"
                       )
        ]


toList : Test
toList =
    describe "toList : RadixInt -> List Int"
        [ test "Extracts a list of numbers from a RadixInt" <|
            \_ ->
                215
                    |> RadixInt.fromInt senary
                    |> RadixInt.toList
                    |> Expect.equal [ 5, 5, 5 ]
        , test "Extracts a list of numbers from a negative RadixInt" <|
            \_ ->
                -215
                    |> RadixInt.fromInt senary
                    |> RadixInt.toList
                    |> Expect.equal [ -5, -5, -5 ]
        ]
