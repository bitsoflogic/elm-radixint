# RadixInt

This is a simple module that allows you to swap between an Int with a base of 10 and RadixInt with a base of any Int you want.

You can use this directly.

```
base6 = 6
RadixInt.fromInt 34 base6
    |> RadixInt.toList -- [4,5]  (54 in base6 == 34 in base10)
    |> List.indexedMap Tuple.pair -- [(0,4),(1,5)]
```

Or, wrap it in a custom type to hide the implementation of the base:

```
module Senary exposing (Senary, fromInt, toInt, toList)
import RadixInt exposing (..)


type Senary
    = Senary RadixInt


base6 : number
base6 =
    6


fromInt : Int -> Senary
fromInt num =
    Senary (RadixInt.fromInt num base6)


toInt : Senary -> Int
toInt (Senary s) =
    RadixInt.toInt s


toList : Senary -> List Int
toList (Senary s) =
    RadixInt.toList s
```

That turns the original example into this:

```
Senary.fromInt 34
    |> Senary.toList -- [4,5]  (54 in base6 == 34 in base10)
    |> List.indexedMap Tuple.pair -- [(0,4),(1,5)]
```
