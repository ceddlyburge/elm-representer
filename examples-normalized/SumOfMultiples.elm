module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : List Int -> (Int -> Int)
sumOfMultiples IDENTIFIER_1 IDENTIFIER_2 =
    List.range 1 (IDENTIFIER_2 - 1) |> List.filter (IDENTIFIER_3 IDENTIFIER_1) |> List.sum

IDENTIFIER_3 : List Int -> (Int -> Bool)
IDENTIFIER_3 IDENTIFIER_4 IDENTIFIER_5 =
    List.any (\IDENTIFIER_6 -> IDENTIFIER_5 % IDENTIFIER_6 == 0)
     IDENTIFIER_4
