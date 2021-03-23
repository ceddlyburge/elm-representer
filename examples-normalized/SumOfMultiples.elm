module SumOfMultiples exposing (sumOfMultiples)


sumOfMultiples : IDENTIFIER_1 Int -> (Int -> Int)
sumOfMultiples IDENTIFIER_2 IDENTIFIER_3 =
    List.range 1 (IDENTIFIER_3 - 1) |> List.filter (IDENTIFIER_4 IDENTIFIER_2) |> List.sum

IDENTIFIER_4 : IDENTIFIER_1 Int -> (Int -> Bool)
IDENTIFIER_4 IDENTIFIER_5 IDENTIFIER_6 =
    List.any (\IDENTIFIER_7 -> IDENTIFIER_6 % IDENTIFIER_7 == 0)
     IDENTIFIER_5
