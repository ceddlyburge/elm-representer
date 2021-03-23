module Hamming exposing (hammingDistance)
import List  exposing (filter, map2)
import String  exposing (length, toList)

hammingDistance : String -> (String -> IDENTIFIER_1 String Int)
hammingDistance IDENTIFIER_2 IDENTIFIER_3 =
    if length IDENTIFIER_2 == length IDENTIFIER_3 then
      map2 (/=) (toList IDENTIFIER_2) (toList IDENTIFIER_3) |> filter IDENTIFIER_4 |> List.length |> IDENTIFIER_5
    else
      IDENTIFIER_6 "left and right strands must be of equal length"
