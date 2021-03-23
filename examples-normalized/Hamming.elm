module Hamming exposing (hammingDistance)
import List  exposing (filter, map2)
import String  exposing (length, toList)

hammingDistance : String -> (String -> Result String Int)
hammingDistance IDENTIFIER_1 IDENTIFIER_2 =
    if length IDENTIFIER_1 == length IDENTIFIER_2 then
      map2 (/=) (toList IDENTIFIER_1) (toList IDENTIFIER_2) |> filter identity |> List.length |> Ok
    else
      Err "left and right strands must be of equal length"
