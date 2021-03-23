module Pangram exposing (isPangram)
import Set  exposing (..)

isPangram : String -> Bool
isPangram IDENTIFIER_1 =
    let
      
      
      IDENTIFIER_2  =
          26
    in
      String.toLower IDENTIFIER_1 |> String.toList |> List.filter Char.isAlpha |> Set.fromList |> Set.size |> (==) IDENTIFIER_2
