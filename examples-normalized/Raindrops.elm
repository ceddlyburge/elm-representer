module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops number =
    let
      
      
      IDENTIFIER_1  =
          [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    in
      
      case IDENTIFIER_2 number IDENTIFIER_1 of
        [] ->
          String.fromInt number
        IDENTIFIER_3 ->
          IDENTIFIER_3 |> String.join ""
      

IDENTIFIER_2 : Int -> (IDENTIFIER_4 ((Int, String)) -> IDENTIFIER_4 String)
IDENTIFIER_2 number IDENTIFIER_1 =
    IDENTIFIER_1 |> List.filter (Tuple.first >> IDENTIFIER_5 number) |> List.map Tuple.second

IDENTIFIER_5 : Int -> (Int -> Bool)
IDENTIFIER_5 IDENTIFIER_6 IDENTIFIER_7 =
    IDENTIFIER_8 IDENTIFIER_7 IDENTIFIER_6 == 0
