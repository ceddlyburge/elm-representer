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
      

IDENTIFIER_2 : Int -> (List ((Int, String)) -> List String)
IDENTIFIER_2 number IDENTIFIER_1 =
    IDENTIFIER_1 |> List.filter (Tuple.first >> IDENTIFIER_4 number) |> List.map Tuple.second

IDENTIFIER_4 : Int -> (Int -> Bool)
IDENTIFIER_4 IDENTIFIER_5 IDENTIFIER_6 =
    modBy IDENTIFIER_6 IDENTIFIER_5 == 0
