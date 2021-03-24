module Raindrops exposing (raindrops)


raindrops : Int -> String
raindrops IDENTIFIER_1 =
    let
      
      
      IDENTIFIER_2  =
          [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    in
      
      case IDENTIFIER_3 IDENTIFIER_1 IDENTIFIER_2 of
        [] ->
          String.fromInt IDENTIFIER_1
        IDENTIFIER_4 ->
          IDENTIFIER_4 |> String.join ""
      

IDENTIFIER_3 : Int -> (List ((Int, String)) -> List String)
IDENTIFIER_3 IDENTIFIER_1 IDENTIFIER_2 =
    IDENTIFIER_2 |> List.filter (Tuple.first >> IDENTIFIER_5 IDENTIFIER_1) |> List.map Tuple.second

IDENTIFIER_5 : Int -> (Int -> Bool)
IDENTIFIER_5 IDENTIFIER_6 IDENTIFIER_7 =
    modBy IDENTIFIER_7 IDENTIFIER_6 == 0
