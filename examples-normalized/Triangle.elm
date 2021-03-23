module Triangle exposing (Triangle (..), triangleKindGivenSideLengths)
import Set  
type Triangle 
=IDENTIFIER_1 
|IDENTIFIER_2 
|IDENTIFIER_3 

triangleKindGivenSideLengths : Float -> (Float -> (Float -> Result String Triangle))
triangleKindGivenSideLengths IDENTIFIER_4 IDENTIFIER_5 IDENTIFIER_6 =
    if IDENTIFIER_4 <= 0 || IDENTIFIER_5 <= 0 || IDENTIFIER_6 <= 0 then
      Err "Invalid lengths"
    else
      if IDENTIFIER_4 + IDENTIFIER_5 < IDENTIFIER_6 || IDENTIFIER_4 + IDENTIFIER_6 < IDENTIFIER_5 || IDENTIFIER_5 + IDENTIFIER_6 < IDENTIFIER_4 then
        Err "Violates inequality"
      else
        
        case [IDENTIFIER_4, IDENTIFIER_5, IDENTIFIER_6] |> IDENTIFIER_7 of
          1 ->
            Ok IDENTIFIER_1
          2 ->
            Ok IDENTIFIER_2
          _ ->
            Ok IDENTIFIER_3
        

IDENTIFIER_7 : List comparable -> Int
IDENTIFIER_7  =
    Set.fromList >> Set.size
