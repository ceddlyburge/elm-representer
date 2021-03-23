module Triangle exposing (Triangle (..), triangleKindGivenSideLengths)
import Set  
type Triangle 
=IDENTIFIER_1 
|IDENTIFIER_2 
|IDENTIFIER_3 

triangleKindGivenSideLengths : Float -> (Float -> (Float -> IDENTIFIER_4 String Triangle))
triangleKindGivenSideLengths IDENTIFIER_5 IDENTIFIER_6 IDENTIFIER_7 =
    if IDENTIFIER_5 <= 0 || IDENTIFIER_6 <= 0 || IDENTIFIER_7 <= 0 then
      IDENTIFIER_8 "Invalid lengths"
    else
      if IDENTIFIER_5 + IDENTIFIER_6 < IDENTIFIER_7 || IDENTIFIER_5 + IDENTIFIER_7 < IDENTIFIER_6 || IDENTIFIER_6 + IDENTIFIER_7 < IDENTIFIER_5 then
        IDENTIFIER_8 "Violates inequality"
      else
        
        case [IDENTIFIER_5, IDENTIFIER_6, IDENTIFIER_7] |> IDENTIFIER_9 of
          1 ->
            IDENTIFIER_10 IDENTIFIER_1
          2 ->
            IDENTIFIER_10 IDENTIFIER_2
          _ ->
            IDENTIFIER_10 IDENTIFIER_3
        

IDENTIFIER_9 : IDENTIFIER_11 comparable -> Int
IDENTIFIER_9  =
    Set.fromList >> Set.size
