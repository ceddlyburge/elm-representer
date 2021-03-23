module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear IDENTIFIER_1 =
    let
      
      
      IDENTIFIER_2 number =
          Basics.remainderBy number IDENTIFIER_1 == 0
    in
      if IDENTIFIER_2 400 then
        True
      else
        if IDENTIFIER_2 100 then
          False
        else
          if IDENTIFIER_2 4 then
            True
          else
            False
