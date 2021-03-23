module TwelveDays exposing (recite)


recite : Int -> (Int -> List String)
recite IDENTIFIER_1 IDENTIFIER_2 =
    List.map IDENTIFIER_3 (List.range IDENTIFIER_1 IDENTIFIER_2)

IDENTIFIER_3 : Int -> String
IDENTIFIER_3 IDENTIFIER_4 =
    "On the " ++ IDENTIFIER_5 IDENTIFIER_4 ++ " day of Christmas my true love gave to me, " ++ IDENTIFIER_6 IDENTIFIER_4

IDENTIFIER_5 : Int -> String
IDENTIFIER_5 IDENTIFIER_7 =
    
    case IDENTIFIER_7 of
      1 ->
        "first"
      2 ->
        "second"
      3 ->
        "third"
      4 ->
        "fourth"
      5 ->
        "fifth"
      6 ->
        "sixth"
      7 ->
        "seventh"
      8 ->
        "eighth"
      9 ->
        "ninth"
      10 ->
        "tenth"
      11 ->
        "eleventh"
      12 ->
        "twelfth"
      _ ->
        ""
    

IDENTIFIER_6 : Int -> String
IDENTIFIER_6 IDENTIFIER_4 =
    String.join " "
     (List.range 1 IDENTIFIER_4 |> List.reverse |> List.map IDENTIFIER_8)

IDENTIFIER_8 : Int -> String
IDENTIFIER_8 IDENTIFIER_4 =
    
    case IDENTIFIER_4 of
      1 ->
        "a Partridge in a Pear Tree."
      2 ->
        "two Turtle Doves, and"
      3 ->
        "three French Hens,"
      4 ->
        "four Calling Birds,"
      5 ->
        "five Gold Rings,"
      6 ->
        "six Geese-a-Laying,"
      7 ->
        "seven Swans-a-Swimming,"
      8 ->
        "eight Maids-a-Milking,"
      9 ->
        "nine Ladies Dancing,"
      10 ->
        "ten Lords-a-Leaping,"
      11 ->
        "eleven Pipers Piping,"
      12 ->
        "twelve Drummers Drumming,"
      _ ->
        ""
    
