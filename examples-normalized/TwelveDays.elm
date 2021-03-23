module TwelveDays exposing (recite)


recite : Int -> (Int -> IDENTIFIER_1 String)
recite IDENTIFIER_2 IDENTIFIER_3 =
    List.map IDENTIFIER_4 (List.range IDENTIFIER_2 IDENTIFIER_3)

IDENTIFIER_4 : Int -> String
IDENTIFIER_4 IDENTIFIER_5 =
    "On the " ++ IDENTIFIER_6 IDENTIFIER_5 ++ " day of Christmas my true love gave to me, " ++ IDENTIFIER_7 IDENTIFIER_5

IDENTIFIER_6 : Int -> String
IDENTIFIER_6 IDENTIFIER_8 =
    
    case IDENTIFIER_8 of
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
    

IDENTIFIER_7 : Int -> String
IDENTIFIER_7 IDENTIFIER_5 =
    String.join " "
     (List.range 1 IDENTIFIER_5 |> List.reverse |> List.map IDENTIFIER_9)

IDENTIFIER_9 : Int -> String
IDENTIFIER_9 IDENTIFIER_5 =
    
    case IDENTIFIER_5 of
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
    
