module Bob exposing (hey)
import Regex  exposing (Regex)
type IDENTIFIER_1 IDENTIFIER_2 IDENTIFIER_3
=IDENTIFIER_1 IDENTIFIER_2 IDENTIFIER_3
type alias IDENTIFIER_4 IDENTIFIER_2 IDENTIFIER_3 =
    IDENTIFIER_1 IDENTIFIER_2 IDENTIFIER_3
type alias IDENTIFIER_5  =
    {IDENTIFIER_6 : String, IDENTIFIER_7 : Int}

hey : String -> String
hey IDENTIFIER_8 =
    if IDENTIFIER_9 IDENTIFIER_8 && IDENTIFIER_10 IDENTIFIER_8 then
      "Calm down, I know what I'm doing!"
    else
      if IDENTIFIER_9 IDENTIFIER_8 then
        "Whoa, chill out!"
      else
        if IDENTIFIER_10 IDENTIFIER_8 then
          "Sure."
        else
          if IDENTIFIER_11 IDENTIFIER_8 then
            "Fine. Be that way!"
          else
            "Whatever."

IDENTIFIER_9 : String -> Bool
IDENTIFIER_9 IDENTIFIER_8 =
    IDENTIFIER_8 == String.toUpper IDENTIFIER_8 && IDENTIFIER_12 IDENTIFIER_8

IDENTIFIER_10 : String -> Bool
IDENTIFIER_10 IDENTIFIER_8 =
    IDENTIFIER_8 |> String.trim |> String.endsWith "?"

IDENTIFIER_11 : String -> Bool
IDENTIFIER_11 IDENTIFIER_8 =
    String.trim IDENTIFIER_8 == ""

IDENTIFIER_12 : String -> Bool
IDENTIFIER_12 IDENTIFIER_8 =
    String.any Char.isAlpha IDENTIFIER_8
