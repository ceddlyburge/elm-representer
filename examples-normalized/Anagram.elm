module Anagram exposing (..)
import List as List exposing (filter, sort)
import String  exposing (toList, toLower)

IDENTIFIER_1 : String -> (List String -> List String)
IDENTIFIER_1 IDENTIFIER_2 =
    List.filter (IDENTIFIER_3 IDENTIFIER_2) >> List.filter (IDENTIFIER_4 IDENTIFIER_2)

IDENTIFIER_3 : String -> (String -> Bool)
IDENTIFIER_3 IDENTIFIER_2 IDENTIFIER_5 =
    IDENTIFIER_6 IDENTIFIER_2 == IDENTIFIER_6 IDENTIFIER_5

IDENTIFIER_4 : String -> (String -> Bool)
IDENTIFIER_4 IDENTIFIER_7 IDENTIFIER_8 =
    String.toLower IDENTIFIER_7 /= String.toLower IDENTIFIER_8

IDENTIFIER_6 : String -> List Char
IDENTIFIER_6  =
    String.toLower >> String.toList >> List.sort
