module Bob exposing (hey)

import Regex exposing (Regex)


hey : String -> String
hey s =
    if isShouting s && isQuestioning s then
        "Calm down, I know what I'm doing!"

    else if isShouting s then
        "Whoa, chill out!"

    else if isQuestioning s then
        "Sure."

    else if isSilent s then
        "Fine. Be that way!"

    else
        "Whatever."


isShouting : String -> Bool
isShouting s =
    s == String.toUpper s && wordChars s


isQuestioning : String -> Bool
isQuestioning s =
    s |> String.trim |> String.endsWith "?"


isSilent : String -> Bool
isSilent s =
    String.trim s == ""


wordChars : String -> Bool
wordChars s =
    String.any Char.isAlpha s