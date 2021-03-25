module CustomTypeDeclaration exposing (..)

--exposing (suite)

import Helpers exposing (..)
import Test exposing (..)


temp =
    1



-- suite : Test
-- suite =
--     describe "Normalize"
--         [ skip <|
--             test "shoud normalize Name and ValueConstructors of Custom Types" <|
--                 \_ ->
--                     givenElmCodeOf "type FirstName = FirstName String"
--                         |> whenNormalize
--                         |> thenContains "type Identifier_1\n=Identifier_1 String"
--         , skip <|
--             test "shoud normalize generic type parameters of Custom Types" <|
--                 \_ ->
--                     givenElmCodeOf "type InputType a = InputType a String"
--                         |> whenNormalize
--                         |> thenContains "type Identifier_1 identifier_2\n=Identifier_1 identifier_2 String"
--         , skip <|
--             test "shoud ignore typeclass type parameters Custom Types" <|
--                 \_ ->
--                     givenElmCodeOf "type InputType a = InputType a number"
--                         |> whenNormalize
--                         |> thenContains "type Identifier_1 identifier_2\n=Identifier_1 identifier_2 number"
--         ]
