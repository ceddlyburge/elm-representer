module Normalize exposing (suite)


import Test exposing (..)
import Expect

import Normalization


suite : Test
suite =
    describe "Normalization"
    [ test "shoud ignore primitive String type" <|
            \_ ->
                normalize "String"
                |> Expect.equal "String"
        
    , test "shoud ignore primitive Char type" <|
            \_ ->
                normalize "Char"
                |> Expect.equal "Char"
      
    , test "shoud ignore primitive Bool type" <|
            \_ ->
                normalize "Bool"
                |> Expect.equal "Bool"
      
    , test "shoud ignore primitive Int type" <|
            \_ ->
                normalize "Int"
                |> Expect.equal "Int"
      
    , test "shoud ignore primitive Float type" <|
            \_ ->
                normalize "Float"
                |> Expect.equal "Float"
      
    , test "shoud ignore primitive True value" <|
            \_ ->
                normalize "True"
                |> Expect.equal "True"
      
    , test "shoud ignore primitive False value" <|
            \_ ->
                normalize "False"
                |> Expect.equal "False"
      
    , test "shoud ignore primitive number typeclass" <|
            \_ ->
                normalize "number"
                |> Expect.equal "number"
      
    , test "shoud ignore primitive appendable typeclass" <|
            \_ ->
                normalize "appendable"
                |> Expect.equal "appendable"
      
    , test "shoud ignore primitive comparable typeclass" <|
            \_ ->
                normalize "comparable"
                |> Expect.equal "comparable"
      
    , test "shoud ignore primitive compappend typeclass" <|
            \_ ->
                normalize "compappend"
                |> Expect.equal "compappend"
      
    ]

normalize string =
    Normalization.initialize 
    |> \state -> Normalization.normalize state string
    |> Tuple.second