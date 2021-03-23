module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)
import Dict  exposing (Dict)
type alias IDENTIFIER_1  =
    Int
type alias IDENTIFIER_2  =
    String
type alias IDENTIFIER_3  =
    Dict IDENTIFIER_1 (IDENTIFIER_4 IDENTIFIER_2)

empty : IDENTIFIER_3
empty  =
    Dict.empty

addStudent : IDENTIFIER_1 -> (IDENTIFIER_2 -> (IDENTIFIER_3 -> IDENTIFIER_3))
addStudent IDENTIFIER_5 IDENTIFIER_6 IDENTIFIER_7 =
    let
      
      
      IDENTIFIER_8  =
          IDENTIFIER_6 :: studentsInGrade IDENTIFIER_5 IDENTIFIER_7 |> List.sort
    in
      Dict.insert IDENTIFIER_5 IDENTIFIER_8 IDENTIFIER_7

studentsInGrade : IDENTIFIER_1 -> (IDENTIFIER_3 -> IDENTIFIER_4 IDENTIFIER_2)
studentsInGrade IDENTIFIER_5 IDENTIFIER_7 =
    Dict.get IDENTIFIER_5 IDENTIFIER_7 |> Maybe.withDefault []

allStudents : IDENTIFIER_3 -> IDENTIFIER_4 ((IDENTIFIER_1, IDENTIFIER_4 IDENTIFIER_2))
allStudents IDENTIFIER_7 =
    Dict.toList IDENTIFIER_7
