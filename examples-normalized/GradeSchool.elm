module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)
import Dict  exposing (Dict)
type alias IDENTIFIER_1  =
    Int
type alias IDENTIFIER_2  =
    String
type alias IDENTIFIER_3  =
    Dict IDENTIFIER_1 (List IDENTIFIER_2)

empty : IDENTIFIER_3
empty  =
    Dict.empty

addStudent : IDENTIFIER_1 -> (IDENTIFIER_2 -> (IDENTIFIER_3 -> IDENTIFIER_3))
addStudent IDENTIFIER_4 IDENTIFIER_5 IDENTIFIER_6 =
    let
      
      
      IDENTIFIER_7  =
          IDENTIFIER_5 :: studentsInGrade IDENTIFIER_4 IDENTIFIER_6 |> List.sort
    in
      Dict.insert IDENTIFIER_4 IDENTIFIER_7 IDENTIFIER_6

studentsInGrade : IDENTIFIER_1 -> (IDENTIFIER_3 -> List IDENTIFIER_2)
studentsInGrade IDENTIFIER_4 IDENTIFIER_6 =
    Dict.get IDENTIFIER_4 IDENTIFIER_6 |> Maybe.withDefault []

allStudents : IDENTIFIER_3 -> List ((IDENTIFIER_1, List IDENTIFIER_2))
allStudents IDENTIFIER_6 =
    Dict.toList IDENTIFIER_6
