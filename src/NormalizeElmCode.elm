module NormalizeElmCode exposing (normalize)

import Elm.Parser
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Syntax.Expression exposing (Expression(..), Function, FunctionImplementation)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.Infix exposing (Infix)
import Elm.Syntax.Signature exposing (Signature)    
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import Elm.Writer exposing (writeFile)
import Normalization 
import List
import Dict as Dict exposing (Dict)
import Maybe as Maybe

-- todo
-- work my way through the other Declaration cases (adding tests)
-- CustomTypeDeclaration probably next, and probably fairly easy
-- add pull request to elm-syntax repo about the process init thing which is hard to work out

-- Declaration.Destructuring
-- functionsignature
-- LetExpression
-- CaseExpression
-- LambdaExpression
-- RecordAccessFunction
-- RecordExpr
-- RecordUpdateExpression
-- test functionorvalue expression
-- QualifiedNameRef part of Named pattern
--  prob want to normalize names in this file, but not imported ones
--  although this creates a problem if normalizing multiple files
--  so maybe special case known libraries
--  I guess the absolute best thing is to check which are external by looking at elm.json and working it out,
--  or maybe looking for matching files in the src directories.


-- special case all primitive types, not just String and Int, so they aren't normalized
-- special case core types such as Maybe and Result and so on
-- don't normalize anything outside of the file being looked at? could cause issues with multi file exercism submissions
-- normalize within scope (so 'a' can normalize to different values if it is defined in different scopes). This would improve the normalization, but the mapping format defined by exercism doesn't support it, so there probably isn't much point, and it would be harder to do

-- convention
-- only use 'normalizeNodes' from other lower level stuff, create and use higher level things elsewhere

-- convention variable names to 
--  'state'
--  'original'

-- do a round trip at the end of this to make sure that the normalization
-- code is working. This will catch the potential error when a returned
-- Normalization.State is ignored instead of being passed in to normalize
normalize : String -> (Dict String String, String)
normalize unNormalised =
    case Elm.Parser.parse unNormalised of
        Err error ->
            (Dict.empty, "Failed: " ++ Debug.toString error)
        Ok rawFile ->
            normalizeElmFile 
                Normalization.initialize
                (process init rawFile) 
            |> Tuple.mapFirst Normalization.getIdentifierMapping
            |> Tuple.mapSecond (writeFile >> write)


normalizeElmFile : Normalization.State -> File -> (Normalization.State, File)
normalizeElmFile state original = 
    let
        (state2, normalizedDeclarations) =
            normalizeNodes normalizeNodeDeclaration state original.declarations

        normalizedFile = 
            File
                original.moduleDefinition
                original.imports
                normalizedDeclarations
                []
                
    in
        ( state2
        , normalizedFile)

normalizeNodeDeclaration : Normalization.State -> Node Declaration -> (Normalization.State, Node Declaration)
normalizeNodeDeclaration state original =
    normalizeNode normalizeDeclaration state original

normalizeDeclaration : Normalization.State -> Declaration -> (Normalization.State, Declaration)
normalizeDeclaration state declaration =
    case declaration of
        Declaration.AliasDeclaration typeAlias ->
            let
                normalizedTypeAlias = normalizeTypeAlias state typeAlias
            in
                ( Tuple.first normalizedTypeAlias
                , Declaration.AliasDeclaration (Tuple.second normalizedTypeAlias)
                )

        Declaration.CustomTypeDeclaration theType ->
            let
                normalizedType = normalizeType state theType
            in
                ( Tuple.first normalizedType
                , Declaration.CustomTypeDeclaration (Tuple.second normalizedType)
                )

        Declaration.FunctionDeclaration function ->
            let
                normalizedFunction = normalizeFunction state function
            in
                ( Tuple.first normalizedFunction
                , Declaration.FunctionDeclaration (Tuple.second normalizedFunction)
                )

        Declaration.Destructuring originalPattern originalExpression ->
            let
                (state2, normalizedPattern) = normalizeNodePattern state originalPattern
                (state3, normalizedExpression) = normalizeNodeExpression state2 originalExpression
                normalized = 
                    Declaration.Destructuring
                        normalizedPattern
                        normalizedExpression

            in
                ( state3
                , normalized
                )

        -- I don't think we need to worry about port declarations and infixes (which are for core packages only)
        -- but we can revisit later if needs be 
        _ ->
            (state, declaration)


normalizeFunction : Normalization.State -> Function -> (Normalization.State, Function)
normalizeFunction state original =
    let
        (state2, normalizedFunctionImplementation) =
            normalizeNodeFunctionImplementation 
                state 
                original.declaration
        
        normalizedFunction = 
            Function
                Maybe.Nothing
                Maybe.Nothing -- should maybe do the function signature, its not a required thing, but is something that a mentor might comment on, so probably wants to normalize differently 
                normalizedFunctionImplementation
    in
        ( state2, normalizedFunction )

normalizeNodeFunctionImplementation : Normalization.State -> Node FunctionImplementation -> (Normalization.State, Node FunctionImplementation)
normalizeNodeFunctionImplementation state original =
    normalizeNode normalizeFunctionImplementation state original

normalizeFunctionImplementation : Normalization.State -> FunctionImplementation -> (Normalization.State, FunctionImplementation)
normalizeFunctionImplementation state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                original.name
        
        (state3, normalizedArguments) =
            normalizeNodePatterns 
                state2 
                original.arguments
        
        (state4, normalizedExpression) =
            normalizeNodeExpression 
                state3
                original.expression
                
        normalizedFunctionImplementation = 
            FunctionImplementation
                normalizedName
                normalizedArguments
                normalizedExpression
    in
        ( state4, normalizedFunctionImplementation )

normalizeNodeExpression : Normalization.State -> Node Expression -> (Normalization.State, Node Expression)
normalizeNodeExpression state original =
    normalizeNode normalizeExpression state original

normalizeNodeExpressions : Normalization.State -> List (Node Expression) -> (Normalization.State, List (Node Expression))
normalizeNodeExpressions state original =
    normalizeNodes normalizeNodeExpression state original

normalizeExpression : Normalization.State -> Expression -> (Normalization.State, Expression)
normalizeExpression state originalExpression =
    case originalExpression of
        UnitExpr ->
            (state, UnitExpr)

        Application original ->
            let
                normalized = normalizeNodeExpressions state original
            in
                ( Tuple.first normalized
                , Application (Tuple.second normalized)
                )

        OperatorApplication op dir left right ->
            let
                (state2, normalizedLeft) = normalizeNodeExpression state left
                (state3, normalizedRight) = normalizeNodeExpression state2 right
                normalized = 
                    OperatorApplication
                        op
                        dir
                        normalizedLeft
                        normalizedRight

            in
                ( state3
                , normalized
                )

        FunctionOrValue originalModuleName originalName ->
            let
                normalized = normalizeString state originalName
            in
                ( Tuple.first normalized
                , FunctionOrValue originalModuleName (Tuple.second normalized)
                )

        IfBlock c t e ->
            let
                (state2, normalizedCondition) = normalizeNodeExpression state c
                (state3, normalizedThen) = normalizeNodeExpression state2 t
                (state4, normalizedElse) = normalizeNodeExpression state3 e
                normalized = 
                    IfBlock
                        normalizedCondition
                        normalizedThen
                        normalizedElse

            in
                ( state4
                , normalized
                )

        PrefixOperator original ->
            (state, PrefixOperator original)

        Operator original ->
            (state, Operator original)

        Hex original ->
            (state, Hex original)

        Integer original ->
            (state, Integer original)

        Floatable original ->
            (state, Floatable original)

        Negation original ->
            (state, Negation original)

        Literal original ->
            (state, Literal original)

        CharLiteral original ->
            (state, CharLiteral original)

        TupledExpression original ->
            let
                normalized = normalizeNodeExpressions state original
            in
                ( Tuple.first normalized
                , TupledExpression (Tuple.second normalized)
                )

        ListExpr original ->
            let
                normalized = normalizeNodeExpressions state original
            in
                ( Tuple.first normalized
                , ListExpr (Tuple.second normalized)
                )

        ParenthesizedExpression original ->
            let
                normalized = normalizeNodeExpression state original
            in
                ( Tuple.first normalized
                , ParenthesizedExpression (Tuple.second normalized)
                )

        LetExpression original ->
            (state, LetExpression original) -- todo

        CaseExpression original ->
            (state, CaseExpression original) -- todo

        LambdaExpression original ->
            (state, LambdaExpression original) -- todo

        RecordAccess exp name ->
            let
                (state2, normalizedExpression) = normalizeNodeExpression state exp
                (state3, normalizedName) = normalizeNodeString state2 name
                normalized = 
                    RecordAccess
                        normalizedExpression
                        normalizedName
            in
                ( state3
                , normalized
                )

        -- I think this is '.name' type stuff, so probably need to strip the dot before normalizing
        RecordAccessFunction original ->
            (state, RecordAccessFunction original) -- todo

        RecordExpr original ->
            (state, RecordExpr original) -- todo

        RecordUpdateExpression name updates -> -- todo
            (state, 
                RecordUpdateExpression 
                    name
                    updates 
            )

        GLSLExpression original ->
            (state, GLSLExpression original)


normalizeNodePattern : Normalization.State -> Node Pattern -> (Normalization.State, Node Pattern)
normalizeNodePattern state original =
    normalizeNode normalizePattern state original

normalizeNodePatterns : Normalization.State -> List (Node Pattern) -> (Normalization.State, List (Node Pattern))
normalizeNodePatterns state original =
    normalizeNodes normalizeNodePattern state original

normalizePattern : Normalization.State -> Pattern -> (Normalization.State, Pattern)
normalizePattern state originalPattern =
    case originalPattern of
        AllPattern ->
            (state, AllPattern)

        UnitPattern ->
            (state, UnitPattern)

        CharPattern original ->
            (state, CharPattern original)

        StringPattern original ->
            (state, StringPattern original)

        HexPattern original ->
            (state, HexPattern original)

        IntPattern original ->
            (state, IntPattern original)

        FloatPattern original ->
            (state, FloatPattern original)

        TuplePattern original ->
            let
                normalized = normalizeNodePatterns state original
            in
                ( Tuple.first normalized
                , TuplePattern (Tuple.second normalized)
                )

        RecordPattern original ->
            let
                normalized = normalizeNodeStrings state original
            in
                ( Tuple.first normalized
                , RecordPattern (Tuple.second normalized)
                )

        UnConsPattern original1 original2 ->
            let
                (state2, normalized1) = normalizeNodePattern state original1
                (state3, normalized2) = normalizeNodePattern state2 original2

            in
                ( state3
                , UnConsPattern normalized1 normalized2
                )

        ListPattern original ->
            let
                normalized = normalizeNodePatterns state original
            in
                ( Tuple.first normalized
                , ListPattern (Tuple.second normalized)
                )

        VarPattern original ->
            let
                normalized = normalizeString state original
            in
                ( Tuple.first normalized
                , VarPattern (Tuple.second normalized)
                )

        NamedPattern qualifiedNameRef patterns ->
            let
                -- should probably do the qualifiedNameRef as well
                --(state2, normalized1) = normalizeNodePattern state qualifiedNameRef
                (state2, normalized2) = normalizeNodePatterns state patterns

            in
                ( state2
                , NamedPattern qualifiedNameRef normalized2
                )

        AsPattern pattern name ->
            let
                (state2, normalizedPattern) = normalizeNodePattern state pattern
                (state3, normalizedName) = normalizeNodeString state2 name

            in
                ( state3
                , AsPattern normalizedPattern normalizedName
                )

        ParenthesizedPattern original ->
            let
                normalized = normalizeNodePattern state original
            in
                ( Tuple.first normalized
                , ParenthesizedPattern (Tuple.second normalized)
                )

normalizeType : Normalization.State -> Type -> (Normalization.State, Type)
normalizeType state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                original.name
        
        (state3, normalizedGenerics) = 
            normalizeNodeStrings 
                state2
                original.generics
        
        (state4, normalizedValueConstructors) = 
            normalizeNodeValueConstructors 
                state3
                original.constructors
        
        normalizedType = 
            Type
                Maybe.Nothing
                normalizedName
                normalizedGenerics
                normalizedValueConstructors      
    in
        ( state4, normalizedType )

normalizeNodeValueConstructor : Normalization.State -> Node ValueConstructor -> (Normalization.State, Node ValueConstructor)
normalizeNodeValueConstructor state original =
    normalizeNode normalizeValueConstructor state original

normalizeNodeValueConstructors : Normalization.State -> List (Node ValueConstructor) -> (Normalization.State, List (Node ValueConstructor))
normalizeNodeValueConstructors state original =
    normalizeNodes normalizeNodeValueConstructor state original

normalizeValueConstructor : Normalization.State -> ValueConstructor -> (Normalization.State, ValueConstructor)
normalizeValueConstructor state original =
    let
        (state2, normalizedName) = normalizeNodeString state original.name
        (state3, normalizedArguments) = normalizeNodeTypeAnnotations state2 original.arguments
        normalizedValueConstructor = 
            ValueConstructor
                normalizedName
                normalizedArguments
    in
        (state3, normalizedValueConstructor)



normalizeTypeAlias : Normalization.State -> TypeAlias -> (Normalization.State, TypeAlias)
normalizeTypeAlias state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                original.name
        
        (state3, normalizedGenerics) = 
            normalizeNodeStrings 
                state2
                original.generics
        
        (state4, normalizedTypeAnnotation) = 
            normalizeNodeTypeAnnotation 
                state3
                original.typeAnnotation
        
        typeAlias = 
            TypeAlias
                Maybe.Nothing
                normalizedName
                normalizedGenerics
                normalizedTypeAnnotation      
    in
        ( state4, typeAlias )


normalizeNodeTypeAnnotation : Normalization.State -> Node TypeAnnotation -> (Normalization.State, Node TypeAnnotation)
normalizeNodeTypeAnnotation state original =
    normalizeNode normalizeTypeAnnotation state original

normalizeNodeTypeAnnotations : Normalization.State -> List (Node TypeAnnotation) -> (Normalization.State, List (Node TypeAnnotation))
normalizeNodeTypeAnnotations state original =
    normalizeNodes normalizeNodeTypeAnnotation state original

normalizeTypeAnnotation : Normalization.State -> TypeAnnotation -> (Normalization.State, TypeAnnotation)
normalizeTypeAnnotation state typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.Record original ->
            let
                normalized = normalizeNodeRecordFields state original
            in
                ( Tuple.first normalized
                , TypeAnnotation.Record (Tuple.second normalized)
                )
        
        TypeAnnotation.Tupled original ->
            let
                normalized = normalizeNodeTypeAnnotations state original
            in
                ( Tuple.first normalized
                , TypeAnnotation.Tupled (Tuple.second normalized)
                )

        TypeAnnotation.Typed originalName originalTypes ->
            let
                (state2, normalizedName) = normalizeNodeTypeName state originalName
                (state3, normalizedTypes) = normalizeNodeTypeAnnotations state2 originalTypes
            in
                ( state3
                , TypeAnnotation.Typed normalizedName normalizedTypes
                )
        
        TypeAnnotation.GenericRecord originalName originalRecordDefinition ->
            let
                (state2, normalizedName) = normalizeNodeString state originalName
                (state3, normalizedRecordDefinition) = normalizeNodeRecordDefinition state2 originalRecordDefinition
            in
                ( state3
                , TypeAnnotation.GenericRecord normalizedName normalizedRecordDefinition
                )

        TypeAnnotation.FunctionTypeAnnotation originalParameters originalReturn ->
            let
                (state2, normalizedParameters) = normalizeNodeTypeAnnotation state originalParameters
                (state3, normalizedReturn) = normalizeNodeTypeAnnotation state2 originalReturn
            in
                ( state3
                , TypeAnnotation.FunctionTypeAnnotation normalizedParameters normalizedReturn
                )

        TypeAnnotation.GenericType original ->
            let
                (state2, normalized) = normalizeString state original
            in
                ( state2
                , TypeAnnotation.GenericType normalized
                )

        _ ->
            (state, typeAnnotation)

normalizeNodeRecordDefinition: Normalization.State -> Node TypeAnnotation.RecordDefinition -> (Normalization.State, Node TypeAnnotation.RecordDefinition)
normalizeNodeRecordDefinition state original =
    normalizeNode normalizeRecordDefinition state original


normalizeRecordDefinition : Normalization.State -> TypeAnnotation.RecordDefinition -> (Normalization.State, TypeAnnotation.RecordDefinition)
normalizeRecordDefinition state original =
    normalizeNodeRecordFields state original


normalizeNodeRecordField: Normalization.State -> Node TypeAnnotation.RecordField -> (Normalization.State, Node TypeAnnotation.RecordField)
normalizeNodeRecordField state original =
    normalizeNode normalizeRecordField state original

normalizeNodeRecordFields: Normalization.State -> List (Node TypeAnnotation.RecordField) -> (Normalization.State, List (Node TypeAnnotation.RecordField))
normalizeNodeRecordFields state original =
    normalizeNodes normalizeNodeRecordField state original

normalizeRecordField : Normalization.State -> TypeAnnotation.RecordField -> (Normalization.State, TypeAnnotation.RecordField)
normalizeRecordField state original =
    let
        (state2, normalizedName) =
            normalizeNodeString 
                state 
                (Tuple.first original)
        
        (state3, normalizedTypeAnnotation) = 
            normalizeNodeTypeAnnotation
                state2
                (Tuple.second original)

        recordField = (normalizedName, normalizedTypeAnnotation)        
    in
        ( state3, recordField )


normalizeNodeTypeName : Normalization.State -> Node ( ModuleName, String ) -> (Normalization.State, Node ( ModuleName, String ))
normalizeNodeTypeName state original =
    normalizeNode normalizeTypeName state original

normalizeTypeName : Normalization.State -> ( ModuleName, String ) -> (Normalization.State, ( ModuleName, String ))
normalizeTypeName state (originalModuleName, originalTypeName) =
    let
        (state2, normalizedTypeName) =
            normalizeString 
                state 
                originalTypeName

        typeName = (originalModuleName, normalizedTypeName)        
    in
        ( state2, typeName )


{-| Custom type for different type annotations. For example:
  - `Var`: `a`
  - `Type`: `Maybe (Int -> String)`
  - `Tuples`: `(a, b, c)` or Unit `()`
  - `Record`: `{ name : String }`
  - `ExtensionRecord`: `{ a | name : String }`
  - `FunctionTypeAnnotation`: `Int -> String`
-}
-- type TypeAnnotation
--     = Var String
--     | Type (Node ( ModuleName, String )) (List (Node TypeAnnotation))
--     | Tuple (List (Node TypeAnnotation))
--     | Record (List (Node RecordField))
--     | ExtensionRecord (Node String) (Node RecordField) (List (Node RecordField))
--     | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)



normalizeNodeString : Normalization.State -> Node String -> (Normalization.State, Node String)
normalizeNodeString state original =
    normalizeNode normalizeString state original

normalizeString : Normalization.State -> String -> (Normalization.State, String)
normalizeString =
    Normalization.normalize

normalizeNodeStrings : Normalization.State -> List (Node String) -> (Normalization.State, List (Node String))
normalizeNodeStrings state original =
    normalizeNodes normalizeNodeString state original

normalizeNode : 
    (Normalization.State -> a -> (Normalization.State, a)) 
    -> Normalization.State 
    -> Node a 
    -> (Normalization.State, Node a)
normalizeNode normalizer state original =
    let
        normalized =
            Node.map 
                (normalizer state) 
                original
    in
        ( Node.value normalized |> Tuple.first
        , Node.map Tuple.second normalized)

normalizeNodes : 
    (Normalization.State -> Node a -> (Normalization.State, Node a)) 
    -> Normalization.State 
    -> List (Node a) 
    -> (Normalization.State, List (Node a))
normalizeNodes normalizer state original =
    List.foldl
        (normalizeAccumulateNode normalizer) 
        (state, []) 
        original

normalizeAccumulateNode : 
    (Normalization.State -> Node a -> (Normalization.State, Node a)) 
    -> Node a 
    -> (Normalization.State, List (Node a)) 
    -> (Normalization.State, List (Node a))
normalizeAccumulateNode normalizer original (state, normalizedNodes) =
    let
        (nextState, normalized) = normalizer state original
    in
        ( nextState
        , normalizedNodes ++ [normalized]
        )
