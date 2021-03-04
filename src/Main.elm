port module Main exposing (main)

import Platform exposing (Program)
import Elm.Parser
import Elm.Syntax.File
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Expression as Expression exposing (Function, FunctionImplementation, CaseBlock, Case, Lambda)
import Elm.Syntax.Infix as Infix exposing (Infix)
--import Elm.Syntax.Port as Port exposing (Port)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAlias as TypeAlias exposing (TypeAlias)
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Elm.Writer exposing (writeFile, write)
import Elm.Processing exposing (init, process)
import Json.Encode
import List
import Html.Attributes exposing (name)
import Elm.Syntax.Documentation exposing (Documentation)
import Html.Attributes exposing (pattern)
import Elm.Writer exposing (writeFile)

type alias InputType = String
type alias OutputType = String

port get : (InputType -> msg) -> Sub msg

port put : OutputType -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init2
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    ()


type Msg
    = Input String


type alias Flags =
    ()


init2 : Flags -> ( Model, Cmd Msg )
init2 _ =
    ( (), Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input -> ( model, put (transform input))


subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input

normaliseElmFile: Elm.Syntax.File.File -> Elm.Syntax.File.File
normaliseElmFile elmFile = 
    Elm.Syntax.File.File
        elmFile.moduleDefinition
        elmFile.imports
        (List.map (Node.map normaliseDeclaration) elmFile.declarations)
        []

normaliseDeclaration : Declaration -> Declaration
normaliseDeclaration decl =
    case decl of
        Declaration.FunctionDeclaration function ->
            Declaration.FunctionDeclaration (normaliseFunctionDeclaration function)

        Declaration.AliasDeclaration typeAlias ->
            Declaration.AliasDeclaration (normaliseTypeAlias typeAlias)

        Declaration.CustomTypeDeclaration typeDeclaration ->
            Declaration.CustomTypeDeclaration (normaliseTypeDeclaration typeDeclaration)

        Declaration.PortDeclaration p ->
            Declaration.PortDeclaration p

        Declaration.InfixDeclaration inf ->
            Declaration.InfixDeclaration inf
        
        Declaration.Destructuring x y ->
            Declaration.Destructuring x y

normaliseTypeDeclaration: Type -> Type
normaliseTypeDeclaration elmType =
    Type
        elmType.documentation
        (Node.map normaliseString elmType.name)
        (List.map (Node.map normaliseString) elmType.generics)
        elmType.constructors -- normalise

normaliseTypeAlias: TypeAlias -> TypeAlias
normaliseTypeAlias typeAlias =
    TypeAlias
        typeAlias.documentation
        (Node.map normaliseString typeAlias.name)
        (List.map (Node.map normaliseString) typeAlias.generics)
        typeAlias.typeAnnotation -- maybe normalise, maybe remove


normaliseFunctionDeclaration : Function -> Function
normaliseFunctionDeclaration { documentation, signature, declaration } =
    Function
        documentation
        signature
        (Node.map normaliseFunctionImplementation declaration)

normaliseFunctionImplementation : FunctionImplementation -> FunctionImplementation
normaliseFunctionImplementation { name, arguments, expression } =
    FunctionImplementation
        (Node.map normaliseString name)
        (List.map (Node.map normalisePattern) arguments)
        (Node.map normaliseExpression expression)

normaliseLetBlock : Expression.LetBlock -> Expression.LetBlock
normaliseLetBlock { declarations, expression } =
    Expression.LetBlock
        (List.map (Node.map normaliseLetDeclaration) declarations)
        (Node.map normaliseExpression expression)

normaliseLetDeclaration : Expression.LetDeclaration -> Expression.LetDeclaration
normaliseLetDeclaration letDeclaration =
    case letDeclaration of
        Expression.LetFunction f ->
            Expression.LetFunction (normaliseFunctionDeclaration f)

        Expression.LetDestructuring pattern expression ->
            Expression.LetDestructuring
                (Node.map normalisePattern pattern)
                (Node.map normaliseExpression expression)


normaliseCaseBlock: CaseBlock -> CaseBlock
normaliseCaseBlock { cases, expression } =
    CaseBlock
        (Node.map normaliseExpression expression)
        (List.map normaliseCase cases) 

normaliseCase : Case -> Case
normaliseCase ( pattern, expression ) =
    (
        (Node.map normalisePattern pattern)
        , (Node.map normaliseExpression expression)
    )

normaliseLambda: Lambda -> Lambda
normaliseLambda { args, expression } =
    Lambda
        (List.map (Node.map normalisePattern) args)
        (Node.map normaliseExpression expression) 

normalisePattern: Pattern -> Pattern
normalisePattern pattern =
    case pattern of
        AllPattern ->
            AllPattern

        UnitPattern ->
            UnitPattern

        CharPattern c ->
            CharPattern c

        StringPattern v ->
            StringPattern v

        HexPattern h ->
            HexPattern h

        IntPattern i ->
            IntPattern i

        FloatPattern f ->
            FloatPattern f

        TuplePattern patterns ->
            TuplePattern (List.map (Node.map normalisePattern) patterns)

        RecordPattern pointers ->
            RecordPattern (List.map (Node.map normaliseString) pointers)

        UnConsPattern p1 p2 ->
            UnConsPattern (Node.map normalisePattern p1) (Node.map normalisePattern p2)

        ListPattern patterns ->
            ListPattern (List.map (Node.map normalisePattern) patterns)

        VarPattern name ->
            VarPattern (normaliseString name)

        NamedPattern qualifiedNameRef patterns ->
            NamedPattern 
                qualifiedNameRef -- normalise?
                (List.map (Node.map normalisePattern) patterns)

        AsPattern destructured name ->
            AsPattern
                (Node.map normalisePattern destructured)
                (Node.map normaliseString name)

        ParenthesizedPattern p1 ->
            ParenthesizedPattern (Node.map normalisePattern p1)


-- maybe split in to the type of things that are normalised?
-- such as
-- - module names
-- - type names
-- - type alias names
-- - function / value names
-- - function parameter names
-- - names of values in expressions
normaliseString: String -> String
normaliseString unNormalised =
    "cedd"


normaliseExpression: Expression.Expression -> Expression.Expression
normaliseExpression expression =
    case expression of
        Expression.UnitExpr ->
            Expression.UnitExpr

        Expression.Application nodes ->
            Expression.Application (List.map (Node.map normaliseExpression) nodes)

        Expression.OperatorApplication op dir left right ->
            Expression.OperatorApplication 
                op
                dir
                (Node.map normaliseExpression left)
                (Node.map normaliseExpression right)

        Expression.FunctionOrValue moduleName name ->
            Expression.FunctionOrValue 
                moduleName -- normalise?    
                (normaliseString name)

        Expression.IfBlock c t e ->
            Expression.IfBlock 
                (Node.map normaliseExpression c)
                (Node.map normaliseExpression t)
                (Node.map normaliseExpression e)

        Expression.PrefixOperator x ->
            Expression.PrefixOperator x

        Expression.Operator x ->
            Expression.Operator x

        Expression.Hex h ->
            Expression.Hex h

        Expression.Integer x ->
            Expression.Integer x

        Expression.Floatable x ->
            Expression.Floatable x

        Expression.Negation x ->
            Expression.Negation (Node.map normaliseExpression x)

        Expression.Literal x ->
            Expression.Literal x

        Expression.CharLiteral c ->
            Expression.CharLiteral c

        Expression.TupledExpression xs ->
            Expression.TupledExpression (List.map (Node.map normaliseExpression) xs)

        Expression.ListExpr xs ->
            Expression.ListExpr  (List.map (Node.map normaliseExpression) xs)

        Expression.ParenthesizedExpression x ->
            Expression.ParenthesizedExpression (Node.map normaliseExpression x)

        Expression.LetExpression x ->
            Expression.LetExpression (normaliseLetBlock x)

        Expression.CaseExpression x ->
            Expression.CaseExpression (normaliseCaseBlock x) 

        Expression.LambdaExpression x ->
            Expression.LambdaExpression (normaliseLambda x)

        Expression.RecordAccess exp name ->
            Expression.RecordAccess 
                (Node.map normaliseExpression exp) 
                (Node.map normaliseString name)

        Expression.RecordAccessFunction x ->
            Expression.RecordAccessFunction (normaliseString x)

        Expression.RecordExpr xs -> -- normalise?
            Expression.RecordExpr xs

        Expression.RecordUpdateExpression name updates ->
            Expression.RecordUpdateExpression 
                (Node.map normaliseString name)
                updates -- normalise?

        Expression.GLSLExpression x ->
            Expression.GLSLExpression (normaliseString x)


transform : InputType -> OutputType
transform unNormalised =
    case Elm.Parser.parse unNormalised of
        Err error ->
            "Failed: " ++ Debug.toString error
        Ok rawFile ->
            -- "Success: " ++ (Debug.toString (Elm.RawFile.encode v))
            process init rawFile
            |> normaliseElmFile
            |> writeFile
            |> write
            -- |> Elm.Syntax.File.encode
            -- |> Json.Encode.encode 1
    
