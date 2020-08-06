module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, span)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Decode

import Parser exposing (..)

type Expr    
    = Number Float
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    | Exponent Expr Expr
    | Sin Expr
    | Cos Expr
    
type Associativity = Left | Right
    
type alias BinaryOperator =
    { opr: Expr -> Expr -> Expr
    , oprSymbol: String
    , bindingPower: Int
    , associativity: Associativity
    }

type alias UnaryOperator =
    { opr: Expr -> Expr
    , oprSymbol: String
    , bindingPower: Int
    }

expr : Int -> Parser Expr 
expr lastOprBPower =
    succeed identity
        |. spaces
        |= oneOf 
            [ number
            , paren
            , unaryOperator (UnaryOperator Sin "sin" 40)
            , unaryOperator (UnaryOperator Cos "cos" 40)
            ]
    |> andThen (\leftExpr -> loop (lastOprBPower, leftExpr) operators)
    |> map (\(_, exp) -> exp)

number : Parser Expr
number =
    succeed Number
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= float
            , float
            ]

paren : Parser Expr
paren =
    succeed identity
        |. symbol "("
        |= lazy (\_ -> expr 0)
        |. symbol ")"
        
unaryOperator : UnaryOperator -> Parser Expr
unaryOperator { opr, oprSymbol, bindingPower } =
    succeed opr
        |. keyword oprSymbol
        |= lazy (\_ -> expr bindingPower)

operators : (Int, Expr) -> Parser (Step (Int, Expr) (Int, Expr))
operators (lastOprBPower, leftExpr) =
    succeed identity
        |. spaces
        |= oneOf 
            [ succeed (\exp -> Loop (0, exp) ) 
                |= oneOf 
                    [ operator (BinaryOperator Add "+" 10 Left) lastOprBPower leftExpr
                    , operator (BinaryOperator Subtract "-" 10 Left) lastOprBPower leftExpr
                    , operator (BinaryOperator Multiply "*" 20 Left) lastOprBPower leftExpr
                    , operator (BinaryOperator Divide "/" 20 Left) lastOprBPower leftExpr
                    , operator (BinaryOperator Exponent "^" 30 Right) lastOprBPower leftExpr
                    ]
            , succeed () 
                |> map (\_ -> Done (lastOprBPower, leftExpr))
            ]

operator : BinaryOperator -> Int -> Expr -> Parser Expr 
operator {opr, oprSymbol, bindingPower, associativity} lastOprBPower leftExpr =
    backtrackable <| succeed (opr leftExpr)
        |. symbol oprSymbol
        |. checkOperator lastOprBPower bindingPower
        |. spaces
        |= lazy (\_ -> expr <| getBindingPower bindingPower associativity )

getBindingPower : Int -> Associativity -> Int
getBindingPower bindingPower associativity =
    if associativity == Left then
        bindingPower
    else
        bindingPower - 1
    
checkOperator : Int -> Int -> Parser ()
checkOperator lastOperator currentOperator =
    if currentOperator > lastOperator then
        commit ()
    else
        problem ""

evalExpr : Expr -> Float
evalExpr exp =
    case exp of
        Number fl -> 
            fl
        
        Add leftExpr rightExpr -> 
            evalExpr leftExpr + evalExpr rightExpr 
        
        Subtract leftExpr rightExpr ->
            evalExpr leftExpr - evalExpr rightExpr 
        
        Multiply leftExpr rightExpr ->
            evalExpr leftExpr * evalExpr rightExpr 
        
        Divide leftExpr rightExpr ->
            evalExpr leftExpr / evalExpr rightExpr 
        
        Exponent leftExpr rightExpr ->
            evalExpr leftExpr ^ evalExpr rightExpr
        
        Sin xp ->
            sin <| evalExpr xp
        
        Cos xp ->
            cos <| evalExpr xp

type alias Model =
    { pastExpressions : List (String, (Result (List DeadEnd) Float))
    , exprStr: String
    , evaulatedExpr: Expression
    }

type Expression 
    = ExprNotEvaulated
    | ExprEvaulated (Result (List DeadEnd) Float)


initialModel : Model
initialModel =
    Model [] "" ExprNotEvaulated


type Msg
    = Evaulate
    | Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Evaulate ->
            let
                expResult = run (expr 0) model.exprStr
            in
            case expResult of
                Ok exp -> 
                    Model model.pastExpressions model.exprStr (ExprEvaulated <| Ok <| evalExpr exp)
                Err error ->
                    Model model.pastExpressions model.exprStr (ExprEvaulated <| Err error)
        Change str ->
            case model.evaulatedExpr of
                ExprNotEvaulated ->
                    Model model.pastExpressions str ExprNotEvaulated
                    
                ExprEvaulated lastEvaulatedExpr ->
                    
                    Model (model.pastExpressions ++ [(model.exprStr, lastEvaulatedExpr)]) str ExprNotEvaulated
            


view : Model -> Html Msg
view model =
    div []
        [ div []
            (List.map (\pastExpression -> pastExpressionView pastExpression) model.pastExpressions)
        , div []
            [ case model.evaulatedExpr of 
                ExprNotEvaulated ->
                    div [] []
                ExprEvaulated result ->
                    div [] [ text <| model.exprStr ++ " = " ++ (resultToStr result) ]
            , input [ placeholder "Press enter to evaulate", value model.exprStr, onInput Change, onEnter Evaulate ] []
            ]
        ]

pastExpressionView : (String, (Result (List DeadEnd) Float)) -> Html Msg
pastExpressionView (str, result) =
    div []
        [ span [] [ text <| str ++ " = " ++ (resultToStr result)]
        ]

resultToStr : Result (List DeadEnd) Float -> String
resultToStr result =
    case result of
        Ok fl -> 
            String.fromFloat fl
        
        Err error -> 
            Debug.toString error
        
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)