module Haiku exposing (..)

import Regex exposing (split, regex)
import Char
import Dict
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing ( unique )
import Json.Decode as Decode


--- MODEL

dictUrl : String
dictUrl =
    "./terms.json"


type alias SyllableCounts =
    Dict.Dict String Int


type alias Term =
    { term : String
    , normalized : String }


type LineState = EnterText | Tokenized


type alias Line =
     { text : String
     , textInput : String
     , terms : List Term
     , syllables: Int
     , lineState : LineState
     }


initialLine : Line
initialLine =
    { text = ""
    , textInput = ""
    , terms = []
    , syllables = 0
    , lineState = EnterText
    }


-- Just one line for now
type alias Haiku =
    { line0 : Line
    , line1: Line
    , line2: Line
    }


initialHaiku : Haiku
initialHaiku =
    { line0 = { initialLine | syllables = 5 }
    , line1 = { initialLine | syllables = 7 }
    , line2 = { initialLine | syllables = 5 }
    }

type alias Model =
    { haiku: Haiku
    , syllables: SyllableCounts
    , alertMessage : Maybe String
    }


initialModel : Model
initialModel =
    { haiku = initialHaiku
    , syllables = Dict.fromList[("the", 1), ("rain", 1), ("falls", 1), ("down", 1), ("softly", 2)]
    , alertMessage = Nothing
    }


--- COMMANDS

fetchSyllableCounts : (Result Http.Error SyllableCounts -> msg) -> String -> Cmd msg
fetchSyllableCounts msg url =
    Decode.dict Decode.int
        |> Http.get url
        |> Http.send msg


loadDict : Cmd Msg
loadDict =
    fetchSyllableCounts LoadTerms dictUrl


--- UPDATE

type Msg
    = NewHaiku
    | LoadTerms (Result Http.Error (Dict.Dict String Int))
    | SetSyllableCount String String
    | ClearSyllableCount String
    | EditLine Int
    | EnterTempLineText Int String
    | SaveLineText Int
    | CancelLineText Int
    | CloseAlert
    | SetTermSyllables


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        NewHaiku ->
            ( { model | haiku = initialHaiku }, Cmd.none )

        LoadTerms (Ok terms) ->
            ( { model | syllables = terms }, Cmd.none )

        LoadTerms (Result.Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        SetSyllableCount term countStr ->
            case (String.toInt countStr) of
                Err msg ->
                    ( model, Cmd.none )

                Ok count ->
                    ( { model | syllables = Dict.insert term count model.syllables }, Cmd.none )

        ClearSyllableCount term ->
            ( { model | syllables = Dict.remove term model.syllables }, Cmd.none )

        EditLine i ->
            ( (setEditLine model i), Cmd.none )

        SaveLineText i ->
            ( (saveLineText model i), Cmd.none )

        CancelLineText i ->
            ( (cancelTempLineText model i), Cmd.none )

        EnterTempLineText i text ->
            ( (saveTempLineText model i text), Cmd.none )

        SetTermSyllables ->
            ( model, Cmd.none )


-- ACTIONS

getHaikuLine : Haiku -> Int -> Line
getHaikuLine haiku lineNum =
    case lineNum of
        0 -> haiku.line0
        1 -> haiku.line1
        2 -> haiku.line2
        _ -> initialLine


setHaikuLine : Haiku -> Int -> Line -> Haiku
setHaikuLine haiku lineNum newLine =
    case lineNum of
        0 -> { haiku | line0 = newLine }
        1 -> { haiku | line1 = newLine }
        2 -> { haiku | line2 = newLine }
        _ -> haiku


setEditLine: Model -> Int -> Model
setEditLine model lineNum =
    let
        haiku =
            model.haiku
        inLine =
            getHaikuLine haiku lineNum
        outLine =
            { inLine | textInput = "", lineState = EnterText }
    in
        { model | haiku = setHaikuLine haiku lineNum outLine }


saveLineText: Model -> Int -> Model
saveLineText model lineNum =
    let
        haiku =
            model.haiku
        inLine =
            getHaikuLine haiku lineNum
        tokenized =
            tokenizeLine model inLine.textInput
        outLine =
            { inLine | text = inLine.textInput, terms = tokenized, lineState = Tokenized }
    in
        { model | haiku = setHaikuLine haiku lineNum outLine }


saveTempLineText: Model -> Int -> String -> Model
saveTempLineText model lineNum text =
    let
        haiku =
            model.haiku
        inLine =
            getHaikuLine haiku lineNum
        outLine =
            { inLine | textInput = text }
    in
        { model | haiku = setHaikuLine haiku lineNum outLine }


cancelTempLineText: Model -> Int -> Model
cancelTempLineText model lineNum =
    let
        haiku =
            model.haiku
        inLine =
            getHaikuLine haiku lineNum
        outLine =
            { inLine | textInput = "", lineState = Tokenized }
    in
        { model | haiku = setHaikuLine haiku lineNum outLine }


normalizeTerm : String -> String
normalizeTerm term =
    term
    |> String.toLower
    |> String.filter Char.isLower


syllablesForTerm : Model -> String -> Maybe Int
syllablesForTerm model term =
    Dict.get term model.syllables


makeTerm : Model -> String -> Term
makeTerm model text =
    Term text (normalizeTerm text)


tokenizeLine : Model -> String -> List Term
tokenizeLine model line =
    let
        words =
            Regex.split Regex.All (regex "[- ]+") line

    in
        List.map (makeTerm model) words


maybeSum : Maybe Int -> Maybe Int -> Maybe Int
maybeSum a b =
    Maybe.map2 (+) a b


syllableCountForTokens : Model -> List Term -> Maybe Int
syllableCountForTokens model terms =
    terms
    |> List.map .normalized
    |> List.map (syllablesForTerm model)
    |> List.foldl maybeSum (Just 0)


normalizedTermsForLine : Model -> Int -> List String
normalizedTermsForLine model lineNum =
    let
        line =
            getHaikuLine model.haiku lineNum
        terms =
            line.terms
     in
         List.map .normalized terms


normalizedTerms : Model -> List String
normalizedTerms model =
    List.concat [ normalizedTermsForLine model 0
                , normalizedTermsForLine model 1
                , normalizedTermsForLine model 2]


uniqueTerms : Model -> List String
uniqueTerms model =
    model
    |> normalizedTerms
    |> unique


missingTerms : Model -> List String
missingTerms model =
    let
        isUnmapped model term =
            (syllablesForTerm model term) == Nothing
    in
        model
        |> uniqueTerms
        |> List.filter (isUnmapped model)


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.BadStatus response ->
            (toString response.status)

        Http.BadPayload message _ ->
            "Decoding Failed: " ++ message

        _ ->
            (toString error)


-- VIEW

viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewAlert : msg -> Maybe String -> Html msg
viewAlert msg alertMessage =
       case alertMessage of
           Just message ->
               div [ class "alert" ]
                   [ span [ class "close", onClick msg ] [ text "X" ]
                   , text message
                   ]
           Nothing  ->
               text ""

viewLine : Model -> Int -> Html Msg
viewLine model lineNum =
    let
        haiku =
            model.haiku
        line =
            getHaikuLine haiku lineNum
    in
        case line.lineState of
            EnterText ->
                p [ class "line-input" ]
                    [ input
                        [ type_ "text"
                        , placeholder "Enter a haiku line"
                        , value line.textInput
                        , onInput (EnterTempLineText lineNum)
                        ]
                        []
                    , button [ onClick (SaveLineText lineNum) ] [ text "Save" ]
                    , button [ onClick (CancelLineText lineNum) ] [ text "Cancel" ]
                    ]

            Tokenized ->
                p [ class "line-tokenized" ]
                    [viewLineStatus model line,
                     viewLineTerms model line
                    ]


viewLineStatus : Model -> Line -> Html Msg
viewLineStatus model line =
    let
        target =
            line.syllables
        terms =
            line.terms
        syllables =
            syllableCountForTokens model terms
    in
        if syllables == Just target then
            span [ class "glyphicon glyphicon-ok" ] []
        else
            span [ class "glyphicon glyphicon-remove" ] []


viewLineTerms : Model -> Line -> Html Msg
viewLineTerms model line =
    let
        terms =
            line.terms
    in
        span [class "terms"]
            (List.map (viewLineTerm model) terms)


viewLineTerm : Model -> Term -> Html Msg
viewLineTerm model term =
    let
        syllables =
            syllablesForTerm model term.normalized
        syllableSup =
            case syllables of
                Just i ->
                    a [ href "#", onClick (ClearSyllableCount term.normalized) ]
                      [ text (toString i) ]
                Nothing ->
                    text "?"
    in
        span []
            [ text term.term
            , sup
                  [ ]
                  [ syllableSup ]
            , text " "
            ]


viewHaiku : Model -> Html Msg
viewHaiku model =
    div [ class "row" ]
        [
          div [ class "col-md-8" ]
              [ viewLine model 0
              , viewLine model 1
              , viewLine model 2
              ]
        , div [ class "col-md-4" ]
              [ viewMissingTerms model ]
        ]


viewMissingTerms : Model -> Html Msg
viewMissingTerms model =
    let
        missing =
            missingTerms model
        listItemize term =
            li [] [ input [ type_ "number", onInput (SetSyllableCount term) ] []
                  , text term
                  ]
    in
        ul [] (List.map listItemize missing)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader "Haiku Maker"
        , viewAlert CloseAlert model.alertMessage
        , viewHaiku model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, loadDict )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
