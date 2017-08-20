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


type alias Line =
     { text : String
     , textInput : String
     , terms : List Term
     , syllables: Int
     }


initialLine : Line
initialLine =
    { text = ""
    , textInput = ""
    , terms = []
    , syllables = 0
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
    | EnterLineText Int String
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

        EnterLineText i text ->
            ( (saveLineText model i text), Cmd.none )

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


saveLineText: Model -> Int -> String -> Model
saveLineText model lineNum lineText =
    let
        haiku =
            model.haiku
        inLine =
            getHaikuLine haiku lineNum
        tokenized =
            tokenizeLine model lineText
        outLine =
            { inLine | text = lineText, terms = tokenized }
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


inputSyllableValue : Maybe Int -> String
inputSyllableValue syl =
    case syl of
        Just i ->
            (toString i)

        Nothing ->
            ""


makeTerm : Model -> String -> Term
makeTerm model text =
    Term text (normalizeTerm text)


tokenizeLine : Model -> String -> List Term
tokenizeLine model line =
    let
        nonEmpty x =
            not (String.isEmpty x)
        words =
            Regex.split Regex.All (regex "[- ]+") line
            |> List.filter nonEmpty

    in
        List.map (makeTerm model) words


maybeSum : Maybe Int -> Maybe Int -> Maybe Int
maybeSum a b =
    Maybe.map2 (+) a b


syllableCountForTokens : Model -> List Term -> Maybe Int
syllableCountForTokens model terms =
    if List.isEmpty terms then
        Nothing
    else
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
    |> List.sort


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
        div [ class "input-group input-group-lg" ]
            [ span [ class "input-group-addon" ]
                   [ viewLineStatus model line ]
            , input
                  [ class "form-control"
                  , type_ "text"
                  , placeholder "Enter a haiku line"
                  , value line.text
                  , onInput (EnterLineText lineNum)
                  ]
                  []
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
        case syllables of
            Just count ->
                if count == target then
                    span [ class "haiku-status-good" ] [ text (toString count) ]
                else
                    span [ class "haiku-status-bad" ] [ text (toString count) ]
            Nothing ->
                span [ class "haiku-status-unknown" ] [ text "?" ]


viewHaiku : Model -> Html Msg
viewHaiku model =
    div [ class "row" ]
        [
          div [ class "col-md-8" ]
              [ viewLine model 0
              , viewLine model 1
              , viewLine model 2
              ]
        , div [ class "col-md-4 terms" ]
              [ viewTerms model ]
        ]


viewTerms : Model -> Html Msg
viewTerms model =
    let
        terms =
            uniqueTerms model
        listItemize term =
            li [ ]
                [ input [ type_ "number"
                        , value (inputSyllableValue (syllablesForTerm model term))
                        , onInput (SetSyllableCount term) ] []
                , text term
                ]
    in
        ul [] (List.map listItemize terms)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewHeader "Haiku Maker"
        , viewAlert CloseAlert model.alertMessage
        , viewHaiku model
        , div [ class "debug" ]
              [ text (toString model.haiku) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, loadDict )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
