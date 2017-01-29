module Main exposing (..)

import Html exposing (Html, text, div, h3, button, hr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Http


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = initModel
        , subscriptions = (\_ -> Sub.none)
        }



{- Model -}


type Choice
    = Rock
    | Paper
    | Scissors
    | Lizard
    | Spock


type GameResult
    = PlayerWins
    | ComputerWins
    | Tie


type alias Score =
    Int


type Model
    = PlayerTurn Score
    | ComputerTurn Score Choice
    | GameOver Score Choice Choice GameResult


type Msg
    = NoOp
    | ChoiceClicked Choice
    | RandomNumber (Result Http.Error Int)
    | Reset


initModel : ( Model, Cmd Msg )
initModel =
    ( PlayerTurn 0, Cmd.none )



{- UPDATE -}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChoiceClicked choice ->
            updatePlayerChoice choice model

        RandomNumber (Ok num) ->
            updateComputerChoice num model

        RandomNumber (Err err) ->
            let
                _ =
                    Debug.log "Error: " err
            in
                ( model, Cmd.none )

        Reset ->
            initModel


updatePlayerChoice : Choice -> Model -> ( Model, Cmd Msg )
updatePlayerChoice choice model =
    case model of
        PlayerTurn score ->
            ( ComputerTurn score choice, getRandomNumber )

        GameOver score _ _ _ ->
            ( ComputerTurn score choice, getRandomNumber )

        ComputerTurn _ _ ->
            ( model, Cmd.none )


updateComputerChoice : Int -> Model -> ( Model, Cmd Msg )
updateComputerChoice num model =
    case model of
        ComputerTurn score pChoice ->
            let
                cChoice =
                    intToChoice num

                result =
                    determineResult pChoice cChoice

                newScore =
                    updateScore score result
            in
                ( GameOver newScore pChoice cChoice result, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateScore : Score -> GameResult -> Score
updateScore score result =
    case result of
        PlayerWins ->
            score + 1

        ComputerWins ->
            score - 1

        Tie ->
            score


determineResult : Choice -> Choice -> GameResult
determineResult player computer =
    if player == computer then
        Tie
    else if wins player computer then
        PlayerWins
    else
        ComputerWins


wins : Choice -> Choice -> Bool
wins a b =
    let
        match =
            (==) ( a, b )
    in
        List.any match winConditions


winConditions : List ( Choice, Choice )
winConditions =
    [ ( Rock, Scissors )
    , ( Rock, Lizard )
    , ( Paper, Rock )
    , ( Paper, Spock )
    , ( Scissors, Paper )
    , ( Scissors, Lizard )
    , ( Lizard, Paper )
    , ( Lizard, Spock )
    , ( Spock, Rock )
    , ( Spock, Scissors )
    ]


intToChoice : Int -> Choice
intToChoice num =
    case num of
        1 ->
            Rock

        2 ->
            Paper

        3 ->
            Scissors

        4 ->
            Lizard

        5 ->
            Spock

        _ ->
            intToChoice <| (rem num 5) + 1


getRandomNumber : Cmd Msg
getRandomNumber =
    let
        request =
            Http.get url Decode.int
    in
        Http.send RandomNumber request


url : String
url =
    "https://www.random.org/integers/?num=1&min=1&max=5&col=1&base=10&format=plain&rnd=new"



{- VIEW -}


view : Model -> Html Msg
view model =
    case model of
        PlayerTurn score ->
            div []
                [ gameView score Nothing Nothing Nothing
                , h3 [] [ text "Choose, wisely!" ]
                ]

        ComputerTurn score player ->
            gameView score (Just player) Nothing Nothing

        GameOver score player computer result ->
            gameView score (Just player) (Just computer) (Just result)


gameView : Score -> Maybe Choice -> Maybe Choice -> Maybe GameResult -> Html Msg
gameView score player computer gameResult =
    div []
        [ choiceButton Rock
        , choiceButton Paper
        , choiceButton Scissors
        , choiceButton Lizard
        , choiceButton Spock
        , divider
        , maybeRender showChoice player
        , maybeRender showChoice computer
        , maybeRender showResult gameResult
        , showPlayerScore score
        , maybeRender (\_ -> resetButton) gameResult
        ]


choiceButton : Choice -> Html Msg
choiceButton choice =
    button
        [ onClick (ChoiceClicked choice)
        , class ("choice " ++ choiceToClass choice)
        ]
        [ text <| choiceToString choice ]


choiceToString : Choice -> String
choiceToString choice =
    case choice of
        Rock ->
            "Rock"

        Paper ->
            "Paper"

        Scissors ->
            "Scissors"

        Lizard ->
            "Lizard"

        Spock ->
            "Spock"


choiceToClass : Choice -> String
choiceToClass =
    String.toLower << choiceToString


divider : Html a
divider =
    hr [] []


maybeRender : (a -> Html Msg) -> Maybe a -> Html Msg
maybeRender f a =
    Maybe.map f a
        |> Maybe.withDefault (div [] [])


showChoice : Choice -> Html Msg
showChoice choice =
    div [ class ("card " ++ choiceToClass choice) ]
        []


showResult : GameResult -> Html Msg
showResult result =
    div [] [ text <| resultToString result ]


resultToString : GameResult -> String
resultToString result =
    case result of
        PlayerWins ->
            "You won!!"

        ComputerWins ->
            "You lost :'("

        Tie ->
            "It's a tie."


showPlayerScore : Score -> Html Msg
showPlayerScore score =
    div [] [ text <| "Score: " ++ toString score ]


resetButton : Html Msg
resetButton =
    button
        [ onClick Reset
        , class "reset"
        ]
        [ text "Reset" ]
