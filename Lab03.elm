module Lab03 exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


-- MVU
-- Model: Data of app
-- View: What the user sees
-- Msg: User interaction
-- Update: How the Model changes as a response to a Msg


type alias Model =
    { p1Choice : String
    , p2Choice : String
    , p3Choice : String
    , p1Score : Int
    , p2Score : Int
    , p3Score : Int
    }


init : Model
init =
    { p1Choice = ""
    , p2Choice = ""
    , p3Choice = ""
    , p1Score = 0
    , p2Score = 0
    , p3Score = 0
    }


type Msg
    = MsgClickP1A
    | MsgClickP1B
    | MsgClickP1C
    | MsgClickP2A
    | MsgClickP2B
    | MsgClickP2C
    | MsgClickP3A
    | MsgClickP3B
    | MsgClickP3C
    | MsgContinue
    | MsgReset


update : Msg -> Model -> Model
update msg model =
    let
        updateScoreOfWinner : Model -> Model
        updateScoreOfWinner current =
            if current.p1Choice /= "" && current.p2Choice /= "" && current.p3Choice /= "" then
                -- We have a winner
                if (current.p1Choice == "Rock") && (current.p2Choice == "Scissors" && current.p3Choice == "Scissors") then
                    { current | p1Score = current.p1Score + 1 }
                else if (current.p1Choice == "Scissors") && (current.p2Choice == "Paper" && current.p3Choice == "Paper") then
                    { current | p1Score = current.p1Score + 1 }
                else if (current.p1Choice == "Paper") && (current.p2Choice == "Rock" && current.p3Choice == "Rock") then
                    { current | p1Score = current.p1Score + 1 }
                else if (current.p2Choice == "Rock") && (current.p1Choice == "Scissors" && current.p3Choice == "Scissors") then
                    { current | p2Score = current.p2Score + 1 }
                else if (current.p2Choice == "Scissors") && (current.p1Choice == "Paper" && current.p3Choice == "Paper") then
                    { current | p2Score = current.p2Score + 1 }
                else if (current.p2Choice == "Paper") && (current.p1Choice == "Rock" && current.p3Choice == "Rock") then
                    { current | p2Score = current.p2Score + 1 }
                else if (current.p3Choice == "Rock") && (current.p1Choice == "Scissors" && current.p2Choice == "Scissors") then
                    { current | p3Score = current.p3Score + 1 }
                else if (current.p3Choice == "Scissors") && (current.p1Choice == "Paper" && current.p2Choice == "Paper") then
                    { current | p3Score = current.p3Score + 1 }
                else if (current.p3Choice == "Paper") && (current.p1Choice == "Rock" && current.p2Choice == "Rock") then
                    { current | p3Score = current.p3Score + 1 }
                else 
                    { current | p3Score = current.p3Score + 0 }
            
            else
                -- Still waiting for choice/s
                current
    in
    case msg of
        MsgClickP1A ->
            { model | p1Choice = "Rock" }
            |> updateScoreOfWinner
    
        MsgClickP1B ->
            { model | p1Choice = "Scissors" }
            |> updateScoreOfWinner
    
        MsgClickP1C ->
            { model | p1Choice = "Paper" }
            |> updateScoreOfWinner
    
        MsgClickP2A ->
            { model | p2Choice = "Rock" }
            |> updateScoreOfWinner
    
        MsgClickP2B ->
            { model | p2Choice = "Scissors" }
            |> updateScoreOfWinner
    
        MsgClickP2C ->
            { model | p2Choice = "Paper" }
            |> updateScoreOfWinner
        
        MsgClickP3A ->
            { model | p3Choice = "Rock" }
            |> updateScoreOfWinner
    
        MsgClickP3B ->
            { model | p3Choice = "Scissors" }
            |> updateScoreOfWinner
        
        MsgClickP3C ->
            { model | p3Choice = "Paper" }
            |> updateScoreOfWinner
        
        MsgContinue ->
            { model | p1Choice = "", p2Choice = "", p3Choice = "" }
        
        MsgReset ->
            init


view : Model -> Html Msg
view model =
    let
        p1Elems =
            if model.p1Choice /= "" then
                [ text " is done" ]
            else
                [ button [onClick MsgClickP1A] [text "Rock"]
                , button [onClick MsgClickP1B] [text "Scissors"]   
                , button [onClick MsgClickP1C] [text "Paper"]
                ]
        
        p2Elems =
            if model.p2Choice /= "" then
                [ text " is done" ]
            else
                [ button [onClick MsgClickP2A] [text "Rock"]
                , button [onClick MsgClickP2B] [text "Scissors"]
                , button [onClick MsgClickP2C] [text "Paper"]
                ]
    
        p3Elems =
            if model.p3Choice /= "" then
                [ text " is done" ]
            else
                [ button [onClick MsgClickP3A] [text "Rock"]
                , button [onClick MsgClickP3B] [text "Scissors"]
                , button [onClick MsgClickP3C] [text "Paper"]
                ]
            
        children =
            [ div [] ([text ("Player 1 (" ++ String.fromInt model.p1Score ++ ") ")
                    ] ++ p1Elems)
            , div [] ([text ("Player 2 (" ++ String.fromInt model.p2Score ++ ") ")
                    ] ++ p2Elems)
            , div [] ([text ("Player 3 (" ++ String.fromInt model.p3Score ++ ") ")
                    ] ++ p3Elems)
            ]

        getWinner : String -> String -> String -> Int
        getWinner p1Choice p2Choice p3Choice =
            if (p1Choice == "Scissors") && (p2Choice == "Scissors") && (p3Choice == "Rock") then
                3
            else if (p1Choice == "Paper") && (p2Choice == "Paper") && (p3Choice == "Scissors") then
                3
            else if (p1Choice == "Rock") && (p2Choice == "Rock") && (p3Choice == "Paper") then
                3
            else if (p2Choice == "Scissors") && (p3Choice == "Scissors") && (p1Choice == "Rock") then
                1
            else if (p2Choice == "Paper") && (p3Choice == "Paper") && (p1Choice == "Scissors") then
                1
            else if (p2Choice == "Rock") && (p3Choice == "Rock") && (p1Choice == "Paper") then
                1
            else if (p1Choice == "Scissors") && (p3Choice == "Scissors") && (p2Choice == "Rock") then
                2
            else if (p1Choice == "Paper") && (p3Choice == "Paper") && (p2Choice == "Scissors") then
                2
            else
                2
    in
    if model.p1Score == 5 then
        div []
            [ text "Player 1 wins"
            , button [onClick MsgReset] [ text "Reset" ]
            ]
    else if model.p2Score == 5 then
        div []
            [ text "Player 2 wins"
            , button [onClick MsgReset] [ text "Reset" ]
            ]
    else if model.p3Score == 5 then
        div []
            [ text "Player 3 wins"
            , button [onClick MsgReset] [ text "Reset" ]
            ]
-- possible draw cases
    else if model.p1Choice == "Rock" && model.p2Choice == "Rock" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Scissors" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Paper" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Rock" && model.p2Choice == "Rock" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Rock" && model.p2Choice == "Scissors" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Rock" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Scissors" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Paper" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Scissors" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Paper" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Rock" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Rock" && model.p2Choice == "Paper" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Rock" && model.p2Choice == "Scissors" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Rock" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Paper" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Paper" && model.p2Choice == "Scissors" && model.p3Choice == "Rock" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Rock" && model.p2Choice == "Paper" && model.p3Choice == "Scissors" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else if model.p1Choice == "Scissors" && model.p2Choice == "Rock" && model.p3Choice == "Paper" then
        -- Show results
        div []
            [ div [] [text ("Draw")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
-- end of draw states

    else if model.p1Choice /= "" && model.p2Choice /= "" && model.p3Choice /= "" then
        -- Show results
        div []
            [ div [] [text ("Player " ++ (String.fromInt <| getWinner model.p1Choice model.p2Choice model.p3Choice) ++ " wins")]
            , div [] [text ("Player 1: " ++ model.p1Choice)]
            , div [] [text ("Player 2: " ++ model.p2Choice)]
            , div [] [text ("Player 3: " ++ model.p3Choice)]
            , button [onClick MsgContinue] [text "Continue"]
            ]
    else
        -- Show screen with buttons
        div [] children
    

main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
