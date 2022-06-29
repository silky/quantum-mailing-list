module Main exposing (..)

import Browser
import Html.Attributes exposing (type_, id, name, for, min, max, value, checked)
import Html.Events exposing     (onInput, onClick)
import Html exposing            (Html, div, text, input, label, h1, button)
import Random exposing          (Generator)


main =
  Browser.element
    { view          = view
    , init          = init
    , update        = update
    , subscriptions = \_ -> Sub.none
    }


type alias Model =
  { yesChecked : Bool
  , noChecked  : Bool
  , alpha      : Float
  , beta       : Float
  , subscribed : Maybe Bool
  }


type Msg
  = NoOp
  | AlphaDragged String
  | BetaDragged String
  | Measure
  | Measurement Float


init : () -> (Model, Cmd Msg)
init _ =
    ( { yesChecked = False
      , noChecked  = False
      , alpha      = 0.5
      , beta       = 0.5
      , subscribed = Nothing
      }
    , Cmd.none
    )


randomFloat : Generator Float
randomFloat = Random.float 0 1


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)

    AlphaDragged v ->
      let new = Maybe.withDefault (model.alpha) (Maybe.map (\x -> x / 100) (String.toFloat v))
      in ( { model | alpha = new, beta = 1 - new }, Cmd.none)

    BetaDragged v ->
      let new = Maybe.withDefault (model.beta) (Maybe.map (\x -> x / 100) (String.toFloat v))
      in ( { model | beta = new, alpha = 1 - new }, Cmd.none)

    Measure ->
      (model, Random.generate Measurement randomFloat)

    -- Our approach will be to generate a number, x, between 0 and 1, then
    -- pick say alpha = a then beta = 1 - a and so if x < a pick "Yes",
    -- otherwise "No".
    Measurement x ->
      let
          subscribed = x < model.alpha
      in ({ model | subscribed = Just subscribed }, Cmd.none)



view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Do you want to join my quantum mailing list?" ]
    , div [ ] [ input [ id "yes", type_ "checkbox", checked True ] []
              , label [ for "yes" ] [ text "Yes" ]
              , text ","
              , label [ for "alpha" ] [ text "|α|²" ]
              , input [ id "alpha", type_ "range", value (String.fromFloat (model.alpha*100)), onInput AlphaDragged, Html.Attributes.min "0", Html.Attributes.max "100" ] []
              ]
    , div [ ] [ input [ id "no", type_ "checkbox", checked True ] []
              , label [ for "no" ] [ text "No" ]
              , text ","
              , label [ for "beta" ] [ text "|β|²" ]
              , input [ id "beta", type_ "range", value (String.fromFloat (model.beta*100)), onInput BetaDragged, Html.Attributes.min "0", Html.Attributes.max "100" ] []
            ]

    , case model.subscribed of
        Nothing -> button [ onClick Measure ] [ text "Join!" ]
        Just y  ->
          if y
          then div [] [ text "You're signed up." ]
          else div [] [ text "No worries; catch you in a different universe." ]
    ]
