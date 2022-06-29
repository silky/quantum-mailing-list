module Main exposing (..)

import Browser
import Html            exposing (Html, div, text, input, label, h1, button)
import Html.Attributes exposing (type_, id, name, for, min, max, value)
import Html.Events     exposing (onInput, onClick)

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
  }

type Msg
  = NoOp
  | AlphaDragged String
  | BetaDragged String
  | Measure

init : () -> (Model, Cmd Msg)
init _ =
    ( { yesChecked = False
      , noChecked = False
      , alpha     = 0.5
      , beta      = 0.5
      }
    , Cmd.none
    )

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
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Do you want to join my quantum mailing list?" ]
    , div [ ] [ input [ id "yes", type_ "checkbox" ] []
              , label [ for "yes" ] [ text "Yes" ]
              , text ","
              , label [ for "alpha" ] [ text "|α|²" ]
              , input [ id "alpha", type_ "range", value (String.fromFloat (model.alpha*100)), onInput AlphaDragged, Html.Attributes.min "0", Html.Attributes.max "100" ] []
              ]
    , div [ ] [ input [ id "no", type_ "checkbox" ] []
              , label [ for "no" ] [ text "No" ]
              , text ","
              , label [ for "beta" ] [ text "|β|²" ]
              , input [ id "beta", type_ "range", value (String.fromFloat (model.beta*100)), onInput BetaDragged, Html.Attributes.min "0", Html.Attributes.max "100" ] []
            ]

    , button [ onClick Measure ] [ text "Join!" ]
    ]
