import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = \_ -> Sub.none
  , view = view
  }

init : () -> (Model, Cmd msg)
init _ = (Init, Cmd.none)

type Model = Init

type Msg = Msg

update msg model =
  (model, Cmd.none)

view model =
  div []
    [ text "Hello Elm"
    ]