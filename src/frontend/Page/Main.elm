module Page.Main where

import Effects as Fx exposing (Effects)
import Html
import StartApp

import Component.Header as Header

app =
  StartApp.start
  { init = init
  , view = view
  , update = update
  , inputs = []
  }

main = app.html

type alias Model =
    { welp : Int
    }

init : (Model, Effects a)
init = (Model 0, Fx.none)

update : a -> Model -> (Model, Effects a)
update _ model =
    (model, Fx.none)

view _ model = Html.text ("Hello World " ++ toString model.welp)
