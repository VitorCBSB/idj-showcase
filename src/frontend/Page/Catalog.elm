module Page.Catalog where

import Effects as Fx exposing (Effects)
import Json.Decode as Json
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import String
import Debug
import Task

import Types.GameInfo exposing (..)
import Component.Header as Header
import Component.Catalog as Catalog

app = StartApp.start
        { init = init
        , view = view
        , update = update
        , inputs = []
        }

main = app.html

port tasks : Signal (Task.Task Fx.Never ())
port tasks =
    app.tasks

-- MODEL

type alias Model =
    { header : Header.Model
    , catalog : Catalog.Model
    }

init : (Model, Effects Action)
init =
    let
        (header, headerFx) = Header.init
        (catalog, catalogFx) = Catalog.init
    in
        (Model header catalog,
            Fx.batch
                [ Fx.map UpdateHeader headerFx
                , Fx.map UpdateCatalog catalogFx
                ])

-- UPDATE

type Action =
    UpdateHeader Header.Action
    | UpdateCatalog Catalog.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        UpdateHeader headerAction ->
            let
                (newHeader, headerFx) =
                    Header.update headerAction model.header
            in
                ( { model | header = newHeader }
                , Fx.map UpdateHeader headerFx
                )
        UpdateCatalog catalogAction ->
            let
                (newCatalog, catalogFx) =
                    Catalog.update catalogAction model.catalog
            in
                ( { model | catalog = newCatalog }
                , Fx.map UpdateCatalog catalogFx
                )

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view addr model =
    Header.view (Signal.forwardTo addr UpdateHeader) model.header
        [ Catalog.view (Signal.forwardTo addr UpdateCatalog) model.catalog
        ]
