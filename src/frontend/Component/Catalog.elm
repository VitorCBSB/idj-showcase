module Component.Catalog where

import Effects as Fx exposing (Effects)
import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import String
import Task

import Types.GameInfo exposing (..)

type Model =
    Loading
    | Failed Http.Error
    | Loaded
        { games : List GameInfo
        , query : String
        }

init : (Model, Effects Action)
init = (Loading, getGamesList)

type Action =
    Load (List GameInfo)
    | Fail Http.Error
    | Query String

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Fail httpError -> (Failed httpError, Fx.none)
        Load gamesList ->
            (Loaded
                { games = gamesList
                , query = ""
                },
             Fx.none)
        Query query ->
            flip (,) Fx.none <|
            case model of
                Loaded currentData -> Loaded { currentData | query = query }
                Loading -> model
                Failed _ -> model

filterSearch : String -> List GameInfo -> List GameInfo
filterSearch query gameList =
    let lowerQuery = String.toLower query in
    let matchesQuery gameInfo =
        let lowerTitle = String.toLower (gameInfo.title) in
        lowerQuery `String.contains` lowerTitle
    in
    List.filter matchesQuery gameList

-- EFFECTS

getGamesList : Effects Action
getGamesList =
    Http.get (Json.list decodeGameInfo) "/api/games"
    |> Task.map Load
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view addr model =
    body
        [ style
            [ "font-size" => "18px"
            , "max-width" => "800px"
            , "margin" => "40px auto"
            , "line-height" => "1.6"
            ]
        ]
        [ h1 [] [ text "Lista de Jogos" ]
        , div [] <|
            case model of
                Failed httpError ->
                    [ p [] [ text "Problema ao carregar lista de jogos!" ]
                    , p [] [ text (toString httpError) ]
                    ]
                Loading ->
                    [ p [] [ text "Carregando..." ]
                    ]
                Loaded ({games, query} as gameInfo) ->
                    [ input
                        [ placeholder "Buscar"
                        , value query
                        , on "input" targetValue (Signal.message addr << Query)
                        , autofocus True
                        , style [ "width" => "800px"
                                , "font-size" => "1.3em"
                                ]
                        ]
                        []
                    , div [] (List.map viewGameInfo (filterSearch query games))
                    ]
        ]

viewGameInfo : GameInfo -> Html
viewGameInfo {id, year, title, description, awards} =
    table
        [ style [ "border-collapse" => "collapse"
                , "border" => "1px solid black"
                ]
        ]
        [ tbody
           []
           [ tr
                []
                [ td
                    [ rowspan 3
                    , style [ "width" => "320px"
                            , "vertical-align" => "top"
                            ]
                    ]
                    [ img
                        [ width 320
                        , height 240
                        , src ("/assets/" ++ toString id ++ ".png")
                        ]
                        []
                    ]
                , th
                    [ colspan 2
                    , style [ "font-size" => "1.3em" ]
                    ]
                    [ text title
                    ]
                ]
           , tr
                []
                [ td
                    []
                    [ text ("Ano: " ++ toString year)
                    , br [] []
                    , text "Download: link do download vem aqui"
                    ]
                , td
                    []
                    [ div [] (List.map viewAward awards)
                    ]
                ]
           , tr
                []
                [ td
                    [ colspan 2
                    ]
                    [ text description
                    ]
                ]
           ]
        ]

viewAward : Award -> Html
viewAward (place, category) =
    let trophyString = case place of
            1 -> "gold"
            2 -> "silver"
            3 -> "bronze"
            _ -> "Unknown placement: " ++ (toString place) in
    let titleText =
        case category of
            Game -> "Jogo"
            Art -> "Arte"
            Music -> "Musica"
            Programming -> "Programacao" in
    img
        [ src ("/assets/" ++ trophyString ++ ".jpg")
        , title titleText
        ]
        []
