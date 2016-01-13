module Component.Header where

import Effects as Fx exposing (Effects)
import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task

type Model =
    Fetching
    | Failed String
    | Success
        { loggedUser : Maybe String
        , pendingReviews : Maybe Int
        }

init : (Model, Effects Action)
init = (Fetching, getLoggedUser)

-- UPDATE

type Action =
    Fail Http.Error
    | LoggedUser (Maybe String)
    | PendingReviews Int

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        Fail error -> (Failed ("Falhou em pegar informações de login: " ++ (toString error)), Fx.none)
        LoggedUser Nothing -> (Success { loggedUser = Nothing, pendingReviews = Nothing }, Fx.none)
        LoggedUser (Just userName) -> (Success { loggedUser = Just userName, pendingReviews = Nothing }, getPendingReviews)
        PendingReviews pending -> flip (,) Fx.none <|
            case model of
                Success {loggedUser, pendingReviews} -> (Success { loggedUser = loggedUser, pendingReviews = Just pending})
                _ -> model -- Should not get here.

-- EFFECTS

getLoggedUser : Effects Action
getLoggedUser =
    Http.get (Json.maybe Json.string) "/api/logged_user"
    |> Task.map LoggedUser
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task

getPendingReviews : Effects Action
getPendingReviews =
    Http.get Json.int "/api/pending_reviews"
    |> Task.map PendingReviews
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> List Html -> Html
view addr model contents =
  div []
    [ colored "#60B5CC"
        [ center [ header [ text "Mostruário de Jogos" ] ]
        , right (loggedMessages model)
        ]
    , div [] contents
    , footer
        [ text "Todo código escrito nesta página é livre! "
        , a [ href "http://www.github.com/VitorCBSB/idj-showcase" ] [ text "Dê uma olhada!" ]
        ]
    ]

loggedMessages model =
    case model of
        Fetching -> [ text "" ]
        Failed failedMessage -> [ text failedMessage ]
        Success {loggedUser, pendingReviews} ->
            case loggedUser of
                Nothing -> [ text "" ]
                Just user -> [ text ("Olá, " ++ user ++ "! ")
                             , pendingReviewsAnchor pendingReviews
                             ]

pendingReviewsAnchor : Maybe Int -> Html
pendingReviewsAnchor pendingQuantity =
    case pendingQuantity of
        Just p -> a [ href "/reviews" ] [ text ("Há " ++ (toString p) ++ " revisões pendentes.") ]
        Nothing -> text ""

header kids =
    div [ style [ "padding" => "10px 0"
                , "color" => "#333333"
                , "font-size" => "30px"
                , "margin" => "0"
                , "text-overflow" => "ellipsis" ] ] kids

footer kids =
    div [ style [ "text-align" => "center"
                , "margin-top" => "4em"
                , "border-top" => "1px solid #eeeeee"
                , "padding" => "2em 0"
                , "color" => "#bbbbbb" ] ] kids

colored color kids =
    div [ style [ "background-color" => color ] ] kids

center kids =
    div [ style [ "text-align" => "center" ] ] kids

right kids =
    div [ style [ "text-align" => "right" ] ] kids
