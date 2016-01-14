module Page.Login where

import Effects as Fx exposing (Effects)
import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Signal
import StartApp
import String
import Task

import Component.Header as Header

app = StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }

main = app.html

port tasks : Signal (Task.Task Fx.Never ())
port tasks =
    app.tasks

port title : String
port title = "Login"

redirectAddressMailbox = Signal.mailbox ""

port redirect : Signal String
port redirect = redirectAddressMailbox.signal

-- MODEL

type Status =
    Awaiting
    | Logging

type alias Model =
    { name : String
    , password : String
    , status : Status
    , message : Maybe String
    , header : Header.Model
    }

-- INIT

init : (Model, Effects Action)
init =
    let
        (header, headerFx) = Header.init
    in
        ({ name = ""
         , password = ""
         , status = Awaiting
         , message = Nothing
         , header = header
         },
         Fx.map UpdateHeader headerFx
        )

-- UPDATE

type Action =
    UpdateUsername String
    | UpdatePassword String
    | SubmitForm
    | Fail Http.Error
    | ServerResponse (Maybe String)
    | UpdateHeader Header.Action
    | Dummy -- Should never happen.

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        UpdateUsername user -> ({ model | name = user}, Fx.none)
        UpdatePassword pass -> ({ model | password = pass}, Fx.none)
        SubmitForm -> ({ model | status = Logging}, sendLogin model.name model.password)
        Fail err -> flip (,) Fx.none <|
                        { model
                        | message = Just (toString err)
                        , status = Awaiting
                        }
        ServerResponse Nothing -> (model, redirectToHome)
        ServerResponse (Just message) -> flip (,) Fx.none <|
                                            { model
                                            | message = Just message
                                            , status = Awaiting
                                            }
        UpdateHeader headerAction ->
            let
                (newHeader, headerFx) =
                    Header.update headerAction model.header
            in
                ( { model | header = newHeader }
                , Fx.map UpdateHeader headerFx
                )
        Dummy -> (model, Fx.none) -- Should never happen.

-- EFFECTS

redirectToHome : Effects Action
redirectToHome =
    Signal.send redirectAddressMailbox.address "/"
    |> Task.map (\_ -> Dummy)
    |> flip Task.onError (\_ -> Task.succeed Dummy)
    |> Fx.task

sendLogin : String -> String -> Effects Action
sendLogin username password =
    Http.send Http.defaultSettings
        { verb = "POST"
        , headers = [("Content-Type", "application/x-www-form-urlencoded")]
        , url = "/api/login"
        , body = formUrlEncoded [("username", username), ("password", password)]
        }
    |> Http.fromJson (Json.maybe Json.string)
    |> Task.map ServerResponse
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task

joinArgs: List (String, String) -> String
joinArgs args =
  String.join "&" (List.map queryPair args)

formUrlEncoded: List (String, String) -> Http.Body
formUrlEncoded args =
  args
    |> joinArgs
    |> Http.string

queryPair : (String,String) -> String
queryPair (key,value) =
  queryEscape key ++ "=" ++ queryEscape value

queryEscape : String -> String
queryEscape string =
  String.join "+" (String.split "%20" (Http.uriEncode string))

-- VIEW

(=>) = (,)

view : Signal.Address Action -> Model -> Html
view addr model =
    let isLogging = model.status == Logging

        isFormEmpty =
            String.isEmpty model.name || String.isEmpty model.password

        userMessage =
            case model.message of
                Nothing -> div [] []
                Just message -> div [] [ text message
                                       ]
        loginForm =
            Html.form
                [ onSubmit addr SubmitForm
                , action "javascript:void(0);"
                ]

                -- Form title
                [ h2 [] [ text "Login" ]
                -- Username
                , div
                    []
                    [ input
                        [ type' "text"
                        , placeholder "Nome"
                        , value model.name
                        , on "input" targetValue (Signal.message addr << UpdateUsername)
                        , required True
                        , disabled isLogging
                        , size 40
                        ]
                        []
                    ]
                -- Password
                , div
                    []
                    [ input
                        [ type' "password"
                        , placeholder "Senha"
                        , value model.password
                        , on "input" targetValue (Signal.message addr << UpdatePassword)
                        , required True
                        , disabled isLogging
                        , size 40
                        ]
                        []
                    ]
                -- Submit button
                , button
                    [ onClick addr SubmitForm
                    , disabled (isLogging || isFormEmpty)
                    ]
                    [ text "Login!" ]
                ]

        loginPage =
            div []
                [ horizontallyCentered
                    [ userMessage
                    , loginForm
                    , a [href "/"] [ text "Voltar para o inicio" ]
                    ]
                ]
    in
        Header.view (Signal.forwardTo addr UpdateHeader) model.header
            [ loginPage ]

horizontallyCentered : List Html -> Html
horizontallyCentered contents =
    div [ style ["text-align" => "center"] ] contents
