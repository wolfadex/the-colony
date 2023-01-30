module Backend exposing (..)

import Dict
import Env
import Http
import Json.Decode
import Json.Encode
import Lamdera exposing (ClientId, SessionId)
import Set
import Task
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        CreateSessionRepsonded _ ->
            ( model, Cmd.none )

        SignleSignOnResponded sessionId (Ok profile) ->
            case Dict.get sessionId model.sessions of
                Nothing ->
                    ( model, Cmd.none )

                Just session ->
                    ( { model
                        | sessions =
                            Dict.insert sessionId
                                { session | workOsProfile = Just profile }
                                model.sessions
                      }
                    , sendToSession (RedirectFrontend "/game") session
                    )

        SignleSignOnResponded _ (Err err) ->
            ( model, Cmd.none )

        OnConnect sessionId clientId ->
            ( { model
                | sessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Nothing ->
                                    Just { clients = Set.singleton clientId, workOsProfile = Nothing }

                                Just session ->
                                    Just { session | clients = Set.insert clientId session.clients }
                        )
                        model.sessions
              }
            , case Dict.get sessionId model.sessions of
                Nothing ->
                    Cmd.none

                Just session ->
                    case session.workOsProfile of
                        Nothing ->
                            Cmd.none

                        Just _ ->
                            Lamdera.sendToFrontend clientId (RedirectFrontend "/game")
            )

        OnDisconnect sessionId clientId ->
            ( { model
                | sessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Nothing ->
                                    Nothing

                                Just session ->
                                    let
                                        remainingClients : Set.Set ClientId
                                        remainingClients =
                                            Set.remove clientId session.clients
                                    in
                                    if Set.isEmpty remainingClients then
                                        Nothing

                                    else
                                        Just { session | clients = remainingClients }
                        )
                        model.sessions
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        CreateSessionAttempt email ->
            ( model
            , createSession email
            )

        AuthenticateProfile code ->
            ( model
            , authenticate sessionId code
            )


authenticate : SessionId -> String -> Cmd BackendMsg
authenticate sessionId code =
    Http.request
        { method = "POST"
        , url = workOsUrl ++ "/sso/token"
        , headers = [ workOsAuth ]
        , body =
            [ ( "client_id", Json.Encode.string Env.workosClientId )
            , ( "client_secret", Json.Encode.string Env.workosApiKey )
            , ( "grant_type", Json.Encode.string "authorization_code" )
            , ( "code", Json.Encode.string code )
            ]
                |> Json.Encode.object
                |> Http.jsonBody
        , expect = Http.expectJson (SignleSignOnResponded sessionId) (Json.Decode.field "profile" decodeProfile)
        , timeout = Nothing
        , tracker = Nothing
        }


createSession : String -> Cmd BackendMsg
createSession email =
    Http.task
        { method = "POST"
        , url = workOsUrl ++ "/passwordless/sessions"
        , body =
            [ Just ( "email", Json.Encode.string email )
            , Just ( "type", Json.Encode.string "MagicLink" )
            , case Env.mode of
                Env.Development ->
                    Just ( "redirect_uri", Json.Encode.string "http://localhost:8000/authenticated" )

                Env.Production ->
                    Nothing
            ]
                |> List.filterMap identity
                |> Json.Encode.object
                |> Http.jsonBody
        , headers = [ workOsAuth ]
        , resolver = Http.stringResolver (jsonResolver decodeWorkOsSession)
        , timeout = Nothing
        }
        |> Task.mapError (Debug.log "session err 1")
        |> Task.andThen
            (\workOsSession ->
                Http.task
                    { method = "POST"
                    , headers = [ workOsAuth ]
                    , url = workOsUrl ++ "/passwordless/sessions/" ++ workOsSession.id ++ "/send"
                    , body =
                        [ ( "email", Json.Encode.string workOsSession.email )
                        , ( "type", Json.Encode.string "MagicLink" )
                        ]
                            |> Json.Encode.object
                            |> Http.jsonBody
                    , resolver = Http.stringResolver (jsonResolver Json.Decode.bool)
                    , timeout = Nothing
                    }
            )
        |> Task.mapError (Debug.log "session err 2")
        |> Task.attempt CreateSessionRepsonded


workOsUrl : String
workOsUrl =
    case Env.mode of
        Env.Development ->
            "http://localhost:8001/https://api.workos.com"

        Env.Production ->
            "https://api.workos.com"


workOsAuth : Http.Header
workOsAuth =
    Http.header "Authorization" ("Bearer " ++ Env.workosApiKey)


jsonResolver : Json.Decode.Decoder a -> Http.Response String -> Result Http.Error a
jsonResolver decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err err ->
                    Err (Http.BadBody (Json.Decode.errorToString err))

                Ok a ->
                    Ok a


decodeWorkOsSession : Json.Decode.Decoder WorkOsSession
decodeWorkOsSession =
    Json.Decode.map4 WorkOsSession
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "expires_at" Json.Decode.string)
        (Json.Decode.field "link" Json.Decode.string)


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect OnConnect
        , Lamdera.onDisconnect OnDisconnect
        ]


sendToSession : ToFrontend -> Session -> Cmd BackendMsg
sendToSession toFrontendMsg session =
    session.clients
        |> Set.toList
        |> List.map (\clientId -> Lamdera.sendToFrontend clientId toFrontendMsg)
        |> Cmd.batch
