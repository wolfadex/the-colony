module Frontend exposing (..)

import AppUrl
import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Dict
import Html
import Lamdera
import Page.Authenticate
import Page.Game
import Page.Login
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Browser.Navigation.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        initialRoute : Route
        initialRoute =
            case urlToRoute url of
                Nothing ->
                    RouteLogin

                Just route ->
                    route

        ( initialPage, initialCmd ) =
            case initialRoute of
                RouteLogin ->
                    Page.Login.init
                        |> Tuple.mapBoth
                            Login
                            (frontendEffectToCommand LoginMessage)

                RouteAuthenticate code ->
                    Page.Authenticate.init code
                        |> Tuple.mapBoth
                            Authenticate
                            (frontendEffectToCommand AuthenticateMessage)

                RouteGame ->
                    Page.Game.init
                        |> Tuple.mapBoth
                            Game
                            (frontendEffectToCommand GameMessage)
    in
    ( { key = key
      , page = initialPage
      }
    , initialCmd
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute : Route
                newRoute =
                    case urlToRoute url of
                        Nothing ->
                            RouteLogin

                        Just route ->
                            route

                ( page, cmd ) =
                    case newRoute of
                        RouteLogin ->
                            Page.Login.init
                                |> Tuple.mapBoth
                                    Login
                                    (frontendEffectToCommand LoginMessage)

                        RouteAuthenticate code ->
                            Page.Authenticate.init code
                                |> Tuple.mapBoth
                                    Authenticate
                                    (frontendEffectToCommand AuthenticateMessage)

                        RouteGame ->
                            Page.Game.init
                                |> Tuple.mapBoth
                                    Game
                                    (frontendEffectToCommand GameMessage)
            in
            ( { model | page = page }, cmd )

        ( NoOpFrontendMsg, _ ) ->
            ( model, Cmd.none )

        ( LoginMessage loginMsg, Login loginModel ) ->
            Page.Login.update loginMsg loginModel
                |> Tuple.mapBoth
                    (\page -> { model | page = Login page })
                    (frontendEffectToCommand LoginMessage)

        ( AuthenticateMessage authMsg, Authenticate authModel ) ->
            Page.Authenticate.update authMsg authModel
                |> Tuple.mapBoth
                    (\page -> { model | page = Authenticate page })
                    (frontendEffectToCommand AuthenticateMessage)

        _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        RedirectFrontend url ->
            ( model
            , Browser.Navigation.pushUrl model.key url
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "The Colony"
    , body =
        case model.page of
            Login pageModel ->
                Page.Login.view pageModel
                    |> List.map (Html.map LoginMessage)

            Authenticate pageModel ->
                Page.Authenticate.view pageModel
                    |> List.map (Html.map AuthenticateMessage)

            Game pageModel ->
                Page.Game.view pageModel
                    |> List.map (Html.map GameMessage)
    }
