module Page.Login exposing
    ( init
    , update
    , view
    )

import Html
import Html.Attributes
import Html.Events
import Lamdera
import Types exposing (..)


init : ( LoginModel, FrontendEffect LoginMsg )
init =
    ( { loginEmail = "" }, EffNone )


update : LoginMsg -> LoginModel -> ( LoginModel, FrontendEffect LoginMsg )
update msg model =
    case msg of
        LoginEmailChanged email ->
            ( { model | loginEmail = email }, EffNone )

        LoginAttempt ->
            ( model
            , EffCreateSessionAttempt model.loginEmail
            )


view : LoginModel -> List (Html.Html LoginMsg)
view model =
    [ Html.input
        [ Html.Attributes.value model.loginEmail
        , Html.Attributes.type_ "email"
        , Html.Events.onInput LoginEmailChanged
        ]
        []
    , Html.button
        [ Html.Events.onClick LoginAttempt ]
        [ Html.text "Login" ]
    ]
