module Page.Authenticate exposing
    ( init
    , update
    , view
    )

import Html
import Html.Attributes
import Html.Events
import Lamdera
import Types exposing (..)


init : String -> ( AuthenticateModel, FrontendEffect AuthenticateMsg )
init code =
    ( ()
    , EffAuthenticateProfile code
    )


update : AuthenticateMsg -> AuthenticateModel -> ( AuthenticateModel, FrontendEffect AuthenticateMsg )
update msg model =
    case msg of
        AuthenticateMsgNoOp ->
            ( model, EffNone )


view : AuthenticateModel -> List (Html.Html AuthenticateMsg)
view model =
    [ Html.text "signing in..." ]
