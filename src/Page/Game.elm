module Page.Game exposing
    ( init
    , update
    , view
    )

import Html
import Html.Attributes
import Html.Events
import Lamdera
import Types exposing (..)


init : ( GameModel, FrontendEffect GameMsg )
init =
    ( ()
    , EffNone
    )


update : GameMsg -> GameModel -> ( GameModel, FrontendEffect GameMsg )
update msg model =
    case msg of
        GameMsgNoOp ->
            ( model, EffNone )


view : GameModel -> List (Html.Html GameMsg)
view model =
    [ Html.text "Playing the game!" ]
