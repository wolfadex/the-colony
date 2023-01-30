module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Http
import Lamdera
import Set
import Url


type alias LoginModel = 
    { loginEmail : String
    }


type alias AuthenticateModel = ()


type alias GameModel = ()


type Page
    = Login LoginModel
    | Authenticate AuthenticateModel
    | Game GameModel


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    }


type alias WorkOsProfile = 
    { id : String
    , connectionId : String
    , email : String
    , idpId : String
    }


type alias Session = 
    { clients : (Set.Set Lamdera.ClientId)
    , workOsProfile : (Maybe WorkOsProfile)
    }


type alias BackendModel =
    { sessions : (Dict.Dict Lamdera.SessionId Session)
    }


type LoginMsg
    = LoginEmailChanged String
    | LoginAttempt


type AuthenticateMsg
    = AuthenticateMsgNoOp


type GameMsg
    = GameMsgNoOp


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | LoginMessage LoginMsg
    | AuthenticateMessage AuthenticateMsg
    | GameMessage GameMsg


type ToBackend
    = NoOpToBackend
    | CreateSessionAttempt String
    | AuthenticateProfile String


type BackendMsg
    = NoOpBackendMsg
    | CreateSessionRepsonded (Result Http.Error Bool)
    | SignleSignOnResponded Lamdera.SessionId (Result Http.Error WorkOsProfile)
    | OnConnect Lamdera.SessionId Lamdera.ClientId
    | OnDisconnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | RedirectFrontend String