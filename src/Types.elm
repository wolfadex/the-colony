module Types exposing (..)

import AppUrl
import Browser
import Browser.Navigation
import Dict
import Http
import Json.Decode
import Lamdera exposing (..)
import Set
import Url exposing (Url)


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    }


type Page
    = Login LoginModel
    | Authenticate AuthenticateModel
    | Game GameModel


type Route
    = RouteLogin
    | RouteAuthenticate String
    | RouteGame


urlToRoute : Url.Url -> Maybe Route
urlToRoute url =
    let
        appUrl : AppUrl.AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [] ->
            Just RouteLogin

        [ "authenticated" ] ->
            case Dict.get "code" appUrl.queryParameters of
                Just [ code ] ->
                    Just (RouteAuthenticate code)

                _ ->
                    Nothing

        [ "game" ] ->
            Just RouteGame

        _ ->
            Nothing


type alias LoginModel =
    { loginEmail : String
    }


type alias AuthenticateModel =
    ()


type alias GameModel =
    ()


type alias BackendModel =
    { sessions : Dict.Dict SessionId Session
    }


type alias Session =
    { clients : Set.Set ClientId
    , workOsProfile : Maybe WorkOsProfile
    }


type alias WorkOsSession =
    { id : String
    , email : String
    , expiresAt : String -- "2020-08-13T05:50:00.000Z"
    , link : String -- "https://auth.workos.com/passwordless/4TeRexuejWCKs9rrFOIuLRYEr/confirm"
    }


type alias WorkOsProfile =
    { id : String -- "prof_01DMC79VCBZ0NY2099737PSVF1",
    , connectionId : String --"conn_01E4ZCR3C56J083X43JQXF3JK5",

    --  , connectionType : WorkOsConnectionType
    --  , organizationId : String -- "org_01EHWNCE74X7JSDV0X3SZ3KJNY",
    , email : String -- "todd@foo-corp.com",

    --  , firstName : String --"Todd",
    --  , lastName : String -- "Rundgren",
    , idpId : String -- "00u1a0ufowBJlzPlk357",
    }


decodeProfile : Json.Decode.Decoder WorkOsProfile
decodeProfile =
    Json.Decode.map4 WorkOsProfile
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "connection_id" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
        (Json.Decode.field "idp_id" Json.Decode.string)


type WorkOsConnectionType
    = ADFSSAML
    | ADPOIDC
    | Auth0SAML
    | AzureSAML
    | CASSAML
    | ClassLinkSAML
    | CloudflareSAML
    | CyberArkSAML
    | DuoSAML
    | GenericOIDC
    | GenericSAML
    | GoogleOAuth
    | GoogleSAML
    | JumpCloudSAML
    | KeycloakSAML
    | LastPassSAML
    | LoginGovOIDC
    | MicrosoftOAuth
    | MiniOrangeSAML
    | NetIqSAML
    | OktaSAML
    | OneLoginSAML
    | OracleSAML
    | PingFederateSAML
    | PingOneSAML
    | SalesforceSAML
    | SimpleSamlPhpSAML
    | ShibbolethSAML
    | ShibbolethGenericSAML
    | VMwareSAML


decodeWorkOsConnectionType : Json.Decode.Decoder WorkOsConnectionType
decodeWorkOsConnectionType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\type_ ->
                case type_ of
                    "ADFSSAML" ->
                        Json.Decode.succeed ADFSSAML

                    "ADPOIDC" ->
                        Json.Decode.succeed ADPOIDC

                    "Auth0SAML" ->
                        Json.Decode.succeed Auth0SAML

                    "AzureSAML" ->
                        Json.Decode.succeed AzureSAML

                    "CASSAML" ->
                        Json.Decode.succeed CASSAML

                    "ClassLinkSAML" ->
                        Json.Decode.succeed ClassLinkSAML

                    "CloudflareSAML" ->
                        Json.Decode.succeed CloudflareSAML

                    "CyberArkSAML" ->
                        Json.Decode.succeed CyberArkSAML

                    "DuoSAML" ->
                        Json.Decode.succeed DuoSAML

                    "GenericOIDC" ->
                        Json.Decode.succeed GenericOIDC

                    "GenericSAML" ->
                        Json.Decode.succeed GenericSAML

                    "GoogleOAuth" ->
                        Json.Decode.succeed GoogleOAuth

                    "GoogleSAML" ->
                        Json.Decode.succeed GoogleSAML

                    "JumpCloudSAML" ->
                        Json.Decode.succeed JumpCloudSAML

                    "KeycloakSAML" ->
                        Json.Decode.succeed KeycloakSAML

                    "LastPassSAML" ->
                        Json.Decode.succeed LastPassSAML

                    "LoginGovOIDC" ->
                        Json.Decode.succeed LoginGovOIDC

                    "MicrosoftOAuth" ->
                        Json.Decode.succeed MicrosoftOAuth

                    "MiniOrangeSAML" ->
                        Json.Decode.succeed MiniOrangeSAML

                    "NetIqSAML" ->
                        Json.Decode.succeed NetIqSAML

                    "OktaSAML" ->
                        Json.Decode.succeed OktaSAML

                    "OneLoginSAML" ->
                        Json.Decode.succeed OneLoginSAML

                    "OracleSAML" ->
                        Json.Decode.succeed OracleSAML

                    "PingFederateSAML" ->
                        Json.Decode.succeed PingFederateSAML

                    "PingOneSAML" ->
                        Json.Decode.succeed PingOneSAML

                    "SalesforceSAML" ->
                        Json.Decode.succeed SalesforceSAML

                    "SimpleSamlPhpSAML" ->
                        Json.Decode.succeed SimpleSamlPhpSAML

                    "ShibbolethSAML" ->
                        Json.Decode.succeed ShibbolethSAML

                    "ShibbolethGenericSAML" ->
                        Json.Decode.succeed ShibbolethGenericSAML

                    "VMwareSAML" ->
                        Json.Decode.succeed VMwareSAML

                    _ ->
                        Json.Decode.fail ("Unknown WorkOS connection type: " ++ type_)
            )


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | LoginMessage LoginMsg
    | AuthenticateMessage AuthenticateMsg
    | GameMessage GameMsg


type FrontendEffect msg
    = EffCreateSessionAttempt String
    | EffAuthenticateProfile String
    | EffNone


frontendEffectToCommand : (msg -> FrontendMsg) -> FrontendEffect msg -> Cmd FrontendMsg
frontendEffectToCommand mapEff eff =
    case eff of
        EffCreateSessionAttempt email ->
            Lamdera.sendToBackend (CreateSessionAttempt email)

        EffAuthenticateProfile code ->
            Lamdera.sendToBackend (AuthenticateProfile code)

        EffNone ->
            Cmd.none


type LoginMsg
    = LoginEmailChanged String
    | LoginAttempt


type AuthenticateMsg
    = AuthenticateMsgNoOp


type GameMsg
    = GameMsgNoOp


type ToBackend
    = NoOpToBackend
    | CreateSessionAttempt String
    | AuthenticateProfile String


type BackendMsg
    = NoOpBackendMsg
    | CreateSessionRepsonded (Result Http.Error Bool)
    | SignleSignOnResponded SessionId (Result Http.Error WorkOsProfile)
    | OnConnect SessionId ClientId
    | OnDisconnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | RedirectFrontend String
