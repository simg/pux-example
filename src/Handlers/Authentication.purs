module Handlers.Authentication where

import Foundation

import Data.Argonaut.Core
import Data.Argonaut.Encode

--import Prelude (($), (+), (-), const, show, map)
import Pux (EffModel)
import Pux.Html (Html, form, div, p, ul, li, h1, h2, span, button, text, input, select, option, label, a, img)
import Pux.Html.Attributes (className, type_, id_, value, style, height, width, maxLength, action, method, aria, src)
import Pux.Html.Attributes (label, title) as A
import Pux.Html.Events (FormEvent, onClick, onInput, onChange)
import Pux.Router (link)

import WebSocket

--import App.Action as App
import App.Utils

type State = {
  userId      :: Maybe String
, userName    :: Maybe String
, password    :: Maybe String
, userProfile :: Maybe UserProfile
, jwt         :: Maybe String
, roles       :: Array UserRole
, messages    :: Array String
}
--derive instance genericState :: Generic State
------------------------------------------------------
data Action = LoginClick
            | SetUserProfile UserProfile
            | UpdateUserName FormEvent
            | UpdatePass FormEvent
            | LoginSuccess String
            | LoginFail String
            | Nop

------------------------------------------------------
data UserRole = Unapproved | Admin
derive instance genericUserOption :: Generic UserRole

------------------------------------------------------
data Auth = Auth {
  email :: String
, pass  :: String
}
derive instance genericAuth :: Generic Auth

instance encodeJsonAuth :: EncodeJson Auth where
  encodeJson (Auth a) 
    =  "email" := a.email
    ~> "pass"  := a.pass
    ~> jsonEmptyObject

------------------------------------------------------
data UserProfile = UserProfile {
  uid   :: Int
, name  :: String
, email :: String
, icon  :: String
}    
derive instance genericUserProfile :: Generic UserProfile

instance encodeUserProfile :: EncodeJson UserProfile where
  encodeJson (UserProfile a) 
    =  "id"    := a.uid
    ~> "name"  := a.name
    ~> "email" := a.email
    ~> "icon"  := a.icon
    ~> jsonEmptyObject

instance userProfileIsForeign :: IsForeign UserProfile where
  read val = do
    uid   <- readInt    =<< readProp "id" val
    name  <- readString =<< readProp "name" val
    email <- readString =<< readProp "email" val
    icon  <- readString =<< readProp "icon" val
    pure $ UserProfile { uid:uid, name:name, email:email, icon:icon}    

------------------------------------------------------
init :: State
init = { 
  userId:      Nothing 
, userName:    Nothing
, userProfile: Nothing
, password:    Nothing
, jwt:         Nothing
, roles:       []
, messages:    []
}

------------------------------------------------------
icon :: State -> Html Action
icon state = 
  span [ className "authentication" ] [ c ]
  where
    c = case state.userProfile of 
          Nothing -> login
          Just (UserProfile profile) -> link "/my-account" [ className "" ] [ 
              a [ A.title profile.name, style [ (Tuple "display" "block" ), ( Tuple "margin-top" "8px" ) ] ] [ 
                img [ src profile.icon, width "48", height "48" ] []
              ]  
              --text $ "User: " <> profile.name 
            ]
    login = link "/login" [ className ""] [
            span [ className "oi oi-account-login", A.title "Sign in", aria "hidden" "true" ] []
          , span [ className "text" ] [ text "Sign in"]
          ]        

------------------------------------------------------
view :: State -> Html Action
view state = do
  div
    []   
    [ h1 [] [ text "Sign In" ]
    , infoMessage
    , div [] [  
        div [] [
          label [] [ text "Username" ]
        , input [ type_ "text", id_ "email", onChange UpdateUserName, value (fromMaybe "" state.userName) ] [ ]
        ]
      , div [] [
          label [] [ text "Password" ]
        , input [ type_ "password", id_ "pass", onChange UpdatePass, value (fromMaybe "" state.password) ] [ ]
        ]

      , button [ onClick (const LoginClick), className "waves-effect waves-light btn-large" ] [ text "Login" ]
      ]
    ]  

    where
      infoMessage = case state.messages of
                [] -> div [] [] -- TODO: need a way of nothing here rather than an empty div 
                xs -> div [ className "card-panel red lighten-5" ] $ map renderMessage state.messages
      renderMessage m = div [] [ text m ]

------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update action state = case action of
    LoginClick -> { state: state 
              ,  effects: [ 
                do
                  case state.userName, state.password of 
                    Just userName, Just password -> do
                      let auth = Auth { email:userName, pass:password }
                      res <- apiRequest2 baseUrl "rpc/login" Nothing (Just <<< show $ encodeJson auth)
                      
                      case runExcept $ readString =<< readProp "token" res.response of 
                        Left _ -> pure $ LoginFail "Incorrect Username or Password"
                        Right token -> pure $ LoginSuccess token
                      
                      --pure Nop
                    _, _ -> do
                      pure Nop
                  
              ] }
    SetUserProfile profile -> { state: state { userProfile = Just profile } , effects:[] } 
    UpdateUserName ev -> { state: state { userName = Just (ev).target.value } , effects:[] } 
    UpdatePass ev -> {state: state { password = Just ev.target.value } , effects:[] }
    LoginFail msg -> {state: state { messages = [ msg ] } , effects:[] }
    LoginSuccess jwt -> {state: state { jwt = Just jwt, messages = [ "Login Successful" ] }, effects:[ 
        do
          res <- apiRequest baseUrl "rpc/my_profile" (Just jwt) (Just "{}")
          case (runExcept $ readJSON res.response :: F (Array UserProfile)) of
            Left x -> do
              liftEff $ log $ "invalid profile" <> res.response <> " : " <> show x
              pure Nop
            Right profiles -> do
              case head profiles of
                Nothing -> pure Nop
                Just profile -> pure $ SetUserProfile profile 
          
      ] }
    Nop -> {state: state, effects:[] }


