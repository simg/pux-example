module App.App where

import App.Routes

import Foundation

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.StrMap as StrMap
import Data.Foreign
import Data.Foreign.Null
import Data.List.NonEmpty (NonEmptyList)

import Components.Counter as Counter
import Handlers.Authentication as Authentication
import Handlers.Profile as Profile
import Handlers.Notifications as Notifications
import Handlers.Messages as Messages
import Handlers.People as People
import Handlers.UserAdmin as UserAdmin

import Handlers.Feedback as Feedback

import DOM (DOM)


import Pux (App, Config, CoreEffects, EffModel, noEffects, mapEffects, mapState, fromSimple, renderToDOM, start, Update)
--import Pux.Devtool (Action, start) as Pux.Devtool
--import Pux.Html (Html, (!), (#), h1, h2, h3, nav, section, div, a, ul, li, p, span, text, html, button)
--import Pux.Html.Attributes
--import Pux.Router (sampleUrl, link)

import Signal ((~>))
import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

import WebSocket

import App.Global
import App.Model
import App.Utils


type AppEffects = (dom :: DOM,  ajax :: AJAX, console :: CONSOLE, now :: NOW, ws :: WEBSOCKET )

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update action state = update_ action $ state { statusMessages = messages }
  where
    messages = case action of 
                 WSConnected _  -> state.statusMessages
                 WSDisconnected -> state.statusMessages
                 _ -> [] -- if Action is any user initiated interaction then clear the status messages

update_ :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update_ (ChildAbout n) state =
  { state: state, effects: [] }

update_ (ChildFeedback n) state =
  mapEffects ChildFeedback $ mapState (state {feedback = _}) $ Feedback.update n state.feedback

update_ (ChildMessages a) state = 
  mapEffects ChildMessages $ mapState ( state { messages = _ } ) $ Messages.update a state.messages

update_ (ChildPeople a) state = 
  mapEffects ChildPeople $ mapState ( state { people = _ } ) $ People.update a state.people


update_ (ChildCounter n) state =
 { state: state, effects: [] }

update_ (ChildUserAdmin (UserAdmin.PostSaveUser muser message)) state = 
  mapEffects ChildUserAdmin 
            $ mapState (state { statusMessages = [message], route = route, userAdmin = _}) 
            $ UserAdmin.update (UserAdmin.PostSaveUser muser message) state.userAdmin
  where
    route = case muser of 
      Just user -> UserAdmin
      Nothing -> state.route

update_ (ChildUserAdmin n) state = 
  mapEffects ChildUserAdmin $ mapState (state {userAdmin = _}) $ UserAdmin.update n state.userAdmin

update_ (ChildProfile n) state = 
  mapEffects ChildProfile $ mapState (state {userProfile = _}) $ Profile.update n state.userProfile

update_ (ChildNotifications n) state = 
  noEffects $ state { notifications = Notifications.update n state.notifications }

update_ (ChildAuthentication (Authentication.LoginSuccess n)) state = 
  mapEffects ChildAuthentication $ mapState (state {auth = _}) $ Authentication.update (Authentication.LoginSuccess n) state.auth

update_ (ChildAuthentication n) state = 
  mapEffects ChildAuthentication $ mapState (state {auth = _}) $ Authentication.update n state.auth

update_ (ChildMyAccount n) state =
  noEffects state

update_ (GotoPage route) state = 
  { state: ( state { route = route }), effects: [
    do
      case route of
        Messenger route -> pure $ ChildMessages $ Messages.Load route
        People    route -> pure $ ChildPeople   $ People.Load route
        UserView uid -> pure $ ChildUserAdmin $ UserAdmin.DetailViewInit uid 
        UserEdit uid -> pure $ ChildUserAdmin $ UserAdmin.EditFormInit $ StrMap.lookup uid (state.userAdmin).users --find (\(User u) -> u.id_ == Just uid) (state.userAdmin).users 
        UserAdd      -> pure $ ChildUserAdmin $ UserAdmin.EditFormInit Nothing
        UserAdmin    -> pure $ ChildUserAdmin UserAdmin.ListInit
        _ -> pure Nop
    ] }

update_ (WSConnected connection) state =
  noEffects $ state { connected = true 
                    , server = state.server { connection = connection }
                    }

update_ (WSDisconnected) state = 
  { state: state { connected = false }
  , effects: [
    do
      socket <- liftEff $ webSocketConnection (state.server).chan (state.server).wsHost (fromMaybe "" (state.auth).jwt)
      pure $ WSConnected socket
    ]}

update_ Nop state = 
  noEffects $ state                                           


-------------------------------------------------------------------------------------------
-- Websockets Communication
-------------------------------------------------------------------------------------------

webSocketConnection :: S.Channel Action -> String -> String -> forall eff. Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff) Connection
webSocketConnection chan host jwt = do
  connection@(Connection ws) <- newWebSocket (URL $ "ws://" <> host <> "/" <> jwt) []

  ws.onopen $= \event -> do
    log $ "onopen: socket connection opened"
    S.send chan ((WSConnected connection)) --TODO: I think this ends up setting the state.socket value twice. won't be a problem other than duplication of neglible effort

  ws.onclose $= \event -> do
    log $ "onclose: socket connection closed"
    S.send chan ((WSDisconnected))

  ws.onmessage $= \event -> do
    dt <- now
    let received = runMessage $ runMessageEvent event
    log $ "message received from websocket: " <> show received
    S.send chan ((receiveSocketCommand received $ toDateTime dt) :: Action)

  pure connection


receiveSocketCommand :: String -> DateTime -> Action
receiveSocketCommand m dt = 
  case parseKeys of 
    Right keys -> case head keys of 
      Just "notification" -> 
        case parseNote of
          Right n         -> ChildNotifications $ Notifications.Receive n
          Left  ex        -> ChildNotifications $ Notifications.Receive $ note ("invalid notification:" <> show ex <> "==" <> m) dt
      Just "gotoPage"     -> GotoPage parseRoute
      Just "message"      -> 
        case spy parseMessage  of 
          Right m         -> ChildMessages $ Messages.Receive $ spy m
          Left ex         -> ChildNotifications $ Notifications.Receive $ note ("invalid message:" <> show ex <> "==" <> m) dt
      Just _              -> ChildNotifications $ Notifications.Receive $ note "some other message" dt
      Nothing             -> ChildNotifications $ Notifications.Receive $ note "unrecogniseable message" dt
    Left ex               -> ChildNotifications $ Notifications.Receive $ note (show ex) dt --TODO: disable notifications in production      
  where
    parseKeys   = runExcept $ ( keys <=< parseJSON) m
    parseNote   = runExcept $ (readProp "notification" <=< parseJSON) m
    parseMessage :: Either (NonEmptyList ForeignError) MessageReceive
    parseMessage= runExcept $ (readProp "message" <=< parseJSON) m
    parseRoute  = case runExcept $ (readString <=< readProp "route" <=< readProp "goto_route" <=< parseJSON) m of
                    Left _ -> NotFound
                    Right x -> match x
    note msg tm = Notification { message:msg, status:NotificationUnSeen, icon:"", time:dt}

{-
--update (SendMessage m)    state = { state: state, effects: [ doWebSocketCall state.socket ] }

doWebSocketCall :: forall e. Connection -> Aff (err :: EXCEPTION, ws :: WEBSOCKET | e) Action
doWebSocketCall (Connection ws) =
  liftEff $ ws.send(SendMessage.Message "button four sends this message") *> pure Nop
-}