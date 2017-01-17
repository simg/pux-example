module App.Global where

import Foundation 

import App.Routes

import WebSocket

import Components.Counter as Counter

import Handlers.About as About
import Handlers.Authentication as Authentication
import Handlers.Feedback as Feedback
import Handlers.Profile as Profile
import Handlers.Notifications as Notifications
import Handlers.Messages as Messages
import Handlers.People as People
import Handlers.MyAccount as MyAccount
import Handlers.UserAdmin as UserAdmin

import App.Model

import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

type State = { 
    route          :: Route
  , count          :: Counter.State 
  , uid            :: Maybe Int
  , username       :: Maybe String
  , auth           :: Authentication.State
  , server         :: ServerChannelConnection
  , connected      :: Boolean
  , notifications  :: Notifications.State
  , messages       :: Messages.State
  , people         :: People.State
  , statusMessages :: Array StatusMessage
  , userAdmin      :: UserAdmin.State
  , userProfile    :: Profile.State
  , feedback       :: Feedback.State
  }

data Action
  = ChildCounter        Counter.Action
  | ChildAbout          About.Action
  | ChildAuthentication Authentication.Action
  | ChildFeedback       Feedback.Action
  | ChildProfile        Profile.Action
  | ChildNotifications  Notifications.Action
  | ChildMessages       Messages.Action
  | ChildPeople         People.Action
  | ChildMyAccount      MyAccount.Action
  | ChildUserAdmin      UserAdmin.Action
  | GotoPage Route
  | WSConnected Connection
  | WSDisconnected
  | Nop

type ServerChannelConnection = {
    connection :: Connection
  , chan :: S.Channel Action
  , apiHost :: String 
  , wsHost :: String
  }


