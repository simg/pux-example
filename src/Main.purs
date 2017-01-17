module Main where

import Foundation

import App.Routes
import App.Layout

import Handlers.Authentication as Authentication
import Components.Counter as Counter
import Handlers.Profile as Profile
import Handlers.Notifications as Notifications
import Handlers.Messages as Messages
import Handlers.People as People
import Handlers.UserAdmin as UserAdmin

import Handlers.Feedback as Feedback

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Keys (keys)

import Network.HTTP.Affjax (AJAX(), get)

import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)

import Signal ((~>))
import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

import WebSocket
import App.App
import App.Global
import App.Utils

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  wsInput <- S.channel Nop
  --appState <- initialState wsInput "ws://localhost:3000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNpbW9uQGhvbGlzdGljc3lzdGVtcy5jby51ayIsIm1vZGUiOiJydyIsImNoYW5uZWwiOiIyIiwicm9sZSI6InBvc3RncmVzIn0.Xz9Q0wZOzc9B9pBvqCJExUkWCAHmDwA43nB3WLbfYds"
  appState <- initialState wsInput baseWS baseWS --TODO: baseWS needs to be configurable
  let wsSignal = S.subscribe wsInput :: S.Signal Action

  -- | Map a signal of URL changes to PageView actions. 
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> \r -> GotoPage (match r)

  app <- start 
    {
      initialState: appState
    , update: update
    , view: view
    , inputs: [wsSignal, routeSignal]
    }
  --app <- start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  wsInput <- S.channel Nop
  --appState <- initialState wsInput "ws://localhost:3000/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNpbW9uQGhvbGlzdGljc3lzdGVtcy5jby51ayIsIm1vZGUiOiJydyIsImNoYW5uZWwiOiJfdXNlcl8yIiwicm9sZSI6InBvc3RncmVzIn0.Xfjx5zKWHFmjt6y3Fd_MbVZ0TtIEAzXJcSS6iYTDxmI"    
  appState <- initialState wsInput baseWS baseWS --TODO: baseWS needs to be configurable
  
  let wsSignal = S.subscribe wsInput :: S.Signal Action

  -- | Map a signal of URL changes to PageView actions. 
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> \r -> GotoPage (match r)

  app <- Pux.Devtool.start 
    {
      initialState: appState
    , update: update
    , view: view
    , inputs: [wsSignal, routeSignal]
    }
  --app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  pure app

initialState :: S.Channel Action -> String -> String -> forall eff. Eff ( ws :: WEBSOCKET, err :: EXCEPTION | eff) State
initialState chan apiHost wsHost = do
  connection <- webSocketConnection chan wsHost ""
  pure { 
       route: Home
     , count: Counter.init
     , uid: Nothing
     , username: Nothing
     , auth: Authentication.init
     , server:{ connection:connection, chan:chan, apiHost:apiHost, wsHost:wsHost }
     , connected: false
     , notifications: Notifications.init
     , messages: Messages.init
     , people: People.init
     , statusMessages: []
     , userAdmin: UserAdmin.init --TODO: need a way to scale to many temporary states
     , userProfile: Profile.init
     , feedback: Feedback.init
     }







