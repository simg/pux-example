module App.Layout where


import App.Routes

import Components.Counter as Counter

import Handlers.Authentication as Authentication
import Handlers.Profile as Profile
import Handlers.Notifications as Notifications
import Handlers.Messages as Messages
import Handlers.People as People
import Handlers.MyAccount as MyAccount
import Handlers.UserAdmin as UserAdmin
import Handlers.About as About
import Handlers.Feedback as Feedback

import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.StrMap as StrMap


import DOM (DOM)

import Pux (App, Config, CoreEffects, EffModel, noEffects, fromSimple, renderToDOM, start, Update)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Html (Html, (!), (#), h1, h2, h3, nav, section, div, a, ul, li, p, span, text, html, button)
import Pux.Html.Attributes
import Pux.Html.Attributes (label, title) as A
import Pux.Router (sampleUrl, link)


import Signal ((~>))
import Signal as S
import Signal.Channel (CHANNEL(), Channel, channel, send, subscribe) as S

import WebSocket

import Foundation
import App.App
import App.Model
import App.Global

 
view :: State -> Html Action
view (state) =
  div
    [ className "container"] [ 
      header state
    , div [ className "main block" ] [
        content (state) state.route
      ]
    ]

content :: State -> Route -> Html Action
content (state) route = case route of 
  Home         -> div [] [
                     h1 [] [ text "Example App" ]
                   , navigation
                   ]
  About        -> map ChildAbout          $ About.page 
  Feedback     -> map ChildFeedback       $ Feedback.page       state.feedback
  Login        -> map ChildAuthentication $ Authentication.view state.auth
  Messenger r  -> map ChildMessages       $ Messages.view r     state.messages
  People    r  -> map ChildPeople         $ People.view r       state.people     
  MyAccount    -> map ChildMyAccount      $ MyAccount.view      MyAccount.init
  NotFound     -> h1 [] [ text "Not Found" ]                         
  Notifier     -> map ChildNotifications  $ Notifications.view  state.notifications  
  Profiler     -> map ChildProfile        $ Profile.view        state.userProfile
  UserAdd      -> map ChildUserAdmin      $ UserAdmin.addView    state.userAdmin                        
  UserAdmin    -> map ChildUserAdmin      $ UserAdmin.listView   state.userAdmin
  UserEdit uid -> map ChildUserAdmin      $ UserAdmin.editView   state.userAdmin  --TODO: this whole section needs re-thinking                      
  UserView uid -> map ChildUserAdmin      $ UserAdmin.detailView state.userAdmin $ StrMap.lookup uid (state.userAdmin).users--(find (\(User u) -> u.id_ == Just uid) (state.userAdmin).users) 
  

header :: State -> Html Action
header (state) = 
  section [ id_ "header"] [
    nav [ className "navbar" ] [ 
      div [ className "nav-wrapper" ] [ 
        ul [ className "left" ] [
          li [] [ 
            link "/" [] [ 
              span [ className "oi oi-home", A.title "Messages", aria "hidden" "true" ] [] 
            , span [ className "text" ] [ text "Home"]
            ]
          ]
        , li [] [ (map ChildNotifications $ Notifications.icon state.notifications) ]
        --, li [] [ (map ChildMessages $ Messages.icon state.messages) ]
        , li [] [ (map ChildAuthentication $ Authentication.icon state.auth) ]
        ]
      ]
    ]
  ]

            

navigation :: Html Action
navigation = 
  ul [ className "nav navbar-nav" ] [
    menuItem "/profile"    "My Profile"
  , menuItem "/messages"   "Messages"
  , menuItem "/feedback"   "Feedback"
  , menuItem "/about"      "About"
  , menuItem "/people"     "People"
  , menuItem "/users"      "Users"
  --, menuLink "/about"      "About"  -- menuLink creates a real hyperlink ie a server round trip
  ]
  where
    menuItem h t = li [] [ 
        div [ className "card blue-grey"] [
          link h [] [ 
            div [ className "card-content white-text" ] [
              span [ className "card-title" ] [ text t ] 
            ]
          ]
        ]
      ]
    menuLink h t = li [] [ 
      div [ className "card blue-grey"] [
          a [ href h, className "card blue-grey" ] [ 
            div [ className "card-content white-text" ] [
              span [ className "card-title" ] [ text t ] 
            ]
          ]
        ]
      ]
