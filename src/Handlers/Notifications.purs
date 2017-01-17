module Handlers.Notifications where

import Foundation
import Handlers.Common

import Data.Formatter.DateTime as FDT

-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  notifications :: Array Notification
} 

data Action = MarkAsSeen
            | Receive Notification

init :: State
init = { 
  notifications: [] 
}

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: Action -> State -> State
update MarkAsSeen state = state { notifications = [] }

update (Receive n) state = state { notifications = [n] <> state.notifications }


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

icon :: State -> Html Action
icon state =  span [ className "notificationIcon" ] [ 
         --link "/notifications" [] [ text "hello" ]
         link "/notifications" [ className ""] [
            span [ className "oi oi-bell", {-A.title "Notifications",-} aria "hidden" "true" ] []
          , span [ className "count" ] [ text (show unSeenCount) ] 
          , span [ className "text" ] [ text "Notifications"]
          ]
      ]
  where
    unSeenCount = length $ filter (\(Notification n) -> n.status == NotificationUnSeen) state.notifications
    menuItem h t = li [] [ link h [ className "nav-link", data_ "toggle" "collapse", data_ "target" ".nav-collapse" ] [ text t ] ]


view :: State -> Html Action
view state = 
  div [ className "notifications" ] [ 
    h1 [] [ text "Notifications" ]
  , icon state
  , div [ className "panel panel-default notifications" ] [
      div [ className "panel-body" ] [
        ul [] ( map renderNotification notifications )
                                       
      , button [ onClick (const MarkAsSeen), className "waves-effect waves-light btn-large" ] [ text "Mark These as Seen" ]
      ]
    ]
  ]
  where
    notifications = take 5 $ filter (\(Notification n) -> n.status == NotificationUnSeen) state.notifications 


renderNotification :: Notification -> Html Action
renderNotification (Notification n) = 
  li [ className "row notification" ] [ 
    {-span [ className (show n.status) ] [ 
      span [ className "icon"] [ img [ src n.icon, width "48", height "48" ] [] ]
    , span [ className "message"] [ text n.message ] 
    , span [ className "when" ] [ text (shortDateTime n.time) ] 
    ]-}
    div [ className "card horizontal" ] [
      div [ className "card-image" ] [
        span [ className "icon" ] [ img [ src n.icon, width "48", height "48" ] [] ]
      ]
    , div [ className "card-stacked" ] [
        div [ className "card-content" ] [
          p [ className "valign" ] [ text n.message ]
        , p [ className ""] [ text $ "sent at: " <> (shortDateTime n.time)] 
        ]
      ]
    ]
  ]  


