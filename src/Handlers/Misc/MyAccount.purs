module Handlers.MyAccount where

import Foundation
import Handlers.Common

import App.Form


data Action = Increment | Decrement

type State = {
  firstName :: String
, lastName :: String
, emailAddress :: String
, contactPreference :: ContactPreference
}

data ContactPreference = Immediate | Daily | Weekly | Monthly | Quaterly | BiAnnual | Annual | NoContact


init :: State
init = {
  firstName:""
, lastName:""
, emailAddress:""
, contactPreference:NoContact
}

update :: Action -> State -> State
update Increment state = state
update Decrement state = state

view :: State -> Html Action
view state =
  div
    []   
    [ p [] [text "hello simon"]
    , button [ onClick (const Increment) ] [ text "Increment" ]
    --, span [] [ text (show state) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
