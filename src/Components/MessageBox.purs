module Components.MessageBox where

import Foundation
import Components.Common


type State = {
  message :: String
}

data Action = UpdateMessageBox FormEvent
            | SendMessage 


init :: State
init = {
  message:""
}

view :: State -> Html Action
view state = div [ className "message-box"] [
               textarea [ onChange UpdateMessageBox, value state.message ] []
             , button [ onClick (const SendMessage), className "waves-effect waves-light btn-large"] [ text "Send Message" ]
             ]


update :: Action -> State -> State
update (UpdateMessageBox ev) state = state { message = ev.target.value }

update SendMessage state = state { message = "" }



