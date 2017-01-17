module Components.Header where

import Foundation
import Components.Common



-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  statusMessages :: Array StatusMessage
}

data Action = SetStatus (Array StatusMessage)

init :: State 
init = {
  statusMessages: []
}

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update (SetStatus messages) state = 
  noEffects $ state { statusMessages = messages }


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

view :: State -> Html Action
view state = div [] [
    statusMessages state.statusMessages 
  ]


statusMessages :: Array StatusMessage -> Html Action
statusMessages us = div [ className "status-messages"] [
    ul [] (map msg us)
  ]
  where
    msg (StatusMessage m) = li [ className ("card-panel lighten-5 " <> color m.type_)] ( 
                 [ span [ className "title" ] [ text m.title ] ]
                 <> ( case m.detail of 
                      Just detail -> [ span [ className "detail"] [ text detail ] ]
                      Nothing -> [] )
               )
    color type_ = case type_ of 
                    InfoMessage -> "green"
                    WarnMessage -> "orange"
                    ErrorMessage -> "red"