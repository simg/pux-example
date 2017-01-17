module Handlers.Feedback where

import Foundation
import Handlers.Common

import App.Form as Form

-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  formState :: Form.State
} 
    
data Action = Submit
            | ChildForm Form.Action
            | Nop


init :: State
init = { formState: formInit }


formInit :: Form.State
formInit = { elements: [
      Form.Rating { label:"How are we doing today?", range: { min:Just 1, max:Just 10 } } Nothing
    , Form.Text   { label:"Comments", maxLength:5000 } Nothing
    ]}

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)

update Submit state = 
  noEffects $ state

update (ChildForm n) state =
  noEffects $ state { formState = Form.update n state.formState } 

update Nop state = 
  noEffects $ state


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

page :: State -> Html Action
page state = 
  div [] [ 
    h1 [] [ text "Feedback" ]
  , map ChildForm $ Form.view $ spy state.formState
  , button [ onClick (const Submit) ] [ text "Send" ]
  ]  






  
  