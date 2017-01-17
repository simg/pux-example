module Handlers.People.Edit

where

import Foundation

import Data.Int as Int 

import Handlers.Common

import Components.Header as Header


-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  person :: Maybe User
, header   :: Header.State
} 

data Action = Load String
            | SetState State
            | ChildHeader Header.Action
            | Nop

init :: State
init = { person:Nothing
       , header: Header.init
       }


-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update (Load userId) state = 
  { state: state, effects: [
    do
      res <- fetchUser userId --TODO: refactor "user" to person
      case res of 
        Left err -> pure $ ChildHeader $ Header.SetStatus [ StatusMessage { type_:ErrorMessage, title:"User not found", detail:(Just $ show err) } ] --TODO: handle this more elegantly
        Right person -> pure $ SetState $ state { person = Just person }           
  ]}
  where 
    uid = case Int.fromString userId of
            Just i -> i
            Nothing -> -1 --TODO: eek, how awful. refactoring entity id's to UUID will remove this crime
  

update (ChildHeader a) state = 
  mapEffects ChildHeader $ mapState ( state { header = _ } ) $ Header.update a state.header

update (SetState newState) state = 
  noEffects $ newState
  
update Nop state = 
  noEffects $ state   
          


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------


view :: State -> Html Action
view state = div [] [ text "hello world" ]
  

defaultProfilePhoto :: String
defaultProfilePhoto = "/static/images/default_profile_photo.png"   

