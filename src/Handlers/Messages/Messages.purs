module Handlers.Messages where

import Foundation
import Handlers.Common

import Handlers.Messages.List as List

import App.Models.Message


-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  listState :: List.State
}

init :: State
init = {
  listState: List.init
}

data Action = Load Route
            | Receive MessageReceive
            | ChildList List.Action
            | Nop

data Route = List
           | New
           | View String

match :: Match Route
match = lit "messages" *> (
            (lit "new" *> pure New)
        <|> (lit "view" *> (View <$> str) )
        <|> (pure List )
        )

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update (Load route) state = 
  { state: state
  , effects: [
    do
    case route of
      List     -> pure $ ChildList List.Load
      New      -> pure Nop
      View str -> pure Nop
  ]}

update (ChildList a) state = 
  mapEffects ChildList 
      $ mapState ( state { listState = _ } )
      $ List.update a state.listState

update (Receive m) state = 
  noEffects $ state { listState = state.listState { list = [m] <> state.listState.list } }

update Nop state = 
  noEffects state

-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

view :: Route -> State -> Html Action
view route state = div [] [
               case route of 
                 List   -> map ChildList $ List.view state.listState
                 New    -> text "new"
                 View i -> text "view"
             ]


             