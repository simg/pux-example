module Handlers.People where

import Foundation
import Handlers.Common

import Handlers.People.List as List
import Handlers.People.View as View
import Handlers.People.Edit as Edit

import App.Models.User


-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  listState :: List.State
, viewState :: View.State
, editState :: Edit.State
}

init :: State
init = {
  listState: List.init
, viewState: View.init
, editState: Edit.init
}

data Action = Load Route
            | ChildList List.Action
            | ChildView View.Action
            | ChildEdit Edit.Action
            | Nop

data Route = List
           | New
           | Edit String
           | View String

match :: Match Route
match = lit "people" *> 
        (   (lit "new"  *> pure New)
        <|> ((Edit <$> str) <* lit "edit")
        <|> ((View <$> str) )
        <|> (pure List )
        )

        --<|> UserEdit    <$> (lit "user" *> str <* lit "edit") <* end

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update (Load route) state = 
  { state: state
  , effects: [
    do
    case spy route of
      List     -> pure $ ChildList List.Load
      New      -> pure Nop
      Edit str -> pure $ ChildEdit $ Edit.Load str
      View str -> pure $ ChildView $ View.Load str
  ]}

update (ChildList a) state = 
  mapEffects ChildList 
      $ mapState ( state { listState = _ } )
      $ List.update a state.listState

update (ChildView a) state = 
  mapEffects ChildView 
      $ mapState ( state { viewState = _ } )
      $ View.update a state.viewState

update (ChildEdit a) state = 
  mapEffects ChildEdit
      $ mapState ( state { editState = _ } )
      $ Edit.update a state.editState


update Nop state = 
  noEffects state

-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

view :: Route -> State -> Html Action
view route state = div [] [
               case route of 
                 List   -> map ChildList $ List.view state.listState
                 New    -> text "new" --map ChildList $ Edit.view state.editState
                 View i -> map ChildView $ View.view state.viewState
                 Edit i -> map ChildEdit $ Edit.view state.editState
             ]