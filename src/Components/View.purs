module Components.View where

import Foundation

import Pux (EffModel, noEffects, mapEffects)
import Pux.Html (Html, form, div, p, ul, li, span, button, text, input, select, option, label)
import Pux.Html.Attributes (className, type_, value, maxLength, action, method)
import Pux.Html.Attributes (label) as A
import Pux.Html.Events (FormEvent, onClick, onInput, onChange)

import Network.HTTP.Affjax (AJAX(), get)

import App.Form

import WebSocket

data Action = Init
            | NextPage
            | PrevPage
            | FetchData

type State = {
  title :: String
, source :: String
--, fields :: Array FieldSpec
--, rows :: Array Row
, curPage :: Int
}

{-type Row = {
  columns :: Array FieldValue
}-}

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update action state = case action of
  Init -> { state:state
          , effects: [
            do
              pure FetchData
          ] }
  NextPage -> noEffects $ state { curPage = state.curPage + 1 } 
  PrevPage -> noEffects $ state { curPage = state.curPage - 1 }
  FetchData -> noEffects $ state { curPage = state.curPage + 1 } 
  {-FetchData -> { state: state
               , effects: [
                 do
                   res <- apiRequest2 "localhost:3000" (state.source) Nothing Nothing
               ] }-} 

view :: State -> Html Action
view state = do
  div
    []   
    [ p [] [ text "Hello world: " ] ]


