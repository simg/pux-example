module Handlers.People.List

where

import Foundation
import Handlers.Common

import Components.Header as Header


-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  header :: Header.State
, list :: Array User
, pageNo :: Int
, itemsPerPage :: Int
}

data Action = Load
            | GotoPage Int
            | SetList (Array User)
            | ChildHeader Header.Action
            | Nop

init :: State
init = {
  header: Header.init
, list: []
, pageNo: 1
, itemsPerPage: 20
}

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update Load state = 
  { state: state { header = Header.init, list = [], pageNo = 1 }, effects: [
      do
        res <- fetchUsers $ UserListFilterOptions {id_:Nothing, name:Nothing}
        case res of 
          Left err -> pure $ ChildHeader $ Header.SetStatus $ [ StatusMessage { type_:ErrorMessage, title:"Error Fetching user list", detail:(Just $ show err) } ]
          Right people -> pure $ SetList people
  ]}

update (GotoPage i) state = 
  noEffects $ state

update (SetList people) state =
  noEffects $ state { list = people }

update (ChildHeader a) state = 
  mapEffects ChildHeader $ mapState ( state { header = _ } ) $ Header.update a state.header

update Nop state = 
  noEffects $ state


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

view :: State -> Html Action
view state = 
  div [ className "section"] [
    map ChildHeader $ Header.view state.header
  , div [] [ 
      ul [] ( map item state.list  )
    ] 
  ]
  where
    item i = li [ className ""] [ personIndexItem i ]


personIndexItem :: User -> Html Action  
personIndexItem (User u) = div [ className "card horizontal" ] [
             --a [ href "#", onClick (const (GotoView (Detail u.id_))) ] [
             div [ className "card-image" ] [
               link ("/people/" <> uid) [] [
                 span [ className "icon" ] [ img [ src (fromMaybe defaultProfilePhoto u.icon), width "48", height "48" ] [] ]
               ]
             ]
           , div [ className "card-stacked" ] [
               div [ className "card-content" ] [
                 link ("/people/" <> uid) [] [
                   span [ className "title valign" ] [ text u.name ] 
                 ]
               ]
             ]
           {-,-}
           ]
    where 
      uid = case u.id_ of
              Just i -> i
              Nothing -> ""

defaultProfilePhoto :: String
defaultProfilePhoto = "/static/images/default_profile_photo.png"


{-
div [] [
    sectionNav
  , div [ className "controls" ] [
        ul [] [
          li [] [ 
            link "/user/new" [] [ 
              H.span [ className "oi oi-plus", A.title "New user", aria "hidden" "true" ] [] 
            , H.span [ className "text" ] [ text "New user" ]
            ]
          ]
        ]
      ]
  , div [] [ 
      --ul [] ( List.mapMaybe item $ StrMap.values state.users )
      ul [] ( map item userList  )
    ] 
  ]
  where
    userList :: Array User
    userList = Array.fromFoldable $ StrMap.values state.users --TODO: has to be a better way than this (suspect wasteful)
    item i = li [ className ""] [ teaser i ]
-}