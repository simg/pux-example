module Handlers.UserAdmin where

import Foundation

--import Data.Argonaut.Core
import Data.Argonaut.Encode
import Data.Array as Array
import Data.Int as Int
import Data.List as List
import Data.StrMap (StrMap)
import Data.StrMap as StrMap

--import Prelude (($), (+), (-), const, show, map)
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.Html (Html, form, div, nav, h1, h2, p, ul, li, a, img, button, text, input, select, option, label)
import Pux.Html (span) as H
import Pux.Html.Attributes 
import Pux.Html.Attributes (label, title) as A
import Pux.Html.Events (onClick)
import Pux.Router (link)

import Network.HTTP.Affjax (AJAX(), get)

import WebSocket

import App.Utils
import App.Form as Form
import App.Model


import Components.MessageBox as MessageBox

-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  users :: StrMap User
, editUser :: Maybe User
, editFormState :: Form.State
, messageBox :: MessageBox.State
, userMessages :: Array MessageReceive
} 

data Action = ListInit
            | EditFormInit (Maybe User)
            | DetailViewInit String
            | SetUserMessages (Array MessageReceive)
            | SubmitUser
            | PostSaveUser (Maybe User) StatusMessage 
            | SetStatusMessage StatusMessage
            --| FetchList UserListFilterOptions
            | SetListData (Array User)
            | ChildForm Form.Action
            | ChildMessageBox MessageBox.Action
            | Nop

init :: State
init = { users:StrMap.fromFoldable []
       , editUser:Nothing
       , editFormState: {elements:[]}
       , messageBox:MessageBox.init
       , userMessages:[]
       }

-------------------------------------------------------------------------------------------
-- State Transformations
-------------------------------------------------------------------------------------------

update :: forall e. Action -> State -> EffModel State Action (ajax :: AJAX, console :: CONSOLE, ws :: WEBSOCKET, now :: NOW | e)
update SubmitUser state = 
  { state: state, effects:[ 
    do
      --res <- apiCall method baseUrl (endPoint state.editUser) (Just "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNsZ2FyZEBnbWFpbC5jb20iLCJtb2RlIjoicnciLCJjaGFubmVsIjoiX3VzZXJfMyIsInJvbGUiOiJwb3N0Z3JlcyJ9.HVxfS3vYPO-iyqTmmAykAX_E4aoOaWUBqmIYOXhEDbo") (Just <<< show $ encodeJson obj)
      liftEff $ log $ "submit user" --<> (show state.editUser)
      case validateForm state of
        Right user -> do
          res <- saveUser user
          case res of 
            Right _  -> pure $ PostSaveUser (Just user) $ StatusMessage { type_:InfoMessage, title:"User saved", detail:Nothing } 
            Left err -> pure $ PostSaveUser Nothing $ StatusMessage { type_:ErrorMessage, title:"User Save - Failed", detail:Just err } 
        Left err -> pure $ PostSaveUser Nothing $ StatusMessage { type_:ErrorMessage, title:"Form validation error", detail:Nothing } -- Nop -- TODO: show form validation errors
    ]}

update (PostSaveUser muser message) state =
  noEffects $ state { 
                      users = updateUser
                    }
    where
      updateUser = case muser of --TODO: this needs refactoring (think about whether separating keys / entities as per Haskell Persistent would be a good idea)
                     Just (User user) -> case (user.id_) of 
                                           Just id_ -> StrMap.insert id_ (User user) state.users
                                           Nothing -> state.users
                     Nothing -> state.users

update Nop state = 
  { state: state, effects:[] }

update (DetailViewInit userId) state = 
  { state: state, effects: [
    do
      res <- fetchMessages uid
      case res of 
        Left err -> pure $ SetStatusMessage $ StatusMessage { type_:ErrorMessage, title:"Error Fetching message list", detail:(Just $ show err) } 
        Right messages -> pure $ SetUserMessages messages
  ]}
  where 
    uid = case Int.fromString userId of
            Just i -> i
            Nothing -> -1 --TODO: eek, how awful. refactoring entity id's to UUID will remove this crime
          

update (SetUserMessages messages) state = 
  noEffects $ state { userMessages = messages }

update (EditFormInit user) state = 
  { state: state { editUser = user, editFormState = editForm user }, effects:[] }

update ListInit state = 
  { state: state, effects:[
    do
      res <- fetchUsers $ UserListFilterOptions {id_:Nothing, name:Nothing}
      case res of 
        Left err -> do 
          pure $ SetStatusMessage $ StatusMessage { type_:InfoMessage, title:"Error Fetching user list", detail:(Just $ show err) } 
        Right users -> do
          pure $ SetListData users
    ]}

update (SetStatusMessage msg) state =
 { state: state, effects:[] } --TODO: need global message

update (SetListData users) state =
 { state: state { users = toStrMap users }, effects: [
    do
      liftEff $ log $ "set list data"
      pure Nop
    ]}
  where
    toStrMap users = StrMap.fromFoldable $ catMaybes $ map (\(User u) -> case u.id_ of 
                                                              Just id_ -> Just (Tuple id_ (User u))
                                                              Nothing -> Nothing 
                                                           ) users

update (ChildForm n) state = 
  noEffects $ state { editFormState = Form.update n state.editFormState }

update (ChildMessageBox MessageBox.SendMessage) state = 
  { state: state { messageBox = MessageBox.update MessageBox.SendMessage state.messageBox }
  , effects: [
     do
       liftEff $ log $ "send message"
       res <- sendMessage $ MessageSend {recipients:[2], message:state.messageBox.message} --TODO: need to not hardcode user ids!
       case res of 
         Right _ -> pure $ SetStatusMessage $ StatusMessage { type_:InfoMessage, title:"Message Sent", detail:Nothing } 
         Left err -> pure $ SetStatusMessage $ StatusMessage { type_:ErrorMessage, title:"Message Sending Failed", detail:Just err } 
  ]}
  
update (ChildMessageBox n) state = 
  noEffects $ state { messageBox = MessageBox.update n state.messageBox }


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------

listView :: State -> Html Action
listView state = div [] [
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

--TODO: have an id of "Maybe String" leaves the possibilty of odd behaviour here :(
teaser :: User -> Html Action  
teaser (User u) = div [ className "card horizontal" ] [
             --a [ href "#", onClick (const (GotoView (Detail u.id_))) ] [
             div [ className "card-image" ] [
               link ("/user/" <> uid) [] [
                 H.span [ className "icon" ] [ img [ src (fromMaybe defaultProfilePhoto u.icon), width "48", height "48" ] [] ]
               ]
             ]
           , div [ className "card-stacked" ] [
               div [ className "card-content" ] [
                 link ("/user/" <> uid) [] [
                   H.span [ className "title valign" ] [ text u.name ] 
                 ]
               ]
             ]
           {-,-}
           ]
    where 
      uid = case u.id_ of
              Just i -> i
              Nothing -> ""
      

-- TODO: refactor to remove the possibility of Maybe User
detailView :: State -> Maybe User -> Html Action
detailView state user = case user of
   Just (User u) -> 
      div [] [ 
        --button [ onClick (const $ GotoView List) ] [ text "Back to List" ]
        --link "/users" [] [ text "Back to List" ]
        sectionNav
      , div [ className "controls" ] [
          ul [] [
            li [] [ 
              link ("/user/" <> uid u <> "/edit") [] [
                H.span [ className "oi oi-pencil", A.title "Edit person", aria "hidden" "true" ] [] 
              , H.span [ className "text" ] [ text "edit" ] 
              ]
            ]
          ]
        ]        
      , h2 [] [ text u.name ]
      , div [ className "field icon" ] [ img [ src (fromMaybe defaultProfilePhoto u.icon), width "300", height "300" ] [] ]
      , div [ className "field email"] [ 
        a [ className "oi oi-envelope-closed", href ("mailto:" <> fromMaybe "" u.email) ] [
          text $ fromMaybe "" u.email ]
        ]
      {-, div [ className "field chat"] [ 
        a [ className "oi oi-chat", href "", onClick (const SubmitUser) ] [ text "Chat" ]
        ]-}
        , map ChildMessageBox $ MessageBox.view state.messageBox

      ]
   Nothing -> div [] [ 
      sectionNav
    , p [] [ text "Not found" ]
    ]
    where 
      uid u = case u.id_ of
              Just i -> i
              Nothing -> ""    
                  

editView :: State -> Html Action
editView state = 
  div [] [
    sectionNav
  , h2 [] [ text "Edit person"]
  , map ChildForm $ Form.view state.editFormState
  , button [ onClick (const SubmitUser), className "waves-effect waves-light btn-large" ] [ text "Save" ]
  ]

addView :: State -> Html Action
addView state = 
  div [] [
    sectionNav
  , h2 [] [ text "Add new person"]
  , map ChildForm $ Form.view state.editFormState
  , button [ onClick (const SubmitUser), className "waves-effect waves-light btn-large" ] [ text "Save" ]
  ]

defaultProfilePhoto :: String
defaultProfilePhoto = "/static/images/default_profile_photo.png"

--TODO: move this into the global layout?
sectionNav :: Html Action
sectionNav =  div [ className "section-nav" ] [
      h1 [] [ 
        link "/users" [] [
          text "People" 
        ]
      ] 
    ]    
  


-------------------------------------------------------------------------------------------
-- Forms
-------------------------------------------------------------------------------------------

editForm :: Maybe User -> Form.State
editForm muser = { elements: [
      Form.Text { label:"Name", maxLength:100 } (_.name <$> user)
    , Form.Text { label:"Email", maxLength:100 } (join $ _.email <$> user)    
    , Form.Text { label:"Icon", maxLength:255 } (join $ _.icon <$> user)    
    ]
    }
  where
    user = case muser of 
             Just (User u) -> Just u
             _ -> Nothing


--TODO: need to have some failing validation
validateForm :: State -> Either (Array Form.FieldError) User
validateForm state = Right $ User { 
              id_:   userId --EntityId (Just "2")
            , name:  (fromMaybe "" $ Form.fieldStringValue <$> (state.editFormState.elements !! 0))
            , pass:  Nothing
            , email: (Form.fieldStringValue <$> (state.editFormState.elements !! 1))
            , icon:  (Form.fieldStringValue <$> (state.editFormState.elements !! 2))
            , role:  Admin
            }
    where 
      userId = case state.editUser of
                  Just (User user) -> user.id_
                  Nothing -> Nothing 

 
--------------------------------------------------------------------------------------------------------------------------------

{-
fetchUserById :: EntityId -> Maybe User
fetchUserById uid = find (\(UserAdmin.User u) -> u.id_ == EntityId (Just uid)) (state.userAdmin).users) 
-}





  
  