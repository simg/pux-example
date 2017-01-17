module Handlers.People.View

where

import Foundation

import Data.Int as Int 

import Handlers.Common

import Components.Header as Header
import Components.MessageBox as MessageBox


-------------------------------------------------------------------------------------------
-- State 
-------------------------------------------------------------------------------------------

type State = {
  person     :: Maybe User
, messageBox :: MessageBox.State
, messages   :: Array MessageReceive
, header     :: Header.State
} 

data Action = Load String
            | SetState State
            | ChildHeader Header.Action
            | ChildMessageBox MessageBox.Action
            | Nop

init :: State
init = { person:Nothing
       , messageBox:MessageBox.init
       , messages:[]
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
        Right person -> do
          res <- fetchMessages uid
          case res of 
            Left err -> pure $ ChildHeader $ Header.SetStatus [ StatusMessage { type_:ErrorMessage, title:"Error Fetching message list", detail:(Just $ show err) } ]
            Right messages -> pure $ SetState $ state { person = Just person, messages = messages }
  ]}
  where 
    uid = case Int.fromString userId of
            Just i -> i
            Nothing -> -1 --TODO: eek, how awful. refactoring entity id's to UUID will remove this crime
  

update (ChildHeader a) state = 
  mapEffects ChildHeader $ mapState ( state { header = _ } ) $ Header.update a state.header

update (ChildMessageBox MessageBox.SendMessage) state = 
  { state: state { messageBox = MessageBox.update MessageBox.SendMessage state.messageBox }
  , effects: [
     do
       liftEff $ log $ "send message"
       case state.person of 
         Just (User person) -> do
           res <- sendMessage $ MessageSend {recipients:[hackyUid person.id_], message:state.messageBox.message} --TODO: need to not hardcode user ids!
           case res of 
             Right _ -> pure $ ChildHeader $ Header.SetStatus [ StatusMessage { type_:InfoMessage, title:"Message Sent", detail:Nothing } ]
             Left err -> pure $ ChildHeader $ Header.SetStatus [ StatusMessage { type_:ErrorMessage, title:"Message Sending Failed", detail:Just err } ]
         Nothing -> pure Nop --TODO: this can never happen, but not sure how to make state.person a non-maybe value :/
  ]}
  where
    hackyUid uid = case uid of  --TODO: awful.
                     Just uid -> case Int.fromString uid of
                                   Just i -> i
                                   Nothing -> -1
                     Nothing  -> -1

update (SetState newState) state = 
  noEffects $ newState
  
update (ChildMessageBox n) state = 
  noEffects $ state { messageBox = MessageBox.update n state.messageBox }         

update Nop state = 
  noEffects $ state   
          


-------------------------------------------------------------------------------------------
-- Views
-------------------------------------------------------------------------------------------


view :: State -> Html Action
view state = 
  case state.person of
    Just (User u) -> 
      div [] [ 
        map ChildHeader $ Header.view state.header      
        --text "hello"
        --button [ onClick (const $ GotoView List) ] [ text "Back to List" ]
        --link "/users" [] [ text "Back to List" ]
        --sectionNav
      , div [ className "controls" ] [
          ul [] [
            li [] [ 
              link ("/user/" <> uid u <> "/edit") [] [
                span [ className "oi oi-pencil", {-A.title "Edit person" ,--} aria "hidden" "true" ] [] 
              , span [ className "text" ] [ text "edit" ] 
              ]
            ]
          ]
        ]        
      , h2 [] [ text u.name ]
      , div [ className "field icon" ] [ img [ src (fromMaybe defaultProfilePhoto u.icon), width "300", height "300" ] [] ]
      , div [ className "field email"] [ 
          a [ className "oi oi-envelope-closed", href ("mailto:" <> fromMaybe "" u.email) ] [
            text $ fromMaybe "" u.email 
          ]
        ]
        {-, div [ className "field chat"] [ 
        a [ className "oi oi-chat", href "", onClick (const SubmitUser) ] [ text "Chat" ]
        ]-}
      , map ChildMessageBox $ MessageBox.view state.messageBox 
      , messageList (take 10 state.messages)
      ]
    Nothing -> div [] [ 
        --sectionNav
        p [] [ text "Not found" ]
      ]
  where 
    uid u = case u.id_ of
              Just i -> i
              Nothing -> "" 

defaultProfilePhoto :: String
defaultProfilePhoto = "/static/images/default_profile_photo.png"   


messageList :: Array MessageReceive -> Html Action
messageList messages = div [] [ 
      ul [] ( map message messages  )
    ] 

message :: MessageReceive -> Html Action  
message (MessageReceive m) = div [ className "card horizontal" ] [
             --a [ href "#", onClick (const (GotoView (Detail u.id_))) ] [
             div [ className "card-image" ] [
               --link ("/user/" <> uid) [] [
                 span [ className "icon" ] [ img [ src m.icon, width "48", height "48" ] [] ]
               --]
             ]
           , div [ className "card-stacked" ] [
               div [ className "card-content" ] [
                 --link ("/user/" <> uid) [] [
                 p [ className "valign" ] [ text m.message ]
               , p [ className ""] [ text $ "sent at: " <> (shortDateTime m.sentAt) <> " by: " <> m.name] 
                 --]
               ]
             ]
           {-,-}
           ]

