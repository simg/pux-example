module App.Models.Message where

import Foundation

import Data.Argonaut.Encode
import Data.DateTime.Locale
import Data.String as Str


import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL

import Data.JSDate as JSD

import Global.Unsafe

--import Data.Foreign.Generic


import App.Models.Common
import App.Utils



-------------------------------------------------------------------------------------------------------
-- | Data 
-------------------------------------------------------------------------------------------------------

data MessageReceive = MessageReceive {
  uuid       :: UUID
, sender     :: Int
, icon       :: String
, name       :: String
, sentAt     :: DateTime
, recipients :: Array Int
, readAts    :: Array (Maybe DateTime)
, message    :: String
}
derive instance genericMessageReceive :: Generic MessageReceive


instance isForeignMessageReceive :: IsForeign MessageReceive where
  read val = do
    uuid       <- parseUUID_ =<< readString =<< readProp "uuid" val
    sender     <- readInt           =<< readProp "sender"       val
    icon       <- readString        =<< readProp "icon"         val
    name       <- readString        =<< readProp "name"         val
    sentAt     <- parseDate         =<< readProp "sent_time"    val
    recipients <-                       readProp "recipients"   val
    readAts    <- map parseDateTime <$> readProp "read_times"   val
    message    <- readString        =<< readProp "message"      val
    pure $ MessageReceive {uuid:uuid, sender:sender, icon:icon, name:name, sentAt:sentAt, recipients:recipients, readAts:readAts, message:message}
    where
      parseUUID_ u = exceptNoteA ((Identity <<< parseUUID) u) $ NEL.singleton $ JSONError "invalid uuid"
      parseDate str = exceptNoteA ((Identity <<< spy parseDateTime) $ spy str) $ NEL.singleton $ JSONError "invalid date"      

data MessageSend = MessageSend {
  recipients :: Array Int
, message    :: String
}
derive instance genericMessageSend :: Generic MessageSend

instance isForeignMessageSend :: IsForeign MessageSend where
  read x = readGeneric (defaultOptions) x  --{ unwrapSingleConstructors = true }

instance asForeignMessageSend :: AsForeign MessageSend where
  write x = toForeignGeneric defaultOptions x -- { unwrapSingleConstructors = true }) x


-------------------------------------------------------------------------------------------------------
-- | API
-------------------------------------------------------------------------------------------------------

sendMessage :: forall e. MessageSend -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Either String Unit)
sendMessage (MessageSend msg) = do
  res <- apiCall POST baseUrl "rpc/send_message" (Just devjwt) $ Just <<< foreignStringify <<< write $ MessageSend msg
  pure $ case res.response of 
           "[{\"send_message\":\"\"}]" -> Right unit  --TODO: something less clumsy than this
           _                           -> Left res.response


fetchMessages :: forall e. Int -> Aff (ajax :: AJAX, console :: CONSOLE | e) (Either String (Array MessageReceive))
fetchMessages recipient = do
  res <- apiCall GET baseUrl "message_list" (Just devjwt) $ Just $ "{\"recipient\":" <> (show recipient) <> "\"}"
  case runExcept ( readJSON res.response :: F (Array MessageReceive) ) of
    Left err -> do
      liftEff $ log $ "invalid message list" <> show err <> " : " <> res.response 
      pure $ Left $ "invalid message list" <> show err <> " : " <> res.response 
    Right messages -> do
      liftEff $ log $ "valid messages"                 
      pure $ Right messages
