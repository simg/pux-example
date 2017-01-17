module App.Models.Notification where

import Foundation

import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL

import App.Models.Common
import App.Utils

-------------------------------------------------------------------------------------------------------
-- | Data 
-------------------------------------------------------------------------------------------------------

data Notification = Notification {
  message :: String
, icon :: String
, status :: NotificationStatus
, time :: DateTime
}
derive instance genericNotification :: Generic Notification

instance isForeignNotification :: IsForeign Notification where
  read val = do
    icon    <- readString =<< readProp "icon" val
    message <- readString =<< readProp "message" val
    status  <- readProp "status" val
    time    <- parseDate  =<< readProp "sent_time"    val
    pure $ Notification { message:message, status:status, icon:icon, time:time}
    where 
      parseDate str = exceptNoteA ((Identity <<< spy parseDateTime) $ spy str) $ NEL.singleton $ JSONError "invalid date"      

data NotificationStatus 
    = NotificationUnSeen | NotificationSeen
derive instance notificationStatus :: Generic NotificationStatus
derive instance eqNotificationStatus :: Eq NotificationStatus

instance showNotificationStatus :: Show NotificationStatus where
  show NotificationSeen   = "seen"
  show NotificationUnSeen = "unseen"

instance notificationStatusIsForeign :: IsForeign NotificationStatus where
  read val = do
    status <- readString val
    pure $ case status of 
      "unseen" -> NotificationUnSeen
      "seen"   -> NotificationSeen
      _        -> NotificationUnSeen --TODO: something better, but this OK for now

-------------------------------------------------------------------------------------------------------
-- | API
-------------------------------------------------------------------------------------------------------


