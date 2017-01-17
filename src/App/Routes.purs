module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Prelude (($),(<$>))
import Pux.Router (param, router, lit, int, str, end)
import Pux.Html (Html)

import Handlers.Messages as Messages
import Handlers.People as People

data Route = Home 
           | About
           | Feedback
           | Login
           | Messenger Messages.Route
           | People People.Route
           | MyAccount 
           | NotFound
           | Notifier
           | Profiler
           | UserAdd           
           | UserAdmin
           | UserEdit String
           | UserView String
           
match :: String -> Route
match url = fromMaybe NotFound $ router url $
                  Home        <$   end
              <|> About       <$   lit "about"                      <* end              
              <|> Feedback    <$   lit "feedback"                   <* end                                
              <|> Login       <$   lit "login"                      <* end
              --<|> Messenger   <$   lit "messages"                   <* end              
              <|> Messenger   <$>  Messages.match
              <|> People      <$>  People.match
              <|> MyAccount   <$   lit "my-account"                 <* end
              <|> Profiler    <$   lit "profile"                    <* end
              <|> Notifier    <$   lit "notifications"              <* end              
              <|> UserAdmin   <$   lit "users"                      <* end
              <|> UserAdd     <$  (lit "user" *> lit "new")         <* end
              <|> UserEdit    <$> (lit "user" *> str <* lit "edit") <* end
              <|> UserView    <$> (lit "user" *> str)               <* end
              
              





