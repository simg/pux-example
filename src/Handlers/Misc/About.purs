module Handlers.About where

import Foundation
import Handlers.Common


data Action = Nop

page :: Html Action
page = div
         [] [ 
           h1 [] [ text "About" ]
         , p [] [ text "This App is an example of a mobile first, responsive, pure functional web app built with Purescript and Purescript-Pux." ]
         , p [] [ text "Features:" ]
         , ul [ className "browser-default" ] [ 
              li [] [ text "WebSockets" ]
            , li [] [ text "Strongly Typed Pure Functional code" ]
            , li [] [ text "Authentication with JSON Web Tokens (JWT)" ]
            , li [] [ text "Authenticated RESTFUL API client" ]
            , li [] [ text "Material Design" ]
            , li [] [ text "Webpack" ]
            , li [] [ text "Offline Mobile First" ]
           ]
         ]  
