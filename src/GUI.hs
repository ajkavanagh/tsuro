{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GUI where

import           Control.Monad                 (void)

import           GI.Gtk                        (Label (..), Window (..),
                                                Button (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple


data State = NotClicked | Clicked

data Event = ButtonClicked | Closed

view' :: State -> AppView Window Event
view' s =
    bin Window [ #title := "Hi there"
               , on #deleteEvent (const (True, Closed))
               , #widthRequest := 400
               , #heightRequest := 300
               ]
        $ case s of
            NotClicked ->
                widget
                Button [ #label := "Click me"
                        , on #clicked ButtonClicked
                        ]
            Clicked ->
                widget
                Button [ #label := "Thanks for clicking me"
                        , #sensitive := False
                        ]

update' :: State -> Event -> Transition State Event
update' _ ButtonClicked = Transition Clicked (return Nothing)


guiMain :: IO ()
guiMain = void $ run
    App { view = view'
        , update = update'
        , inputs = []
        , initialState = NotClicked
        }
