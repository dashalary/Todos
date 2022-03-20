{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Lib

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Text (Text)
import Lucid
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)

data Todo = Todo {author :: Text, contents :: Text}

newtype ServerState = ServerState {todos :: IORef [Todo]}

type Server a = SpockM () () ServerState a

app :: Server ()
app = get root $ do
  todos' <- getState >>= (liftIO . readIORef . todos)
  lucid $ do
    h1_ "Todo List"
    ul_ $
      forM_ todos' $ \todo -> li_ $ do
        toHtml (author todo)
        ": "
        toHtml (contents todo)

main :: IO ()
main = do
  st <-
    ServerState
      <$> newIORef
        [ Todo "Dasha" "Call Dylan",
          Todo "Doug" "Work on the car"
        ]
  cfg <- defaultSpockCfg () PCNoDatabase st
  runSpock 8080 (spock cfg app)