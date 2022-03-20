{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Lib

import Data.Semigroup ((<>))
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
app = 
  get root $ do
    todos' <- getState >>= (liftIO . readIORef . todos)
    lucid $ do
        h1_ "Todo List"
        ul_ $
        forM_ todos' $ \todo -> li_ $ do
            toHtml (author todo)
            ": "
            toHtml (contents todo)
        h2_ "New Todo"
        form_ [method_ "post"] $ do
            label_ $ do
                "Author: "
                input_ [name_ "author"]
            label_ $ do
                "Contents: "
                textarea_ [name_ "contents"] ""
            input_ [type_ "submit", value_ "Add Todo"]
  post root $ do
      author <- param' "author"
      contents <- param' "contents"
      todosRef <- todos <$> getState
      liftIO $ atomicModifyIORef' todosRef $ \todos ->
          (todos <> [Todo author contents], ())
      redirect "/"

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