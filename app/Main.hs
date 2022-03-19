{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Lib

import Lucid
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)

type Server a = SpockM () () () a

app :: Server ()
app = get root $
  lucid $ do
    h1_ "Hello friend :)"
    p_ "How are you?"

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock cfg app)