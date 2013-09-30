{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.AhoCorasick
import           Data.AhoCorasick.Conduit
import           Data.Conduit
import           Data.Text


main :: IO ()
main = do
    let acm = construct ["he", "she", "his", "hers"]
    yield "ushers" $$ conduitACMachineText acm =$ awaitForever (liftIO . print)
    let acm' = constructWithValues [("he", 1.2), ("she", 3.4), ("his", 5.6), ("hers", 7.8)]
    yield "ushers" $$ conduitACMachineText acm' =$ awaitForever (liftIO . print)
