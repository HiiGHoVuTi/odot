{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, DeriveGeneric, RecordWildCards, DeriveAnyClass, BangPatterns, NumericUnderscores #-}

module Main where

import Logic
import Parser
import Tui

import Brick hiding (on)
import qualified Brick.BChan as BCh
import Brick.Widgets.Edit
import qualified Codec.Serialise as CS
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Time
import Data.Generics.Product hiding (list)
import qualified Graphics.Vty as V
import Lens.Micro
import qualified Options.Applicative as O
import System.Directory


main :: IO ()
main = O.execParser optinfo >>= doTheThing
  where
    optinfo = O.info (optparser O.<**> O.helper)
      $ O.fullDesc
     <> O.progDesc (unlines
        [
        ]
                 )
     <> O.header "Odot -- a TODO TUI"


baseDir :: IO FilePath 
baseDir = fmap (<> "/.odot/") getHomeDirectory

doTheThing :: Options -> IO ()
doTheThing opts = do
  baseDirExists <- doesDirectoryExist =<< baseDir
  todosPath <- (<> "todos.bin") <$> baseDir
  donePath  <- (<> "done.bin" ) <$> baseDir

  unless baseDirExists $ do
     createDirectory =<< baseDir
     writeFile todosPath ""
     writeFile donePath  ""

  
  ioTodos <- getTodos
  now <- getCurrentTime 

  case opts of
    Tui -> do
      chan <- BCh.newBChan 5
      void . forkIO . forever $ do
        t <- getCurrentTime
        d <- getTodos
        BCh.writeBChan chan (t, d)
        threadDelay 1_000_000
      vty <- V.mkVty V.defaultConfig
      void $ customMain vty (pure vty) (Just chan) app $ AppState
        { todos         = ioTodos
        , currentTime   = now
        , editorActive  = False
        , addTodoEditor = editor "Search" (Just 1) ""
        }
    List Nothing ->
      putStrLn $ pPrint now ioTodos
    List (Just tags') ->
      ioTodos 
      & filter (flip all tags' . flip elem . tags) 
      & pPrint now & putStrLn
    New todo -> let
      -- NOTE(Maxime): sort 'em ?
      stamped  = todo & field @"todoDate" .~ now
      newTodos = stamped : ioTodos
      binaryD  = CS.serialise newTodos
                 in LBS.writeFile todosPath binaryD
    Remove ids -> let
      newTodos = ioTodos
        & sortTodos now
        & zip [1..]
        & filter ((`notElem` ids).fst)
        & map snd
      binaryD  = CS.serialise newTodos
                   in LBS.writeFile todosPath binaryD

getTodos :: IO [Todo]
getTodos = do
  ioTodos' <- CS.deserialiseOrFail . LBS.fromStrict <$> (SBS.readFile . (<> "todos.bin") =<< baseDir)

  case ioTodos' of
    Left _  -> pure []
    Right a -> pure a


