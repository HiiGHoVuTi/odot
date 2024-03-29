{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, DeriveGeneric, RecordWildCards, DeriveAnyClass, NumericUnderscores, BangPatterns #-}

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

-- | "main" function that runs one action
doTheThing :: Options -> IO ()
doTheThing opts = do
  baseDirExists <- doesDirectoryExist =<< baseDir
  todosPath <- (<> "todos.bin") <$> baseDir
  donePath  <- (<> "done.bin" ) <$> baseDir

  unless baseDirExists $ do
     createDirectory =<< baseDir
     writeFile todosPath ""
     writeFile donePath  ""
  
  ioTodos <- getSerialisedFile todosPath
  now <- getCurrentTime 

  case opts of
    
    -- displays the TUI
    Tui -> do
      chan <- BCh.newBChan 5
      -- Pings the channel every minute
      void . forkIO . forever $ do
        t <- getCurrentTime
        d <- getSerialisedFile todosPath
        BCh.writeBChan chan (t, d)
        threadDelay 60_000_000

      -- Create the Brick app
      vty <- V.mkVty V.defaultConfig
      void $ customMain vty (pure vty) (Just chan) app $ AppState
        { todos         = ioTodos
        , currentTime   = now
        , editorActive  = False
        , addTodoEditor = editor "Search" (Just 1) ""
        }
    
    -- Displays the list
    List Nothing ->
      ioTodos 
      & sortTodos now 
      & zip [1..] 
      & pPrint now & putStrLn
    
    -- Filters by taglist then displays
    List (Just tags') ->
      ioTodos
      & sortTodos now
      & zip [1..]
      & filter (flip all tags' . flip elem . tags . snd) 
      & pPrint now & putStrLn

    -- Adds a todo
    New todo -> let
      -- timestamps the todo
      stamped  = todo & field @"todoDate" %~ (Just.(?: now))
      newTodos = stamped : ioTodos
      !binaryD = CS.serialise newTodos
                 in LBS.writeFile todosPath binaryD
    
    -- Filter-out all matching ids
    Remove ids -> let
      newTodos = ioTodos
        & sortTodos now
        & zip [1..]
        & filter ((`notElem` ids).fst)
        & map snd
      binaryD  = CS.serialise newTodos
                   in LBS.writeFile todosPath binaryD

    -- Get all done items, then turn them into Done objects
    Complete ids -> do
      ioDone <- getSerialisedFile donePath
      let 
        newElem = ioTodos
          & sortTodos now
          & zip [1..]
          & filter ((`elem` ids).fst)
          & map (toDone now . snd)
        newDone = newElem ++ ioDone
        binaryD = CS.serialise newDone
      LBS.writeFile donePath binaryD
      doTheThing (Remove ids)
    

getSerialisedFile :: CS.Serialise a => String -> IO [a]
getSerialisedFile path = do
  ioTodos' <- CS.deserialiseOrFail . LBS.fromStrict <$> SBS.readFile path

  case ioTodos' of
    Left _  -> pure []
    Right a -> pure a


