{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, DeriveGeneric, RecordWildCards #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Monad
import Data.Function
import Data.Time
import Data.Generics.Product hiding (list)
import GHC.Generics
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import Lens.Micro
import qualified Options.Applicative as O
import System.Directory

-- PARSER

data Options
  = Options
    {
    }

optparser :: O.Parser Options
optparser = pure Options

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


-- FILES AND USER

data ModelType = Exponential Double | Linear Double Double | Logarithmic Double
  deriving (Show, Eq)

data Todo
  = Todo
    { todoName :: String
    , todoDue  :: UTCTime
    , todoMod  :: ModelType
    }
  | Done
    { todoName :: String
    , todoDone :: UTCTime
    , todoDue  :: UTCTime
    , todoMod  :: ModelType
    }

baseDir :: IO FilePath 
baseDir = fmap (<> "/.odot/") getHomeDirectory

doTheThing :: Options -> IO ()
doTheThing opts = do
  baseDirExists <- doesDirectoryExist =<< baseDir
  unless baseDirExists $ do
     createDirectory =<< baseDir

  let todoEditor = editor "Search" (Just 1) ""

  now <- getCurrentTime 
  
  void $ defaultMain app $ AppState
    { todos         = [ Todo "pet my cat" now (Linear 1 0) ]
    , editorActive  = False
    , addTodoEditor = todoEditor
    }

-- TUI

data AppState
  = AppState 
    { todos         :: [Todo]
    , addTodoEditor :: Editor String String
    , editorActive  :: Bool
    }
  deriving Generic

app :: App AppState () String
app =
  App
  { appDraw = draw
  , appChooseCursor = const.const Nothing
  , appHandleEvent  = update
  , appStartEvent   = pure
  , appAttrMap      = const (attrMap defAttr 
    [ (attrName "infoTitle", fg magenta)
    ])
  }

draw :: AppState -> [Widget String]
draw s = 
  [ borderWithLabel (center $ title "Odot") $ vBox
    [ {- border $ padAll 1 $
      htitle "Add a todo: " <+> renderEditor drawEditor True (addTodoEditor s)
      -}
      padAll 1 $ hCenter $ border $ hCenter $ padTopBottom 2 $ str "WIP" 
    , hCenter $ title "Here are your todos:"
    , padAll 1 $ viewport "TodosViewport" Vertical 
      $ vBox $ map drawTodo (todos s)
    ]
  ]
    where
      title t =
        withAttr "infoTitle" $
        txt t

      drawEditor = foldl1 (<+>).map str

      drawTodo Todo {..} = border $ padAll 1 $
        str todoName <+> strWrap " " <+> str (formatTime defaultTimeLocale "%a, %d/%m, %l%P" todoDue)
      drawTodo Done {  } = undefined

update :: AppState -> BrickEvent String () -> EventM String (Next AppState)

update s (VtyEvent (V.EvKey (K.KChar 'q') []))
  | s & not.editorActive = halt s

update s (VtyEvent (V.EvKey (K.KChar 'a') []))
  | s & not.editorActive 
  = s
  & field @"editorActive" .~ True
  & continue

update s (VtyEvent (V.EvKey K.KEnter      [])) = s
  & field @"editorActive" .~ False
  & continue

update s (VtyEvent e)
  | editorActive s = do

  editor' <- s 
    & addTodoEditor 
    & handleEditorEvent e
  s 
    & field @"addTodoEditor" .~ editor' 
    & continue

update s _ = continue s

