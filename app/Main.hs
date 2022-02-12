{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds, DeriveGeneric, RecordWildCards, DeriveAnyClass, BangPatterns #-}

module Main where

import Brick hiding (on)
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Codec.Serialise as CS
import Control.Monad
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.List
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
  = Tui
  | New Todo


newparser :: O.Parser Options
newparser = New <$> todo
  where
    expo = Exponential
      <$> O.option O.auto
        ( O.long "exp-coef"
       <> O.help "exponential coefficient"
       <> O.metavar "EXP"
        )
    lin = Linear
      <$> O.option O.auto
        ( O.long "lin-coef"
       <> O.help "linear coefficient"
       <> O.metavar "LIN"
        )
      <*> O.option O.auto
        ( O.long "lin-offset"
       <> O.help "linear offset"
       <> O.metavar "OFF"
       <> O.value 0
        )
    loga = Exponential
      <$> O.option O.auto
        ( O.long "log-coef"
       <> O.help "logarithmic coefficient"
       <> O.metavar "LOG"
        )


    parseTime' :: String -> UTCTime
    parseTime' timeString =
      case parseTimeM True defaultTimeLocale "%-d/%-m %Hh" timeString of
        Just t -> t
        Nothing -> error "Invalid format, should be DD/MM HHh"

    todo = Todo
      <$> O.strOption
        ( O.long "name"
       <> O.metavar "NAME"
       <> O.help "your todo's name"
        )
      <*> (fmap parseTime' <$>
      O.optional (O.strOption
        ( O.long "due"
       <> O.metavar "DUE"
       <> O.help "the date your todo is due, DD/MM(/YY)-hh"
        )))
      <*> pure undefined -- NOTE(Maxime): is replaced later
      <*> (expo O.<|> lin O.<|> loga)



optparser :: O.Parser Options
optparser 
  =     O.subparser
    ( O.command "tui" (O.info (pure Tui) (O.progDesc "open the tui"))
    )
  O.<|> O.subparser
    ( O.command "new" (O.info newparser (O.progDesc "add a new todo"))
    )

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
  deriving (Show, Eq, Generic, CS.Serialise)

data Todo
  = Todo
    { todoName :: String
    , todoDue  :: Maybe UTCTime
    , todoDate :: UTCTime
    , todoMod  :: ModelType
    }
    {-
  | Done
    { todoName :: String
    , todoDone :: UTCTime
    , todoDue  :: Maybe UTCTime
    , todoMod  :: ModelType
    }
    -}
  deriving (Generic, CS.Serialise)

baseDir :: IO FilePath 
baseDir = fmap (<> "/.odot/") getHomeDirectory

doTheThing :: Options -> IO ()
doTheThing opts = do
  baseDirExists <- doesDirectoryExist =<< baseDir
  todosPath <- (<> "todos.bin") <$> baseDir

  unless baseDirExists $ do
     createDirectory =<< baseDir
     writeFile todosPath ""


  ioTodos' <- CS.deserialiseOrFail . LBS.fromStrict <$> SBS.readFile todosPath
  let 
    ioTodos = case ioTodos' of
      Left _  -> []
      Right a -> a
  
  now <- getCurrentTime 

  case opts of
    Tui ->
      void $ defaultMain app $ AppState
        { todos         = ioTodos
        , currentTime   = now
        , editorActive  = False
        , addTodoEditor = editor "Search" (Just 1) ""
        }
    New todo -> let
      -- NOTE(Maxime): sort 'em ?
      stamped  = todo & field @"todoDate" .~ now
      newTodos = stamped : ioTodos
      binaryD  = CS.serialise newTodos
                 in LBS.writeFile todosPath binaryD

-- TUI

data AppState
  = AppState 
    { todos         :: [Todo]
    , currentTime   :: UTCTime
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
      padAll 1 $ hCenter $ border $ hCenter $ padTopBottom 1 $ str "WIP" 

    , hCenter $ title "Here are your todos:"
    
    , padAll 1 $ withClickableVScrollBars (const id) $ 
      withVScrollBarHandles $ withVScrollBars OnRight $
      viewport "TodosViewport" Vertical $
      
      vBox $ map drawTodo $ sortTodos (currentTime s) (todos s)
    ]
  ]
    where
      title t =
        withAttr "infoTitle" $
        txt t

      drawEditor = foldl1 (<+>).map str

      drawDue _ (Just t) = str ("due "  <> formatTime defaultTimeLocale "%a %d/%m, %l%P" t)
      drawDue t Nothing  = str ("from " <> formatTime defaultTimeLocale "%a %d/%m, %l%P" t)


      drawTodo Todo {..} = border $ padAll 1 $
        str todoName <+> strWrap " " <+> drawDue todoDate todoDue

update :: AppState -> BrickEvent String () -> EventM String (Next AppState)

update s (VtyEvent (V.EvKey (K.KChar 'q') []))
  | s & not.editorActive = halt s

{- 
update s (VtyEvent (V.EvKey (K.KChar 'a') []))
  | s & not.editorActive 
  = s
  & field @"editorActive" .~ True
  & continue
-}

update s (VtyEvent (V.EvKey K.KEnter      [])) = s
  & field @"editorActive" .~ False
  & continue

update s (VtyEvent (V.EvKey K.KUp         [])) = 
  vScrollBy (viewportScroll "TodosViewport") (-1) >> continue s

update s (VtyEvent (V.EvKey K.KDown       [])) = 
  vScrollBy (viewportScroll "TodosViewport")   1  >> continue s


update s (VtyEvent e)
  | editorActive s = do

  editor' <- s 
    & addTodoEditor 
    & handleEditorEvent e
  s 
    & field @"addTodoEditor" .~ editor' 
    & continue

update s _ = continue s


sortTodos :: UTCTime -> [Todo] -> [Todo]
sortTodos t = sortBy (compare `on` getPriority)
  where
    getPriority Todo{..} = 
      case todoDue of
        Just dueDate -> 0 :: Double -- TODO(Maxime)
        Nothing      -> 
          let
            elapsed = diffUTCTime t todoDate 
            seconds = nominalDiffTimeToSeconds elapsed
            days    = realToFrac (seconds / (3600 * 24))
           in -case todoMod of
                Linear m p    -> m * days + p
                Exponential k -> exp (k * days / 10)
                Logarithmic k -> log (k * days * 50)

