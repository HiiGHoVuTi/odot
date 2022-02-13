{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Logic (
  ModelType(..), Todo(..), Done(..), sortTodos, sortTodos', pPrint, showTime, toDone
              ) where


import qualified Codec.Serialise as CS
import Data.Function
import Data.List
import Data.Time
import GHC.Generics

data ModelType = Exponential Double | Linear Double Double | Logarithmic Double
  deriving (Show, Eq, Generic, CS.Serialise)

data Todo
  = Todo
    { todoName :: String
    , todoDue  :: Maybe UTCTime
    , todoDate :: UTCTime
    , todoMod  :: ModelType
    , tags     :: [String]
    }
  deriving (Generic, CS.Serialise)

data Done = Done
    { doneName :: String
    , doneDate :: UTCTime
    , doneDue  :: Maybe UTCTime
    , doneMod  :: ModelType
    , doneTags :: [String]
    }
  deriving (Generic, CS.Serialise)


-- Tue 03/02/2022, 11am
showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%a %d/%m/%Y, %l%P"

pPrint :: UTCTime -> [(Int, Todo)] -> String
pPrint t = unlines . map (uncurry showTodo) . sortTodos' snd t
  where
    showTodo :: Int -> Todo -> String
    showTodo n Todo{..} =
      case todoDue of
        Just dueDate 
          -> show n     <> "- " <> todoName 
          <> " | due "  <> showTime dueDate
        Nothing
          -> show n     <> "- " <> todoName 
          <> " | from " <> showTime todoDate


toDone :: UTCTime -> Todo -> Done
toDone t Todo{..} = Done
  { doneName = todoName
  , doneDate = t
  , doneDue  = todoDue
  , doneMod  = todoMod
  , doneTags = tags
  }

sortTodos :: UTCTime -> [Todo] -> [Todo]
sortTodos = sortTodos' id

-- | Generalised sorting function with a predicate
sortTodos' :: (a -> Todo) -> UTCTime -> [a] -> [a]
sortTodos' p' t = sortBy (compare `on` getPriority . p')
  where
    diffInDays :: UTCTime -> UTCTime -> Double
    diffInDays t1 t2 =
      let
        elapsed = diffUTCTime t1 t2
        seconds = nominalDiffTimeToSeconds elapsed
        days    = realToFrac (seconds / (3600 * 24))
      in days

    getPriority Todo{..} = 
      case todoDue of
        Just dueDate ->
          let
            totalTime = diffInDays todoDate dueDate
            elapsed   = diffInDays t todoDate
           in -case todoMod of
                 Linear m p    -> p   + elapsed * (m  * 50 - p) / totalTime
                 Exponential k -> exp $ elapsed * (log (50 * k) / totalTime)
                 Logarithmic k -> log $ elapsed * (exp (50 * k) / totalTime)
        Nothing      -> 
          let
            days    = diffInDays t todoDate
           in -case todoMod of
                Linear m p    -> m * days + p
                Exponential k -> exp (k * days / 10)
                Logarithmic k -> log (k * days * 50)


