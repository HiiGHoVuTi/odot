{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Logic (
  ModelType(..), Todo(..), sortTodos, pPrint, showTime
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


showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%a %d/%m, %l%P"

pPrint :: UTCTime -> [Todo] -> String
pPrint t = unlines . zipWith showTodo [1..] . sortTodos t
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


