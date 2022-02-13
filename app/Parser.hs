
module Parser (
  optparser, Options(..)
              ) where

import Logic

import Data.Time
import Options.Applicative

data Options
  = Tui
  | List (Maybe [String])
  | New Todo
  | Remove [Int]
  | Complete [Int]

newparser :: Parser Options
newparser = New <$> todo
  where
    expo = Exponential
      <$> option auto
        ( long "exp-coef"
       <> help "exponential coefficient"
       <> metavar "EXP"
        )
    lin = Linear
      <$> option auto
        ( long "lin-coef"
       <> help "linear coefficient"
       <> metavar "LIN"
        )
      <*> option auto
        ( long "lin-offset"
       <> help "linear offset"
       <> metavar "OFF"
       <> value 0
        )
    loga = Exponential
      <$> option auto
        ( long "log-coef"
       <> help "logarithmic coefficient"
       <> metavar "LOG"
        )


    parseTime' :: String -> UTCTime
    parseTime' timeString =
      case parseTimeM True defaultTimeLocale "%-d/%-m/%-Y %Hh" timeString of
        Just t -> t
        Nothing -> error "Invalid format, should be DD/MM/YY HHh"

    todo = Todo
      <$> strOption
        ( long "name"
       <> metavar "NAME"
       <> help "your todo's name"
        )
      <*> (fmap parseTime' <$>
      optional (strOption
        ( long "due"
       <> metavar "DUE"
       <> help "the date your todo is due, ex: 22/08/1993 12h"
        )))
      <*> pure undefined -- NOTE(Maxime): is replaced later
      <*> (expo <|> lin <|> loga)
      <*> (words <$> strOption
        ( long "tags"
       <> short 't'
       <> metavar "TAGS"
        ))

listParser :: Parser Options
listParser = List <$>
  optional (words <$> strOption 
    ( long "tags"
   <> short 't'
   <> metavar "TAGS"
    )
  )

remparser :: Parser Options
remparser = Remove . pure <$>
  option auto
    ( long  "idx"
   <> short 'i'
   <> metavar "REMIDX"
   <> help "the unique index of the item to remove"
    )

cmpparser :: Parser Options
cmpparser = Complete . pure <$>
  option auto
    ( long  "idx"
   <> short 'i'
   <> metavar "COMPLETEIDX"
   <> help "the unique index of the item to mark as complete"
    )


optparser :: Parser Options
optparser 
  =     subparser
    ( command "tui"    (info (pure Tui) (progDesc "open the tui"))
    )
  <|> subparser
    ( command "list"   (info listParser (progDesc "list current todos"))
   <> command "new"    (info newparser  (progDesc "add a new todo"))
   <> command "remove" (info remparser  (progDesc "removes items"))
   <> command "done"   (info cmpparser  (progDesc "marks items as done"))
    )


