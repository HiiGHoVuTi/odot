
module Parser (
  optparser, Options(..)
              ) where

import Logic

import Data.Time
import Options.Applicative

data Options
  = Tui
  | List
  | New Todo
  | Remove [Int]

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
      case parseTimeM True defaultTimeLocale "%-d/%-m %Hh" timeString of
        Just t -> t
        Nothing -> error "Invalid format, should be DD/MM HHh"

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
       <> help "the date your todo is due, DD/MM(/YY)-hh"
        )))
      <*> pure undefined -- NOTE(Maxime): is replaced later
      <*> (expo <|> lin <|> loga)

remparser :: Parser Options
remparser = Remove . pure <$>
  option auto
    ( long  "idx"
   <> short 'i'
   <> metavar "REMIDX"
   <> help "the unique index of the item to remove"
    )

optparser :: Parser Options
optparser 
  =     subparser
    ( command "tui"    (info (pure Tui)  (progDesc "open the tui"))
    )
  <|> subparser
    ( command "list"   (info (pure List) (progDesc "list current todos"))
   <> command "new"    (info newparser   (progDesc "add a new todo"))
   <> command "remove" (info remparser   (progDesc "removes a todo by idx"))
    )


