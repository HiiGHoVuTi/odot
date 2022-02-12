
module Parser (
  optparser, Options(..)
              ) where

import Logic

import Data.Time
import qualified Options.Applicative as O

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


