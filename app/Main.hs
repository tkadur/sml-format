module Main where

import           Options.Applicative
import           Language.Sml
import qualified Language.Sml.Pretty           as Pretty

import           Config

handle :: Parser InputFile
handle =
  (\case
      Nothing       -> Stdin
      Just filename -> File filename
    )
    <$> optional (argument str (metavar "FILE"))

config :: Parser Config
config =
  Config
    <$> option
          (auto @Int)
          (long "indent-width" <> help "Number of spaces to use for indentation" <> showDefault <> value 2 <> metavar
            "WIDTH"
          )

    <*> option (auto @Int)
               (long "line-length" <> help "Maximum line length" <> showDefault <> value 80 <> metavar "LENGTH")

    <*> handle

main :: IO ()
main = do
  Config {..} <- execParser $ info (config <**> helper) idm
  let filename = inputFileName inputFile
  input <- readInput inputFile

  (comments, lexed) <- case lex filename input of
    Right res -> return res
    Left  err -> reportLexerError err

  parsed <- case parse lexed of
    Right res -> return res
    Left  err -> reportParserError err

  let prettyPrintConfig = Pretty.Config { Pretty.lineLength, Pretty.indentWidth }

  let prettyPrinted     = prettyPrint prettyPrintConfig comments parsed

  -- Safety check
  let (recomments, relexed) = case lex "recheck" prettyPrinted of
        Right res -> res
        Left  _   -> error "Relexing failed"
  let reparsed = case parse relexed of
        Right res -> res
        Left  _   -> error "Reparsing failed"
  if comments /= recomments
    then error "Relexed comments were different"
    else if parsed /= reparsed then error "Reparsed AST was different" else return ()


  putTextLn prettyPrinted
