module Main where

import           Options.Applicative
import qualified Language.Sml.Lexer            as Lexer
import qualified Language.Sml.Parser           as Parser
import qualified Language.Sml.Pretty           as Pretty
-- import           System.IO

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

  (comments, lexed) <- case Lexer.runLexer filename input of
    Right res -> return res
    Left  err -> do
      putStrLn (Lexer.showError err)
      exitFailure

  parsed <- case Parser.parseToplevel filename input lexed of
    Right res -> return res
    Left  err -> do
      putStrLn (Parser.showError err)
      exitFailure

  let prettyPrinted = Pretty.prettyPrint (Pretty.Config { Pretty.lineLength, Pretty.indentWidth }) comments parsed
  putTextLn prettyPrinted
