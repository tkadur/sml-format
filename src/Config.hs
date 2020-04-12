module Config where

import           System.IO                      ( getContents )

data Config =
  Config
  { indentWidth :: Int
  , lineLength :: Int
  , inputFile :: InputFile
  }

data InputFile = Stdin | File String

inputFileName :: InputFile -> String
inputFileName = \case
  Stdin         -> "stdin"
  File filename -> filename

readInput :: InputFile -> IO Text
readInput input = toText <$> case input of
  Stdin         -> getContents
  File filename -> readFile filename
