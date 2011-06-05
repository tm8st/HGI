-- Global illumination renderer implement in haskell.

module HGI where

import Text.Parsec
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Char

-- readObjFile :: String -> IO ObjMesh
-- readObjFile filePath =

-- objFile = endBy

-- csvFile = endBy line eol
-- line = seqBy cell (char ',')
-- cell = many (noneOf ",\n")
-- eol = char '\n'
-- parseCSV input = parse csvFile "(unknown)" input

-- simple :: Parser Char
-- simple = letter


-- run :: Show a => Parser a -> String -> IO ()
-- run p input =
--     case (parse p "" input) of
--         Left err -> do putStr "parse error at "
--                        print err
--         Right x  -> print x

