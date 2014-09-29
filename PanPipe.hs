module PanPipe where

import Control.Applicative
import Data.List
import Data.Maybe
import System.Directory
import System.IO.Temp
import System.Process
import Text.Pandoc
import Text.Pandoc.Walk (walkM)

key = "pipe"

pipe :: Block -> IO Block
pipe (CodeBlock as s) = case partPipes as of
                             (as', Nothing) -> CodeBlock as' <$> return s
                             (as', Just p)  -> CodeBlock as' <$> readProcess p [] s
pipe x                = return x

partPipes :: Attr -> (Attr, Maybe String)
partPipes (x, y, zs) = let (zs', pipes) = partition ((key ==) . fst) zs
                        in ((x, y, zs'), snd <$> listToMaybe pipes)

transform :: Pandoc -> IO Pandoc
transform doc = let f dir = do cwd <- getCurrentDirectory
                               setCurrentDirectory dir
                               let doc' = transformDoc doc
                               setCurrentDirectory cwd
                               doc'
                 in withSystemTempDirectory "panpipe" f

transformDoc = walkM pipe

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

processDoc :: String -> IO String
processDoc = fmap writeDoc . transform . readDoc
