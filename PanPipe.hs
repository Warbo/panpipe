module PanPipe where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Maybe
import System.Directory
import System.IO.Temp
import System.Process
import Text.Pandoc
import Text.Pandoc.Walk (walkM)

pipe :: Block -> IO Block
pipe (CodeBlock as s) = case partPipes as of
                             (as', Nothing) -> CodeBlock as' <$> return s
                             (as', Just p)  -> CodeBlock as' <$> readProcess p [] s
pipe x                = return x

partPipes :: Attr -> (Attr, Maybe String)
partPipes (x, y, zs) = let (pipes, zs') = partition (("pipe" ==) . fst) zs
                        in ((x, y, zs'), snd <$> listToMaybe pipes)

transform :: Pandoc -> IO Pandoc
transform doc = withSystemTempDirectory "panpipe" $ doInDir `flip` transformDoc doc

cd :: FilePath -> IO FilePath
cd d = getCurrentDirectory <* setCurrentDirectory d

doInDir :: FilePath -> IO a -> IO a
doInDir d = bracket (cd d) setCurrentDirectory . const

transformDoc = walkM pipe

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

processDoc :: String -> IO String
processDoc = fmap writeDoc . transform . readDoc
