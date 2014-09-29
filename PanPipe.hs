module PanPipe where

import Control.Applicative
import Data.List
import System.Directory
import System.IO.Temp
import System.Process
import Text.Pandoc
import Text.Pandoc.Walk (walkM)

key = "pipe"

pipe :: Block -> IO Block
pipe b = case b of
              CodeBlock as s -> runPipe as s
              _              -> return b

runPipe as s = case getPipe as of
                    Nothing -> return (CodeBlock as s)
                    Just p  -> fmap   (CodeBlock (noPipe as))
                                      (readProcess p [] s)

noPipe :: Attr -> Attr
noPipe (x, y, zs) = (x, y, filter ((/= key) . fst) zs)

getPipe (_, _, as) = snd <$> find ((== key) . fst) as

transform :: Pandoc -> IO Pandoc
transform = let f d = do setCurrentDirectory d
                         transformDoc
             in withSystemTempDirectory "panpipe" f

transformDoc = walkM pipe

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

processDoc :: String -> IO String
processDoc x = writeDoc <$> transform (readDoc x)
