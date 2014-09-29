> module PanPipe where

> import Control.Applicative
> import Control.Exception
> import Data.List
> import Data.Maybe
> import System.Directory
> import System.IO.Temp
> import System.Process
> import Text.Pandoc
> import Text.Pandoc.Walk (walkM)

> pipe :: Block -> IO Block
> pipe (CodeBlock as s) = case partPipes as of
>                              (as', Nothing) -> CodeBlock as' <$> return s
>                              (as', Just p)  -> CodeBlock as' <$> readProcess "sh" ["-c", p] s
> pipe x                = return x

> partPipes :: Attr -> (Attr, Maybe String)
> partPipes (x, y, zs) = let (pipes, zs') = partition (("pipe" ==) . fst) zs
>                         in ((x, y, zs'), snd <$> listToMaybe pipes)

> transform :: Pandoc -> IO Pandoc
> transform doc = withSystemTempDirectory "panpipe" $ doInDir (transformDoc doc)

> cd :: FilePath -> IO FilePath
> cd d = getCurrentDirectory <* setCurrentDirectory d

> doInDir :: IO a -> FilePath -> IO a
> doInDir a d = bracket (cd d) setCurrentDirectory (const a)

Use Pandoc to parse, traverse and pretty-print our documents

> transformDoc = walkM pipe

> readDoc :: String -> Pandoc
> readDoc = readMarkdown def

> writeDoc :: Pandoc -> String
> writeDoc = writeMarkdown def

> processDoc :: String -> IO String
> processDoc = fmap writeDoc . transform . readDoc
