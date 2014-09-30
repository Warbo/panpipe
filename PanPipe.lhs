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

> pipeB :: Block -> IO Block
> pipeB (CodeBlock as s) = case partPipes as of
>                               (as', Nothing) -> CodeBlock as' <$> return s
>                               (as', Just p)  -> CodeBlock as' <$> readShell p s
> pipeB x                = return x

> pipeI :: Inline -> IO Inline
> pipeI (Code      as s) = case partPipes as of
>                               (as', Nothing) -> Code as' <$> return s
>                               (as', Just p)  -> Code as' <$> readShell p s
> pipeI x                = return x

> readShell :: FilePath -> String -> IO String
> readShell p s = readProcess "sh" ["-c", p] s

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

> transformDoc d = do d' <- walkM pipeB d
>                     walkM pipeI d'

> readDoc :: String -> Pandoc
> readDoc = readMarkdown def

> writeDoc :: Pandoc -> String
> writeDoc = writeMarkdown def

> processDoc :: String -> IO String
> processDoc = fmap writeDoc . transform . readDoc
