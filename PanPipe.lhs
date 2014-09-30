> module PanPipe where

> import Control.Applicative
> import Data.List
> import System.IO.Temp (withSystemTempDirectory)
> import System.Process
> import Text.Pandoc
> import Text.Pandoc.Shared (inDirectory)
> import Text.Pandoc.Walk (walkM)

> pipeB :: Block -> IO Block
> pipeB (CodeBlock as s)
>     |  Just (as', p) <- partPipes as = CodeBlock as' <$> readShell p s
> pipeB x = walkM pipeI x

> pipeI :: Inline -> IO Inline
> pipeI (Code as s)
>     |  Just (as', p) <- partPipes as = Code as' <$> readShell p s
> pipeI x = walkM pipeI x

> readShell :: FilePath -> String -> IO String
> readShell p s = readProcess "sh" ["-c", p] s

> partPipes :: Attr -> Maybe (Attr, String)
> partPipes (x, y, zs) = case partition (("pipe" ==) . fst) zs of
>                             ((_, p):_, zs') -> Just ((x, y, zs'), p)
>                             _               -> Nothing

> transform :: Pandoc -> IO Pandoc
> transform doc = withSystemTempDirectory "panpipe" $ (`inDirectory` transformDoc doc)

Use Pandoc to parse, traverse and pretty-print our documents

> transformDoc :: Pandoc -> IO Pandoc
> transformDoc = walkM pipeB

> readDoc :: String -> Pandoc
> readDoc = readMarkdown def

> writeDoc :: Pandoc -> String
> writeDoc = writeMarkdown def

> processDoc :: String -> IO String
> processDoc = fmap writeDoc . transform . readDoc
