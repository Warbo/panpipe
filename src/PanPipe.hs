module PanPipe where

import Control.Applicative
import Data.List
import System.Exit
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix
import System.Process
import Text.Pandoc
import Text.Pandoc.JSON   (toJSONFilter)
import Text.Pandoc.Shared (inDirectory)
import Text.Pandoc.Walk   (walkM)

pipeBWith :: (Functor m, Monad m) => (String -> String -> m String)
                                  -> Block
                                  -> m Block
pipeBWith f (CodeBlock as s)
          |  Just (as', p) <- partPipes as = CodeBlock as' <$> f p s
pipeBWith f x = walkM (pipeIWith f) x

pipeB = pipeBWith readShell

pipeIWith :: (Functor m, Monad m) => (String -> String -> m String)
                                  -> Inline
                                  -> m Inline
pipeIWith f (Code as s)
          |  Just (as', p) <- partPipes as = Code as' <$> f p s
pipeIWith f x = return x

pipeI = pipeIWith readShell

readShell :: FilePath -> String -> IO String
readShell p = readProcess "sh" ["-c", p]

partPipes :: Attr -> Maybe (Attr, String)
partPipes (x, y, zs) = case partition (("pipe" ==) . fst) zs of
                            ((_, p):_, zs') -> Just ((x, y, zs'), p)
                            _               -> Nothing

transform :: Pandoc -> IO Pandoc
transform doc = do cwd <-getWorkingDirectory
                   withSystemTempDirectory
                       "panpipe"
                       (\dir -> do createSymbolicLink cwd (dir ++ "/root")
                                   inDirectory dir (transformDoc doc))

transformDoc :: Pandoc -> IO Pandoc
transformDoc = walkM pipeB

panpipeMain = toJSONFilter transform
