{-# LANGUAGE OverloadedStrings #-}
module PanPipe where

import Control.Applicative
import Data.List
import Data.Text (pack, Text, unpack)
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Posix
import System.Process
import Text.Pandoc.Definition
import Text.Pandoc.JSON   (toJSONFilter)
import Text.Pandoc.Walk   (walkM)

pipeBWith :: (Functor m, Monad m) => (FilePath -> Text -> m Text)
                                  -> Block
                                  -> m Block
pipeBWith f (CodeBlock as s)
          |  Just (as', p) <- partPipes as = CodeBlock as' <$> f (unpack p) s
pipeBWith f x = walkM (pipeIWith f) x

pipeB = pipeBWith readShell

pipeIWith :: (Functor m, Monad m) => (FilePath -> Text -> m Text)
                                  -> Inline
                                  -> m Inline
pipeIWith f (Code as s)
          |  Just (as', p) <- partPipes as = Code as' <$> f (unpack p) s
pipeIWith f x = return x

pipeI = pipeIWith readShell

readShell :: FilePath -> Text -> IO Text
readShell path stdin = pack <$> readProcess "sh" ["-c", path] (unpack stdin)

partPipes :: Attr -> Maybe (Attr, Text)
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

inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = do
  oldDir <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory oldDir
  return result

panpipeMain :: IO ()
panpipeMain = toJSONFilter transform
