{-# Language ExistentialQuantification #-}
import           Control.Applicative
import           Data.List
import qualified Data.Map as DM
import           Data.Monoid
import           PanPipe
import           System.IO.Unsafe
import           System.Timeout
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Readers.Native
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk (walk, query)
import           Text.Pandoc.Writers.Native
import           Tests.Arbitrary

data T = forall a. Testable a => T a

instance Testable T where
  property (T x) = property x

tests = DM.fromList [

  ("pipeB passes pipe and stdin properly", T $ \p s a1 a2 ->
   let at x  = (undefined, undefined, a1 ++ x ++ a2)
       f a b = [a ++ b]
       [CodeBlock _ lhs] = pipeBWith f (CodeBlock (at [("pipe", p)]) s)
    in lhs == p ++ s),

  ("pipeB leaves attributes intact", T $ \i c a1 a2 ->
   let at x = (i, c, a1 ++ x ++ a2)
       [CodeBlock as _] = pipeBWith (\_ _ -> [undefined])
                                    (CodeBlock (at [("pipe", undefined)])
                                               undefined)
    in at [] == as),

  ("pipeI passes pipe and stdin properly", T $ \p s a1 a2 ->
   let at x  = (undefined, undefined, a1 ++ x ++ a2)
       f a b = [a ++ b]
       [Code _ lhs] = pipeIWith f (Code (at [("pipe", p)]) s)
    in lhs == p ++ s),

  ("pipeI leaves attributes intact", T $ \i c a1 a2 ->
   let at x = (i, c, a1 ++ x ++ a2)
       [Code as _] = pipeIWith (\_ _ -> [undefined])
                               (Code (at [("pipe", undefined)])
                                     undefined)
    in at [] == as),

  ("non-pipes ignored", T $ \as ->
   partPipes (undefined, undefined, filter ((/= "pipe") . fst) as) == Nothing),

  ("pipe class found", T $ \p as ->
   (snd <$> partPipes (undefined, undefined, ("pipe", p):as)) == Just p)]

testWith f = let go name test = (putStrLn ("Testing " ++ name) >> f test >>)
              in DM.foldWithKey go (return ()) tests

test = testWith quickCheck

main = test
