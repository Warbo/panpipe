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

  ("pipeB passes pipe and stdin properly", T $ \p s i c a1 a2 s1 s2 s3 ->
   let at x  = (i, c, a1 ++ x ++ a2)
       f a b = [s1 ++ a ++ s2 ++ b ++ s3]
       [lhs] = pipeBWith f (CodeBlock (at [("pipe", p)]) s)
       [rhs] = CodeBlock (at []) <$> (f p s)
    in lhs == rhs),

  ("pipeI passes pipe and stdin properly", T $ \p s i c a1 a2 s1 s2 s3 ->
   let at x  = (i, c, a1 ++ x ++ a2)
       f a b = [s1 ++ a ++ s2 ++ b ++ s3]
       [lhs] = pipeIWith f (Code (at [("pipe", p)]) s)
       [rhs] = Code (at []) <$> (f p s)
    in lhs == rhs),

  ("non-pipes ignored", T $ \x ys zs ->
   partPipes (x, ys, filter ((/= "pipe") . fst) zs) == Nothing),

  ("pipe class found", T $ \p x ys zs ->
   (snd <$> partPipes (x, ys, ("pipe", p):zs)) == Just p)]

testWith f = let go name test = (putStrLn ("Testing " ++ name) >> f test >>)
              in DM.foldWithKey go (return ()) tests

test = testWith quickCheck

main = test
