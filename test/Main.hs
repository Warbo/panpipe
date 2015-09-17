import           Control.Applicative
import           Data.Maybe
import           PanPipe
import           Text.Pandoc
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

pipeBPass p s a1 a2 = let at x  = (undefined, undefined, a1 ++ x ++ a2)
                          f a b = [a ++ b]
                          [CodeBlock _ lhs] = pipeBWith f (CodeBlock (at [("pipe", p)]) s)
                      in  lhs == p ++ s

pipeBAttr i c a1 a2 = let at x = (i, c, a1 ++ x ++ a2)
                          [CodeBlock as _] = pipeBWith (\_ _ -> [undefined])
                                                       (CodeBlock (at [("pipe", undefined)])
                                                                  undefined)
                      in  at [] == as

pipeIPass p s a1 a2 = let at x  = (undefined, undefined, a1 ++ x ++ a2)
                          f a b = [a ++ b]
                          [Code _ lhs] = pipeIWith f (Code (at [("pipe", p)]) s)
                      in  lhs == p ++ s

pipeIAttr i c a1 a2 = let at x = (i, c, a1 ++ x ++ a2)
                          [Code as _] = pipeIWith (\_ _ -> [undefined])
                                                  (Code (at [("pipe", undefined)])
                                                        undefined)
                      in at [] == as

nonPipe as = isNothing $ partPipes (undefined, undefined, filter ((/= "pipe") . fst) as)

pipeClass p as = (snd <$> partPipes (undefined, undefined, ("pipe", p):as)) == Just p

main = defaultMain $ testGroup "All tests"
       [
         testProperty "pipeB passes pipe and stdin properly" pipeBPass
       , testProperty "pipeB leaves attributes intact"       pipeBAttr
       , testProperty "pipeI passes pipe and stdin properly" pipeIPass
       , testProperty "pipeI leaves attributes intact"       pipeIAttr
       , testProperty "non-pipes ignored"                    nonPipe
       , testProperty "pipe class found"                     pipeClass
       ]
