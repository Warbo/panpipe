{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Maybe
import qualified Data.Text as T
import           PanPipe
import           Text.Pandoc
import           Test.QuickCheck (Arbitrary, arbitrary, shrink)
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

u = undefined

(+++) = T.append

pipeBPass p s a1 a2 = let at x  = (u, u, a1 ++ x ++ a2)
                          f a b = [T.pack a +++ b]
                          [CodeBlock _ lhs] = pipeBWith f
                                                (CodeBlock (at [("pipe", p)]) s)
                      in  lhs == p +++ s

pipeBAttr i c a1 a2 = let at x = (i, c, a1 ++ x ++ a2)
                          [CodeBlock as _] = pipeBWith (\_ _ -> [u])
                                               (CodeBlock (at [("pipe", u)])
                                                                  u)
                      in  at [] == as

pipeIPass p s a1 a2 = let at x  = (u, u, a1 ++ x ++ a2)
                          f a b = [T.pack a +++ b]
                          [Code _ lhs] = pipeIWith f (Code (at [("pipe", p)]) s)
                      in  lhs == p +++ s

pipeIAttr i c a1 a2 = let at x = (i, c, a1 ++ x ++ a2)
                          [Code as _] = pipeIWith (\_ _ -> [u])
                                                  (Code (at [("pipe", u)])
                                                        u)
                      in at [] == as

nonPipe as = isNothing $ partPipes (u, u, filter ((/= "pipe") . fst) as)

pipeClass p as = (snd <$> partPipes (u, u, ("pipe", p):as)) == Just p

main = defaultMain $ testGroup "All tests"
       [
         testProperty "pipeB passes pipe and stdin properly" pipeBPass
       , testProperty "pipeB leaves attributes intact"       pipeBAttr
       , testProperty "pipeI passes pipe and stdin properly" pipeIPass
       , testProperty "pipeI leaves attributes intact"       pipeIAttr
       , testProperty "non-pipes ignored"                    nonPipe
       , testProperty "pipe class found"                     pipeClass
       ]

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink x  = map T.pack (shrink (T.unpack x))
