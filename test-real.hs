import Test.QuickCheck
import PanPipe

--main :: IO ()
--main = quickCheck 

--pipeFound :: String -> String -> [String] -> [(String, String)]
pipeFound p x ys zs = snd (partPipes (x, ys, ("pipe", p):zs)) == Just p
