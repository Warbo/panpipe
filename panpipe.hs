module Main where
import PanPipe
main = getContents >>= processDoc >>= putStr
