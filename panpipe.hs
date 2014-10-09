module Main where
import PanPipe
import Text.Pandoc.JSON

main = toJSONFilter transform
