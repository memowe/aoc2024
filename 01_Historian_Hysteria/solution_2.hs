main :: IO ()
main = interact $ unlines . map ("Hello, " ++) . lines
