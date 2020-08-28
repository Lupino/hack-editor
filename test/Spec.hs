import           HackEditor

main :: IO ()
main = do
  trees <- getFileTreeList "."
  print $ encodeTreeList trees
