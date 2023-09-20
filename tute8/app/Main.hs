module Main (main) where

import BinTree (prettifyBinaryTree)

main :: IO ()
main = do
  putStrLn "Input a binary tree string:"
  btreeString <- getLine
  putStrLn $ prettifyBinaryTree btreeString
  main
