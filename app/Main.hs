module Main where
import Data.List
-- import BasicDeclarations
-- import BasicDefinitions
import ExampleGrids
-- import RowsColsBoxes
-- import ValidityChecking
import Solve0

main :: IO ()
main = putStrLn (unlines (head (solve4 diabolical)))
