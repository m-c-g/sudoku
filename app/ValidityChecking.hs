
module ValidityChecking where
import BasicDeclarations (Grid)
import RowsColsBoxes (rows, cols, boxs)

-- Validity checking
--------------------

-- Now let us turn our attention from matrices to Sudoku grids.  
-- is valid if there are no duplicates in any row, column or box:

valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                         all nodups (cols g) &&
                         all nodups (boxs g)
nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs

