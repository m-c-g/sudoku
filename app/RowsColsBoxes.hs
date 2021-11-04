-- Extracting rows, columns and boxes
-------------------------------------
module RowsColsBoxes where
import BasicDeclarations (Matrix, Row)
import BasicDefinitions (boxsize)
import Data.List

-- Extracting rows is trivial:

rows                  :: Matrix a -> [Row a]
rows                  =  id

-- We also have, trivially, that rows . rows = id.  This property (and
-- similarly for cols and boxs) will be important later on.
----------------------------------------------------------------------

-- Extracting columns is just matrix transposition:

cols                  :: Matrix a -> [Row a]
cols                  =  transpose

-- Example: cols [[1,2],[3,4]] = [[1,3],[2,4]].  Exercise: define
-- transpose, without looking at the library definition.
-----------------------------------------------------------------

-- We also have that cols . cols = id.

-- Extracting boxes is more complicated:

boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split  = chop boxsize
                            unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

{- Example: if boxsize = 2, then we have 

   [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

                          |
                         pack
                          |
                          v

   [[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]

                          |
                       map cols
                          |
                          v

   [[[[1,2],[5,6]],[[3,4],[7,8]]],[[[9,10],[13,14]],[[11,12],[15,16]]]]

                          | 
                        unpack
                          |
                          v

   [[1,2,5,6],[3,4,7,8],[9,10,13,14],[11,12,15,16]]

Note that concat . split = id, and moreover, boxs . boxs = id.
-}
