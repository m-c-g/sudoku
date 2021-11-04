module BasicDefinitions where
import BasicDeclarations (Value)

boxsize               :: Int
boxsize               =  3
values                :: [Value]
values                =  ['1'..'9']
empty                 :: Value -> Bool
empty                 =  (== '.')
single                :: [a] -> Bool
single [_]            =  True
single _              =  False
