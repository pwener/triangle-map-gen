{-

SOME SAMPLES

To insert use:
> insert (Node NorthW Null (Node East Null Null)) NorthE
Node NorthW (Node NorthE Null Null) (Node East Null Null)

To count use:
> count (Node NorthW)

To create new tree
> ctree [East, West, NorthE]

-}

import System.Random

-- Triangles vertex are formed only to this directions
data Orientation = East | West | NorthE | NorthW | SouthE | SouthW | None deriving (Ord, Show, Eq, Enum)

-- Attribute one code to each Orientation
equivalent :: Int -> Orientation
equivalent code
  | code == 1 = East
  | code == 2 = West
  | code == 3 = NorthE
  | code == 4 = NorthW
  | code == 5 = SouthE
  | code == 6 = SouthW
  | otherwise = None


-- Get a random number of elements
getRandomOrientation :: StdGen -> Orientation
getRandomOrientation gen = equivalent(randomNum) where (randomNum, novoGen) = randomR(1,6) gen :: (Int, StdGen)


-- Sample Node: (Node East (Node West Null Null) (Node NorthW Null Null))
data OrientationTree tree = Null | Node tree (OrientationTree tree) (OrientationTree tree) deriving (Show)

-- Count the number of leafs
count :: (Ord tree) => OrientationTree tree -> Int
count Null = 0
count (Node x left right) = 1 + count(left) + count(right)

-- See if tree is empty
empty :: (Ord tree) => OrientationTree tree -> Bool
empty Null = True
empty  _  = False

-- Insert a new Node
insert :: (Ord tree) => OrientationTree tree -> tree -> OrientationTree tree
insert Null t = (Node t Null Null)
insert (Node root left right) t
   | root == t = Node root left right
   | root  < t = Node root left (insert right t)
   | root  > t = Node root (insert left t) right

-- Create a new tree passing one list of Orientation
ctree :: (Ord tree) => [tree] -> OrientationTree tree
ctree [] = Null
ctree (h:t) = ctree2 (Node h Null Null) t
   where
   ctree2 tr [] = tr
   ctree2 tr (h:t) = ctree2 (insert tr h) t


