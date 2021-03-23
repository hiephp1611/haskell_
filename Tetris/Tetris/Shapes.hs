-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing, isJust)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
creatshape :: (Int, Int) -> Square -> Shape
creatshape (row,col) square = S (replicate col (replicate row square))

emptyShape :: (Int,Int) -> Shape
emptyShape (row,col) = creatshape (row, col) Nothing

testShape :: Shape
testShape = emptyShape(3,2)
-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S (row1:rest)) = (row,col)
  where
    col = length rest+1
    row = length row1


-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S shape) = length (filter (/= Nothing) (concat (shape)) ) 

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (S (row1:rest)) = (row /=0) && (col /=0) && rectangle
    where
      (row,col) = shapeSize (S (row1:rest))
      rectangle = and [(length row') == row | row' <- rest] 


-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black , Red , Green , Yellow , Blue , Purple , Cyan , Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes


instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (S s) = S (reverse (transpose s))


-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (left,upp) s = shiftDown (shiftLeft s)
  where 
    emptyL s    = emptyShape (snd (shapeSize s),left)
    shiftLeft s = S (transpose (rows (emptyL s) ++ transpose (rows s)))

    emptyU s    = emptyShape (fst (shapeSize s),upp)
    shiftDown s = S (rows (emptyU s)  ++ rows s)

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (right, down) s = padDown (padRight s)
  where
    emptyR s   = emptyShape (snd (shapeSize s),right)
    padRight s = S (transpose (transpose (rows s) ++ rows (emptyR s)))

    emptyD s   = emptyShape (fst (shapeSize s),down)
    padDown s  = S (rows s ++ rows (emptyD s))

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (lrow,lcol) s =  padShape (right, down) s
  where
    (row,col)          = shapeSize s
    right | lrow > row = lrow - row
          | otherwise  = 0
    down  | lcol > col = lcol - col
          | otherwise  = 0

-- * Comparing and combining shapes

-- ** B01

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
overlaps (S []) _                            = False
overlaps _ (S [])                            = False
overlaps s1 s2  | rowsOverlap h1 h2 == True  = True
                | otherwise                  = overlaps (S t1) (S t2)
  where 
    h1:t1 = rows s1
    h2:t2 = rows s2

rowsOverlap :: Row -> Row -> Bool
rowsOverlap [] _                                           = False
rowsOverlap _ []                                           = False
rowsOverlap (x:xs) (y:ys) | (isNothing x) || (isNothing y) = rowsOverlap xs ys
                          | otherwise                      = True

-- ** B02
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S []) _       = S []
zipShapeWith f  _ (S [])      = S []
zipShapeWith f  s1 s2         = S (zipWith f h1 h2 : rows (zipShapeWith f (S t1) (S t2)))
    where 
      h1:t1 = rows s1
      h2:t2 = rows s2

-- ** B03
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = 
  if not (s1 `overlaps` s2)
    then zipShapeWith (\x1 x2 -> if (isJust x1) then x1 else x2) s1' s2'
    else error "Overlapping shapes"
  where 
    (lrow1,lcol1)   = shapeSize s1
    (lrow2,lcol2)   = shapeSize s2
    newSize         = (max lrow1 lrow2, max lcol1 lcol2)
    s1'             = padShapeTo newSize s1
    s2'             = padShapeTo newSize s2