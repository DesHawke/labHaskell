import Data.Monoid

data Tree a
  = Leaf
  | Node Int
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)
 
foldTree
  :: Ord a
  => [a] -> Tree a
foldTree = foldr insert Leaf
 
height Leaf = -1
height (Node h _ _ _) = h
 
depth a b = 1 + (height a `max` height b)
 
insert
  :: Ord a
  => a -> Tree a -> Tree a
insert vertex Leaf = Node 1 Leaf vertex Leaf
insert vertex t@(Node n left vertex_ right)
  | vertex_ < vertex = rotate $ Node n left vertex_ (insert vertex right)
  | vertex_ > vertex = rotate $ Node n (insert vertex left) vertex_ right
  | otherwise = t
 
max_
  :: Ord a
  => Tree a -> Maybe a
max_ Leaf = Nothing
max_ (Node _ _ vertex right) =
  case right of
    Leaf -> Just vertex
    _ -> max_ right
 
--delete
--  :: Ord a
--  => a -> Tree a -> Tree a
--delete _ Leaf = Leaf
--delete x (Node h left vertex right)
--  | x == vertex =
--   maybe left (\m -> rotate $ Node h left m (delete m right)) (max_ right)
--  | x > vertex = rotate $ Node h left vertex (delete x right)
--  | x < vertex = rotate $ Node h (delete x left) vertex right
 
rotate :: Tree a -> Tree a
rotate Leaf = Leaf
-- left left case
rotate (Node h (Node left_h left_left left_vertex left_right) vertex right)
  | left_h - height right > 1 && height left_left - height left_right > 0 =
    Node left_h left_left left_vertex (Node (depth right left_right) left_right vertex right)
-- right right case
rotate (Node h left vertex (Node right_h right_left right_vertex right_right))
  | right_h - height left > 1 && height right_right - height right_left > 0 =
    Node right_h (Node (depth left right_left) left vertex right_left) right_vertex right_right
-- left right case
rotate (Node h (Node left_h left_left left_vertex (Node right_h right_left right_vertex right_right)) vertex right)
  | left_h - height right > 1 =
    Node h (Node (right_h + 1) (Node (left_h - 1) left_left left_vertex right_left) right_vertex right_right) vertex right
-- right left case
rotate (Node h left vertex (Node right_h (Node left_h left_left left_vertex left_right) right_vertex right_right))
  | right_h - height left > 1 =
    Node h left vertex (Node (left_h + 1) left_left left_vertex (Node (right_h - 1) left_right right_vertex right_right))
-- balancing
rotate (Node h left vertex right) =
  let (left_, right_) = (rotate left, rotate right)
  in Node (depth left_ right_) left_ vertex right_
 
draw
  :: Show a
  => Tree a -> String
draw t = '\n' : draw_ t 0 <> "\n"
  where
    draw_ Leaf _ = []
    draw_ (Node h left vertex right) d = draw_ right (d + 1) <> node <> draw_ left (d + 1)
      where
        node = padding d <> show (vertex, h) <> "\n"
        padding n = replicate (n * 4) ' '

main :: IO ()
main = do
  let tree = foldTree [1, 5, 2, 10, 12, 8, 15]
  putStr $ draw tree
  putStrLn "/////////////////////"
  let ins = insert 4 tree
  putStr $ draw ins
