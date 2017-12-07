module Main where

data Zipper a = Zipper [a] a [a]

left :: Zipper a -> Maybe (Zipper a)
left (Zipper [] _ _) = Nothing
left (Zipper (x:xs) y rs) = Just $ Zipper xs x (y:rs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ _ []) = Nothing
right (Zipper ls y (x:xs)) = Just $ Zipper (y:ls) x xs

focus :: Zipper a -> a
focus (Zipper _ x _) = x

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper l x r) = Zipper l (f x) r

fromList :: [a] -> Zipper a
fromList [] = error "empty zipper"
fromList (x:xs) = Zipper [] x xs

move :: (a -> Maybe a) -> Int -> a -> Maybe a
move f 0 x = Just x
move f n x = f x >>= move f (n - 1)

part1 :: Zipper Int -> Int
part1 = go 1
  where go n program = let amt = focus program
                           (dir, dist) = if (amt < 0)
                                         then (left, (- amt))
                                         else (right, amt)
                           program' = modify (+ 1) program
                       in case (move dir dist program') of
                         Nothing -> n
                         (Just program'') -> go (n + 1) program''

main :: IO ()
main = interact $ show . part1 . fromList . map read . lines
