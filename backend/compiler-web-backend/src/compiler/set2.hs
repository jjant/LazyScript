import Prelude hiding (Maybe(..))

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a kvs =
  case headMay (filter (\(k, _) -> k == a) kvs) of
    Nothing -> Nothing
    Just (_,v) -> Just v

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a / b

-- maximumMay :: Ord a => [a] -> Maybe a
-- minimumMay :: Ord a => [a] -> Maybe a

type GreekData = [(String, [Integer])]

greekDataA :: GreekData
greekDataA = [("A", [2,3,4])]

greekDataB :: GreekData
greekDataA = [("B", [6,7])]
