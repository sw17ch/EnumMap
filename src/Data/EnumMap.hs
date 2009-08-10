{-# LANGUAGE ExistentialQuantification #-}

module Data.EnumMap (
    EnumMap,
    Key(..),
) where

import qualified Data.IntMap as M

newtype EnumMap k v = EnumMap { uEM :: M.IntMap v }

data Key k = (Enum k) => Key { uK :: k }

(!) :: EnumMap k a -> Key k -> a
(\\) :: EnumMap k a -> EnumMap k b -> EnumMap k a
null :: EnumMap k a -> Bool
size :: EnumMap k a -> Int
member :: Key k -> EnumMap k a -> Bool
notMember :: Key k -> EnumMap k a -> Bool
lookup :: Key k -> EnumMap k a -> Maybe a
findWithDefault :: a -> Key k -> EnumMap k a -> a
empty :: EnumMap k a
singleton :: Key k -> a -> EnumMap k a
insert :: Key k -> a -> EnumMap k a -> EnumMap k a
insertWith :: (a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertWithKey k :: (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertLookupWithKey k :: (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
delete :: Key k -> EnumMap k a -> EnumMap k a
adjust :: (a -> a) -> Key k -> EnumMap k a -> EnumMap k a
adjustWithKey k :: (Key k -> a -> a) -> Key k -> EnumMap k a -> EnumMap k a
update :: (a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
updateWithKey k :: (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
updateLookupWithKey k :: (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> (Maybe a, EnumMap k a)
alter :: (Maybe a -> Maybe a) -> Int -> EnumMap k a -> EnumMap k a
union :: EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith :: (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey :: (Key k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unions :: [EnumMap k a] -> EnumMap k a
unionsWith :: (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
difference :: EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey :: (Key k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWith :: (a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWithKey :: (Key k -> a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
map :: (a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey :: (Key k -> a -> b) -> EnumMap k a -> EnumMap k b
mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey :: (a -> Key k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
fold :: (a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey :: (Key k -> a -> b -> b) -> b -> EnumMap k a -> b
elems :: EnumMap k a -> [a]
keys :: EnumMap k a -> [Key k]
keysSet :: EnumMap k a -> IntSet
assocs :: EnumMap k a -> [(Key k, a)]
toList :: EnumMap k a -> [(Key k, a)]
fromList :: [(Key k, a)] -> EnumMap k a
fromListWith :: (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromListWithKey :: (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
toAscList :: EnumMap k a -> [(Key k, a)]
fromAscList :: [(Key k, a)] -> EnumMap k a
fromAscListWith :: (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromAscListWithKey :: (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromDistinctAscList :: [(Key k, a)] -> EnumMap k a
filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey :: (Key k -> a -> Bool) -> EnumMap k a -> EnumMap k a
partition :: (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey :: (Key k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
mapMaybe :: (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey :: (Key k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapEither :: (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey :: (Key k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
split :: Key k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
splitLookup :: Key k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
isSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
maxView :: EnumMap k a -> Maybe (a, EnumMap k a)
minView :: EnumMap k a -> Maybe (a, EnumMap k a)
findMin :: EnumMap k a -> a
findMax :: EnumMap k a -> a
deleteMin :: EnumMap k a -> EnumMap k a
deleteMax :: EnumMap k a -> EnumMap k a
deleteFindMin :: EnumMap k a -> (a, EnumMap k a)
deleteFindMax :: EnumMap k a -> (a, EnumMap k a)
updateMin :: (a -> a) -> EnumMap k a -> EnumMap k a
updateMax :: (a -> a) -> EnumMap k a -> EnumMap k a
updateMinWithKey :: (Key k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey :: (Key k -> a -> a) -> EnumMap k a -> EnumMap k a
minViewWithKey :: EnumMap k a -> Maybe ((Key k, a), EnumMap k a)
maxViewWithKey :: EnumMap k a -> Maybe ((Key k, a), EnumMap k a)
showTree :: Show a => EnumMap k a -> String
showTreeWith :: Show a => Bool -> Bool -> EnumMap k a -> String
