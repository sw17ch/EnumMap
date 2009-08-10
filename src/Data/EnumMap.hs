{-# LANGUAGE ExistentialQuantification #-}

module Data.EnumMap (
    EnumMap,
    Key(..),
) where

import qualified Data.IntMap as M
import Data.IntSet

newtype EnumMap k v = EnumMap { uEM :: M.IntMap v }

data Key k = (Enum k) => Key { uK :: k }

(!) :: Enum k => EnumMap k a -> Key k -> a
m ! k = (M.!) (uEM m) (fromEnum . uK $ k)

(\\) :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
a \\ b = EnumMap $ (M.\\) (uEM a) (uEM b)

null :: Enum k => EnumMap k a -> Bool
null = M.null . uEM

size :: Enum k => EnumMap k a -> Int
size = M.size . uEM

member :: Enum k => Key k -> EnumMap k a -> Bool
member k m = M.member (fromEnum . uK $ k) (uEM m)

notMember :: Enum k => Key k -> EnumMap k a -> Bool
notMember k m = M.notMember (fromEnum . uK $ k) (uEM m)

lookup :: Enum k => Key k -> EnumMap k a -> Maybe a
lookup k m = M.lookup (fromEnum . uK $ k) (uEM m)

findWithDefault :: Enum k => a -> Key k -> EnumMap k a -> a
findWithDefault d k m = M.findWithDefault d (fromEnum . uK $ k) (uEM m)

empty :: Enum k => EnumMap k a
empty = EnumMap M.empty

singleton :: Enum k => Key k -> a -> EnumMap k a
singleton k v = EnumMap $ M.singleton (fromEnum . uK $ k) v

insert :: Key k -> a -> EnumMap k a -> EnumMap k a
insert = undefined

insertWith :: (a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertWith = undefined

insertWithKey :: (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertWithKey = undefined

insertLookupWithKey :: (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey = undefined

delete :: Key k -> EnumMap k a -> EnumMap k a
delete = undefined

adjust :: (a -> a) -> Key k -> EnumMap k a -> EnumMap k a
adjust  = undefined

adjustWithKey :: (Key k -> a -> a) -> Key k -> EnumMap k a -> EnumMap k a
adjustWithKey  = undefined
update :: (a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
update  = undefined
updateWithKey :: (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
updateWithKey  = undefined
updateLookupWithKey :: (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> (Maybe a, EnumMap k a)
updateLookupWithKey  = undefined
alter :: (Maybe a -> Maybe a) -> Int -> EnumMap k a -> EnumMap k a
alter  = undefined
union :: EnumMap k a -> EnumMap k a -> EnumMap k a
union  = undefined
unionWith :: (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith  = undefined
unionWithKey :: (Key k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey  = undefined
unions :: [EnumMap k a] -> EnumMap k a
unions  = undefined
unionsWith :: (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
unionsWith  = undefined
difference :: EnumMap k a -> EnumMap k b -> EnumMap k a
difference  = undefined
differenceWith :: (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith  = undefined
differenceWithKey :: (Key k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey  = undefined
intersection :: EnumMap k a -> EnumMap k b -> EnumMap k a
intersection  = undefined
intersectionWith :: (a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWith  = undefined
intersectionWithKey :: (Key k -> a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWithKey  = undefined
map :: (a -> b) -> EnumMap k a -> EnumMap k b
map  = undefined
mapWithKey :: (Key k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey  = undefined
mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccum  = undefined
mapAccumWithKey :: (a -> Key k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey  = undefined
fold :: (a -> b -> b) -> b -> EnumMap k a -> b
fold  = undefined
foldWithKey :: (Key k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey  = undefined
elems :: EnumMap k a -> [a]
elems  = undefined
keys :: EnumMap k a -> [Key k]
keys  = undefined
keysSet :: EnumMap k a -> IntSet
keysSet  = undefined
assocs :: EnumMap k a -> [(Key k, a)]
assocs  = undefined
toList :: EnumMap k a -> [(Key k, a)]
toList  = undefined
fromList :: [(Key k, a)] -> EnumMap k a
fromList  = undefined
fromListWith :: (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromListWith  = undefined
fromListWithKey :: (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromListWithKey  = undefined
toAscList :: EnumMap k a -> [(Key k, a)]
toAscList  = undefined
fromAscList :: [(Key k, a)] -> EnumMap k a
fromAscList  = undefined
fromAscListWith :: (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromAscListWith  = undefined
fromAscListWithKey :: (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromAscListWithKey  = undefined
fromDistinctAscList :: [(Key k, a)] -> EnumMap k a
fromDistinctAscList  = undefined
filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filter  = undefined
filterWithKey :: (Key k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey  = undefined
partition :: (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partition  = undefined
partitionWithKey :: (Key k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey  = undefined
mapMaybe :: (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe  = undefined
mapMaybeWithKey :: (Key k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey  = undefined
mapEither :: (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither  = undefined
mapEitherWithKey :: (Key k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey  = undefined
split :: Key k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split  = undefined
splitLookup :: Key k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup  = undefined
isSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isSubmapOf  = undefined
isSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isSubmapOfBy  = undefined
isProperSubmapOf :: Eq a => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOf  = undefined
isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOfBy  = undefined
maxView :: EnumMap k a -> Maybe (a, EnumMap k a)
maxView  = undefined
minView :: EnumMap k a -> Maybe (a, EnumMap k a)
minView  = undefined
findMin :: EnumMap k a -> a
findMin  = undefined
findMax :: EnumMap k a -> a
findMax  = undefined
deleteMin :: EnumMap k a -> EnumMap k a
deleteMin  = undefined
deleteMax :: EnumMap k a -> EnumMap k a
deleteMax  = undefined
deleteFindMin :: EnumMap k a -> (a, EnumMap k a)
deleteFindMin  = undefined
deleteFindMax :: EnumMap k a -> (a, EnumMap k a)
deleteFindMax  = undefined
updateMin :: (a -> a) -> EnumMap k a -> EnumMap k a
updateMin  = undefined
updateMax :: (a -> a) -> EnumMap k a -> EnumMap k a
updateMax  = undefined
updateMinWithKey :: (Key k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMinWithKey  = undefined
updateMaxWithKey :: (Key k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey  = undefined
minViewWithKey :: EnumMap k a -> Maybe ((Key k, a), EnumMap k a)
minViewWithKey  = undefined
maxViewWithKey :: EnumMap k a -> Maybe ((Key k, a), EnumMap k a)
maxViewWithKey  = undefined
showTree :: Show a => EnumMap k a -> String
showTree  = undefined
showTreeWith :: Show a => Bool -> Bool -> EnumMap k a -> String
showTreeWith  = undefined
