{-# LANGUAGE ExistentialQuantification #-}

module Data.EnumMap (
    EnumMap,
    Key(..),
) where

import qualified Data.IntMap as M
import Prelude
import qualified Prelude
import Data.IntSet

newtype EnumMap k v = EnumMap { uEM :: M.IntMap v }

data Key k = (Enum k) => Key { uK :: k }

u :: Enum k => Key k -> Int
u = fromEnum . uK

t :: Enum k => M.Key -> Key k
t = Key . toEnum

(!) :: Enum k => EnumMap k a -> Key k -> a
m ! k = (M.!) (uEM m) (u k)

(\\) :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
a \\ b = EnumMap $ (M.\\) (uEM a) (uEM b)

null :: Enum k => EnumMap k a -> Bool
null = M.null . uEM

size :: Enum k => EnumMap k a -> Int
size = M.size . uEM

member :: Enum k => Key k -> EnumMap k a -> Bool
member k m = M.member (u k) (uEM m)

notMember :: Enum k => Key k -> EnumMap k a -> Bool
notMember k m = M.notMember (u k) (uEM m)

lookup :: Enum k => Key k -> EnumMap k a -> Maybe a
lookup k m = M.lookup (u k) (uEM m)

findWithDefault :: Enum k => a -> Key k -> EnumMap k a -> a
findWithDefault d k m = M.findWithDefault d (u k) (uEM m)

empty :: Enum k => EnumMap k a
empty = EnumMap M.empty

singleton :: Enum k => Key k -> a -> EnumMap k a
singleton k v = EnumMap $ M.singleton (u k) v

insert :: Enum k => Key k -> a -> EnumMap k a -> EnumMap k a
insert k v m = EnumMap $ M.insert (u k) v (uEM m)

insertWith :: Enum k => (a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertWith f k v m = EnumMap $ M.insertWith f (u k) v (uEM m)

insertWithKey :: Enum k => (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k v m = EnumMap $ M.insertWithKey (f . t) (u k) v (uEM m)

insertLookupWithKey :: Enum k => (Key k -> a -> a -> a) -> Key k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k i m = let (a,m') = M.insertLookupWithKey (f . t) (u k) i (uEM m)
                              in  (a, EnumMap m')

delete :: Enum k => Key k -> EnumMap k a -> EnumMap k a
delete k m = EnumMap $ M.delete (u k) (uEM m)

adjust :: Enum k => (a -> a) -> Key k -> EnumMap k a -> EnumMap k a
adjust f k m = EnumMap $ M.adjust f (u k) (uEM m)

adjustWithKey :: Enum k => (Key k -> a -> a) -> Key k -> EnumMap k a -> EnumMap k a
adjustWithKey f k m = EnumMap $ M.adjustWithKey (f . t) (u k) (uEM m)

update :: Enum k => (a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
update f k m = EnumMap $ M.update f (u k) (uEM m)

updateWithKey :: Enum k => (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
updateWithKey f k m = EnumMap $ M.updateWithKey (f . t) (u k) (uEM m)

updateLookupWithKey :: Enum k => (Key k -> a -> Maybe a) -> Key k -> EnumMap k a -> (Maybe a, EnumMap k a)
updateLookupWithKey f k m = let (a,b) = M.updateLookupWithKey (f . t) (u k) (uEM m)
                            in  (a,EnumMap b)

alter :: Enum k => (Maybe a -> Maybe a) -> Key k -> EnumMap k a -> EnumMap k a
alter f k m = EnumMap $ M.alter f (u k) (uEM m)

union :: Enum k => EnumMap k a -> EnumMap k a -> EnumMap k a
union m1 m2 = EnumMap $ M.union (uEM m1) (uEM m2)

unionWith :: Enum k => (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f m1 m2 = EnumMap $ M.unionWith f (uEM m1) (uEM m2)

unionWithKey :: Enum k => (Key k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f m1 m2 = EnumMap $ M.unionWithKey (f . t) (uEM m1) (uEM m2)

unions :: Enum k => [EnumMap k a] -> EnumMap k a
unions ms = EnumMap $ M.unions $ Prelude.map uEM ms

unionsWith :: Enum k => (a -> a -> a) -> [EnumMap k a] -> EnumMap k a
unionsWith f ms = EnumMap $ M.unionsWith f $ Prelude.map uEM ms

difference :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
difference m1 m2 = EnumMap $ M.difference (uEM m1) (uEM m2)

differenceWith :: Enum k => (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f m1 m2 = EnumMap $ M.differenceWith f (uEM m1) (uEM m2)

differenceWithKey :: Enum k => (Key k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f m1 m2 = EnumMap $ M.differenceWithKey (f . t) (uEM m1) (uEM m2)

intersection :: Enum k => EnumMap k a -> EnumMap k b -> EnumMap k a
intersection m1 m2 = EnumMap $ M.intersection (uEM m1) (uEM m2)

intersectionWith :: Enum k => (a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWith f m1 m2 = EnumMap $ M.intersectionWith f (uEM m1) (uEM m2)

intersectionWithKey :: Enum k => (Key k -> a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWithKey f m1 m2 = EnumMap $ M.intersectionWithKey (f . t) (uEM m1) (uEM m2)

map :: Enum k => (a -> b) -> EnumMap k a -> EnumMap k b
map f m = EnumMap $ M.map f (uEM m)

mapWithKey :: Enum k => (Key k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f m = EnumMap $ M.mapWithKey (f . t) (uEM m)

mapAccum :: Enum k => (a -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccum f i m = let (a, m') = M.mapAccum f i (uEM m) in  (a, EnumMap m')

mapAccumWithKey :: Enum k => (a -> Key k -> b -> (a, c)) -> a -> EnumMap k b -> (a, EnumMap k c)
mapAccumWithKey f i m = let f' n k = f n (Key . toEnum $ k)
                            (a, m') = M.mapAccumWithKey f' i (uEM m)
                        in  (a, EnumMap m')

fold :: Enum k => (a -> b -> b) -> b -> EnumMap k a -> b
fold f i m = M.fold f i (uEM m)

foldWithKey :: Enum k => (Key k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey f i m = M.foldWithKey (f . t) i (uEM m)

elems :: Enum k => EnumMap k a -> [a]
elems m = M.elems (uEM m)

keys :: Enum k => EnumMap k a -> [Key k]
keys = Prelude.map t . M.keys . uEM

-- TODO: When we have an EnumSet, we'll need to revisit this.
keysSet :: Enum k => EnumMap k a -> IntSet
keysSet = M.keysSet . uEM

assocs :: Enum k => EnumMap k a -> [(Key k, a)]
assocs = let f (k,a) = (Key . toEnum $ k, a)
         in Prelude.map f . M.assocs . uEM

toList :: Enum k => EnumMap k a -> [(Key k, a)]
toList = let f (k,a) = (Key . toEnum $ k, a)
         in Prelude.map f . M.toList . uEM

fromList :: Enum k => [(Key k, a)] -> EnumMap k a
fromList = let f (k,a) = (u k, a)
           in EnumMap . M.fromList . Prelude.map f

fromListWith :: Enum k => (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromListWith f as = let g (k,a) = (u k, a)
                    in EnumMap $ M.fromListWith f $ Prelude.map g as

fromListWithKey :: Enum k => (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromListWithKey f as = let g (k,a) = (u k, a)
                       in EnumMap $ M.fromListWithKey (f . t) (Prelude.map g as)

toAscList :: Enum k => EnumMap k a -> [(Key k, a)]
toAscList m = let f (k,v) = (Key . toEnum $ k, v)
              in Prelude.map f $ M.toAscList (uEM m)

fromAscList :: Enum k => [(Key k, a)] -> EnumMap k a
fromAscList as = let f (k,v) = (u k,v)
                 in EnumMap $ M.fromAscList $ Prelude.map f as

fromAscListWith :: Enum k => (a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromAscListWith f as = let g (k,v) = (u k, v)
                       in EnumMap $ M.fromAscListWith f (Prelude.map g as)

fromAscListWithKey :: Enum k => (Key k -> a -> a -> a) -> [(Key k, a)] -> EnumMap k a
fromAscListWithKey f as = let g (k,v) = (u k, v)
                          in EnumMap $ M.fromAscListWithKey (f . t) $ Prelude.map g as

fromDistinctAscList :: Enum k => [(Key k, a)] -> EnumMap k a
fromDistinctAscList as = let f (k,v) = (u k, v)
                         in EnumMap $ M.fromDistinctAscList $ Prelude.map f as

filter :: (a -> Bool) -> EnumMap k a -> EnumMap k a
filter f m = EnumMap $ M.filter f (uEM m)

filterWithKey :: Enum k => (Key k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey f m = EnumMap $ M.filterWithKey (f . t) (uEM m)

partition :: Enum k => (a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partition f m = let (m1,m2) = M.partition f (uEM m) in (EnumMap m1, EnumMap m2)

partitionWithKey :: Enum k => (Key k -> a -> Bool) -> EnumMap k a -> (EnumMap k a, EnumMap k a)
partitionWithKey f m = let (m1,m2) = M.partitionWithKey (f . t) (uEM m)
                       in (EnumMap m1, EnumMap m2)

mapMaybe :: Enum k => (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe  = undefined
mapMaybeWithKey :: Enum k => (Key k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey  = undefined
mapEither :: Enum k => (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither  = undefined
mapEitherWithKey :: Enum k => (Key k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey  = undefined
split :: Enum k => Key k -> EnumMap k a -> (EnumMap k a, EnumMap k a)
split  = undefined
splitLookup :: Enum k => Key k -> EnumMap k a -> (EnumMap k a, Maybe a, EnumMap k a)
splitLookup  = undefined
isSubmapOf :: (Eq a, Enum k) => EnumMap k a -> EnumMap k a -> Bool
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
