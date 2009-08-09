{-# LANGUAGE ExistentialQuantification #-}

module Data.EnumMap (
    EnumMap,
    Key(..),
) where

import qualified Data.IntMap as M

newtype EnumMap k v = EnumMap { uEM :: M.IntMap v }

data Key k = (Enum k) => Key { uK :: k }

(!) :: EnumMap a b -> Key a -> b
(\\) :: EnumMap a b -> EnumMap a c -> EnumMap a b
null :: EnumMap a b -> Bool
size :: EnumMap a b -> Int
member :: Key a -> EnumMap a b -> Bool
notMember :: Key a -> EnumMap a b -> Bool
lookup :: Key a -> EnumMap a b -> Maybe b
findWithDefault :: b -> Key a -> EnumMap a b -> b
empty :: EnumMap a b
singleton :: Key a -> b -> EnumMap a b
insert :: Key a -> b -> EnumMap a b -> EnumMap a b
insertWith :: (b -> b -> b) -> Key a -> b -> EnumMap a b -> EnumMap a b
insertWithKey :: (Key a -> b -> b -> b) -> Key a -> EnumMap a b -> EnumMap a b
insertLookupWithKey :: (Key a -> b -> b -> ) -> Key a -> EnumMap a b -> (Maybe b, EnumMap a b)
delete :: Key a -> EnumMap a b -> EnumMap a b
adjust :: (b -> b) -> Key a -> EnumMap a b -> EnumMap a b
adjustWithKey :: (Key a -> b -> b) -> Key a -> EnumMap a b -> EnumMap a b
update :: (b -> Maybe b) -> Key a -> EnumMap a b -> EnumMap a b
updateWithKey :: (Key a -> b -> Maybe b) -> Key a -> EnumMap a b -> EnumMap a b
updateLookupWithKey :: (Key a -> b -> Maybe b) -> Key a -> EnumMap a b -> (Maybe b, EnumMap a b)
alter :: (Maybe b -> Maybe b) -> Key a -> EnumMap a b -> EnumMap a b
union :: EnumMap a b -> EnumMap a b -> EnumMap a b
unionWith :: (b -> b -> b) -> EnumMap a b -> EnumMap a b -> EnumMap a b
unionWithKey :: (Key a -> b -> b -> b) -> EnumMap a b -> EnumMap a b -> EnumMap a b
unions :: [EnumMap a b] -> EnumMap a b
unionsWith :: (b -> b -> b) -> [EnumMap a b] -> EnumMap a b
difference :: EnumMap a b -> EnumMap a c -> EnumMap a b
differenceWith :: (b -> c -> Maybe b) -> EnumMap a b -> EnumMap a c -> EnumMap a b
differenceWithKey :: (Key a -> b -> c -> Maybe b) -> EnumMap a b -> EnumMap a c -> EnumMap a b
intersection :: EnumMap a b -> EnumMap a c -> EnumMap a b
intersectionWith :: (b -> c -> b) -> EnumMap a b -> EnumMap a c -> EnumMap a b
intersectionWithKey :: (Key a -> b -> c -> b) -> EnumMap a b -> EnumMap a c -> EnumMap a b
map :: (b -> c) -> EnumMap a b -> EnumMap a c
