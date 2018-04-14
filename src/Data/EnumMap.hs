{-# LANGUAGE CPP,
             DeriveDataTypeable, DeriveGeneric,
             NoBangPatterns,
             MagicHash
             #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.EnumMap
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to values.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.EnumMap (EnumMap)
-- >  import qualified Data.EnumMap k as EnumMap
--
-- The implementation is based on /big-endian patricia trees/.  This data
-- structure performs especially well on binary operations like 'union'
-- and 'intersection'.  However, my benchmarks show that it is also
-- (much) faster on insertions and deletions when compared to a generic
-- size-balanced map implementation (see "Data.Map").
--
--    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
--      Workshop on ML, September 1998, pages 77-86,
--      <http://citeseer.ist.psu.edu/okasaki98fast.html>
--
--    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
--      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
--      October 1968, pages 514-534.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation <http://en.wikipedia.org/wiki/Big_O_notation>.
-- Many operations have a worst-case complexity of /O(min(n,W))/.
-- This means that the operation can become linear in the number of
-- elements with a maximum of /W/ -- the number of bits in an 'Int'
-- (32 or 64).
-----------------------------------------------------------------------------

module Data.EnumMap  (
            -- * Map type
              EnumMap, Key_          -- instance Eq,Show

            -- * Operators
            , (!), (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , lookup
            , findWithDefault

            -- * Construction
            , empty
            , singleton

            -- ** Insertion
            , insert
            , insertWith, insertWithKey, insertLookupWithKey

            -- ** Delete\/Update
            , delete
            , adjust
            , adjustWithKey
            , update
            , updateWithKey
            , updateLookupWithKey
            , alter

            -- * Combine

            -- ** Union
            , union
            , unionWith
            , unionWithKey
            , unions
            , unionsWith

            -- ** Difference
            , difference
            , differenceWith
            , differenceWithKey

            -- ** Intersection
            , intersection
            , intersectionWith
            , intersectionWithKey

            -- * Traversal
            -- ** Map
            , map
            , mapWithKey
            , mapAccum
            , mapAccumWithKey

            -- ** Fold
            , fold
            , foldWithKey

            -- * Conversion
            , elems
            , keys
            , keysSet
            , assocs

            -- ** Lists
            , toList
            , fromList
            , fromListWith
            , fromListWithKey

            -- ** Ordered lists
            , toAscList
            , fromAscList
            , fromAscListWith
            , fromAscListWithKey
            , fromDistinctAscList

            -- * Filter
            , filter
            , filterWithKey
            , partition
            , partitionWithKey

            , mapMaybe
            , mapMaybeWithKey
            , mapEither
            , mapEitherWithKey

            , split
            , splitLookup

            -- * Submap
            , isSubmapOf, isSubmapOfBy
            , isProperSubmapOf, isProperSubmapOfBy

            -- * Min\/Max

            , maxView
            , minView
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , updateMin
            , updateMax
            , updateMinWithKey
            , updateMaxWithKey
            , minViewWithKey
            , maxViewWithKey

            -- * Debugging
            , showTree
            , showTreeWith
            ) where


import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import qualified Prelude
import Data.Bits
import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import Data.Typeable
import Data.Foldable (Foldable(foldMap))
import Control.Monad ( liftM )
import GHC.Generics (Generic)
{-
-- just for testing
import qualified Prelude
import Debug.QuickCheck
import List (nub,sort)
import qualified List
-}

#if __GLASGOW_HASKELL__
import Text.Read
import Data.Data (Data(..))
#endif

#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#elif __GLASGOW_HASKELL__
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#else
import Data.Word
#endif

infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

{-# INLINE natFromInt #-}
natFromInt :: (Enum k) => k -> Nat
natFromInt i = fromIntegral . fromEnum $ i

{-# INLINE intFromNat #-}
intFromNat :: (Enum k) => Nat -> k
intFromNat w = toEnum . fromIntegral $ w

-- shiftRL :: (Enum k) => Nat -> k -> Nat
shiftRL :: Nat -> Int -> Nat
shiftRL x i = magicShiftRL x (fromEnum i)

magicShiftRL :: Nat -> Int -> Nat
#if __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
magicShiftRL (W# x) (I# i)
  = W# (shiftRL# x i)
#else
magicShiftRL x i   = shiftR x i
#endif

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: (Show k, Enum k) => EnumMap k a -> k -> a
m ! k    = find' k m

-- | Same as 'difference'.
(\\) :: (Enum k) => EnumMap k a -> EnumMap k b -> EnumMap k a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}
-- | A map of integers to values @a@.
data EnumMap k a = Nil
                | Tip {-# UNPACK #-} !Key_ a
                | Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !(EnumMap k a) !(EnumMap k a)
  deriving (Data, Typeable, Generic)

type Prefix = Int
type Mask   = Int
type Key_   = Int

instance (Enum k) => Monoid (EnumMap k a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Foldable (EnumMap k) where
    foldMap _ Nil = mempty
    foldMap f (Tip _k v) = f v
    foldMap f (Bin _ _ l r) = foldMap f l `mappend` foldMap f r

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
--
-- > Data.EnumMap.null (empty)           == True
-- > Data.EnumMap.null (singleton 1 'a') == False

null :: EnumMap k a -> Bool
null Nil = True
null _   = False

-- | /O(n)/. Number of elements in the map.
--
-- > size empty                                   == 0
-- > size (singleton 1 'a')                       == 1
-- > size (fromList([(1,'a'), (2,'c'), (3,'b')])) == 3
size :: EnumMap k a -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil     -> 0

-- | /O(min(n,W))/. Is the key a member of the map?
--
-- > member 5 (fromList [(5,'a'), (3,'b')]) == True
-- > member 1 (fromList [(5,'a'), (3,'b')]) == False

member :: (Enum k) => k -> EnumMap k a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _  -> True

-- | /O(log n)/. Is the key not a member of the map?
--
-- > notMember 5 (fromList [(5,'a'), (3,'b')]) == False
-- > notMember 1 (fromList [(5,'a'), (3,'b')]) == True

notMember :: (Enum k) => k -> EnumMap k a -> Bool
notMember k m = not $ member k m

-- | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: (Enum k) => k -> EnumMap k a -> Maybe a
lookup k t
  = let nk = natFromInt k  in seq nk (lookupN nk t)

lookupN :: Nat -> EnumMap k a -> Maybe a
lookupN k t
  = case t of
      Bin _ m l r
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx x
        | (k == natFromInt kx)  -> Just x
        | otherwise             -> Nothing
      Nil -> Nothing

find' :: (Show k, Enum k) => k -> EnumMap k a -> a
find' k m
  = case lookup k m of
      Nothing -> error ("EnumMap.find: key " ++ show k ++ " is not an element of the map")
      Just x  -> x


-- | /O(min(n,W))/. The expression @('findWithDefault' def k map)@
-- returns the value at key @k@ or returns @def@ when the key is not an
-- element of the map.
--
-- > findWithDefault 'x' 1 (fromList [(5,'a'), (3,'b')]) == 'x'
-- > findWithDefault 'x' 5 (fromList [(5,'a'), (3,'b')]) == 'a'

findWithDefault :: (Enum k) => a -> k -> EnumMap k a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x  -> x

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
--
-- > empty      == fromList []
-- > size empty == 0

empty :: EnumMap k a
empty
  = Nil

-- | /O(1)/. A map of one element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1

singleton :: (Enum k) => k -> a -> EnumMap k a
singleton k x
  = Tip (fromEnum k) x

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
-- > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
-- > insert 5 'x' empty                         == singleton 5 'x'
{-# SPECIALIZE insert :: Int -> a -> EnumMap Int a -> EnumMap Int a #-}
insert :: (Enum k) => k -> a -> EnumMap k a -> EnumMap k a
insert k x t
  = case t of
      Bin p m l r
        | nomatch k p m -> join k' (Tip k' x) p t
        | zero k m      -> Bin p m (insert k x l) r
        | otherwise     -> Bin p m l (insert k x r)
      Tip ky _
        | k' == ky      -> Tip k' x
        | otherwise     -> join k' (Tip k' x) ky t
      Nil -> Tip k' x
    where
        k' = fromEnum k

-- right-biased insertion, used by 'union'
-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f new_value old_value@.
--
-- > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
-- > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"

insertWith :: (Enum k) => (a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWith f k x t
  = insertWithKey (\_ x' y' -> f x' y') k x t

-- | /O(min(n,W))/. Insert with a combining function.
-- @'insertWithKey' f key value mp@
-- will insert the pair (key, value) into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert @f key new_value old_value@.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
-- > insertWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
-- > insertWithKey f 5 "xxx" empty                         == singleton 5 "xxx"

insertWithKey :: (Enum k) => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> EnumMap k a
insertWithKey f k x t
  = case t of
      Bin p m l r
        | nomatch k p m -> join k' (Tip k' x) p t
        | zero k m      -> Bin p m (insertWithKey f k x l) r
        | otherwise     -> Bin p m l (insertWithKey f k x r)
      Tip ky y
        | k' == ky      -> Tip k' (f k x y)
        | otherwise     -> join k' (Tip k' x) ky t
      Nil -> Tip k' x
    where k' = fromEnum k


-- | /O(min(n,W))/. The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:xxx|a")])
-- > insertLookupWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "xxx")])
-- > insertLookupWithKey f 5 "xxx" empty                         == (Nothing,  singleton 5 "xxx")
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "x")])
-- > insertLookup 7 "x" (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a"), (7, "x")])

insertLookupWithKey :: (Enum k) => (k -> a -> a -> a) -> k -> a -> EnumMap k a -> (Maybe a, EnumMap k a)
insertLookupWithKey f k x t
  = case t of
      Bin p m l r
        | nomatch k p m -> (Nothing,join k' (Tip k' x) p t)
        | zero k m      -> let (found,l') = insertLookupWithKey f k x l in (found,Bin p m l' r)
        | otherwise     -> let (found,r') = insertLookupWithKey f k x r in (found,Bin p m l r')
      Tip ky y
        | k' == ky      -> (Just y,Tip k' (f k x y))
        | otherwise     -> (Nothing,join k' (Tip k' x) ky t)
      Nil -> (Nothing,Tip k' x)
    where k' = fromEnum k


{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
--
-- > delete 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > delete 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > delete 5 empty                         == empty

delete :: (Enum k) => k -> EnumMap k a -> EnumMap k a
delete k t
  = case t of
      Bin p m l r
        | nomatch k p m -> t
        | zero k m      -> bin p m (delete k l) r
        | otherwise     -> bin p m l (delete k r)
      Tip ky _
        | k' == ky      -> Nil
        | otherwise     -> t
      Nil -> Nil
    where k' = fromEnum k

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjust ("new " ++) 7 empty                         == empty

adjust :: (Enum k) => (a -> a) -> k -> EnumMap k a -> EnumMap k a
adjust f k m
  = adjustWithKey (\_ x -> f x) k m

-- | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > adjustWithKey f 7 empty                         == empty

adjustWithKey :: (Enum k) => (k -> a -> a) -> k -> EnumMap k a -> EnumMap k a
adjustWithKey f k m
  = updateWithKey (\k' x -> Just (f k' x)) k m

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: (Enum k) => (a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
update f k m
  = updateWithKey (\_ x -> f x) k m

-- | /O(min(n,W))/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f k x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateWithKey :: (Enum k) => (k -> a -> Maybe a) -> k -> EnumMap k a -> EnumMap k a
updateWithKey f k t
  = case t of
      Bin p m l r
        | nomatch k p m -> t
        | zero k m      -> bin p m (updateWithKey f k l) r
        | otherwise     -> bin p m l (updateWithKey f k r)
      Tip ky y
        | k' == ky      -> case (f k y) of
                             Just y' -> Tip ky y'
                             Nothing -> Nil
        | otherwise     -> t
      Nil -> Nil
    where k' = fromEnum k

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than 'Data.Map.updateLookupWithKey'.
-- Returns the original key value if the map entry is deleted.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "a", fromList [(3, "b"), (5, "5:new a")])
-- > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
-- > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

updateLookupWithKey :: (Enum k) => (k -> a -> Maybe a) -> k -> EnumMap k a -> (Maybe a,EnumMap k a)
updateLookupWithKey f k t
  = case t of
      Bin p m l r
        | nomatch k p m -> (Nothing,t)
        | zero k m      -> let (found,l') = updateLookupWithKey f k l in (found,bin p m l' r)
        | otherwise     -> let (found,r') = updateLookupWithKey f k r in (found,bin p m l r')
      Tip ky y
        | k' == ky      -> case (f k y) of
                             Just y' -> (Just y,Tip ky y')
                             Nothing -> (Just y,Nil)
        | otherwise     -> (Nothing,t)
      Nil -> (Nothing,Nil)
    where k' = fromEnum k



-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in an 'EnumMap'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: (Maybe a -> Maybe a) -> Int -> EnumMap k a -> EnumMap k a
alter f k t
  = case t of
      Bin p m l r
        | nomatch k p m -> case f Nothing of
                             Nothing -> t
                             Just x -> join k (Tip k x) p t
        | zero k m      -> bin p m (alter f k l) r
        | otherwise     -> bin p m l (alter f k r)
      Tip ky y
        | k==ky         -> case f (Just y) of
                             Just x -> Tip ky x
                             Nothing -> Nil
        | otherwise     -> case f Nothing of
                             Just x -> join k (Tip k x) ky t
                             Nothing -> Tip ky y
      Nil               -> case f Nothing of
                             Just x -> Tip k x
                             Nothing -> Nil


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
--
-- > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
-- >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

unions :: (Enum k) => [EnumMap k a] -> EnumMap k a
unions xs
  = foldlStrict union empty xs

-- | The union of a list of maps, with a combining operation.
--
-- > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
-- >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

unionsWith :: (Enum k) => (a->a->a) -> [EnumMap k a] -> EnumMap k a
unionsWith f ts
  = foldlStrict (unionWith f) empty ts

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: (Enum k) => EnumMap k a -> EnumMap k a -> EnumMap k a
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip k x) t = insert (toEnum k) x t
union t (Tip k x) = insertWith (\_ y -> y) (toEnum k) x t  -- right bias
union Nil t       = t
union t Nil       = t

-- | /O(n+m)/. The union with a combining function.
--
-- > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

unionWith :: (Enum k) => (a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWith f m1 m2
  = unionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. The union with a combining function.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

unionWithKey :: (Enum k) => (k -> a -> a -> a) -> EnumMap k a -> EnumMap k a -> EnumMap k a
unionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (unionWithKey f l1 l2) (unionWithKey f r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (unionWithKey f l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (unionWithKey f r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (unionWithKey f t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (unionWithKey f t1 r2)

unionWithKey f (Tip k x) t = insertWithKey f (toEnum k) x t
unionWithKey f t (Tip k x) = insertWithKey (\k' x' y' -> f k' y' x') (toEnum k) x t  -- right bias
unionWithKey _ Nil t  = t
unionWithKey _ t Nil  = t

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two maps (based on keys).
--
-- > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: (Enum k) => EnumMap k a -> EnumMap k b -> EnumMap k a
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t1@(Tip k _) t2
  | member (toEnum k) t2  = Nil
  | otherwise    = t1

difference Nil _       = Nil
difference t (Tip k _) = delete (toEnum k) t
difference t Nil       = t

-- | /O(n+m)/. Difference with a combining function.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
-- >     == singleton 3 "b:B"

differenceWith :: (Enum k) => (a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference).
-- If it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
-- >     == singleton 3 "3:b|B"

differenceWithKey :: (Enum k) => (k -> a -> b -> Maybe a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
differenceWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (differenceWithKey f l1 l2) (differenceWithKey f r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (differenceWithKey f l1 t2) r1
                | otherwise         = bin p1 m1 l1 (differenceWithKey f r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = differenceWithKey f t1 l2
                | otherwise         = differenceWithKey f t1 r2

differenceWithKey f t1@(Tip k x) t2
  = case lookup (toEnum k) t2 of
      Just y  -> case f (toEnum k) x y of
                   Just y' -> Tip k y'
                   Nothing -> Nil
      Nothing -> t1

differenceWithKey _ Nil _       = Nil
differenceWithKey f t (Tip k y) = updateWithKey (\k' x -> f k' x y) (toEnum k) t
differenceWithKey _ t Nil       = t


{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The (left-biased) intersection of two maps (based on keys).
--
-- > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: (Enum k) => EnumMap k a -> EnumMap k b -> EnumMap k a
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1@(Tip k _) t2
  | member (toEnum k) t2  = t1
  | otherwise    = Nil
intersection t (Tip k _)
  = case lookup (toEnum k) t of
      Just y  -> Tip k y
      Nothing -> Nil
intersection Nil _ = Nil
intersection _ Nil = Nil

-- | /O(n+m)/. The intersection with a combining function.
--
-- > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: (Enum k) => (a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

-- | /O(n+m)/. The intersection with a combining function.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"

intersectionWithKey :: (Enum k) => (k -> a -> b -> a) -> EnumMap k a -> EnumMap k b -> EnumMap k a
intersectionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersectionWithKey f l1 l2) (intersectionWithKey f r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersectionWithKey f l1 t2
                  | otherwise         = intersectionWithKey f r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersectionWithKey f t1 l2
                  | otherwise         = intersectionWithKey f t1 r2

intersectionWithKey f (Tip k x) t2
  = let k' = toEnum k
    in case lookup k' t2 of
      Just y  -> Tip k (f k' x y)
      Nothing -> Nil
intersectionWithKey f t1 (Tip k y)
  = let k' = toEnum k
    in case lookup k' t1 of
      Just x  -> Tip k (f k' x y)
      Nothing -> Nil
intersectionWithKey _ Nil _ = Nil
intersectionWithKey _ _ Nil = Nil


{--------------------------------------------------------------------
  Min\/Max
--------------------------------------------------------------------}

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: (Enum k) => (k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMinWithKey f t
    = case t of
        Bin p m l r | m < 0 -> let t' = updateMinWithKeyUnsigned f r in Bin p m l t'
        Bin p m l r         -> let t' = updateMinWithKeyUnsigned f l in Bin p m t' r
        Tip k y -> Tip k (f (toEnum k) y)
        Nil -> error "maxView: empty map has no maximal element"

updateMinWithKeyUnsigned :: (Enum k) => (k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMinWithKeyUnsigned f t
    = case t of
        Bin p m l r -> let t' = updateMinWithKeyUnsigned f l in Bin p m t' r
        Tip k y -> Tip k (f (toEnum k) y)
        Nil -> error "updateMinWithKeyUnsigned Nil"

-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
-- > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: (Enum k) => (k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMaxWithKey f t
    = case t of
        Bin p m l r | m < 0 -> let t' = updateMaxWithKeyUnsigned f l in Bin p m t' r
        Bin p m l r         -> let t' = updateMaxWithKeyUnsigned f r in Bin p m l t'
        Tip k y -> Tip k (f (toEnum k) y)
        Nil -> error "maxView: empty map has no maximal element"

updateMaxWithKeyUnsigned :: (Enum k) => (k -> a -> a) -> EnumMap k a -> EnumMap k a
updateMaxWithKeyUnsigned f t
    = case t of
        Bin p m l r -> let t' = updateMaxWithKeyUnsigned f r in Bin p m l t'
        Tip k y -> Tip k (f (toEnum k) y)
        Nil -> error "updateMaxWithKeyUnsigned Nil"


-- | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
-- > maxViewWithKey empty == Nothing

maxViewWithKey :: (Enum k) => EnumMap k a -> Maybe ((k, a), EnumMap k a)
maxViewWithKey t
    = case t of
        Bin p m l r | m < 0 -> let (result, t') = maxViewUnsigned l in Just (result, bin p m t' r)
        Bin p m l r         -> let (result, t') = maxViewUnsigned r in Just (result, bin p m l t')
        Tip k y -> Just ((toEnum k,y), Nil)
        Nil -> Nothing

maxViewUnsigned :: (Enum k) => EnumMap k a -> ((k, a), EnumMap k a)
maxViewUnsigned t
    = case t of
        Bin p m l r -> let (result,t') = maxViewUnsigned r in (result,bin p m l t')
        Tip k y -> ((toEnum k,y), Nil)
        Nil -> error "maxViewUnsigned Nil"

-- | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
--
-- > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
-- > minViewWithKey empty == Nothing

minViewWithKey :: (Enum k) => EnumMap k a -> Maybe ((k, a), EnumMap k a)
minViewWithKey t
    = case t of
        Bin p m l r | m < 0 -> let (result, t') = minViewUnsigned r in Just (result, bin p m l t')
        Bin p m l r         -> let (result, t') = minViewUnsigned l in Just (result, bin p m t' r)
        Tip k y -> Just ((toEnum k,y),Nil)
        Nil -> Nothing

minViewUnsigned :: (Enum k) => EnumMap k a -> ((k, a), EnumMap k a)
minViewUnsigned t
    = case t of
        Bin p m l r -> let (result,t') = minViewUnsigned l in (result,bin p m t' r)
        Tip k y -> ((toEnum k,y),Nil)
        Nil -> error "minViewUnsigned Nil"


-- | /O(log n)/. Update the value at the maximal key.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: (Enum k) => (a -> a) -> EnumMap k a -> EnumMap k a
updateMax f = updateMaxWithKey (const f)

-- | /O(log n)/. Update the value at the minimal key.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: (Enum k) => (a -> a) -> EnumMap k a -> EnumMap k a
updateMin f = updateMinWithKey (const f)

-- Similar to the Arrow instance.
first :: (a -> c) -> (a, b) -> (c, b)
first f (x,y) = (f x,y)

-- | /O(log n)/. Retrieves the maximal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
maxView :: (Enum k) => EnumMap k a -> Maybe (a, EnumMap k a)
maxView t = liftM (first snd) (maxViewWithKey t)

-- | /O(log n)/. Retrieves the minimal key of the map, and the map
-- stripped of that element, or 'Nothing' if passed an empty map.
minView :: (Enum k) => EnumMap k a -> Maybe (a, EnumMap k a)
minView t = liftM (first snd) (minViewWithKey t)

-- | /O(log n)/. Delete and find the maximal element.
deleteFindMax :: (Enum k) => EnumMap k a -> (a, EnumMap k a)
deleteFindMax = fromMaybe (error "deleteFindMax: empty map has no maximal element") . maxView

-- | /O(log n)/. Delete and find the minimal element.
deleteFindMin :: (Enum k) => EnumMap k a -> (a, EnumMap k a)
deleteFindMin = fromMaybe (error "deleteFindMin: empty map has no minimal element") . minView

-- | /O(log n)/. The minimal key of the map.
findMin :: (Enum k) => EnumMap k a -> a
findMin = maybe (error "findMin: empty map has no minimal element") fst . minView

-- | /O(log n)/. The maximal key of the map.
findMax :: (Enum k) => EnumMap k a -> a
findMax = maybe (error "findMax: empty map has no maximal element") fst . maxView

-- | /O(log n)/. Delete the minimal key.
deleteMin :: (Enum k) => EnumMap k a -> EnumMap k a
deleteMin = maybe (error "deleteMin: empty map has no minimal element") snd . minView

-- | /O(log n)/. Delete the maximal key.
deleteMax :: (Enum k) => EnumMap k a -> EnumMap k a
deleteMax = maybe (error "deleteMax: empty map has no maximal element") snd . maxView


{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' (==)@).
isProperSubmapOf :: (Enum k, Eq a) => EnumMap k a -> EnumMap k a -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])
-}
isProperSubmapOfBy :: (Enum k) => (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _  -> False

submapCmp :: (Enum k) => (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Ordering
submapCmp predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = submapCmpLt
  | p1 == p2       = submapCmpEq
  | otherwise      = GT  -- disjoint
  where
    submapCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = submapCmp predicate t1 l2
                | otherwise         = submapCmp predicate t1 r2
    submapCmpEq = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

submapCmp _         (Bin _ _ _ _) _  = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise                   = GT  -- disjoint
submapCmp predicate (Tip k x) t
  = case lookup (toEnum k) t of
     Just y | predicate x y -> LT
     _                      -> GT -- disjoint
submapCmp _    Nil Nil = EQ
submapCmp _    Nil _   = LT

-- | /O(n+m)/. Is this a submap?
-- Defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (Eq a, Enum k) => EnumMap k a -> EnumMap k a -> Bool
isSubmapOf m1 m2
  = isSubmapOfBy (==) m1 m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f m1 m2@) returns 'True' if
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isSubmapOfBy (==) (fromList [(1,2)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (<) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
-}
isSubmapOfBy :: (Enum k) => (a -> b -> Bool) -> EnumMap k a -> EnumMap k b -> Bool
isSubmapOfBy predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubmapOfBy predicate t1 l2
                                                      else isSubmapOfBy predicate t1 r2)
  | otherwise      = (p1==p2) && isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
isSubmapOfBy _         (Bin _ _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t     = case lookup (toEnum k) t of
                                         Just y  -> predicate x y
                                         Nothing -> False
isSubmapOfBy _         Nil _           = True

{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}
-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: (Enum k) => (a -> b) -> EnumMap k a -> EnumMap k b
map f m
  = mapWithKey (\_ x -> f x) m

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: (Enum k) => (k -> a -> b) -> EnumMap k a -> EnumMap k b
mapWithKey f t
  = case t of
      Bin p m l r -> Bin p m (mapWithKey f l) (mapWithKey f r)
      Tip k x     -> Tip k (f (toEnum k) x)
      Nil         -> Nil

-- | /O(n)/. The function @'mapAccum'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: (Enum k) => (a -> b -> (a,c)) -> a -> EnumMap k b -> (a,EnumMap k c)
mapAccum f a m
  = mapAccumWithKey (\a' _ x -> f a' x) a m

-- | /O(n)/. The function @'mapAccumWithKey'@ threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: (Enum k) => (a -> k -> b -> (a,c)) -> a -> EnumMap k b -> (a,EnumMap k c)
mapAccumWithKey f a t
  = mapAccumL f a t

-- | /O(n)/. The function @'mapAccumL'@ threads an accumulating
-- argument through the map in ascending order of keys.
mapAccumL :: (Enum k) => (a -> k -> b -> (a,c)) -> a -> EnumMap k b -> (a,EnumMap k c)
mapAccumL f a t
  = case t of
      Bin p m l r -> let (a1,l') = mapAccumL f a l
                         (a2,r') = mapAccumL f a1 r
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a (toEnum k) x in (a',Tip k x')
      Nil         -> (a,Nil)

{-
XXX unused code

-- | /O(n)/. The function @'mapAccumR'@ threads an accumulating
-- argument throught the map in descending order of keys.
mapAccumR :: (a -> Key -> b -> (a,c)) -> a -> EnumMap k b -> (a,EnumMap k c)
mapAccumR f a t
  = case t of
      Bin p m l r -> let (a1,r') = mapAccumR f a r
                         (a2,l') = mapAccumR f a1 l
                     in (a2,Bin p m l' r')
      Tip k x     -> let (a',x') = f a k x in (a',Tip k x')
      Nil         -> (a,Nil)
-}

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all values that satisfy some predicate.
--
-- > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
-- > filter (> "x") (fromList [(5,"a"), (3,"b")]) == empty
-- > filter (< "a") (fromList [(5,"a"), (3,"b")]) == empty

filter :: (Enum k) => (a -> Bool) -> EnumMap k a -> EnumMap k a
filter p m
  = filterWithKey (\_ x -> p x) m

-- | /O(n)/. Filter all keys\/values that satisfy some predicate.
--
-- > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

filterWithKey :: (Enum k) => (k -> a -> Bool) -> EnumMap k a -> EnumMap k a
filterWithKey predicate t
  = case t of
      Bin p m l r
        -> bin p m (filterWithKey predicate l) (filterWithKey predicate r)
      Tip k x
        | predicate (toEnum k) x -> t
        | otherwise              -> Nil
      Nil -> Nil

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partition :: (Enum k) => (a -> Bool) -> EnumMap k a -> (EnumMap k a,EnumMap k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

-- | /O(n)/. Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate. See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])

partitionWithKey :: (Enum k) => (k -> a -> Bool) -> EnumMap k a -> (EnumMap k a,EnumMap k a)
partitionWithKey predicate t
  = case t of
      Bin p m l r
        -> let (l1,l2) = partitionWithKey predicate l
               (r1,r2) = partitionWithKey predicate r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip k x
        | predicate (toEnum k) x -> (t,Nil)
        | otherwise              -> (Nil,t)
      Nil -> (Nil,Nil)

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: (Enum k) => (a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybe f m
  = mapMaybeWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: (Enum k) => (k -> a -> Maybe b) -> EnumMap k a -> EnumMap k b
mapMaybeWithKey f (Bin p m l r)
  = bin p m (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x) = case f (toEnum k) x of
  Just y  -> Tip k y
  Nothing -> Nil
mapMaybeWithKey _ Nil = Nil

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
-- >
-- > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: (Enum k) => (a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEither f m
  = mapEitherWithKey (\_ x -> f x) m

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
-- >     == (empty, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: (Enum k) => (k -> a -> Either b c) -> EnumMap k a -> (EnumMap k b, EnumMap k c)
mapEitherWithKey f (Bin p m l r)
  = (bin p m l1 r1, bin p m l2 r2)
  where
    (l1,l2) = mapEitherWithKey f l
    (r1,r2) = mapEitherWithKey f r
mapEitherWithKey f (Tip k x) = case f (toEnum k) x of
  Left y  -> (Tip k y, Nil)
  Right z -> (Nil, Tip k z)
mapEitherWithKey _ Nil = (Nil, Nil)

-- | /O(log n)/. The expression (@'split' k map@) is a pair @(map1,map2)@
-- where all keys in @map1@ are lower than @k@ and all keys in
-- @map2@ larger than @k@. Any key equal to @k@ is found in neither @map1@ nor @map2@.
--
-- > split 2 (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3,"b"), (5,"a")])
-- > split 3 (fromList [(5,"a"), (3,"b")]) == (empty, singleton 5 "a")
-- > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
-- > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", empty)
-- > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], empty)

split :: (Enum k) => k -> EnumMap k a -> (EnumMap k a,EnumMap k a)
split k t
  = case t of
      Bin _ m l r
          | m < 0 -> (if k' >= 0 -- handle negative numbers.
                      then let (lt,gt) = split' k l in (union r lt, gt)
                      else let (lt,gt) = split' k r in (lt, union gt l))
          | otherwise   -> split' k t
      Tip ky _
        | k' > ky      -> (t,Nil)
        | k' < ky      -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)
    where k' = fromEnum k

split' :: (Enum k) => k -> EnumMap k a -> (EnumMap k a,EnumMap k a)
split' k t
  = case t of
      Bin p m l r
        | nomatch k' p m -> if k' > p then (t,Nil) else (Nil,t)
        | zero k m  -> let (lt,gt) = split k l in (lt,union gt r)
        | otherwise -> let (lt,gt) = split k r in (union l lt,gt)
      Tip ky _
        | k' > ky      -> (t,Nil)
        | k' < ky      -> (Nil,t)
        | otherwise    -> (Nil,Nil)
      Nil -> (Nil,Nil)
    where k' = fromEnum k

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- key was found in the original map.
--
-- > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (empty, Nothing, fromList [(3,"b"), (5,"a")])
-- > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (empty, Just "b", singleton 5 "a")
-- > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
-- > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", empty)
-- > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, empty)

splitLookup :: (Enum k) => k -> EnumMap k a -> (EnumMap k a,Maybe a,EnumMap k a)
splitLookup k t
  = case t of
      Bin _ m l r
          | m < 0 -> (if k' >= 0 -- handle negative numbers.
                      then let (lt,found,gt) = splitLookup' k l in (union r lt,found, gt)
                      else let (lt,found,gt) = splitLookup' k r in (lt,found, union gt l))
          | otherwise   -> splitLookup' k t
      Tip ky y
        | k' > ky      -> (t,Nothing,Nil)
        | k' < ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)
    where k' = fromEnum k

splitLookup' :: (Enum k) => k -> EnumMap k a -> (EnumMap k a,Maybe a,EnumMap k a)
splitLookup' k t
  = case t of
      Bin p m l r
        | nomatch k' p m -> if k' > p then (t,Nothing,Nil) else (Nil,Nothing,t)
        | zero k' m  -> let (lt,found,gt) = splitLookup k l in (lt,found,union gt r)
        | otherwise  -> let (lt,found,gt) = splitLookup k r in (union l lt,found,gt)
      Tip ky y
        | k' > ky      -> (t,Nothing,Nil)
        | k' < ky      -> (Nil,Nothing,t)
        | otherwise -> (Nil,Just y,Nil)
      Nil -> (Nil,Nothing,Nil)
    where k' = fromEnum k

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
-- > let f a len = len + (length a)
-- > fold f 0 (fromList [(5,"a"), (3,"bbb")]) == 4

fold :: (Enum k) => (a -> b -> b) -> b -> EnumMap k a -> b
fold f z t
  = foldWithKey (\_ x y -> f x y) z t

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
-- > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
-- > foldWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"

foldWithKey :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldWithKey f z t
  = foldr f z t

foldr :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldr f z t
  = case t of
      Bin 0 m l r | m < 0 -> foldr' f (foldr' f z l) r  -- put negative numbers before.
      Bin _ _ _ _ -> foldr' f z t
      Tip k x     -> f (toEnum k) x z
      Nil         -> z

foldr' :: (Enum k) => (k -> a -> b -> b) -> b -> EnumMap k a -> b
foldr' f z t
  = case t of
      Bin _ _ l r -> foldr' f (foldr' f z r) l
      Tip k x     -> f (toEnum k) x z
      Nil         -> z



{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
-- > elems empty == []

elems :: (Enum k) => EnumMap k a -> [a]
elems m
  = foldWithKey (\_ x xs -> x:xs) [] m

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys empty == []

keys :: (Enum k) => EnumMap k a -> [k]
keys m
  = foldWithKey (\k _ ks -> k:ks) [] m

-- | /O(n*min(n,W))/. The set of all keys of the map.
--
-- > keysSet (fromList [(5,"a"), (3,"b")]) == Data.IntSet.fromList [3,5]
-- > keysSet empty == Data.IntSet.empty

keysSet :: (Enum k) => EnumMap k a -> IntSet.IntSet
keysSet m = IntSet.fromDistinctAscList $ Prelude.map fromEnum (keys m)


-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
--
-- > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > assocs empty == []

assocs :: (Enum k) => EnumMap k a -> [(k,a)]
assocs m
  = toList m


{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the map to a list of key\/value pairs.
--
-- > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
-- > toList empty == []

toList :: (Enum k) => EnumMap k a -> [(k,a)]
toList t
  = foldWithKey (\k x xs -> (k,x):xs) [] t

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order.
--
-- > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

toAscList :: (Num k, Ord k, Enum k) => EnumMap k a -> [(k,a)]
toAscList t
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (\(k,_) -> k >=0) (foldr (\k x xs -> (k,x):xs) [] t) in neg ++ pos

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a"), (3,"b"), (5, "c")] == fromList [(5,"c"), (3,"b")]
-- > fromList [(5,"c"), (3,"b"), (5, "a")] == fromList [(5,"a"), (3,"b")]

fromList :: (Enum k) => [(k,a)] -> EnumMap k a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWith :: (Enum k) => (a -> a -> a) -> [(k,a)] -> EnumMap k a
fromListWith f xs
  = fromListWithKey (\_ x y -> f x y) xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs with a combining function. See also fromAscListWithKey'.
--
-- > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
-- > fromListWith (++) [] == empty

fromListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k,a)] -> EnumMap k a
fromListWithKey f xs
  = foldlStrict ins empty xs
  where
    ins t (k,x) = insertWithKey f k x t

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order.
--
-- > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
-- > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]

fromAscList :: (Enum k) => [(k,a)] -> EnumMap k a
fromAscList xs
  = fromList xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]

fromAscListWith :: (Enum k) => (a -> a -> a) -> [(k,a)] -> EnumMap k a
fromAscListWith f xs
  = fromListWith f xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order, with a combining function on equal keys.
--
-- > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]

fromAscListWithKey :: (Enum k) => (k -> a -> a -> a) -> [(k,a)] -> EnumMap k a
fromAscListWithKey f xs
  = fromListWithKey f xs

-- | /O(n*min(n,W))/. Build a map from a list of key\/value pairs where
-- the keys are in ascending order and all distinct.
--
-- > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]

fromDistinctAscList :: (Enum k) => [(k,a)] -> EnumMap k a
fromDistinctAscList xs
  = fromList xs


{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq a => Eq (EnumMap k a) where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: Eq a => EnumMap k a -> EnumMap k a -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal _   _   = False

nequal :: Eq a => EnumMap k a -> EnumMap k a -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal _   _   = True

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance (Ord k, Ord a, Enum k) => Ord (EnumMap k a) where
    compare m1 m2 = compare (toList m1) (toList m2)

{--------------------------------------------------------------------
  Functor
--------------------------------------------------------------------}

instance (Enum k) => Functor (EnumMap k) where
    fmap = map

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}

instance (Show a, Show k, Enum k) => Show (EnumMap k a) where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)

{-
XXX unused code

showMap :: (Show a) => [(Key,a)] -> ShowS
showMap []
  = showString "{}"
showMap (x:xs)
  = showChar '{' . showElem x . showTail xs
  where
    showTail []     = showChar '}'
    showTail (x':xs') = showChar ',' . showElem x' . showTail xs'

    showElem (k,v)  = shows k . showString ":=" . shows v
-}

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}
instance (Read e, Read k, Enum k) => Read (EnumMap k e) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \ r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    return (fromList xs,t)
#endif

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the map. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => EnumMap k a -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the map. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Show a => Bool -> Bool -> EnumMap k a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> EnumMap k a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip k x
          -> showsBars lbars . showString " " . shows k . showString ":=" . shows x . showString "\n"
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Show a => Bool -> [String] -> EnumMap k a -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip k x
          -> showsBars bars . showString " " . shows k . showString ":=" . shows x . showString "\n"
      Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> Mask -> String
showBin _ _
  = "*" -- ++ show (p,m)

showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node :: String
node           = "+--"

withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars


{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
{-# SPECIALIZE join :: Prefix -> EnumMap Int a -> Prefix -> EnumMap Int a -> EnumMap Int a #-}
join :: Prefix -> EnumMap k a -> Prefix -> EnumMap k a -> EnumMap k a
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> EnumMap k a -> EnumMap k a -> EnumMap k a
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r


{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: (Enum k) => k -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,match :: (Enum k) => k -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

mask :: (Enum k) => k -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)


zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The mantissa
    is retrieved either via the standard C function [frexp] or by some bit
    twiddling on IEEE compatible numbers (float). Note that one needs to
    use at least [double] precision for an accurate mantissa of 32 bit
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x0
  = case (x0 .|. shiftRL x0 1 ) of
     x1 -> case (x1 .|. shiftRL x1 2) of
      x2 -> case (x2 .|. shiftRL x2 4) of
       x3 -> case (x3 .|. shiftRL x3 8) of
        x4 -> case (x4 .|. shiftRL x4 16) of
         x5 -> case (x5 .|. shiftRL x5 32) of   -- for 64 bit platforms
          x6 -> (x6 `xor` (shiftRL x6 1))


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree :: [Int] -> EnumMap Int
testTree xs   = fromList [(x,x*x*30696 `mod` 65521) | x <- xs]
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (EnumMap k a) where
  arbitrary = do{ ks <- arbitrary
                ; xs <- mapM (\k -> do{ x <- arbitrary; return (k,x)}) ks
                ; return (fromList xs)
                }


{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Key -> Int -> Bool
prop_Single k x
  = (insert k x empty == singleton k x)

prop_InsertDelete :: Key -> Int -> EnumMap Int -> Property
prop_InsertDelete k x t
  = not (member k t) ==> delete k (insert k x t) == t

prop_UpdateDelete :: Key -> EnumMap Int -> Bool
prop_UpdateDelete k t
  = update (const Nothing) k t == delete k t


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionInsert :: Key -> Int -> EnumMap Int -> Bool
prop_UnionInsert k x t
  = union (singleton k x) t == insert k x t

prop_UnionAssoc :: EnumMap Int -> EnumMap Int -> EnumMap Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: EnumMap Int -> EnumMap Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == unionWith (\x y -> y) t2 t1)


prop_Diff :: [(Key,Int)] -> [(Key,Int)] -> Bool
prop_Diff xs ys
  =  List.sort (keys (difference (fromListWith (+) xs) (fromListWith (+) ys)))
    == List.sort ((List.\\) (nub (Prelude.map fst xs))  (nub (Prelude.map fst ys)))

prop_Int :: [(Key,Int)] -> [(Key,Int)] -> Bool
prop_Int xs ys
  =  List.sort (keys (intersection (fromListWith (+) xs) (fromListWith (+) ys)))
    == List.sort (nub ((List.intersect) (Prelude.map fst xs)  (Prelude.map fst ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [(x,()) | x <- [0..n::Int]]
    in fromAscList xs == fromList xs

prop_List :: [Key] -> Bool
prop_List xs
  = (sort (nub xs) == [x | (x,()) <- toAscList (fromList [(x,()) | x <- xs])])


{--------------------------------------------------------------------
  updateMin / updateMax
--------------------------------------------------------------------}
prop_UpdateMinMax :: [Key] -> Bool
prop_UpdateMinMax xs =
  let m = fromList [(x,0)|x<-xs]
      minKey = fst . head . Prelude.filter ((==1).snd) . assocs . updateMin succ $ m
      maxKey = fst . head . Prelude.filter ((==1).snd) . assocs . updateMax succ $ m
  in  all (>=minKey) xs && all (<=maxKey) xs

-}
