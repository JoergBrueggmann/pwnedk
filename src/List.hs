{-|
Description : provides helper functions for lists.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'List' provides advanced helper functions for lists.

See also more basic list functions in module 'Safer'.

See also sting functions in module 'String'.

Suggested import line: 'import qualified List as Lst'

-}


module List
    (
        Lst.splitOn, 
        areAllOrdUnique, 
        duplicates, 
        atDef, 
        mAt, 
        setAt, 
        takeEnd, 
        dropBegin, 
        dropEnd, 
        splitAt, 
        isLengthMin, 
        shiftR, 
        mListConcat, 
        isLongAtLeast, 
        doesMatchBegin
    ) where


import qualified Safer as Sfr

import qualified Data.List as Lst
import qualified Data.List.Split as Lst
import Prelude hiding (splitAt)


-- areAllUnique
{-| ...to test whether all elements are unique (no duplicates and not any element is equal to any other element in the list).
* empty lists result in True
* elements have to be of class Ord, to have computational complexity of 𝒪(n⋅log n)

    * sorts the list and then checks whether all neighbours are different
-}
areAllOrdUnique :: Ord a => [a] -> Bool
areAllOrdUnique = areAllNeighboursDifferent . Lst.sort

-- areAllNeighboursDifferent
-- | ...to test whether all neighbour of elements are different.
areAllNeighboursDifferent :: Eq a => [a] -> Bool
areAllNeighboursDifferent []  = True
areAllNeighboursDifferent [_] = True
areAllNeighboursDifferent (x0:lrx1@(x1:_))
    | x0 == x1  = False
    | otherwise = areAllNeighboursDifferent lrx1

duplicates :: Ord a => [a] -> [(a, Integer)]
duplicates = equalNeighboursAcc [] . lIndexedSort . lIndexedAcc 0

compareIndexed :: Ord a => (a, Integer) -> (a, Integer) -> Ordering
compareIndexed (x0,_) (x1,_) = compare x0 x1

lIndexedSort :: Ord a => [(a, Integer)] -> [(a, Integer)]
lIndexedSort = Lst.sortBy compareIndexed

lIndexedAcc :: Integer -> [a] -> [(a, Integer)]
lIndexedAcc _ [] = []
lIndexedAcc niIndex (x:lrx) = (x, niIndex) : lIndexedAcc (niIndex + 1) lrx

-- equalNeighboursAcc
-- | ...to test whether all neighbour of elements are different.
equalNeighboursAcc :: Eq a => [(a, Integer)] -> [(a, Integer)] -> [(a, Integer)]
equalNeighboursAcc acc []  = acc
equalNeighboursAcc acc [_] = acc
equalNeighboursAcc acc ((x0,ni0):lrx@((x1,ni1):_))
    | x0 == x1  = equalNeighboursAcc ((x0,ni0):(x1,ni1):acc) lrx
    | otherwise = equalNeighboursAcc acc lrx

-- setAt...
-- | ...sets (replaces) an elements at a indexed position within a list.
{- |
* if the index is negative or the index is >= length of the list

    * leaves the list unchanged
-}
setAt :: Integer -> a -> [a] -> [a]
setAt _ _ [] = []
setAt nPos xReplacement lx@(x:lrx)
    | nPos == 0 = xReplacement : lrx
    | nPos < 0 = lx
    | otherwise = x : setAt (nPos -  1) xReplacement lrx

atDef :: a -> [a] -> Integer -> a
atDef aDef [] _ = aDef                          -- case: is empty anyway
atDef _ (a:_) 0 = a                             -- case: index is 0 -> take it
atDef aDef (_:la) nIndex
    | nIndex > 0 = atDef aDef la (nIndex - 1)   -- case: index is positive
    | otherwise  = aDef                         -- case: index is negative

mAt :: [a] -> Integer ->  Maybe a
mAt []     _ = Nothing                        -- case: is empty anyway
mAt (a:_)  0 = Just a                         -- case: index is 0 -> take it
mAt (_:la) nIndex
    | nIndex > 0 = mAt la (nIndex - 1)        -- case: index is positive
    | otherwise  = Nothing                      -- case: index is negative

takeEnd :: Integer -> [a] -> [a]
takeEnd ni lx                                       -- algorithm adapted from Data.List.Extra
    | ni <= 0 = []
    | otherwise = takeEnd' lx (Sfr.drop ni lx)
    where
        takeEnd' (_:lrx) (_:ys) = takeEnd' lrx ys
        takeEnd' lx' _ = lx'

-- dropBegin
-- | ...removes the first elements of a given amount.
{- |
* will drop the given amount at the maximum
* will drop elements as they are available

Example:

    @
print $ dropBegin (-1) "123"
print $ dropBegin 0 "123"
print $ dropBegin 1 "123"
print $ dropBegin 2 "123"
print $ dropBegin 3 "123"
print $ dropBegin 4 "123"
    @

output:

    @
"123"
"123"
"23"
"3"
""
""
    @
-}
dropBegin 
    -- | the maximum amount of elements to be removed
    :: Integer 
    -- | list of elements where elements to be removed
    -> [a] 
    -- | the list of elements where elements are removed
    -> [a]
dropBegin _ [] = []
dropBegin ni lx@(_:lrx) 
    | ni <= 0               = lx
    | otherwise             = dropBegin (ni - 1) lrx

dropEnd :: Integer -> [a] -> [a]
dropEnd _ [] = []
dropEnd ni lx@(x:lrx)
    | ni <= 0               = lx
    | isLengthMin ni lrx = x : dropEnd ni lrx
    | otherwise             = []

-- splitAt
-- | ...splits a list into a tupple of elements before and after the given position.
{- |
* will not evaluate the complete list, 
    but at least as many as needed to evaluate the result
* to the right means towards tail of the list
-}
splitAt 
    -- | position, whereas 0 means left most position
    :: Integer
    -- | list to split
    -> [a]
    -- | tupple of split parts, left most and right most
    -> ([a], [a])
splitAt n lx = splitAt' n ([], lx)
    where
        splitAt' :: Integer -> ([a], [a]) -> ([a], [a])
        splitAt' _ (xAcc, []) = (reverse xAcc, [])
        splitAt' n' (xAcc, lx'@(x:lrx))
            | n' > 0    = splitAt' (n' - 1) (x : xAcc, lrx)
            | otherwise = (reverse xAcc, lx')

-- isLengthMin
-- | ...whether there is at least the given amount of elements available.
{- |
* will not evaluate the complete list, 
    but at least as many as needed to evaluate the result
* to the right means towards tail of the list
-}
isLengthMin :: Integer -> [a] -> Bool
isLengthMin ni [] = ni <= 0
isLengthMin ni (_:lrx)
    | ni <= 0   = True
    | otherwise = isLengthMin (ni - 1) lrx

-- shiftR
-- | ...shifts elements of a list to the right.
{- |
* keeps the amount of elements in the list
* to the right means towards tail of the list
-}
shiftR :: a -> Integer -> [a] -> [a]
shiftR xDflt ni lx = Sfr.pad xDflt (Sfr.niLen lx) (dropEnd ni lx)

-- mListConcat
-- | ...'Just' a concatenateed list in an outer list, or 'Nothing' if the outer list is empty.
{- |
* keeps the order
-}
mListConcat :: [[a]] -> Maybe [a]
mListConcat [] = Nothing
mListConcat (lx:rllx) = Just (lx ++ Sfr.ifJust (mListConcat rllx) id [])

-- isLongAtLeast
-- | ...whether the amount of elements are available at least
{- |
* keeps the order
-}
isLongAtLeast :: Integer -> [a] -> Bool
isLongAtLeast ni [] = ni <= 0
isLongAtLeast ni (_:lrx)
    | ni <= 0   = True
    | otherwise = isLongAtLeast (ni - 1) lrx

-- doesMatchBegin
-- | ...whether a list of elements equals the first elements of another list
{- |
* if the list to be checked is smaller than the list of elements to be checked
    then the the list to be checked is not considered a match
-}
doesMatchBegin 
    :: Eq a 
    -- | the list of elements that have to be in the list to check
    => [a] 
    -- | the list to be checked, where elements have to be the same
   -> [a] 
    -- | True if it matches otherwise False
    -> Bool
doesMatchBegin sPattern sContent
        | isLengthMin (Sfr.niLen sPattern) sContent = sContentToCompare == sPattern
        | otherwise                                 = False
    where
        (sContentToCompare, _) = splitAt (Sfr.niLen sPattern) sContent
