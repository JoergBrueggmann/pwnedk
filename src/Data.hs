
{-|
Description : provides functions to form general data structures like lists and tuples.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Data' provides functions to form general data structures like lists and tuples.

Suggested import line: 'import qualified Data as Dt'

-}


module Data (
        Alignment(..), 
        tpl2FromList
    ) where


import qualified List as Lst


data Alignment = 
    -- | elements are taken from left to right
      LeftAlign 
    -- | elements are taken from right to left
    | RightAlign 
    -- | elements are taken according to the index, whereas the index is 0 based
    | PickOrder [Integer]
    deriving Show

-- tpl2FromList
-- | ...creates a two elements tuple from a list of monoids.
{- |
* when list elements are not available, mempty will be taken

Example:

    @
print $ tpl2FromList LeftAlign [ "a", "b", "c" ]
print $ tpl2FromList LeftAlign [ "a", "b" ]
print $ tpl2FromList LeftAlign [ "a" ]
print $ tpl2FromList LeftAlign ([  ] :: [ String ])
print $ tpl2FromList RightAlign [ "a", "b", "c" ]
print $ tpl2FromList RightAlign [ "a", "b" ]
print $ tpl2FromList RightAlign [ "a" ]
print $ tpl2FromList RightAlign ([  ] :: [ String ])
print $ tpl2FromList (PickOrder [1,0]) [ "a", "b", "c" ]
    @

prints:

    @
("a","b")
("a","b")
("a","")
("","")
("b","c")
("a","b")
("","a")
("","")
("b","a")
    @

-}
tpl2FromList
    :: (Monoid a) 
    => Alignment
    -> [ a ]
    -> ( a, a )
tpl2FromList LeftAlign  ( e1 : e2 : _ )   = ( e1, e2 )
tpl2FromList LeftAlign  [ e1 ]            = ( e1, mempty )
tpl2FromList _          [  ]              = ( mempty, mempty )
tpl2FromList RightAlign [ e1, e2 ]        = ( e1, e2 )
tpl2FromList RightAlign [ e2 ]            = ( mempty, e2 )
tpl2FromList RightAlign le                = tpl2FromList RightAlign (Lst.takeEnd 2 le)
tpl2FromList (PickOrder (nIndex1:nIndex2:_)) le = ( Lst.atDef mempty le nIndex1, Lst.atDef mempty le nIndex2 )
tpl2FromList (PickOrder [nIndex1]) le = ( Lst.atDef mempty le nIndex1, mempty )
tpl2FromList (PickOrder []) _ = ( mempty , mempty )
