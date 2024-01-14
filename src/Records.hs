{-|
Description : provides functions to parse records like CSV from strings.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Records' provides functions to parse records like CSV from strings.

Suggested import line: 'import qualified Records as Rcr'

-}

module Records
    (
        fromContent,
        nextFromContent,
        Config(..)
    ) where


import qualified Safer as Sfr
import qualified List as L


data Config =
    Config {
        -- | symbols to eat in the beginning of a line
        rrowBegin :: String,
        -- | symbols to seperate columns
        rcolSep :: String,
        -- | symbols to eat at the end of a line, 
        -- | a line break is implied and must not be in here
        rrowEnd :: String }
    deriving Show


-- fromContent
{- | ...parse records from a string.

Example:

    @
fromContent (Config "\"" "\",\"" "\"") "\\\"erste A\\\",\\\"zweite A\\\"\\n\\\"erste B\\\",\\\"zweite B\\\""
    @

Output:

    @
[["erste A","zweite A"],["erste B","zweite B"]]
    @
-}
fromContent :: Config -> String -> [[String]]
fromContent = fromContent' []
    where
        fromContent' :: [[String]] -> Config -> String -> [[String]]
        fromContent' accu _ "" = accu
        fromContent' accu config sContent = nextRecord accu config sContent
        nextRecord :: [[String]] -> Config -> String -> [[String]]
        nextRecord accu config sContent =
            let
                (record,sRemaingContent) = nextFromContent config sContent
            in
                accu ++ fromContent' [record] config sRemaingContent

-- nextFromContent
{- | ...parse a record from a string.

Example:

    @
nextFromContent (Config "\"" "\",\"" "\"") "\\\"erste\\\",\\\"zweite\\\""
    @

Output:

    @
        (["erste","zweite"],"")
    @
-}
nextFromContent :: Config -> String -> ([String], String)
nextFromContent = nextFromContent' []
    where
        nextFromContent' :: [String] -> Config -> String -> ([String], String)
        nextFromContent' (s:lrs) _ "" = ( s : lrs, "" )
        nextFromContent' [] config sContent
            | L.doesMatchBegin rowBegin sContent = nextFromContent' [""] config sRemaining
            | otherwise                          = nextFromContent' [""] config (sEaten ++ sRemaining)
            where
                rowBegin = rrowBegin config
                (sEaten, sRemaining) = L.splitAt (Sfr.niLen rowBegin) sContent
        nextFromContent' {-accu@-}(s:lrs) config sContent@(ch:_)
            | L.doesMatchBegin colSep             sContent = nextFromContent' ( [] : lrs ++ [s]  ) config (L.dropBegin nColSep sContent)
            | L.doesMatchBegin (rowEnd ++ "\r\n") sContent = ( lrs ++ [s], L.dropBegin (nRowEnd + 2) sContent )
            | L.doesMatchBegin (rowEnd ++ "\r")   sContent = ( lrs ++ [s], L.dropBegin (nRowEnd + 1) sContent )
            | L.doesMatchBegin (rowEnd ++ "\n")   sContent = ( lrs ++ [s], L.dropBegin (nRowEnd + 1) sContent )
            | rowEnd == sContent                           = ( lrs ++ [s], "" )
            | otherwise                                    = nextFromContent' ((s ++ [ch]):lrs) config (L.dropBegin 1 sContent)
            where
                colSep = rcolSep config
                nColSep = Sfr.niLen colSep
                rowEnd = rrowEnd config
                nRowEnd = Sfr.niLen rowEnd
