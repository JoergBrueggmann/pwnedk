{-|
Description : provides advanced helper functions for 'String's.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2023
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides advanced helper functions for 'String's.

See also more basic list functions in module Safer.

Suggested import line: 'import qualified String as S'

-}


module String
    (
        lines
    ) where


import Prelude hiding (lines)


-- lines
{-| ...to split lines into a list of lines without line delimiter.
* automatically takes the following as a single line break
    * CR LF
    * CR
    * LF
-}
lines :: String -> [String]
lines = lines' ""
    where
        lines' :: String -> String -> [String]
        lines' "" "" = []
        lines' sAcc [] = [reverse sAcc]
        lines' sAcc ('\r':'\n':lrLines) = reverse sAcc : lines' "" lrLines
        lines' sAcc ('\r':lrLines) = reverse sAcc : lines' "" lrLines
        lines' sAcc ('\n':lrLines) = reverse sAcc : lines' "" lrLines
        lines' sAcc (ch:[]) = lines' (ch : sAcc) ""
        lines' sAcc (ch:lrLines) = lines' (ch : sAcc) lrLines
