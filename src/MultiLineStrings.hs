{-|
Description : provides a quasi quoter to define multi line strings.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'MultiLineStrings' provides a quasi quoter to define multi line strings.

Suggested import line: 'import qualified MultiLineStrings as Mls'

-}


{-# LANGUAGE TemplateHaskellQuotes #-}


module MultiLineStrings
    (
        s
    ) where


import qualified Language.Haskell.TH.Quote as THQ
import qualified Data.String as Str


-- s
{- | ...QuasiQuoter for a non-interpolating IsString literal. The pattern portion is undefined.

* can be used to define multiline strings embedded in haskell code

Example:

The following code defines a multi line string.

    @
{-# LANGUAGE QuasiQuotes #-}

...

import qualified MultiLineStrings as Mls

...

sourceCode = 
    [Mls.s|
main :: IO ()
main = putStrLn "Hello..."
|]
    @

The code equals:

    @
sourceCode = "main :: IO ()\nmain = putStrLn "Hello..."\n"
    @

-}
s :: THQ.QuasiQuoter
s = THQ.QuasiQuoter 
        ((\a -> [|Str.fromString a|]) . trimLeadingNewline . removeCRs)
        (error "Cannot use q as a pattern")
        (error "Cannot use q as a type")
        (error "Cannot use q as a dec")
    where
        removeCRs :: String -> String
        removeCRs = filter (/= '\r')
        trimLeadingNewline :: String -> String
        trimLeadingNewline ('\n':xs) = xs
        trimLeadingNewline xs = xs
