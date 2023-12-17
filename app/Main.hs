{-|
Description : main function imported from module 'PwnedK'.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2023
License     : GPLv3+, see also section 'copyright' in file 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides main function imported from module 'PwnedK'.

-}


module Main (main) where


import qualified Arguments
import qualified PwnedK

import qualified System.Environment as Sys


main :: IO ()
main = 
    Sys.getArgs >>= 
    Arguments.standardInterpretation >>= 
    PwnedK.mainFromArgs
