{-|
Description : entry point of the command line application 'pwnedk'.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX
-}


import qualified Arguments
import qualified PwnedKCli as Cli

import qualified System.Environment as Sys


main :: IO ()
main = 
    Sys.getArgs >>= 
    Arguments.standardInterpretation >>= 
    Cli.mainFromArgs
