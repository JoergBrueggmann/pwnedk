{-|
Description : provides standard functions to interpret program arguments.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'Arguments' provides standard functions to interpret program arguments.

-}


module Arguments
    (
        standardInterpretation
    ) where


import qualified BuildInfo

-- standardInterpretation...
{- | ...interpretes an argument list as comming from 'System.Environment.getArgs' 
    to provide standard functions.

Implemented standard functions:

* switch "--version" prints build information

Behaviour:

* stops interpreting as soon as an argument is recognised and hence its function is executed
* collects and passes all arguments only if no argument has been recognised
-}
standardInterpretation :: [String] -> IO (Maybe [String])
standardInterpretation = standardInterpretation' []
    where
        standardInterpretation' :: [String] -> [String] -> IO (Maybe [String])
        standardInterpretation' lsAcc [] = 
            return $ Just (reverse lsAcc)
        standardInterpretation' _ ("--version":_) = 
            putStrLn
                (
                    BuildInfo.name ++ 
                    ", version: " ++ 
                    BuildInfo.version ++ 
                    ", built: " ++ 
                    BuildInfo.time) >> 
            return Nothing
        standardInterpretation' lsAcc (sArg:lrs) = 
            standardInterpretation' (sArg : lsAcc) lrs
