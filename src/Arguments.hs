{-|
Description : provides standard functions to interpret program arguments.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2023
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides standard functions to interpret program arguments.

-}


module Arguments
    (
        standardInterpretation
    ) where


import qualified BuildInfo


standardInterpretation :: [String] -> IO [String]
standardInterpretation = standardInterpretation' []
    where
        standardInterpretation' :: [String] -> [String] -> IO [String]
        standardInterpretation' lsAcc [] = return $ reverse lsAcc
        standardInterpretation' lsAcc ("--version":lrs) = putStrLn (BuildInfo.name ++ ", version " ++ BuildInfo.version) >> standardInterpretation' lsAcc lrs
        standardInterpretation' lsAcc (sArg:lrs) = standardInterpretation' (sArg : lsAcc) lrs
