
{-|
Description : provides helper functions to execute shell commands.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2023
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides advanced helper functions to execute shell commands.

Suggested import line: 'import qualified ShellExecute as Sh'

-}


module ShellExecute
    (
        runCommandWithIO
    ) where


import qualified System.Process as Proc
import qualified Control.Concurrent as Conc
import qualified System.IO as SyIO
import qualified System.Exit as Sys

import Data.Functor ( (<&>) )
import Control.Monad ( void )

import qualified Safer as Sfr


runCommandWithIO :: Maybe FilePath -> Maybe FilePath -> String -> [String] -> Maybe String -> IO (Maybe (String, String, Int))
runCommandWithIO msShell msWorkingDirectory sCommand lsArgs msInputToPassAsStdIn = 
    do
        mHandles <- runInteractiveCommandInDir msShell msWorkingDirectory sCommand lsArgs
        case mHandles of
            Just (hInput, hOutput, hError, hProcess) ->
                do
                    Sfr.ifJust msInputToPassAsStdIn (forkIO' . SyIO.hPutStr hInput) (return ())
                    mVarStdOut <- Conc.newEmptyMVar
                    mVarStdErr <- Conc.newEmptyMVar
                    forkIO' (hOutput `hGetContentsStrictlyAnd` Conc.putMVar mVarStdOut)
                    forkIO' (hError `hGetContentsStrictlyAnd` Conc.putMVar mVarStdErr)
                    exitCode <- Proc.waitForProcess hProcess <&> fromExitCode
                    sStdOut <- Conc.takeMVar mVarStdOut
                    sStdErr <- Conc.takeMVar mVarStdErr
                    return (Just (sStdOut, sStdErr, exitCode))
            _                                           ->
                do
                    putStrLn "Error: Function 'runInteractiveCommandInDir' failed to create handles."
                    return Nothing
    where
        forkIO' :: IO () -> IO ()
        forkIO' io = void (Conc.forkIO io)

fromExitCode :: Sys.ExitCode -> Int
fromExitCode Sys.ExitSuccess = 0
fromExitCode (Sys.ExitFailure exitCode) = exitCode

runInteractiveCommandInDir :: Maybe FilePath -> Maybe FilePath -> String -> [String] -> IO (Maybe (SyIO.Handle, SyIO.Handle, SyIO.Handle, Proc.ProcessHandle))
runInteractiveCommandInDir msShell msWorkingDirectory sCommand lsArgs = do
        (mhStdIn, mhStdOut, mhStdErr, hProcess) <-
            Proc.createProcess $
                processSpec
                    { Proc.cwd = msWorkingDirectory
                    , Proc.std_in  = Proc.CreatePipe
                    , Proc.std_out = Proc.CreatePipe
                    , Proc.std_err = Proc.CreatePipe }
        return
            (Sfr.ifJust3
                mhStdIn
                mhStdOut
                mhStdErr
                (\hStdIn hStdOut hStdErr -> Just (hStdIn, hStdOut, hStdErr, hProcess))
                Nothing)
    where
        processSpec = Sfr.ifJust msShell (\msShell' -> Proc.proc msShell' (["-c", sCommand] ++ lsArgs)) (Proc.proc sCommand lsArgs)

hGetContentsStrictlyAnd :: SyIO.Handle -> (String -> IO b) -> IO b
hGetContentsStrictlyAnd h f = SyIO.hGetContents h >>= \s -> length s `seq` f s
