{-|
Description : provides the main function to execute pwnedk.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2023
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module Type provides advanced helper functions to execute shell commands.

Suggested import line: 'import qualified PwnedK as P'

-}


module PwnedK
    (
        mainFromArgs, 
        interpretArgs, 
        Settings(..)
    ) where

import qualified ShellExecute as Sh
import qualified String as S
import qualified Safer as Sfr
import qualified List as Lst

import qualified System.IO as Sys
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Char as Chr
import qualified Numeric as Num
import qualified Control.Monad as M


mainFromArgs :: [String] -> IO ()
mainFromArgs = mainFromInterpretedArgs . interpretArgs


data Settings = 
      Verbose                   -- | verbose, interactive mode
    | Plain OKString NOKString  -- | plain mode
    | Help                      -- | to print a help page

type OKString = String
type NOKString = String
type ErrorString = String

interpretArgs :: [String] -> Either ErrorString Settings
interpretArgs = interpretArgs' (Right Verbose)

interpretArgs' 
    :: Either ErrorString Settings 
    -> [String] 
    -> Either ErrorString Settings
interpretArgs' (Right Help) _ = Right Help
interpretArgs' (Right settings) [] = Right settings
interpretArgs' (Left sError) _ = Left sError
interpretArgs' _ ("--help":_) = Right Help
interpretArgs' _ ("-nv":lrsArgs) = 
    interpretArgs' (Right (Plain "0" "!")) lrsArgs
interpretArgs' (Right Verbose) ("-ok":sOK:lrsArgs) = 
    interpretArgs' (Right (Plain sOK "!")) lrsArgs
interpretArgs' (Right (Plain _ sNotOK)) ("-ok":sOK:lrsArgs) = 
    interpretArgs' (Right (Plain sOK sNotOK)) lrsArgs
interpretArgs' (Right Verbose) ("-nok":sNotOK:lrsArgs) = 
    interpretArgs' (Right (Plain "0" sNotOK)) lrsArgs
interpretArgs' (Right (Plain sOK _)) ("-nok":sNotOK:lrsArgs) = 
    interpretArgs' (Right (Plain sOK sNotOK)) lrsArgs
interpretArgs' (Right Verbose) (sUnknown:_) = 
    Left ("Error: Incomplete or unknown parameter \"" ++ sUnknown ++ "\".")
interpretArgs' (Right (Plain _ _)) (sUnknown:_) = 
    Left ("Error: Incomplete or unknown parameter \"" ++ sUnknown ++ "\".")

mainFromInterpretedArgs :: Either ErrorString Settings -> IO ()
mainFromInterpretedArgs (Left sErrorMessage) = 
    do
        putStrLn ""
        putStrLn sErrorMessage
        printIntroduction (Left sErrorMessage)
mainFromInterpretedArgs (Right Help) = printIntroduction (Right Help)
mainFromInterpretedArgs (Right settings) = mainFromSettings settings

mainFromSettings :: Settings -> IO ()
mainFromSettings settings = 
    do
        Sys.hSetBuffering Sys.stdout Sys.NoBuffering
        printIntroduction (Right settings)
        sPassword <- getPassword settings
        let
            sha1Hash = hexStringFromPassword sPassword
            (first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash
            requestURL = 
                "https://api.pwnedpasswords.com/range/" ++ first5FromHash
        printInfo settings sPassword sha1Hash first5FromHash
        commandResult <- 
            Sh.runCommandWithIO Nothing Nothing "curl" [requestURL] Nothing
        printExecutionInfo settings requestURL
        let
            hashCodeListResult = listFromPwned requestURL commandResult
        case hashCodeListResult of
            Right hashCodeList  -> 
                printSuccess settings sPassword restFromHash hashCodeList
            Left sError         -> 
                printFailure sError

printIntroduction :: Either ErrorString Settings -> IO ()
printIntroduction eitherSettingsOrError =
    do
        M.when (isNotPlain eitherSettingsOrError)
            (do
                putStrLn ""
                putStrLn "NAME"
                putStrLn "       pwnedk"
                putStrLn ""
                putStrLn "SYNOPSIS"
                putStrLn "       pwnedk [ option ]"
                putStrLn ""
                putStrLn "DESCRIPTION"
                putStrLn "       Searches for leaked passwords applying k-anonymity."
                putStrLn "       K-anonymity means it does not send the password nor "
                putStrLn "       it sends the complete hash code."
                putStrLn ""
                putStrLn "       NOTE: It uses the API \"Searching by range\" from \"haveibeenpwned.com\"."
                putStrLn "       See also: https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange"
                putStrLn "")
        M.when (isErrorOrHelp eitherSettingsOrError)
            (do
                putStrLn "       --help"
                putStrLn "              ...prints this help."
                putStrLn ""
                putStrLn "       --version"
                putStrLn "              ...prints the version of 'pwnedk'."
                putStrLn ""
                putStrLn "       -nv"
                putStrLn "              ...it will not output the complete text, but send "
                putStrLn "                     the following text to stdout:"
                putStrLn "                     on ok,  no entries found: \"0\""
                putStrLn "                     on NOT ok, entries found: \"!\""
                putStrLn ""
                putStrLn "       -ok <oksymbol>"
                putStrLn "              ...like plain but will replace the symbol for the ok case "
                putStrLn "                     to <oksymbol>."
                putStrLn ""
                putStrLn "       -nok <noksymbol>"
                putStrLn "              ...like plain but will replace the symbol for the NOT ok case "
                putStrLn "                     to <noksymbol>."
                putStrLn ""
                putStrLn "       Example:"
                putStrLn "              $ echo \"alexguo029\" | pwnedk -nv"
                putStrLn "              !"
                putStrLn ""
                putStrLn "AUTHOR"
                putStrLn "       Written by Jörg Karl-Heinz Walter Brüggmann."
                putStrLn ""
                putStrLn "REPORTING BUGS"
                putStrLn "       GitHub: <https://github.com/JoergBrueggmann>"
                putStrLn ""
                putStrLn "COPYRIGHT"
                putStrLn ""
                putStrLn "    Copyright © 2023 Jörg Karl-Heinz Walter Brüggmann. "
                putStrLn ""
                putStrLn "    This program is free software: you can redistribute it and/or modify"
                putStrLn "    it under the terms of the GNU General Public License as published by"
                putStrLn "    the Free Software Foundation, either version 3 of the License, or"
                putStrLn "    (at your option) any later version."
                putStrLn ""
                putStrLn "    This program is distributed in the hope that it will be useful,"
                putStrLn "    but WITHOUT ANY WARRANTY; without even the implied warranty of"
                putStrLn "    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
                putStrLn "    GNU General Public License for more details."
                putStrLn ""
                putStrLn "    You should have received a copy of the GNU General Public License in "
                putStrLn "    file 'LICENSE' along with this program. If not, see "
                putStrLn "    <https://www.gnu.org/licenses/>."
                putStrLn "")
    where
        isNotPlain (Right (Plain _ _)) = False
        isNotPlain (Right _) = True
        isNotPlain (Left _) = True
        isErrorOrHelp (Left _) = True
        isErrorOrHelp (Right Help) = True
        isErrorOrHelp _ = False

getPassword :: Settings -> IO String
getPassword Verbose =
    do
        putStr   "Please, enter the password to check here:"
        getLine
getPassword _ = getLine

printInfo :: Settings -> String -> String -> String -> IO ()
printInfo Verbose sPassword sha1Hash first5FromHash =
    do
        putStrLn ""
        putStrLn $ "    The hash code of \"" ++ sPassword ++ "\" is " ++ sha1Hash ++ "."
        putStrLn $ "    The first 5 characters of the hash code are " ++ first5FromHash ++ "."
printInfo _ _ _ _ = return ()

printExecutionInfo :: Settings -> String -> IO ()
printExecutionInfo Verbose requestURL = 
    do
        putStrLn $ "    The command \"curl " ++ requestURL ++ " is executed."
printExecutionInfo _ _ = return ()

printSuccess :: Settings -> String -> String -> [String] -> IO ()
printSuccess settings sPassword sSearchedCode hashCodeList = printEntries settings foundEntriesToPrint
    where
        searchList :: [(String, String)]
        searchList = map (Lst.splitAt 35) hashCodeList
        foundEntries :: [(String, String)]
        foundEntries = 
            filter (\(sHashCode, _) -> sSearchedCode == sHashCode) searchList
        foundEntriesToPrint :: [String]
        foundEntriesToPrint = map (uncurry (++)) foundEntries
        printEntries :: Settings -> [String] -> IO ()
        printEntries Help _ = return ()
        printEntries Verbose [] =
            do
                putStrLn   ""
                putStrLn   "    No entries have been found."
                putStrLn $ "    Your password \"" ++ sPassword ++ "\" is NOT reported."
                putStrLn   ""
                putStrLn $ "    Your password \"" ++ sPassword ++ "\" may be safe."
                putStrLn   ""
                putStrLn   "        :-)"
                putStrLn   ""
        printEntries (Plain sOK _) [] = putStrLn sOK
        printEntries Verbose entries =
            do
                putStrLn ""
                putStrLn "The following entries have been found:"
                printRemainingEntries entries
                putStrLn   ""
                putStrLn $ "    Your password \"" ++ sPassword ++ "\" is leaked!"
                putStrLn   ""
                putStrLn $ "    Do NOT use password \"" ++ sPassword ++ "\"!"
                putStrLn   ""
                putStrLn "          :-("
                putStrLn ""
        printEntries (Plain _ sNotOK) _ = putStrLn sNotOK
        printRemainingEntries [] = return ()
        printRemainingEntries (sEntry:lrsEntry) = putStrLn ("    " ++ sEntry) >> printRemainingEntries lrsEntry

printFailure :: String -> IO ()
printFailure = print

hexStringFromPassword :: String -> String
hexStringFromPassword  =
    map Chr.toUpper .
    concatMap (\n -> Sfr.pad '0' 2 (Num.showHex n "")) .
    BS.unpack .
    SHA1.hash .
    BS.pack .
    map (fromIntegral . Chr.ord)

listFromPwned :: String -> Maybe (String, String, Int) -> Either String [String]
listFromPwned _ (Just (sStdOut, _, 0)) = 
    Right ((S.lines . map Chr.toUpper) sStdOut)
listFromPwned sURL (Just (_, _, n)) = 
    Left 
        ("Error: Return code from command \"curl " ++ 
            sURL ++ 
            " is " ++ 
            show n ++ 
            ".")
listFromPwned sURL Nothing = 
    Left ("Error: Command \"curl " ++ sURL ++ "\" could not be executed.")
