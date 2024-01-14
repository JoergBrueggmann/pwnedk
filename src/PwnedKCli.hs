{-|
Description : provides the command line interface (CLI) to execute pwnedk.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'PwnedKCli' provides the CLI to execute pwnedk.

Suggested import line: 'import qualified PwnedKCli as Cli'


Overview:

    * the function 'mainFromArgs' provides the complete functionality from a list of arguments

        NOTE: The function 'mainFromArgs' will not be called to interpret the switch "--version".

    * the function 'mainFromArgs' uses the function 'interpretArgs' to create 'Either' a 'ErrorString' of an error message or the 'Settings'

    * the result 'Either ErrorString Settings' will be passed to 'mainFromInterpretedArgs'

    * in case the result keeps 'Settings' the 'Right' part function 'mainFromSettings' performs dependent on the sum type 'Settings'

        * 'Settings', and here on sum type 'Mode'

            * 'Interactive'

            * 'NonInteractive'

        * 'DontDoAnything'

        * 'Help'

    * this module imports and uses the module 'PwnedKCore' to retrieve information from the API https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange

-}


module PwnedKCli
    (
        mainFromArgs
    ) where


import qualified PwnedKCore as Cr
import qualified PwnedKTexts as Txt
import qualified Data as Dt
import qualified Records as Rcr

import qualified System.IO as Sys
import qualified System.IO.Echo as Sys
import qualified Control.Monad as M


data Settings =
      Settings Mode     -- | settings to execute pwnedk for its main purpose
    | DontDoAnything    -- | for the case the arguments forbid further execution
    | Help              -- | to print a help page

data Mode =
    -- | the user will be guided and promted to enter one password
      Interactive InteractiveSettings
    -- | multiple records of passwords and account ref. can be read from input
    | NonInteractive NonInteractiveSettings

newtype InteractiveSettings = InteractiveSettings {
        -- | whether the entering of the password is echoed, and not printed or submitted elsewhere
        rEcho :: Bool
      }

data NonInteractiveSettings = NonInteractiveSettings {
        -- | configuration to read records, e.g. a comma seperated value structure (CSV)
        rRecordsConfig :: Rcr.Config,
        -- | column assignement for reference and passsword 
        rColumnAssignment :: ColumnAssignment,
        -- | form of indication in case of OK (leak unknown)
        rOKSymbol :: FormOKIndication,
        -- | form of indication in case of NOT OK (leaked)
        rNotOKSymbol :: FormOKIndication,
        -- | whether only leaked records shall be reported
        rOnlyLeaked :: Bool
    }

data ColumnAssignment = ColumnAssignment { rIndexReferenceColumn :: Integer, rIndexPasswordColumn :: Integer }

data FormOKIndication = NumberIndicator | SymbolIndicator String
    deriving Show

type ErrorString = String


mainFromArgs :: Maybe [String] -> IO ()
mainFromArgs = mainFromInterpretedArgs . interpretArgs

interpretArgs :: Maybe [String] -> Either ErrorString Settings
interpretArgs Nothing = Right DontDoAnything
interpretArgs (Just lsArgs) =
    interpretArgs'
        (Right
            (Settings
                (NonInteractive (NonInteractiveSettings (Rcr.Config "\"" "\",\"" "\"") (ColumnAssignment 1 2) NumberIndicator NumberIndicator False))))
        lsArgs

interpretArgs'
    :: Either ErrorString Settings
    -> [String]
    -> Either ErrorString Settings
interpretArgs' (Right Help) _ = Right Help
interpretArgs' (Right settings@(Settings {})) [] = Right settings
interpretArgs' (Left sError) _ = Left sError
interpretArgs' _ ("--help":_) = Right Help
interpretArgs' (Right (Settings _)) ("-i":lrsArgs) =
    interpretArgs' 
        (Right (Settings (Interactive InteractiveSettings { rEcho = False }))) 
        lrsArgs
interpretArgs' (Right (Settings _)) ("-ie":lrsArgs) =
    interpretArgs' 
        (Right (Settings (
            Interactive InteractiveSettings{ rEcho = True }))) 
        lrsArgs
interpretArgs' 
        (Right (Settings (NonInteractive 
            nonInteractiveSettings@(NonInteractiveSettings {})) )) 
        ("-r":rowBegin:sFieldSeperator:rowEnd:lrsArgs) =
    interpretArgs' 
        (Right (Settings (NonInteractive 
            (nonInteractiveSettings { rRecordsConfig = Rcr.Config rowBegin sFieldSeperator rowEnd})))) 
        lrsArgs
interpretArgs' 
        (Right (Settings (NonInteractive nonInteractiveSettings@(NonInteractiveSettings {})) )) 
        ("-p":refCol:pwdCol:lrsArgs) =
    interpretArgs' 
        (Right (Settings (NonInteractive 
            (nonInteractiveSettings { rColumnAssignment = ColumnAssignment (read refCol) (read pwdCol) })))) lrsArgs
interpretArgs' 
        (Right (Settings (NonInteractive nonInteractiveSettings@(NonInteractiveSettings {})) )) 
        ("-ok":sOK:lrsArgs) =
    interpretArgs' 
        (Right (Settings (NonInteractive 
            (nonInteractiveSettings { rOKSymbol = SymbolIndicator sOK })))) lrsArgs
interpretArgs' 
        (Right (Settings (NonInteractive nonInteractiveSettings@(NonInteractiveSettings {})) )) 
        ("-nok":sNotOK:lrsArgs) =
    interpretArgs' 
        (Right (Settings (NonInteractive 
            (nonInteractiveSettings { rNotOKSymbol = SymbolIndicator sNotOK })))) lrsArgs
interpretArgs' 
        (Right (Settings (NonInteractive nonInteractiveSettings@(NonInteractiveSettings {})) )) 
        ("-l":lrsArgs) =
    interpretArgs' 
        (Right (Settings (NonInteractive 
            (nonInteractiveSettings { rOnlyLeaked = True })))) lrsArgs
interpretArgs' (Right _) lrsArgs = Left ("Unexpected parameter combination. Remaining parameters: " ++ showPrettyList "" lrsArgs)

    where
        showPrettyList :: String -> [String] -> String
        showPrettyList _ [] = ""
        showPrettyList sPreSeperator (sArg:lrsArg) = sPreSeperator ++ sArg ++ showPrettyList " " lrsArg

mainFromInterpretedArgs :: Either ErrorString Settings -> IO ()
mainFromInterpretedArgs (Left sErrorMessage) =
    do
        Txt.printDescription
        printFailure sErrorMessage
mainFromInterpretedArgs (Right settings) = mainFromSettings settings

mainFromSettings :: Settings -> IO ()
mainFromSettings settings@(Settings (Interactive (InteractiveSettings doWithEcho))) =
    do
        Sys.setInputEcho doWithEcho
        Sys.hSetBuffering Sys.stdout Sys.NoBuffering
        Txt.printShortDescription
        sEnteredPassword <- getPassword settings
        passwordProcessResult <- Cr.processPassword sEnteredPassword
        case passwordProcessResult of
            Right processSuccessRecord ->
                do
                    printInfo doWithEcho sEnteredPassword (Cr.rsha1Hash processSuccessRecord) (Cr.rfirst5FromHash processSuccessRecord)
                    printExecutionInfo (Cr.rrequestURL processSuccessRecord)
                    printSuccess doWithEcho sEnteredPassword (Cr.rfoundEntries processSuccessRecord)
            Left sError ->
                printFailure sError
mainFromSettings (Settings (NonInteractive nonInteractiveSettings@(NonInteractiveSettings {}))) =
    do
        Sys.setInputEcho False
        Sys.hSetBuffering Sys.stdout Sys.NoBuffering
        sContent <- getContents
        let
            records :: [[String]]
            records = Rcr.fromContent (rRecordsConfig nonInteractiveSettings) sContent
            pickOrder :: Dt.Alignment
            pickOrder =
                Dt.PickOrder
                    [
                        rIndexReferenceColumn (rColumnAssignment nonInteractiveSettings) - 1,
                        rIndexPasswordColumn (rColumnAssignment nonInteractiveSettings) - 1]
            listOfTuples :: [(String,String)]
            listOfTuples = map (Dt.tpl2FromList pickOrder) records
        passwordProcessResults <- Cr.processPasswords listOfTuples
        let
            passwordProcessResultsToPrint =
                if rOnlyLeaked nonInteractiveSettings
                    then filter leakedOrError passwordProcessResults
                    else passwordProcessResults
        printLineByLine passwordProcessResultsToPrint
    where
        leakedOrError :: Either (String,String) (String,String) -> Bool
        leakedOrError (Right (_,"0")) = False
        leakedOrError _ = True
        printLineByLine :: [ Either (String,String) ( String, String ) ] -> IO ()
        printLineByLine []              =
            return ()
        printLineByLine (Right (s1,s2):lrtpl) =
            putStr s1 >>
            putStr ":" >>
            putStrLn (indicator (rOKSymbol nonInteractiveSettings) (rNotOKSymbol nonInteractiveSettings) s2) >>
            printLineByLine lrtpl
        printLineByLine (Left (s1,sError):lrtpl) =
            putStr s1 >>
            putStr ":" >>
            putStrLn ("Error: " ++ sError) >>
            printLineByLine lrtpl
        indicator :: FormOKIndication -> FormOKIndication -> String -> String
        indicator NumberIndicator           _ "0" = "0"
        indicator (SymbolIndicator sSymbol) _ "0" = sSymbol
        indicator _ NumberIndicator           sNumber = sNumber
        indicator _ (SymbolIndicator sSymbol) _       = sSymbol
mainFromSettings DontDoAnything = return ()
mainFromSettings Help = Txt.printHelp

getPassword :: Settings -> IO String
getPassword (Settings (Interactive {})) =
    do
        putStr   "Please, enter the password to check here:"
        getLine
getPassword _ = getLine

printInfo :: Bool -> String -> String -> String -> IO ()
printInfo doWithEcho sPassword sha1Hash first5FromHash =
    do
        putStrLn ""
        M.when
            doWithEcho
            (putStrLn $ "    The hash code of \"" ++ sPassword ++ "\" is " ++ sha1Hash ++ ".")
        putStrLn $ "    The first 5 characters of the hash code are " ++ first5FromHash ++ "."

printExecutionInfo :: String -> IO ()
printExecutionInfo requestURL =
    do
        putStrLn $ "    The command \"curl " ++ requestURL ++ " is executed."

printSuccess :: Bool -> String -> [(String, String)] -> IO ()
printSuccess doWithEcho sPassword = printEntries
    where
        printEntries :: [(String, String)] -> IO ()
        printEntries [] =
            do
                putStrLn   ""
                putStrLn   "    No entries have been found."
                if doWithEcho
                    then
                        putStrLn $ "    Your password (\"" ++ sPassword ++ "\") is NOT reported."
                    else
                        putStrLn   "    Your password is NOT reported."
                putStrLn   ""
                if doWithEcho
                    then
                        putStrLn $ "    Your password (\"" ++ sPassword ++ "\") may be safe."
                    else
                        putStrLn   "    Your password may be safe."
                putStrLn   ""
                putStrLn   "        :-)"
                putStrLn   ""
        printEntries entries =
            do
                putStrLn ""
                putStrLn "The following entries have been found:"
                printRemainingEntries entries
                printNumberOfFoundDataSets entries
                putStrLn   ""
                if doWithEcho
                    then
                        putStrLn $ "    Your password \"" ++ sPassword ++ "\" is leaked!"
                    else
                        putStrLn   "    Your password is leaked!"
                putStrLn   ""
                if doWithEcho
                    then
                        putStrLn $ "    Do NOT use password \"" ++ sPassword ++ "\"!"
                    else
                        putStrLn   "    Do NOT use that password !"
                putStrLn   ""
                putStrLn "          :-("
                putStrLn ""
        printRemainingEntries :: [(String, String)] -> IO ()
        printRemainingEntries [] = return ()
        printRemainingEntries (entry:lrsentry) = putStrLn ("    " ++ foundEntriesToPrint entry) >> printRemainingEntries lrsentry
        printNumberOfFoundDataSets :: [(String, String)] -> IO ()
        printNumberOfFoundDataSets [(_,sNumberOfFoundDataSets)] =
            do
                putStrLn   ""
                putStrLn $ "    The number of found data sets of leaked passwords is " ++ sNumberOfFoundDataSets ++ "."
        printNumberOfFoundDataSets _       = return ()
        foundEntriesToPrint :: (String, String) -> String
        foundEntriesToPrint (sRestOfHash,sNumberOfFoundDataSets) = sRestOfHash ++ ":" ++ sNumberOfFoundDataSets

printFailure :: String -> IO ()
printFailure sErrorMessage =
    do
        putStrLn   ""
        putStrLn $ "    Error: " ++ sErrorMessage
        putStrLn   ""
