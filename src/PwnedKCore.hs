{-|
Description : provides functions to check whether a passowrd appears in the dataset of leaked accounts.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'PwnedKCore' provides functions to check whether a passowrd appears in the dataset of leaked accounts.

This version applies the following methods:

    * uses k-anonymity, see also https://en.wikipedia.org/wiki/K-anonymity
    * in particular it uses the API "passwords by range" of HIBP (https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange)

Suggested import line: 'import qualified PwnedKCore as Cr'

-}


module PwnedKCore
    (
        ProcessSuccessRecord(..),
        processPassword,
        processPasswords
    ) where


import qualified ShellExecute as Sh
import qualified String as S
import qualified Safer as Sfr
import qualified List as Lst
import qualified Data as Dt

import qualified Data.Char as Chr
import qualified Numeric as Num
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified System.IO.Unsafe as Sys


type PasswordProcessResult = Either String ProcessSuccessRecord

data ProcessSuccessRecord = ProcessSuccessRecord { rsha1Hash :: String, rfirst5FromHash :: String, rrestFromHash :: String, rrequestURL :: String, rfoundEntries :: [(String, String)] }

processPassword :: String -> IO PasswordProcessResult
processPassword sPassword =
    do
        let
            sha1Hash = hexStringFromPassword sPassword
            (first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash
            requestURL =
                "https://api.pwnedpasswords.com/range/" ++ first5FromHash
        commandResult <-
            Sh.runCommandWithIO Nothing Nothing "curl" [requestURL] Nothing
        return (processResultFromHIBP sha1Hash first5FromHash restFromHash requestURL commandResult)
    where
        processResultFromHIBP :: String -> String -> String -> String -> Maybe (String, String, Int) -> PasswordProcessResult
        processResultFromHIBP sha1Hash first5FromHash restFromHash requestURL (Just (sStdOut, _, 0)) =
            let
                sDataSetRecords = (S.lines . map Chr.toUpper) sStdOut
                sSearchList = map (Dt.tpl2FromList Dt.LeftAlign . Lst.splitOn ":") sDataSetRecords
                foundEntries = filter (\(sHashCode, _) -> restFromHash == sHashCode) sSearchList
            in
                Right (ProcessSuccessRecord sha1Hash first5FromHash restFromHash requestURL foundEntries)
        processResultFromHIBP _ _ _ requestURL (Just (_, _, n)) =
            Left
                ("Return code from command \"curl " ++
                    requestURL ++
                    " is " ++
                    show n ++
                    ".")
        processResultFromHIBP _ _ _ sURL Nothing =
            Left ("Command \"curl " ++ sURL ++ "\" could not be executed.")

processPasswords :: [(String,String)] -> IO [Either (String,String) (String,String)]
processPasswords [] = return []
processPasswords ((sReference,sPassword):lrtpl) =
    do
        passwordProcessResult <- processPassword sPassword
        passwordProcessResults <- Sys.unsafeInterleaveIO $ processPasswords lrtpl
        case passwordProcessResult of
            Right processSuccessRecord ->
                Sfr.ifJust (Sfr.mHead ( rfoundEntries processSuccessRecord ))
                    -- then
                    (\(_,sCountOfFoundDataSets) ->
                        do
                            return (Right (sReference,sCountOfFoundDataSets) : passwordProcessResults ))
                    -- else
                    (
                        do
                            return (Right (sReference,"0") : passwordProcessResults) )
            Left sError ->
                do
                    return ( Left (sReference,sError) : passwordProcessResults )

hexStringFromPassword :: String -> String
hexStringFromPassword  = 
    map Chr.toUpper .
    concatMap (\n -> Sfr.pad '0' 2 (Num.showHex n "")) .
    BS.unpack .
    SHA1.hash .
    BS.pack .
    map (fromIntegral . Chr.ord)
