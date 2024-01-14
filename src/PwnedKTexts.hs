{-|
Description : provides text ressources and its print functions to inform the user about pwnedk.
Copyright   : (c) Jörg K.-H. W. Brüggmann, 2021-2024
License     : GPLv3+, see also file 'LICENSE' and 'README.md'
Maintainer  : info@joerg-brueggmann.de
Stability   : experimental
Portability : POSIX

The module 'PwnedKTexts' provides text ressources and its print functions to inform the user about pwnedk.

Suggested import line: 'import qualified PwnedK as Txt'

-}


{-# LANGUAGE QuasiQuotes #-}


module PwnedKTexts
    (
        printHelp, 
        printDescription, 
        printShortDescription, 
        printDescriptionDetails, 
        printFormalDetails
    ) where


import qualified MultiLineStrings as Mls


printHelp :: IO ()
printHelp = printShortDescription >> printDescriptionDetails >> printFormalDetails

printDescription :: IO ()
printDescription = printShortDescription >> printDescriptionDetails

printShortDescription :: IO ()
printShortDescription = putStr [Mls.s|

NAME
       pwnedk

SYNOPSIS
       pwnedk [ --version ]
       pwnedk [ --help ]
       pwnedk [ -i | -ie ]
       pwnedk [ -r <RecordBegin> <FieldSeperator> <RecordBegin> ]
              [ -p <ReferenceColumn> <PasswordColumn> ]
              [ -l ]

DESCRIPTION
       The command line tool 'pwnedk' checks quasi anonymously (applying 
         *k-anonymity*) whether a particular password is leaked.
       It searches for leaked passwords applying *k-anonymity* via the API 
       "Searching by range" of haveibeenpwned.com (HIBP).

       The tool is available in source code, and easily showing that your 
         password in clear text will not be transferred to anyone.

       Even the hash code will not be transferred completely, because 
       **K-anonymity** means it does not send the password to check nor it 
       sends its complete hash code.
       See also https://www.youtube.com/watch?v=hhUb5iknVJs, 
       https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange and 
       https://en.wikipedia.org/wiki/K-anonymity.

       NOTE: It uses the command line tool "curl".

|]

printDescriptionDetails :: IO ()
printDescriptionDetails = putStr [Mls.s|
       Modes (non-interactive / interactive)
              By default, this tool is in non-interactive mode, and expects 
                passwords according to a defined record pattern.
       Record Pattern
              By default, each record may be seperated by a pattern that equals 
                the CSV format.
              Each record starts with a <RecordBegin> (default: '"').
              Then, each field is seperated by the <FieldSeperator> (default: '","').
              Each record ends with a <RecordEnd> (default: '"').
              Afterwards, optionally a line break may seperate the records.
       Record Field Pick Order
              By default, the record field pick order is defined by column 
                numbers that start at one, which are:
                     <ReferenceColumn> (default: 1, first column) and 
                     <PasswordColumn> (default: 2, second column).
       Input Stream
              By default, the input will be taken from stdin and 
                if entered by console the input will NOT be echoed.
       Output Stream
              In non-interactive mode it prints each line with 
                an at symbol ('@'), 
                account reference, 
                a colon, 
                and the number of how many times it appears 
                in the data set of leaked passwords.
              The number of how many times it appears can be overwritten by 
                defined symbols (strings), which are
                <OKSymbol>
                <NOKSymbol>.
              See also switches "-ok" and "-nok"

       --version
              ...prints the version of 'pwnedk' all other parameters below will 
                be ignored.

       --help
              ...prints this help all other parameters below will be ignored.

       -i
              ...interactive mode without echo when entering the password.

       -ie
              ...interactive mode with echo when entering the password, and 
                with repeated password.

       -r <RecordBegin> <FieldSeperator> <RecordBegin>
              ...record pattern each line can have multiple records
                a password (right hand side), 
                whereas the <FieldSeperator> separates them.
                Each line of the output will be in the form 
                <AccountReference>:<NumberOfAppearences>

       -p <ReferenceColumn> <PasswordColumn>
              ...determines record field pick order.

       -l
              ...display only leaked passwords.

       -ok <OKSymbol>
              ...will replace the symbol for the ok case (0) by <OKSymbol>.

       -nok <NOKSymbol>
              ...will replace the number of appearences for the NOT ok case by 
                <NOKSymbol>.

       Example:
              $ echo "alexguo029" | pwnedk
              1:3
|]

printFormalDetails :: IO ()
printFormalDetails = putStr [Mls.s|
AUTHOR
       Written by Jörg Karl-Heinz Walter Brüggmann.

REPORTING BUGS
       GitHub: <https://github.com/JoergBrueggmann>

COPYRIGHT

    Copyright © 2023 Jörg Karl-Heinz Walter Brüggmann. 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License in 
    file 'LICENSE' along with this program. If not, see 
    <https://www.gnu.org/licenses/>.

|]
