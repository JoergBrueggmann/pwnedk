# pwnedk

## Online *password checkers* - you don't *trust* them, do you?
The command line tool 'pwnedk' checks quasi anonymously (applying *k-anonymity*) whether a particular password is leaked.
It searches for leaked passwords applying *k-anonymity* via the API "Searching by range" of haveibeenpwned.com (HIBP).

It is available in source code, and easily showing that your password in clear text will not be transferred to anyone.

Even the hash code will not be transferred completely, because **K-anonymity** means it does not send the password to 
check nor it sends its complete hash code.
See also https://www.youtube.com/watch?v=hhUb5iknVJs, https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange and 
https://en.wikipedia.org/wiki/K-anonymity.

NOTE: It uses the command line tool "curl".


This is donation ware.


## Donations
Please, donate if you think that 'pwnedk' is useful to you or when you want to support software projects of *software 4sd*.

Here: https://www.paypal.com/paypalme/sw4sd

If you are going to donate, please don't be below 3.90 $ to avoid unreasonable fee overhead.

**Thank you!**


## Synopsis
       pwnedk [ --version ]
       pwnedk [ --help ]
       pwnedk [ -i | -ie ]
       pwnedk [ -r <RecordBegin> <FieldSeperator> <RecordBegin> ]
              [ -p <ReferenceColumn> <PasswordColumn> ]
              [ -l ]

## Options
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

## Example 1
To check which passwords in an CSV file are compromised (e.g. export from KeePass password database), 
  you can pipe the CSV file to pwnedk as follows.

### Command line (shell: bash)
    cat ./exampledata/Database.csv | awk 'NR>1' | pwnedk -p 1 3 -nok "ATTENTION: Password is leaked!"

IMPORTANT: If you use your real operational passwords, do NOT forget to delete the CSV-File!

### Output
    500px:0
    Adecco:0
    Adobe:0
    Sony:ATTENTION: Password is leaked!
    Linux Mint:0

## Example 2
If you want to be guided by pwnedk and understand the whole process then type 'pwnedk' as indicated below, and follow the instruction to enter the password.


### Command line (shell: bash)
    pwnedk -i

NOTE: The switch "-i" will suppress printing the entered password. Alternatively, the switch '-ie' will cause the password to appear in clear text. 
  See also Example 3.

### Output

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

        ...
        
    Please, enter the password to check here:
        The first 5 characters of the hash code are 7110E.
        The command "curl https://api.pwnedpasswords.com/range/7110E is executed.

    The following entries have been found:
        DA4D09E062AA5E4A390B0A572AC0D2C0220:1473205

        The number of found data sets of leaked passwords is 1473205.

        Your password is leaked!

        Do NOT use that password !

            :-(

## Example 3
Like Example 2, but printing the password. This helps to reinsure the password has been entered and used correctly.

### Command line (shell: bash)
    pwnedk -ie

NOTE: This example will print the entered password. Alternatively, the switch '-i' will cause the password NOT to appear in clear text. 
  See also Example 2.

### Output

    NAME
        pwnedk

    SYNOPSIS
        pwnedk [ --version ]
        
        ...
        
        NOTE: It uses the command line tool "curl".

    Please, enter the password to check here:1234

        The hash code of "1234" is 7110EDA4D09E062AA5E4A390B0A572AC0D2C0220.
        The first 5 characters of the hash code are 7110E.
        The command "curl https://api.pwnedpasswords.com/range/7110E is executed.

    The following entries have been found:
        DA4D09E062AA5E4A390B0A572AC0D2C0220:1473205

        The number of found data sets of leaked passwords is 1473205.

        Your password "1234" is leaked!

        Do NOT use password "1234"!

            :-(

## Questions?

### Q: Where do I see that pwnedk will not transfer the password in clear text and only a small portion of the hash-code?

### A: In file PwnedKCore.hs there are the following lines:

    let
        sha1Hash = hexStringFromPassword sPassword
        (first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash
        requestURL =
            "https://api.pwnedpasswords.com/range/" ++ first5FromHash
    commandResult <-
        Sh.runCommandWithIO Nothing Nothing "curl" [requestURL] Nothing
    return (processResultFromHIBP sha1Hash first5FromHash restFromHash requestURL commandResult)

The line `sha1Hash = hexStringFromPassword sPassword` creates the hash code string.

The line `(first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash` splits the hash code string in 5 characters and the rest.

The line `"https://api.pwnedpasswords.com/range/" ++ first5FromHash` shows that only 5 characters are used to be send to the server.


## Runtime requirements

- The standard command line tool 'curl' is installed properly (with path variables set properly)
       E.g. The command line 'curl https://api.pwnedpasswords.com/range/BAC09' provides a reply to stdout.


## Build requirements

- GHCup 0.1.20.0
- Haskell Stack 2.13.1
- cabal 3.10.2.1
- GHC 9.4.8


## How to build 'pwnedk'?

* Install ghcup (https://www.haskell.org/ghcup/)
* Achieve the above mentioned 'Build requirements'

       1)a) on Windows by entering the following commands into a 'Command-line interface', CLI (cmd, bash, zsh), without the prompt (without '$')
              $ ghcup install stack 2.13.1
              $ ghcup set stack 2.13.1
              $ ghcup install cabal ...          # according to 'Build requirements'
              $ ghcup set cabal ...
              $ ghcup install ghc ...
              $ ghcup set ghc ...
       1)b) on Linux or Mac by entering
              $ ghcup tui                        # inside setting up in console

* Start build process via stack (The Haskell Tool Stack, https://docs.haskellstack.org/en/stable/) by:

    * changing working directory to project directory 'pwnedk', using command line tool 'cd'
    * enter:

            $ stack build


## How to create documentation?

* enter:

    $ stack haddock

## How to install 'pwnedk' after build?

* in CLI, enter
        
        $ stack install

* you may have to set the environment path for the executable

    * the path of the executable image is printed by 'stack install'


## Author

Written by Jörg Karl-Heinz Walter Brüggmann.


## Reporting bugs or suggestions

Here at GitHub.


## Copyright

    Copyright © 2023 Jörg Karl-Heinz Walter Brüggmann.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License in file 'LICENSE' 
    along with this program. If not, see <https://www.gnu.org/licenses/>.
