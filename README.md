# pwnedk

## Online *password checkers* - you don't *trust* them, do you?

The command line tool 'pwnedk' is available in source code, 
easily showing that your password in clear text will not be transferred to anyone.

Even the hashcode will not be transferred completely, 
because 'pwnedk' searches for leaked passwords applying *k-anonymity*!

**K-anonymity** means it does not send the password to check nor it sends its complete hash code. See also https://en.wikipedia.org/wiki/K-anonymity and https://www.youtube.com/watch?v=hhUb5iknVJs.

NOTE: It uses the API "Searching by range" from "haveibeenpwned.com" and the command "curl".
See also: https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange


This is donationware.


## Donations

Here: https://www.paypal.com/paypalme/sw4sd

Dont be below 3.90 $ to avoid unreasonable fee overhead.


**Thank you!**


## Synopsis

pwnedk [ option ]


## Options

       --help
              ...prints this help.

       -nv
              ...it will not output the complete text, but send the following text to stdout:
                     on ok,  no entries found: "0"
                     on NOT ok, entries found: "!"

       -ok <oksymbol>
              ...like plain but will replace the symbol for the ok case to <oksymbol>.

       -nok <noksymbol>
              ...like plain but will replace the symbol for the NOT ok case to <noksymbol>.


## Example 1

You want to check whether the password "alexguo029" is leaked, type the command below. An exclamation mark will indicate that the password is leaked.

### Command

       echo "alexguo029" | pwnedk -nv

### Output

       !


## Example 2

If you want to be guided and understand the whole process then type 'pwnedk' as indicated below, and follow the instruction to enter the password.

### Command

       pwnedk

### Output

       NAME
              pwnedk

       SYNOPSIS
              pwnedk [ option ]

       DESCRIPTION
              Searches for leaked passwords applying k-anonymity.
              K-anonymity means it does not send the password nor 
              it sends the complete hash code.

              NOTE: It uses the API "Searching by range" from "haveibeenpwned.com".
              See also: https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange

       Please, enter the password to check here:alexguo029

       The hash code of "alexguo029" is 21BD100D4F6E8FA6EECAD2A3AA415EEC418D38EC.
       The first 5 characters of the hash code are 21BD1.
       The command "curl https://api.pwnedpasswords.com/range/21BD1 is executed.

       The following entries have been found:
       00D4F6E8FA6EECAD2A3AA415EEC418D38EC:3

       Your password "alexguo029" is leaked!

       Do NOT use password "alexguo029"!

              :-(


## Questions?

### Q: Where do I see that pwnedk will not transfer the password in clear text and only a small portion of the hash-code?

### A: In file PwnedK.hs there are the following lines:

       let
              sha1Hash = hexStringFromPassword sPassword
              (first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash
              requestURL = 
                     "https://api.pwnedpasswords.com/range/" ++ first5FromHash

The line `sha1Hash = hexStringFromPassword sPassword` creates the hash code string.

The line `(first5FromHash, restFromHash) = Lst.splitAt 5 sha1Hash` splits the hash code string in 5 characters and the rest.

The line `"https://api.pwnedpasswords.com/range/" ++ first5FromHash` shows that only 5 characters are used to be send to the server.


## Runtime requirements

- The standard command line tool 'curl' is installed properly (with path variables set properly)
       E.g. The command line 'curl https://api.pwnedpasswords.com/range/BAC09' provides a reply to stdout.


## Build requirements

- Haskell Stack 2.11.1
- cabal 3.6.2.0
- GHC 9.2.8


## How to build 'pwnedk'?

- Install ghcup (https://www.haskell.org/ghcup/)
- Achieve the above mentioned 'Build requirements'

       1)a) on Windows by entering the following commands into a 'Command-line interface', CLI (cmd, bash, zsh), without the prompt (without '$')
              $ ghcup install stack 2.11.1
              $ ghcup set stack 2.11.1
              $ ghcup install cabal ...          # according to 'Build requirements'
              $ ghcup set cabal ...
              $ ghcup install ghc ...
              $ ghcup set ghc ...
       1)b) on Linux or Mac by entering
              $ ghcup tui                        # inside setting up in console

- Start build process via stack (The Haskell Tool Stack, https://docs.haskellstack.org/en/stable/)

       - change working directory to project directory 'pwnedk', using command line tool 'cd'
       - enter
              $ stack build


## How to install 'pwnedk' after build?

       - in CLI, enter
              $ stack install
       - you may have to set the environment path for the executable


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
