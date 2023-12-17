# pwnedk

The command line tool 'pwnedk' searches for leaked passwords applying k-anonymity.

K-anonymity means it does not send the password to check nor it sends its complete hash code.

NOTE: It uses the API "Searching by range" from "haveibeenpwned.com" and the command "curl".
See also: https://haveibeenpwned.com/API/v3#SearchingPwnedPasswordsByRange

If you want to understand k-anonymity the following sources are recommended:
- https://www.youtube.com/watch?v=hhUb5iknVJs
- https://en.wikipedia.org/wiki/K-anonymity


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


## Example

### Command
echo "alexguo029" | pwnedk -nv

### Output
!


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


## Repoerting bugs

GitHub: <https://github.com/JoergBrueggmann>


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
