# Changelog for `pwnedk`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Changed

- partly prepare for package upload

## 2.0.0 - 2024-01-14

### Added

- CLI switch "-i" for interactive guidance, but without echo when entering passowrd nor output of any kind
- CLI switch "-ie" for interactive guidance, with echo when entering passowrd and printing the entered password
- CLI switch "-r" to define record pattern, for non-interactive mode
- CLI switch "-p" to define field pick order, for non-interactive mode
- CLI switch "-l" to display only leaked passwords, for non-interactive mode
- the number of data sets of leaked passwords is printed, if one record has been found
- example data to check, an example password database ('KeePass', master password: 1234), a CSV export of the database
- build time on switch "--version"
- several haddock comments

### Fixed

- - none -

### Changed

- Default mode is not interactive and line by line from stdin.
- Default output is in the format <Reference>:<SymbolOrNumberOfAppearences>, 
  whereas <Reference> is defined by field pick order
- resolver in file 'stack.yaml'
- many things regarding design

### Removed

- CLI switch "-nv" for non-interactive mode is obsolete since this is the default mode

## 1.0.0.0 - 2023-11-20

### Added

- Complete initial functional software, manually tested

### Fixed

- - none -

### Changed

- - none -

### Removed

- - none -
