# Changelog for `fp2024`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2024-10-29

### Added

- Option 'None' to \<branches\> DT. Now it is:  
\<branches>  ::= \<branch> | \<branch> \<branches> | None

### Changed

- Changed command trigger to plant forest from "plant" to "plant forest":  
"plant" \<forest> -> "plant forest" \<forest>

## 0.1.0.0 - 2024-12-02

### Added

- Support for batch commands using BEGIN ... END syntax.
- New commands: 'load' and 'save' to read/persist program's state from/to a file.
- Property tests for state marshalling.
- Separate file for Lib2.hs parsers.

### Deleted

- Deleted "plant forest" \<forest\> from \<plant_command\> and related commands and parsers.
