# Changelog

All notable changes to this project will be documented in this file.

## [0.2.8] - 2025-06-20

### 🚀 Features

- Log severity

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.8

## [0.2.7] - 2025-06-16

### 🚀 Features

- Allow zip sources

### 🐛 Bug Fixes

- 301 should be logged with info level

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.7

## [0.2.6] - 2025-06-14

### 🚀 Features

- Redirect / -> /indexes

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.6

## [0.2.5] - 2025-06-14

### 🚀 Features

- Index name in indexes table

### 🚜 Refactor

- Simplify logging inside main

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.5

## [0.2.4] - 2025-06-14

### 🚀 Features

- Favicon

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.4

## [0.2.3] - 2025-06-14

### 🚀 Features

- Table (handler) with indexes

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.3

## [0.2.2] - 2025-06-14

### 🚀 Features

- Clean temporary files on startup

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.2

## [0.2.1] - 2025-06-13

### 🐛 Bug Fixes

- Set buffering to line (no output in docker)

### 🚜 Refactor

- Move some functions from Utils to IndexHandler

### ⚙️ Miscellaneous Tasks

- Update to ghc984
- Bump version to 0.2.1

## [0.2.0] - 2025-05-06

### 🚀 Features

- Support multiple indexes

### 🚜 Refactor

- ReaderT

### ⚙️ Miscellaneous Tasks

- Bump version to 0.2.0

## [0.1.1] - 2025-04-24

### 🐛 Bug Fixes

- OpenFile -> openBinaryFile (cache)

### 📚 Documentation

- Address -> host

### ⚙️ Miscellaneous Tasks

- Bump version to 0.1.1

## [0.1.0] - 2025-04-23

### 🚀 Features

- Prokki
- Env, logging with monad stack (#4)

### 🐛 Bug Fixes

- Concurrent cache usage (temporary solution)
- Disable compression
- Do not send a request to index if package is cached

### 📚 Documentation

- Readme

### ⚙️ Miscellaneous Tasks

- Compress and replace one-liner
- *(ci)* Ormolu lint action
- *(ci)* Changelog for release

<!-- generated by git-cliff -->
