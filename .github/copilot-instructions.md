# ForecastPDRB64 Workspace Instructions

## Purpose
This repository is a small R package and companion forecasting script for PDRB forecasting. Use these instructions to help contributors understand the project structure, common tasks, and how to make safe changes.

## Project structure
- `DESCRIPTION` — package metadata
- `NAMESPACE` — exported functions and package namespace
- `R/ESTIMATE.R` — main package logic, including data loading, forecasting, export, and output display
- `man/` — documentation files for exported functions
- `Forecasting PDRB64.R` — example driver script for package installation and forecast workflow
- `README.md` — user-facing setup and usage instructions

## What to do
- Keep changes compatible with an R package workflow
- Preserve the current workflow: package install, `package.check()`, `load.data.pdrb()`, `forecast.pdrb.64()`, `buka.hasil()`
- Improve reliability and portability for the core functions in `R/ESTIMATE.R`
- Keep README guidance accurate for end users
- When editing code, prefer clear, minimal fixes and document behavior in `README.md` or `man/`

## What not to do
- Do not add unrelated frameworks or tooling
- Do not rewrite the repository as a different application type
- Do not assume tests exist; there is no test suite in this repo
- Do not remove the existing package helper functions without a migration path

## Notes for AI assistance
- This repo is not large; changes should be local and easy to validate manually
- There is no CI configuration or automated test directory
- The package uses `openxlsx`, `forecast`, `tibble`, `ggplot2`, `mixOmics`, and `ggcats`
- `buka.hasil()` currently uses `shell.exec()`, which is a Windows-specific call and may need review for cross-platform compatibility

## Example prompts
- "Refactor `load.data.pdrb()` to accept a configurable Excel file path and add input validation."
- "Update the README to describe how to run this package from RStudio and what files the user must place in the template folders."
- "Review `R/ESTIMATE.R` for cross-platform compatibility issues and propose fixes for Linux/macOS support."

## Next agent customization suggestions
- Create an `agent-instructions` file targeted at package maintenance and documentation updates
- Add a `README`-focused prompt guide for improving user-facing instructions
- Add a `bug-fix` agent customization for diagnosing and fixing forecasting logic issues in `R/ESTIMATE.R`
