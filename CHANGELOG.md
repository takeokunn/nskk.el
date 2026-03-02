# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Breaking Changes

- **Cursor keys in conversion mode**: C-n/C-p and ↑↓ now commit the current 
  candidate and move the cursor, instead of navigating candidates. Use SPC/x 
  for candidate navigation (ddskk-compatible behavior).
  
  **Migration**: If you previously used C-n/C-p to navigate candidates, use 
  SPC (next candidate) and x (previous candidate) instead.

### Changed

- C-n/C-p/↑↓ in converting mode (▼): Now commits candidate then moves cursor
  (ddskk-compatible behavior)
