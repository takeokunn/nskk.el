# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **AZIK y-prefix youon rows**: AZIK mode now supports standard romaji y-prefix
  youon sequences (ky, ry, ny, hy, my, gy, jy, by, py) in addition to the
  existing AZIK-specific g-substitution rows (kg, rg, etc.). AZIK extension
  keys apply to y-prefix sequences: e.g. `ryp` → りょう, `ryh` → りゅう,
  `ryz` → りゃん (DDSKK-compatible behavior).

### Breaking Changes

- **Cursor keys in conversion mode**: C-n/C-p and ââ now commit the current 
  candidate and move the cursor, instead of navigating candidates. Use SPC/x 
  for candidate navigation (ddskk-compatible behavior).
  
  **Migration**: If you previously used C-n/C-p to navigate candidates, use 
  SPC (next candidate) and x (previous candidate) instead.

### Changed

- C-n/C-p/ââ in converting mode (â¼): Now commits candidate then moves cursor
  (ddskk-compatible behavior)
