# 0.2.0

**New**
- Added `init!` macro to replace old buggy initialization functions.
- Added `test` procedural macro.

**Breaking Changes:**
- Merged initialization functions into single `initialize` function.
- Changed the search path, it now uses the `directories` crate for standardized config lookup locations.

**Bug fixes**
- Fixed a bug where the initialization routines would keep a strong reference to worker guards
  making it impossible to safely cleanup on guard drop or program exit.

**Notes**
- The project now includes some standard `.md` files and the `README.md` is generated to reflect the
  documentation as viewed on `docs.rs`.

# 0.1.0 (2024-08-24)

- Initial release.