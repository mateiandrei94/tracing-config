# 0.2.2 (2025-04-24)

**New**
- Added `standard_error` writer.

**Notes**
- Fixed alignment in the emit macro
- Performed a `cargo update`

# 0.2.1 (2025-04-02)

**Bug fixes**
- Fixed a bug where the the program would panic in the sifting layer when constructing a file writer having
  as path input from span data containing file system characters that are not allowed.
  the file name and extension are now sanitized.
  The problem still exists as TODO code comment.

# 0.2.0 (2025-01-27)

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