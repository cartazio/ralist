# 0.4.1.0

- **Bug fix**: `Data.RAList.Co` — off-by-one in all index operations (`!!`, `lookup`, `lookupM`, `lookupWithDefault`, `lookupCC`). Index formula was `length - n` instead of `length - 1 - n`.
- **Bug fix**: `Data.RAList.Co.adjust` — infinite recursion (called itself instead of `QRA.adjust`).
- **Bug fix**: `Data.RAList.Co.genericReplicate` — infinite recursion (called itself instead of `QRA.genericReplicate`).
- Widened dependency bounds for GHC 9.14 (base < 6, deepseq < 1.7, transformers < 0.7, hspec < 2.12).
- Added test coverage for `Data.RAList.Co` (35 new tests).

# 0.4.0.0

- Breaking change to api to make api flavors more consistent.

# 0.3.0.0

- Added `Data.RAList.Co` module for reversed (co-indexed) access lists.
- Added filter/catMaybe/wither family of operations.
- Added NFData/NFData1 instances.
- Added indexed traversal instances (FunctorWithIndex, FoldableWithIndex, TraversableWithIndex).

# 0.2.1.1

- Documentation tweaks.

# 0.2.1.0

- Various improvements.

# 0.2.0.0

- Major rework by Carter Schonwald.

# 0.1.0.0

- Initial release by Lennart Augustsson.
