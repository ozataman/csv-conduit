1.0.0.0
* Removed `return` from the `Monad` instance for `Parser`, and
  transfered its definition to `pure` in the `Applicative` instance of
  `Parser`. This was necessary to support GHC 9.6.4.
* Create new API to choose whether to handle empty CSV cells as empty
  strings or NULLs.
* Added imports that were removed from `Prelude` in GHC 9.6.4.
* Bumped the default Stack resolver to LTS-22.20.

0.7.3.0
* Add ordered versions of named records for consistent, controllable header column ordering. [PR 44](https://github.com/ozataman/csv-conduit/pull/44)
* Add support for GHC 9.0.1

0.7.2.0
* Remove some dependency upper bounds for forward compatibility.

0.7.1.0
* Add MonadFail instance for Parser. [PR 38](https://github.com/ozataman/csv-conduit/pull/38)

0.7.0.0
* BREAKING: Switch from partial Monoid instance on Parser to total Semigroup instance.
* Compatibility with GHC 8.4.x/base-4.11.1.0

0.6.8.1
* Fix documentation mistake in FromNamedRecord/ToNamedRecord examples.

0.6.8
* Haddocks improvements
* Fix inlining and specialization rules around formatDecimal
* Updates to permit newest conduit/resourcet packages

0.6.7
* Fix build for GHC 8.0.1
