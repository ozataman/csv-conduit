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
