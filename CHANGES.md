
## v0.4.4

* Add option --wait to ft call and ft multisig --transfer to wait for the
  thread of message to end

## v0.4.3

* Fix build without PostgresQL server, `ft crawler` always included now,
  package ft-crawler is deprecated
* Fix bug in `ft inspect --past`
* New substitution %{account:in-message:ACCOUNT:NANOTONS:METH:PARAMS}
  to create an in-message for 'tonos-cli debot invoke'
* Better Checking of call parameters in `ft call` against ABI

## v0.4.2

* Remove build dependency to a running PostgresQL server
* Fix documentation of subcommands
* Set contract (if known) from code_hash at `ft account`
* `ft contract --build/--import` create new versions of the contract XXX/NUM
* `ft init --code-hashes` to initialize a local database of code_hashes
    for embedded contracts
* `ft node --update`: docker pull a new version of TONOS SE
* `ft node --live`: open browser on the new block explorer included in TONOS SE
* New embedded contract GiverV2 for TONOS SE

## v0.4.1

* Add `--abis CONTRACT` to `ft inspect --past ACCOUNT` to be able to parse
  all messages using CONTRACT's ABI, even to other contracts
* Add contract versioning. When contracts are built or imported, they get
  an increasing number (X -> X/1, etc.)
* Use freeton_ocaml_sdk.0.3.1 to have support for TVC code and code_hash
   without using tvm_linker
* Add substitution `get-code-hash:SUBST` where SUBST returns a TVC filename
* Lookup code_hash during `ft account`, associate corresponding contract
  if known

## v0.4.0

* Add 'ft crawler ACCOUNT' to crawle transactions on ACCOUNT and store
  corresponding events in a PostgresQL database. Use --start/status/stop
  to create a crawler with a monitoring process.

## v0.1.0 ( 2021-05-14 )

* Initial commit
