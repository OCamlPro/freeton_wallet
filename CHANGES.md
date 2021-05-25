
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
