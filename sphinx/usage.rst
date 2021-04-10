How to use :code:`ft`
=====================

Creating a new Smart Contract
-----------------------------

If you have a file :code:`Contract.sol`, you can use::

  ft contract --build Contract.sol

to build it and register in the contract database of :code:`ft`.
:code:`ft` will perform the following calls::

  $HOME/.ft/bin/solc Contract.sol
  $HOME/.ft/bin/tvm_linker compile --abi-json Contract.abi.json Contract.code --lib ~/.ft/bin/stdlib_sol.tvm


So, you must make sure that :code:`solc` and :code:`tvm_linker` are
correctly installed for :code:`ft`:

1. Follow the steps in https://github.com/tonlabs/TON-Solidity-Compiler,
   and copy the resulting files (:code:`solc` from :code:`build/solc` and 
   :code:`stdlib_sol.tvm` from :code:`lib/stdlib_sol.tvm`) into
   :code:`$HOME/.ft/bin/`

2. Follow the steps from https://github.com/tonlabs/TVM-linker, and copy
   :code:`tvm_linker/target/release/tvm_linker` into  :code:`$HOME/.ft/bin/`
