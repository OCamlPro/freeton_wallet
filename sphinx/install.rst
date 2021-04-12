How to install
==============

Build and install
~~~~~~~~~~~~~~~~~

In the following, you will need to use the OCaml Package Manager
called :code:`opam`. It is available from
https://github.com/ocaml/opam (you can directly download binary
versions from the Releases section, just make sure to choose a version
above 2.0.0).

From OPAM
---------

For now, these packages are available from https://github.com/OCamlPro/ocp-opam-repository .

You can use::

  $ opam repo add ocp git+https://github.com/OCamlPro/ocp-opam-repository
  $ opam install ft

To use :code:`ft`, you can then either everytime use::

  $ eval `opam env`
  # ft is now in your PATH:
  $ ft --help

or copy it permanently in some other location (here, we use 4.10.0 to build)::

  $ sudo cp ~/.opam/4.10.0/bin/ft /usr/local/bin/ft
  $ ft --help

From Sources
------------

First, you need a switch with at least version :code:`4.10.0` of OCaml,
you can for example create it with::

  $ opam switch create 4.10.0
  
Second, checkout the sources of :code:`freeton_wallet` in a directory::

  $ git clone git@github.com:OCamlPro/freeton_wallet.git

Then, you need to install all the dependencies::

  $ cd freeton_wallet/
  $ opam install --deps-only .

Finally, you can build the package and install it::

  $ eval $(opam env)
  $ dune build
  $ dune install

Initial Configuration
~~~~~~~~~~~~~~~~~~~~~

To be able to use :code:`ft`, we will need some tools from TON-SDK
(:code:`tonos-cli`, :code:`tvm_linker`, :code:`solc` and
:code:`stdlib_sol.tvm`) to be installed in :code:`$HOME/.ft/bin/`.

For that, :code:`ft` provides a simple command that will download
their sources and build them, provided that you have a recent version
of Rust installed::

  $ ft init

If it fails, you may have to do it manually:

1. Follow the steps from https://github.com/tonlabs/tonos-cli, and copy
   :code:`tonos-cli` into  :code:`$HOME/.ft/bin/`

2. Follow the steps in https://github.com/tonlabs/TON-Solidity-Compiler,
   and copy the resulting files (:code:`solc` from :code:`build/solc` and 
   :code:`stdlib_sol.tvm` from :code:`lib/stdlib_sol.tvm`) into
   :code:`$HOME/.ft/bin/`

3. Follow the steps from https://github.com/tonlabs/TVM-linker, and copy
   :code:`tvm_linker/target/release/tvm_linker` into  :code:`$HOME/.ft/bin/`
