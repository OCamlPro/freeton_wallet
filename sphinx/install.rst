How to install
==============

Using Docker
~~~~~~~~~~~~

Using :code:`ft` with Docker will work on Linux, Mac OS and Windows.

The latest image of :code:`ft` is available on:

https://hub.docker.com/r/ocamlpro/ft

You just need to copy the script (either for Linux/MacOS or Windows)
on your computer and run it!

There are a few known limitations:

* All files to which you refer should be in the current directory (or
  its subdirectories)

* You cannot update the utility tools (solc, tvm_linker, etc.) using
  :code:`ft init`

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
  $ opam install ft --destdir $HOME/local

to get :code:`ft` installed as :code:`$HOME/local/bin/ft`.

You can then either add :code:`$HOME/local/bin` to your :code:`PATH`
or copy the binary in another directory in your :code:`PATH`.

From Sources
------------

First, you need a switch with at least version :code:`4.10.0` of OCaml,
you can for example create it with::

  $ opam switch create 4.10.0
  
Second, checkout the sources of :code:`freeton_wallet` in a directory::

  $ git clone git@github.com:OCamlPro/freeton_wallet.git

Then, you need to install all the dependencies::

  $ cd freeton_wallet/
  $ opam repo add ocp git+https://github.com/OCamlPro/ocp-opam-repository
  $ opam install --deps-only ./ft.opam

Finally, you can build the package it::

  $ eval $(opam env)
  $ dune build

The :code:`ft` binary should be available in the top directory, you
can copy it somewhere in your :code:`PATH`.
  

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

Note that you only need the steps (2) and (3) if you plan to build and
deploy smart contracts.

FreeTON Crawler :code:`ft crawler`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The :code:`ft crawler` command requires a specific installation.
Indeed, this command crawls the blockchain and stores events for a
specific contract in the local PostgresQL database.

You will need to configure PostgresQL to use it::

     $ sudo -i -u postgres
     $ psql
     CREATE USER <user>;
     ALTER ROLE <user> CREATEDB;

The command :code:`ft crawler account` will create a database
:code:`account`. You can check it afterwards, for this example using
:code:`psql account`.
