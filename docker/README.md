
## Using `ft` with Docker

This directory contains the script to be used to run `ft` using Docker:

* `ft` is the shell script for Linux and MacOS. Just run it as if it
  was `ft` and it will automatically download the image and run it.

* `ft.bat` is the bat file for Windows. Just run it as if it
  was `ft` and it will automatically download the image and run it.

Images are stored on:

https://hub.docker.com/r/ocamlpro/ft

## Generating images

The image is generated in 2 steps: (1) we start by creating a
container `ft-build`, we copy the sources inside and build them; (2)
we extract the binaries and generate a new image using a Dockerfile.

First, create the container and start it:
```
make dev-create
make dev-start
```

Then, start a prompt inside the container:
```
make dev
```

Inside the prompt, we will need to install all dependencies:
```
apk add --upgrade --no-cache make patch gcc curl coreutils musl-dev
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# choose 1
apk add opam
opam init -a -y --comp 4.10.0 --disable-sandboxing
apk add git
opam repo add ocp git+https://github.com/OCamlPro/ocp-opam-repository --set-default --all
apk add gmp-dev pkgconf libressl-dev
opam install ssl.0.5.9
```

Finally, we can build ft:
```
source $HOME/.cargo/env
opam install ft
cp -f $HOME/.opam/4.10.0/bin/ft /bin/ft
```

or manually:
```
source $HOME/.cargo/env
git clone https://github.com/OCamlPro/freeton_wallet
cd freeton_wallet
drom build --switch 4.10.0
cp -f ft /bin/ft
```

Finally, we can build all utilities:
```
FT_HOME=/root/ft ft init
```
