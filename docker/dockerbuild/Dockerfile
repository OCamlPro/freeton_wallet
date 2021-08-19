ARG BASE_TAG
FROM frolvlad/alpine-glibc:latest

RUN apk add gmp-dev pkgconf libressl-dev docker git
RUN mkdir /user
RUN chmod 777 /user

COPY bin/ft /bin/ft
COPY bin/solc /bin/solc
COPY bin/stdlib_sol.tvm /bin/stdlib_sol.tvm
COPY bin/tvm_linker /bin/tvm_linker
COPY bin/tonos-cli /bin/tonos-cli

ENTRYPOINT [ ]
CMD [ "/bin/ft" ]
