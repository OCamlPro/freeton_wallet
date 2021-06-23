all:
	mkdir -p dockerbuild/bin
	docker cp -L ft-build:/root/ft/bin/ dockerbuild/
	docker cp -L ft-build:/bin/freeton-wallet/ft dockerbuild/bin/
	docker build -t "ocamlpro/ft:latest" dockerbuild/

dev-create:
	docker run -d -it --name ft-build -v /var/run/docker.sock:/var/run/docker.sock frolvlad/alpine-glibc:latest

dev-start:
	docker start ft-build

dev:
	docker exec -it ft-build /bin/sh 

dev-stop:
	docker stop ft-build

dev-remove:
	docker rm ft-build

test:
	docker run -v /var/run/docker.sock:/var/run/docker.sock -v $$HOME/.ft:/user/.ft --user 1000:1000 ocamlpro/ft /bin/ft account

push:
	docker push ocamlpro/ft:latest
