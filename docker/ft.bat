@ECHO OFF
docker run %FT_DOCKER% -v /var/run/docker.sock:/var/run/docker.sock -v %HOME%/.ft:/user/.ft -v %cd%:/local --network host ocamlpro/ft:latest /bin/ft %*
