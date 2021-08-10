@ECHO OFF

setlocal enableextensions
md %HOME%/.ft
endlocal

docker run %FT_DOCKER% -v /var/run/docker.sock:/var/run/docker.sock -v %HOME%:/user -v %cd%:/local --network host ocamlpro/ft:latest /bin/ft %*
