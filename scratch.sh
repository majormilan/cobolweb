#!/bin/bash

docker rm -f cobolka
docker build -t cobol-webserver . --no-cache
docker run -d -p 8080:8080 -v ${PWD}/www:/www --name cobolka cobol-webserver

curl http://localhost:8080
