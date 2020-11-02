# Using the Dockerfiles locally

First we need to build the docker image:  
`docker build -t debian-image -f docker/debian/Dockerfile .`  
`docker build -t static-image -f docker/static/Dockerfile .`

Then we can use that image to run our tests or builds like the following:

### Stancjs Tests:
`docker run -u root -w /stanc3 -v $(pwd):/stanc3 debian-image /bin/bash -c "eval \$(opam env); dune build @runjstest"`

### Build stanc.js:
`docker run -u root -w /stanc3 -v $(pwd):/stanc3 debian-image /bin/bash -c "eval \$(opam env); dune subst; dune build --profile release src/stancjs"`

### Build & test a static Linux binary:

In some cases like this one, we may need a longer script.
To do this, let's create a new script in the root of local `/stanc3` and name it `build-static.sh`

```
#!/bin/bash

eval $(opam env)

dune subst
dune build @install --profile static

time dune runtest --profile static --verbose
```

Make our script executable: `chmod +x build-static.sh`

Then we'll run the container:  
`docker run -u root -w /stanc3 -v $(pwd):/stanc3 static-image ./build-static.sh`


### Build & test static Windows binary:

In some cases like this one, we may need a longer script.
To do this, let's create a new script in the root of local `/stanc3` and name it `build-static.sh`

```
#!/bin/bash

eval $(opam env)

dune subst
dune build -x windows

time dune runtest --verbose
```

Make our script executable: `chmod +x build-static.sh`

Then we'll run the container:  
`docker run -u root -w /stanc3 -v $(pwd):/stanc3 static-image ./build-static.sh`

### Run TFP Tests:
`docker run -u root -w /stanc3 -v $(pwd):/stanc3 tensorflow/tensorflow@sha256:08901711826b185136886c7b8271b9fdbe86b8ccb598669781a1f5cb340184eb /bin/bash -c "pip3 install tfp-nightly==0.11.0.dev20200516; python3 test/integration/tfp/tests.py"`

Where:
- `-u root` We're running the container with root user in case it needs to write back to disk
- `-w /stanc3` Working directory inside the container when running
- `-v $(pwd):/stanc3` We're mounting current working directory ( pwd prints working directory ) to container path `/stanc3` ( Use '%cd%' for cmd, '${PWD}' for powershell )
- `debian-image` Docker image we want to run
- `/bin/bash -c "eval \$(opam env); dune build @runjstest"` What we want to run inside the container
- `/stanc3/build-static.sh` Script to be executed when container is starting