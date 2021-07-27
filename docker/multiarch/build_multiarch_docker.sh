echo "
  FROM debian:bullseye-slim
  USER root
  
  RUN adduser opam --disabled-password

  RUN addgroup --gid=1004 jenkins-slave

  RUN delgroup opam nogroup

  RUN addgroup opam opam
  RUN addgroup opam jenkins-slave

  #Set our distro_style
  LABEL distro_style=\"apt\"

  #Install os dependencies
  RUN apt-get update && apt-get install binfmt-support qemu qemu-user-static debootstrap build-essential -y

  RUN mkdir -p /var/chroot/$1

  RUN debootstrap --arch $1 bullseye /var/chroot/$1 http://deb.debian.org/debian/

  RUN chroot /var/chroot/$1 /bin/bash -c \"\\
    apt-get update && \\
    apt-get install opam curl bzip2 git tar curl ca-certificates openssl m4 bash -y\"

  RUN chroot /var/chroot/$1 /bin/bash -c \"opam init --disable-sandboxing -y\"
  RUN chroot /var/chroot/$1 /bin/bash -c \"eval \\\$(opam env) && opam switch create 4.07.0\"
  RUN chroot /var/chroot/$1 /bin/bash -c \"eval \\\$(opam env) && opam switch 4.07.0\"
  RUN chroot /var/chroot/$1 /bin/bash -c \"eval \\\$(opam env) && opam repo add internet https://opam.ocaml.org\"

  COPY ./install_build_deps.sh ./

  RUN cp ./install_build_deps.sh /var/chroot/$1/install_build_deps.sh

  RUN chroot /var/chroot/$1 /bin/bash -x ./install_build_deps.sh

  USER opam
" > $1-dockerfile

cp scripts/install_build_deps.sh install_build_deps.sh

docker build -t andrjohns/stanc3-building:$1-debootstrap -f $1-dockerfile .
docker push andrjohns/stanc3-building:$1-debootstrap

rm $1-dockerfile
rm install_build_deps.sh
