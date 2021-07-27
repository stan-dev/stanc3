echo "
chroot /var/chroot/$1/ /bin/bash << EOF
  cd $(pwd)

  eval \$(opam env)
  dune build @install --profile static
EOF
" > scripts/build_stanc3.sh

mkdir -p /var/chroot/$1/$(pwd)
mount --bind $(pwd) /var/chroot/$1/$(pwd)

docker run --volumes-from=$(docker ps -q):rw andrjohns/stanc3-building:$1-debootstrap /bin/bash -x $(pwd)/scripts/build_stanc3.sh

rm scripts/build_stanc3.sh
