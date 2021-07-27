echo "
mkdir -p /var/chroot/$1/$(pwd)
mount --bind $(pwd) /var/chroot/$1/$(pwd)
cp /usr/bin/qemu-arm-static /var/chroot/$1/usr/bin
chroot /var/chroot/$1/ /bin/bash << EOF
  cd $(pwd)

  eval \\\$(opam env)
  dune build @install --profile static
EOF

umount /var/chroot/$1/$(pwd)
" > scripts/build_stanc3.sh

docker run --privileged --volumes-from=$(docker ps -q):rw andrjohns/stanc3-building:$1-debootstrap /bin/bash -x $(pwd)/scripts/build_stanc3.sh

rm scripts/build_stanc3.sh
