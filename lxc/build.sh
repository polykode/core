#!/usr/bin/env bash

CONTAINER="test"

ROOT="$HOME/.local/share/lxc/$CONTAINER/rootfs"

CONFIG_FILE="$(pwd)/lxc/config"

lxc-create \
  -t download -n test \
  -f $CONFIG_FILE \
  -- -d ubuntu -r bionic -a amd64;

sudo cp ./lxc/install-nix.sh "$ROOT/home/ubuntu"
sudo chmod 644 "$ROOT/home/ubuntu/install-nix.sh"
#sudo chown -R root:root "$ROOT/home/ubuntu/install-nix.sh"

lxc-start -n $CONTAINER --logfile ./logs/lxc-start.log -d

lxc-attach \
  -n $CONTAINER \
  --clear-env \
  -v 'PATH=$PATH:/bin:/usr/bin:/usr/local/bin' \
  -- sh -c "
    cd /home/ubuntu;
    apt-update;
    apt-get install curl wget build-essential pkg-config
    ls -l;
    bash ./install-nix.sh
  "

# TODO: Create image

lxc-stop -n $CONTAINER -k -W
lxc-destroy -n $CONTAINER

