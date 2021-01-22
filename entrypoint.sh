#!/usr/bin/env bash

echo -e "\n-- Network stuff --";
# bridge=lxcbr0;
# ip link add name $bridge type bridge;
# ip link set $bridge up;
#ip link set eth0 master $bridge;
$(dirname $(dirname $(which lxc-create)))/libexec/lxc/lxc-net start;
iptables -t nat -A POSTROUTING -s 10.0.3.0/24 ! -d 10.0.3.0/24 -j MASQUERADE;

#nix-shell --run 'iptables -D INPUT -i lxcbr0 -j REJECT';
#nix-shell --run 'iptables -D OUTPUT -o lxcbr0 -j DROP';

CONTAINER=test;
ROOTFS=$(lxc-config lxc.lxcpath)/$CONTAINER/rootfs;

run_lxc() { lxc-attach -n $CONTAINER -- sh -c "$@"; }

setup_lxc_network() {
  echo -e "\n-- Setting up network inside container --";
  run_lxc 'ip route del default || true 2>/dev/null; ip route add default via 10.0.3.1 dev eth0';

  local netplan="$ROOTFS/etc/netplan/10-lxc.yaml";
  rm -f $netplan;
  cp ./netplan.yaml $netplan;
  run_lxc 'netplan apply || echo "failed"'
}

echo -e "\n-- Setting up prefetch container --";
lxc-create -t download -n $CONTAINER -- -d ubuntu -r hirsute -a amd64 && \
  lxc-start -n $CONTAINER -d && \
  setup_lxc_network && \
  echo "Done";
# Inside container => ip route del default || true 2>/dev/null; ip route add default via 10.0.3.1 dev eth0;
#nix-shell --run 'cp ./lxc/container/sysctl.conf $(lxc-config lxc.lxcpath)/$CONTAINER/rootfs/etc/sysctl.conf';

# TODO: Setup prefetch container (installs and shit)


echo -e "\n-- Ready for shell... --";
tail -f /dev/null;

