#!/usr/bin/env sh

die_with() {
  echo "[x] $1";
  #exit 1;
};

echo -e "\n# Network stuff";
# ln -s $(dirname $(dirname $(which lxc-create)))/libexec/lxc/lxc-net /usr/local/bin/lxc-net;
# lxc-net start \
#   && echo "- lxc-net started" \
#   || die_with "unable to start lxc-net";

address=10.0.5.0/24;
bridge=mvlan0;
ip link add $bridge link eth0 type macvlan mode bridge;
ip addr add $address dev $bridge;
ip link set mvlan0 up;
ip route add 10.0.5.1 dev $bridge;

# iptables -t nat -A POSTROUTING -s $address ! -d $address -j MASQUERADE;

# bridge=lxcbr0;
# ip link add name $bridge type bridge;
# ip link set $bridge up;
# ip link set eth0 master $bridge;

#iptables -D INPUT -i lxcbr0 -j REJECT
#iptables -D OUTPUT -o lxcbr0 -j DROP

distro=ubuntu
release=groovy

PREFETCH_CONTAINER=${PREFETCH_CONTAINER:-"__prefetch"};
ROOTFS=$(lxc-config lxc.lxcpath)/$PREFETCH_CONTAINER/rootfs;

run_lxc() { lxc-attach -n $PREFETCH_CONTAINER -- sh -c "$@"; }
copy_to_lxc() { local remote="${ROOTFS}$2"; rm -f $remote; cp $1 $remote; }

copy_files() {
  #copy_to_lxc ./netplan.yaml /etc/netplan/10-lxc.yaml;
  __not_implemented=1;
}

setup_container() {
  lxc-create -t download -n $PREFETCH_CONTAINER -- -d $distro -r $release -a amd64 \
    && echo "- created" \
    || die_with "couldn't create container";

  copy_files;

  ip a;
  ip r;

  lxc-start -n $PREFETCH_CONTAINER -d \
    && echo "- started" \
    || die_with "couldn't start container";
}

setup_lxc_network() {
  run_lxc 'ip route add default via 10.0.3.1 dev eth0' \
    && echo "- network route created" \
    || echo "! couldn't create network route";

  #run_lxc 'netplan apply || echo "failed netplan apply"';
}

echo -e "\n# Setting up prefetch container ($distro $release)";
setup_container && setup_lxc_network # && lxc-stop -n test;

# TODO: Setup prefetch container (installs and shit)

# Install build dependencies
#cd /opt/app/code;
#nix-shell shells/build.nix --run 'echo "INSTALLED"';

echo -e "\n-- Ready for shell... --";

tail -f /dev/null;

