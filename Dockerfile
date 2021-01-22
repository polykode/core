FROM nixos/nix

WORKDIR /opt/app

# Maybe use stable channel instead? - https://channels.nixos.org/nixos-20.09
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update

COPY lxc/env.nix default.nix
RUN nix-shell --run 'echo INSTALLED'

COPY lxc/config /etc/lxc/default.conf
COPY lxc/net-config /etc/default/lxc
COPY lxc/netplan.yaml .
#COPY lxc/template .
#COPY lxc/interfaces /etc/networking/interfaces
#COPY lxc/build.sh .

RUN mkdir -p /var/lib/lxc/rootfs

ENV PATH /bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/root/.nix-profile/bin/:/root/.nix-profile/libexec

#ENV TINI_VERSION v0.19.0
#ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
#RUN chmod +x /tini
#ENTRYPOINT ["/tini", "--"]

COPY entrypoint.sh .
CMD ["nix-shell", "--run", "sh ./entrypoint.sh"]

