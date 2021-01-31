#!/usr/bin/env bash

[[ $# != 2 ]] && echo "Invalid set of arguments" && exit 1;

IMAGE=ubuntu:20.04
CONTAINER_NAME=default-container
WORKDIR=/opt/app
PROFILE_NAME=default

profile() { echo "profile function must be defined in lxd file"; exit 1; }
build() { echo "build function must be defined in lxd file"; exit 1; }
run() { echo "run function must be defined in lxd file"; exit 1; }

# NOTE: Must define the following - 
# Functions: profile, build, run
source "$2";

load_profile() {
  echo "- Loading profile $PROFILE_NAME"
  lxc profile create $PROFILE_NAME 2>/dev/null
  profile | lxc profile edit $PROFILE_NAME
  echo "";
}

run_cmd() { lxc exec $CONTAINER_NAME -- "$@"; }

RUN() {
  echo "- RUN => $@";
  run_cmd "$@" || exit 1;
  echo "";
}

COPY() {
  echo "- COPY => $1 to $2";
  local targetPath=$(echo "$2" | sed "s|^\\.|$WORKDIR|g");
  run_cmd rm -rf $targetPath;
  lxc file push -p $1 ${CONTAINER_NAME}${targetPath} || exit 1;
  echo "";
}

case "$1" in
  create)
    load_profile;
    lxc stop $CONTAINER_NAME 2>/dev/null;
    lxc delete $CONTAINER_NAME 2>/dev/null;
    lxc launch $IMAGE $CONTAINER_NAME --profile $PROFILE_NAME;
    sleep 2;
    run_cmd mkdir -p $WORKDIR;
  ;;
  build) build ;;
  run) run ;;
esac;

