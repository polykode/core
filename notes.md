## Issues/Solutions
- Created returns false? -> Maybe lxc not setup/container already exists
- Started returns false? -> `setfacl -m u:100000:x ~`
- ExitFailure 32512 (Path not set correctly) -> `PATH=$PATH:/bin:/usr/bin:/usr/local/bin`
- CLONE_NEWNS -> `docker run -it --privileged cotainer`
- Failed to receive the container state - `lxc.apparmor.allow_incomplete = 1`
- 
