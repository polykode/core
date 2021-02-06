## Current
  - [X] Parse out code blocks
  - [X] Route stdout via file descriptors
  - [X] Route stderr via fd
  - [X] Fix privileged container issue
  - [X] Setup network management for containers
  - [X] Setup unit tests for container code
  - [X] Get LXC to run code blocks serially (js, bash)
  - [ ] Create modules
  - [ ] Work on [modules](#modules)
  - [ ] Finalize javascript [adapter](#adapter responsibilities)
  - [ ] Parallelize container pool creation

## Ideas
  - [ ] Isolated networks? (lxd . webserver) <-> (lxd . xmd server <-> (lxd . pool)))
  - [ ] Simpler api for same language? (Cons - inconsistent api)
  - [ ] Global metadata with comments or metadata table
  - [ ] Block hints with comments
  - [ ] Allow disabling stdout logging
  - [ ] Executable markdown code blocks
  - [ ] Interactions between multiple languages
  - [ ] XMD server + clients (code blocks)
  - [ ] Allow importing other markdown files as modules?
  - [ ] Caching?

## Dependencies
  - [X] Markdown parser https://hackage.haskell.org/package/cmark
  - [X] Container - https://hackage.haskell.org/package/lxc

## Metadata?
  - [ ] Dependencies/Dependency files (package.json, requirements.txt, etc)
  - [ ] Timeout (has to be less than container timeout)

## Container
  - [X] Execute inside lxc
  - [ ] Setup seccomp profile
  - [ ] Destroy and re-create images ones used `OR` standard cleanup?
  - [ ] Cleanup processes - `pkill -u user --signal SIGKILL`
  - [ ] Cleanup files - `find /home/user -user user -delete`
  - [ ] LXC image with all languages setup
  - [ ] Run xmd server inside lxc
  - [ ] Limit interactions with the network? (Wont be neccassary if the container is destroyed after use)
  - [ ] Mount as read-only with write access to `/home/{user}` and `/tmp` only
  - [ ] Limit total execution time
  - [ ] Limit max number of processes
  - [ ] Limit max number of files created/open
  - [ ] Create set of users with limited access
  - [ ] Create a pool of containers to optimize
  - [ ] Execution timeout (for the whole program)

## Modules
  - [ ] Create dependency map
  - [ ] Add boilerplate + additional required langauge specific parsing for modules

## Adapter responsibilities
  - Parsing
  - Transformation
  - Check if the required dependencies are satisfied
  - If dependency not present and you know how to install it, install it
  - Import data from xmd server
  - Make function calls to xmd server
  - Update global context (via xmd server)
  - Run

## Language adapters
  - [ ] Javascript
  - [ ] Typescript
  - [ ] Python
  - [ ] Reasonml
  - [ ] Scheme
  - [ ] Bash
  - [ ] C
  - [ ] Rust
  - [ ] Haskell

