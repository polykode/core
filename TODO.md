## Current
  - [X] Parse out code blocks
  - [X] Route stdout via file descriptors
  - [X] Route stderr via fd
  - [ ] Route stdin via fd (with custom prompts)
  - [ ] Get LXC to run code blocks serially (only js)
  - [ ] Finalize javascript [adapter](#adapter responsibilities)
  - [ ] Work on [modules](#modules)

## Rough tasks
  - [ ] Meta/Hints parsing parsec
  - [ ] Executable markdown code blocks
  - [ ] Interactions between multiple languages
  - [ ] XMD server + clients (code blocks)
  - [ ] Allow importing other markdown files
  - [ ] Meta data syntax on top of document as comment

## Dependencies
  - [X] Markdown parser https://hackage.haskell.org/package/cmark
  - [ ] Container - https://hackage.haskell.org/package/lxc

## Metadata?
  - [ ] Dependencies/Dependency files (package.json, requirements.txt, etc)
  - [ ] Timeout (has to be less than container timeout)

## Container
  - [ ] Execute inside lxc
  - [ ] Run xmd server inside lxc
  - [ ] Limit interactions with the network
  - [ ] Limit total execution time (to avoid fork bombs)
  - [ ] Create a pool of containers to optimize

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

## Clients
  - [ ] Web
    - [ ] Syntax highlighting
  - [ ] CLI
    - [ ] Syntax highlighting

## Maybe
