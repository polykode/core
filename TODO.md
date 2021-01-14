## Current
  - [X] Parse out code blocks
  - [ ] Get LXC to run code blocks serially (only js)
  - [ ] Finalize javascript [adapter](#adapter responsibilities)
  - [ ] Work on [modules](#modules)

## Rough tasks
  - [ ] Meta/Hints parsing parsec
  - [ ] Executable markdown code blocks
  - [ ] Interactions between multiple languages
  - [ ] Communication?
    - [ ] server-client
  - [ ] Run section code upto a section?
  - [ ] Meta data syntax on top of document as comment

## Dependencies
  - [X] Markdown parser https://hackage.haskell.org/package/cmark
  - [ ] Container - https://hackage.haskell.org/package/lxc

## Modules
  - [ ] Create dependency map
  - [ ] Add boilerplate + additional required langauge specific parsing for modules

## Adapter responsibilities
  - Parsing
  - Transformation
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
