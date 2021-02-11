# FFI Syntax

## Strategy

### HTTP

- `GET /uffi/variable/{var}?exec_id=1` - `{ "status": "success", "value": "hello" }`
- `POST /uffi/variable/{var}?exec_id=1` - `request { "value": "hello" }`
- `GET /uffi/call/{module}/{fn}?exec_id=1&args=[1,2,3]` - `{ "status": "success", "value": "hello" }`


## Languages

### JS

<!--@ (module foobar) -->
```js
export const fn1 = s => console.log(s);
export const fn2 = (a, b) => a + b;

context.mynum = 1;
```

Glue - module (for export)
```js
const foobar = require('./modules/foobar.js');

const returnValue = (key, result) =>
  fetch(`/uffi/function-return/${key}?exec_id=${execId}`, { method: 'POST', body: result });

if (typeof foobar[$$fnname] === 'function') {
  const result = foobar[$$fnname].apply(null, $$fnargs);
  returnValue($$fnname, result)
} else {
  throw new Error('Function not found')
}
```



```js
import { fn1, fn2 } from '@@foobar'
fn1("hello")
fn2(1, 2).then(console.log)

context.mynum.then(console.log)
```

Glue - main
```js
const execId = 1; // {{EXEC_ID}}

const readValue = key =>
  fetch(`/uffi/variable/${key}?exec_id=${execId}`)
    .then(r => r.json())
    .then(data => data.value);

const setValue = (key, value) =>
  fetch(`/uffi/variable/${key}?exec_id=${execId}`, { method: 'POST', body: JSON.stringify(value) });

const context = new Proxy({}, {
  get(k, _) => readValue(k),
  set(k, value, _) => setValue(k, value),
});

// If possible make it sync?
const createFn = (mod, fn) => (...args) =>
  fetch(`/uffi/call/${mod}/${fn}?exec_id=${execId}&args=${JSON.stringify(args)}`)
    .then(r => r.json())
    .then(data => data.value);

const fn1 = createFn('foobar', 'fn1');
const fn1 = createFn('foobar', 'fn1');
```

### Bash

<!--@ (module foobar) -->
```bash
fn1() { echo "$1"; }
fn2() { echo "$1 + $2" | bc; }

context mynum 2
```

Glue - module (for export)
```bash
source './modules/foobar.sh';

if (declare -f -F $fnname > /dev/null); then
  echo "Don't have ze function"
else
  foobar.$$fnname $${fnargs.join(' ')}
fi;
```


```bash
source '@@foobar'

call foobar.fn1 "hello"
echo $(call foobar.fn2 1 2)

context mynum
```

Glue - main
```bash
execId=1 // {{EXEC_ID}}
// baseUrl=...

context() {
  if [[ $# == 1 ]]; then
    curl "/uffi/variable/$1?exec_id=$execId" 2>/dev/null | jq ".value";
  else if [[ $# == 2 ]]; then
    curl -X POST "/uffi/variable/$1?exec_id=$execId" -d 'body=$2' >/dev/null 2>&1;
  else
    // Error
  fi;
}

call() {
  local modFn=$1; shift;
  // TODO: Split by . into module and fn
  curl "/uffi/call/$mod/$fn?exec_id=$execId&args=[$(join "," $@)]" 2>/dev/null | jq ".value"
}
```



### Python

<!--@ (module foobar) -->
```python
def fn1(s):
  print(s)

def fn2(a, b):
  return a + b

context.set('mynum', 5)
```

Glue - module (for export)
```python
import modules.foobar

# $$fnname(**$$fnargs)
```



```python
import '@@foobar'

fn1("hello")
print(fn2(1, 2))

context.get('mynum')
```

Glue - main
```python
execId=1 # {{EXEC_ID}}
# baseUrl=...

class Context(Object):
  def get(self, k, v):
    val = request.get("/uffi/variable/$k?exec_id=$execId")
    return json_decode(val).value
  def set(self, k, v):
    request.post("/uffi/variable/$k?exec_id=$execId", json_encode(v))

context = Context()


def fn1(**args):
  mod="foobar"
  fn="fn1"
  request.post("/uffi/call/$mod/$fn?exec_id=$execId&args=" + json_encode(args))
def fn2(**args):
  mod="foobar"
  fn="fn2"
  request.post("/uffi/call/$mod/$fn?exec_id=$execId&args=" + json_encode(args))
```



### Haskell

<!--@ (module foobar) -->
```haskell
fn1 = putStrLn
fn2 = (+)

// TODO: context?
set context "mynum" 20 -- :: IO ()
```

```haskell
#include "@@foobar"

// TODO: extern fn1?

fn1 "hello" -- :: IO ()
fn2 1 2 -- :: IO Int

-- Can you identify the type of variable on build?
-- Can have a simple type definition for context in metadata
get context "mynum" -- :: IO Int
```



### C

<!--@ (module foobar) -->
```c
void fn1(char* str) { printf("%s", str); }
int fn2(int a, int b) { return a + b; }

// TODO: context?
context_set("mynum", ??)
```

```c
#include "@@foobar"

// TODO: extern fn1?

fn1("hello")
fn2(1, 2)

context_get("mynum")
```


