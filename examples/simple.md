# Simple JS modules
A simple demo of js modules in markdown


### Creating simple modules
Export functions just like it was any other js module

<!--@ (module timer) -->
```js
export const waitFor = delay => new Promise(res => setTimeout(res, delay));
```


### Computing and exporting values
Asynchronous, synchronous, its all the same man. Export values after some time

<!--@ (module main) -->
```js
import { waitFor } from '@@timer';

(async () => {
  await waitFor(1000);
  console.log("foobar");
})();

export const value = 200;
```


### Importing values from modules
This is where you start doing things. Any codeblock that doesn't have a `module` or `noop` annotation, will be executed.

```js
import { value } from '@@main';

console.log(value);
```


### Updating context
You can update the global context. This context will be available to read everywhere

```js
context.numbers = Array.from({ length: 20 }, Math.random);
context.a = 20;
context.b = "Hello world";
```


### Consuming context

<!--@
(dependencies
  (ramda 0.27.1)
)
-->
```js
import { sort } from 'rambda'
context.numbers.then(sort).then(console.log);
```


### Inline blocks

Remember those values we updated in the context? Here they are - `@@a` and `@@b`

Prefixing an inline code with `@@` will make it display a value with that name from the global context


