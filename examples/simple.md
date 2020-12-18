---
name: <string> Name of the user
age: <number> Age of the user
---

# Header

Some **bold** ~~stuff~~

## Section
Content

```javascript
const name = args.name || 'Mr. Dicken Bols';
const age = args.age || 18;

const fibo = n => n < 2 ? n : fibo(n - 1) + fibo(n - 2);

return {
  name,
  age,
  someFibo: Array.from({ length: 10 }, (_, i) => i).map(fibo),
  isAdult: age >= 18,
};
```


## New section
Some more stuff

```javascript
const { name, someFibo, isAdult } = context;

someFibo.forEach((n, i) => {
  console.log(`Fibo #${i} = ${n}`);
});

console.log(isAdult ? 'You good bro' : 'Fuck off titty sucker')
```


## Section 2
Content 2

```python
name = context['name']

print("This is the python block, %s" % name)
```

