---
name: <string> Name of the user
age: <number> Age of the user
---

# Header

Some **bold** ~~stuff~~

## JS Section 1
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

## Python section 1
Content 2

```python
def fact(n):
  if n <= 1:
    return 1
  return n * fact(n - 1)
  
return { 'fact10': fact(10) }
```


## JS section 2
Some more stuff

```javascript
const { name, someFibo, fact10, isAdult } = context;

console.log(`Factorial of 10 = ${fact10}`);

someFibo.forEach((n, i) => {
  console.log(`Fibo #${i} = ${n}`);
});

console.log(isAdult ? 'You good bro' : 'Fuck off titty sucker')
```


## Python section 2
Content 2

```python
name = context['name']

print("This is the python block, %s" % name)
```

