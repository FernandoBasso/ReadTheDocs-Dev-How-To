# Gotchas, Surprises and Pain Points | JavaScript

## JavaScript Is Fun

```bash
$ node -e 'console.log("JavaScript is", (![]+[])[+[]]+([][[]]+[])[+[]]+([][[]]+[])[+!![]]);'
JavaScript is fun
```

```javascript
console.log('JavaScript is', (![]+[])[+[]]+([][[]]+[])[+[]]+([][[]]+[])[+!![]]);
// JavaScript is fun
```
