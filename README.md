Implementation of a Pratt parser for a simple arithmetic language (supports `+ * / ^`, grouping via parentheses, and a `zero-huh?` operator (ternary `<X> ? <Y> : <Z>`) that branches to the true case if `<X> == 0`.

#### Examples
```racket
-2 ^ 2 ^ 3 + 10 ; = 266
(10 - 5 - 4) ? (2 * 5) : (0 ? 100 : 200) ; = 100
-((1 + 1) ? 1 : 2 ^ 3) * 5 - (1 ? 10 : 20) ; = -45
```

#### References
[Top-Down operator precedence (Pratt) parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/)
