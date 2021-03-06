---
title: Factorial
---

Definitions

- The factorial of `0` (zero) is defined as being  `1` (unity).
- The **Factorial Function** of a positive integer, `n`, is defined as the product of the sequence: `n`, `n-1`, `n-2`, ...  `1`

Example

| n	  | n!                     | n	 | n!                     | n	  | n!                   |
|-----|------------------------|-----|------------------------|-----|----------------------|
| 0	  | 1                      | 10  | 3628800                | 20  | 2432902008176640000  |
| 1	  | 1                      | 11  | 39916800               | ... |
| 2	  | 2                      | 12  | 479001600              | 100 | 9.332621544 × 10^157 |
| 3   | 6                      | 13  | 6227020800             |
| 4	  | 24                     | 14	 | 87178291200            |
| 5	  | 120                    | 15  | 1307674368000          |
| 6	  | 720                    | 16	 | 20922789888000         |
| 7	  | 5040                   | 17	 | 355687428096000        |
| 8	  | 40320                  | 18	 | 6402373705728000       |
| 9	  | 362880                 | 19	 | 121645100408832000     |


Task

Write a function to return the factorial of a number.
Solutions can be recursive or iterative.




## Ruby

``` ruby
# Recursive
def factorial(n)
  n == 0 ? 1 : n * factorial(n - 1)
end

# Iterative with Range#each
def factorial(n)
  (2...n).each { |i| n *= i }
  n == 0 ? 1 : n
end

# Functional with Range#reduce
def factorial(n)
  (1..n).reduce(1){ |prod, i| prod * i }
end
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Ruby))



## Perl

``` perl
# Recursive
sub factorial
{
  my $n = shift;
  ($n == 0)? 1 : $n*factorial($n-1);
}

# Iterative using a .. range
sub factorial {
    my $r = 1;
    $r *= $_ for 1..shift;
    $r;
}

# Functional with reduce
use List::Util qw(reduce);
sub factorial
{
  my $n = shift;
  reduce { $a * $b } 1, 1 .. $n
}
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Perl))



## Python

``` python
# Recursive
def factorial(n):
    z=1
    if n>1:
        z=n*factorial(n-1)
    return z

# Iterative
def factorial(n):
    result = 1
    for i in range(1, n+1):
        result *= i
    return result

# Functional with reduce
from operator import mul
from functools import reduce

def factorial(n):
    return reduce(mul, range(1,n+1), 1)
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Python))



## PHP

``` php
<?php
// Recursive
function factorial($n) {
  if ($n < 0)  return 0;

  if ($n == 0) return 1;
  else         return $n * factorial($n-1);
}
?>

<?php
// Iterative
function factorial($n) {
  if ($n < 0)  return 0;

  $factorial = 1;
  for ($i = $n; $i >= 1; $i--) {
    $factorial = $factorial * $i;
  }
  return $factorial;
}
?>
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#PHP))


## Lua

``` lua
# Recursive
function fact(n)
  return n > 0 and n * fact(n-1) or 1
end

# Iterative
# ??
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Lua))



## JavaScript

``` js
// Recursive
var factorial = n => (n < 2) ? 1 : n * factorial(n - 1);

// Recursive (Classic)
function factorial(n) {
    return n < 2 ? 1 : n * factorial(n - 1);
}

// Iterative
function factorial(n) {
  var sum = 1;
  while (n > 1) {
    sum *= n;
    n--;
  }
  return sum;
}
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#JavaScript))



## Java

``` java
// Recursive
public static long factorial(int n) {
    return (n < 2) ? 1 : n * factorial(n - 1);
}

// Iterative
public static long fact(int n) {
    long result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Java))


## C

``` c
/* Recursive */
int factorial(int n) {
    return n == 0 ? 1 : n * factorial(n - 1);
}

/* Iterative */
int factorial(int n) {
    int result = 1;
    for (int i = 1; i <= n; ++i)
        result *= i;
    return result;
}
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#C))


## Go

``` go
// Recursive
func Factorial(n uint64)(result uint64) {
	if(n > 0) {
		result = n * Factorial(n-1)
		return result
	}
	return 1
}

// Iterative
// ??
```


## Rust

``` rs
// Recursive
fn factorial(n: u64) -> u64 {
    match n {
        0 => 1,
        _ => n * factorial(n-1)
    }
}

// Iterative
fn factorial(n: u64) -> u64 {
    (1..n+1).fold(1, |p, n| p*n)
}
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Rust))


## Erlang

``` erl
%% Recursive
factorial(1) -> 1;
factorial(N) -> N * factorial(N-1).

%% Functional with a fold
lists:foldl(fun(X,Y) -> X*Y end, 1, lists:seq(1,N)).
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Erlang))


## Elixir (on Erlang)

``` ex
# Recursive
def factorial(0), do: 1
def factorial(n) when n > 0, do: n * factorial(n - 1)

# Functional with reduce
def factorial(0), do: 1
def factorial(n) when n > 0, do: Enum.reduce(1..n, 1, &*/2)  
```

(Source: [Rosetta Code](http://rosettacode.org/wiki/Factorial#Elixir))


## Crystal

``` cs
# Recursive
def factorial(n)
  n == 0 ? 1 : n * factorial(n - 1)
end

# Iterative with Range#each
def factorial(n)
  (2...n).each { |i| n *= i }
  n == 0 ? 1 : n
end

# Functional with Range#reduce
def factorial(n)
  (1..n).reduce(1){ |prod, i| prod * i }
end
```

<!-- todo/fix - check if ruby version works for crystal as-is 1:1
      what needs to get changed ??
  -->




## References

- [Factorial @ Rosetta Code](http://rosettacode.org/wiki/Factorial)
- [Factorial @ Wikipedia](https://en.wikipedia.org/wiki/Factorial)
