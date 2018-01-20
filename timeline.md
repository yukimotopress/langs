---
title: Languages Timeline w/ Hello, World! Programs
---

_The program prints Hello, World! on the terminal, including a newline (`\n`) character._




### 1972  (46 years ago)

**C**  - Static, weak, manifest, nominal  ++ Imperative (procedural), structured

``` c
/* Hello world in C */

#include <stdio.h>

main() {
  printf("Hello, World!\n");
}
```


### 1985  (33 years ago)

**C++**  - Static, nominative, partially inferred ++ Multi-paradigm: procedural, functional, object-oriented, generic

``` cpp
// Hello world in C++

#include <iostream.h>

main() {
  cout << "Hello, World!" << endl;
}
```


### 1986  (32 years ago)

**Erlang**  - Dynamic, strong ++ Multi-paradigm: concurrent, functional, distributed, process-oriented

```erl
%% Hello world in Erlang

-module(hello).

-export([hello/0]).

hello() ->
   io:format("Hello, World!~n", []).
```


### 1987 (30 years ago)

**Perl** - Dynamic ++ Multi-paradigm: functional, imperative, object-oriented (class-based), reflective, procedural, event-driven, generic

``` perl
# Hello world in perl
print "Hello, World!\n";
```


### 1991 (26 years ago)

**Python** - Dynamic, strong, duck ++ Object-oriented, imperative, functional, procedural, reflective

``` python
# Hello world in Python
print("Hello, World!")
```


### 1993 (25 years ago)

**Lua**  - Dynamic, strong, duck ++ Multi-paradigm: scripting, imperative (procedural, prototype-based, object-oriented), functional

``` lua
# Hello world in Lua
print "Hello, World!"
```


### 1995 (23 years ago)

**Ruby** - Dynamic, strong, duck ++ Multi-paradigm: object-oriented, imperative, functional, reflective

``` ruby
# Hello World in Ruby
puts "Hello, World!"
```

**PHP**  - Dynamic, weak ++ Imperative, functional, object-oriented, procedural, reflective

``` php
<?php
  // Hello world in PHP
  echo 'Hello, World!';
?>
```


**Java** - Static, strong, safe, nominative, manifest ++ Multi-paradigm: object-oriented (class-based), structured, imperative, generic, reflective, concurrent

``` java
// Hello world in Java

class HelloWorld {
  static public void main( String args[] ) {
    System.out.println( "Hello, World!" );
  }
}
```

**JavaScript**  - Dynamic, duck ++ Multi-paradigm: object-oriented (prototype-based), imperative, functional, event-driven


``` js
// Hello world in JavaScript
console.log("Hello, World!");
```



### 2009 (9 years ago)

**Go** - Static, strong, inferred, structural ++ Compiled, concurrent, imperative, structured

``` go
// Hello world in Go

package main

import "fmt"

func main() {
  fmt.Println("Hello, World!")
}
```


### 2010 (8 years ago)

**Rust** - Static, strong, inferred, nominal, linear ++ Multi-paradigm: compiled, concurrent, functional, imperative, structured, generic

``` rs
// Hello world in Rust

fn main() {
    println!("Hello, World!");
}
```


### 2011 (7 years ago)

**Elixir (on Erlang)** - Dynamic, strong ++ Multi-paradigm: functional, concurrent, distributed, process-oriented   

``` ex
# Hello world in Elixir

defmodule HelloWorld do
  IO.puts "Hello, World!"
end
```

### 2014 (3 years ago)

**Crystal** - Static ++ Multi-paradigm: object-oriented

``` cs
# Hello world in Crystal
puts "Hello, World!"
```



### Hello, World! References

- [Hello, World! (Text Terminal Versions) @ Rosseta Code](http://rosettacode.org/wiki/Hello_world/Text)
- [Hello, World! Program @ Wikipedia](https://en.wikipedia.org/wiki/%22Hello,_World!%22_program)



### Typing Discipline

Dynamic vs Static (Manifest vs Inferred) -
Strong vs Weak -
Implicit (Structural/Duck) vs Explicit Interface/Protocol Declarations


| Lang                   | Typing Discipline                                                    |
|------------------------|----------------------------------------------------------------------|
| **Ruby**               | Dynamic, strong, duck                                                |
|                        | - vs -                                                               |
| **Perl**               | Dynamic                                                              |
| **Python**             | Dynamic, strong, duck                                                |
| **PHP**                | Dynamic, weak                                                        |
| **Lua**                | Dynamic, strong, duck                                                |
| **JavaScript**         | Dynamic, duck                                                        |
| **Erlang**             | Dynamic, strong                                                      |
| **Elixir (on Erlang)** | Dynamic, strong                                                      |
|                        |                                                                      |
| **C**                  | Static, weak, manifest, nominal                                      |
| **C++**                | Static, nominative, partially inferred                               |
| **Java**               | Static, strong, safe, nominative, manifest                           |
| **Go**                 | Static, strong, inferred, structural                                 |
| **Rust**               | Static, strong, inferred, nominal, linear                            |
| **Crystal**            | Static                                                               |



### Paradigms


| Lang                   | Object-Oriented  |  Functional  | Reflective | Concurrent |
|------------------------|------------------|--------------|------------|------------|
| **Ruby**               | Yes              | Yes (*)      | Yes        | X          |
|                        | - vs -           |         
| **Perl**               | Yes              | Yes (*)      | Yes        | X          |
| **Python**             | Yes              | Yes (*)      | Yes        | X          |
| **PHP**                | Yes              | Yes (*)      | Yes (*)    | X          |
| **Lua**                | Yes (*)          | Yes (*)      | Yes        | X          |
| **JavaScript**         | Yes (*)          | Yes (*)      | Yes        | X          |
| **Erlang**             | X                | Yes          | ?          | Yes        |
| **Elixir (on Erlang)** | X                | Yes          | Yes        | Yes        |
|                        |                  |              |            |            |
| **C**                  | X                | X            | X          | X          |
| **C++**                | Yes              | Yes (*)      | X          | Yes (*)    |
| **Java**               | Yes              | Yes (*)      | Yes (*)    | Yes (*)    |
| **Go**                 | X                | Yes (*)      | X          | Yes        |
| **Rust**               | X                | Yes          | X          | Yes (*)    |
| **Crystal**            | Yes              | Yes (*)      | X          | Yes        |
