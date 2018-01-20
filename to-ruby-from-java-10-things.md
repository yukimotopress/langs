---
title: 10 Things Every Java Programmer Should Know About Ruby
---

_Note: This chapter was written by Jim Weirich._



## First Things First

I used to teach an after hours course in C programming for employees of a large manufacturing company.
It was pretty easy to tell what programming languages the students had used previously just
by looking at the style of C code they produced. It is certainly true that
"You can write FORTRAN in any language".

Java programmers investigating Ruby will find a language that look similar in many ways.
There are classes and modules, namespaces and scopes, instance variables and methods.
A Java programmer will feel quite at home in this Object Oriented language.

So the temptation will be to continue to program in a Java-style.
Sure, there are some things that are different (the lack of type declarations will probably
be the first thing that strikes them).
But nothing that can't be worked around with a little effort ... and they will miss a golden opportunity.


### Sapir-Whorf Hypothesis

The Sapir-Whorf Hypothesis theorizes that thoughts and behavior are determined
(or are at least partially influenced) by language. [...] To this day it has not been completely
disputed or defended, but has continued to intrigue researchers around the world.

> A language that doesn't affect the way you think about programming
> is not worth knowing  
>
> --  Alan Perlis


### The Ruby Way

This is _not_ yet another "Ruby is better than Java" article.
Nor is it intended to bash Java or Java programmers.
Rather, it is an attempt to aid Java programmers who are investigating Ruby
by helping them quickly get over the "_Writing Java in Ruby_" syndrome and to discover the Ruby Way.

Now, on to our 10 Things...


## Item #10 - Learn Ruby Conventions

* `ClassNames`
* `method_names` and `variable_names`
* `methods_asking_a_question?`
* `slightly_dangerous_methods!`
* `@instance_variables`
* `$global_variables`
* `SOME_CONSTANTS` or `OtherConstants`

Some of the conventions are enforced by the language,
others are merely standards used by the community.


## Item #9 - Everything is an Object

Everything in Ruby that can be bound to a variable name
is a full-fledged object.

This has interesting consequences


### Classes are Objects!

* `Array` is a constant name that is bound to the Array class object.
* Creating new objects does not require special syntax. We just send `new` to the class object.

``` ruby
a = Array.new
```


### This make factories trivial

Since Classes create instances of themselves,
they are the ultimate factory object.

```ruby
def create_from_factory(factory)
  factory.new
end

obj = create_from_factory(Array)
```    


### No Primitives!

Even integers are full fledged objects.

``` ruby
0.zero?          # => true
1.zero?          # => false
1.abs            # => 1
-1.abs           # => 1
1.methods        # => list of methods for object 1
2.+(3)           # => 5  (same as 2+3)
10.class         # => Fixnum
(10**100).class  # => Bignum
```

### `nil` is an Object!

* Java:
  * `null` means "no reference to object"
  * Invoking a method on `null` is an error
* Ruby:
  * `nil` is a normal object
  * You can never get a null pointer error!

``` ruby
a = nil
a.nil?     # => true
a.methods  # => list of methods
a.abs      # => NoMethodError
```

### Things that are not Objects

* Variable names are not objects
  * You cannot have a variable reference another variable
    * (no indirection to variables)
  * There are workarounds to this
    * But that's an advanced topic
    * And no one really _needs_ it anyways.


### More Things that are not Objects

* Blocks are not objects
  * But that's a distinction without a difference
  * By the time you need them, they automatically convert to `Proc` objects


``` ruby
def with_block
  yield
end

with_block {
  # Never converted
}    
```

or

``` ruby
def with_proc(&block)
  block.call
end

with_proc {
  # Converted internally
}
```



## Item #8 - (Almost) Everything is a Message

All computation in Ruby happens through:

* Binding names to objects (assignment)
* Primitive controls structures (e.g. `if`/`else`, `while`) and operators (e.g. `defined?`)
* Sending messages to objects


### Yes, all of these are messages...

`string.index("x")`
:  Send `:index` (with argument "`x`")

`string.length`
:  Send `:length` (with no argument)

`run_status_reports`
:  Send `:run_status_reports` (to `self`)

`1 + 2`
:  Send `:+` (with argument `2`) to the object `1`

`array[i]`
:  Send `:[]` (with argument `i`) to the array



### Messages, not Function Calls

Java Programmers tend to think of `obj.method()` as looking up
a member function in a table and calling it.

Ruby programmers tend to think of `obj.method` as sending a message
to an object.

What's the Difference?

The difference is subtle, but important!


### What _Kind_ of Differences?

Consider the following class.
It defines an object that is able to record all the messages ever sent to it, and then playback those messages to another object.

``` ruby  
class VCR

  def initialize
    @messages = []
  end

  def method_missing(method, *args, &block)
    @messages << [method, args, block]
  end

  def play_back_to(obj)
    @messages.each do |method, args, block|
      obj.send(method, *args, &block)
    end
  end

end
```

### Playing Back Data

Example Code

``` ruby
require 'src/vcr'

vcr = VCR.new
vcr.sub!(/Java/) { "Ruby" }
vcr.upcase!
vcr[11,5] = "Universe"
vcr << "!"

string = "Hello Java World"
puts string

vcr.play_back_to(string)
puts string
```

Output

```
Hello Java World
HELLO RUBY Universe!
```


### Message Opportunities...

**Remote Proxies**
:  Automatically forward any message to a remote object.

**Auto Loaders**
:  Stand in for an object until it gets its first message. Then load it and act like a regular proxy. Great for autoloading database backed objects.

**Decorators**
:  Intercept the messages you want and pass the rest through.

**Mock Objects**
:  Just write the methods that need to be mocked. Proxy or ignore the others as needed.

**Builders**
:  Generate XML/HTML/Whatever based on the methods called on the builder.



## Item #7 - Ruby is _Way_ More Dynamic Than You Expect

One of the big attractions of Java over C++ was the dynamic features
of the language. You could easily load classes at run time,
query objects about their classes and methods,
and even call methods discovered at runtime.


### Dynamic Beyond Java

Ruby takes dynamic behavior several steps beyond Java.


* `method_missing`
* Easy Reflection
* Open Classes
* Singleton Objects
* Definition Hooks
* Code Evalutation


### Easy Reflection

Create Object - The Java Version

``` java
public static Object create(Class c, String value) throws Exception
{
  Constructor ctor = c.getConstructor( new Class[] { String.class } );
  return ctor.newInstance( new Object[] { "Hello" } );
}

public static void main (String args[]) throws Exception
{
  Greeting g = (Greeting) create(Greeting.class, "Hello");
  g.show();
}
```

The Ruby Version

``` ruby
def create(klass, value)
  klass.new(value)
end

g = create(Greeting, "Hello")
g.show
```

### Open Classes

Methods can be added to classes at any point ... even built in classes.

``` ruby
class Integer
  def even?
    (self % 2) == 0
  end
end

p (1..10).select { |n| n.even? }
# => [2, 4, 6, 8, 10]
```

Caution is advised, but this feature can be very useful.


### Singleton Methods

Singleton methods are defined on individual objects, not classes.

``` ruby
class Dog
end

rover = Dog.new
fido = Dog.new

def rover.speak
  puts "Red Rover"
end

rover.speak  # => "Red Rover"
fido.speak   # => NoMethodError
```

### Hooks

Hooks allow the user to gain control at interesting moments
during the execution of a program.

``` ruby
class MyClass

  def MyClass.method_added(name)
    puts "Adding Method #{name}"
  end

  def new_method
    # Yada yada yada
  end

end
```

Output

```
Adding Method new_method
```


### Code Eval

``` ruby
class Module

  def trace_attr(sym)
    self.module_eval %{def #{sym}
                         printf "Accessing %s with value %s\n", "#{sym}", @#{sym}.inspect
                         @#{sym}
                       end}
  end

end

class Dog
  trace_attr :name

  def initialize(string)
    @name = string
  end
end

Dog.new("Fido").name  # => Accessing name with value "Fido"
```



## Item #6 - Objects are Strongly Typed - Not Statically Typed

### What is a Type?

A type is  

**a set of values**  

and  

**a set of operations**  



### C Code (Weak)

``` c
#include <stdio.h>

extern float two();

int main() {
  float x = 1.5 + two();
  printf("%f\n", x);
  printf("%d\n", x);
  return 0;
}
```

---

``` c
int two() { return 2; }
```

Output

```
nan
0
```


### Java Code (Strong)

``` java
public class Main {
  public static void main (String args[]) {
    double x = 1.5 + Two.two();
    System.out.println(x);
  }
}
```

---

``` java
public class Two {
  public static int two() {
    return 2;
  }
}
```

Output

```
3.5
```

### Ruby Code (?)

``` ruby
require 'two'

x = 1.5 + two
puts x
printf "%d", x
```

---

``` ruby
def two
  2
end
```

Output

```
3.5
3
```


### So what makes a language type safe?

* Compiler knowledge of the variable types?
* Declaring all variables?
* Compiler catching all type errors?

Or...

* Catching all inappropriate operations on a type, either at
  * compile time, or
  * run time


### Ruby Code

``` ruby
def factorial(n)
  result = 1
  (2..n).each do |i|
    result *= i
  end
  result
end

puts factorial(20)
puts factorial(21)
```

Output

```
2432902008176640000
51090942171709440000
```


### Java Code

``` java
public class Fact {
  static long factorial(long n) {
    long result = 1;
    for (long i=2; i<=n; i++)
      result *= i;
    return result;
  }
  public static void main(String args[]) {
    System.out.println(factorial(20));
    System.out.println(factorial(21));
  }
}
```

Output

```
2432902008176640000
-4249290049419214848       // Overflow!!
```



### Lanaguage Typing Systems


Java is

* Strongly,
* Statically,
* Manifestly

typed.


Ruby is

* Strongly,
* Dynamically,
* Implicitly

typed.



### Testimonial

> I've been a statically typed bigot for quite a few years.
> I learned my lesson the hard way while using C.
> Too many systems crashed in the field due to silly typing errors. [...]
>
> Four years ago I got involved with Extreme Programming. [...] I can't
> imagine not having a comprehensive suite of unit tests to back up
> my development. [...]
>
> About two years ago I noticed something.
> I was depending less and less on the type system for safety.
> My unit tests were preventing me from making type errors. [...]
>
> So I tried writing some applications in Python, and then Ruby.
> I was not entirely surprised when I found that type issues simply
> never arose.
>
> -- [Bob Martin](http://www.artima.com/weblogs/viewpost.jsp?thread=4639)



## Item #5 - Don't Worry About Interfaces


### Ruby Uses Duck Typing

* If it walks like a duck,
* And talks like a duck,
  * Then we can treat it like a duck.
  * (who cares what it _really_ is)


``` ruby
class Duck
  def talk
     puts "Quack"
  end
end

class DuckLikeObject
  def talk
     puts "Kwak"
  end
end

flock = [
  Duck.new,
  DuckLikeObject.new
]

flock.each do |d|
   d.talk
end
```

No need to inherit from a common interface.




## Item #4 - Mix it up with Mix-Ins

Although Ruby does not have interfaces,
it does have mix-ins defined by modules.

A module...

* Is a namespace (like a class)
* Can have defined methods (like a class)
* Can not be instanciated (unlike a class)
* Can be mixed (included) into a class
  * The module methods become instance methods of the class



### Tedious comparison operators

Although all the logic is in the less-than method,
all the other comparisons must still be defined.

``` ruby
class Pair
  attr_accessor :first, :second
  # ...

  def <(other)
    (first < other.first) ||
    (first == other.first && second < other.second)
  end

  def >(other)
    other < self
  end

  # Other methods defined in terms of less than:
  #     <=, >=, ==
end
```


### Reuse the Mix-in

A mix-in allows the commonality to be factored out.

``` ruby
module ComparableUsingLess
  def >(other)
    other < self
  end

  # Other methods defined in terms of less than:
  #     <=, >=, ==
end

class Pair
  include ComparableUsingLess

  attr_accessor :first, :second
  # ...

  def <(other)
    (first < other.first) ||
    (first == other.first && second < other.second)
  end
end
```



## Item #3 - Embrace Closures

**Iteration**

``` ruby
[1,2,3].each do |item| puts item end
```


**Resource Management**

``` ruby
file_contents = open(file_name) { |f| f.read }
```

**Callbacks**

``` ruby
widget.on_button_press { puts "Got Button Press" }
```



## Item #2 - `ri` is Your Friend, `irb` is Your Other Friend


**`ri`**
:  Ruby Information. Man pages for standard Ruby objects.

**`irb`**
:  Interactive Ruby. Console based interactive Ruby interpreter.


```
$ ri Array
---------------------------------------------------------- Module: Array
     Arrays are ordered, integer-indexed collections of any object.
     Array indexing starts at 0, as in C or Java. A negative index is
     assumed to be relative to the end of the array---that is, an index
     of -1 indicates the last element of the array, -2 is the next to
     last element in the array, and so on.
------------------------------------------------------------------------
Includes:
---------
     Enumerable(all?, any?, collect, detect, each_with_index, entries,
     find, find_all, grep, include?, inject, map, max, member?, min,
     partition, reject, select, sort, sort_by, to_a, zip)
Class methods:
--------------
     [], new
Instance methods:
-----------------
     &, *, +, -, <<, <=>, ==, [], []=, assoc, at, clear, collect,
     collect!, compact, compact!, concat, delete, delete_at, delete_if,
     each, each_index, empty?, eql?, fetch, fill, first, flatten,
     flatten!, frozen?, hash, include?, index, indexes, indices, insert,
     inspect, join, last, length, map, map!, nitems, pack, pop, push,
     rassoc, reject, reject!, replace, reverse, reverse!, reverse_each,
     rindex, select, shift, slice, slice!, sort, sort!, to_a, to_ary,
     to_s, transpose, uniq, uniq!, unshift, values_at, zip, |
```


### Another RI Example

Ask about the instance method `last`...

```
$ ri Array#last
------------------------------------------------------------- Array#last
     array.last     =>  obj or nil
     array.last(n)  =>  an_array
------------------------------------------------------------------------
     Returns the last element(s) of _self_. If the array is empty, the
     first form returns +nil+.

      [ "w", "x", "y", "z" ].last   #=> "z"
```



### IRB Sample

Add `1+2`, then find the methods defined in `Proc`...

```
$ irb --simple-prompt

>> 1 + 2
=> 3
>> Proc.instance_methods(false)
=> ["[]", "==", "dup", "call", "binding", "to_s", "clone", "to_proc", "arity"]
```




## Item #1 - Stop Writing So Much Code!

Coworker Quote (paraphrased)

> "I decided to try out Ruby to solve my problem.
> So I wrote a little code and all of a sudden
> I discovered that I was done."

Examples

* Copland (Hivemind based) vs Needle Libraries
* Rake (Ruby version of Make)




## Item #Zero - Ruby Puts the Fun Back In Programming

![](i/fun.jpg)




## Some More Things You Should Know

* Namespaces (Classes and Modules) are Independent of Packages
* String Interpolation
* "`.`" (dot) vs "`::`" (double colon)
* No Overloading on Method Signatures
* There is a [JRuby](http://jruby.org) project
* Java static member function and Ruby class methods a kinda alike and kinda different
* `finally` is named `ensure`
* Flexible quoting



## Summary

- #10 Learn Ruby Conventions
- #9 Everything is an Object
- #8 (Almost) Everything is a Message
- #7 Ruby is Way More Dynamic Than You Expect
- #6 Objects are Strongly Typed, Not Statically Typed
- #5 Don't Worry About Interfaces
- #4 Mix it up with Mix-ins
- #3 Embrace Closures
- #2 `ri` is Your Friend, `irb` is Your Other Friend
- #1 Write Less Code
- #0 Ruby Makes Programming Fun Again
