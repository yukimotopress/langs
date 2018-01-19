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


## The Ruby Way

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

<!-- clear -->

``` ruby
def with_block
  yield
end

with_block {
  # Never converted
}    
```

- or -

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
:  Generate XML/HTML/Whatever based on the methods called on the builder



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
public static Object create(Class c, String value)
  throws Exception
{
  Constructor ctor = c.getConstructor(
    new Class[] { String.class } );
  return ctor.newInstance( new Object[] { "Hello" } );
}
    
public static void main (String args[])
  throws Exception
{
  Greeting g =(Greeting) create(Greeting.class, "Hello");
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

```
class Module
  def trace_attr(sym)
    self.module_eval %{
      def #{sym}
        printf "Accessing %s with value %s\n",
          "#{sym}", @#{sym}.inspect
        @#{sym}
      end
    }
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

_What is a Type?_

A type is  
**a set of values**  
and  
**a set of operations**  
%class center



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


### Java Code (Strong)

``` java
public class Main {
  public static
    void main (String args[]) {
    double x = 1.5 + Two.two();
    System.out.println(x);
  }
}
```

```
int two() { return 2; }
```

Output

```
nan
0
```


```
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
