---
title: 10 One Liners to Impress Your Friends
---

_Ruby • Perl • Python • PHP •  Erlang • Java • JavaScript_


The one liners are:

 1. Multiple Each Item in a List by 2
 2. Sum a List of Numbers
 3. Verify if Exists in a String
 4. Read in a File
 5. Happy Birthday to You!
 6. Filter list of numbers
 7. Fetch and Parse an XML web service
 8. Find minimum (or maximum) in a List
 9. Parallel Processing
10. Sieve of Eratosthenes



## One Liner #1 - Multiple Each Item in a List by 2

Ruby

``` ruby
p (1..10).map { |n| n * 2 }

# => [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
```

Perl

``` perl
my @list = map { $_ * 2 } 1..10;
say foreach @list;
```


Python

``` python
print map(lambda x: x * 2, range(1,11))
```

PHP

``` php
<? foreach(range(1, 10) as $i) echo $i * 2 . " ";
```

Erlang

``` erl
[X * 2 || X <- lists:seq(1, 11)].
```

Java

``` java
int[] ia = range(1, 10).map(i -> i * 2).toArray();
// -or-
List<Integer> result = range(1, 10).map(i -> i * 2).boxed().collect(toList());
```

JavaScript

``` js
Array.from({ length: 10 }).map((_, i) => (i + 1) * 2);
```


## One Liner #2 - Sum a List of Numbers

Ruby

``` ruby
p (1..1000).reduce { |sum, n| sum + n }
# -or-
p (1..1000).reduce(&:+)
# -or-
p  (1..1000).reduce(:+)
# -or-
p (1..1000).to_a.sum     # requires ruby 2.4+
# => 500500

```

Perl

``` perl
use List::Util qw(sum);
my $sum = sum 1..1000;
say $sum;
```


Python

``` python
print sum(range(1,1001))
```

PHP

``` php
<? echo array_sum(range(1, 1000));
```

Erlang

``` erl
lists:sum(lists:seq(1, 1001)).
```


Java

``` java
range(1, 1000).sum();
// -or-
range(1, 1000).reduce(0, Integer::sum);
// -or-
Stream.iterate(0, i -> i + 1).limit(1000).reduce(0, Integer::sum);
// -or-
IntStream.iterate(0, i -> i + 1).limit(1000).reduce(0, Integer::sum);
```

JavaScript

``` js
??
```



## One Liner #3 - Verify if Exists in a String

Ruby

``` ruby
words = ["scala", "akka", "play framework", "sbt", "typesafe"]
tweet = "This is an example tweet talking about scala and sbt."
p words.any? { |word| tweet.include?(word) }
# => true
```

Perl

``` perl
my @wordList = ('groovy', 'akka', 'grails framework', 'spock', 'typesafe');
my $tweet = 'This is an example tweet talking about groovy and spock.';
my @verify = grep { rindex($tweet, $_) >= 0 } @wordList;
say @verify ? 'yes' : 'no';
```


Python

``` python
words = ["scala", "akka", "play framework", "sbt", "typesafe"]
tweet = "This is an example tweet talking about scala and sbt."
print any(word in tweet for word in words)
```

PHP

``` php
<?
$words = array("php", "foo", "framework", "apache", "nginx");
$tweet = "This is an example tweet talking about PHP and Apache.";

foreach($words as $word) if(stripos($tweet, $word) !== false) echo "$word\n";
```

Erlang

``` erl
Words = ["scala", "akka", "play framework", "sbt", "typesafe"].
Tweet = "This is an example tweet talking about scala and sbt".
[lists:member(S, Words) || S <- string:tokens(Tweet, " ")].
```

Java

``` java
final List<String> keywords = Arrays.asList("brown", "fox", "dog", "pangram");
final String tweet = "The quick brown fox jumps over a lazy dog. #pangram";

keywords.stream().anyMatch(tweet::contains);
keywords.stream().reduce(false, (b, keyword) -> b || tweet.contains(keyword), (l, r) -> l || r);
```

JavaScript

``` js
??
```


## One Liner #4 - Read in a File

Ruby

``` ruby
file_text = File.read("data.txt")
# -or-
file_lines = File.readlines("data.txt")
```

Perl

``` perl
my $content = do { undef $/; open my $fh, "10_oneliners.pl"; <$fh> };
say $content;
# -or -
my @lines = do { open my $fh, "10_oneliners.pl"; <$fh> };
say @lines;
```

Python

``` python
print open("ten_one_liners.py").readlines()
```

PHP

``` php
<? echo file_get_contents("oneliners.php");
```

Erlang

``` erl
file:read_file("ten_one_liners.erl").
```

Java

``` java
try (BufferedReader reader = new BufferedReader(new FileReader("data.txt"))) {
  String fileText = reader.lines().reduce("", String::concat);
}
// -or-
try (BufferedReader reader = new BufferedReader(new FileReader("data.txt"))) {
  List<String> fileLines = reader.lines().collect(toCollection(LinkedList<String>::new));
}
// -or-
try (Stream<String> lines = Files.lines(new File("data.txt").toPath(), Charset.defaultCharset())) {
  List<String> fileLines = lines.collect(toCollection(LinkedList<String>::new));
}
```


JavaScript

``` js
??
```


## One Liner #5 - Happy Birthday to You!

Ruby

``` ruby
4.times { |n| puts "Happy Birthday #{n==2 ? "dear Tony" : "to You"}" }
# => Happy Birthday to You
# => Happy Birthday to You
# => Happy Birthday dear Tony
# => Happy Birthday to You
```

Perl

``` perl
for my $i (1..4) { say 'Happy Birthday ', (($i == 3) ? 'dear Name' : 'to You') };
```

Python

``` python
print map(lambda x: "Happy Birthday to " + ("you" if x != 2 else "dear Name"),range(4))
```

PHP

``` php
<? foreach(range(1, 4) as $i) echo "Happy Birthday " .($i == 3 ? "dear Martin" : "to You") . "\n";
```

Erlang

``` erl
["Happy Birthday " ++ case X of 2 -> "dear Robert"; _ -> "You" end || X <- lists:seq(1, 4)].
```

Java

``` java
range(1, 5).boxed().map(i -> { out.print("Happy Birthday "); if (i == 3) return "dear NAME"; else return "to You"; }).forEach(out::println);
```

JavaScript

``` js
??
```


## One Liner #6 - Filter list of numbers

Ruby

``` ruby
p [49, 58, 76, 82, 88, 90].partition { |n| n > 60 }
# => [[76, 82, 88, 90], [49, 58]]
```

Perl

``` perl
my (@passed, @failed);
foreach my $n (49, 58, 76, 82, 88, 90) { my $aref = $n > 60 ? \@passed : \@failed; push @$aref, $n };
say join ",", @passed;
say join ",", @failed;
```

Python

``` python
print reduce(lambda(a,b),c: (a+[c],b) if c > 60 else (a,b + [c]), [49, 58, 76, 82, 88, 90],([],[]))
```

PHP

``` php
<? foreach(array(49, 58, 76, 82, 88, 90) as $i) $i > 60 ? ($passed[] = $i) : ($failed[] = $i);
```

Erlang

``` erl
[X || X <- lists:seq(40, 60), X >= 50].
```

Java

``` java
Map<String, List<Integer>> result = Stream.of(49, 58, 76, 82, 88, 90).collect(groupingBy(forPredicate(i -> i > 60, "passed", "failed")));
```

JavaScript

``` js
??
```


## One Liner #7 - Fetch and Parse an XML web service

Ruby

``` ruby
require 'open-uri'
require 'nokogiri'
results = Nokogiri::XML(open("http://search.twitter.com/search.atom?&amp;q=scala"))
```

Perl

``` perl
use HTTP::Tiny;
use XML::Simple;
use Data::Dumper;
print Dumper XMLin(HTTP::Tiny->new->get('http://search.twitter.com/search.atom?&q=perl')->{content});
```


Python

``` python
from xml.dom.minidom import parse, parseString
import urllib2
print parse(urllib2.urlopen("http://search.twitter.com/search.atom?&amp;q=python")).toprettyxml(encoding="utf-8")
```

PHP

``` php
<? echo simplexml_load_file("http://search.twitter.com/search.atom?q=php")->asXML();
```

Erlang

``` erl
inets:start().
xmerl_scan:string(element(3, element(2, httpc:request("http://search.twitter.com/search.atom?&q=erlang")))).
```

Java

``` java
FeedType feed = JAXB.unmarshal(new URL("http://search.twitter.com/search.atom?&q=java8"), FeedType.class);
JAXB.marshal(feed, System.out);
```

JavaScript

``` js
??
```


## One Liner #8 - Find minimum (or maximum) in a List

Ruby

``` ruby
p [14, 35, -7, 46, 98].min
# => -7
p [14, 35, -7, 46, 98].max
# => 98
```

Perl

``` perl
my ($min, $max);
for my $n (14, 35, -7, 46, 98) { $min=$n if $n<$min };
for my $n (14, 35, -7, 46, 98) { $max=$n if $n>$max };
say $min;
say $max;
# -or-
use List::Util qw(min max);
my ($min, $max);
$min = min (14, 35, -7, 46, 98);
$max = max (14, 35, -7, 46, 98);
say $min;
say $max;
```

Python

``` python
print min([14, 35, -7, 46, 98])
print max([14, 35, -7, 46, 98])
```

PHP

``` php
<?
echo min(array(14, 35, -7, 46, 98)) . "\n";
echo max(array(14, 35, -7, 46, 98)) . "\n";
```


Erlang

``` erl
lists:min(lists:seq(1, 10)).
lists:max(lists:seq(1, 10)).
```

Java

``` java
int min = Stream.of(14, 35, -7, 46, 98).reduce(Integer::min).get();
    min = Stream.of(14, 35, -7, 46, 98).min(Integer::compare).get();
    min = Stream.of(14, 35, -7, 46, 98).mapToInt(Integer::new).min();

int max = Stream.of(14, 35, -7, 46, 98).reduce(Integer::max).get();
    max = Stream.of(14, 35, -7, 46, 98).max(Integer::compare).get();
    max = Stream.of(14, 35, -7, 46, 98).mapToInt(Integer::new).max();
```

JavaScript

``` js
??
```


## One Liner #9 - Parallel Processing

Ruby

``` ruby
require 'parallel'

Parallel.map(lots_of_data) do |chunk|
  heavy_computation(chunk)
end
```

Perl

``` perl
use Parallel::Loops;
my @values;
my $pl = Parallel::Loops->new(4);
$pl->share(\@values);
$pl->foreach( [0..4], sub { push @values, exp($_) } );
say foreach @values;
```

Python

``` python
import multiprocessing
import math

print list(multiprocessing.Pool(processes=4).map(math.exp,range(1,11)))
```

PHP

``` php
<? ($pid = pcntl_fork()) == -1 ? exit(1) : ($pid ? hardCoreAction() && pcntl_wait($status) : hardCoreAction());
```

Erlang

``` erl
[spawn(fun() -> io:format("~w~n", [X * 2]) end) || X <- lists:seq(1, 10)].
```

Java

``` java
long result = dataList.parallelStream().mapToInt(line -> processItem(line)).sum();
```

JavaScript

``` js
??
```




## One Liner #10 - Sieve of Eratosthenes


> In mathematics, the sieve of Eratosthenes is a simple, ancient algorithm for
> finding all prime numbers up to any given limit.
>
> It does so by iteratively marking as composite (i.e., not prime) the multiples of each prime,
> starting with the first prime number, 2. The multiples of a given prime are generated
> as a sequence of numbers starting from that prime,
> with constant difference between them that is equal to that prime.
> This is the sieve's key distinction from using trial division
> to sequentially test each candidate number for divisibility by each prime.
>
> The sieve of Eratosthenes can be expressed in pseudocode, as follows:
>
>     Input: an integer n > 1.
>   
>     Let A be an array of boolean values, indexed by integers 2 to n,
>     initially all set to true.
>
>     for i = 2, 3, 4, ..., not exceeding √n:
>       if A[i] i true:
>         for j=i^2, i^2+i, i^2+2i, i^2+3i, ..., not exceeding n:
>            A[j] := false.
>
>     Output: all i such that A[i] is true.
>
> -- [Wikipedia](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)



<!-- fix/todo: use primes below 121 for calculation sample / output -->



Ruby

``` ruby
def primeSieve(n)
  primes = Array.new

  for i in 0..n-2
   primes[i] = i+2
  end

  index = 0
  while primes[index]**2 <= primes.last
    prime = primes[index]
    primes = primes.select { |x| x == prime || x % prime != 0 }
    index += 1
  end
  primes
end
primeSieve(11)
# => [2, 3, 5, 7, 11]

# -or-

def eratosthenes(n)
  # eratosthenes starts with nums = [nil, nil, 2, 3, 4, 5, ..., n],
  # then marks (　the nil setting　) multiples of 2, 3, 5, 7, ... there,
  # then returns all non-nil numbers which are the primes.

  nums = [nil, nil, *2..n]
  (2..Math.sqrt(n)).each do |i|
    (i**2..n).step(i){ |m| nums[m] = nil}  if nums[i]
  end
  nums.compact
end

# -or-

def primeSieve(n)
  nums = (2..n).to_a
  nums.each { |num| nums.map! { |i| i % num == 0 && i != num ? nil : i }.compact! }
end

# -or-

require 'prime'
Prime.take(5)
# => [2, 3, 5, 7, 11]
```

Perl

``` perl
sub sieve {
  my $n = shift;
  my @composite;
  for my $i (2 .. int(sqrt($n))) {
    if (!$composite[$i]) {
      for (my $j = $i*$i; $j <= $n; $j += $i) {
        $composite[$j] = 1;
      }
    }
  }
  my @primes;
  for my $i (2 .. $n) {
    $composite[$i] || push @primes, $i;
  }
  @primes;
}
```
---

<!-- shortcut version -->

``` perl
sub p { $_[0], @_ > 1 ? p(grep { $_ % $_[0] } @_) : () }
my @primes = p(2 .. 100);
say foreach @primes;
```


Python

``` python
# Using set lookup
def eratosthenes2(n):
    multiples = set()
    for i in range(2, n+1):
        if i not in multiples:
            yield i
            multiples.update(range(i*i, n+1, i))

print(list(eratosthenes2(100)))

# Using array lookup
def primes_upto(limit):
    is_prime = [False] * 2 + [True] * (limit - 1)
    for n in range(int(limit**0.5 + 1.5)): # stop at ``sqrt(limit)``
        if is_prime[n]:
            for i in range(n*n, limit+1, n):
                is_prime[i] = False
    return [i for i, prime in enumerate(is_prime) if prime]
```

---

<!-- shortcut version -->

``` python
print sorted(set(range(2,n+1)).difference(set((p * f) for p in range(2,int(n**0.5) + 2) for f in range(2,(n/p)+1))))
```

PHP

``` php
function iprimes_upto($limit)
{
  for ($i = 2; $i < $limit; $i++)
  {
	   $primes[$i] = true;
  }

  for ($n = 2; $n < $limit; $n++)
  {
	   if ($primes[$n])
	   {
	     for ($i = $n*$n; $i < $limit; $i += $n)
	     {
		      $primes[$i] = false;
	     }
	   }
  }
  return $primes;
}
```

---

<!-- shortcut version -->

``` php
<?
foreach($p = range(2, 100) as $v) foreach(range(2, $v - 1) as $c) if(!($v % $c) && $v != 2) unset($p[$v - 2]);

echo join("\n", $p);
```

Erlang


``` erl
-module(sieveof).
-export([main/1,primes/1, primes/2]).                 

main(X) -> io:format("Primes: ~w~n", [ primes(X) ]).  

primes(X) -> sieve(range(2, X)).                                         
primes(X, Y) -> remove(primes(X), primes(Y)).                            

range(X, X) -> [X];                                                      
range(X, Y) -> [X | range(X + 1, Y)].                                    

sieve([X]) -> [X];                                                       
sieve([H | T]) -> [H | sieve(remove([H * X || X <-[H | T]], T))].        

remove(_, []) -> [];                                                     
remove([H | X], [H | Y]) -> remove(X, Y);                                
remove(X, [H | Y]) -> [H | remove(X, Y)].
```

---

<!-- shortcut version -->

``` erl
N = 50.
[X || X <- lists:usort(lists:seq(2, N + 1)), not lists:member(X, lists:usort([(P * F) || P <- lists:seq(2, round(math:pow(N, 0.5)) + 2), F <- lists:seq(2, round(N / P))]))].
```



Java

``` java
import java.util.LinkedList;

public class Sieve{
       public static LinkedList<Integer> sieve(int n){
               if(n < 2) return new LinkedList<Integer>();
               LinkedList<Integer> primes = new LinkedList<Integer>();
               LinkedList<Integer> nums = new LinkedList<Integer>();

               for(int i = 2;i <= n;i++){ //unoptimized
                       nums.add(i);
               }

               while(nums.size() > 0){
                       int nextPrime = nums.remove();
                       for(int i = nextPrime * nextPrime;i <= n;i += nextPrime){
                               nums.removeFirstOccurrence(i);
                       }
                       primes.add(nextPrime);
               }
               return primes;
       }
}
```


JavaScript

``` js
function eratosthenes(limit) {
    var primes = [];
    if (limit >= 2) {
        var sqrtlmt = Math.sqrt(limit) - 2;
        var nums = new Array(); // start with an empty Array...
        for (var i = 2; i <= limit; i++) // and
            nums.push(i); // only initialize the Array once...
        for (var i = 0; i <= sqrtlmt; i++) {
            var p = nums[i]
            if (p)
                for (var j = p * p - 2; j < nums.length; j += p)
                    nums[j] = 0;
        }
        for (var i = 0; i < nums.length; i++) {
            var p = nums[i];
            if (p)
                primes.push(p);
        }
    }
    return primes;
}
```


### Sieve of Eratosthenes References

- [Sieve of Eratosthenes @ Rosetta Code](http://rosettacode.org/wiki/Sieve_of_Eratosthenes)
- [Sieve of Eratosthenes @ Wikipedia](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
