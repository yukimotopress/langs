To Ruby From C and C++
To Ruby From Java
To Ruby From JavaScript
To Ruby From Perl
To Ruby From PHP
To Ruby From Python




# 10 One Liners to Impress Your Friends

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


Java

``` java
int[] ia = range(1, 10).map(i -> i * 2).toArray();
List<Integer> result = range(1, 10).map(i -> i * 2).boxed().collect(toList());
```



## One Liner #2 - Sum a List of Numbers

Ruby

``` ruby
p (1..1000).reduce { |sum, n| sum + n }
# -or-
p (1..1000).reduce(&:+)
# -or-
p  (1..1000).reduce(:+)
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

Java

``` java
range(1, 1000).sum();
range(1, 1000).reduce(0, Integer::sum);
Stream.iterate(0, i -> i + 1).limit(1000).reduce(0, Integer::sum);
IntStream.iterate(0, i -> i + 1).limit(1000).reduce(0, Integer::sum);
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



## One Liner #10 - Sieve of Eratosthenes

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
sub p { $_[0], @_ > 1 ? p(grep { $_ % $_[0] } @_) : () }
my @primes = p(2 .. 100);
say foreach @primes;
```


Python

``` python
print sorted(set(range(2,n+1)).difference(set((p * f) for p in range(2,int(n**0.5) + 2) for f in range(2,(n/p)+1))))
```

PHP

``` php
<?
foreach($p = range(2, 100) as $v) foreach(range(2, $v - 1) as $c) if(!($v % $c) && $v != 2) unset($p[$v - 2]);

echo join("\n", $p);
```

