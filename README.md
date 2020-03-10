# A Little Scheme in Ruby

This is a small interpreter of a subset of Scheme.
It runs on Ruby 2.3.

As a Scheme implementation, 
it optimizes _tail calls_ and handles _first-class continuations_ properly.

## How to run

```
$ ./scm.rb
> (+ 5 6)
11
> (cons 'a (cons 'b 'c))
(a b . c)
> (list
1
2
3)
(1 2 3)
> 
```

