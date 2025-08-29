
Literals

  $ echo "3" | brischeme
  Brischeme
  > 3
  > 

  $ echo "33524135" | brischeme
  Brischeme
  > 33524135
  > 

Operators

  $ echo "(+ 2 4)" | brischeme
  Brischeme
  > 6
  > 

  $ echo "(- 5 3)" | brischeme
  Brischeme
  > 2
  > 

  $ echo "(* 2 8)" | brischeme
  Brischeme
  > 16
  > 

  $ echo "(/ 5 2)" | brischeme
  Brischeme
  > 2
  > 

Relations
 
  $ echo "(< 3 2)" | brischeme
  Brischeme
  > #f
  > 

  $ echo "(< 2 3)" | brischeme
  Brischeme
  > #t
  > 

  $ echo "(= 3 2)" | brischeme
  Brischeme
  > #f
  > 

  $ echo "(= 3 3)" | brischeme
  Brischeme
  > #t
  > 
