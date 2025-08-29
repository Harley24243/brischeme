
Literals 

  $ echo "#t" | brischeme
  Brischeme
  > #t
  > 

  $ echo "#f" | brischeme
  Brischeme
  > #f
  > 

Computations

  $ echo "(and #t #f)" | brischeme
  Brischeme
  > #f
  > 

  $ echo "(and #t #t)" | brischeme
  Brischeme
  > #t
  > 

  $ echo "(or #t #f)" | brischeme
  Brischeme
  > #t
  > 

  $ echo "(or #f #f)" | brischeme
  Brischeme
  > #f
  > 

  $ echo "(not #f)" | brischeme
  Brischeme
  > #t
  > 

Conditional

  $ echo "(if #t 1 2)" | brischeme
  Brischeme
  > 1
  > 

  $ echo "(if #f 1 2)" | brischeme
  Brischeme
  > 2
  > 
