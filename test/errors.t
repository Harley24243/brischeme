
Lex errors

  $ echo "(+ 2 $)" | brischeme
  Brischeme
  > LEX ERROR: Expected valid character but found $.
  > 

  $ echo "(define foo$ 3)" | brischeme
  Brischeme
  > LEX ERROR: Expected valid character but found $.
  > 

  $ echo "#x" | brischeme
  Brischeme
  > LEX ERROR: Expected t or f but found x.
  > 

Parse errors

  $ echo "(+ -)" | brischeme
  Brischeme
  > PARSE ERROR: Expected s-expression list but got -.
  > 
