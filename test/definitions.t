
Abbreviation

  $ echo "(define x 3) x" | brischeme
  Brischeme
  > 3
  > 

Redefinition

  $ echo "(define x 3) (define x 4) x" | brischeme
  Brischeme
  > 4
  > 

Recursion

  $ cat > input.txt << EOF
  > (define fac (lambda (x) (if (< x 1) 1 (* x (fac (- x 1))))))
  > (fac 3)
  > (fac 5)
  > EOF

  $ brischeme < input.txt
  Brischeme
  > > 6
  > 120
  > 

Curried recursive function

  $ cat > input.txt << EOF
  > (define exp (lambda (x) (lambda (y) (if (= y 0) 1 (* x ((exp x) (- y 1)))))))
  > ((exp 2) 3)
  > ((exp 4) 5)
  > EOF

  $ brischeme < input.txt
  Brischeme
  > > 8
  > 1024
  > 

