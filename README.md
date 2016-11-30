## Japanese crosswords solver
by Vyacheslav Dubinin <vyacheslav.dubinin@gmail.com>
  * http://code.google.com/p/jc-solver/
  * https://habrahabr.ru/post/151819/

build
```
  cabal sandbox init
  cabal install --only-dependencies --enable-tests
  cabal configure --enable-tests
  cabal build
```
test
```
  cabal test
```
run with interpretator
```
  cd src
  ghci
  Prelude> :l test-01
  Prelude> t1
```
