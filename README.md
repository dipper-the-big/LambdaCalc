# LambdaCalc

This is an interpreter for Lambda calculus.

## Installation

  ``` console
  git clone https://github.com/zack-carnet/LambdaCalc.git
  cd LambdaCalc
  cabal build && cabal install
  ```
## Quick Start 

  * Simply start lambda calc repl like as follows:
  
  ``` console
❯ lambdacalc repl
  ```
  * Here are some things you can try 
  ``` text
❯ lambdacalc repl
>>> S
λa.λb.λe.a e (b e)
>>> K
λa.λb.a
>>> I
λa.a
>>> S K K
λa.a
>>> S K K === I
True
>>> S I I
λa.a a
>>> load examples/test.lambda
λa.λb.a (a (a b))
λa.λb.a (a b)
success
>>> one
λa.a
>>> tow
Could not fetch unbound variable: tow
>>> two
λa.λb.a (a b)
>>> one succ one
λa.λb.a (a b)
>>> one succ one === two
True
>>> exit
  ```
  * You can also run a file as follows:
  
  ``` console
❯ lambdacalc examples/test.lambda
λa.λb.a (a (a b))
λa.λb.a (a b)
  ```

## Syntax 

  * Here's a file (`test.lambda`) to give a brief overview of the syntax
  
  ```text
id = λx.x,
succ = λn f x.f (n f x),
zero = K I,
I ≡ S K K,
one = succ zero,
two = succ one,
display: succ two,
display: one succ one
  ```
  * if you're a peasant and don't have any easy way to type in λ or ≡, you can use \\ and === instead.
  
  * You can write lambda expressions like `\a.\b.c.b` or `\a b c. b`. They behave mostly as you would expect them to.

  * Apart from the lambda calculus there are a few special commands:-

    * `display: <expr>` : prints the expr onto stdin

    * `expand: <expr>` : return the expr written purely with lambdas (no bindings)

    * `<expr> === <expr>` : returns true or false after checking for equality

    * `identifier = <expr>` : binds an expression to a identifier which can be any string of letters

    * `load [files]` : loads a list of lambda files into the environment
    
  * Note: load runs the files fed to it so it also ends up running any display commands in those files.
