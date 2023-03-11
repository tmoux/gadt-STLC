Implementation of simply typed lambda calculus, using GADTs achieve a representation of well-typed terms.

Inspired by [this talk](https://www.youtube.com/watch?v=6snteFntvjM) by Richard Eisenberg.

## Installing
Run
```
cabal configure
cabal build
```

Then run ```cabal run gadt-STLC```

## Language
The language is just simply-typed lambda calculus with recursion.
* The only types are ```Int```, ```Bool```, and ```t -> t```
* There are integer constants and the operations ```+```, ```-```, ```==```, ```<```, ```>```
* There are lambda abstractions and applications: ```lam x:Int.x``` and ```e e```. You must annotate the type of all lambdas
* There are let expressions: ```let x = e in e```
* There are if expressions: ```if e1 then e2 else e3```, where ```e1``` must have type ```Bool``` and ```e2``` must have the same type as ```e3```.
* There is the fixed point combinator: ```fix f```, where ```f``` has type ```a -> a```. We can use this to define recursive functions.

## Repl Interaction

The REPL uses commands prefixed with ```:``` like in GHCi.
* ```:eval``` or ```:e``` evaluates an expression and displays its type. This is also the default action of the REPL (if you just type an expression)
* ```:step``` or ```:s``` uses small-step evaluation and displays all the reductions
* ```:type``` or ```:t``` displays the type of an expression
* ```:parse``` or ```:p``` displays the result of parsing (but not typechecking) the input
* ```Ctrl-D``` to exit
