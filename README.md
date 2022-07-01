# conway.life
Some very old implementations I made of Conway's Game of Life.

## Direct version
The C version is a very direct interpretation of Conway's Game of Life on a Torus.
To build it, run
```
cd c; make
```

## Comonadic interpretation
Two blog posts inspired the haskell and ocaml implementations: [Piponi's][1] and
[Kmett's][2]. Both describe cellular automata evaluation as comonadic. The
haskell source is better documented and shows what is happening and where. To
build it, use
```
cd hs; stack exec --resolver lts-14.18 -- ghc life.hs
```
To build the ocaml version,
```
cd ml; ocamlbuild -lib unix life.native
```

[1]: http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
[2]: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/cellular-automata
