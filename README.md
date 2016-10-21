# Mathlogic Homework, Task 1 : Proof correctness checker

## Build
`cabal install` should do it

Alternatively, `ghc Main.hs -O2`

### Build dependencies
`base >=4.8 && <4.9`
`attoparsec >=0.13 && <0.14`

## Invocation
* `<path>/Main` -- interactive - stdin / stdout, check after each statement, don't forget head!
* `<path>/Main <input>` -- output to stdout
* `<path>/Main <input> <output>`
