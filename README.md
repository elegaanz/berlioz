# Berlioz, a functional language to play sounds

And maybe even music!

This project is part of a school assignment (for the _Računalniška zvočna produkcija_ class at university of Ljubljana)
and should probably not be used.

## Overview of the language

A berlioz program is a sequence of binding, including one `main` binding (the one that
will be evaluated). A binding is made of a name and an expression, separated by a `=`.
Each binding creates a function with the given name. Functions can have arguments
whose names must be written between the function name and the equal sign.

```
C1 = 32.70320Hz

bass note = sin(note)

main = bass(C1)
```

Expressions can be : a constant, a function call (if there are parameters, they are between parenthesis and separated with comma),
a sum, a product, or a sequence (either with two expression separated by a `;`, or using an indented block).

When running a programm, the main node should be evaluated with various time values (each value in the language is actually
a function that takes a time as a parameter, producing a stream of values), to produce samples that can then be played or
recorded.

### Built-in functions

- `sin freq`
- `loop expr`
- `linear_adsr attack_duration attack_value decay_duration decay_value sustain_duration sustain_value release_duration`
- `sample file`
