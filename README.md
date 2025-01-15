# linlang

This is an implementation of a small language with uniqueness types, inspired
by Philip Wadler's _Linear types can change the world!_ ([link][wadler]). It
was slightly modified to match more closely with the syntax of MinHS in the
_Concepts of Programming Language Design_ course taught by Gabriele Keller at
Utrecht University.

The `let!` syntax from Wadler's paper is not supported because I think its
typing rule is overly crude.

[wadler]: https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=24c850390fba27fc6f3241cb34ce7bc6f3765627


## Usage

```sh
$ cabal run linlang <examples/example-lin.txt
```

This first outputs a pretty-printed version of the desugared program. The
program is typechecked and run; the last line of the output contains the return
value and its type.

Type errors are not very readable (e.g. they concern the desugared program, and
do not have source locations that point back to where the error occurred).


## Language

A rough BNF that does not concern itself with precedence or proper whitespace spacing:
```
program ::= datadefs mainprog

datadefs ::= ε | datadef datadefs*
datadef ::= "data" uname "=" datacons
datacons ::= datacon | datacon "|" datacons
datacon ::= uname type*

mainprog ::= "main" "=" expr

type ::= uname | type funarrow type
funarrow ::= "->" | "¡->"

expr ::= "λ(" lname ":" type ")" "->" expr               -- nonlinear non-recursive function
       | "¡λ(" lname ":" type ")" "->" expr              -- linear non-recursive function
       | "recfun" lname ":" "(" type ")" lname "=" expr  -- nonlinear recursive function
       | "case" expr "of" "{" clauses "}"                -- case analysis
       | "let" pattern "=" expr "in" expr                -- let binding
       | uname lname*                                    -- data constructor (must be fully-applied)
       | expr expr                                       -- function application
clauses ::= clause | clause ";" clauses
clause ::= uname lname* "->" expr
pattern ::= uname pattern*      -- match a data constructor
          | "_"                 -- wildcard (ignore)
          | lname               -- bind as variable

lname ::= <lowercase letter, then zero or more of "0-9a-zA-Z" or "_" or "'">
uname ::= <optionally "¡", then an uppercase letter, then zero or more of "0-9a-zA-Z" or "_" or "'">
```
- Parentheses are allowed in types, expressions and patterns to disambiguate precedence in the usual way.
- The linearity marker, `¡` (both for lambda functions and for `uname`s), may also be spelled `#` for convenience.
- Lambda (`λ`) may also be spelled `\` for convenience.
- The function arrows (`->` and `¡->`) are right-associative, as usual.
- In a `case`, even if non-trivial patterns are used, each constructor of the data type being matched on must appear exactly once.
  Nested patterns are desugared to nested `case` expressions before type-checking.
