# glam

**glam** is an implementation of the guarded λ-calculus, as described in the paper [The Guarded Lambda-Calculus: Programming and Reasoning with Guarded Recursion for Coinductive Types](https://arxiv.org/abs/1606.09455) by Ranald Clouston, Aleš Bizjak, Hans Bugge Grathwohl and Lars Birkedal, and extended with polymorphism and automatic boxing as described in my [internship report](https://monade.li/glam.pdf).

Please refer to those papers for basic motivation and introduction to the language, as well as a description of its type system, operational and denotational semantics. This README only covers the specifics of my implementation.

An [online demo](https://glam.monade.li) is available, as well as [rendered documentation](https://glam.monade.li/doc).

- [Usage](#usage)
- [Syntax](#syntax)
  - [Types](#types)
  - [Terms](#terms)
  - [Programs](#programs)
- [Evaluation](#evaluation)
- [Type system](#type-system)
  - [Polymorphism](#polymorphism)
  - [Automatic boxing](#automatic-boxing)
- [To do](#to-do)

## Usage

This project is built using Cabal (`cabal build`).

```
usage: glam [options...] files...
  -i  --interactive  run in interactive mode (default if no files are provided)
```

The interactive mode gives you a REPL that will execute statements and display their results. It also provides a `:type` command that displays the type of a given term.

## Syntax

**glam**'s syntax is intended to be similar to Haskell's. See the `examples` directory for example programs.

### Types

The syntax for types is as follows:

```
TVar = [a-zA-Z_] [a-zA-Z_0-9']* ; (excluding keywords)

Type = "(" Type ")"
     | TVar                ; type variables
     | TVar Type*          ; application of type synonyms
     | "Int"               ; integer type
     | "0"                 ; zero/void/initial type
     | "1"                 ; unit/terminal type
     | Type "*" Type       ; product types
     | Type "+" Type       ; coproduct/sum types
     | Type "->" Type      ; function types
     | ">" Type            ; Later types
     | "#" Type            ; Constant types
     | "Fix" TVar "." Type ; fixed point types

TypeDef = "type" TVar TVar* "=" Type ; type synonyms

Polytype = ("forall" ("#"? TVar)+ ".")? Type
```

`*`, `+` and `->` are right-associative. Unicode syntax is supported (`μ`, `∀`, `→`, `×`, `⊤`, `⊥`, `ℤ`, `▸`, `■`).

Some syntactic sugar is provided:

| Construct | Desugars to |
| --- | --- |
| `type T x y z = ... (T x y z) ...` | `type T x y z = Fix T. ... T ...` |

Due to the absence of type-level lambdas, type synonyms must be applied to exactly as many arguments as they expect. When using a type synonym recursively inside its own definition, it must be applied to its exact formal arguments.

### Terms

The syntax for terms is as follows:

```
Var = [a-zA-Z_] [a-zA-Z_0-9']* ; (excluding keywords)
Integer = [0-9]+
Binary = "+" | "-" | "*" | "/" | "<$>" | "<*>"
Unary = "fst" | "snd"
      | "abort" | "left" | "right"
      | "fold" | "unfold"
      | "next" | "prev"
      | "box" | "unbox"

Term = "(" Term ")"
     | Var                                  ; variables
     | Integer                              ; integers
     | "intrec"                             ; integer recursion operator (forall a. (a -> a) -> a -> (a -> a) -> Int -> a)
     | "(" ","-separated(Term+) ")"         ; tuples
     | "(" ")"                              ; unit
     | Term Term                            ; applications
     | Term Binary Term                     ; binary operators
     | Unary Term                           ; unary operators
     | "\" Var+ "." Term                    ; λ-abstractions
     | "fix" Var+ "." Term                  ; fixed points
     | "let" "{" ";"-separated(TermDef) "}" ; let expressions
       "in" Term
     | "case" Term "of" "{"                 ; case expressions
       "left" Var "." Term ";"
       "right" Var "." Term "}"

TermDef = Var Var* "=" Term
```

Unicode syntax is supported (`λ`, `⊛`).

Some syntactic sugar is provided:

| Construct | Desugars to |
| --- | --- |
| `f x y z = t` | `f = \x y z. t` |
| `f = ... f ...` | `f = fix f. ... f ...` |
| `(a, b, c, ...)` | `(a, (b, (c, ...)))` |
| `\x y z. t` | `\x. \y. \z. t` |
| `fix x y z. t` | `fix x. \y z. t` |
| `f <$> x` | `next f <*> x` |

### Programs

**glam** programs are structured as follows:

```
Signature = Var ":" Polytype

Statement = TypeDef
          | Signature
          | TermDef
          | Term

Program = newline-separated(Statement)
```

Statements can span over multiple lines, provided that subsequent lines are indented further than the first line.

Type signatures and term definitions don't have to appear consecutively, but signatures have to appear *before* definitions.

Type signatures can be omitted; a most general type will then be inferred. In practice, most signatures can be omitted, except for terms involving `fold` and `unfold`.

The interpreter currently prints the (inferred or checked) type for each top-level term definition, as well as the value and inferred type of top-level terms.

## Evaluation

Terms are evaluated using a call-by-need strategy (piggy-backing on Haskell's), based on the operational semantics given in the paper.
This is a form of normalisation by evaluation (NbE), except we don't reify values back into terms.

## Type system

### Polymorphism

**glam** extends the guarded λ-calculus with predicative, rank-1 polymorphism à la Hindley-Milner.

Polymorphic types (or *polytypes*) are of the form `forall a #b. ...`, where `a` refers to any type, while `b` refers to any *constant* type.

To allow for interesting polymorphic types involving the constant (`#`) modality, the definition of **valid types** has been modified to allow polymorphic type variables to appear under `#`.

The definition of **constant types** has also been modified as follows:

- `x` is a constant type if and only if `x` is bound by `forall #x`
- `0`, `1`, `#T` are constant types
- `>T` is not a constant type
- `T1 * T2`, `T1 + T2` are constant if and only if `T1` and `T2` are both constant
- `T1 -> T2` is constant if and only if `T2` is constant¹
- `Fix x. T` is constant if and only if `T` is constant

¹ This is also more permissive than the paper's definition; this modification is based on semantic considerations.

Just like in standard Hindley-Milner, polymorphic generalisation only occurs for `let`-bound terms (but not inside recursive definitions).

### Automatic boxing

Consider the following motivating examples:

- The term `let { z = consG 0 z } in box z` (where `consG` is the constructor for guarded recursive streams) should type-check, because `box (fix z. consG 0 z)` does; but the typing rules given by the paper prohibit this because `z` has the non-constant type `Fix s. Int * >s`.
- The **box⁺** term former introduced in the paper is inelegant. We would like to be able to define such a construct internally:
  ```
  box' : forall a b. #(a + b) -> #a + #b
  box' ab = case unbox ab of {
      left a. left box a;
      right b. right box b }
  ```
  Again, this would not be allowed, because `a` and `b` have the potentially non-constant types `a` and `b`.

We solve these two problems simultaneously by introducing (context-dependent) notions of **boxable terms** and **boxed variables**, defined as follows:
- A term is boxable if each of its free variables is either boxed or has a constant type.
- A variable that is `let`-bound to a boxable term is automatically boxed (this includes top-level bindings; as a consequence, all top-level bound variables are boxed). Similarly, a variable bound in a `case` expression from matching on a boxable term is boxed.

The `box t` and `prev t` constructs require `t` to be boxable. This makes the examples above type-check as expected.

## To do

- Infer types for `fold` and `unfold` via higher-orderish unification. This should type-check:
  ```
  type CoNat = 1 + >CoNat
  rec : forall a. a -> (>a -> a) -> CoNat -> a
  rec z s = let { go n = case unfold n of { left _. z; right m. s (go <*> m) } } in go
  ```
  (Note that it does if you use `fix go.`)
- Better type error reporting.
- Make semicolons and braces optional using something like Haskell's layout rules.
- Add infix operators.
- Make `left`, `right`, `abort`, `fst`, `snd`, `(,)` (the pair former), `fix`, `next`, `(<$>)`, `(<*>)` and `unbox` first-class functions instead of keywords.
- Proper type constructors and pattern matching.
- Add basic types and operations (booleans, characters, strings...), and some sort of standard library for dealing with streams, colists, ...
- Add syntactic sugar to make recursive definitions less painful to write and read: idiom brackets, Idris-style `!(notation)`, something.
- More generally turn **glam** into a usable and useful programming language.
