---
title:              Final Year Project Interim Report
author:             Jack Higgins 12367281
email:              Jack.Higgins@nuim.ie
date:               14 December 2015

toc:                yes
fontsize:           11pt
geometry:           margin=25mm
linespacing:        1.15
papersize:          A4 
...





## Goals of your Project
- To design a *new* language, *'Vlad'*, a language focused on performing
    automatic differentiation.
- To minimise the barrier into automatic differentiation through clever
    semantic design.
- To create a parser for the language using Haskell's [Monadic Parser
    Combinator Library][parsec]
- to create an interactive environment for automatic differentiation.

## Overview of Background
Automatic differentiation is a set of techniques to numerically evaluate the
derivative of a function. Derivatives of arbitrary order can be computed
automatically, accurately to working precision, and using at most a small
constant factor more arithmetic operations than the original program. Automatic
Differentiation excels over traditional approximative and symbolic methods.
Approximative methods are are prone to truncation and rounding errors and
symbolic differentiation lead to significantly long computation times. While
Automatic Differentiation is also numerical differentiation, in the sense that
it computes numerical values, it computes derivatives up to machine precision.
That is, the only inaccuracies which occur are those which appear due to
rounding errors in floating-point arithmetic or due to imprecise evaluations of
elementary functions.

In this project project I will be using an implementation of Automatic
Differentiation written in Haskell, by Edward Kmett, Barak Pearlmutter and
Jeffrey Mark Siskind. This package can be found on *Hackage* under the name
[*ad*][ad]. This package implements forward-, reverse- and mixed- mode automatic
differentiation combinators. Forward mode AD computes directional derivatives
by defining *Dual Numbers*, a formal truncated Taylor series of the form $x
+ \varepsilon x'$. *Dual Numbers* can be considered as data structures for
carrying the derivative around together with the undifferentiated answer.
Reverse mode AD computes directional gradients using sparse [*Jacobian
Matrices*][Jacobian].

The project will be written in Haskell to make use of the [ad][ad] package and
to take advantage of the rich ecosystem surrounding compiler and parser design
in Haskell. ^[See Gabriel Gonzalez' [State of the Haskell ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#compilers)]
The library I have chosen for this task is [Text.Parsec][parsec], a *monadic
parser combinator library*, as it is well documented with plenty of material
available online.

## Progress to Date
I began with getting to grips with the *Parser Combinators* library by writing
a couple of small parsers, a CSV file parser and a very simple Scheme parser
(yes, an almost cliché Scheme parser, it has to be done). I used these simple
techniques to get acquainted with the library, following this I'm beginning
work on a simple Haskell parser as I would like this language to be very
Haskell-like, taking inspiration from its design principles. Stephen Diehl has
written a great [blog series](http://dev.stephendiehl.com/fun/index.html) on
this which I have been enjoying.

In parallel to this I have been reading up on *AD*, the papers from the
Hamilton mailing list have been a great help along with more I've found online.
In addition to this I have been doing small problems in *AD*, usually by
importing the **ad** cabal package into *ghci* and playing around with
problems.

## Problems Encountered
I have not run into many technical problems in the process of this project,
although most of my time has gone into research and learning surrounding the
project. Although I was quite experienced with Haskell, there was still a lot to
learn. I spent a lot of time learning to use both the *Parsec* and *AD*
packages, luckily both topics have a plethora of papers written about them.[^Papers]

The main issue I have encountered is poor time management on my behalf. I feel
that I have greatly overestimated the amount of time I had to work on this. I
realise that I have prioritised short term and extra-curricular goals over work
on the project, this coupled with the overestimation of time meant that
progress was slow.

In light of this, I have taken measures to reduce the amount
of extra-curricular work I do. I have cut back on the hours I demonstrate, I no
longer teach Haskell to the Computational Thinking course (although I feel this
was beneficial to my skill in Haskell), and I have decided to take more of a
back seat approach to running the Minds society.

## Planned Next Steps
The next step in this process is to finalise work on the simple Haskell parser.
The goals for this parser is to be able to Tokenise a Haskell-like language
with helpful syntax error reporting and implement *type checking*
functionality, as I believe the use of a statically typed language would be
suited to the application of *AD*, as opposed to a dynamically typed language.

The next step I would like to take is to implement [*Hindley-Milner*][HM] type
inference. This will afford the user the need to reason about types when using
the language while still providing the benefit of a statically typed language.
*Hindley-Milner* type inference is a popular algorithm for type inference, with
languages like *Haskell* and *ML* with advanced type systems implemented using
the *Hindley-Milner* algorithm.

The next step would then to modify this language to focus on automatic
differentiation by creating primitive types for types and data structures such
as *Derivatives*, *Gradients*, *Dual Numbers*, *Jacobian Matrices*, etc.

[ad]: http://hackage.haskell.org/package/ad "ad: Automatic Differentiation"
[Jacobian]: https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant "Jacobian matrix and determinant"
[parsec]: https://hackage.haskell.org/package/parsec "parsec: Monadic parser combinators"
[HM]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf "Generalizing Hindley-Milner Type Inference Algorithms"
[^Papers]: The two I have been using most are [Parsec, a fast combinator
  parser][parsec-paper] by Daan Leijen for parsec and [A Hitchhiker’s Guide to
  Automatic Differentiation][Hitchhiker] by Philipp H. W. Hoffmann for ad

[parsec-paper]: https://web.archive.org/web/20120401040711/http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf
[Hitchhiker]: http://arxiv.org/abs/1411.0583
