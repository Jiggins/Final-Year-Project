---
title:              Final Year Project
subtitle:           Final Year Project
author:             Jack Higgins
email:              Jack.Higgins@nuim.ie
date:               Thu 28 Jan 2016
year:               2016

supervisor:         Barak A. Pearlmutter
course:             Computer Science and Software Engineering
degree:             B.Sc. Single Honours
department:         Department of Computer Science

bibliography:       yes
colorlinks:         yes
font:               arial
fontsize:           11pt
geometry:           margin=25mm
linespacing:        1.15
papersize:          A4
toc:                yes

abstract: |
    - The abstract must be an accurate reflection of what is in your report.

    - Your abstract must be self-contained, without abbreviations, footnotes,
        or references. It should be a microcosm of the full report.

    - Your abstract must be 150-250 words written as one paragraph, and should
        not contain displayed mathematical equations or tabular material.

    - Ensure that your abstract reads well and is grammatically correct.

    - The abstract must cover; motivation, the problem statement, the approach,
        your results, and your conclusions.
...

# Introduction
<!-- Change this -->
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

A number of automatic differentiation tools exist in the community,[^ad-tools]
most of which for C/C++, but also Python, F#, Fortran, Haskell and more. The
most notable of these; Python ad[^python-ad], ADIC for C/C++[^ADIC], DiffSharp
for F#[^DiffSharp] and [ad][ad] for Haskell. 

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
in Haskell[^ecosystem]. Automatic differentiation is well established in the
Haskell community with the [ad][ad] package being actively developed since May
2010.[^ad-git-hist].

Haskell is fortunate to have a strong community behind it though the community
is still relatively small when compared to other mainstream languages. This is
due in part to the perceived difficulty to learn the language. Haskell is
infamous for its steep learning curve and misconceptions like 'a knowledge
category theory is needed to learn Haskell'. There are a number of blogs and
posts contributing to this misconception, [Ian Connolly - A Lazy
Evaluation](http://connolly.io/posts/lazy-evaluation/) and [Are there any
downsides or problems with Haskell? - Programmers Stack
Exchange](http://programmers.stackexchange.com/a/131865)

The motivation for this project is to lower the barrier of entry to AD imposed
by Haskell by developing a similar language that takes advantage of Haskell's
type system and ad package while introducing new syntax to represent AD
specific types. I will be encoding many of the commonly used types in automatic
differentiation directly into the language's syntax. This will make it easier
for people to reason about the new types introduced by the ad package.

To tackle this program I will be building a tokeniser and parser combination to
implement the language. The parser will be able to be read files from the
command line or read expressions from an interactive prompt. In both cases it
will create an abstract syntax tree using Haskell type classes. The expressions
can then be evaluated by descending the syntax tree with a recursive `eval`
function.

The library I have chosen for this task is [Text.Parsec][parsec], a *monadic
parser combinator library*. An alternative, would be the highly performant
[attoparsec][attoparsec] library or creating a parser combinator system from
scratch using String manipulation. Parsec was the preferable library to use as
it has a greater focus on parsing language than Attoparsec. Attoparsec is more
suitable for large files or data streams such as log files or HTTP
Headers[^attoparsec-performance].

# Technical Background

## Topic Material
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

## Technical Material

# The Problem

# The Solution

\pagebreak

## Choosing a Parser Library
Haskell provides to prominent parser Libraries [attoparsec][attoparsec] and
[parsec][parsec]. Both libraries implement _parser combinators_ but are
different in their design principles.

### Attoparsec
Attoparsec is a fast Haskell parser combinator library, aimed particularly at
dealing efficiently with network protocols and complicated text/binary file
formats.[^attoparsec-github]

Attoparsec focuses on high performance parsing of large amounts of raw data or
working with binary file formats[^attoparsec-performance]. Attoparsec can work
with `ByteString`s, a more efficient way of representing Strings as Byte
Strings rather than Lists of Characters.[^string]

Attoparsec forgoes some high-level features and readability of error messages
for performance.

### Parsec
Parsec is designed from scratch as an industrial-strength parser library. It is
simple, safe, well documented (on the package homepage), has extensive
libraries, good error messages, and is fast. It is defined as a monad
transformer that can be stacked on arbitrary monads, and it is also parametric
in the input stream type.[^parsec-package]

Parsec seems to be more suited to user facing applications, providing a rich
error reporting module [Text.Parsec.Error][error].

Parsec also implements a **monad Transformer** which allows parsec to be
layered in a monad stack.

Parsec seems to be the best choice to write a language parser as Attoparsec
seems to be more suited to applications dealing with binary data or network
protocols. Parsec may not be as performant as Attoparsec, but that should never
be an issue as a programming language should never surpass gigabytes in size.
Additionally Parsec provides a more user friendly experience with well written
documentation.

# Evaluation

# Conclusions

# References

# Appendices
Include here all extra material, e.g. your source code, project management
(optional) including: the task list, Gantt Chart diagrams (or equivalent),
discussion of any significant deviations from plan, and how you managed them,
discussion of what you would do differently if you repeated the project.

[ad]: http://hackage.haskell.org/package/ad "ad: Automatic Differentiation"

[Jacobian]: https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant "Jacobian matrix and determinant"
[parsec]: https://hackage.haskell.org/package/parsec "parsec: Monadic parser combinators"
[HM]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf "Generalizing Hindley-Milner Type Inference Algorithms"

[attoparsec]: http://hackage.haskell.org/package/attoparsec
[error]:      https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Error.html

<!-- References -->

[^ad-tools]: Autodiff.org <http://www.autodiff.org/?module=Tools&language=ALL>
[^python-ad]: Python Automatic Differentiation package <https://pypi.python.org/pypi/ad/1.2.3>
[^ADIC]: ADIC: Automatic Differentiation for C and C++ <http://trac.mcs.anl.gov/projects/ADIC>
[^DiffSharp]: DiffSharp: Differentiable Functional Programming <http://diffsharp.github.io/DiffSharp/>
[^ad-git-hist]: Commit history for the ad package <https://github.com/ekmett/ad/graphs/contributors>

[^ecosystem]: Gabriel Gonzalez' [State of the Haskell ecosystem][ecosystem-link]
[^Papers]: The two I have been using most are [Parsec, a fast combinator
  parser][parsec-paper] by Daan Leijen for parsec and [A Hitchhiker’s Guide to
  Automatic Differentiation][Hitchhiker] by Philipp H. W. Hoffmann for ad

[^attoparsec-performance]: Bryan O' Sullivan (author of Attoparsec) - What's in
    a parsing library? [Part 1][attoparsec-1], [Part 2][attoparsec-2]

[^attoparsec-github]: attoparsec readme file.
    <https://github.com/bos/attoparsec/blob/master/README.markdown>
[^parsec-package]: Parsec Hackage page
    <https://hackage.haskell.org/package/parsec-3.1.9>
[^string]: `type String = [Char]`
    <https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-String.html#t:String>

[parsec-paper]: https://web.archive.org/web/20120401040711/http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf
[Hitchhiker]: http://arxiv.org/abs/1411.0583
[ecosystem-link]: https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#compilers]
[attoparsec-1]: http://www.serpentine.com/blog/2010/03/03/whats-in-a-parsing-library-1/
[attoparsec-2]: http://www.serpentine.com/blog/2010/03/03/whats-in-a-parser-attoparsec-rewired-2/
