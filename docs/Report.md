---
title:              VLAD - Very Lovely Automatic Differentiation
subtitle:           Automatic Differentiation interpreter
author:             Jack Higgins
email:              Jack.Higgins@nuim.ie
date:               Thu 28 Jan 2016
year:               2016

supervisor:         Barak A. Pearlmutter
course:             Computer Science and Software Engineering
degree:             B.Sc. Single Honours
department:         Department of Computer Science

colorlinks:         yes
font:               arial
fontsize:           11pt
geometry:           margin=25mm
linespacing:        1.15
papersize:          A4
toc:                yes

abstract: |
    This report documents my findings from an exploration into developing
    a programming language focused on simplifying automatic differentiation.

    Despite its known advantages over Numeric and Symbolic Differentiation,
    Automatic Differentiation has, up until recently, been underutilised in
    fields that require differential algorithms, such as machine learning.
    One possible cause of this 'criminal' underutilisation of of automatic
    differentiation may be the intimidating APIs. This project attempts to
    tackle this issue by developing a new programming language that integrates
    automatic differentiation types directly into the syntax of the language.

    This theseis the process involved in developing a new language from early
    concept to final implemtation.
...

# Declaration
I hereby certify that this material, which I now submit for assessment on the
program of study leading to the award of B.Sc. Single Honours in Computer
Science and Software Engineering, is _entirely_ my own work and has not been
taken from the work of others - save and to the extent that such work has been
cited and acknowledged within the text of my work.

Signed:

Date: 31 Mar 2016

\pagebreak

# Introduction
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
elementary functions.[@hitchhiker-paper]

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

<!-- **Include 'AD is underused'** -->

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
from[attoparsec][attoparsec] library or creating a parser combinator system
scratch using String manipulation. Parsec was the preferable library to use as
it has a greater focus on parsing language than Attoparsec. Attoparsec is more
suitable for large files or data streams such as log files or HTTP
Headers[^attoparsec-performance].

\pagebreak

# Technical Background

## Topic Material
Automatic differentiation is a set of techniques to numerically evaluate the
derivative of a function.

Automatic differentiation works by systematically applying the chain rule of
differential calculus at the elementary operator level. It differers from
numerical differentiation in that, numerical differentiation is the finite
difference approximation of derivatives calculated by evaluating the function
at a set of points. Simply, its based on the standard definition of
a derivative; given a function $f$ of many variables $f : \mathbb{R}
\rightarrow \mathbb{R}$, a gradient $\nabla f = \left ( \frac{\partial
f}{\partial x_1}, \dots, \frac{\partial f}{\partial x_n} \right )$ can be
approximated using

$$
\frac{\partial f(x)}{\partial x_i} \approx \frac{f(x + hv_i) - f(x)}{h},
$$

where $v_i$ is the $i$-th unit vector and $h$ is the step size where $h > 0$.
Numeric differentiation suffers from cancellation and rounding-errors in the
discretization, the process of transferring continuous functions into discrete
counterparts. Additionally, numerical differentiation performs in $O(n)$ time
for a gradient in $n$ dimensions, this can be slow at computing the partial
derivatives of a function with respect to many inputs, as is often the case in
gradient based optimization algorithms. In contrast, automatic differentiation
calculates the derivatives while evaluating the function.

Automatic differentiation utilises the fact that any function is the
composition of a number of primitive operations. The partial derivative of
these operations can be composed to derive an entire program at
a point[^ad-readme].

### Forward Mode Automatic Differentiation
One approach to automatic differentiation is to use operator overloading. The
primitive operations can be overloaded to work with a dual representation of
number. In its simplest form, can be represented as a pair of values and
derivatives. Here they are represented in Haskell using a type class.

```haskell
{-# LANGUAGE RankNTypes #-}
data Dual s a = Dual
  { value      :: a
  , derivative :: a
  } deriving (Show)

lift ::  Num a => a -> Dual s a
lift x = Dual x 0

infinitesimal :: Num a => Dual s a
infinitesimal = Dual 0 1

instance Num a => Num (Dual s a) where
  Dual x x' + Dual y y' = Dual (x + y) (x' + y')
  Dual x x' * Dual y y' = Dual (x * y) (x' * y + x * y')
  negate (Dual x x')    = Dual (negate x) (negate x')
  abs    (Dual x x')    = Dual (abs x) (signum x * x')
  signum (Dual x _)     = lift (signum x)
  fromInteger           = lift . fromInteger

instance Fractional a => Fractional (Dual s a) where
  recip (Dual x x') = Dual (recip x) (-x'/x/x)
  fromRational      = lift . fromRational
```

This example represents a dual number as a pair of type class containing two
polymorphic fields 'value' and 'derivative'. By creating an instance for the
$Num$ and $Fractional$ class, the standard numeric operators, $+, -, *\ and\ /$,
are overloaded for the $Dual$ type to evaluate both the value and derivative of
a function. A dual number can be created using the $lift$ function, which
'lifts' a number to a constant quantity. By defining the function $diff$,

```haskell
diff :: Num a => (forall s. Dual s a -> Dual s a) -> (a, a)
diff f = let Dual y y' = f infinitesimal
         in (y,y')
```

it is now possible to compute the derivative for any function of type $\lambda
a:Num \cdot\ \forall s\ \cdot\ Dual\ s\ a \rightarrow Dual\ s\ a$. For higher
dimensional functions, a Jacobian matrix is used.

### Reverse Mode Automatic Differentiation
Reverse mode AD computes directional gradients using sparse [*Jacobian
Matrices*][Jacobian]. A Jacobian matrix is the matrix of first order partial
derivatives of a function. Given the function $f: \mathbb{R}^n \rightarrow
\mathbb{R}^m$ with an input vector $x \in \mathbb{R}^n$ that produces an vector
$f(x) \in \mathbb{R}^m$, then the corresponding Jacobian matrix $J$ of $f$ is
an $m \times n$

$$
J = \frac{df}{dx} =
\begin{bmatrix}
    \frac {\partial f_1} {\partial x_1} & \dots  & \frac {\partial f_1} {\partial x_n} \\
    \vdots                              & \ddots & \vdots                              \\
    \frac {\partial f_m} {\partial x_1} & \dots  & \frac {\partial f_m} {\partial x_n}
\end{bmatrix}
$$

Automatic differentiation in the reverse accumulation mode is a generalised
form of the back propagation algorithm of neural networks. Given the fact that
any function is the composition of a number of primitive operations, a function
can be broken up into a directed computational graph of intermediate primitive
functions. In the case of the [ad][ad] package, the [data-reify][data-reify]
package is used[^ad-cabal]. [data-reify][data-reify] is a library for
transforming a recursive data structure into an explicit graph. The [ad][ad]
package utilises this to construct the computational graph of a function before
applying back propagation.

<!-- Maybe include an image here -->

The algorithm propagates derivatives backwards through this graph from a given
output. This is done by supplementing each intermediate variable $v_i$, with an
adjoint

$$
\bar{v}_i = \frac {\partial y_i} {\partial v_i},
$$

which represents the sensitivity of a considered output $y_j$ with respect to
changes in $v_i$.\cite{DBLP:journals/corr/BaydinPR15} Derivatives are computed
in a two stage process; first the function is run forward, populating the
variables $v_i$, keeping track of dependencies in the graph. Derivatives are
then calculated by propagating adjoints $\bar{v}_i$ in reverse. [@baydin]

### Monadic Parser Combinators
Combinators are higher order functions that use function application and other
combinators to define a composite function. Combinators are a product of
combinatory logic. Combinatory logic was introduced by Haskell Curry and Moses
Schönfinkel[@10.2307/2370619]. In computer science, combinatory logic can be
used as a simplified model of computation and forms the basis for many
functional programming languages.

In the realm of software design, combinators are implemented as higher order
functions that compose functions together. Take, for example, function
composition $(.)$ as defined in the Haskell prelude.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

This combinator takes two functions as arguments and returns a new function,
the composition of the two arguments. Combinators allow for flexibly in
software development as many complex composite functions can be created with
relatively few functions and combinator.

Another common example, one that will be used extensively throughout this
project and the libraries that support it, is the $Alternative$ monoid. The
Alternative monoid is a sub-class of the $Applicative$ class[^alternative].
The Alternative class is used to model an action that can fail.

The Alternative class defines the `<|>` operator. This operator defines a
sequence of alternate actions to perform in the case of an earlier computation
failing. This combinator serves to compose a number of actions and follow the
first successful path.

When applied to a Parser, a number of functions can be defined to represent the
lexemes of a language. These lexemes can be composed using the `<|>` operator
such that if the input does not satisfy the lexeme, it will attempt to use the
next alternative.

## Technical Material

### Write You a Haskell - Stephen Diehl
'Write you a Haskell' is a blog series by Stephen Diehl on _Building a modern
functional compiler from first principles_[@write-you-a-haskell]. The name is a
parody of the popular [_Learn You a Haskell for Great
Good_](http://learnyouahaskell.com) by _Miran Lipovača_. This series of blog
posts attempts build a small functional language in an effort to teach the
reader some intermediate level Haskell and highlight some of the features and
libraries the language has to offer. At the time of writing, this series is
unfinished with 10 of the planned 28 chapters completed.

### Real World Haskell - Bryan O'Sullivan, Don Stewart, and John Goerzen
Similar to Stephen Diehl's 'Write You a Haskell', chapter 16 of _Real World
Haskell_[@real-world-haskell] focuses on introducing the reader to
Monadic Parser libraries through a series of simple examples. In contrast,
however, Real World Haskell provides a number of small self-contained examples
rather than one large project.

# The Problem
As mentioned earlier, automatic differentiation is advantageous over its
Numeric and symbolic counterparts due too its efficiency and accuracy. It is
ideal for certain applications like machine learning. Despite this, up until
recently automatic differentiation was thought to be 'criminally' underused in
the field of machine learning[@ad-underused].

I believe that automatic differentiation is intimidating to new users. In
particular the $ad$ package, from its use of $Rank\ N\ Types$ and lengthy type
signatures will be intimidating even to users with an intermediate to advanced
knowledge of Haskell. Take, for example, the $grad$ function as it is defined
in [Numeric.AD](https://hackage.haskell.org/package/ad-4.3.2/docs/Numeric-AD.html#g:2)

```haskell
grad :: (Traversable f, Num a) => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> f a
```

Type signatures like $grad$ can be difficult to reason about without prior
knowledge of the library. The reserved word $forall$ requires experience of
Rank N types and $Reifies$ implies that the _data-reify_ library is needed but
it is not clear as to why unless the user understands the underlying
implementation of _Reverse Mode_.

In light of this, the goal of the project is to build a domain specific
language to simplify the process of automatic differentiation. I hope to
develop a similar language with support for automatic differentiation built
into the syntax. The intention is develop a system where the difficult concepts
of automatic differentiation are simplified by the syntax so that a new user
does not need to waste any time learning about the implementation details of
ad.

The user should expect to see similar features to any modern interpreter. That
is the following:

-   Evaluate a number of files on the command line.
-   Enter a REPL (Read Evaluate Print Loop), like *irb (Ruby), python and
    ghci (Haskell)* to quickly evaluate code.
-   Have support for the GNU Readline library, that grants support for using
    standard GNU navigation controls, like the arrow keys. (mappings taken from
    the `.inputrc` file)
-   Specify an `.rc` file for the user to specify run time settings.

# The Solution

## The Combinator Pattern
The combinator design pattern is popular amongst Haskell libraries. This is
where the library provides some very simple 'primitive' functions and a set of
combinators. Complex structures can be created by combining primitive
functions using combinators.[@combinator-pattern]

The combinator pattern has the advantage of providing a wide range of
functionality with relatively little code. By combining primitive functions the
end user can flexibly create any number of composite functions to suit their
needs. Another advantage of the combinator pattern is the streamlining of
testing and verification. Once the primitive and combinator functions have been
thoroughly tested/verified, the composite functions should not require as much
scrutiny in testing since many of the edge cases should be caught when testing
primitive functions.

ad, Parsec, Attoparsec and this project implement the combinator pattern.

## Choosing a Parser Library
Haskell provides to prominent parser Libraries [attoparsec][attoparsec] and
[parsec][parsec]. Both libraries implement _parser combinators_ but are
different in their design principles.

### Attoparsec
Attoparsec is a fast Haskell parser combinator library, aimed particularly at
dealing efficiently with network protocols and complicated text/binary file
formats.[^attoparsec-github]

Attoparsec is a fork of the Parsec library that focuses on high performance
parsing of large amounts of raw data or working with binary file
formats[^attoparsec-performance]. Attoparsec can work with `ByteString`s, a
more efficient way of representing Strings as Byte Strings rather than Lists of
Characters.[^string]

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

## Using the Monadic Parser Combinators
Parsec provides an instance for $Alternative$ for the $ParsecT$ type class in
[Text.Parsec.Prim](https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Prim.html).
That defines the `<|>` operator for the `ParsecT` type. It is then possible to
compose simple parsers to build expressions.

As mentioned before, the `<|>` operator defines a sequence of alternate actions
to perform in the case of an earlier computation failing. This can be used to
compose multiple lexeme in order of precedence. Consider the example below,
given the input $1 + 1$ this composite parser will first try to parse a
$typeDefinition$. The defining feature of a type definition is the `::`
operator, since this does not appear in the input string, `typeDefinition` will
fail. Next it will try `infixexp` and succeed.

```haskell
expr :: Parsec String u Expr
expr =  try typeDefinition
    <|> infixexp
```

The `try` function is important in this case, without `try` the input would
have been consumed by the `typeDefinition` function before failing without from
not consuming `::`. The `try` functions prevents input from being consumed if
the action fails. This allows for infinite look ahead.

## Building a Lexical Analyser
Most parsers take their input from a lexical analyzer or scanner which filters
white space and comments and returns a list tokens for the parser.
[Text.Parsec.Token][token] provides a `GenTokenParser` type class, which takes
a language definition, defined below, and generates a token parser.

```haskell
langDef :: LanguageDef st
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = lower <|> char '_'
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = Token.opLetter langDef
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.caseSensitive   = True
  , Token.reservedOpNames = ["=", "::", "\\", "->", "=>", "|", ",", ".."]
  , Token.reservedNames   = ["if", "then", "else", "let", "type", "data", "newtype", "class", "instance", "deriving", "where"]
```

The generated token parser provides common tokens such as identifiers, numbers,
parentheses and basic whitespace rules. These can easily be overwritten by
defining a new function in their place.

## Syntax tree
Using compositions of the primitive parser functions generated by the token
parser, I was able to define the syntax tree shown below, where `expr` is the
entry point of the parser. Each expression has a corresponding type defined in
the $Parser.Types$ module.

```
Entry point to the parser
    expr → infixexp :: [context =>] typeExpr
         | infixexp

Any infix expression. Either a special character or a function surrounded in backticks `` ` ``
     infixexp → lexp operator infixoperator
              | - infixexp
              | lexp

Declarations, lambdas and lets
    lexp → \ {variable} -> expr
         | variable = expr
         | let declarations in expr
         | if expr then expr else expr
         | fexpr

function application
    fexpr → [fexpr] aexpr

Everything else
    aexpr → (expr)
          | variable
          | constructor
          | literal
          | ( expr , ... , expr )           (tuple)
          | [ expr , ... , expr ]           (list)
          | [ expr , expr .. [expr] ]       (list sequence)
          | ( infixexp infixOperator )          (sectionLeft)
          | ( infixOperator infixexp )          (sectionRight)
```

The function application, $fexpr$, is particularly notable as it defines how
functions are constructed.  It parses an optional function followed by a
symbol. This means that any function that it parses can only have _one_
argument. On the surface, this may seem like an error but it is a strategy used
by a number of functional languages called _currying_. If a function is defined
with two or more arguments, it will parse the function name (a symbol) and the
first argument as an expression. It then returns a new function, a composition
of the function and argument which will accept one more argument.

Consider the function `add`,

```haskell
add x y = x + y
```

Suppose we want to calculate `add 1 2`. $fexpr$ will consume `add 1` and
evaluate a new function that takes one argument and adds one to it. So `add 1
2` would become $(\lambda y \rightarrow 1 + y)\ 2$.

## Reading Files, Interactive Prompt and GNU Readline
This section was implemented entirely in the $Main$ module. This module differs
form others in that it adopts an imperative style rather than functional, since
the Main module defines a sequential set of actions which is easier to express
in imperative programming

The interactive prompt and interpreter are very similar in their
implementation. They only differ in that the interactive prompt uses recursive
calls to loop until and it receives an EOF.

Implementing the GNU Readline library was a relatively simple task. I used the
[Haskeline][haskeline] library which provides an interface to GNU Readline.
Very little needed to be changed in the Main module for Readline support. I
swapped out Haskell's built in `getLine` and `putStrLn` with `getInputLine`
and `outputStrLn` respectively, both provided by Haskeline. Finally I wrapped
the main loop for the interactive prompt in an `InputT` monad transformer that
carries the state and settings required by GNU Readline.

# Evaluation

## Hspec Test Suite
I used the [Hspec](http://hspec.github.io) library for testing, a testing
framework for Haskell. Hspec is heavily influenced by Ruby's RSpec library. The
DSL provided by Hspec is almost identical to that of RSpec. I have experience
using RSpec in Ruby from another project, which was a factor in my choice of
Hspec over some of the other Haskell testing frameworks like QuickCheck and
HUnit. Hspec also integrates well with HUnit and QuickCheck in case they
provided some functionality not available in Hspec.

Luckily testing is integrated into _cabal_, Haskell's build system. I added a
_test-suite_ build target to the project `.cabal` file then compiled the
project using

```sh
cabal configure --enable-tests
cabal build
cabal test
```

After compilation, unit tests could be run at any point using `cabal test`, it
will recompile if needed.

As mentioned before, Hspec's DSL is very similar to Ruby's RSpec. The DSL was
designed to be human readable, it uses verbose syntax that almost seems like
the vernacular. For each test it requires a 'describe' field to describe the
function followed by many 'tasks'. A sample from the test-suite of this
project.

```haskell
testSymbol :: SpecWith ()
testSymbol = describe "Parser.Syntax.variable" $ do
  it "parses a variable, lower-case alpha-numeric word" $
    parse variable env "a" `shouldBe` Right "a"

  it "parses an operator" $
    parse variable env "(!!)" `shouldBe` Right "!!"
```

In this example, the test describes the function $Parser.Syntax.variable$ which
should be able to parse a variable (an alpha-numeric word beginning with a
lower-case letter) and an operator (any series of special characters). The
`shouldBe` function is the equivalent of an `assertEquals()` call in
traditional testing frameworks.

The parser has a return type of `Either ParseError Expr`. If the parser
successfully consumes the input, it will return a `Right Expr` which represents
a syntax tree. If the parse fails, it will return `Left ParseError` which
bundles any error messages with the source position of the error. A sample from
an interactive session:

```haskell
vlad> (
"<interactive>" (line 1, column 2):
unexpected end of input
expecting "(", identifier, type, "[", character, literal string, float, expecting integer, "-", "\\", "let", "if" or ")"
```

While the `ParseError` type is great for reporting error, it is very difficult
to test against. To test for a failing case would require predicting the exact
character the parser fails on and supply the line and column number. Having to
specify the source location and exact error messages in tests may lead to more
errors. To combat this, I used the `shouldSatisfy isLeft` combinator, which
returns true if it returns any error. 

```haskell
testSymbol :: SpecWith ()
testSymbol = describe "Parser.Syntax.variable" $ do
  it "should not parse a type, a word beginning with an uppercase letter" $
    parse variable env "Type" `shouldSatisfy` isLeft
```

This is not ideal, but it works. Perhaps it can be improved on in the future by
defining a function to ignore the source position and error messages, only
exposing the sole reason for a failure.

# Conclusions
The goals of this project were to

-   explore the concept of creating a language
-   select a suitable development tool for the job
-   develop a prototype using parser combinators
-   obtain an understanding of automatic differentiation so it can be
    integrated into the language in the future.

I spent the early stages of the project comparing and contrasting parsing
libraries. I mainly focused on the [Parsec][parsec] and
[Attoparsec][attoparsec] libraries in this report, but there were more I
considered before choosing to go with a Monadic Parser Combinator.
[Happy](https://www.haskell.org/happy/) is a Parser Generator used in the
Glasgow Haskell Compiler (GHC), this tool acts like 'yacc' for C as it takes an
annotated BNF specification of a grammar and produces a parser. Similarly
[Alex](https://www.haskell.org/alex/) is a lexical analyser generator for
Haskell, which is a tool similar to 'lex' or 'flex' for C/C++. I dismissed
these options early on in the research as they lacked the flexibility offered
by their Monadic counterparts and the produced parser may not take full
advantage of Haskell's unique feature set. Development of Happy seems to have
slowed down since 2010.

I believe I made the right choice in choosing Parsec over the alternatives. The
library is very flexible due to the combinator design pattern and it is easily
extensible as shown in the `Parser.Lexer` module where I replaced the integer
lexeme with my own implementation. It makes use of many of Haskell's features,
like using the 'Either' type class to propagate errors through the parser until
it is safe to report the error, keeping the parser code functionally pure.

Parsec is efficient, not quite as efficient as Attoparsec but the features it
provides outweigh the performance boost of Attoparsec. The difference in
performance between the two is negligible for the small files as one would
expect for a programming language. Attoparsec would excel as a log parser or
network data parser where file sizes are large or continuous.

The error reporting features that Parsec provides make it ideal for a
programming language interpreter. It provides clear and helpful error messages
as would be expected in any modern programming language.

The use of the `<|>` combinator in the parser definition makes the code easy to
reason about and easy to extend or modify. The syntax resembles that of a
formal grammar making it simple to translate from a formal grammar to a parser.

In my research on the topic of automatic differentiation, I have gained a
wealth of knowledge on the subject. Most of this learning was obtained from
the various papers on the topic, in particular, Philipp Hoffmann's 'A
Hitchhiker's Guide to Automatic Differentiation'[@hitchhiker-paper] which
provides an excellent starting point for understanding automatic
differentiation.

Another factor in my learning of the subject was using the ad library for
simple problems like optimising low dimensional problems.

In the future, I would like to take what I have learned from these sources and
apply them to the language. I would like to have the ad package integrated into
the project by August 2016. Although next on the agenda is to flesh out the
syntax evaluation.

# References

[ad]: http://hackage.haskell.org/package/ad "ad: Automatic Differentiation"

[Jacobian]: https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant "Jacobian matrix and determinant"
[parsec]: https://hackage.haskell.org/package/parsec "parsec: Monadic parser combinators"
[HM]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.9348&rep=rep1&type=pdf "Generalizing Hindley-Milner Type Inference Algorithms"

[attoparsec]: http://hackage.haskell.org/package/attoparsec
[error]:      https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Error.html

[data-reify]: https://hackage.haskell.org/package/data-reify

[token]: https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Token.html

[haskeline]: https://hackage.haskell.org/package/haskeline

<!-- References -->

[^ad-tools]: Autodiff.org <http://www.autodiff.org/?module=Tools&language=ALL>
[^python-ad]: Python Automatic Differentiation package <https://pypi.python.org/pypi/ad/1.2.3>
[^ADIC]: ADIC: Automatic Differentiation for C and C++ <http://trac.mcs.anl.gov/projects/ADIC>
[^DiffSharp]: DiffSharp: Differentiable Functional Programming <http://diffsharp.github.io/DiffSharp/>
[^ad-git-hist]: Commit history for the ad package <https://github.com/ekmett/ad/graphs/contributors>

[^ecosystem]: Gabriel Gonzalez' [State of the Haskell ecosystem][ecosystem-link]
[^parsec-paper]: Daan Leijen [Parsec, a fast combinator parser][parsec-paper]
[^hitchhiker-paper]: Philipp H. W. Hoffmann [A Hitchhiker’s Guide to Automatic Differentiation][Hitchhiker]

[^ad-readme]: Edward Kmett - ad README.markdown <https://github.com/ekmett/ad/blob/master/README.markdown>
[^ad-cabal]: Edward Kmett - ad.cabal - data-reify listed as dependency for ad <https://github.com/ekmett/ad/blob/master/ad.cabal#L110>

[^alternative]: Shown by the definition of $Alternative$ - `class Applicative f => Alternative f where` $\dots$ -
    <http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#g:2>

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
<!-- vim: set makeprg=./mkReport.sh -->
