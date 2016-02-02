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

\pagebreak

# Introduction
- Topic: Introduction to the topic addressed in the project
- Motivation: Why would one care about the problem and the results? Cite
    appropriate references in this section. Explain the high-level, abstract
    problem that your project addresses. Explain what you are trying to achieve
    in a way that leads naturally into the next section.
- Problem statement: Describe the technical problem needed to be solved in your
    project. Note that most projects solve both a more abstract, high-level
    problem and a specific, technical problem: your problem statement is the
    detailed technical problem (your motivation should cover the more abstract
    high-level problem).
- Approach: summarise how you addressed solving the problem. Provide an
    overview of how you analysed the problem, how you designed a solution, and
    how you evaluated your solution. (e.g. use of models, simulation,
    prototypes, real-world experiments, cases- studies, etc.). What important
    variables did you control, ignore, or measure in your evaluation.
- Metrics: describe how are you going to evaluate your work.
- Project: list, and briefly describe your significant achievements in the
    project (probably 3-5 of these in a typical project). If you have come up
    with any contributions to the state-of-the-art, make sure to include them
    here.

# Technical Background
- The purpose of this chapter is to show your depth and breadth of reading and
    understanding of the problem domain 
- Include two sections:
    - Topic material (research material, if used, from published journals and
        conference proceedings; less academic publications, if required by the
        project, from other sources) – for example, what other work researchers
        have done already in this area, what results they have produced, what
        work has been done in related areas, what software already exists to
        solve this or similar problems, etc.
    -  Technical material (from any source: including books, websites) – for
        example, how to write a web server, how to use speci c Java features,
        how to use Ajax, how to use UML to validate your design, etc.
- Note that material relating to the motivation or non-technical background
    should NOT go here, but rather in the introduction

# The Problem
- The purpose of this chapter is to clearly explain the technical problem
    and/or identify the user requirements
- Provide any model(s) of the problem (e.g. equations, ERD’s, UML Use Cases
    & Scenarios, Activity Diagrams, etc.)
- Provide any analysis of the problem, leading to a greater understanding
- There should be no decisions made in this chapter

# The Solution
-   The purpose of this chapter is to clearly identify, discuss, and justify the
    decisions you make
-   Depending on your type of project, you may not need to include all of these
-   Anaytical Work: E.g. Equations, etc. that describe your solution
-   Architectural Level: E.g. Implementation Diagrams
-   High-Level Design: E.g. Packages, Class Diagrams, etc.
-   Low-Level Design: E.g. Method speci cations, Algorithms, etc.
-   Implementation: discuss anything interesting here, put full source code in
    an appendix or attachment

\pagebreak

## Choosing a Parser Library
Haskell provides to prominent parser Libraries [attoparsec][attoparsec] and
[parsec][parsec]. Both libraries implement _parser combinators_ but are
different in their design principles.

### Attoparsec
attoparsec is a fast Haskell parser combinator library, aimed particularly at
dealing efficiently with network protocols and complicated text/binary file
formats.[^attoparsec-github]

Attoparsec focuses on high performance parsing of large amounts of raw data or
working with binary file formats. Attoparsec can work with `ByteString`s,
a more efficient way of representing Strings as Byte Strings rather than Lists
of Characters.[^string]

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

[attoparsec]: http://hackage.haskell.org/package/attoparsec
[error]:      https://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec-Error.html
[parsec]:     http://hackage.haskell.org/package/parsec

[^attoparsec-github]: attoparsec readme file.
    <https://github.com/bos/attoparsec/blob/master/README.markdown>
[^parsec-package]: Parsec Hackage page
    <https://hackage.haskell.org/package/parsec-3.1.9>
[^string]: `type String = [Char]`
    <https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-String.html#t:String>

# Evaluation
-   Solution Verification
    -   E.g. use your equations to verify the correctness of your solution

-   Software Design Verification: how did you show that your design worked properly?
    -   Using a model of your solution Page 9 of 18
    -   E.g. use UML interaction diagrams to verify each scenario
    -   Software Verification: how did you demonstrate your software worked
        properly?


    - If you have not tested your software, then you cannot rely on
        your results. Clearly describe:
        1. Your test approach (i.e. unit testing, sub-system testing, system
            testing)
        2. Your tests (e.g. scenarios, test cases, test data, etc.)
        3. Your test results
        4. An interpretation of the results

-   Validation/Measurements: how did you measure how well your solution solved
    the problem
    -   Results
    -   Explanation of Results
    -   Analysis of Results
    -   Comparison with previous solutions (if relevant)


# Conclusions
-   Identify and discuss the implications of your work.
-   If you made a contribution to the state-of-the-art, clearly identify it
    here.
-   Discuss whether your results are general, potentially generalizable, or
    specific to a particular case.
-   Identify threats to the validity of your results (e.g. limitations, risks
    introduced by your approach, etc.)
-   Discuss your project approach
-   Discuss future work, based on what you have done (and not done)


# References
-   You must include a list of all cited works here. Use standard guidelines for
    these (e.g. IEEE guidelines3, APA guidelines) as advised by your supervisor.
-   The best references are peer-reviewed publications and books.
-   Note: you must NEVER give just a URL for a reference. At the least it must
    include an author (even if the company’s name), a publisher (may also be the
    company’s name), a title, and a date (if none available, use the date
    last accessed). If you have a URL for the document, include \[<URL>,
    accessed on: <date last accessed>\] in square brackets at the end of
    the reference.
-   Non-reviewed material (such as blogs, Wikipedia, etc.) does not make a
    suitable reference, except in the case of technical material have you have
    been able to verify by checking their implementation (you should state how
    you did this).

    -   Material from other publications (such as journals, newspapers,
        magazines, official reports, etc.) may be used sparingly, especially if
        important for motivating your work.


# Appendices
Include here all extra material, e.g. your source code, project management
(optional) including: the task list, Gantt Chart diagrams (or equivalent),
discussion of any significant deviations from plan, and how you managed them,
discussion of what you would do differently if you repeated the project. 
