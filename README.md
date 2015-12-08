# Symbolic Execution of Concurrent Programs under Arbitrary Memory Models

Authored by Aaron Bembenek and Hannah Blumberg for the Fall 2015 version of CS 252r at Harvard University.

## Overview

This project is a prototype demonstrating the symbolic execution of programs written in a simple concurrent, shared-memory language under arbitrary memory models.

The source code consists of the following files:

* **assumptions.ml**: interfaces with the SMT solver
* **ast.ml**: the abstract syntax tree of our toy language
* **eval.ml**: the actual symbolic interpreter
* **lex.mll**: the lexer for our language
* **log.ml**: provides mechanisms to log errors encountered during symbolic execution
* **mmodel.ml**: the implementations for the memory models, which in our case are the strictly consistent memory model, the Progressive Java Memory Model (PJMM), and what we call the restricted PJMM
* **parse.mly**: the parser for our language
* **prettyprint.ml**: a pretty-printer for our language
* **state.ml**: contains data structures representing program state during symbolic execution

## Running the Code

Our code is written in OCaml and requires the following dependencies:

* ocamlfind
* Alt-Ergo Zero
* cppo
* Yojson

They can be installed via `opam install ocamlfind aez cppo yojson`.

The symbolic execution engine is parameterized over the memory model, which must be selected at compile time. We provide the following `make` options:

* `make pjmm` compiles the symbolic execution engine configured to use the PJMM
* `make rpjmm` compiles the engine configured to use the restricted PJMM
* `make strict` compiles the engine configured to use the strictly consistent memory model

The symbolic executor can be run on a program written in our toy language by `./interp [filename]`.

## The Tests

We provide a suite of tests that demonstrate how the symbolic executor performs differently under the different memory models (i.e., whether it is able to catch certain concurrency bugs). The baseline is the PJMM, which is the weakest memory model we implemented.  If a test fails, it means that there is a bug that would be caught if the symbolic execution engine were configured to use the PJMM, but is not caught given the current memory model in use.

The tests can be run via `make test`.
