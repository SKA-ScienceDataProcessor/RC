---
layout: page
permalink: /
title: Radio Cortex Project
description: "Radio Cortex Home"
tags: [radio cortex, haskell]
image:
  feature: lights.jpg
  credit: mcsquaredjr
share: true
---


Here is list of module and their purpose. This is current state of package and things will undoubtedly change.

#### Module DNA

Public interface to the DNA. It provide reexports and DSLs for definition of actors and dataflow graph. It’s not complete and export from the internal modules are required so far.

#### Module DNA.AST

Abstract syntax tree for the functional language. It’s  internal representation which uses De Bruijn indices.

#### Module DNA.Actor

Data type for definition of dataflow graph.

#### Module DNA.Compiler.Basic

Simple checks for the dataflow graph

#### Module DNA.Compiler.Types

Common data type for the compiler. It include monad for error handling and source of fresh names.

#### Module DNA.Compiler.Scheduler

Very simple scheduler for dataflow graph.

#### Module DNA.Compiler.CH

Code generation of for cloud haskell.
