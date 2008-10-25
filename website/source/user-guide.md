{include resources/ug-header.md}
{set-property title "CL-Containers User's Guide"}
{set-property style-sheet user-guide}
{set-property docs-package cl-containers}

# CL-Containers User's Guide

# Table of Contents 

{table-of-contents :start 2 :depth 3}

## Introduction

Common Lisp ships with a set of powerful built in data
structures including the venerable list, full featured
arrays, and hash-tables. The Common Lisp Container Library
{clcl} enhances and builds on these structures in
two ways:

  1. By adding containers that are not available in native
  Lisp (for example: binary search trees, red-black trees,
  sparse arrays and so on).

  2. By standardizing the container interface so that they
  are simpler to use and so that changing design decisions
  becomes significantly easier.

The containers in {clcl} can be divided into three storage
types: Ordered, Unordered and Associative.

 * Ordered containers store things in (some) order. The order
   may be based on how items were inserted into the container
   or it may depend on an explicit sorting.

 * Unordered containers also store things and share much of
   Ordered containers interface. However, the items in an
   Unordered container do not maintain any particular
   arrangement.

 * Associative containers store items *associated*
   with some index or key. The key may be simple (for
   example, a one-dimensional array indexed by an integer) or
   complex (for example, a nested hash table indexed by
   color, size and object class).

The way containers store their contents provides another view
in {clcl}'s design: containers may store their contents
directly (think of a list) or they may wrap each element
within some data-structure -- a node -- that helps the
container keep track of what is going on (think of a binary
tree). We'll see many examples of both styles below.

There are many other mixins that combine to produce generic
container behavior. For example, a
[bounded-container-mixin][] starts at some fixed size and
cannot grow beyond it whereas a [keyed-container-mixin][]
provide a `key` function that is used by other container
functions (e.g., [sort-container][]) to modify their
behavior.

## Terminology

*Containers* store *elements*. Sometimes the elements are
wrapped in *nodes*. We can *insert*, *delete*, *search-for*,
*find*, *iterate*, and *collect* both elements and nodes from
a container. We can also look at the *size* of a container,
see it is *empty* (with [emptyp][]) and *empty* it (with
[empty!][]). Ordered containers also let us find the *first*,
*last* and *nth* element (or node) and they may let us *sort*
or *reverse* the contents as a whole. Associative containers
provide the ability to look at *keys* and elements (or,
sometimes, *values*). We can collect and iterate either one
of these or both simultaneously. Finally, certain specialized
container classes provide synonyms for their usual
operations. For example, you can still pop and push into a
[stack-container][].

## Using a container

### Creating and inspecting

{include user-guide/creation-and-inspection.mmd}

### General use: adding, deleting, finding

{include user-guide/editing.mmd}

### Some and Every

{include user-guide/querying.mmd}

### Counting, Collecting, and Canvasing

{include user-guide/iteration-and-collection.mmd}

### Finding and Searching

{include user-guide/searching.mmd}

### Miscellaneous

{docs reduce-elements}
{docs reduce-nodes}
{docs dimensions}

### Container taxonomy

{include user-guide/taxonomy.mmd}

### Iterators

{include user-guide/iteration.mmd}

## Indices

### Index of Functions

{docs-index (function macro) function}

### Index of variables

{docs-index variable}

### Full symbol index

{docs-index :all}

<hr>

#### Glossary

{glossary}


#### Footnotes

{footnotes}

{include resources/ug-footer.md}
