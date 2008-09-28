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

{docs make-container}
{docs size}
{docs total-size}
{docs empty!}
{docs empty-p}

### General use: adding, deleting, finding

{docs some-item-p}
{docs every-item-p}
{docs some-element-p}
{docs every-element-p}
{docs every-item-p}
{docs every-key-value-p}
{docs some-key-value-p}

{docs iterate-nodes}
{docs iterate-elements}
{docs iterate-elements-stably}
{docs iterate-keys}
{docs iterate-key-value}
{docs iterate-key-value-stably}

{docs collect-items}
{docs collect-nodes}
{docs collect-elements}
{docs collect-keys}
{docs collect-key-value}
{docs collect-elements-stably}
{docs collect-key-value-stably}

{docs count-elements}
{docs count-items}
{docs count-elements-if}
{docs reduce-elements}
{docs reduce-nodes}

{docs delete-item}
{docs delete-element}
{docs delete-node}

{docs nth-item}
{docs nth-element}
{docs item-at}
{docs item-at-}1

{docs find-item}
{docs find-node}
{docs find-element}

{docs search-for-element}
{docs search-for-key}
{docs search-for-matching-node}
{docs search-for-node}

{docs dimensions}
{docs enqueue}
{docs dequeue}
{docs push-item}
{docs pop-item}

{docs element}
{docs has-children-p}
{docs make-node-for-container}

### Iterators

{docs finish}
{docs move-p}
{docs element-passes-p}
{docs move}
{docs advance}
{docs current-element}
{docs current-element-p}
{docs iterate-forward}
{docs move-forward} }
{docs move-forward-to-next-element}
{docs next-element}
{docs reset}

{docs base-class-for-iteratee}
{docs class-for-contents-as}
{docs setup-initial-container}
{docs make-internal-iterator}
{docs move-internal}

{docs open-file-for-iterator}

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
