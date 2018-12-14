
# Table of Contents

1.  [html-tools-mode](#org583322b)
    1.  [Introduction](#orgd6323ae)
    2.  [What's the idea](#org652fd91)
    3.  [What is NOT the idea](#org1aeebc3)
    4.  [Know Issues](#orgb5d14c8)
        1.  [Attempt to nest DWIM inline tags doesn't always respect correct nesting](#org647e435)


<a id="org583322b"></a>

# html-tools-mode

html-tools-mode is an emacs minor mode which extends [web-mode](http://web-mode.org/) to
allow quick formatting of existing text/html files into html5.

Those were originally a bunch of **ad hoc** Emacs Lisp functions
written to speed up formatting of already existent entries for the
[Instituto de Arte Contemporáneo](http://iac.org.es) website.


<a id="orgd6323ae"></a>

## Introduction

Ever had to format legacy pages to move the content to a new design?
Had to work with given text of unknow origin?


<a id="org652fd91"></a>

## What's the idea

The idea is quickly wrapping regions, words, or whatever in the
elements they should have under your needs.


<a id="org1aeebc3"></a>

## What is NOT the idea

This is not:

-   a template tool replacement
-   a DOM model manipulator


<a id="orgb5d14c8"></a>

## Know Issues


<a id="org647e435"></a>

### Attempt to nest DWIM inline tags doesn't always respect correct nesting

Workaround: use regions to enclose everything correctly if you
already have tags in your text.
