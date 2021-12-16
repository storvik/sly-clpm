# sly-clpm - CLPM integration for SLY

External contrib for [sly](https://github.com/joaotavora/sly) that provides basic support for the [Common Lisp Package Manager](https://gitlab.common-lisp.net/clpm/clpm).
It aims to provide helper functions for all basic CLPM usage though a set of functions which also are added to the SLY repl shortcut menu.
Some of these functions are:

|-------------------------------------------|---------------------------|
| CLPM function                             | sly-clpm function         |
|-------------------------------------------|---------------------------|
| (clpm-client:bundle-init)                 | sly-clpm-bundle-init      |
| (clpm-client:install :context "clpmfile") | sly-clpm-install-context  |
| (clpm-client:activate-context "clpmfile") | sly-clpm-activate-context |
| (clpm-client:active-context)              | sly-clpm-active-context   |

## Usage

sly-clpm will try to detect dominating `clpmfile`, but it's possible to specify which file to use too.
There are mainly to ways to use these functions:

*1. Call functions from `M-x` interface*
After starting SLY functions can be called from the `M-x` interface or be bound to a keybinding.
For example to activate context for the current project `M-x sly-clpm-activate-context` should be used.

*2. Call functions from SLY shortcut menu*
Every function is also available from the SLY shortcut menu.
To acces the manu press `,` in the SLY REPL.
To activate context for the current project use `, clpm activate context`.

## Installation

Easiest way to install package is using [straight.el](https://github.com/raxod502/straight.el):

``` emacs-lisp
(use-package sly-clpm
  :straight
  (sly-clpm
   :type git
   :host github
   :repo "storvik/sly-clpm"
   :files ("*.el" "*.asd" "*.lisp")))
```
