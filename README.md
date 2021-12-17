# sly-clpm - CLPM integration for SLY

External contrib for [SLY](https://github.com/joaotavora/sly) that provides basic support for the [Common Lisp Package Manager(CLPM)](https://gitlab.common-lisp.net/clpm/clpm).
It aims to provide helper functions for all basic CLPM usage trough a set of functions which also are added to the SLY repl shortcut menu.

## Usage

sly-clpm will try to detect dominating `clpmfile`, but it's possible to specify which file to use too.
There are mainly to ways to use these functions:

**1. Call functions from `M-x` interface**

After starting SLY functions can be called from the `M-x` interface or be bound to a keybinding.
For example to activate context for the current project `M-x sly-clpm-activate-context` should be used.

**2. Call functions from SLY shortcut menu**

Every function is also available from the SLY shortcut menu.
To acces the menu press `,` in the SLY REPL.
For example to activate context for the current project use `, clpm activate context`.

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

## Functions

Currently thes functions are implemented:

- `sly-clpm-bundle-init` setup CLPM in project, prompts for clpmfile and .asd files
- `sly-clpm-install-context` install context, use dominating clpmfile if present
- `sly-clpm-install-from-source` install project / system from configured source
- `sly-clpm-active-contex` return active context
- `sly-clpm-activate-context` activate bundle context from clpmfile
- `sly-clpm-activate-global-context` activate global context
