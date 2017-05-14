Flycheck coala Checker
======================
[![Build Status](https://travis-ci.org/coala/coala-emacs.svg?branch=master)](https://travis-ci.org/coala/coala-emacs)

Integrate [coala](https://coala.io) with
[flycheck](http://www.flycheck.org).

Setup
-----

Put this in your Emacs initialization script.

```elisp
(eval-after-load 'flycheck
  '(require 'flycheck-coala))
```

License
-------

This one is licensed GPLv3.

