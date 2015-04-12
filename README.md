# A primitive PYthon ParsER

Incomplete, dirty, with no proper tokenization, full of hacks, workarounds and
dead kitten. Fast enough.

I use it with Helm to jump around a single file. To install drop both
pyper.el and helm-pyper.el somewhere in your load-path and the following into
.emacs:

``` emacs-lisp
(require 'helm)
(require 'pyper)
(require 'helm-pyper)

(define-key python-mode-map (kbd "C-c m") 'helm-pyper)
```
