;; -*- lexical-binding: t -*-

(require 'f)
(let ((coala-emacs-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path coala-emacs-dir)
  (add-to-list 'process-environment (format "PYTHONPATH=%s" coala-emacs-dir)))
(require 'flycheck-coala)
