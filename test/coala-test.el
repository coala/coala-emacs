;;; coala-emacs --- tests
;;; Commentary:
(require 'flycheck)
(require 'flycheck-ert)

;;; Code:

;;; Directories

(flycheck-ert-def-checker-test coala python coala-python
  (unwind-protect
      (flycheck-ert-should-syntax-check
       "test/resources/language/pep8.py" 'python-mode
       '(1 nil warning "The code does not comply to PEP8."
           :checker coala))))

(provide 'coala-test)
;;; coala-test.el ends here
