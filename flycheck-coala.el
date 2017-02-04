;;; flycheck-coala.el --- Integrate coala with flycheck

;; Copyright (c) 2015 Alex Murray
;;
;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/coala/coala-emacs
;; Version: 0.9.0
;; Package-Requires: ((flycheck "0.24"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Integrate coala with flycheck


;;; Code:
(require 'flycheck)
(require 'json)

(defun flycheck-coala-severity-to-level (severity)
  "Convert the SEVERITY from coala to a flycheck level type."
  (pcase severity
    (1 'info) ; INFO in coala
    (2 'warning) ; NORMAL in coala
    (3 'error)  ; MAJOR in coala
    (_ 'info)))

(defun flycheck-coala-parse-json (output checker buffer)
  "Parse coala-json errors from OUTPUT via CHECKER for BUFFER."
  (let ((errors)
        (results (cdr (assoc 'results
                             (let ((json-array-type 'list))
                               (json-read-from-string output))))))
    ;; iterate through members of results since each is from a separate section
    ;; of the configuration
    (dolist (section results)
      (dolist (err (cdr section))
        (let ((err-end (car (car (cdr (assoc 'affected_code err))))))
          (push (flycheck-error-new
                 :buffer buffer
                 :checker checker
                 :filename (cdr (assoc 'file err-end))
                 :line (cdr (assoc 'line err-end))
                 :message (format "[%s]: %s" (car section) (cdr (assoc 'message err)))
                 :level (flycheck-coala-severity-to-level
                         (cdr (assoc 'severity err)))) errors))))
    errors))

(flycheck-define-checker coala
  "A checker using coala.
See URL `https://coala.io'."
  :command ("coala" "--json" "--find-config" "--files" source)
  :error-parser flycheck-coala-parse-json
  :modes (c-mode
          csharp-mode
          c++-mode
          cmake-mode
          css-mode
          csv-mode
          cuda-mode
          coffee-mode
          dart-mode
          dockerfile-mode
          fortran-mode
          go-mode
          html-mode
          haskell-mode
          json-mode
          js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode
          java-mode
          jinja2-mode
          julia-mode
          lua-mode
          markdown-mode
          matlab-mode
          objc-mode
          octave-mode
          opencl-mode
          org-mode
          php-mode php+-mode
          plsql-mode
          perl-mode cperl-mode
          po-mode
          puppet-mode
          python-mode pip-requirements-mode
          R-mode
          rst-mode
          ruby-mode enh-ruby-mode
          scss-mode
          sh-mode
          sql-mode
          scala-mode
          swift-mode swift3-mode
          latex-mode plain-tex-mode
          text-mode
          typescript-mode
          vhdl-mode
          verilog-mode
          xml-mode nxml-mode
          yaml-mode))

(add-to-list 'flycheck-checkers 'coala)

(provide 'flycheck-coala)

;;; flycheck-coala.el ends here
