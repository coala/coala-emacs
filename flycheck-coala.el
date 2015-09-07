;;; flycheck-coala.el --- Integrate coala with flycheck

;; Copyright (c) 2015 Alex Murray
;;
;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/flycheck-coala
;; Version: 0.1
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))

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
(require 'json)

(defun flycheck-coala-parse-json (output checker buffer)
  "Parse coala-json errors from OUTPUT via CHECKER for BUFFER."
  (let ((errors)
        (results (cdr (assoc 'default
                             (cdr (assoc 'results
                                         (json-read-from-string output)))))))
    (dotimes (i (length results))
      (let ((err (elt results i)))
        (message (format "%S" err))
        (add-to-list 'errors
                     (flycheck-error-new :buffer buffer
                                         :checker checker
                                         :filename (cdr (assoc 'file err))
                                         :line (cdr (assoc 'line_nr err))
                                         :message (cdr (assoc 'message err))
                                         :level 'info))))
    (message (format "%S" errors))
    errors))

(flycheck-define-checker coala
  "A checker using coala.

See URL `http://coala-analyzer.org/'."
  :command ("coala-json" "--find-config" "-f" source)
  :error-parser flycheck-coala-parse-json)



(provide 'flycheck-coala)

;;; flycheck-coala.el ends here
