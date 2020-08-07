;;; tspew.el --- Clean and format "template spew" errors from gcc and Clang

;; Author: Jeff Trull <edaskel@att.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(require 'compile)
(require 'cc-mode)

(defgroup tspew nil
  "Display C++ compilation results more cleanly.
Suggested usage: (add-hook 'compilation-mode-hook 'tspew-mode)
"
  :group 'compilation :group 'programming)

;; BOZO mark things internal properly

;; the beginnings of tspew-mode
;; first, a syntax table for error messages
(defvar tspew-syntax-table (standard-syntax-table)
  "Syntax table for lexing compiler errors" )
(modify-syntax-entry ?< "(>" tspew-syntax-table)
(modify-syntax-entry ?> ")<" tspew-syntax-table)
;; tell it colon is a "symbol constituent" - though we really only want double colons...
(modify-syntax-entry ?: "_" tspew-syntax-table)
;; now we can use (with-symbol-table tspew-syntax-table (movement-fn))

(defun tspew--handle-type (tstart tend)
  "Handle a single type within an error message"
  ;; tstart is position, tend is marker
  (save-excursion
    (goto-char tstart)
    (insert ":TYPE:")
    (goto-char tend)
    (insert-before-markers ":ENDTYPE:"))
  )

(defun tspew--handle-line (lstart lend)
  "Process a single line of error output"
  ;; lstart is a position, lend is a marker
  ;; is this an error message with a type?
  (let* ((err-regexp (cadr (assoc 'gnu compilation-error-regexp-alist-alist)))
         ;; types are enclosed by Unicode left and right single quotes
         ;; but sometimes non-type (or function) things are in quotes
         ;; a prefix is necessary to distinguish these
         (type-prefix-regexp "\\(error:\\|warning:\\|member\\|type\\) ")
         (quoted-type-regexp "\u2018\\([]\[[:alnum:]:()<>,&_ ]+\\)\u2019")
         (type-regexp (concat type-prefix-regexp quoted-type-regexp))
         )
    (save-excursion
      (goto-char lstart)
      (if (looking-at err-regexp)
        ;; while there is still a match remaining in the line:
        (while (re-search-forward type-regexp lend t)
          (let ((tend (make-marker)))
            ;; process this type match
            (set-marker tend (match-end 2))
            (tspew--handle-type (match-beginning 2) tend)
            ;; advance past matched text
            (goto-char tend)
            (if (not (equal (point) (point-max)))
                (forward-char))))
        ))))


;; remember where we are in the buffer
;; the compilation filter may give us partial lines, so we have to keep track of how far
;; we've come
(defvar tspew--parse-start nil
  "Starting point for incremental error parsing." )

(defun tspew--parse-reset (proc)
  "Reset compilation output buffering"
  (setq-local tspew--parse-start nil))

;; create a compilation filter hook to incrementally parse errors
(defun tspew--compilation-filter ()
  "Transform error messages into something prettier."
  ;; Parse from tspew--parse-start to point, or as close as you can get,
  ;; updating tspew--parse-start past the last newline we got.
  ;; Be sure to use "markers" when necessary, as positions are strictly
  ;; buffer offsets and are not "stable" in the iterator sense
  (if (not tspew--parse-start)
      (setq-local tspew--parse-start compilation-filter-start))
  (while (and (< tspew--parse-start (point))
              (> (count-lines tspew--parse-start (point)) 1))
    ;; we have at least one newline in our working region
    (save-excursion
      (goto-char tspew--parse-start)
      (forward-line)
      (let ((line-end-marker (point-marker)))
        ;; process a single line
        (tspew--handle-line tspew--parse-start line-end-marker)
        (setq-local tspew--parse-start (marker-position line-end-marker)))))
)

(define-minor-mode tspew-mode
  "Toggle tspew (Template Spew) mode"
  :init-value nil
  :lighter "TSpew"
  (if tspew-mode
      (progn
        (add-hook 'compilation-start-hook 'tspew--parse-reset nil t)
        (add-hook 'compilation-filter-hook 'tspew--compilation-filter nil t))
    ;; if we are being toggled off, remove hooks
    (remove-hook 'compilation-start-hook 'tspew--parse-reset)
    (remove-hook 'compilation-filter-hook 'tspew--compilation-filter)))

;; BOZO should this be tspew-mode?
(provide 'tspew)
