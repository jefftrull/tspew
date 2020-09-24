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

;; the beginnings of tspew-mode
;; first, a syntax table for error messages
(defvar tspew-syntax-table (standard-syntax-table)
  "Syntax table for lexing compiler errors" )
(modify-syntax-entry ?< "(>" tspew-syntax-table)
(modify-syntax-entry ?> ")<" tspew-syntax-table)
;; tell it colon is a "symbol constituent" - though we really only want double colons...
(modify-syntax-entry ?: "_" tspew-syntax-table)
;; now we can use (with-symbol-table tspew-syntax-table (movement-fn))

(defvar tspew-indent-level c-basic-offset
  "Indentation amount for types in error messages")

(defvar tspew-default-fill-width 100
  "Default maximum width of error type display, in columns
If the compilation window is visible, its width will be used instead")

;; captured initial column width
(defvar tspew--fill-width nil
  "Max width in columns for current run")

(defun tspew--handle-indented-type-line (line indentation)
  "Fill and indent a line containing an indented portion of a type"
  ;; point is after *indentation* spaces
  ;; if line exceeds window width
  ;;   mark end of this line
  ;;   for each split point between point and eol
  ;;      insert newline and indentation+tspew-indent-level, leaving
  ;;        point after the indentation
  ;;      recursive call with new indentation
  ;;      move to the marker that has the end of the pre-split line
  line
)

(defun tspew--handle-type (tstart tend)
  "Fill and indent a single type within an error message"
  ;; tstart is position, tend is marker
  (save-excursion
    (goto-char tstart)
    ;; the line this type is on exceeds the desired width
    ;; so we will create a reformatted version

    ;; create an overlay covering the type
    (let ((ov (make-overlay tstart tend))
          (contents (buffer-substring tstart tend))
          result)
      ;; make existing contents invisible
      (overlay-put ov 'invisible t)

      ;; break lines at spaces within the contents, if any
      (dolist (line (split-string contents " ") result)
        ;; then process each resulting line
        (setq result
              (concat result "\n" (tspew--handle-indented-type-line line 0) "\n")))

      ;; join the results and store
      (overlay-put ov 'before-string result)

      ;; remember overlay
      ;; I initially kept a list of overlays and used that, but compilation-mode
      ;; calls kill-all-local-variables, which deletes the buffer-local value
      ;; of my list. So instead, we use properties:
      (overlay-put ov 'is-tspew t)))
  )

(defun tspew--handle-line (lstart lend)
  "Process a single line of error output"
  ;; lstart is a position, lend is a marker
  ;; is this an error message with a type?
  (let* ((err-regexp (cadr (assoc 'gnu compilation-error-regexp-alist-alist)))
         ;; types are enclosed by Unicode left and right single quotes
         ;; but sometimes non-type (or function) things are in quotes
         ;; a prefix is necessary to distinguish them
         (type-prefix-regexp "\\(error:\\|warning:\\|member\\|type\\) ")
         (quoted-type-regexp "\u2018\\([]\[[:alnum:]:()<>,&_ ]+\\)\u2019")
         (type-regexp (concat type-prefix-regexp quoted-type-regexp))
         )
    (save-excursion
      (goto-char lstart)
      (if (and (looking-at err-regexp)  ;; error line
               ;; the line is too long
               (>= (- (line-end-position) (line-beginning-position)) tspew--fill-width))
        ;; while there is still a match remaining in the line:
        (while (re-search-forward type-regexp lend t)
          (let ((tend (make-marker)))
            ;; process this type match
            (set-marker tend (match-end 2))
            (tspew--handle-type (match-beginning 2) tend)
            ;; advance past matched text
            (goto-char tend)
            (if (not (eobp))
                (forward-char))))
        ))))


;; remember where we are in the buffer
;; the compilation filter may give us partial lines, so we have to keep track of how far
;; we've come
(defvar-local tspew--parse-start nil
  "Starting point for incremental error parsing." )

(defun tspew--parse-initialize (proc)
  "Reset compilation output processing"
  (setq tspew--parse-start nil)
  (let ((win (get-buffer-window)))
    (if win
        (setq-local tspew--fill-width (window-body-width win))
      (setq-local tspew--fill-width tspew-default-fill-width)))
  (let ((overlays (seq-filter (lambda (ov) (overlay-get ov 'is-tspew))
                              (overlays-in (point-min) (point-max)))))
    (dolist (ov overlays)
      (delete-overlay ov)))
  )

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
        (add-hook 'compilation-start-hook 'tspew--parse-initialize nil t)
        (add-hook 'compilation-filter-hook 'tspew--compilation-filter nil t))
    ;; if we are being toggled off, remove hooks
    (remove-hook 'compilation-start-hook 'tspew--parse-initialize)
    (remove-hook 'compilation-filter-hook 'tspew--compilation-filter)
    (kill-local-variable 'tspew--parse-start)))

;; BOZO should this be tspew-mode?
(provide 'tspew)
