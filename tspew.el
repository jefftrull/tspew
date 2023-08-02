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
(require 'cl-lib)

(defgroup tspew nil
  "Display C++ compilation results more cleanly.
Suggested usage: (add-hook 'compilation-mode-hook 'tspew-mode)
"
  :group 'compilation :group 'programming)

;; the beginnings of tspew-mode
;; first, a syntax table for error messages
(defvar tspew-syntax-table (standard-syntax-table)
  "Syntax table for lexing compiler errors" )
;; modify to suit our needs
;; BOZO may be unnecessary - default syntax table appears to do these things already:
;; left and right angle brackets are a kind of parentheses in type names
(modify-syntax-entry ?< "(>" tspew-syntax-table)
(modify-syntax-entry ?> ")<" tspew-syntax-table)
;; colon is a "symbol constituent" - usable in identifiers
(modify-syntax-entry ?: "_" tspew-syntax-table)
;; & and * can be present after any type. symbol constituent seems right
(modify-syntax-entry ?& "_" tspew-syntax-table)
(modify-syntax-entry ?* "_" tspew-syntax-table)

;; now we can use (with-symbol-table tspew-syntax-table (movement-fn))

;; we need a grammar for several reasons:
;; 1) to resolve the fact that angle brackets may appear in operator overloads
;; 2) for iterating over template parameters I want "XXX<YYY>" to be a single item
;;    e.g. <int, std::allocator<int>> should be an sexp of length 2, not 3,
;;    with elements "int" and "std::allocator<int>"
;;    - although maybe this is OK? We have to stop before emitting "<" anyway

(defvar tspew-indent-level c-basic-offset
  "Indentation amount for types in error messages")

(defvar tspew-default-fill-width 100
  "Default maximum width of error type display, in columns
If the compilation window is visible, its width will be used instead")

;; captured initial column width
(defvar tspew--fill-width nil
  "Max width in columns for current run")

;; here I try to implement a two-part pretty-printing system (that is,
;; both indentation and "fill") as described in a paper by Rose and Welsh
;; (1981), which is paywalled, but there is a nice description of it and
;; related work in "the PretzelBook", see
;; http://www.literateprogramming.com/pretzelbook.pdf

;; the "scanner" (front end) part of the system
(defun tspew--scan ()
  "Scan tokens, supplying length information to the back end"
  (with-syntax-table tspew-syntax-table
    ;; tokenize
    (let* ((start (point))
           (syntax (char-syntax (char-after)))
           (tok
            ;; "consume" (by moving point) and return next token
            (cl-case syntax
              (?.
               ;; punctuation is passed straight through
               (skip-syntax-forward ".")
               (let ((punct-end (point)))
                 ;; skip trailing whitespace
                 (skip-syntax-forward " ")
                 (buffer-substring start punct-end)))
              (?\(
               ;; supply just the open "paren" (of whatever type)
               (forward-char)
               (buffer-substring start (point)))
              (?\)
               (forward-char)
               ;; closing "paren" may be followed by whitespace
               ;; consume it *if* followed immediately by another closing paren
               (when (and (not (eobp)) (equal (char-syntax (char-after)) ?\s))
                 (skip-syntax-forward " ")
                 (when (not (equal (char-syntax (char-after)) ?\)))
                   ;; NOT another close paren. supply whitespace as next token.
                   (skip-syntax-backward " ")))
               (string (char-after start)))
              (?\s
               ;; whitespace not following punctuation or closing paren
               ;; preserve for readability
               (skip-syntax-forward " ")
               (buffer-substring start (point)))
              (t
               ;; grab the next sexp
               (forward-sexp)
               (buffer-substring start (point))))))

      ;; send token to indent/fill engine
      (tspew--print tok)

      ;; optionally send some control information
      ;; we send three kinds:
      ;; "internal break" - a spot to put a newline in between sequential elements, if needed
      ;; "enter hierarchy" - a parenthesized expression begins of specified length
      ;; "exit hierarchy" - a parenthesized expression ends

      (cond
       ((equal tok ",")
        (tspew--print 'intbrk))    ;; optional newline between elements

       ((equal (char-syntax (string-to-char tok)) ?\()
        ;; we just entered a parenthesized expression
        (tspew--print
         (cons
          'enter                   ;; beginning of new hierarchy level
          (-                       ;; supply length
           ;; (this calculation will be pessimistic by the amount of whitespace)
           ;; locate the end of the parenthesized expression beginning here
           (save-excursion
             (backward-char)       ;; start at open "paren"
             (forward-sexp)        ;; skip over balanced parens
             (point))
           (point)))))

       ((equal (char-syntax (string-to-char tok)) ?\))
        (tspew--print 'exit))))))  ;; exit hierarchy level

;; the "printer" (back end)
;; maintains a stack reflecting the current indentation level
(defvar-local tspew--indentation-stack
  "Maintains the indentation levels in a template parameter hierarchy.
Each element is a dotted pair of:
1) the current indentation level in columns
2) whether we are splitting the elements of this level one per line" )

(defvar-local tspew--indented-result
  "Accumulates the indented output of the scanner/printer combination" )

(defvar-local tspew--space-remaining
  "The number of columns remaining before tspew--fill-width" )

;; BOZO add these to init

(defun tspew--print (cmd)
  "\"print\" tokens while maintaining appropriate indentation"
  ;; the printer maintains the current indentation level and decides when it's
  ;; necessary to start putting out sequence elements on separate lines.
  ;; It maintains a stack of ('brksym . indent) pairs giving for each level
  ;; what the amount of indentation is and whether we are currently breaking
  ;; up the sequence with newlines
  (cl-typecase cmd
    (string
     ;; a plain token to output unconditionally
     (message (format "string: %s" cmd))
     ;; append and update column counter
     (setq tspew--indented-result
           (concat tspew--indented-result cmd))
     (setq tspew--space-remaining (- tspew--space-remaining
                                     (length cmd))))

    (cons        ;; an "enter" - push mode for this level
     (cl-assert (equal (car cmd) 'enter))
     (message "cons")
     (let ((len (cdr cmd))
           (indentation (cdar tspew--indentation-stack)))
       (if (or (< len tspew--space-remaining)
               (equal len 1))   ;; trivial (empty) parens
           (progn
             (message (format "enough room: len %d vs. space remaining %d"
                              len tspew--space-remaining))
             ;; there is room enough to print the rest of this sexp
             ;; don't require line breaks
             (push (cons 'no-break indentation) tspew--indentation-stack)
             )
         (setq indentation (+ indentation tspew-indent-level))
         ;; new space remaining: whatever is left after indentation
         (setq tspew--space-remaining (- tspew--fill-width indentation))
         ;; require elements at this level to break/indent
         (push (cons 'break indentation) tspew--indentation-stack)
         ;; output line break and indent
         (setq tspew--indented-result
               (concat tspew--indented-result
                       "\n"
                       (make-string indentation ?\s)))
         )))

    (symbol
     (cl-case cmd

       ('exit
        (message "exit")
        ;; BOZO
        ;; here I used to add another newline if we were previously breaking in between
        ;; the purpose AFAICT was to ensure "decltype" got its own line break
        ;; unfortunately this means we don't get ">>>" etc. at the end of nested parens
        ;; so we need another solution for this case
        (pop tspew--indentation-stack))

       ('intbrk
        (message "intbrk")
        (when (equal (caar tspew--indentation-stack) 'break)
          ;; we have a sequence element and previously decided to split one per line
          ;; break and indent to current level (for a new sequence element)
          (setq tspew--space-remaining (- tspew--fill-width (cdar tspew--indentation-stack)))
          (setq tspew--indented-result
                (concat tspew--indented-result
                        "\n"
                        (make-string (cdar tspew--indentation-stack) ?\s)))))))
     ))

(defun tspew--handle-type-region (end)
  "Fill and indent region starting at point containing a type
or part of a function."

  ;; initialize indent+fill machinery
  (setq tspew--indentation-stack '((no-break . 0)))  ;; current indent level info
  (setq tspew--space-remaining tspew--fill-width)    ;; tracking horizontal space
  (setq tspew--indented-result "")                   ;; the result

  ;; send one token at a time, inserting indentation and line breaks as required
  (save-excursion
    (while (not (equal (point) end))
      (cl-assert (<= (point) end))
      (tspew--scan)))

  tspew--indented-result
)

(defun tspew--next-type-chunk (limit)
  "Return the end of the next portion of a type, or limit if none.
Leaves point at the start of the chunk."
  (skip-syntax-forward " ")
  (if (equal (point) limit)
      limit
    (save-excursion
      (when (looking-at "decltype\\|const")
        (forward-word)
        (skip-syntax-forward " "))
      (with-syntax-table tspew-syntax-table
        (if (equal (point) limit)
            limit
          (forward-sexp)
          ;; grab following parenthesized expression, if any
          (if (and (not (equal (point) limit)) (equal (char-syntax (char-after)) ?\())
              (forward-sexp))))
      (point)))
)

(defun tspew--handle-chunks (tstart tend)
  "Fill and indent a series of \"chunks\" (whitespace-separated bits of a quoted type expression
within an error message)"
  (save-excursion
    ;; the line this type is on exceeds the desired width
    ;; so we will create a reformatted version
    (let ((result "\n"))
      ;; break lines at "chunk boundaries" within the contents, if any (such as in a function signature)
      ;; those are spaces between major sections of a function signature, like "decltype (...)"
      ;; that are best placed on a separate line for readability
      (goto-char tstart)
      (while (not (equal (point) tend))
        ;; get the next chunk (point through tint)
        (let ((tint (tspew--next-type-chunk tend)))
         ;; fill and indent
          (message (format "chunk extends from %d to %d (end %d)" tstart tint tend))
          (setq result
                (concat result (tspew--handle-type-region tint) "\n"))
          (goto-char tint)
          ))
      result)))

(defun tspew--handle-type (tstart tend)
  "Fill and indent a single type within an error message"
    ;; create an overlay covering the type
  (let ((ov (make-overlay tstart tend))
        (result (tspew--handle-chunks tstart tend)))

      ;; make existing contents invisible
      (overlay-put ov 'invisible t)

      ;; display indented and filled types in place of the original
      (overlay-put ov 'before-string result)

      ;; remember overlay
      ;; [JET] I initially kept a list of overlays and used that, but compilation-mode
      ;; calls kill-all-local-variables, which deletes the buffer-local value
      ;; of my list. So instead, we use properties:
      (overlay-put ov 'is-tspew t)))

(defun tspew--handle-line (lstart lend)
  "Process a single line of error output"
  ;; lstart is a position, lend is a marker
  ;; is this an error message with a type?
  (let* ((err-regexp (cadr (assoc 'gnu compilation-error-regexp-alist-alist)))
         ;; types are enclosed by Unicode left and right single quotes
         ;; but sometimes non-type (or function) things are in quotes
         ;; a prefix is necessary to distinguish them

         ;; experiment: forget about the prefix. Any quoted expression is a type...?
         ;; (type-prefix-regexp "\\(?:error:\\|warning:\\|member\\|type\\|note:\\)[ ]+")
         (type-prefix-regexp "")
         ;; some surprising things can be in type names, because of "operator"
         (quoted-type-regexp "\u2018\\([]\[[:alnum:]:()<>,&_ =+/*%^.-]+\\)\u2019")
         (type-regexp (concat type-prefix-regexp quoted-type-regexp))
         )
    (save-excursion
      (goto-char lstart)
      (if (and (looking-at err-regexp)  ;; error line
               ;; the line is too long
               (>= (- (line-end-position) (line-beginning-position)) tspew--fill-width))
        ;; while there is still a match remaining in the line:
        (while (re-search-forward type-regexp lend t)
          (let ((tend (match-end 1)))
            ;; process this type match
            (tspew--handle-type (match-beginning 1) tend)
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

(defun tspew--remove-overlays ()
  (let ((win (get-buffer-window)))
    (setq-local tspew--fill-width
                (if win (window-body-width win) tspew-default-fill-width)))
  (let ((overlays (seq-filter (lambda (ov) (overlay-get ov 'is-tspew))
                              (overlays-in (point-min) (point-max)))))
    (dolist (ov overlays)
      (delete-overlay ov)))
  )

(defun tspew--parse-initialize (proc)
  "Reset compilation output processing"

  (tspew--remove-overlays)
  (setq tspew--parse-start nil)
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

;; TSpew is a minor mode for compilation buffers, not source code
;; To use it you need to enable it after a compilation buffer is created,
;; and they are not created until compilation begins. So you must tell
;; compilation-mode to do it for you using compilation-mode-hook.
;; For example:
;; (add-hook 'compilation-mode-hook 'tspew-mode)
;; will enable tspew for all compiles. You may prefer to restrict it to
;; certain projects instead by writing your own hook.

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
