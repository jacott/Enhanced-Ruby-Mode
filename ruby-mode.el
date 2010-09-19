;;; ruby-mode.el --- Major mode for editing Ruby files

;; Copyright (C) 2010
;;   Geoff Jacobsen

;; Author: Geoff Jacobsen
;; URL: http://http://github.com/jacott/Enhanced-Ruby-Mode
;; Created: Sep 18 2010
;; Keywords: languages ruby
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with it.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides fontification, indentation, and navigation for Ruby code.
;;
;; If you're installing manually, you should add this to your .emacs
;; file after putting it on your load path:
;;
;;    (add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") ; must be added after any path containing old ruby-mode
;;    (setq enh-ruby-program "(path-to-ruby1.9.2)/bin/ruby") ; so that still works if ruby points to ruby1.8
;;    (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
;;    (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;    (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
;;
(defcustom enh-ruby-program "ruby"
  "The ruby program to parse the source."
  :group 'ruby)

(defcustom ruby-indent-tabs-mode nil
  "*Indentation can insert tabs in ruby mode if this is non-nil."
  :type 'boolean :group 'ruby)
(put 'ruby-indent-tabs-mode 'safe-local-variable 'booleanp)

(defcustom ruby-indent-level 2
  "*Indentation of ruby statements."
  :type 'integer :group 'ruby)
(put 'ruby-indent-level 'safe-local-variable 'integerp)

(defcustom ruby-hanging-indent-level 1
  "*Extra hanging Indentation for continued ruby statements."
  :type 'integer :group 'ruby)
(put 'ruby-hanging-indent-level 'safe-local-variable 'integerp)

(defcustom ruby-comment-column 32
  "*Indentation column of comments."
  :type 'integer :group 'ruby)
(put 'ruby-comment-column 'safe-local-variable 'integerp)

(defcustom ruby-deep-arglist t
  "*Deep indent lists in parenthesis when non-nil.
Also ignores spaces after parenthesis when 'space."
  :group 'ruby)
(put 'ruby-deep-arglist 'safe-local-variable 'booleanp)

(defcustom ruby-deep-indent-paren '(?\( ?\[ ?\] t)
  "*Deep indent lists in parenthesis when non-nil. t means continuous line.
Also ignores spaces after parenthesis when 'space."
  :group 'ruby)

(defcustom ruby-deep-indent-paren-style 'space
  "Default deep indent style."
  :options '(t nil space) :group 'ruby)

(defcustom ruby-encoding-map '((shift_jis . cp932) (shift-jis . cp932))
  "Alist to map encoding name from emacs to ruby."
  :group 'ruby)

(defcustom ruby-use-encoding-map t
  "*Use `ruby-encoding-map' to set encoding magic comment if this is non-nil."
  :type 'boolean :group 'ruby)

(defconst ruby-symbol-chars "a-zA-Z0-9_=?!")
(defconst ruby-symbol-re (concat "[" ruby-symbol-chars "]"))

(defconst ruby-defun-beg-keywords
  '("class" "module" "def")
  "Keywords at the beginning of definitions.")

(defconst ruby-defun-beg-re
  (regexp-opt ruby-defun-beg-keywords)
  "Regexp to match the beginning of definitions.")

(defconst ruby-defun-and-name-re
  (concat "\\(" ruby-defun-beg-re "\\)[ \t]+\\("
                                         ;; \\. and :: for class method
                                         "\\([A-Za-z_]" ruby-symbol-re "*\\|\\.\\|::" "\\)" 
                                         "+\\)")
  "Regexp to match definitions and their name")

(defface ruby-string-delimiter-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight string delimiters like \" and %Q."
  :group 'ruby)

(defface ruby-heredoc-delimiter-face
  '((t :foreground "PeachPuff3"))
  "Face used to highlight string heredoc anchor strings like <<END and END"
  :group 'ruby)

(defface ruby-regexp-delimiter-face
  '((t :foreground "goldenrod"))
  "Face used to highlight regexp delimiters like / and %r."
  :group 'ruby)

(defface ruby-op-face
  '((t :foreground "dark violet" :bold t))
  "Face used to highlight operators like + and ||"
  :group 'ruby)


(defun ruby-mode-set-encoding ()
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "[^\0-\177]" nil t)
      (goto-char (point-min))
      (let ((coding-system
             (or coding-system-for-write
                 buffer-file-coding-system)))
        (if coding-system
            (setq coding-system
                  (or (coding-system-get coding-system 'mime-charset)
                      (coding-system-change-eol-conversion coding-system nil))))
        (setq coding-system
              (if coding-system
                  (symbol-name
                   (or (and ruby-use-encoding-map
                            (cdr (assq coding-system ruby-encoding-map)))
                       coding-system))
                "ascii-8bit"))
        (if (looking-at "^#!") (beginning-of-line 2))
        (cond ((looking-at "\\s *#.*-\*-\\s *\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)\\s *\\(;\\|-\*-\\)")
               (unless (string= (match-string 2) coding-system)
                 (goto-char (match-beginning 2))
                 (delete-region (point) (match-end 2))
                 (and (looking-at "-\*-")
                      (let ((n (skip-chars-backward " ")))
                        (cond ((= n 0) (insert "  ") (backward-char))
                              ((= n -1) (insert " "))
                              ((forward-char)))))
                 (insert coding-system)))
              ((looking-at "\\s *#.*coding\\s *[:=]"))
              (t (insert "# -*- coding: " coding-system " -*-\n"))
              )))))

(defun erm-ruby-get-process ()
  (when (and erm-ruby-process (not (equal (process-status erm-ruby-process) 'run)))
    (erm-initialise)
    (throw 'interrupted t))
  (unless erm-ruby-process
    (set-process-filter
     (setq erm-ruby-process
           (start-process "erm-ruby-process"
                          nil
                          enh-ruby-program (concat (file-name-directory (find-lisp-object-file-name 'erm-parse (symbol-function 'erm-parse))) "ruby/erm.rb")))
     'erm-filter)
    (set-process-query-on-exit-flag erm-ruby-process nil))
  
  erm-ruby-process)

(defvar erm-response nil "Private variable.")
(defvar erm-parsing-p nil "Private variable.")
(defvar erm-full-parse-p nil "Private variable.")

(defvar erm-ruby-process nil
  "The current erm process where emacs is interacting with")

(defvar erm-buff-num nil "Private variable.")
(defvar erm-next-buff-num nil "Private variable.")
(defvar erm-parse-buff nil "Private variable.")
(defvar erm-reparse-list nil "Private variable.")

(defun erm-initialise ()
  (setq erm-reparse-list nil
        erm-full-parse-p nil
        erm-parsing-p nil
        erm-parse-buff nil
        erm-next-buff-num 0)
  (when erm-ruby-process
    (delete-process erm-ruby-process) 
    (setq erm-ruby-process nil))

  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq 'ruby-mode major-mode)
        (erm-reset-buffer)))))



(erm-initialise)

(defun erm-major-mode-changed ()
  (erm-buffer-killed))

(defun erm-buffer-killed ()
  (process-send-string (erm-ruby-get-process) (concat "k" (number-to-string erm-buff-num) ":\n\0\0\0\n")))

(defun erm-reset-buffer ()
  (setq erm-buff-num erm-next-buff-num)
  (setq erm-next-buff-num (1+ erm-buff-num))
  (add-hook 'after-change-functions #'erm-req-parse nil t)
  (erm-full-parse))

(defvar ruby-mode-syntax-table nil
  "Syntax table in use in ruby-mode buffers.")

(if ruby-mode-syntax-table
    ()
  (setq ruby-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" ruby-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" ruby-mode-syntax-table)
  (modify-syntax-entry ?\` "\"" ruby-mode-syntax-table)
  (modify-syntax-entry ?# "<" ruby-mode-syntax-table)
  (modify-syntax-entry ?\n ">" ruby-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" ruby-mode-syntax-table)
  (modify-syntax-entry ?$ "." ruby-mode-syntax-table)
  (modify-syntax-entry ?? "_" ruby-mode-syntax-table)
  (modify-syntax-entry ?_ "_" ruby-mode-syntax-table)
  (modify-syntax-entry ?< "." ruby-mode-syntax-table)
  (modify-syntax-entry ?> "." ruby-mode-syntax-table)
  (modify-syntax-entry ?& "." ruby-mode-syntax-table)
  (modify-syntax-entry ?| "." ruby-mode-syntax-table)
  (modify-syntax-entry ?% "." ruby-mode-syntax-table)
  (modify-syntax-entry ?= "." ruby-mode-syntax-table)
  (modify-syntax-entry ?/ "." ruby-mode-syntax-table)
  (modify-syntax-entry ?+ "." ruby-mode-syntax-table)
  (modify-syntax-entry ?* "." ruby-mode-syntax-table)
  (modify-syntax-entry ?- "." ruby-mode-syntax-table)
  (modify-syntax-entry ?\; "." ruby-mode-syntax-table)
  (modify-syntax-entry ?\( "()" ruby-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ruby-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" ruby-mode-syntax-table)
  (modify-syntax-entry ?\} "){" ruby-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" ruby-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" ruby-mode-syntax-table)
  )


(defvar ruby-mode-map nil "Keymap used in ruby mode.")

(defun ruby-electric-brace (arg)
  (interactive "P")
  (insert-char last-command-char 1)
  (ruby-indent-line t)
  (delete-char -1)
  (self-insert-command (prefix-numeric-value arg)))

(if ruby-mode-map
    nil
  (setq ruby-mode-map (make-sparse-keymap))
  (define-key ruby-mode-map "{" 'ruby-electric-brace)
  (define-key ruby-mode-map "}" 'ruby-electric-brace)
  (define-key ruby-mode-map "\e\C-a" 'ruby-beginning-of-defun)
  (define-key ruby-mode-map "\e\C-e" 'ruby-end-of-defun)
  (define-key ruby-mode-map "\e\C-b" 'ruby-backward-sexp)
  (define-key ruby-mode-map "\e\C-f" 'ruby-forward-sexp)
  (define-key ruby-mode-map "\e\C-p" 'ruby-beginning-of-block)
  (define-key ruby-mode-map "\e\C-n" 'ruby-end-of-block)
  (define-key ruby-mode-map "\e\C-h" 'ruby-mark-defun)
  (define-key ruby-mode-map "\e\C-q" 'ruby-indent-exp)
  (define-key ruby-mode-map "\C-c\C-e" 'ruby-insert-end)
  (define-key ruby-mode-map "\C-m" 'newline)
  (define-key ruby-mode-map "\C-c/" 'ruby-insert-end))

(defvar ruby-mode-abbrev-table nil
  "Abbrev table in use in ruby-mode buffers.")

(define-abbrev-table 'ruby-mode-abbrev-table ())


(defun ruby-mode ()
  "Major mode for editing Ruby code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ruby-mode-map)
  (setq major-mode 'ruby-mode
        mode-name "EnhRuby"
        comment-start "#"  ; used by comment-region; don't change it
        comment-end "")

  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.

  (set-syntax-table ruby-mode-syntax-table)
  (setq local-abbrev-table ruby-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) 'ruby-indent-line)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-variable-buffer-local 'comment-start) "# ")
  (set (make-variable-buffer-local 'comment-end) "")
  (set (make-variable-buffer-local 'comment-column) ruby-comment-column)
  (set (make-variable-buffer-local 'comment-start-skip) "#+ *")
  (setq indent-tabs-mode ruby-indent-tabs-mode)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)

  (set (make-local-variable 'add-log-current-defun-function) 'ruby-add-log-current-method)

  (add-hook
   (cond ((boundp 'before-save-hook)
          (make-local-variable 'before-save-hook)
          'before-save-hook)
         ((boundp 'write-contents-functions) 'write-contents-functions)
         ((boundp 'write-contents-hooks) 'write-contents-hooks))
   'ruby-mode-set-encoding)

  ;; We do our own syntax highlighting based on the parse tree.
  ;; However, we want minor modes that add keywords to highlight properly
  ;; (examples:  doxymacs, column-marker).  We do this by not letting
  ;; font-lock unfontify anything, and telling it to fontify after we
  ;; re-parse and re-highlight the buffer.  (We currently don't do any
  ;; work with regions other than the whole buffer.)
  (dolist (var '(font-lock-unfontify-buffer-function
                 font-lock-unfontify-region-function))
    (set (make-local-variable var) (lambda (&rest args) t)))

  ;; Don't let font-lock do syntactic (string/comment) fontification.
  (set (make-local-variable #'font-lock-syntactic-face-function)
       (lambda (state) nil))

  (set (make-local-variable 'imenu-create-index-function)
       'ruby-imenu-create-index)
  
  (set (make-local-variable #'font-lock-syntactic-face-function)
       (lambda (state) nil))

  (make-local-variable 'erm-full-parse-p)
  (make-local-variable 'erm-buff-num)
  (add-hook 'change-major-mode-hook 'erm-major-mode-changed nil t)
  (add-hook 'kill-buffer-hook 'erm-buffer-killed nil t)

  (erm-reset-buffer)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'ruby-mode-hook)
    (run-hooks 'ruby-mode-hook)))

(defun ruby-imenu-create-index-in-block (prefix beg end)
  (let* ((index-alist '())
         (pos beg)
         (prop (get-text-property pos 'indent)))
    (setq end (or end (point-max)))
    (while (and pos (< pos end))
      (goto-char pos)
      (when (and (eq prop 'b) (looking-at ruby-defun-and-name-re))
        (push (cons (concat (match-string 1) " "(match-string 2)) pos) index-alist))

      (setq prop (and (setq pos (ruby-next-indent-change pos))
                      (get-text-property pos 'indent))))
      
    index-alist))

(defun ruby-imenu-create-index ()
  (nreverse (ruby-imenu-create-index-in-block nil (point-min) nil)))

(defun ruby-add-log-current-method ()
  "Return current method string."
  (condition-case nil
      (save-excursion
        (ruby-beginning-of-defun 1)
        (when (looking-at ruby-defun-and-name-re)
          (concat (match-string 1) " "(match-string 2))))))


;; Stolen shamelessly from James Clark's nxml-mode.
(defmacro erm-with-unmodifying-text-property-changes (&rest body)
  "Evaluate BODY without any text property changes modifying the buffer.
Any text properties changes happen as usual but the changes are not treated as
modifications to the buffer."
  (let ((modified (make-symbol "modified")))
    `(let ((,modified (buffer-modified-p))
	   (inhibit-read-only t)
	   (inhibit-modification-hooks t)
	   (buffer-undo-list t)
	   (deactivate-mark nil)
	   ;; Apparently these avoid file locking problems.
	   (buffer-file-name nil)
	   (buffer-file-truename nil))
       (unwind-protect
           (progn ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))


(defun erm-full-parse ()
  (if (and erm-parsing-p (not (eq erm-parse-buff (current-buffer))))
      (erm-reparse-diff-buf)
    (setq erm-full-parse-p t)
    (erm-req-parse nil nil nil)))

(defun erm-reparse-diff-buf ()
  (setq erm-reparse-list (cons (current-buffer) erm-reparse-list)))


(defun erm-req-parse (min max len)
  (let ((pc (if erm-parsing-p
                (if (eq erm-parse-buff (current-buffer))
                    (setq erm-parsing-p 'a)
                  'dbuf)
              (setq erm-response "")
              (setq erm-parsing-p t)
              (if (not erm-full-parse-p)
                  'p
                (setq min (point-min)
                      max (point-max)
                      len 0
                      erm-full-parse-p nil)
                'r)))
        interrupted-p)
    (setq interrupted-p
          (catch 'interrupted
            (if (eq pc 'dbuf)
                (erm-reparse-diff-buf)
              (setq erm-parse-buff (current-buffer))
              (process-send-string (erm-ruby-get-process)
                                   (format "%s%d:%d:%d:%d:%d:" pc erm-buff-num (point-min) (point-max) min len))
              (process-send-region erm-ruby-process min max)
              (process-send-string erm-ruby-process "\n\0\0\0\n"))
            nil))
    (when interrupted-p
      (setq erm-full-parse-p t))))

(defun erm-wait-for-parse ()
  (while erm-parsing-p
    (accept-process-output (erm-ruby-get-process) 0.5)))

(defun erm-filter (proc response)
  (setq erm-response (concat erm-response response))
  (when (string= "\n\0\0\0\n" (substring erm-response -5 nil))
    (setq response (substring erm-response 0 -5))
    (setq erm-response "")
    (with-current-buffer erm-parse-buff
      (erm-with-unmodifying-text-property-changes
       (erm-parse response)))))

(defsubst erm-ready ()
  (if erm-full-parse-p
      (erm-full-parse)
    (setq erm-parsing-p t)
    (process-send-string (erm-ruby-get-process) (concat "g" (number-to-string erm-buff-num) ":\n\0\0\0\n"))))

(setq ruby-font-names
      '(nil
        font-lock-string-face
        font-lock-type-face
        font-lock-variable-name-face
        font-lock-comment-face
        font-lock-constant-face
        font-lock-string-face
        ruby-string-delimiter-face
        ruby-regexp-delimiter-face
        font-lock-function-name-face
        font-lock-keyword-face
        ruby-heredoc-delimiter-face
        ruby-op-face
        ))

(defun ruby-calculate-indent (&optional start-point)
  "Calculate the indentation of the previous line and its level."
  (save-excursion
    (when start-point (goto-char start-point))
    (if (bobp)
        0
      (forward-line 0)
      (skip-syntax-forward " " (line-end-position))
      (let ((pos (line-beginning-position))
            (prop (get-text-property (point) 'indent))
            (face (get-text-property (point) 'face)))
        (cond
         ((or (eq 'e prop) (eq 's prop))
          (when (eq 's prop) (forward-char))
          (ruby-backward-sexp)
          (if (not (eq 'd (get-text-property (point) 'indent)))
              (current-column)
            (ruby-calculate-indent-1 (point) (line-beginning-position))))

         ((eq 'r prop)
          (ruby-backward-sexp)
          (1+ (current-column)))

         ((or (eq 'font-lock-string-face face) 
              (eq 'ruby-heredoc-delimiter-face face) 
              (and (eq 'font-lock-variable-name-face face)
                   (looking-at "#")))
          (current-column))

         (t 
          (forward-line -1)
          (when (looking-at "^[[:space:]]*$")
            (skip-chars-backward " \n\t\r\v\f"))
          (ruby-calculate-indent-1 pos (line-beginning-position))))))))


(defun ruby-calculate-indent-1 (limit pos)
  (goto-char pos)
  (let ((start-pos pos)
        col max
        (indent (- (current-indentation) (if (eq 'c (get-text-property pos 'indent)) ruby-hanging-indent-level 0)))
        bc (nbc 0)
        pc (npc 0)
        (prop (get-text-property pos 'indent)))

    (while (< pos limit)
      (unless prop
        (setq pos (next-single-property-change pos 'indent (current-buffer) limit))
        (when (< pos limit)
          (setq prop (get-text-property pos 'indent))))
      (setq col (- pos start-pos -1))
      (cond
       ((eq prop 'l) (setq pc (cons col pc)))
       ((eq prop 'r) (if pc (setq pc (cdr pc)) (setq npc col)))
       ((or (eq prop 'b) (eq prop 'd) (eq prop 's)) (setq bc (cons col bc)))
       ((eq prop 'e) (if bc (setq bc (cdr bc)) (setq nbc col))))
      (when (< (setq pos (1+ pos)) limit)
        (setq prop (get-text-property pos 'indent)))
      )

    ;;(prin1 (list indent nbc bc npc pc))
    (setq pc (or (car pc) 0))
    (setq bc (or (car bc) 0))
    (setq max (max pc bc nbc npc))
    (+ (if (eq 'c (get-text-property limit 'indent)) ruby-hanging-indent-level 0)
     (cond
     ((= max 0) 
      (if (not (or (eq (get-text-property start-pos 'face) 'ruby-heredoc-delimiter-face)
                   (eq (get-text-property start-pos 'face) 'font-lock-string-face)))
          indent
        (goto-char (or (ruby-string-start-pos start-pos) limit))
        (current-column)))

     ((= max pc) (if (eq 'c (get-text-property limit 'indent)) (- pc ruby-hanging-indent-level) pc))

     ((= max bc) 
      (if (eq 'd (get-text-property (+ start-pos bc -1) 'indent))
          (+ (ruby-calculate-indent-1 (+ start-pos bc -1) start-pos) ruby-indent-level)
        (+ bc ruby-indent-level -1)))

     ((= max npc)
      (goto-char (+ start-pos npc))
      (ruby-backward-sexp)
      (ruby-calculate-indent-1 (point) (line-beginning-position)))
     
     ((= max nbc)
      (goto-char (+ start-pos nbc -1))
      (ruby-backward-sexp)
      (ruby-calculate-indent-1 (point) (line-beginning-position)))
      
     (t 0)
     ))))

(defun ruby-string-start-pos (pos)
  (when (< 0 (or (setq pos (previous-single-property-change pos 'face)) 0))
    (previous-single-property-change pos 'face)))


(defun ruby-up-sexp (&optional arg)
  "Move up one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (while (>= (setq arg (1- arg)) 0)
    (let* ((pos (ruby-previous-indent-change (point)))
           (prop (get-text-property pos 'indent))
           (count 1))

      (while (< 0 (setq count 
                        (cond
                         ((or (eq prop 'l) (eq prop 'b) (eq prop 'd)) (1- count))
                         ((or (eq prop 'r) (eq prop 'e)) (1+ count))
                         ((eq prop 's) count)
                         (t 0))))
        (setq prop (and (setq pos (ruby-previous-indent-change pos))
                        (get-text-property pos 'indent))))
      
      (goto-char (if prop pos (point-min))))))

(defun ruby-beginning-of-defun (&optional arg)
  "Move backward across one balanced expression (sexp) looking for a definition begining.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (ruby-backward-sexp 1)
                  (setq prop (get-text-property (point) 'indent))
                  (not (and (eq prop 'b) (looking-at ruby-defun-beg-re))))))
       (point)))))


(defun ruby-mark-defun ()
  "Put mark at end of this Ruby definition, point at beginning."
  (interactive)
  (push-mark (point))
  (ruby-beginning-of-defun 1)
  (ruby-forward-sexp 1)
  (forward-line 1)
  (push-mark (point) nil t)
  (ruby-backward-sexp 1)
  (forward-line 0))

(defun ruby-indent-exp (&optional shutup-p)
  "Indent each line in the balanced expression following point syntactically.
If optional SHUTUP-P is non-nil, no errors are signalled if no
balanced expression is found."
  (interactive "*P")
  (let ((end-pos (save-excursion (ruby-forward-sexp 1) (point))))
    (indent-region (point) end-pos)))
        

(defun ruby-beginning-of-block (&optional arg)
  "Move backward across one balanced expression (sexp) looking for a block begining.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (ruby-backward-sexp 1)
                  (setq prop (get-text-property (point) 'indent))
                  (not (or (eq prop 'b) (eq prop 'd))))))
       (point)))))


(defun ruby-end-of-defun (&optional arg)
  "Move forwards across one balanced expression (sexp) looking for a definition end.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (ruby-forward-sexp 1)
                  (setq prop (get-text-property (point) 'indent))
                  (not (and (eq prop 'e)
                            (save-excursion 
                              (ruby-backward-sexp 1)
                              (looking-at ruby-defun-beg-re)))))))
       (point)))))

(defun ruby-end-of-block (&optional arg)
  "Move forwards across one balanced expression (sexp) looking for a block end.
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((cont t)
        prop)
    (goto-char
     (save-excursion
       (while (>= (setq arg (1- arg)) 0)
         (while (progn
                  (ruby-forward-sexp 1)
                  (setq prop (get-text-property (point) 'indent))
                  (not (eq prop 'e)))))
       (point)))))

(defun ruby-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")

  (unless arg (setq arg 1))
  (while (>= (setq arg (1- arg)) 0)
    (let* ((pos (point))
           (prop (get-text-property pos 'indent))
           (count 0))
      
      (unless (or (eq prop 'r) (eq prop 'e))
        (setq prop (and (setq pos (ruby-previous-indent-change pos))
                        (get-text-property pos 'indent))))

      
      (while (< 0 (setq count 
                        (cond
                         ((or (eq prop 'l) (eq prop 'b) (eq prop 'd)) (1- count))
                         ((or (eq prop 'r) (eq prop 'e)) (1+ count))
                         ((eq prop 'c) count)
                         ((eq prop 's) (if (= 0 count) 1 count))
                         (t 0))))
        (goto-char pos)
        (setq prop (and (setq pos (ruby-previous-indent-change pos))
                        (get-text-property pos 'indent))))
      
      (unless prop (error "expression ends prematurely"))
      (goto-char pos))))

(defun ruby-forward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times."
  (interactive "^p")
  (unless arg (setq arg 1))
  (while (>= (setq arg (1- arg)) 0)
    (let* ((pos (point))
           (prop (get-text-property pos 'indent))
           (count 0))
      
      (unless (or (eq prop 'l) (eq prop 'b))
        (setq prop (and (setq pos (ruby-next-indent-change pos))
                        (get-text-property pos 'indent))))

      
      (while (< 0 (setq count 
                        (cond
                         ((or (eq prop 'l) (eq prop 'b) (eq prop 'd)) (1+ count))
                         ((or (eq prop 'r) (eq prop 'e)) (1- count))
                         ((eq prop 'c) count)
                         ((eq prop 's) (if (= 0 count) 1 count))
                         (t 0))))
        (goto-char pos)
        (setq prop (and (setq pos (ruby-next-indent-change pos))
                        (get-text-property pos 'indent))))
      
      (unless prop (error "expression ends prematurely"))
      (goto-char pos))))

(defun ruby-insert-end ()
  (interactive)
  (let ((text (save-excursion
                (forward-line 0)
                (if (looking-at "^[ \t]*$")
                    "end"
                  "\nend"))))
    (insert text)
    (erm-wait-for-parse)
    (ruby-indent-line t)
    (end-of-line)))


(defun ruby-previous-indent-change (pos)
  (and pos (setq pos (1- pos))
       (>= pos (point-min))
       (or (and (get-text-property pos 'indent) pos)
           (and (> pos (point-min))
                (get-text-property (1- pos) 'indent)
                (1- pos))
           (ruby-previous-indent-change (previous-single-property-change pos 'indent)))))

(defun ruby-next-indent-change (pos)
  (and pos (setq pos (1+ pos))
       (<= pos (point-max))
       (or (and (get-text-property pos 'indent) pos)
           (and (< pos (point-max))
                (get-text-property (1+ pos) 'indent)
                (1+ pos))
           (next-single-property-change pos 'indent))))

(defun ruby-calculate-indent-2 ()
  (let ((limit (1- (point))))
    (if (<= limit (point-min))
        0
      (beginning-of-line)
      (ruby-calculate-indent-1 limit (point)))))

(defun ruby-indent-line (&optional flag)
  "Correct indentation of the current ruby line."
  (erm-wait-for-parse)
  (ruby-indent-to (ruby-calculate-indent)))

(defun ruby-indent-to (indent)
  "Indent the current line."
  (unless (= (current-indentation) indent)
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent)))
  (if (< (current-column) (current-indentation))
      (back-to-indentation)))

(defun ruby-add-faces (list)
  (let* ((ipos (car list))
         (istart (car ipos))
         (iend (cadr ipos))
         (rpos (cdr (cadr list))))

    (unless (and (= (point-min) istart) (= (point-max) iend))
      (throw 'interrupted))
    
    (when (> iend 0)
      (remove-text-properties istart iend '(indent nil))

      (setq ipos (cddr ipos))

      (while ipos
        (put-text-property (cadr ipos) (1+ (cadr ipos)) 'indent (car ipos))
        (setq ipos (cddr ipos))
        )
      
      (while rpos
        (remove-text-properties (car rpos) (cadr rpos) '(face nil))
        (setq rpos (cddr rpos))
        ))
    
    (while (setq list (cdr list))
      (let ((face (nth (caar list) ruby-font-names))
            (pos (cdar list)))
        (while pos
          (put-text-property (car pos) (cadr pos) 'face face)
          (setq pos (cddr pos)))))))

(defun erm-parse (response)
  (let (interrupted-p
        (send-next-p (eq 'a erm-parsing-p)))
    (setq erm-parsing-p nil)
    (setq interrupted-p
          (condition-case nil
              (catch 'interrupted
                (if send-next-p
                    (erm-ready)
                  (ruby-add-faces (car (read-from-string response))))
                nil)
            (error t)))
    (if interrupted-p 
        (setq erm-full-parse-p t)
      (if erm-full-parse-p 
          (erm-full-parse)
        (when (car erm-reparse-list)
          (with-current-buffer (car erm-reparse-list)
            (setq erm-reparse-list (cdr erm-reparse-list))
            (erm-full-parse)))))))


(provide 'ruby-mode)
