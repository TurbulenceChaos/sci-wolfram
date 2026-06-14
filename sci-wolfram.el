;;; sci-wolfram.el --- Major mode for editing Wolfram Language. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;;
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Created: 2025-05-20
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, Wolfram Language, Mathematica
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram

;; This file is not part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing Wolfram Language code
;;
;; Installation and usage:
;;
;; Please check README.md.
;;
;; To customize all configurable variables of `sci-wolfram' package,
;; just type M-x customize-group RET sci-wolfram-mode RET
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'org)
(require 'org-element)
(require 'comint)
(require 'sci-wolfram-repl)

;;;###autoload
(defgroup sci-wolfram-mode nil "Major mode for wolfram script")

(defcustom sci-wolfram-formula-type "image"
  "Wolfram fomula output type: image (default) or latex"
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-image-dpi 150
  "Wolfram image resolution: default 150 for emacs and 100 for vscode"
  :type 'number
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-play "no"
  "Convert plots to CDF interactive file: yes or no (default)"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram-mode)

(setq sci-wolfram-path-script (expand-file-name
			       "sci-wolfram-path.wl"
			       (file-name-directory (or load-file-name buffer-file-name))))

(unless (file-exists-p (concat (file-name-sans-extension sci-wolfram-path-script) ".el"))
  (shell-command (format "wolframscript -script %s" sci-wolfram-path-script)))
(require 'sci-wolfram-path)

;; run wolfram script region or buffer code
(defun sci-wolfram-region-or-buffer-code ()
  "Return code in region or buffer without space lines"
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (code (buffer-substring-no-properties beg end)))
    (sci-wolfram-remove-space-lines code)))

(defvar sci-wolfram-image-script
  (expand-file-name "sciWolframDisplayImage.wl"
		    (file-name-directory (or load-file-name buffer-file-name))))

(defmacro sci-wolfram-run-region-or-buffer-macro (func-name func-doc lang)
  "Define a function to run wolfram script region or buffer code"
  `(defun ,func-name ()
     ,func-doc
     (interactive)
     (let ((code (sci-wolfram-region-or-buffer-code))
	   (outbuf (get-buffer-create "*Sci-Wolfram Run Result*")))
       (with-current-buffer outbuf
	 (unless (eq major-mode 'org-mode)
	   (org-mode))
	 (erase-buffer)
	 (insert (concat
		  "#+name: import-sci-wolfram-image-package\n"
		  (format "#+begin_src %s\n" ,lang)
		  (sci-wolfram-image-package)
		  "#+end_src\n\n"))
	 (insert (concat
		  "#+name: sci-wolfram-run-region-or-buffer\n"
		  (format "#+begin_src %s\n" ,lang)
		  code
		  "\n#+end_src\n\n"))
	 (org-fold-hide-block-all)
	 (org-babel-execute-buffer))
       (display-buffer outbuf))))

(sci-wolfram-run-region-or-buffer-macro
 sci-wolfram-run-region-or-buffer
 "Run wolfram script region or buffer code"
 "wolfram")

;; Convert wolfram script to PDF and Mathematica notebook
(setq sci-wolfram-pdf-script (expand-file-name
			      "sciWolframConvertToNotebook.wl"
			      (file-name-directory (or load-file-name buffer-file-name))))

(defmacro sci-wolfram-convert-to-notebook-macro (func-name func-doc lang)
  "Define a function to run wolfram script region or buffer code"
  `(defun ,func-name ()
     ,func-doc
     (interactive)
     (let ((file (buffer-file-name))
	   (outbuf (get-buffer-create "*Sci-Wolfram Convert Result*")))
       (with-current-buffer outbuf
	 (unless (eq major-mode 'org-mode)
	   (org-mode))
	 (erase-buffer)
	 (insert (concat
		  "#+name: import-sci-wolfram-convert-to-notebook-package\n"
		  (format "#+begin_src %s\n" ,lang)
		  (sci-wolfram-image-package)
		  "#+end_src\n\n"))
	 (insert (concat
		  "#+name: sci-wolfram-convert-to-notebook\n"
		  (format "#+begin_src %s\n" ,lang)
		  (format "sciWolframConvertToNotebook[\"%s\"];" file)
		  "\n#+end_src\n\n"))
	 (org-fold-hide-block-all)
	 (org-babel-execute-buffer))
       (display-buffer outbuf))))

(sci-wolfram-convert-to-notebook-macro
 sci-wolfram-convert-to-notebook
 "Convert wolfram script to PDF and Mathematica notebook"
 "wolfram")

;; format region or buffer
(defun sci-wolfram-format-code ()
  "Format wolfram codes"
  (interactive)
  (sci-wolfram-make-repl)
  (let* ((eoe (format "comint_wolfram_format_%s" (org-id-uuid)))
	 (code (concat
		(format "Needs[\"CodeFormatter`\"];\nWriteString[\"stdout\", CodeFormat[\"%s\"], \"\\n\"];\n"
			(sci-wolfram-region-or-buffer-code))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))
	 (result
	  (org-babel-comint-with-output
	      (sci-wolfram-repl-buffer eoe)
	    (comint-send-string sci-wolfram-repl-buffer code))))
    (save-excursion
      (if (region-active-p)
	  (delete-region (region-beginning) (region-end))
	(erase-buffer))
      (insert (sci-wolfram-remove-eoe result eoe)))))

;; doc lookup
(defun sci-wolfram-doc-lookup ()
  "Look up wolfram doc in web browser."
  (interactive)
  (let* ((word
	  (if (region-active-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (upcase-initials (current-word))))
         (url (format "https://reference.wolfram.com/language/ref/%s.html" word)))
    (browse-url url)))

;; completion
(require 'sci-wolfram-all-symbols)

(defun sci-wolfram-completion-at-point ()
  "Add wolfram LSP symbols to completion-at-point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
	    (cdr bounds)
	    sci-wolfram-all-symbols
	    ;; (completion-table-dynamic
	    ;;  (lambda (_string)
	    ;;    (unless (eglot or lsp) sci-wolfram-all-symbols nil)))
	    :exclusive 'no))))

(add-hook 'sci-wolfram-mode-hook
	  (lambda () (add-hook 'completion-at-point-functions 'sci-wolfram-completion-at-point nil t)))

(defun sci-wolfram-org-block-completion-at-point ()
  "Provide completion in wolfram org block"
  (when (and (org-in-src-block-p)
             (string=
              (car (org-babel-get-src-block-info))
              "wolfram"))
    (sci-wolfram-completion-at-point)))

(add-hook 'org-mode-hook
	  (lambda () (add-hook 'completion-at-point-functions 'sci-wolfram-org-block-completion-at-point nil t)))

;; lsp server
(defun sci-wolfram-remove-local-lsp-server ()
  (interactive)
  "Remove local installed LSPServer if needed."
  (async-shell-command "wolframscript -code 'PacletUninstall[\"LSPServer\"];'"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
 	       `(sci-wolfram-mode . (,sci-wolfram-kernel
				     "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets" "-run"
				     "Needs[\"LSPServer`\"]; LSPServer`StartServer[]"))))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
		     `(,sci-wolfram-kernel
		       "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets" "-run"
		       "Needs[\"LSPServer`\"]; LSPServer`StartServer[]"))
    :major-modes '(sci-wolfram-mode) ; '(sci-wolfram-mode sci-wolfram-ts-mode)
    :server-id 'wolfram-lsp)))

;; syntax table
(defvar sci-wolfram-mode-syntax-table nil)

(setq
 sci-wolfram-mode-syntax-table
 (let ((synTable (make-syntax-table)))
   ;; comment
   (modify-syntax-entry ?\( "()1n" synTable)
   (modify-syntax-entry ?\) ")(4n" synTable)
   (modify-syntax-entry ?* ". 23n" synTable)
   ;; symbol
   (modify-syntax-entry ?$ "_" synTable)
   ;; punctuation
   (modify-syntax-entry ?! "." synTable)
   (modify-syntax-entry ?# "." synTable)
   (modify-syntax-entry ?% "." synTable)
   (modify-syntax-entry ?& "." synTable)
   (modify-syntax-entry ?' "." synTable)
   (modify-syntax-entry ?+ "." synTable)
   (modify-syntax-entry ?, "." synTable)
   (modify-syntax-entry ?- "." synTable)
   (modify-syntax-entry ?. "." synTable)
   (modify-syntax-entry ?/ "." synTable)
   (modify-syntax-entry ?: "." synTable)
   (modify-syntax-entry ?\; "." synTable)
   (modify-syntax-entry ?< "." synTable)
   (modify-syntax-entry ?= "." synTable)
   (modify-syntax-entry ?> "." synTable)
   (modify-syntax-entry ?? "." synTable)
   (modify-syntax-entry ?@ "." synTable)
   (modify-syntax-entry ?\ "." synTable)
   (modify-syntax-entry ?^ "." synTable)
   (modify-syntax-entry ?_ "." synTable)
   (modify-syntax-entry ?` "." synTable)
   (modify-syntax-entry ?| "." synTable)
   (modify-syntax-entry ?~ "." synTable)
   (modify-syntax-entry ?\\ "." synTable)
   synTable))

;; font-lock color
(defvar sci-wolfram-font-lock-keywords nil)

(setq
 sci-wolfram-font-lock-keywords
 `((,(regexp-opt BuiltinFunctions-1 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-2 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-3 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-4 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-usr-functions 'symbols) . font-lock-function-name-face)
   (,(regexp-opt Constants 'symbols) . font-lock-constant-face)
   (,(regexp-opt SystemLongNames 'symbols) . font-lock-constant-face)
   (,(regexp-opt SpecialLongNames 'symbols) . font-lock-constant-face)
   (,(regexp-opt UndocumentedLongNames 'symbols) . font-lock-type-face)
   (,(regexp-opt FreeLongNames 'symbols) . font-lock-variable-name-face)
   (,(regexp-opt SystemCharacters) . font-lock-constant-face)
   (,(regexp-opt SpecialCharacters) . font-lock-keyword-face)
   (,(regexp-opt UndocumentedCharacters) . font-lock-type-face)
   (,(regexp-opt FreeCharacters) . font-lock-variable-name-face)
   (,(regexp-opt Options 'symbols) . font-lock-builtin-face)
   (,(regexp-opt SessionSymbols 'symbols) . font-lock-preprocessor-face)
   (,(regexp-opt ExperimentalSymbols 'symbols) . font-lock-warning-face)
   (,(regexp-opt UndocumentedSymbols 'symbols) . font-lock-type-face)
   (,(regexp-opt ObsoleteSymbols 'symbols) . font-lock-comment-face)
   (,(regexp-opt BadSymbols 'symbols) . font-lock-warning-face)
   (,(regexp-opt UnsupportedCharacters) . font-lock-comment-face)
   (,(regexp-opt UnsupportedLongNames 'symbols) . font-lock-comment-face)
   ("\\b\\([A-Za-z][A-Za-z0-9]*\\)\\[" 1 font-lock-function-name-face)
   ("\\b\\([A-Za-z][A-Za-z0-9]*]\\)\\s-*@" 1 font-lock-function-name-face)
   ("\\b[A-Za-z][A-Za-z0-9]*\\b" . font-lock-variable-name-face)))

;; sci-wolfram-mode
(define-derived-mode sci-wolfram-mode prog-mode "sci-wolfram"
  "Major mode for Wolfram Language"
  :syntax-table sci-wolfram-mode-syntax-table
  (setq font-lock-defaults '((sci-wolfram-font-lock-keywords)))
  (setq-local comment-start "(*")
  (setq-local comment-end "*)"))

;; keybinding
(defcustom sci-wolfram-mode-leader-key "<f6>"
  "leader key for sci-wolfram-mode"
  :type 'string
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-key-map
  '((sci-wolfram-complete-symbol . "c")
    (sci-wolfram-doc-lookup . "h")
    (sci-wolfram-import-image-pkg . "i")
    (sci-wolfram-import-convert-to-notebook-pkg . "n")
    (sci-wolfram-format-region-or-buffer . "f")
    (sci-wolfram-run-region-or-buffer . "e"))
  "sci-wolfram key map"
  :type '(alist :key-type symbol :value-type string)
  :group 'sci-wolfram-mode)

(dolist (key-map sci-wolfram-key-map)
  (define-key sci-wolfram-mode-map
	      (kbd (format "%s %s" sci-wolfram-mode-leader-key (cdr sci-wolfram-key-map)))
	      (car sci-wolfram-key-map)))

;; prettify symbols
(require 'sci-wolfram-prettify-symbols)

;;;###autoload
(dolist (file '("\\.wl\\'"
		"\\.wls\\'"
		"\\.nb\\'"
		"\\.cdf\\'"))
  (add-to-list 'auto-mode-alist `(,file . sci-wolfram-mode)))


(provide 'sci-wolfram)
;;; sci-wolfram.el ends here
