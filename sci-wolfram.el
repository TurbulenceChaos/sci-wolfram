;;; sci-wolfram.el --- Major mode for editing Wolfram Language -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 PENG

;; Author: PENG <p.peng01@outlook.com>
;; Created: 20250520
;; Version: 20260812
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, processes, tools
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram

;; This file is not part of GNU Emacs

;;; License

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

;; Major mode for editing Wolfram Language

;; Installation and usage:
;; Please check README.md.

;; To customize all configurable variables of `sci-wolfram' package,
;; just type M-x customize-group RET sci-wolfram-mode RET

;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'org-src)
(require 'ob-wolfram)
(require 'bytecomp)

;; group for `sci-wolfram-mode'
;;;###autoload
(defgroup sci-wolfram-mode nil
  "Group for `sci-wolfram-mode'"
  :group 'languages)

(defcustom sci-wolfram-formula-type "image"
  "Wolfram formula output type: image (default) or latex"
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-image-dpi 100
  "Wolfram image resolution: 100 (default)"
  :type 'number
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-image-name "uuid"
  "Wolfram image output name: uuid (default) or N (natural number)"
  :type '(choice (const "uuid") (const "N"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-play "no"
  "Convert plot to Mathematica interactive file: yes or no (default)"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-short-lines 10
  "Short[expr, n]: print output less than n lines, 10 (default)"
  :type 'number
  :group 'sci-wolfram-mode)

;; wolfram package template
(defvar sci-wolfram-script-directory (file-name-directory (or load-file-name buffer-file-name)))

(defvar sci-wolfram-display-image-script (expand-file-name "sciWolframDisplayImage.wl" sci-wolfram-script-directory))

(defvar sci-wolfram-convert-to-notebook-script (expand-file-name "sciWolframConvertToNotebook.wl" sci-wolfram-script-directory))

(defun sci-wolfram-display-image-package ()
  "sciWolframDisplayImage.wl package"
  (let ((n "\n"))
    (concat
     (format "Get[\"%s\"];" sci-wolfram-display-image-script)
     n n "(* sciWolframDisplayImage.wl"
     n n "Display wolfram script image."
     n n "Usage:"
     n n "Default:"
     n "$Post = sciWolframDisplayImage[#] &;"
     n n "All options:"
     n "$Post = sciWolframDisplayImage[#,"
     n "sciWolframFormulaType -> \"image\" (default) or \"latex\","
     n "sciWolframImageDPI    -> 100 (default),"
     n "sciWolframImageName   -> \"uuid\" (default) or \"N\" (natural number),"
     n "sciWolframPlay        -> \"yes\" or \"no\" (default) to convert plots to Mathematica interactive file,"
     n "sciWolframShortLines  -> 10 (default): Long expression are displayed using Short[expr, n], where n is the maximum number of lines to show"
     n "] &;"
     n n "Tyep below code to reset $Post:"
     n "$Post = ."
     n n "*)"
     n n "$Post = sciWolframDisplayImage[#,"
     n (format "sciWolframFormulaType -> \"%s\"," sci-wolfram-formula-type)
     n (format "sciWolframImageDPI    -> %s,"     sci-wolfram-image-dpi)
     n (format "sciWolframImageName   -> \"%s\"," sci-wolfram-image-name)
     n (format "sciWolframPlay        -> \"%s\"," sci-wolfram-play)
     n (format "sciWolframShortLines  -> %s"      sci-wolfram-short-lines)
     n "] &;")))

(defun sci-wolfram-convert-to-notebook-package ()
  "sciWolframConvertToNotebook.wl package"
  (let ((n "\n"))
    (concat
     (format "Get[\"%s\"];" sci-wolfram-convert-to-notebook-script)
     n n "(* sciWolframConvertToNotebook.wl"
     n n "Convert wolfram script to PDF and Mathematica notebook."
     n n "Usage:"
     n n "sciWolframConvertToNoteBook[\"/path/to/file.wl\"];"
     n n "*)")))

(defvar sci-wolfram-package-alist '(("display image" . sci-wolfram-display-image-package)
                                    ("convert to notebook" sci-wolfram-convert-to-notebook-package)))

;;;###autoload
(defun sci-wolfram-import-package ()
  "Import wolfram package:
[1] display image: sciWolframDisplayImage.wl package.
[2] convert to notebook: sciWolframConvertToNotebook.wl package."
  (interactive)
  (let* ((pkg (completing-read "Import package: " sci-wolfram-package-alist nil t))
         (pkg-func (cdr (assoc pkg sci-wolfram-package-alist))))
    (save-excursion
      (if (and (derived-mode-p 'org-mode)
	       (org-in-src-block-p))
	  (progn (org-edit-src-code)
	         (forward-line 1)
	         (insert (funcall pkg-func))
	         (org-edit-src-exit))
        (progn (forward-line 1)
	       (insert (funcall pkg-func)))))))

;; run wolfram script region or buffer code
;;;###autoload
(defun sci-wolfram-run-repl ()
  "Start a wolfram REPL."
  (interactive)
  (ob-wolfram-make-repl)
  (switch-to-buffer-other-window ob-wolfram-session))

(defun sci-wolfram-get-region-or-buffer-code ()
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (code (buffer-substring-no-properties beg end)))
    code))

(defun sci-wolfram-mode-run-region-or-buffer (&optional code)
  (let ((code (or code (sci-wolfram-get-region-or-buffer-code)))
	(outbuf (get-buffer-create "*Sci-Wolfram Run Result*"))
	(lang "wolfram")
        (n "\n"))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
	(org-mode))
      (erase-buffer)
      (insert (concat
	       "#+name: sci-wolfram-import-display-image-package"
	       n (format "#+begin_src %s" lang)
	       n (sci-wolfram-display-image-package)
	       n "#+end_src"
	       n n "#+name: sci-wolfram-run-region-or-buffer"
	       n (format "#+begin_src %s" lang)
	       n code
	       n "#+end_src"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer)
      (display-buffer outbuf))))

;;;###autoload
(defun sci-wolfram-run-region-or-buffer ()
  "Run wolfram script region or buffer code."
  (interactive)
  (cond
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (sci-wolfram-mode-run-region-or-buffer))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang "wolfram")))
    (let ((code (prog2 (org-edit-src-code)
		    (sci-wolfram-get-region-or-buffer-code)
		  (org-edit-src-exit))))
      (sci-wolfram-mode-run-region-or-buffer code)))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; convert wolfram script to PDF and Mathematica notebook
(defun sci-wolfram-mode-convert-to-notebook (&optional file)
  (let ((file (or file (buffer-file-name)))
	(outbuf (get-buffer-create "*Sci-Wolfram Convert Result*"))
	(lang "wolfram")
        (n "\n"))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
	(org-mode))
      (erase-buffer)
      (insert (concat
	       "#+name: sci-wolfram-import-convert-to-notebook-package"
	       n (format "#+begin_src %s" lang)
               n (sci-wolfram-convert-to-notebook-package)
	       n "#+end_src"
	       n n "#+name: sci-wolfram-convert-to-notebook"
	       n (format "#+begin_src %s" lang)
	       n (format "sciWolframConvertToNotebook[\"%s\"];" file)
	       n "#+end_src"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer)
      (display-buffer outbuf))))

;;;###autoload
(defun sci-wolfram-convert-to-notebook ()
  "Convert wolfram script to PDF and Mathematica notebook."
  (interactive)
  (cond
   ((and (not (region-active-p))
	 (buffer-file-name)
	 (derived-mode-p 'sci-wolfram-mode))
    (save-buffer)
    (sci-wolfram-mode-convert-to-notebook))
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (let* ((code (sci-wolfram-get-region-or-buffer-code))
	   (file-name (format "%s-region-or-buffer.wl"
			      (replace-regexp-in-string "[^a-zA-Z0-9_.\\-]" "" (file-name-sans-extension (buffer-name)))))
	   (file (expand-file-name file-name default-directory)))
      (write-region code nil file)
      (sci-wolfram-mode-convert-to-notebook file)))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang "wolfram")))
    (let* ((code (prog2 (org-edit-src-code)
		     (sci-wolfram-get-region-or-buffer-code)
		   (org-edit-src-exit)))
	   (info (org-babel-get-src-block-info))
	   (src-block-name (or (nth 4 info) "wolfram-babel"))
	   (file-name (format "%s-%s.wl"
			      (replace-regexp-in-string "[^a-zA-Z0-9_.\\-]" "" (file-name-sans-extension (buffer-name)))
			      src-block-name))
	   (file (expand-file-name file-name default-directory)))
      (write-region code nil file)
      (sci-wolfram-mode-convert-to-notebook file)))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; format wolfram script region or buffer code
(defun sci-wolfram-mode-format-region-or-buffer ()
  (ob-wolfram-make-repl)
  (ob-wolfram-initiate-session)
  (let* ((code (sci-wolfram-get-region-or-buffer-code))
	 (tmp (org-babel-temp-file "wolfram-" ".wl"))
	 (format-code (progn (with-temp-file tmp (insert code))
                             (concat
                              "Needs[\"CodeFormatter`\"];"
                              (format
                               "WriteString[\"stdout\",CodeFormatter`CodeFormat[File[\"%s\"],CodeFormatter`Airiness->-0.75,\"LineWidth\"->120,\"BreakLinesMethod\"->\"LineBreakerV2\"],\"\\n\"];\n"
                               tmp))))
	 (result (ob-wolfram-evaluate-session format-code)))
    (message "Format wolfram script")
    (save-excursion
      (if (region-active-p)
	  (delete-region (region-beginning) (region-end))
	(erase-buffer))
      (insert result))))

;;;###autoload
(defun sci-wolfram-format-region-or-buffer ()
  "Format wolfram script region or buffer codes."
  (interactive)
  (cond
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (sci-wolfram-mode-format-region-or-buffer))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang "wolfram")))
    (org-edit-src-code)
    (sci-wolfram-format-region-or-buffer)
    (org-edit-src-exit))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; wolfram documentation lookup
;;;###autoload
(defun sci-wolfram-doc-lookup ()
  "Look up wolfram documentation in browser."
  (interactive)
  (let* ((symbol
	  (or (if (region-active-p)
	          (buffer-substring-no-properties (region-beginning) (region-end))
	        (when-let* ((word (current-word)))
                  (upcase-initials word)))
              (upcase-initials (read-string "Wolfram symbol: "))))
         (url (format "https://reference.wolfram.com/language/ref/%s.html" symbol)))
    (browse-url url)))

;; completion-at-point
(eval-and-compile
  (let* ((dir (file-name-directory (or byte-compile-current-file load-file-name buffer-file-name)))
         (script (expand-file-name "sciWolframLSPSymbols.wl" dir))
         (symbols (expand-file-name "LSPSymbols" dir)))
    (unless (file-directory-p symbols)
      (make-directory symbols))

    (add-to-list 'load-path symbols)

    (unless (directory-files symbols nil "\\.el\\'")
      (message "Convert wolfram LSPServer symbols to emacs symbols")
      (shell-command (format "wolframscript -script %s" script)))))

(require 'sci-wolfram-lsp-symbols-builtin-functions-1)
(require 'sci-wolfram-lsp-symbols-builtin-functions-2)
(require 'sci-wolfram-lsp-symbols-builtin-functions-3)
(require 'sci-wolfram-lsp-symbols-builtin-functions-4)
(require 'sci-wolfram-lsp-symbols-builtin-functions-5)
(require 'sci-wolfram-lsp-symbols-constants)
(require 'sci-wolfram-lsp-symbols-options)
(require 'sci-wolfram-lsp-symbols-session-symbols)
(require 'sci-wolfram-lsp-symbols-experimental-symbols)
(require 'sci-wolfram-lsp-symbols-undocumented-symbols)
(require 'sci-wolfram-lsp-symbols-obsolete-symbols)
(require 'sci-wolfram-lsp-symbols-bad-symbols)
(require 'sci-wolfram-lsp-symbols-system-long-names)
(require 'sci-wolfram-lsp-symbols-free-long-names)
(require 'sci-wolfram-lsp-symbols-special-long-names)
(require 'sci-wolfram-lsp-symbols-undocumented-long-names)
(require 'sci-wolfram-lsp-symbols-unsupported-long-names)

(defvar sci-wolfram-lsp-symbols
  (append
   sci-wolfram-lsp-symbols-builtin-functions-1
   sci-wolfram-lsp-symbols-builtin-functions-2
   sci-wolfram-lsp-symbols-builtin-functions-3
   sci-wolfram-lsp-symbols-builtin-functions-4
   sci-wolfram-lsp-symbols-builtin-functions-5
   sci-wolfram-lsp-symbols-constants
   sci-wolfram-lsp-symbols-options
   sci-wolfram-lsp-symbols-session-symbols
   sci-wolfram-lsp-symbols-experimental-symbols
   sci-wolfram-lsp-symbols-undocumented-symbols
   sci-wolfram-lsp-symbols-obsolete-symbols
   sci-wolfram-lsp-symbols-bad-symbols
   sci-wolfram-lsp-symbols-system-long-names
   sci-wolfram-lsp-symbols-free-long-names
   sci-wolfram-lsp-symbols-special-long-names
   sci-wolfram-lsp-symbols-undocumented-long-names
   sci-wolfram-lsp-symbols-unsupported-long-names))

(defun sci-wolfram-completion-at-point ()
  "Add wolfram symbols to completion-at-point."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (list (car bounds)
	  (cdr bounds)
	  sci-wolfram-lsp-symbols
	  :exclusive 'no)))

(add-hook 'sci-wolfram-mode-hook
          (lambda () (add-hook 'completion-at-point-functions #'sci-wolfram-completion-at-point nil t)))

;;;###autoload
(add-hook 'org-mode-hook
          (lambda () (add-hook 'completion-at-point-functions
                               (lambda ()
                                 (when (org-in-src-block-p t)
                                   (let* ((info (org-babel-get-src-block-info))
	                                  (lang (nth 0 info)))
                                     (when (string= lang "wolfram")
                                       (sci-wolfram-completion-at-point)))))
                               nil t)))

;; wolfram LSPServer
(eval-and-compile
  (defvar sci-wolfram-kernel-location
    (expand-file-name "sci-wolfram-kernel-location.txt" (file-name-directory (or byte-compile-current-file load-file-name buffer-file-name))))

  (unless (file-exists-p sci-wolfram-kernel-location)
    (message "Get wolfram kernel location")
    (with-temp-file sci-wolfram-kernel-location
      (insert (string-trim-right (shell-command-to-string "wolframscript -code 'First[$CommandLine]'"))))))

(defcustom sci-wolfram-kernel
  (with-temp-buffer
    (insert-file-contents sci-wolfram-kernel-location)
    (buffer-string))
  "Wolfram kernel used for eglot or lsp-mode."
  :type 'string
  :group 'sci-wolfram-mode)

;; reference:
;; https://github.com/transentis/wolfram-language-mode
;; https://github.com/WolframResearch/vscode-wolfram
(defvar sci-wolfram-lsp-server
  (list sci-wolfram-kernel
        "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets" "-run"
        (concat "Needs[\"LSPServer`\"];"
                "SetOptions[CodeFormatter`CodeFormatCST,CodeFormatter`Airiness->-0.75,\"LineWidth\"->120,\"BreakLinesMethod\"->\"LineBreakerV2\"];"
                "LSPServer`StartServer[]")))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               (cons 'sci-wolfram-mode sci-wolfram-lsp-server)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(sci-wolfram-mode . "wolfram"))

  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection sci-wolfram-lsp-server)
                        :activation-fn (lsp-activate-on "wolfram")
                        :server-id 'wolfram-lsp)))

;; syntax table
(defvar sci-wolfram-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; comment
    (modify-syntax-entry ?\( "()1n"  syntax-table)
    (modify-syntax-entry ?\) ")(4n"  syntax-table)
    (modify-syntax-entry ?*  ". 23n" syntax-table)
    ;; \[Omega]
    (modify-syntax-entry ?\\ "."     syntax-table)
    syntax-table))

;; font-lock
(defvar sci-wolfram-mode-font-lock-keywords
  (list
   (cons (regexp-opt sci-wolfram-lsp-symbols-builtin-functions-1 'symbols)                     'font-lock-function-name-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-builtin-functions-2 'symbols)                     'font-lock-function-name-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-builtin-functions-3 'symbols)                     'font-lock-function-name-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-builtin-functions-4 'symbols)                     'font-lock-function-name-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-builtin-functions-5 'symbols)                     'font-lock-function-name-face)

   (cons (regexp-opt sci-wolfram-lsp-symbols-constants 'symbols)                               'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-options 'symbols)                                 'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-session-symbols 'symbols)                         'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-experimental-symbols 'symbols)                    'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-undocumented-symbols 'symbols)                    'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-obsolete-symbols 'symbols)                        'font-lock-builtin-face)
   (cons (regexp-opt sci-wolfram-lsp-symbols-bad-symbols 'symbols)                             'font-lock-builtin-face)

   (cons (concat "\\\\\\[" (regexp-opt sci-wolfram-lsp-symbols-system-long-names) "\\]")       'font-lock-constant-face)
   (cons (concat "\\\\\\[" (regexp-opt sci-wolfram-lsp-symbols-free-long-names) "\\]")         'font-lock-constant-face)
   (cons (concat "\\\\\\[" (regexp-opt sci-wolfram-lsp-symbols-special-long-names) "\\]")      'font-lock-constant-face)
   (cons (concat "\\\\\\[" (regexp-opt sci-wolfram-lsp-symbols-undocumented-long-names) "\\]") 'font-lock-constant-face)
   (cons (concat "\\\\\\[" (regexp-opt sci-wolfram-lsp-symbols-unsupported-long-names) "\\]")  'font-lock-constant-face)

   (cons "[A-Za-z][A-Za-z0-9]*"                                                                'font-lock-variable-name-face)))

;; keybinding
(defvar sci-wolfram-mode-map (make-sparse-keymap))
(defvar sci-wolfram-mode-leader-key-map (make-sparse-keymap))
(defvar sci-wolfram-mode-leader-key "C-c" "sci-wolfram-mode leader key")
(defvar sci-wolfram-mode-key
  '((sci-wolfram-doc-lookup . "h")
    (sci-wolfram-run-repl . "t")
    (sci-wolfram-import-package . "i")
    (sci-wolfram-format-region-or-buffer . "f")
    (sci-wolfram-run-region-or-buffer . "r")
    (sci-wolfram-convert-to-notebook . "c"))
  "sci-wolfram-mode keymap")

(dolist (key sci-wolfram-mode-key)
  (define-key sci-wolfram-mode-leader-key-map (kbd (cdr key)) (car key)))
(define-key sci-wolfram-mode-map (kbd sci-wolfram-mode-leader-key) sci-wolfram-mode-leader-key-map)

;; sci-wolfram-mode
;;;###autoload
(define-derived-mode sci-wolfram-mode prog-mode "sci-wolfram"
  "Major mode for Wolfram Language."
  :syntax-table sci-wolfram-mode-syntax-table
  :keymap sci-wolfram-mode-map
  (setq font-lock-defaults '((sci-wolfram-mode-font-lock-keywords)))
  (setq-local comment-start "(*")
  (setq-local comment-end "*)"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wls?\\'" . sci-wolfram-mode))

;;;###autoload
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("wolfram" . sci-wolfram)))

;; prettify symbols
(eval-and-compile
  (let* ((dir (file-name-directory (or byte-compile-current-file load-file-name buffer-file-name)))
         (script (expand-file-name "sciWolframPrettifySymbols.wl" dir))
         (symbols (expand-file-name "sci-wolfram-prettify-symbols.el" dir)))
    (unless (file-exists-p symbols)
      (message "Convert wolfram characters to emacs prettify symbols")
      (shell-command (format "wolframscript -script %s" script)))))

(require 'sci-wolfram-prettify-symbols)

(add-hook 'sci-wolfram-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist sci-wolfram-prettify-symbols)
            (setq-local prettify-symbols-compose-predicate (lambda (start end match) t))
            ;; (setq-local prettify-symbols-unprettify-at-point nil)
            (prettify-symbols-mode 1)))


(provide 'sci-wolfram)
;;; sci-wolfram.el ends here
