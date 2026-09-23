;;; sci-wolfram.el --- Major mode for editing Wolfram Language -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 PENG

;; Author: PENG <p.peng01@outlook.com>
;; Created: 20250520
;; Version: 20260922
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

;; Major mode for editing Wolfram Language.

;; Installation and usage:
;; Please check README.md.

;; To customize all configurable variables of `sci-wolfram' package,
;; just type M-x customize-group RET sci-wolfram RET

;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'org-src)
(require 'ob-wolfram)
(require 'bytecomp)

;;;###autoload
(defgroup sci-wolfram nil
  "Group for `sci-wolfram'"
  :group 'languages)

(defcustom sci-wolfram-expr-type "image"
  "Wolfram expression output type: image (default) or latex.
For example, the result of
DSolve[y'[x] + y[x] == a Sin[x], y[x], x]
will be automatically converted to:
[1] image: ./tmp/wolfram/wolfram-uuid.png
[2] latex: \\begin{equation*} expression \\end{equation*}"
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram)

(defcustom sci-wolfram-image-dpi 150
  "Wolfram image output DPI: 150 (default)"
  :type 'integer
  :group 'sci-wolfram)

(defcustom sci-wolfram-plot-play "no"
  "Convert dynamic plots to Mathematica interactive files: yes or no (default)"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram)

(defcustom sci-wolfram-short-lines 10
  "Use Short[code,n] to print long expressions less than n lines, 10 (default)"
  :type 'integer
  :group 'sci-wolfram)

;;;###autoload
(defun sci-wolfram-run-repl ()
  "Create a new Wolfram REPL if it is not exist."
  (interactive)
  (ob-wolfram-make-repl)
  (switch-to-buffer-other-window ob-wolfram-session))

;; import pkg
(defvar sci-wolfram-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar sci-wolfram-display-image-script
  (expand-file-name "DisplayImage.wl" sci-wolfram-directory))

(defvar sci-wolfram-convert-to-notebook-script
  (expand-file-name "ConvertToNotebook.wl" sci-wolfram-directory))

(defun sci-wolfram-display-image-package ()
  "DisplayImage.wl package"
  (let ((n "\n"))
    (concat
     (format "Get[\"%s\"];" sci-wolfram-display-image-script)
     n n "(* DisplayImage.wl"
     n n "Display Wolfram script images."
     n n "Usage:"
     n n "Default:"
     n "$Post = DisplayImage[#] &;"
     n n "All options:"
     n "$Post = DisplayImage[#,"
     n "ExprType   -> \"image\" (default) or \"latex\","
     n "ImageDPI   -> 150 (default),"
     n "PlotPlay   -> \"yes\" or \"no\" (default) to convert dynamic plots to Wolfram Mathematica interactive files,"
     n "ShortLines -> 10 (default): use Short[code, n] to print long expressions less than n lines"
     n "] &;"
     n n "Tyep below code to reset $Post:"
     n "$Post = ."
     n n "*)"
     n n "$Post = DisplayImage[#,"
     n (format "ExprType   -> %S," sci-wolfram-expr-type)
     n (format "ImageDPI   -> %s," sci-wolfram-image-dpi)
     n (format "PlotPlay   -> %S," sci-wolfram-plot-play)
     n (format "ShortLines -> %s"  sci-wolfram-short-lines)
     n "] &;" n)))

(defun sci-wolfram-convert-to-notebook-package ()
  "ConvertToNotebook.wl package"
  (let ((n "\n"))
    (concat
     (format "Get[\"%s\"];" sci-wolfram-convert-to-notebook-script)
     n n "(* ConvertToNotebook.wl"
     n n "Convert Wolfram script to Wolfram Mathematica notebook."
     n n "Usage:"
     n n "ConvertToNoteBook[\"/path/to/file.wl\"];"
     n n "*)" n)))

(defvar sci-wolfram-package-alist '(("display image" . sci-wolfram-display-image-package)
                                    ("convert to notebook" . sci-wolfram-convert-to-notebook-package)))

;;;###autoload
(defun sci-wolfram-import-package ()
  "Import Wolfram package"
  (interactive)
  (let* ((pkg (completing-read "Import Wolfram package: " sci-wolfram-package-alist nil t))
         (func (cdr (assoc pkg sci-wolfram-package-alist))))
    (save-excursion
      (beginning-of-line)
      (insert (funcall func)))))


;; run region or buffer
(defun sci-wolfram-get-region-or-buffer-code ()
  (let ((code (cond ((region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))
                    ((derived-mode-p 'sci-wolfram-mode)
                     (buffer-substring-no-properties (point-min) (point-max))))))
    (if code
        (progn (ob-wolfram-syntax-check code)
               (string-trim-right code))
      (user-error "You must be in either [1] region, [2] `sci-wolfram-mode', or [3] Wolfram src-block!"))))

;;;###autoload
(defun sci-wolfram-run-region-or-buffer ()
  "Run Wolfram script region or buffer code."
  (interactive)
  (let ((code (sci-wolfram-get-region-or-buffer-code))
        (outbuf (get-buffer-create "*Wolfram Results*"))
        (n "\n"))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (erase-buffer)
      (insert (concat
               "#+name: sci-wolfram-import-display-image-package"
               n "#+begin_src wolfram"
               n (sci-wolfram-display-image-package)
               n "#+end_src"
               n n "#+name: sci-wolfram-run-region-or-buffer"
               n "#+begin_src wolfram"
               n code
               n "#+end_src"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer)
      (display-buffer outbuf))))

;; convert to notebook
;;;###autoload
(defun sci-wolfram-convert-to-notebook ()
  "Convert Wolfram script to Wolfram Mathematica notebook."
  (interactive)
  (unless (and (buffer-file-name) (derived-mode-p 'sci-wolfram-mode))
    (user-error "You must be in a Wolfram script file!"))
  (ob-wolfram-syntax-check
   (buffer-substring-no-properties (point-min) (point-max)))
  (let ((file (buffer-file-name))
        (outbuf (get-buffer-create "*Wolfram Convert*"))
        (n "\n"))
    (save-buffer)
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
        (org-mode))
      (erase-buffer)
      (insert (concat
               "#+name: sci-wolfram-import-convert-to-notebook-package"
               n "#+begin_src wolfram"
               n (sci-wolfram-convert-to-notebook-package)
               n "#+end_src"
               n n "#+name: sci-wolfram-convert-to-notebook"
               n "#+begin_src wolfram"
               n (format "ConvertToNotebook[%S];" file)
               n "#+end_src"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer)
      (display-buffer outbuf))))

;; format region or buffer
(defvar sci-wolfram-tab-width 4)

(defvar sci-wolfram-indent-string
  (make-string sci-wolfram-tab-width ?\s))

;; https://github.com/WolframResearch/codeformatter/issues/4
;;;###autoload
(defun sci-wolfram-format-region-or-buffer ()
  "Format Wolfram script region or buffer codes."
  (interactive)
  (let ((env (if (and (derived-mode-p 'org-mode)
                      (org-in-src-block-p))
                 "org src block")))
    ;; if in org src block, enter org-src-mode
    (if env (org-edit-src-code))
    (let* ((code (sci-wolfram-get-region-or-buffer-code))
           (tmp (org-babel-temp-file "wolfram-format-" ".wl"))
           (format (progn (with-temp-file tmp (insert code))
                          (concat
                           "Needs[\"CodeFormatter`\"];"
                           (format "WriteString[%S," tmp)
                           (format "CodeFormatter`CodeFormat[File[%S]," tmp)
                           "\"Airiness\"->-0.75,"
                           "\"LineWidth\"->120,"
                           "\"BreakLinesMethod\"->\"LineBreakerV2\","
                           (format "\"TabWidth\"->%s," sci-wolfram-tab-width)
                           (format "\"IndentationString\"->%S" sci-wolfram-indent-string)
                           "]];"
                           (format "Close[%S];Out[];" tmp))))
           (result (progn (ob-wolfram-evaluate-session format)
                          (with-temp-buffer (insert-file-contents tmp)
                                            (substring-no-properties (buffer-string))))))
      (message "Format Wolfram script")
      (save-excursion
        (if (region-active-p)
            (delete-region (region-beginning) (region-end))
          (erase-buffer))
        (insert result)))
    ;; exit org-src-mode
    (if env (org-edit-src-exit))))

;; completion-at-point
(eval-and-compile
  (let* ((dir (file-name-directory (or byte-compile-current-file load-file-name buffer-file-name)))
         (script (expand-file-name "LSPSymbols.wl" dir))
         (subdir (expand-file-name "LSPSymbols" dir)))
    (unless (file-directory-p subdir)
      (make-directory subdir))

    (add-to-list 'load-path subdir)

    (unless (directory-files subdir nil "\\.el\\'")
      (message "Convert Wolfram LSPServer symbols to Emacs symbols")
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
  "Add Wolfram symbols to completion-at-point."
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

;; doc lookup
;;;###autoload
(defun sci-wolfram-doc-lookup ()
  "Look up Wolfram documentation in browser."
  (interactive)
  (let* ((symbol
          (or (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (when-let* ((word (current-word)))
                  (upcase-initials word)))
              (upcase-initials (completing-read "Wolfram symbol: " sci-wolfram-lsp-symbols))))
         (url (format "https://reference.wolfram.com/language/ref/%s.html" symbol)))
    (browse-url url)))

;; LSPServer
(eval-and-compile
  (defvar sci-wolfram-kernel-location
    (expand-file-name "sci-wolfram-kernel-location.txt"
                      (file-name-directory (or byte-compile-current-file load-file-name buffer-file-name))))

  (unless (file-exists-p sci-wolfram-kernel-location)
    (let ((kernel (string-trim-right
                   (shell-command-to-string "wolframscript -code 'First[$CommandLine]'"))))
      (message (format "Get Wolfram kernel location: %s" kernel))
      (with-temp-file sci-wolfram-kernel-location
        (insert kernel)))))

(defcustom sci-wolfram-kernel
  (with-temp-buffer
    (insert-file-contents sci-wolfram-kernel-location)
    (buffer-string))
  "Wolfram kernel used for eglot or lsp-mode."
  :type 'string
  :group 'sci-wolfram)

(defvar eglot-server-programs)
(defvar lsp-language-id-configuration)
(declare-function lsp-register-client "lsp-mode")
(declare-function make-lsp-client "lsp-mode")
(declare-function lsp-stdio-connection "lsp-mode")
(declare-function lsp-activate-on "lsp-mode")

;; reference:
;; https://github.com/transentis/wolfram-language-mode
;; https://github.com/WolframResearch/vscode-wolfram
(defvar sci-wolfram-lsp-server
  (list sci-wolfram-kernel
        "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets" "-run"
        (concat "Needs[\"LSPServer`\"];"
                "CodeFormatter`$DefaultAiriness=-0.75;"
                "CodeFormatter`$DefaultLineWidth=120;"
                "CodeFormatter`$DefaultBreakLinesMethod=\"LineBreakerV2\";"
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
;; reference: https://github.com/xahlee/xah-wolfram-mode/blob/113317d71684cff630c178553d4c2d2f42983b15/xah-wolfram-mode.el#L2631
(defvar sci-wolfram-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; comment
    (modify-syntax-entry ?\( "()1n"  syntax-table)
    (modify-syntax-entry ?\) ")(4n"  syntax-table)
    (modify-syntax-entry ?*  ". 23n" syntax-table)
    ;; symbol
    (modify-syntax-entry ?$  "_"     syntax-table)
    ;; punctuation
    (modify-syntax-entry ?!  "."     syntax-table)
    (modify-syntax-entry ?#  "."     syntax-table)
    (modify-syntax-entry ?%  "."     syntax-table)
    (modify-syntax-entry ?&  "."     syntax-table)
    (modify-syntax-entry ?'  "."     syntax-table)
    (modify-syntax-entry ?+  "."     syntax-table)
    (modify-syntax-entry ?,  "."     syntax-table)
    (modify-syntax-entry ?-  "."     syntax-table)
    (modify-syntax-entry ?.  "."     syntax-table)
    (modify-syntax-entry ?/  "."     syntax-table)
    (modify-syntax-entry ?:  "."     syntax-table)
    (modify-syntax-entry ?\; "."     syntax-table)
    (modify-syntax-entry ?<  "."     syntax-table)
    (modify-syntax-entry ?=  "."     syntax-table)
    (modify-syntax-entry ?>  "."     syntax-table)
    (modify-syntax-entry ??  "."     syntax-table)
    (modify-syntax-entry ?@  "."     syntax-table)
    (modify-syntax-entry ?^  "."     syntax-table)
    (modify-syntax-entry ?_  "."     syntax-table)
    (modify-syntax-entry ?`  "."     syntax-table)
    (modify-syntax-entry ?|  "."     syntax-table)
    (modify-syntax-entry ?~  "."     syntax-table)
    syntax-table))

;; \[Omega]
;; reference: https://github.com/kawabata/wolfram-mode/blob/be680190cac6ccf579dbce107deaae495928d1b3/wolfram-mode.el#L189
(defvar sci-wolfram-mode-syntax-propertize-function
  (syntax-propertize-rules
   ("\\\\[[A-Z][A-Za-z]*]" (0 "_"))))

;; font-lock
;; reference: https://github.com/xahlee/xah-wolfram-mode/blob/113317d71684cff630c178553d4c2d2f42983b15/xah-wolfram-mode.el#L2685
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
;;;###autoload
(defvar sci-wolfram-mode-key
  '((sci-wolfram-doc-lookup              . "C-h")
    (sci-wolfram-run-repl                . "C-t")
    (sci-wolfram-import-package          . "C-i")
    (sci-wolfram-format-region-or-buffer . "C-f")
    (sci-wolfram-run-region-or-buffer    . "C-c")
    (sci-wolfram-convert-to-notebook     . "C-n"))
  "sci-wolfram-mode keymap")

(dolist (key sci-wolfram-mode-key)
  (define-key sci-wolfram-mode-leader-key-map (kbd (cdr key)) (car key)))
(define-key sci-wolfram-mode-map (kbd sci-wolfram-mode-leader-key) sci-wolfram-mode-leader-key-map)

;; sci-wolfram-mode
;;;###autoload
(define-derived-mode sci-wolfram-mode prog-mode "sci-wolfram"
  "Major mode for Wolfram Language.
\\{sci-wolfram-mode-map}"
  :syntax-table sci-wolfram-mode-syntax-table
  (setq-local syntax-propertize-function sci-wolfram-mode-syntax-propertize-function)
  (setq-local font-lock-defaults '((sci-wolfram-mode-font-lock-keywords)))
  (setq-local tab-width sci-wolfram-tab-width)
  (setq-local indent-tabs-mode nil)
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
         (script (expand-file-name "PrettifySymbols.wl" dir))
         (elisp (expand-file-name "sci-wolfram-prettify-symbols.el" dir)))
    (unless (file-exists-p elisp)
      (message "Convert Wolfram characters to Emacs prettify symbols")
      (shell-command (format "wolframscript -script %s" script)))))

(require 'sci-wolfram-prettify-symbols)

(add-hook 'sci-wolfram-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist sci-wolfram-prettify-symbols)
            (setq-local prettify-symbols-compose-predicate (lambda (_start _end _match) t))
            ;; (setq-local prettify-symbols-unprettify-at-point nil)
            (prettify-symbols-mode 1)))


(provide 'sci-wolfram)
;;; sci-wolfram.el ends here
