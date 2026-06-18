;;; sci-wolfram.el --- Major mode for editing Wolfram Language. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Peng Peng
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
(require 'sci-wolfram-lsp-symbols)

;;;###autoload
(defgroup sci-wolfram-mode nil "Major mode for wolfram script")

(defcustom sci-wolfram-formula-type "image"
  "Wolfram fomula output type: image (default) or latex"
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-image-dpi 100
  "Wolfram image resolution: 100 (default)"
  :type 'number
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-play "no"
  "Convert plots to Mathematica interactive file: yes or no (default)"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram-mode)

;; run wolfram script region or buffer code
;;;###autoload
(defun sci-wolfram-run-repl ()
  "Run wolfram repl"
  (interactive)
  (sci-wolfram-make-repl)
  (switch-to-buffer-other-window sci-wolfram-repl-buffer))

(defvar sci-wolfram-display-image-script
  (expand-file-name "sciWolframDisplayImage.wl"
		    (file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-display-image-package ()
  "sciWolframDisplayImage.wl package"
  (concat
   (format "\nGet[\"%s\"];\n\n" sci-wolfram-display-image-script)
   "(* sciWolframDisplayImage.wl\n\n"
   "Display wolfram script image.\n\n"
   "Usage:\n\n"
   "Default:\n"
   "$Post = sciWolframDisplayImage[#] &;\n\n"
   "All options:\n"
   "$Post = sciWolframDisplayImage[#,\n"
   "sciWolframFormulaType -> \"image\" (default) or \"latex\",\n"
   "sciWolframImageDPI    -> 100 (default),\n"
   "sciWolframPlay        -> \"yes\" or \"no\" (default) to convert plots to Mathematica interactive file\n"
   "] &;\n\n"
   "Tyep below code to reset $Post:\n"
   "$Post = .\n\n"
   "*)\n\n"
   "$Post = sciWolframDisplayImage[#,\n"
   (format "sciWolframFormulaType -> \"%s\",\n" sci-wolfram-formula-type)
   (format "sciWolframImageDPI    -> %s,\n" sci-wolfram-image-dpi)
   (format "sciWolframPlay        -> \"%s\"\n" sci-wolfram-play)
   "] &;\n\n"))

;;;###autoload
(defmacro sci-wolfram-import-package-macro (func-name func-doc pkg)
  "Define a function to import sci-wolfram package"
  `(defun ,func-name ()
     ,func-doc
     (interactive)
     (let ((pkg (,pkg)))
       (save-excursion
	 (forward-line 1)
	 (insert pkg)
	 (if (eq major-mode 'org-mode)
	     (org-element-cache-reset))))))

;;;###autoload
(sci-wolfram-import-package-macro
 sci-wolfram-import-display-image-package
 "Import sciWolframDisplayImage.wl package"
 sci-wolfram-display-image-package)

(defun sci-wolfram-get-region-or-buffer-code ()
  "Return code in region or buffer without space lines"
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (code (buffer-substring-no-properties beg end)))
    (sci-wolfram-remove-space-lines code)))

(defun sci-wolfram-mode-run-region-or-buffer (&optional code)
  "Run wolfram script region or buffer code"
  (let ((code (or code (sci-wolfram-get-region-or-buffer-code)))
	(outbuf (get-buffer-create "*Sci-Wolfram Run Result*"))
	(lang sci-wolfram-org-src-block-name))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
	(org-mode))
      (erase-buffer)
      (insert (concat
	       "#+name: import-sci-wolfram-display-image-package\n"
	       (format "#+begin_src %s\n" lang)
	       (sci-wolfram-display-image-package)
	       "#+end_src\n\n"))
      (insert (concat
	       "#+name: sci-wolfram-run-region-or-buffer\n"
	       (format "#+begin_src %s\n" lang)
	       code
	       "\n#+end_src\n\n"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer))
    (display-buffer outbuf)))

;;;###autoload
(defun sci-wolfram-run-region-or-buffer ()
  "Run wolfram script region or buffer code"
  (interactive)
  (cond
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (sci-wolfram-mode-run-region-or-buffer))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang sci-wolfram-org-src-block-name)))
    (let ((code (prog2 (org-edit-src-code)
		    (sci-wolfram-get-region-or-buffer-code)
		  (org-edit-src-exit))))
      (sci-wolfram-mode-run-region-or-buffer code)))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; Convert wolfram script to PDF and Mathematica notebook
(defvar sci-wolfram-convert-to-notebook-script
  (expand-file-name
   "sciWolframConvertToNotebook.wl"
   (file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-convert-to-notebook-package ()
  "sciWolframConvertToNotebook.wl package"
  (concat
   (format "\nGet[\"%s\"];\n\n" sci-wolfram-convert-to-notebook-script)
   "(* sciWolframConvertToNotebook.wl\n\n"
   "Convert wolfram script to PDF and Mathematica notebook.\n\n"
   "Usage:\n\n"
   "sciWolframConvertToNoteBook[\"/path/to/file.wl\"];\n\n"
   "*)\n\n"))

;;;###autoload
(sci-wolfram-import-package-macro
 sci-wolfram-import-convert-to-notebook-package
 "Import sciWolframDisplayImage.wl package"
 sci-wolfram-convert-to-notebook-package)

(defun sci-wolfram-mode-convert-to-notebook (&optional file)
  "Convert wolfram script to PDF and Mathematica notebook"
  (let ((file (or file (buffer-file-name)))
	(outbuf (get-buffer-create "*Sci-Wolfram Convert Result*"))
	(lang sci-wolfram-org-src-block-name))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
	(org-mode))
      (erase-buffer)
      (insert (concat
	       "#+name: import-sci-wolfram-convert-to-notebook-package\n"
	       (format "#+begin_src %s\n" lang)
	       (sci-wolfram-convert-to-notebook-package)
	       "#+end_src\n\n"))
      (insert (concat
	       "#+name: sci-wolfram-convert-to-notebook\n"
	       (format "#+begin_src %s\n" lang)
	       (format "sciWolframConvertToNotebook[\"%s\"];" file)
	       "\n#+end_src\n\n"))
      (org-fold-hide-block-all)
      (org-babel-execute-buffer))
    (display-buffer outbuf)))

;;;###autoload
(defun sci-wolfram-convert-to-notebook ()
  "Convert wolfram script to PDF and Mathematica notebook"
  (interactive)
  (cond
   ((and (buffer-file-name)
	 (derived-mode-p 'sci-wolfram-mode))
    (sci-wolfram-mode-convert-to-notebook))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang sci-wolfram-org-src-block-name)))
    (let* ((code (prog2 (org-edit-src-code)
		     (sci-wolfram-get-region-or-buffer-code)
		   (org-edit-src-exit)))
	   (info (org-babel-get-src-block-info))
	   (src-block-name (or (nth 4 info) sci-wolfram-org-src-block-name))
	   (file-name (format "%s-%s.wl"
			      (replace-regexp-in-string "[^a-zA-Z0-9_.\\-]" "" (file-name-sans-extension (buffer-name)))
			      src-block-name))
	   (file (expand-file-name file-name default-directory)))
      (write-region code nil file)
      (sci-wolfram-mode-convert-to-notebook file)))
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (let* ((code (sci-wolfram-get-region-or-buffer-code))
	   (file-name (format "%s-region-or-buffer.wl"
			      (replace-regexp-in-string "[^a-zA-Z0-9_.\\-]" "" (file-name-sans-extension (buffer-name)))))
	   (file (expand-file-name file-name default-directory)))
      (write-region code nil file)
      (sci-wolfram-mode-convert-to-notebook file)))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; format region or buffer
(defun sci-wolfram-mode-format-region-or-buffer ()
  "Format wolfram codes"
  (sci-wolfram-make-repl)
  (sci-wolfram-initiate-session)
  (let* ((code (sci-wolfram-get-region-or-buffer-code))
	 (eoe (format "comint_wolfram_format_%s" (org-id-uuid)))
	 (format-code (concat
		       (format "Needs[\"CodeFormatter`\"];\nWriteString[\"stdout\", CodeFormat[\"%s\"], \"\\n\"];\n" code)
		       (format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))
	 (result
	  (org-babel-comint-with-output
	      (sci-wolfram-repl-buffer eoe)
	    (comint-send-string sci-wolfram-repl-buffer format-code))))
    (save-excursion
      (if (region-active-p)
	  (delete-region (region-beginning) (region-end))
	(erase-buffer))
      (insert (sci-wolfram-remove-eoe result eoe)))))

;;;###autoload
(defun sci-wolfram-format-region-or-buffer ()
  "Format wolfram codes"
  (interactive)
  (cond
   ((or (region-active-p)
	(derived-mode-p 'sci-wolfram-mode))
    (sci-wolfram-mode-format-region-or-buffer))
   ((and (derived-mode-p 'org-mode)
	 (org-in-src-block-p)
	 (let* ((info (org-babel-get-src-block-info))
		(lang (nth 0 info)))
	   (string= lang sci-wolfram-org-src-block-name)))
    (org-edit-src-code)
    (sci-wolfram-format-region-or-buffer)
    (org-edit-src-exit))
   (t (user-error "You must be in a selected region, a sci-wolfram-mode buffer, or a wolfram org-src block!"))))

;; doc lookup
;;;###autoload
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
(defun sci-wolfram-completion-at-point ()
  "Add wolfram LSP symbols to completion-at-point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
	    (cdr bounds)
	    sci-wolfram-lsp-symbols ; completion-table-dynamic
	    :exclusive 'no))))

(add-hook 'sci-wolfram-mode-hook
	  (lambda () (add-hook 'completion-at-point-functions 'sci-wolfram-completion-at-point nil t)))

(defun sci-wolfram-org-block-completion-at-point ()
  "Provide completion in wolfram org block"
  (when (org-in-src-block-p 1)
    (let* ((info (org-babel-get-src-block-info))
	   (lang (nth 0 info)))
      (when (string= lang sci-wolfram-org-src-block-name)
	(sci-wolfram-completion-at-point)))))

(add-hook 'org-mode-hook
	  (lambda () (add-hook 'completion-at-point-functions 'sci-wolfram-org-block-completion-at-point nil t)))

;; lsp server
(require 'sci-wolfram-kernel)

;; (defun sci-wolfram-remove-local-lsp-server ()
;;   (interactive)
;;   "Remove local installed LSPServer if needed."
;;   (async-shell-command "wolframscript -code 'PacletUninstall[\"LSPServer\"];'"))

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
    :major-modes '(sci-wolfram-mode)
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
   ;; (modify-syntax-entry ?\ "." synTable)
   (modify-syntax-entry ?^ "." synTable)
   (modify-syntax-entry ?_ "." synTable)
   (modify-syntax-entry ?` "." synTable)
   (modify-syntax-entry ?| "." synTable)
   (modify-syntax-entry ?~ "." synTable)
   ;; (modify-syntax-entry ?\\ "." synTable)
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

   (,(regexp-opt Constants 'symbols) . font-lock-constant-face)
   (,(regexp-opt Options 'symbols) . font-lock-builtin-face)
   (,(regexp-opt SessionSymbols 'symbols) . font-lock-keyword-face)
   (,(regexp-opt ExperimentalSymbols 'symbols) . font-lock-warning-face)
   (,(regexp-opt UndocumentedSymbols 'symbols) . font-lock-doc-face)
   (,(regexp-opt ObsoleteSymbols 'symbols) . font-lock-comment-face)
   (,(regexp-opt BadSymbols 'symbols) . font-lock-negation-char-face)

   (,(concat "\\\\\\[" (regexp-opt SystemLongNames) "\\]") . font-lock-constant-face) ; SystemCharacters
   (,(concat "\\\\\\[" (regexp-opt FreeLongNames) "\\]") . font-lock-constant-face) ; FreeCharacters
   (,(concat "\\\\\\[" (regexp-opt SpecialLongNames) "\\]") . font-lock-builtin-face) ; SpecialCharacters
   (,(concat "\\\\\\[" (regexp-opt UndocumentedLongNames) "\\]") . font-lock-doc-face) ; UndocumentedCharacters
   (,(concat "\\\\\\[" (regexp-opt UnsupportedLongNames) "\\]") . font-lock-warning-face) ; UnsupportedCharacters

   ("[A-Za-z][A-Za-z0-9]*" . font-lock-variable-name-face)))

;; keybinding
(defvar sci-wolfram-mode-leader-key "C-c" "leader key for sci-wolfram-mode")

(defvar sci-wolfram-key-map
  '((sci-wolfram-doc-lookup . "h")
    (sci-wolfram-run-repl . "t")
    (sci-wolfram-import-display-image-package . "i")
    (sci-wolfram-import-convert-to-notebook-package . "n")
    (sci-wolfram-format-region-or-buffer . "f")
    (sci-wolfram-run-region-or-buffer . "r")
    (sci-wolfram-convert-to-notebook . "c"))
  "sci-wolfram key map")

(defvar sci-wolfram-mode-map nil "keymap for sci-wolfram-mode.")

(setq sci-wolfram-mode-map (make-sparse-keymap))

(dolist (key-map sci-wolfram-key-map)
  (define-key sci-wolfram-mode-map
	      (kbd (format "%s %s" sci-wolfram-mode-leader-key (cdr key-map)))
	      (car key-map)))

;; sci-wolfram-mode
;;;###autoload
(define-derived-mode sci-wolfram-mode prog-mode "sci-wolfram"
  "Major mode for Wolfram Language"
  :syntax-table sci-wolfram-mode-syntax-table
  :keymap sci-wolfram-mode-map
  (setq font-lock-defaults '((sci-wolfram-font-lock-keywords)))
  (setq-local comment-start "(*")
  (setq-local comment-end "*)"))

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
