;;; sci-wolfram.el --- Major mode for editing Wolfram Language. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;;
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Created: 2025-05-20
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, Wolfram Language, Mathematica
;; Homepage: https://github.com/TurbulenceChaos/Wolfram-terminal-image

;; This file is not part of GNU Emacs

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Major mode for editing Wolfram Language code
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; How to use:
;; M-x describe-function then sci-wolfram-mode to see documentation.

;;; Customization:
;; (setq sci-wolfram-major-leader-key "TAB")

;;; Code:

;; TODO:
;; - [o] add wolfram LSPServer symbols;
;; - [o] add all wolfram LSPServer symbols to `completion-at-point' when `eglot' or `lsp-mode'=nil
;; - [o] use `CodeFormatter' to format region or buffer when `eglot' or `lsp-mode'=nil (ref: `format-all' pkg);
;; - [o] use `eglot' or `lsp-mode' to format wolframscript region or buffer and complete wolfram symbols;
;; - [o] eval region or buffer, and display output as images and latex in tmp buffer (ref: wolfram-mode pkg `EPrint');
;; - [o] convert wolfram script region or buffer to pdf and notebook, and use Wolfram Player to view notebook;
;; - [o] jupyter org-block find doc, completion-at-point, eval region, format region, convert region to pdf and notebook;
;; - [o] display wolfram images in jupyter repl;
;; - [o] `sci-wolfram-import-pkg': insert package based on emacs config;
;; - [ ] Add more wolfram LSPServer symbols;
;; - [ ] `sci-wolfram-jupyter-eval' and `sci-wolfram-jupyter-comment' based on `jupyter-eval'
;; - [ ] plan to remove `emacs-jupyter' dependence by replacing `jupyter-repl' with native wolfram repl (ref: `imaxima' pkg);
;;       develop `ob-sci-wolfram' and add session support to it (ref: `ob-mathematica', `ob-jupyter' pkg)

(defcustom sci-wolfram-image-dpi 150
  "sci-wolfram-image-dpi

Image output resolution: 150 DPI in \"emacs\" by default

You can specify it by

(custom-set-variables
 '(sci-wolfram-image-dpi 150))
"
  :type 'number
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-formula-type "image"
  "sci-wolfram-formula-type

For emacs, formula output types: \"latex\" or \"image\" (by default)

You can specify it by

(custom-set-variables
 '(sci-wolfram-formula-type \"latex\"))
"
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-orig-expr "no"
  "sci-wolfram-orig-expr

Whether to display both inline images and original expression: \"yes\" (Enable) or \"no\" (disable by default)

You can specify it by

(custom-set-variables
 '(sci-wolfram-orig-expr \"yes\"))
"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-play "no"
  "sci-wolfram-play

Whether to use `wolframplayer' to view `.cdf' files: \"yes\" (Enable) or \"no\" (disable by default)

You can specify it by

(custom-set-variables
 '(sci-wolfram-play \"yes\"))
"
  :type '(choice (const "yes") (const "no"))
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-player
  (string-trim-right
   (shell-command-to-string
    "wolframscript -code 'FileNames[\"*wolframplayer*\", $InstallationDirectory, 2][[1]]'"))

  "sci-wolfram-player

Path to the Wolfram Player, which is set as

(string-trim-right
   (shell-command-to-string
    \"wolframscript -code 'FileNames[\"*wolframplayer*\", $InstallationDirectory, 2][[1]]'\"))

You can also specify the path by

for linux:

(custom-set-variables
 '(sci-wolfram-player \"/path/to/wolframplayer\"))

for windows:
(custom-set-variables
 '(sci-wolfram-player \"/path/to/wolframplayer.exe\"))
"
  :type 'string
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-kernel
  (string-trim-right
   (shell-command-to-string
    "wolframscript -code 'First[$CommandLine]'"))
  "Path to WolframKernel executable for wolfram LSPServer usage"
  :type 'string
  :group 'sci-wolfram-mode)

(defmacro sci-wolfram-region-or-buffer-marco (name-1 name-2 body doc)
  "Use marco to define a function to process wolfram region or buffer code."
  `(defun ,(intern (format "sci-wolfram-%s-region-or-buffer%s" name-1 name-2)) (Begin End &optional Xinsert)
     ,doc
     (interactive
      (append
       (if (region-active-p)
	   (list (region-beginning) (region-end))
	 (list (point-min) (point-max)))
       current-prefix-arg nil))
     ;; (when (not buffer-file-name)
     ;;   (user-error "Buffer is not a file. Save it first!"))
     (let* ((state
	     (if (region-active-p)
		 "region"
	       "buffer"))
	    (buffer-type (if buffer-file-name
			     "file"
			   "not-file"))
	    (code (buffer-substring-no-properties Begin End))
	    (file (if (string= buffer-type "file")
		      buffer-file-name
		    ;; for edit org-src block, where buffer is not a file
		    (let* ((dir default-directory)
			   (filename (expand-file-name
				      (format "%s-%s-%x.wl"
					      (replace-regexp-in-string
					       "^-\\|-$"
					       ""
					       (replace-regexp-in-string
						"[^a-z0-9]+"
						"-"
						(downcase (buffer-name))))
					      (format-time-string "%Y%m%d-%H%M%S")
					      (random #xfffff))
				      dir)))
		      (unless (file-directory-p dir)
			(make-directory dir t))
		      (with-temp-file filename
			(insert code))
		      filename)))
	    (dir (file-name-directory file))
	    (tmpfile (expand-file-name
		      (format "%s-%s-%x.wl"
			      (file-name-base file)
			      (format-time-string "%Y%m%d%-H%M%S")
			      (random #xfffff))
		      dir)))
       (when (and (string= state "buffer")
		  (not (eq major-mode 'sci-wolfram-mode)))
	 (user-error "Buffer is not a wolfram script!"))
       ;; send region or buffer to tmp file
       (with-temp-file tmpfile (insert code))
       ;; (sci-wolfram-format Begin End buffer-type file tmpfile)
       ;; (sci-wolfram-eval state buffer-type file tmpfile)
       ;; (sci-wolfram-to-pdf-and-notebook state buffer-type file tmpfile dir)
       ,body)))

;; format region or buffer
(setq sci-wolfram-format-script
      (expand-file-name "sci-wolfram-format.wl"
			(file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-format (Begin End buffer-type file tmpfile)
  (let ((formatted-code
	 (string-trim-right
	  (shell-command-to-string
	   (format "wolframscript -script %s %s"
		   sci-wolfram-format-script tmpfile)))))
    (delete-region Begin End)
    (insert (concat formatted-code "\n"))
    (if (string= buffer-type "not-file")
	(delete-file file))
    (delete-file tmpfile)))

(sci-wolfram-region-or-buffer-marco
 "format"
 ""
 (sci-wolfram-format Begin End buffer-type file tmpfile)
 "Use `CodeFormatter' (https://reference.wolfram.com/language/CodeFormatter/ref/CodeFormat.html)
to format wolfram region or buffer code.")

;; eval region or buffer
(setq sci-wolfram-eval-script
      (expand-file-name "sci-wolfram-eval.wl"
			(file-name-directory (or load-file-name buffer-file-name))))

(setq sci-wolfram-image-script
      (expand-file-name "sci-wolfram-image.wl"
			(file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-import-pkg-string ()
  "Import `sci-wolfram-image.wl' package to string"
  (concat
   (format
    "Get[\"%s\"];\n\n"
    sci-wolfram-image-script)
   (format 
    "sciWolframImageDPI = %s;\n\n"
    sci-wolfram-image-dpi)
   (format 
    "sciWolframFormulaType = \"%s\";\n\n"
    sci-wolfram-formula-type)
   (format 
    "sciWolframOrigExpr = \"%s\";\n\n"
    sci-wolfram-orig-expr)
   (format 
    "sciWolframPlay = \"%s\";\n\n"
    sci-wolfram-play)
   (format 
    "sciwolframPlayer = \"%s\";"
    sci-wolfram-player)))

(defun sci-wolfram-import-pkg ()
  "Insert `sci-wolfram-image.wl' package at point"
  (interactive)
  (let ((pkg (sci-wolfram-import-pkg-string)))
    (save-excursion
      (forward-line 1)
      (insert (concat pkg "\n")))))

(defun sci-wolfram-eval (state buffer-type file tmpfile)
  "Execute file with `wolframscript' and pretty print all expressions."
  (let* ((wrap-code
	  (string-trim-right
	   (shell-command-to-string
	    (format "wolframscript -script %s %s" sci-wolfram-eval-script tmpfile))))) 
    (with-temp-file tmpfile
      (insert (concat (sci-wolfram-import-pkg-string) "\n\n" wrap-code))))

  (let ((outbuf (get-buffer-create
		 (format
		  "*Eval %s of %s*"
		  state (file-name-nondirectory file))
		 t))
        (command (list "wolframscript" "-script" tmpfile)))
    (with-current-buffer outbuf
      (erase-buffer)
      (insert (if (string= state "region")
		  (format "Eval region of %s\n\n" file)
		(format "Eval %s\n\n" file)))
      (insert (format "Send %s to %s\n\n" state tmpfile))
      (insert (format
	       "Run wolframscript -script %s %s to wrap original code\n\n"
	       sci-wolfram-eval-script tmpfile))
      (insert (format "Import %s package\n\n" sci-wolfram-image-script))
      (insert (format "Run wolframscript -script %s\n\n:results:\n" tmpfile)))

    (let ((proc (make-process
		 :name "sci-wolfram-eval"
		 :buffer outbuf
		 :command command
		 :connection-type 'pipe)))

      (set-process-sentinel
       proc
       (lambda (process event)
	 (when (string-match "finished\\|exited" event)
	   (if (string= buffer-type "not-file")
               (delete-file file))
	   (delete-file tmpfile)
	   (with-current-buffer outbuf
	     (goto-char (point-max))
	     (insert (format ":end:" tmpfile))
	     (insert (format "\n\nDelete %s" tmpfile))
	     (if (string= buffer-type "not-file")
		 (insert (format "\n\nDelete %s" file)))

	     (unless (eq major-mode 'org-mode)
	       (org-mode))
	     (visual-line-mode 1)
	     (goto-char (point-min))
	     (when (string= sci-wolfram-formula-type "latex")
	       (org-remove-latex-fragment-image-overlays)
	       (org-toggle-latex-fragment))
	     (org-remove-inline-images)
	     (org-toggle-inline-images)
	     (goto-char (point-max)))))))
    (display-buffer outbuf)))

(defun sci-wolfram-jupyter-eval (state buffer-type file tmpfile)
  "Execute file with `jupyter-repl' and pretty print all expressions."
  (let ((outbuf (get-buffer-create
		 (format
		  "*Jupyter eval %s of %s*"
		  state (file-name-nondirectory file))
		 t)))
    (with-current-buffer outbuf
      (unless (eq major-mode 'org-mode)
	(org-mode))
      (visual-line-mode 1)
      (erase-buffer)
      (insert (if (string= state "region")
		  (format "Jupyter eval region of %s\n\n" file)
		(format "Jupyter eval %s\n\n" file)))
      (insert (format "Send %s to %s\n\n" state tmpfile))

      (insert (format "Import %s package\n\n" sci-wolfram-image-script))
      (insert (concat
	       "#+name: sci-wolfram-import-pkg\n"
	       "#+begin_src jupyter-Wolfram-Language\n"
	       (sci-wolfram-import-pkg-string)
	       "\n#+end_src\n\n"))

      (insert (format "Insert code from %s\n\n" tmpfile))
      (insert (concat
	       (if (string= state "region")
		   (format "#+name: sci-wolfram-jupyter-eval-region-of-%s\n" (file-name-nondirectory file))
		 (format "#+name: sci-wolfram-jupyter-eval-%s\n" (file-name-nondirectory file)))
	       "#+begin_src jupyter-Wolfram-Language\n"
	       (string-trim-right
		(with-temp-buffer
		  (insert-file-contents tmpfile)
		  (buffer-string)))
	       "\n#+end_src\n\n"))

      (delete-file tmpfile)
      (insert (format "Delete %s\n\n" tmpfile))
      (when (string= buffer-type "not-file")
        (delete-file file)
	(insert (format "Delete %s\n\n" file)))

      (insert "Run `org-babel-execute-buffer'")

      (org-fold-hide-block-all)
      (org-babel-execute-buffer))
    (display-buffer outbuf)))

(sci-wolfram-region-or-buffer-marco
 "eval"
 ""
 (sci-wolfram-eval state buffer-type file tmpfile)
 "Use `wolframscript' to eval wolfram region or buffer code and pretty print all expressions.")

(sci-wolfram-region-or-buffer-marco
 "jupyter-eval"
 ""
 (sci-wolfram-jupyter-eval state buffer-type file tmpfile)
 "Use `jupyter-repl' to eval wolfram region or buffer code and pretty print all expressions.")

;; convert region or buffer to pdf and Mathematica notebook,
;; and then use `wolframplayer' to view the notebook if `sci-wolfram-play' = "yes"
(setq sci-wolfram-pdf-script (expand-file-name
			      "sci-wolfram-pdf.wl"
			      (file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-to-pdf-and-notebook (state buffer-type file tmpfile dir)
  "Execute the current file with WolframScript and convert it to PDF."
  (let* ((outbuf (get-buffer-create
		  (format
		   "*Convert %s of %s to pdf and notebook*"
		   state (file-name-nondirectory file))))
	 (file-no-dir (file-name-nondirectory file)) 
	 (file-name (file-name-base file))
	 (pdf (concat
	       file-name
	       "-convert.pdf"))
	 (notebook (concat
		    file-name
		    "-convert.nb"))
	 (tmpfile-no-dir (file-name-nondirectory tmpfile))
	 (tmpfile-name (file-name-base tmpfile))
	 (tmppdf (concat
		  tmpfile-name
		  "-convert.pdf"))
	 (tmpnotebook (concat
		       tmpfile-name
		       "-convert.nb"))
         (command (list "wolframscript"
			"-script"
			sci-wolfram-pdf-script
			tmpfile
			;; sci-wolfram-play
			;; sci-wolfram-player
			)))
    (with-current-buffer outbuf
      (erase-buffer)
      ;; (unless (eq major-mode 'org-mode)
      ;; 	(org-mode))
      (visual-line-mode 1)
      (if (string= state "region")
	  (insert (format
		   "Convert region of %s to %s.pdf and %s.nb in %s folder"
		   file-no-dir file-name file-name dir))
	(insert (format
		 "Convert %s to %s.pdf and %s.nb in %s folder"
		 file-no-dir file-name file-name dir)))
      (insert (format
	       "\n\nRun %s\n\n"
	       command)))
    (let ((proc (make-process
		 :name "sci-wolfram-to-pdf-and-notebook"
		 :buffer outbuf
		 :command command
		 :connection-type 'pipe)))
      (set-process-sentinel proc
			    (lambda (process event)
			      (when (string-match "finished\\|exited" event)
				(rename-file (concat dir tmppdf) (concat dir pdf) t)
				(rename-file (concat dir tmpnotebook) (concat dir notebook) t)

				(if (string= buffer-type "not-file")
				    (delete-file file))
				(delete-file tmpfile)

				(with-current-buffer outbuf
				  (insert (format "Rename %s to %s\n\n" tmppdf pdf))
				  (insert (format "Rename %s to %s\n\n" tmpnotebook notebook))
				  (save-excursion
				    (goto-char (point-max))
				    (insert (format "Delete %s\n\n" tmpfile)))
				  (if (string= buffer-type "not-file")
				      (save-excursion
					(goto-char (point-max))
					(insert (format "\n\nDelete %s" file)))))

				(when (string= sci-wolfram-play "yes")
				  (with-current-buffer outbuf
				    (save-excursion
				      (goto-char (point-max))
				      (insert (format "Use wolframplayer to view %s" notebook))))
				  (let ((default-directory dir))
				    (start-process "sci-wolfram-play" nil sci-wolfram-player notebook)))

				(let ((right-window (or (window-in-direction 'right) (split-window-right))))
				  (select-window right-window)
				  (with-current-buffer (current-buffer)
				    (setq-local revert-without-query '(".pdf"))
				    (find-file (concat dir pdf))))))))
    (display-buffer outbuf)))

(sci-wolfram-region-or-buffer-marco
 "convert"
 "-to-pdf-and-notebook"
 (sci-wolfram-to-pdf-and-notebook state buffer-type file tmpfile dir)
 "Convert wolfram region or buffer code to pdf and Mathematica notebook.
Then use `wolframplayer' (free to use) (https://www.wolfram.com/player/) to view the notebook.")

;; eldoc
(defun sci-wolfram-doc-lookup ()
  "Look up the symbol under cursor in Wolfram doc site in web browser."
  (interactive)
  (let* ((xword
	  (if (region-active-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (upcase-initials (current-word))))
         (xurl (format "https://reference.wolfram.com/language/ref/%s.html" xword)))
    (browse-url xurl)))

;; completion
(require 'sci-wolfram-all-symbols)

(defun sci-wolfram-complete-symbol ()
  "Do keyword completion on current symbol."
  (interactive)
  (let ((xp0 (point)) xbeg xend xword xresultW)
    (save-excursion
      (skip-chars-backward "$A-Za-z0-9") (setq xbeg (point))
      (goto-char xp0)
      (skip-chars-forward "$A-Za-z0-9") (setq xend (point)))
    (setq xword (buffer-substring-no-properties xbeg xend))
    (setq xresultW (completing-read "keyword:" sci-wolfram-all-symbols nil t xword))
    (delete-region xbeg xend)
    (goto-char xbeg)
    (insert xresultW)))

(defun sci-wolfram-completion-at-point ()
  "Provide completion for completion-at-point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
	    (cdr bounds)
	    sci-wolfram-all-symbols
	    :exclusive 'no))))

(defun sci-wolfram-mode-add-completion ()
  "Add completion of wolfram symbols in sci-wolfram-mode."
  (add-hook 'completion-at-point-functions
	    #'sci-wolfram-completion-at-point nil t))

(defun sci-wolfram-mode-remove-completion ()
  "Remove completion of wolfram symbols in sci-wolfram-mode."
  (remove-hook 'completion-at-point-functions
	       #'sci-wolfram-completion-at-point t))

(add-hook 'sci-wolfram-mode-hook #'sci-wolfram-mode-add-completion)

;; lsp server
(defun sci-wolfram-setup-lsp-server ()
  "Set up wolfram lsp server by removing local version if needed."
  (let ((lsp-server-path
	 (string-trim-right
	  (shell-command-to-string
	   (concat
	    "wolframscript -code "
	    "'"
	    "LSPServerPath = PacletObject[\"LSPServer\"][\"Location\"]; "
	    "If[StringContainsQ[LSPServerPath, $UserBasePacletsDirectory], "
	    "LSPServerPath, "
	    "\"Built-in LSPServer\"]"
	    "'")))))
    (if (not (string= lsp-server-path "Built-in LSPServer"))
	(if (yes-or-no-p
	     (format "Found locally installed LSPServer in \"%s\".
In order to use the built-in LSPServer, the local version should be removed.
Use PacletUninstall[\"LSPServer\"] to remove it?"
		     lsp-server-path))
	    (if (= 0 (shell-command "wolframscript -code 'PacletUninstall[\"LSPServer\"]'"))
		(message "Local LSPServer successfully removed.")
	      (message "Failed to remove local LSPServer."))
	  (message "Local LSPServer retained. Built-in version may not work properly.")))))

(add-hook 'eglot-managed-mode-hook
	  (lambda ()
	    (when (derived-mode-p 'sci-wolfram-mode)
	      (sci-wolfram-mode-remove-completion)
	      (sci-wolfram-setup-lsp-server))))

(add-hook 'eglot-shutdown-hook
	  (lambda ()
	    (when (derived-mode-p 'sci-wolfram-mode)
	      (sci-wolfram-mode-add-completion))))

(add-hook 'lsp-mode-hook
	  (lambda ()
	    (when (derived-mode-p 'sci-wolfram-mode)
	      (sci-wolfram-mode-remove-completion)
	      (sci-wolfram-setup-lsp-server))))

(add-hook 'lsp-after-uninitialized-functions
	  (lambda (_workspace)
	    (when (derived-mode-p 'sci-wolfram-mode)
	      (sci-wolfram-mode-add-completion))))

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
		       "-noinit" "-noprompt" "-nopaclet" "-noicon" "-nostartuppaclets"
		       "-run" "PacletUninstall[\"LSPServer\"]; Needs[\"LSPServer`\"]; LSPServer`StartServer[]"))
    :major-modes '(sci-wolfram-mode)
    :server-id 'wolfram-lsp)))

;; syntax table
(defvar sci-wolfram-mode-syntax-table nil "Syntax table for `sci-wolfram-mode'.")

(setq
 sci-wolfram-mode-syntax-table
 (let ((xsynTable (make-syntax-table )))

   ;; comment
   (modify-syntax-entry ?\( "()1n" xsynTable)
   (modify-syntax-entry ?\) ")(4n" xsynTable)
   (modify-syntax-entry ?* ". 23n" xsynTable)

   ;; symbol
   (modify-syntax-entry ?$ "_" xsynTable)

   ;; punctuation
   (modify-syntax-entry ?! "." xsynTable)
   (modify-syntax-entry ?# "." xsynTable)
   (modify-syntax-entry ?% "." xsynTable)
   (modify-syntax-entry ?& "." xsynTable)
   (modify-syntax-entry ?' "." xsynTable)
   (modify-syntax-entry ?+ "." xsynTable)
   (modify-syntax-entry ?, "." xsynTable)
   (modify-syntax-entry ?- "." xsynTable)
   (modify-syntax-entry ?. "." xsynTable)
   (modify-syntax-entry ?/ "." xsynTable)
   (modify-syntax-entry ?: "." xsynTable)
   (modify-syntax-entry ?\; "." xsynTable)
   (modify-syntax-entry ?< "." xsynTable)
   (modify-syntax-entry ?= "." xsynTable)
   (modify-syntax-entry ?> "." xsynTable)
   (modify-syntax-entry ?? "." xsynTable)
   (modify-syntax-entry ?@ "." xsynTable)
   (modify-syntax-entry ?\ "." xsynTable)
   (modify-syntax-entry ?^ "." xsynTable)
   (modify-syntax-entry ?_ "." xsynTable)
   (modify-syntax-entry ?` "." xsynTable)
   (modify-syntax-entry ?| "." xsynTable)
   (modify-syntax-entry ?~ "." xsynTable)
   (modify-syntax-entry ?\\ "." xsynTable)
   xsynTable))

;; syntax coloring related
(defvar sci-wolfram-font-lock-keywords nil "Value for `font-lock-defaults'")

(setq
 sci-wolfram-font-lock-keywords
 `(
   (,(regexp-opt BuiltinFunctions-1 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-2 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-3 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-4 'symbols) . font-lock-function-name-face)
   (,(regexp-opt BuiltinFunctions-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-usr-functions 'symbols) . font-lock-function-name-face)
   (,(regexp-opt Constants 'symbols) . font-lock-builtin-face)
   (,(regexp-opt SystemLongNames 'symbols) . font-lock-constant-face)
   ("\\b[a-z][A-Za-z0-9]*" . font-lock-variable-name-face)
   ("\\b[A-Z][A-Za-z0-9]*" . font-lock-warning-face)))

;; keybinding
(defvar sci-wolfram-mode-map nil "Keybinding for `sci-wolfram-mode'")
(setq sci-wolfram-mode-map (make-sparse-keymap))
(define-prefix-command 'sci-wolfram-leader-map)

(define-key sci-wolfram-mode-map
	    (kbd (if (boundp 'sci-wolfram-major-leader-key)
		     sci-wolfram-major-leader-key
		   "<f6>"))
	    sci-wolfram-leader-map)
(define-key sci-wolfram-leader-map (kbd "SPC") #'sci-wolfram-complete-symbol)
(define-key sci-wolfram-leader-map (kbd "h") #'sci-wolfram-doc-lookup)
(define-key sci-wolfram-leader-map (kbd "f") #'sci-wolfram-format-region-or-buffer)
(define-key sci-wolfram-leader-map (kbd "e") #'sci-wolfram-eval-region-or-buffer)
(define-key sci-wolfram-leader-map (kbd "j") #'sci-wolfram-jupyter-eval-region-or-buffer)
(define-key sci-wolfram-leader-map (kbd "c") #'sci-wolfram-convert-region-or-buffer-to-pdf-and-notebook)

(defvar sci-wolfram-mode-hook nil "Hook for function `sci-wolfram-mode'.")

;;;###autoload
(define-derived-mode sci-wolfram-mode prog-mode "sci-wolfram"
  "A major mode for Wolfram Language."
  :syntax-table sci-wolfram-mode-syntax-table
  (setq font-lock-defaults '((sci-wolfram-font-lock-keywords)))
  (setq-local comment-start "(*")
  (setq-local comment-end "*)"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wl\\'" . sci-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wls\\'" . sci-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.m\\'" . sci-wolfram-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nb\\'" . sci-wolfram-mode))


(provide 'sci-wolfram)

;;; sci-wolfram.el ends here
