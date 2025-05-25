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
;; - [o] wolfram player;
;; - [o] add usr symbols;
;; - [o] Codeformatter (when eglot=nil, reference: sci-wolfram-eval-region, format-all-region);
;; - [ ] sci-wolfram-jupyter insert package (reference: jupyter-insert);
;; - [o] completion-at-point (when eglot=nil)
;; - [o] emacs-jupyter org block completion-at-point;
;; - [o] emacs-jupyter org-block doc and completion with TAB;
;; - [o] jupyter repl, send line, region, buffer ;
;; - [o] display wolfram images in jupyter repl;
;; - [ ] eval line, region, buffer (display images and latex in tmp buffer, reference: EPrint);

(defmacro sci-wolfram-region-or-buffer-function (name-1 name-2 body doc)
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
		    ;; for sci-wolfram-jupyter to edit source code block
		    ;; buffer is not a file
		    (let ((dir (concat temporary-file-directory "sci-wolfram")))
		      (unless (file-directory-p dir)
			(make-directory dir t))
		      (with-temp-file (expand-file-name
				       (format "%s-%s-%x.wl"
					       (file-name-base file)
					       (format-time-string "%Y%m%d%-H%M%S")
					       (random #xfffff))
				       dir)
			(insert code)))))
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
	   (format "wolframscript -script %s %s" sci-wolfram-format-script tmpfile)))))
    (delete-region Begin End)
    (insert (concat formatted-code "\n"))
    (if (string= buffer-type "not-file")
	(delete-file file))
    (delete-file tmpfile)))

(sci-wolfram-region-or-buffer-function
 "format"
 ""
 (sci-wolfram-format Begin End buffer-type file tmpfile)
 "Use `CodeFormatter' (https://reference.wolfram.com/language/CodeFormatter/ref/CodeFormat.html)
to format wolfram region or buffer code.")

;; eval region or buffer (plain text)
(defun sci-wolfram-eval (state buffer-type file tmpfile)
  "Execute the current file with WolframScript and print all expressions as plain texts."
  (let ((outbuf (get-buffer-create "*sci-wolfram-eval output*" t))
        (command (format "wolframscript -print all -file %s" tmpfile)))
    (with-current-buffer outbuf
      (erase-buffer)
      (insert (if (string= state "region")
		  (format "Eval region of %s\n\n" file)
		(format "Eval %s\n\n" file)))
      (insert (format "Send %s to %s\n\n" state tmpfile))
      (insert (format "Running: %s\n\nResults:\n\n" command)))
    (let ((proc (start-process-shell-command "sci-wolfram-eval-buffer" outbuf command)))
      (set-process-sentinel proc
			    (lambda (process event)
			      (when (string-match "finished\\|exited" event)
				(if (string= buffer-type "not-file")
				    (delete-file file))
				(delete-file tmpfile)
				(with-current-buffer outbuf
				  (insert (format "\nDelete %s" tmpfile))
				  (if (string= buffer-type "not-file")
				      (insert (format "\n\nDelete %s" file))))))))
    (display-buffer outbuf)))

(sci-wolfram-region-or-buffer-function
 "eval"
 ""
 (sci-wolfram-eval state buffer-type file tmpfile)
 "Use `wolframscript' to eval wolfram region or buffer code and print all expressions as plain texts.")

;; eval region or buffer for pretty print
(setq sci-wolfram-eval-script
      (expand-file-name "sci-wolfram-eval.wl"
			(file-name-directory (or load-file-name buffer-file-name))))

(setq sci-wolfram-image-script
      (expand-file-name "sci-wolfram-image.wl"
			(file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-eval-pretty (state buffer-type file tmpfile)
  "Execute the current file with WolframScript and pretty print all expressions."
  (let ((wrap-code
	 (string-trim-right
	  (shell-command-to-string
	   (format "wolframscript -script %s %s" sci-wolfram-eval-script tmpfile)))))
    (with-temp-file tmpfile
      (insert
       (concat
	(format "Get[\"%s\"]\n\n" sci-wolfram-image-script)
	"sciWolframRunner = \"emacs\";\n\n"
	wrap-code)))
    ;; (sci-wolfram-eval state buffer-type file tmpfile)
    (let ((result
	   (string-trim-right
	    (shell-command-to-string
	     (format "wolframscript -script %s" tmpfile))))
	  (tmporgfile (concat (file-name-sans-extension tmpfile) ".org")))
      (with-temp-file tmporgfile
	(insert (concat result "\n")))
      (let ((pretty-buffer (find-file-noselect tmporgfile t)))
	(display-buffer pretty-buffer)
	(with-current-buffer pretty-buffer
	  (rename-buffer (concat "*" (file-name-base tmporgfile) "*"))
	  (revert-buffer nil t nil)
	  (goto-char (point-min))
	  (org-mode)
	  (org-latex-preview)
	  (org-display-inline-images)
	  (goto-char (point-max))))
      )))

(sci-wolfram-region-or-buffer-function
 "eval"
 "-pretty"
 (sci-wolfram-eval-pretty state buffer-type file tmpfile)
 "Use `wolframscript' to eval wolfram region or buffer code and pretty print all expressions.")

(defun wolfram-run-script ()
  "Execute and update file"
  (interactive)
  (save-buffer)
  (let ((cur-name (file-name-base (buffer-file-name)))
	(cur-file (file-name-nondirectory (buffer-file-name)))
	(cur-dir  (file-name-directory (buffer-file-name)))
	(output-buffer (get-buffer-create "*MathematicaOutput*"))
	(pretty-buffer)
	(pretty-file))

    ;; Prepare output buffer
    (with-current-buffer output-buffer
      (delete-region (point-min) (point-max)))

    ;; Ensure that package EPrint.m exists
    (wolfram-run-check-or-make-eprint-package cur-dir)

    ;; Call Mathematica
    (call-process-shell-command (concat "cd "
					cur-dir
					"; wolframscript -script "
					cur-file)
				nil output-buffer)

    ;; Open buffer for pretty printing
    (setq pretty-file (concat cur-dir "pretty" cur-name ".org"))
    (setq pretty-buffer (find-file-noselect pretty-file t))
    (display-buffer pretty-buffer)

    ;;(my-run-command-other-window "MathKernel -script test.m")
    (with-current-buffer pretty-buffer
      (rename-buffer (concat "*MathematicaPrettyPrint_" cur-name "*"))
      (revert-buffer nil t nil)
      (goto-char (point-min))
      (org-remove-latex-fragment-image-overlays)
      (org-toggle-latex-fragment)
      (goto-char (point-max)))))


;; convert region or buffer to pdf and Mathematica notebook
;; use `wolframplayer' to view notebook if `sci-wolfram-play' = t
(defcustom sci-wolfram-play nil
  "Play Mathematica notebook."
  :type 'boolean
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-player
  (string-trim-right
   (shell-command-to-string
    "wolframscript -code 'FileNameJoin[$InstallationDirectory,\"Executables\",\"wolframplayer\"] <> Switch[$OperatingSystem,\"Unix\",\"\",\"Windows\",\".exe\"]'"))
  "Path to Wolfram Player."
  :type 'string
  :group 'sci-wolfram-mode)

(setq sci-wolfram-pdf-script (expand-file-name
			      "sci-wolfram-pdf.wl"
			      (file-name-directory (or load-file-name buffer-file-name))))

(defun sci-wolfram-to-pdf-and-notebook (state buffer-type file tmpfile dir)
  "Execute the current file with WolframScript and convert it to PDF."
  (let* ((outbuf (get-buffer-create "*sci-wolfram-eval output*"))
	 (file-no-dir (file-name-nondirectory file)) 
	 (file-name (file-name-base file))
	 (pdf (concat file-name "-convert.pdf"))
	 (notebook (concat file-name "-convert.nb"))
	 (tmpfile-no-dir (file-name-nondirectory tmpfile))
	 (tmpfile-name (file-name-base tmpfile))
	 (tmppdf (concat tmpfile-name "-convert.pdf"))
	 (tmpnotebook (concat tmpfile-name "-convert.nb"))
         (command (format "wolframscript -script %s %s" sci-wolfram-pdf-script tmpfile))
         (command-wolframplayer (format "cd %s;  %s %s" dir sci-wolfram-player notebook)))
    (with-current-buffer outbuf
      (erase-buffer)
      (if (string= state "region")
	  (insert (format "Convert region of %s to %s.pdf and %s.nb in %s folder"
			  file-no-dir file-name file-name dir))
	(insert (format "Convert %s to %s.pdf and %s.nb in %s folder"
			file-no-dir file-name file-name dir)))
      (insert (format "\n\nRunning:\n\n%s\n\n" command)))
    (let ((proc (start-process-shell-command
		 "sci-wolfram-buffer-to-pdf" outbuf
		 command)))
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
				  (insert (format "Delete %s" tmpfile))
				  (if (string= buffer-type "not-file")
				      (insert (format "\n\nDelete %s" file)))
				  (if sci-wolfram-play
				      (insert (format "\n\nUse wolframplayer to view %s" notebook))))

				(if sci-wolfram-play
				    (call-process-shell-command command-wolframplayer))
				
				(let ((right-window (or (window-in-direction 'right) (split-window-right))))
				  (select-window right-window)
				  (find-file (concat dir pdf)))))))
    (display-buffer outbuf)))

(sci-wolfram-region-or-buffer-function
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
(defcustom sci-wolfram-kernel
  (string-trim-right (shell-command-to-string "wolframscript -code 'First[$CommandLine]'"))
  "Path to WolframKernel executable."
  :type 'string
  :group 'sci-wolfram-mode)

(defun sci-wolfram-setup-lsp-server ()
  "Set up wolfram lsp server by removing local version if needed."
  (let ((lsp-server-path
	 (string-trim-right
	  (shell-command-to-string
	   "wolframscript -code 'LSPServerPath = PacletObject[\"LSPServer\"][\"Location\"]; If[StringContainsQ[LSPServerPath, $UserBasePacletsDirectory], LSPServerPath, \"Built-in LSPServer\"]'"))))
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
(defface sci-wolfram-var-name
  '((t :foreground "#2e8b57" :weight bold ))
  "face for user variables."
  :group 'sci-wolfram-mode)

(face-spec-set
 'sci-wolfram-var-name
 '((t :foreground "#2e8b57" :weight bold)))

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
   ("\\b[a-z]+[0-9]*_+" . 'sci-wolfram-var-name)
   ("#[0-9]" . 'sci-wolfram-var-name)
   ("#+" . 'sci-wolfram-var-name)
   ("\\b[a-z][A-Za-z0-9]*" . font-lock-variable-name-face)
   ("\\b[A-Z][A-Za-z0-9]*" . font-lock-warning-face)
   ))

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
