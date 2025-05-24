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
;; - [ ] Codeformatter (when eglot=nil, reference: sci-wolfram-eval-region, format-all-region);
;; - [ ] sci-wolfram-jupyter insert package (reference: jupyter-insert);
;; - [o] completion-at-point (when eglot=nil)
;; - [o] emacs-jupyter org block completion-at-point;
;; - [o] emacs-jupyter org-block doc and completion with TAB;
;; - [o] jupyter repl, send line, region, buffer ;
;; - [o] display images in jupyter repl (https://github.com/linux-xhyang/WolframLanguageForJupyter/commit/2a4ed08556a3f87e4b134b48d5b0bc44bc81fb8b?w=1);
;; - [ ] eval line, region, buffer (display images and latex in tmp buffer, reference: EPrint);

(defun sci-wolfram-send-region-to-tmp-file (Begin End &optional Xinsert)
  "Eval code in text selection with WolframScript."
  (interactive
   (append
    (if (region-active-p)
	(list (region-beginning) (region-end))
      (list (point-min) (point-max)))
    current-prefix-arg nil))
  (when (not buffer-file-name)
    (user-error "Buffer is not a file. Save it first!"))
  (let* ((state
	  (if (region-active-p)
	      "region"
	    "buffer"))
	 (code (buffer-substring-no-properties Begin End))
	 (file buffer-file-name)
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
    (with-temp-file tmpfile (insert code))
    ;; (sci-wolfram-eval state file tmpfile)
    (sci-wolfram-to-pdf state file tmpfile dir)))

(defun sci-wolfram-eval (state file tmpfile)
  "Execute the current file with WolframScript and print all expressions."
  (interactive)
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
				(delete-file tmpfile)
				(with-current-buffer outbuf
				  (insert (format "\nDelete %s" tmpfile)))))))
    (display-buffer outbuf)))

;; wolfram player
(defvar sci-wolfram-pdf-script nil "Path to sci-wolfram-pdf.wl")
(defvar sci-wolfram-play nil "Play wolfram notebook")
(defvar sci-wolfram-player nil "Path to Wolfram Player")

(setq sci-wolfram-pdf-script (concat (file-name-directory (or load-file-name buffer-file-name)) "sci-wolfram-pdf.wl"))

(defun sci-wolfram-to-pdf (state file tmpfile dir)
  "Execute the current file with WolframScript and convert it to PDF."
  (interactive)
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
				(delete-file tmpfile)
				
				(with-current-buffer outbuf
				  (insert (format "Rename %s to %s\n\n" tmppdf pdf))
				  (insert (format "Rename %s to %s\n\n" tmpnotebook notebook))
				  (insert (format "Delete %s" tmpfile))
				  (if (and sci-wolfram-play sci-wolfram-player)
				      (insert (format "\n\nUse wolframplayer to view %s" notebook))))

				(if (and sci-wolfram-play sci-wolfram-player)
				    (call-process-shell-command command-wolframplayer))
				
				(let ((right-window (or (window-in-direction 'right) (split-window-right))))
				  (select-window right-window)
				  (find-file (concat dir pdf)))))))
    (display-buffer outbuf)))

;; format
(require 'format-all)
(require 'language-id)

(with-eval-after-load 'language-id
  (setq language-id--definitions
        (append language-id--definitions
                '(("Mathematica" sci-wolfram-mode)))))

(with-eval-after-load 'format-all
  (define-format-all-formatter wolframscript
    (:executable "wolframscript")
    (:install "wolframscript")
    (:languages "Mathematica")
    ;; (:features region)
    (:features)
    (:format (format-all--buffer-easy executable "-script" "~/.emacs.d/sci-wolfram-format.wl"))))

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

(defun sci-wolfram-mode-setup-completion ()
  "Setup completion and lsp integration for sci-wolfram-mode."
  (if (or (bound-and-true-p eglot--managed-mode)
          (and (fboundp 'lsp-workspaces)
	       (lsp-workspaces)))
      (remove-hook 'completion-at-point-functions
                   #'sci-wolfram-completion-at-point t)
    (add-hook 'completion-at-point-functions
	      #'sci-wolfram-completion-at-point nil t)))

(add-hook 'sci-wolfram-mode-hook
	  (lambda ()
	    (add-hook 'eglot-managed-mode-hook #'sci-wolfram-mode-setup-completion nil t)
	    (add-hook 'lsp-mode-hook #'sci-wolfram-mode-setup-completion nil t)
	    (sci-wolfram-mode-setup-completion)))

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
	   "wolframscript -code 'LSPServerPath = PacletObject[\"LSPServer\"][\"Location\"]; If[StringContainsQ[LSPServerPath, $UserBasePacletsDirectory], LSPServerPath, \"False\"]'"))))
    (if (not (string= lsp-server-path "False"))
	(if (yes-or-no-p
	     (format "Found locally installed LSPServer in \"%s\".
In order to use the built-in LSPServer, the local version should be removed.
Use PacletUninstall[\"LSPServer\"] to remove it?"
		     lsp-server-path))
	    (if (= 0 (shell-command "wolframscript -code 'PacletUninstall[\"LSPServer\"]'"))
		(message "Local LSPServer successfully removed.")
	      (message "Failed to remove local LSPServer."))
	  (message "Local LSPServer retained. Built-in version may not work properly.")))))

(add-hook 'sci-wolfram-mode-hook #'sci-wolfram-setup-lsp-server)

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
(define-key sci-wolfram-leader-map (kbd "b") #'sci-wolfram-eval-buffer)
(define-key sci-wolfram-leader-map (kbd "p") #'sci-wolfram-buffer-to-pdf)

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
