;;; sci-wolfram.el --- Major mode for editing Wolfram Language. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;;
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Created: 2021-05-20
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
;; - [ ] Codeformatter (when eglot=nil, reference: xah-wolfram-eval-region, format-all-region);
;; - [ ] sci-wolfram-jupyter insert package (reference: jupyter-insert);
;; - [ ] completion-at-point (when eglot=nil)
;; - [o] emacs-jupyter org block completion-at-point;
;; - [o] emacs-jupyter org-block doc and completion with TAB;
;; - [ ] wolfram repl, send line, region, buffer;
;; - [ ] display images and latex in wolfram repl (reference: imaxima);
;; - [ ] eval line, region, buffer (display images and latex in tmp buffer, reference: EPrint);

(defvar sci-wolfram-tmp-dir-path nil "Path to tmp dir used by sci-wolfram-mode commands.")

(setq sci-wolfram-tmp-dir-path
      (let ((xpath "tmp/wolfram/"))
        (if (file-exists-p xpath)
            xpath
          (progn (make-directory xpath :parents) xpath))))

(defun sci-wolfram-eval-buffer ()
  "Execute the current file with WolframScript and print all expressions."
  (interactive)
  (when (or (not buffer-file-name)
            (not (eq major-mode 'sci-wolfram-mode)))
    (user-error "Buffer is not a file. Save it first"))
  (when (buffer-modified-p) (save-buffer))
  (let ((xoutbuf (get-buffer-create "*sci-wolfram-eval output*" t)))
    (save-current-buffer (set-buffer xoutbuf) (erase-buffer))
    (start-process "sci-wolfram-eval" xoutbuf "wolframscript" "-print" "all" "-file" buffer-file-name)
    (display-buffer xoutbuf)))

;; wolfram player
(defvar sci-wolfram-pdf-script nil "Path to sci-wolfram-pdf.wl")
(defvar sci-wolfram-play nil "Play wolfram notebook")
(defvar sci-wolfram-player nil "Path to Wolfram Player")

(setq sci-wolfram-pdf-script (concat (file-name-directory load-file-name) "sci-wolfram-pdf.wl"))

(defun sci-wolfram-buffer-to-pdf ()
  "Execute the current file with WolframScript and convert it to PDF."
  (interactive)
  (when (or (not buffer-file-name)
            (not (eq major-mode 'sci-wolfram-mode)))
    (user-error "Buffer is not a file. Save it first"))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((xoutbuf (get-buffer-create "*sci-wolfram-eval output*"))
	 (file buffer-file-name)
	 (dir (file-name-directory file))
	 (file-no-dir (file-name-nondirectory file)) 
	 (file-name (file-name-base file)) 
	 (notebook-file (concat file-name ".nb"))
         (command (format "wolframscript -script %s %s" sci-wolfram-pdf-script file)))
    (with-current-buffer xoutbuf
      (erase-buffer)
      (insert (format "Convert %s to %s.pdf and %s.nb in %s.\n\n"
		      file-no-dir file-name file-name dir))
      (insert (format "Running: %s\n" command)))
    (if (and sci-wolfram-play sci-wolfram-player)
	(start-process-shell-command
	 "sci-wolfram-buffer-to-pdf" xoutbuf
	 (format "%s && cd %s && %s %s" command dir sci-wolfram-player notebook-file))
      (start-process-shell-command "sci-wolfram-buffer-to-pdf" xoutbuf command))
    (display-buffer xoutbuf)))

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

(add-hook 'sci-wolfram-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      #'sci-wolfram-completion-at-point nil t)))

;; jupyter
(defun sci-wolfram-jupyter-setup ()
  (when (string= (org-element-property :language (org-element-at-point)) "jupyter-Wolfram-Language")
    ;; completion
    (add-hook 'completion-at-point-functions
              #'sci-wolfram-completion-at-point nil t)
    ;; set F6 to use sci-wolfram-leader-map
    (let ((map (current-local-map)))
      (when map
        (define-key map (kbd "<f6>") wolfram-one-leader-map)))))

(add-hook 'jupyter-org-interaction-mode-hook
          (lambda ()
	    (add-hook 'post-command-hook
		      #'sci-wolfram-jupyter-setup nil t)))

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
 `((,(regexp-opt sci-wolfram-funs1 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-funs2 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-funs2-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-funs3 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-funs3-5 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-funs4 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-usr-funs 'symbols) . font-lock-function-name-face)
   (,(regexp-opt sci-wolfram-dollar-names 'symbols) . font-lock-builtin-face)
   (,(regexp-opt sci-wolfram-special-char 'symbols) . font-lock-constant-face)
   ("\\b[A-Z][A-Za-z0-9]*" . font-lock-warning-face)))

;; keybinding
(defvar sci-wolfram-mode-map nil "Keybinding for `sci-wolfram-mode'")
(setq sci-wolfram-mode-map (make-sparse-keymap))
(define-prefix-command 'sci-wolfram-leader-map)

(define-key sci-wolfram-mode-map (kbd (if (boundp 'sci-wolfram-major-leader-key) sci-wolfram-major-leader-key "<f6>")) sci-wolfram-leader-map)
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
(defalias 'wolfram-language-mode 'sci-wolfram-mode)


(provide 'sci-wolfram)

;;; sci-wolfram.el ends here
