;;; Emacs wolfram-terminal-image setup 
;; Author: Peng Peng - 2025
;; Email: 211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Wolfram-terminal-image

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; 1. Install emacs-jupyter
;; https://github.com/emacs-jupyter/jupyter
(package-install 'jupyter)
(require 'jupyter)

;; 2. Install wolfram-mode for syntax highlighting, code completing via `TAB`, and code formatting
;; https://github.com/xahlee/xah-wolfram-mode
;; For emacs 29+, you can use `package-vc-install` to install packages directly from github;
;; otherwise you should manually download the package and add it to `load-path` by using
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/xah-wolfram-mode")
(package-vc-install "https://github.com/xahlee/xah-wolfram-mode.git")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; 3. Add jupyter-Wolfram-Language to org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (jupyter . t)))

(setq org-babel-default-header-args:jupyter-Wolfram-Language '((:async . "yes")
                                                               (:kernel . "wolframlanguage14.1")
                                                               (:session . "jupyter-wolfram-language")
                                                               (:results . "value drawer")
                                                               (:display . "text")
                                                               (:comments . "link")
                                                               (:eval . "never-export")))

;; 4. Clean jupyter-wolfram-language results
(setq org-babel-min-lines-for-block-output 1000)

(defun clean-jupyter-wolfram-language-results ()
  "Clean up jupyter-Wolfram-Language results."
  (interactive)
  (when (org-in-src-block-p)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (string= lang "jupyter-Wolfram-Language")
	(let ((result-start (org-babel-where-is-src-block-result)))
	  (save-excursion
	    (when (and result-start
		       (goto-char result-start))
	      (let ((start (re-search-forward "^:results:" nil t))
		    (end   (re-search-forward "^:end:" nil t)))
		(save-restriction
		  (narrow-to-region start end)
		  ;; Remove ': ' at beginning
		  (goto-char (point-min))
		  (while (re-search-forward "^: " nil t)
		    (replace-match "" nil nil))

		  ;; Remove blank lines
		  (goto-char (point-min))
		  (while (re-search-forward "\n\\s-*\n" nil t)
		    (replace-match "\n" nil nil))
		  
		  ;; Remove '>' at beginning
		  (goto-char (point-min))
		  (while (re-search-forward "^> " nil t)
		    (replace-match " " nil nil))

		  ;; Remove '\' at end
		  (goto-char (point-min))
		  (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
		    (replace-match "\\1" nil nil))

		  ;; Change 'Out[]' to ': Out[]'
		  (goto-char (point-min))
		  (while (re-search-forward "^Out" nil t)
		    (replace-match ": Out" nil nil)))))))))))

;; Display org-babel images
(defun org-babel-display-images ()
  "Display inline images after executing org block."
  (when (org-babel-where-is-src-block-result)
    (clean-jupyter-wolfram-language-results)
    (org-display-inline-images)
    (org-latex-preview)))

(add-hook 'org-babel-after-execute-hook #'org-babel-display-images)