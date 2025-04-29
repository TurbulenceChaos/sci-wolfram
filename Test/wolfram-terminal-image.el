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

;; 2. Install wolfram-mode for syntax highlight, code format and completion
;; https://github.com/xahlee/xah-wolfram-mode
;; For emacs 29+, you can use `package-vc-install` to install packages directly from github;
;; otherwise you can manually download the package and add it to `load-path`
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

(defcustom wolfram-terminal-formula-type=latex t
  "A boolean option.  When set to t, wolfram-terminal-formula-type='latex';
when set to nil, wolfram-terminal-formula-type='image'."
  :type 'boolean
  :group 'wolfram-terminal-image)

(defun clean-jupyter-wolfram-language-results ()
  "Clean up jupyter-Wolfram-Language results."
  (let ((result-beg (org-babel-where-is-src-block-result)))
    (save-excursion
      (when (and result-beg
		 (goto-char result-beg))
	(let ((beg (re-search-forward "^:results:" nil t))
	      (end   (re-search-forward "^:end:" nil t)))
	  (save-restriction
	    (narrow-to-region beg end)
	    ;; Remove ': ' at beginning
	    (goto-char (point-min))
	    (while (re-search-forward "^: " nil t)
	      (replace-match "" nil nil))

	    ;; Change 'Out[number]' to ': Out[number]'
	    (goto-char (point-min))
	    (while (re-search-forward "^Out\\[\\([0-9]+\\)\\]" nil t)
	      (replace-match ": Out[\\1]" nil nil))

	    (when wolfram-terminal-formula-type=latex
	      (goto-char (point-min))
	      (let ((latex-beg 0) (latex-end 0))
		(while (setq latex-beg (re-search-forward "^\\\\begin{equation\\*}" nil t))
		  (setq latex-end (re-search-forward "^\\\\end{equation\\*}" nil t))
		  (narrow-to-region latex-beg latex-end)
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
		    (replace-match "\\1" nil nil)))))))))))

;; Display inline images and latex fragments in org-babel result
;; https://github.com/doomemacs/doomemacs/blob/303dd28db808b42a2397c0f4b9fdd71e606026ff/modules/lang/org/config.el#L297
(defmacro +org-define-babel-result-display-fn (name action doc)
  "Define a function to display elements in org-babel result.
NAME is the function name suffix.
ACTION is the display function to call.
DOC is the docstring."
  `(defun ,(intern (format "+org-redisplay-%s-in-babel-result-h" name)) ()
     ,doc
     (unless (or
              ;; ...but not while Emacs is exporting an org buffer
              (bound-and-true-p org-export-current-backend)
              ;; ...and not while tangling org buffers
              (string-match-p "^ \\*temp" (buffer-name)))
       (save-excursion
         (let* ((beg (org-babel-where-is-src-block-result))
                (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
           (save-restriction
             (narrow-to-region (min beg end) (max beg end))
             ,action))))))

(+org-define-babel-result-display-fn
 "latex-fragments"
 (org-latex-preview)
 "Redisplay latex fragments after executing org-block.")

(+org-define-babel-result-display-fn
 "inline-images"
 (org-display-inline-images)
 "Redisplay inline images after executing org-block.")

(defun org-display-images-in-babel-result ()
  "Display images after executing org block."
  (when (org-babel-where-is-src-block-result)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (string= lang "jupyter-Wolfram-Language")
	(clean-jupyter-wolfram-language-results)
	(when wolfram-terminal-formula-type=latex
	  (+org-redisplay-latex-fragments-in-babel-result-h))))
    (+org-redisplay-inline-images-in-babel-result-h)))

(add-hook 'org-babel-after-execute-hook #'org-display-images-in-babel-result)
