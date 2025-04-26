;;; Emacs wolfram-terminal-image setup 
;; Author: Peng Peng - 2025
;; Email: 211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Wolfram-terminal-image

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Add wolfram language to jupyter
;; https://github.com/WolframResearch/WolframLanguageForJupyter

;; Install emacs-jupyter
;; https://github.com/emacs-jupyter/jupyter
(package-install 'jupyter)

;; Install wolfram-mode
;; https://github.com/xahlee/xah-wolfram-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/xah-wolfram-mode")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; Add jupyter-Wolfram-Language to org-babel
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

;; Clean jupyter-wolfram-language results
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


(provide 'wolfram-terminal-image)