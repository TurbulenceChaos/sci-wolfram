;;; sci-wolfram-jupyter.el --- Display wolfram script graphics in emacs org-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;; Created: 2021-05-20
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages processes tools 
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram-jupyter

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
;; Display wolfram script graphics in emacs org-mode.
;;
;; Installation:
;;
;; Please check README.md.

;; See https://github.com/TurbulenceChaos/sci-wolfram-jupyter for more information.

;;; Code:

(require 'org)
(require 'org-element)
(require 'jupyter)
(require 'ob-jupyter)
(require 'jupyter-org-client)
(require 'sci-wolfram)

(defalias 'wolfram-language-mode 'sci-wolfram-mode)
;; diaply image and latex
(defcustom sci-wolfram-jupyter-formula-type "image"
  "Jupyter formula type. Options are \"image\" and \"latex\"."
  :type '(choice (const "image") (const "latex"))
  :group 'sci-wolfram-mode)

(cond
 ((string= sci-wolfram-jupyter-formula-type "latex")
  (setq org-babel-min-lines-for-block-output 100))
 ((string= sci-wolfram-jupyter-formula-type "image")
  (setq org-babel-min-lines-for-block-output 20)))

(defun sci-wolfram-jupyter-clean-results ()
  "Clean jupyter-Wolfram-Language results."
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

	    (when (string= sci-wolfram-jupyter-formula-type "latex")
	      (goto-char (point-min))
	      (let ((latex-beg 0) (latex-end 0))
		(while (setq latex-beg (re-search-forward "^\\\\begin{equation\\*}" nil t))
		  (setq latex-end (re-search-forward "^\\\\end{equation\\*}" nil t))
		  (save-restriction
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
		      (replace-match "\\1" nil nil))))))))))))

(defmacro sci-wolfram-jupyter-display-marco (name body doc)
  "Use marco to define a function to display latex and image after executing jupyter-Wolfram-Language block."
  `(defun ,(intern (format "sci-wolfram-jupyter-display-%s" name)) ()
     ,doc
     (unless (or
              ;; ...but not while emacs is exporting an org buffer
              (bound-and-true-p org-export-current-backend)
              ;; ...and not while tangling org buffers
              (string-match-p "^ \\*temp" (buffer-name)))
       (save-excursion
         (let* ((beg (org-babel-where-is-src-block-result))
                (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
           (save-restriction
             (narrow-to-region (min beg end) (max beg end))
             ,body))))))

(sci-wolfram-jupyter-display-marco
 "latex"
 (org-latex-preview)
 "Display latex after executing jupyter-Wolfram-Language block.")

(sci-wolfram-jupyter-display-marco
 "images"
 (org-display-inline-images)
 "Display image after executing jupyter-Wolfram-Language block.")

;;;###autoload
(defalias 'wolfram-language-mode 'sci-wolfram-mode)

;;;###autoload
(defun sci-wolfram-jupyter-display ()
  "Display latex and image after executing jupyter-Wolfram-Language block."
  (let ((lang (org-element-property :language (org-element-at-point))))
    (when (string= lang "jupyter-Wolfram-Language")
      (when (org-babel-where-is-src-block-result)
	(sci-wolfram-jupyter-clean-results)
	(when (string= sci-wolfram-jupyter-formula-type "latex")
	  (sci-wolfram-jupyter-display-latex))))
    (sci-wolfram-jupyter-display-images)))

;;;###autoload
(add-hook 'org-babel-after-execute-hook #'sci-wolfram-jupyter-display)

;; completion
(jupyter-org-define-key (kbd "<f6> SPC") #'sci-wolfram-complete-symbol 'Wolfram-Language)
(jupyter-org-define-key (kbd "<f6> h") #'sci-wolfram-doc-lookup 'Wolfram-Language)

(defun sci-wolfram-jupyter-completion-at-point ()
  (jupyter-org-with-src-block-client
   (when (string= (org-element-property :language (org-element-at-point)) "jupyter-Wolfram-Language")
     (sci-wolfram-completion-at-point))))

;;;###autoload
(defun sci-wolfram-jupyter-setup-completion ()
  (add-hook 'completion-at-point-functions
            #'sci-wolfram-jupyter-completion-at-point nil t))

;;;###autoload
(add-hook 'jupyter-org-interaction-mode-hook #'sci-wolfram-jupyter-setup-completion)


(provide 'sci-wolfram-jupyter)

;;; sci-wolfram-jupyter.el ends here
