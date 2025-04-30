;;; wolfram-terminal-image.el --- Display wolfram script graphics in emacs org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Peng Peng
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Package-Version: 0.0.8
;; Package-Requires: ((emacs "29+"))
;; Keywords: languages processes tools 
;; Homepage: https://github.com/TurbulenceChaos/Wolfram-terminal-image

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display wolfram script graphics in emacs org-mode.

;;; Installation:

;; Please check README
;; See https://github.com/TurbulenceChaos/Wolfram-terminal-image for more information.

;;; Code:

(require 'org)
(require 'org-element)

(defgroup wolfram-terminal-image nil
  "Configure wolfram-terminal-image."
  :group 'org)

(defcustom wolfram-terminal-formula-type=latex t
  "A boolean option.  When set to t, wolfram-terminal-formula-type='latex';
when set to nil, wolfram-terminal-formula-type='image'."
  :type 'boolean
  :group 'wolfram-terminal-image)

;; minimum number of lines for block output
(if wolfram-terminal-formula-type=latex
    (setq org-babel-min-lines-for-block-output 100)
  (setq org-babel-min-lines-for-block-output 20))

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

;; Display inline images and latex fragments in org-babel result
(defmacro +org-define-babel-result-display-fn (name action doc)
  "Define a function to display elements in org-babel result.
NAME is the function name suffix.
ACTION is the display function to call.
DOC is the docstring."
  `(defun ,(intern (format "+org-redisplay-%s-in-babel-result-h" name)) ()
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
             ,action))))))

(+org-define-babel-result-display-fn
 "latex-fragments"
 (org-latex-preview)
 "Redisplay latex fragments after executing org-babel.")

(+org-define-babel-result-display-fn
 "inline-images"
 (org-display-inline-images)
 "Redisplay inline images after executing org-babel.")

;;;###autoload
(defun org-display-images-in-babel-result ()
  "Display images after executing org-babel."
  (when (org-babel-where-is-src-block-result)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (string= lang "jupyter-Wolfram-Language")
	(clean-jupyter-wolfram-language-results)
	(when wolfram-terminal-formula-type=latex
	  (+org-redisplay-latex-fragments-in-babel-result-h))))
    (+org-redisplay-inline-images-in-babel-result-h)))

;;;###autoload
(add-hook 'org-babel-after-execute-hook #'org-display-images-in-babel-result)


(provide 'wolfram-terminal-image)

;;; wolfram-terminal-image.el ends here
