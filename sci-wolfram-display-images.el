;;; sci-wolfram-display-images.el --- Display wolfram script images in emacs org-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'org)
(require 'ob-core)

(defun sci-wolfram-display-images ()
  "Display latex or images"
  (unless (or
	   ;; Ref: Doom Emacs org mode module config
	   ;; ...but not while Emacs is exporting an org buffer (where
	   ;; `org-display-inline-images' can be awfully slow).
	   (bound-and-true-p org-export-current-backend)
	   ;; ...and not while tangling org buffers (which happens in a temp
	   ;; buffer where `buffer-file-name' is nil).
	   (string-match-p "^ \\*temp" (buffer-name)))
    (save-excursion
      (when-let* ((beg (org-babel-where-is-src-block-result))
		  (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
	(save-restriction
          (narrow-to-region (min beg end) (max beg end))
	  (when (string= sci-wolfram-formula-type "latex") ; (executable-find "pdflatex")
	    (org-latex-preview))
	  (org-display-inline-images))))))


(provide 'sci-wolfram-display-images)
;;; sci-wolfram-display-images.el ends here
