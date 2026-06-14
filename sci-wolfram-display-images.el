;;; sci-wolfram-display-images.el --- Display wolfram script images in emacs org-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;; Created: 2025-05-20
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages processes tools
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram

;; This file is not part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display wolfram script images in emacs org-mode.
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-src)

(defun sci-wolfram-display-images ()
  "Display latex or images"
  (interactive)
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
	  (when (executable-find "pdflatex") ; (string= sci-wolfram-formula-type "latex")
	    (org-latex-preview))
	  (org-display-inline-images))))))


(provide 'sci-wolfram-display-images)
;;; sci-wolfram-display-images.el ends here
