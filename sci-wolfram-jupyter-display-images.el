;;; sci-wolfram-jupyter-display-images.el --- Display jupyter-Wolfram-Language images in emacs org-mode -*- lexical-binding: t -*-
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
;; Display jupyter-Wolfram-Language images in emacs org-mode.
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

(defun sci-wolfram-jupyter-clean-results ()
  "Clean jupyter-Wolfram-Language results."
  (save-excursion
    (when-let* ((beg (org-babel-where-is-src-block-result))
		(end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
      (save-restriction
        (narrow-to-region (min beg end) (max beg end))
	(goto-char (point-min))
	(replace-regexp "^: " "")
	;; (replace-regexp "^\s-*$" "")
	;; (replace-regexp "^> " "")
	))))

(defun sci-wolfram-jupyter-display-images ()
  "Auto display latex or images after executing jupyter-Wolfram-Language block."
  (let ((lang (org-element-property :language (org-element-at-point))))
    (when (string= lang "jupyter-Wolfram-Language")
      (sci-wolfram-jupyter-clean-results)
      (sci-wolfram-display-images))))

(add-hook 'org-babel-after-execute-hook 'sci-wolfram-jupyter-display-images)


(provide 'sci-wolfram-jupyter-display-images)
;;; sci-wolfram-jupyter-display-images.el ends here
