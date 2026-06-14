;;; sci-wolfram-jupyter.el --- Run wolfram script code with jupyter -*- lexical-binding: t -*-
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
;; Run wolfram script code with jupyter
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
(require 'jupyter)
(require 'jupyter-org-client)
(require 'sci-wolfram)
(require 'sci-wolfram-display-images)

(defcustom sci-wolfram-jupyter-kernel
  (string-trim-right (shell-command-to-string "jupyter kernelspec list | grep wolfram | awk '{print $1}' | head -1"))
  "wolfram jupyter kernel"
  :type 'string
  :group 'sci-wolfram-mode)

(defcustom org-babel-default-header-args:jupyter-Wolfram-Language
  `((:async . "yes")
    (:kernel . ,sci-wolfram-jupyter-kernel)
    (:session . "jupyter-wolfram-language")
    (:results . "value drawer")
    (:display . "text")
    (:comments . "link")
    (:eval . "never-export")
    (:exports . "both"))
  "Default header arguments for jupyter-Wolfram-Language block."
  :type '(alist :key-type symbol :value-type string)
  :group 'sci-wolfram-mode)

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("Wolfram-Language" . sci-wolfram))
  (add-to-list 'org-src-lang-modes '("jupyter-Wolfram-Language" . sci-wolfram)))

(defvar sci-wolfram-jupyter-repl-buffer "*jupyter-repl[Wolfram-Language 12.0]*")

;; run region or buffer code
(sci-wolfram-run-region-or-buffer-macro
 sci-wolfram-jupyter-run-region-or-buffer
 "Run wolfram script region or buffer code with jupyter"
 "jupyter-Wolfram-Language")

(add-to-list 'sci-wolfram-key-map '(sci-wolfram-jupyter-run-region-or-buffer . "j r") t)

(sci-wolfram-convert-to-notebook-macro
 sci-wolfram-jupyter-convert-to-notebook
 "Convert wolfram script to PDF and Mathematica notebook"
 "jupyter-Wolfram-Language")

(add-to-list 'sci-wolfram-key-map '(sci-wolfram-jupyter-convert-to-notebook . "j n") t)

;; format region or buffer
(defun sci-wolfram-jupyter-format-code ()
  "jupyter format wolfram codes"
  (interactive)
  ;; (call-interactively 'jupyter-run-repl)

  (let* ((eoe (format "comint_wolfram_format_%s" (org-id-uuid)))
	 (code (concat
		(format "Needs[\"CodeFormatter`\"];\nWriteString[\"stdout\", CodeFormat[\"%s\"], \"\\n\"];\n"
			(sci-wolfram-region-or-buffer-code))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))
	 (result
	  (org-babel-comint-with-output
	      (sci-wolfram-jupyter-repl-buffer eoe)
	    (comint-send-string sci-wolfram-jupyter-repl-buffer code))))
    (save-excursion
      (if (region-active-p)
	  (delete-region (region-beginning) (region-end))
	(erase-buffer))
      (insert (sci-wolfram-remove-eoe result eoe)))))

;; jupyter-Wolfram-Language block tools
(dolist (key-map sci-wolfram-key-map)
  (jupyter-org-define-key
   (kbd (format "%s %s" sci-wolfram-mode-leader-key (cdr sci-wolfram-key-map)))
   (car sci-wolfram-key-map)
   'Wolfram-Language))

;; auto-completion
(defun sci-wolfram-jupyter-completion-at-point ()
  (jupyter-org-with-src-block-client
   (when (string= (org-element-property :language (org-element-at-point))
		  "jupyter-Wolfram-Language")
     (sci-wolfram-completion-at-point))))

(defun sci-wolfram-jupyter-add-completion ()
  (add-hook 'completion-at-point-functions
	    #'sci-wolfram-jupyter-completion-at-point nil t))

;; (add-hook 'jupyter-org-interaction-mode-hook #'sci-wolfram-jupyter-add-completion)

(defun sci-wolfram-jupyter-clean-results ()
  "Clean jupyter-Wolfram-Language results."
  (save-excursion
    (when-let* ((beg (org-babel-where-is-src-block-result))
		(end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
      (save-restriction
	(narrow-to-region (min beg end) (max beg end))
	(goto-char (point-min))
	(replace-regexp "^: " "")))))

(defun sci-wolfram-jupyter-display-images ()
  "Auto display latex or images after executing jupyter-Wolfram-Language block."
  (let* ((info (org-babel-get-src-block-info))
	 (lang (nth 0 info)))
    (when (string= lang "jupyter-Wolfram-Language")
      (sci-wolfram-jupyter-clean-results)
      (sci-wolfram-display-images))))

(add-hook 'org-babel-after-execute-hook 'sci-wolfram-jupyter-display-images)


(provide 'sci-wolfram-jupyter)
;;; sci-wolfram-jupyter.el ends here
