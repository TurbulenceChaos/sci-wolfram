;;; sci-wolfram-jupyter.el --- Display wolfram script graphics in emacs org-mode -*- lexical-binding: t -*-
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
(require 'jupyter)
(require 'jupyter-org-client)
(require 'sci-wolfram)

;;;###autoload
(defcustom org-babel-default-header-args:jupyter-Wolfram-Language
  `((:async . "yes")
    (:kernel . ,(string-trim-right
                 (shell-command-to-string
                  "jupyter kernelspec list | grep wolfram | awk '{print $1}' | head -1")))
    (:session . "jupyter-wolfram-language")
    (:results . "value drawer")
    (:display . "text")
    (:comments . "link")
    (:eval . "never-export")
    (:exports . "both"))
  "Default header arguments for `Jupyter-Wolfram-Language' block."
  :type '(alist :key-type symbol :value-type string)
  :group 'sci-wolfram-mode)

;;;###autoload
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("Wolfram-Language" . sci-wolfram))
  (add-to-list 'org-src-lang-modes '("jupyter-Wolfram-Language" . sci-wolfram)))

;; tools
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " c"))
			#'sci-wolfram-complete-symbol 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " h"))
			#'sci-wolfram-doc-lookup 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " i"))
			#'sci-wolfram-import-pkg 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " f"))
			#'sci-wolfram-format-region-or-buffer 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " e"))
			#'sci-wolfram-eval-region-or-buffer 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " j"))
			#'sci-wolfram-jupyter-eval-region-or-buffer 'Wolfram-Language)
(jupyter-org-define-key (kbd (concat sci-wolfram-mode-leader-key " p"))
			#'sci-wolfram-convert-region-or-buffer-to-pdf-and-notebook 'Wolfram-Language)

;; completion
(defun sci-wolfram-jupyter-completion-at-point ()
  (jupyter-org-with-src-block-client
   (when (string= (org-element-property :language (org-element-at-point))
		  "jupyter-Wolfram-Language")
     (sci-wolfram-completion-at-point))))

;;;###autoload
(defun sci-wolfram-jupyter-add-completion ()
  (add-hook 'completion-at-point-functions
            #'sci-wolfram-jupyter-completion-at-point nil t))

;;;###autoload
(add-hook 'jupyter-org-interaction-mode-hook #'sci-wolfram-jupyter-add-completion)


(provide 'sci-wolfram-jupyter)
;;; sci-wolfram-jupyter.el ends here
