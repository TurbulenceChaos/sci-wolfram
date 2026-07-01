;;; sci-wolfram-prettify-symbols.el --- Wolfram characters for prettify-symbols-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 PENG

;; Author: PENG <p.peng01@outlook.com>
;; Created: 20250520
;; Version: 20260701
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, processes, tools
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram

;; This file is not part of GNU Emacs

;;; License

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

;; Wolfram characters for prettify-symbols-mode

;; Installation and usage:
;; Please check README.md.

;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

;; https://reference.wolfram.com/language/guide/ListingOfNamedCharacters.html
(defvar sci-wolfram-prettify-symbols-script
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "sciWolframPrettifySymbols.wl"))

(defvar sci-wolfram-prettify-symbols-alist-elisp
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "sci-wolfram-prettify-symbols-alist.el"))

(unless (file-exists-p sci-wolfram-prettify-symbols-alist-elisp)
  (message "Convert Wolfram characters to Emacs prettify symbols")
  (shell-command (format "%s -script %s" sci-wolfram-program sci-wolfram-prettify-symbols-script)))

(require 'sci-wolfram-prettify-symbols-alist)

(defun sci-wolfram-prettify-symbols ()
  "Add wolfram prettify symbols alist"
  (setq-local prettify-symbols-alist sci-wolfram-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate (lambda (start end match) t))
  ;; (setq-local prettify-symbols-unprettify-at-point nil)
  (prettify-symbols-mode 1))

(add-hook 'sci-wolfram-mode-hook #'sci-wolfram-prettify-symbols)


(provide 'sci-wolfram-prettify-symbols)
;;; sci-wolfram-prettify-symbols.el ends here


