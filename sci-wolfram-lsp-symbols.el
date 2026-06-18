;;; sci-wolfram-lsp-symbols.el --- Wolfram LSPServer symbols for completion -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 Peng Peng
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
;; Add wolfram LSPServer symbols for completion
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(defvar sci-wolfram-lsp-symbols-script
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "sciWolframLSPSymbols.wl"))

(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (lsp-symbols-dir (expand-file-name "LSPSymbols" dir)))
  (unless (file-directory-p lsp-symbols-dir)
    (make-directory lsp-symbols-dir))
  (unless (directory-files lsp-symbols-dir nil "\\.el\\'")
    (message "Convert Wolfram LSPServer symbols to emacs lisp")
    (shell-command (format "wolframscript -script %s" sci-wolfram-lsp-symbols-script))))

(add-to-list 'load-path (expand-file-name "LSPSymbols" (file-name-directory (or load-file-name buffer-file-name))))
(require 'BuiltinFunctions-1)
(require 'BuiltinFunctions-2)
(require 'BuiltinFunctions-3)
(require 'BuiltinFunctions-4)
(require 'BuiltinFunctions-5)
(require 'Constants)
(require 'Options)
(require 'SessionSymbols)
(require 'ExperimentalSymbols)
(require 'UndocumentedLongNames)
(require 'ObsoleteSymbols)
(require 'BadSymbols)
(require 'SystemLongNames)
(require 'FreeLongNames)
(require 'SpecialLongNames)
(require 'UndocumentedSymbols)
(require 'UnsupportedLongNames)

(defvar sci-wolfram-lsp-symbols nil "List of Wolfram Language LSPServer symbols")

(setq sci-wolfram-lsp-symbols
      (append
       BuiltinFunctions-1
       BuiltinFunctions-2
       BuiltinFunctions-3
       BuiltinFunctions-4
       BuiltinFunctions-5
       Constants
       Options
       SessionSymbols
       ExperimentalSymbols
       UndocumentedSymbols
       ObsoleteSymbols
       BadSymbols
       SystemLongNames ; SystemCharacters
       FreeLongNames ; FreeCharacters
       SpecialLongNames ; SpecialCharacters
       UndocumentedLongNames ; UndocumentedCharacters
       UnsupportedLongNames ; UnsupportedCharacters
       ))


(provide 'sci-wolfram-lsp-symbols)
;;; sci-wolfram-lsp-symbols.el ends here
