;;; sci-wolfram-lsp-symbols.el --- Wolfram LSPServer symbols for completion -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 PENG

;; Author: PENG <p.peng01@outlook.com>
;; Created: 20250520
;; Version: 20260812
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

;; Wolfram LSPServer symbols for completion

;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(defvar sci-wolfram-lsp-symbols-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar sci-wolfram-lsp-symbols-script
  (expand-file-name "sciWolframLSPSymbols.wl" sci-wolfram-lsp-symbols-directory))

(defvar sci-wolfram-lsp-symbols-script-directory
  (expand-file-name "LSPSymbols" sci-wolfram-lsp-symbols-directory))

(unless (file-directory-p sci-wolfram-lsp-symbols-script-directory)
  (make-directory sci-wolfram-lsp-symbols-script-directory))

(unless (directory-files sci-wolfram-lsp-symbols-script-directory nil "\\.el\\'")
  (message "Convert wolfram LSPServer symbols to emacs symbols")
  (shell-command (format "wolframscript -script %s" sci-wolfram-lsp-symbols-script)))

(load (expand-file-name "sci-wolfram-lsp-symbols-builtin-functions-1.el"     sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-builtin-functions-2.el"     sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-builtin-functions-3.el"     sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-builtin-functions-4.el"     sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-builtin-functions-5.el"     sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-constants.el"               sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-options.el"                 sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-session-symbols.el"         sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-experimental-symbols.el"    sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-undocumented-symbols.el"    sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-obsolete-symbols.el"        sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-bad-symbols.el"             sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-system-long-names.el"       sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-free-long-names.el"         sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-special-long-names.el"      sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-undocumented-long-names.el" sci-wolfram-lsp-symbols-script-directory) t)
(load (expand-file-name "sci-wolfram-lsp-symbols-unsupported-long-names.el"  sci-wolfram-lsp-symbols-script-directory) t)


(provide 'sci-wolfram-lsp-symbols)
;;; sci-wolfram-lsp-symbols.el ends here
