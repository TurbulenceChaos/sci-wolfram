;;; sci-wolfram-lsp-symbols.el --- Wolfram LSPServer symbols for completion -*- lexical-binding: t -*-

;;; Commentary:
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
(require 'SystemLongNames)
(require 'SpecialLongNames)
(require 'UndocumentedLongNames)
(require 'FreeLongNames)
(require 'SystemCharacters)
(require 'SpecialCharacters)
(require 'UndocumentedCharacters)
(require 'FreeCharacters)
(require 'Options)
(require 'SessionSymbols)
(require 'ExperimentalSymbols)
(require 'UndocumentedSymbols)
(require 'ObsoleteSymbols)
(require 'BadSymbols)
(require 'UnsupportedCharacters)
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
