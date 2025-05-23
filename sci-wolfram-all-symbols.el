;;; sci-wolfram-all-symbols.el --- All symbols of Wolfram Language. -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(setq sci-wolfram-all-symbols-script
      (concat (file-name-directory (or load-file-name buffer-file-name))
	      "sci-wolfram-all-symbols.wl"))

(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (data-dir (concat dir "Data")))
  (when (not (file-directory-p data-dir))
    (message "Created wolfram symbols dir.")
    (make-directory data-dir))
  (when (not (directory-files data-dir nil "\\.wl\\'"))
    (message (format "Copy LSPServer wolfram symbols to %s." data-dir))
    (let ((lsp-server-path
	   (string-trim-right
	    (shell-command-to-string "wolframscript -code 'LSPServer = #[\"Location\"]& /@ PacletFind[\"LSPServer\"]; LSPServerBuiltIn = Select[LSPServer, StringContainsQ[#, $InstallationDirectory] &][[1]]; FileNameJoin[{LSPServerBuiltIn, \"Resources\",\"Data\"}]'"))))
      (copy-directory lsp-server-path dir)))
  (if (not (directory-files data-dir nil "\\.el\\'"))
      (shell-command (format "wolframscript -script %s %s" sci-wolfram-all-symbols-script dir))))

(add-to-list 'load-path (expand-file-name "Data" (file-name-directory (or load-file-name buffer-file-name))))
(require 'SystemLongNames)
(require 'BuiltinFunctions)
(require 'Constants)

(defcustom sci-wolfram-usr-functions '()
  "Add Additional Wolfram Language symbols."
  :type '(repeat string)
  :group 'sci-wolfram-mode)

(add-to-list 'sci-wolfram-usr-functions "CodeFormat")

(defvar sci-wolfram-all-symbols nil "List of all Wolfram Language symbols.")

(setq sci-wolfram-all-symbols
      (append
       SystemLongNames
       BuiltinFunctions-1
       BuiltinFunctions-2
       BuiltinFunctions-3
       BuiltinFunctions-4
       BuiltinFunctions-5
       Constants
       sci-wolfram-usr-functions))


(provide 'sci-wolfram-all-symbols)
;;; sci-wolfram-all-symbols.el ends here
