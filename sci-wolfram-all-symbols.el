;;; sci-wolfram-all-symbols.el --- All symbols of Wolfram Language. -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(setq sci-wolfram-all-symbols-script
      (concat (file-name-directory (or load-file-name buffer-file-name))
	      "sci-wolfram-all-symbols.wl"))

(let* ((dir (file-name-directory (or load-file-name buffer-file-name)))
       (data-dir (concat dir "Data")))
  (when (not (file-directory-p data-dir))
    (message "Creat wolfram symbols dir")
    (make-directory data-dir))
  (when (not (directory-files data-dir nil "\\.wl\\'"))
    (let ((lsp-server-path
	   (string-trim-right
	    (shell-command-to-string
	     (concat
	      "wolframscript -code "
	      "'"
	      "LSPServer = #[\"Location\"]& /@ PacletFind[\"LSPServer\"]; "
	      "LSPServerBuiltIn = Select[LSPServer, StringContainsQ[#, $InstallationDirectory] &][[1]]; "
	      "FileNameJoin[{LSPServerBuiltIn, \"Resources\",\"Data\"}]"
	      "'")))))
      (if (yes-or-no-p
	   (format
	    "Wolfram LSPServer symbols not found in the Data folder!
Do you want to copy wolfram symbols from built-in LSPServer package in %s?
If not, copy them from the Data-example folder."
	    lsp-server-path))
	  (progn
	    (message (format "Copy built-in wolfram LSPServer symbols to %s" data-dir))
	    (copy-directory lsp-server-path dir nil t)
	    (if (not (directory-files data-dir nil "\\.el\\'"))
		(shell-command
		 (format
		  "wolframscript -script %s %s"
		  sci-wolfram-all-symbols-script
		  dir))))
	(copy-directory
	 (expand-file-name "Data-example" dir)
	 (expand-file-name "Data" dir)
	 nil t)))))

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
