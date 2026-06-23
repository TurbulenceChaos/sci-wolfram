;;; sci-wolfram-kernel.el --- Find wolfram kernel location -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(defvar sci-wolfram-kernel-location-script
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "sciWolframKernelLocation.wl"))

(defvar sci-wolfram-kernel-location-elisp
  (concat (file-name-directory (or load-file-name buffer-file-name))
	  "sci-wolfram-kernel-location.el"))

(unless (file-exists-p sci-wolfram-kernel-location-elisp)
  (shell-command (format "wolframscript -script %s" sci-wolfram-kernel-location-script)))

(require 'sci-wolfram-kernel-location)


(provide 'sci-wolfram-kernel)
;;; sci-wolfram-lsp-symbols.el ends here

