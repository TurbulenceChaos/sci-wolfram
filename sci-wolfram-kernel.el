;;; sci-wolfram-kernel.el --- Find wolfram kernel location -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(defvar sci-wolfram-kernel-location
  (expand-file-name "sci-wolfram-kernel-location.txt" (file-name-directory (or load-file-name buffer-file-name))))

(unless (file-exists-p sci-wolfram-kernel-location)
  (with-temp-file sci-wolfram-kernel-location
    (insert (string-trim-right (shell-command-to-string "wolframscript -code 'First[$CommandLine]'")))))

(defcustom sci-wolfram-kernel
  (with-temp-buffer
    (insert-file-contents sci-wolfram-kernel-location)
    (buffer-string))
  "Wolfram kernel location used for eglot or lsp-mode."
  :type 'string
  :group 'sci-wolfram-mode)


(provide 'sci-wolfram-kernel)
;;; sci-wolfram-kernel.el ends here
