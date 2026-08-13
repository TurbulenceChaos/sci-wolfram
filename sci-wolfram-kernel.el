;;; sci-wolfram-kernel.el --- Find wolfram kernel location -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(defvar sci-wolfram-kernel-location (concat sci-wolfram-script-directory "sci-wolfram-kernel.txt"))

(unless (file-exists-p sci-wolfram-kernel-location)
  (with-temp-file sci-wolfram-kernel-location
    (insert (string-trim-right (shell-command-to-string "wolframscript -code 'First[$CommandLine]'")))))

(defcustom sci-wolfram-kernel
  (with-temp-buffer
    (insert-file-contents sci-wolfram-kernel-location)
    (buffer-string))
  "Wolfram kernel location"
  :type 'string
  :group 'sci-wolfram-mode)


(provide 'sci-wolfram-kernel)
;;; sci-wolfram-kernel.el ends here
