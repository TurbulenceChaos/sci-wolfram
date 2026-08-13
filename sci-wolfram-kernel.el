;;; sci-wolfram-kernel.el --- Find wolfram kernel location -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(defvar sci-wolfram-kernel-location-script
  (expand-file-name "sci-wolfram-kernel-location.el" (file-name-directory (or load-file-name buffer-file-name))))

(unless (file-exists-p sci-wolfram-kernel-location-script)
  (with-temp-file sci-wolfram-kernel-location-script
    (let ((kernel (string-trim-right (shell-command-to-string "wolframscript -code 'First[$CommandLine]'")))
          (n "\n"))
      (insert (concat
               ";;; sci-wolfram-kernel-location.el --- Wolfram kernel location -*- lexical-binding: t -*-"
               n n ";;; Commentary:"
               n n ";; AUTO GENERATED FILE"
               n n ";;; Code:"
               n n "(defcustom sci-wolfram-kernel-location"
               n (format "\"%s\"" kernel)
               n "\"Wolfram kernel location\""
               n ":type 'string"
               n ":group 'sci-wolfram-mode)"
               n n n"(provide 'sci-wolfram-kernel-location)"
               n";;; sci-wolfram-kernel-location.el ends here")))))

(require 'sci-wolfram-kernel-location)


(provide 'sci-wolfram-kernel)
;;; sci-wolfram-kernel.el ends here
