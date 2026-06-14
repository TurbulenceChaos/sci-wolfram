;;; sci-wolfram-repl.el --- Wolfram Mathematica repl -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
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
;; Add wolfram repl support
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-src)
(require 'comint)
(require 'sci-wolfram-display-images)

(defvar sci-wolfram-repl-buffer "*Wolfram REPL*")

(defvar sci-wolfram-org-babel--initiated nil)

(defvar sci-wolfram-org-babel-async--registered nil)

(defun sci-wolfram-preoutput-filter-function (input-string)
  (replace-regexp-in-string " *\r" "" input-string))

(defun sci-wolfram-make-repl ()
  (unless (comint-check-proc sci-wolfram-repl-buffer)
    (make-comint-in-buffer "sci-wolfram-repl" sci-wolfram-repl-buffer "wolframscript" nil "-rawterm")
    ;; (make-comint-in-buffer "sci-wolfram-repl" sci-wolfram-repl-buffer "wolframscript")
    (with-current-buffer sci-wolfram-repl-buffer
      (setq-local comint-prompt-regexp "^In\\[[0-9]+\\]:= *")
      ;; (setq-local comint-process-echoes t)
      ;; (add-hook 'comint-preoutput-filter-functions
      ;; 		'sci-wolfram-preoutput-filter-function nil t)
      )
    (setq sci-wolfram-org-babel--initiated nil)
    (setq sci-wolfram-org-babel-async--registered nil)))

;;;###autoload
(defun sci-wolfram-run-repl ()
  "Run wolfram repl"
  (interactive)
  (sci-wolfram-make-repl)
  (switch-to-buffer-other-window sci-wolfram-repl-buffer))

(defun sci-wolfram-initiate-session ()
  (unless sci-wolfram-org-babel--initiated
    (let* ((eoe (format "ob_comint_session_wolfram_started_%s" (org-id-uuid))))
      (org-babel-comint-with-output
          (sci-wolfram-repl-buffer eoe)
        (comint-send-string sci-wolfram-repl-buffer (format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))))
  (setq sci-wolfram-org-babel-initiated t))

(defun sci-wolfram-remove-space-lines (body)
  "Remove code string space lines"
  (substring-no-properties
   (replace-regexp-in-string "\n[ \t\n]+" "\n" body)))

(defun sci-wolfram-remove-eoe (result eoe)
  "Remove EOE from result"
  (mapconcat 'identity (cl-remove-if (lambda (s) (string-match-p eoe s)) result)))

(defun sci-wolfram-evaluate-session (body)
  "wolfram org-babel block execute session"
  (let* ((eoe (format "ob_comint_session_wolfram_eoe_%s" (org-id-uuid)))
	 (code (concat
		(format "%s\n" (sci-wolfram-remove-space-lines body))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))
	 (result
	  (if (version<= "9.8" (org-version))
	      (org-babel-comint-with-output
		  (sci-wolfram-repl-buffer eoe nil nil 'disable-prompt-filtering)
		(comint-send-string sci-wolfram-repl-buffer code))
	    (org-babel-comint-with-output
		(sci-wolfram-repl-buffer eoe)
	      (comint-send-string sci-wolfram-repl-buffer code)))))
    (sci-wolfram-remove-eoe result eoe)))

(defun sci-wolfram-result-clean (result)
  (prog1
      result ; (replace-regexp-in-string "^ \n \n" "" result)
    (run-at-time 0 nil 'sci-wolfram-display-images)))

(defun sci-wolfram-org-babel-register-async ()
  (unless sci-wolfram-org-babel-async--registered
    (if (version<= "9.8" (org-version))
        (org-babel-comint-async-register
         sci-wolfram-repl-buffer (current-buffer)
         "ob_comint_async_wolfram_\\(start\\|end\\|file\\)_\\(.+\\)"
	 'sci-wolfram-result-clean ; 'org-babel-chomp
         'org-babel-eval-read-file
         'disable-prompt-filtering)
      (org-babel-comint-async-register
       sci-wolfram-repl-buffer (current-buffer)
       "ob_comint_async_wolfram_\\(start\\|end\\|file\\)_\\(.+\\)"
       'sci-wolfram-result-clean ; 'org-babel-chomp
       'org-babel-eval-read-file))
    (setq sci-wolfram-org-babel-async--registered t)))

(defun sci-wolfram-async-evaluate-session (body)
  "wolfram org-babel block async execute session"
  (sci-wolfram-org-babel-register-async)

  (let* ((uuid (org-id-uuid))
         (start (format "ob_comint_async_wolfram_start_%s" uuid))
         (end   (format "ob_comint_async_wolfram_end_%s" uuid))
         (code (concat
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" start)
		(format "%s\n" (sci-wolfram-remove-space-lines body))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" end))))
    (comint-send-string sci-wolfram-repl-buffer code)
    uuid))

(defun org-babel-execute:wolfram (body params)
  "wolfram org-babel block sync/async execute session"
  (sci-wolfram-make-repl)
  (sci-wolfram-initiate-session)
  (let ((async (cdr (assq :async params))))
    (if (string-match-p "yes" async)
	(sci-wolfram-async-evaluate-session body)
      (sci-wolfram-evaluate-session body))))

(defcustom org-babel-default-header-args:wolfram
  '((:async . "yes")
    (:results . "value drawer")
    (:display . "text")
    (:comments . "link")
    (:eval . "never-export")
    (:exports . "both"))
  "Default header arguments for wolfram org-babel block."
  :type '(alist :key-type symbol :value-type string)
  :group 'sci-wolfram-mode)

(defvar sci-wolfram-org-src-block-name "wolfram")

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes `(,sci-wolfram-org-src-block-name . sci-wolfram)))

(defun sci-wolfram-auto-display-images ()
  "Auto display latex or images after executing wolfram session block."
  (let* ((info (org-babel-get-src-block-info))
	 (lang (nth 0 info))
	 (params (nth 2 info))
	 (async (cdr (assq :async params))))
    (when (string= lang sci-wolfram-org-src-block-name)
      (unless (string-match-p "yes" async)
	(sci-wolfram-display-images)))))

(add-hook 'org-babel-after-execute-hook 'sci-wolfram-auto-display-images)


(provide 'sci-wolfram-repl)
;;; sci-wolfram-repl.el ends here
