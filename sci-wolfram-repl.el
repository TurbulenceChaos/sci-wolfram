;;; sci-wolfram-repl.el --- Wolfram Mathematica repl -*- lexical-binding: t -*-

(require 'comint)

(defvar org-babel-wolfram-async--registered nil)

(defun wolfram-repl ()
  (unless (comint-check-proc "*wolfram*")
    (make-comint-in-buffer "wolfram" "*wolfram*" "wolframscript" nil "-rawterm")
    (with-current-buffer "*wolfram*"
      (setq-local comint-prompt-regexp "^In\\[[0-9]+\\]:= *"))
    (setq org-babel-wolfram-async--registered nil)))

(defun run-wolfram ()
  (interactive)
  (wolfram-repl)
  (switch-to-buffer-other-window "*wolfram*"))

;; (defun org-babel-execute:wolfram (body params)
;;   "wolfram org-babel block session execute"
;;   (wolfram-repl)

;;   (let* ((eoe (format "ob_comint_session_wolfram_eoe_%s" (org-id-uuid)))
;; 	 (result
;; 	  (org-babel-comint-with-output
;; 	      ("*wolfram*" eoe) ; for org 9.8 ("*wolfram*" eoe nil nil 'disable-prompt-filtering)
;; 	    (comint-send-string "*wolfram*" (format "%s\n" body))
;; 	    (comint-send-string "*wolfram*" (format "WriteString[\"stdout\", \"\n%s\n\"]\n" eoe)))))
;;     (mapconcat 'identity (cl-remove-if (lambda (s) (string-match-p eoe s)) result))))

(defun org-babel-execute:wolfram (body params)
  "wolfram org-babel block async execute"
  (wolfram-repl)

  (unless org-babel-wolfram-async--registered
    ;; (let* ((eoe (format "ob_comint_session_wolfram_started_%s" (org-id-uuid))))
    ;;   (org-babel-comint-with-output
    ;;       ("*wolfram*" eoe)
    ;;     (comint-send-string "*wolfram*" (format "WriteString[\"stdout\", \"\n%s\n\"]\n" eoe))))

    (if (version<= "9.8" (org-version))
        (org-babel-comint-async-register
         "*wolfram*" (current-buffer)
         "ob_comint_async_wolfram_\\(start\\|end\\|file\\)_\\(.+\\)"
         'org-babel-chomp
         'org-babel-eval-read-file
         'disable-prompt-filtering)
      (org-babel-comint-async-register
       "*wolfram*" (current-buffer)
       "ob_comint_async_wolfram_\\(start\\|end\\|file\\)_\\(.+\\)"
       'org-babel-chomp
       'org-babel-eval-read-file))
    (setq org-babel-wolfram-async--registered t))

  (let* ((uuid (org-id-uuid))
         (start (format "ob_comint_async_wolfram_start_%s" uuid))
         (end   (format "ob_comint_async_wolfram_end_%s" uuid))
         (code (concat
		(format "WriteString[\"stdout\", \"\\n%s\\n\"];\n" start)
		body "\n"
		(format "WriteString[\"stdout\", \"\\n%s\\n\"];\n" end)
		)))
    (comint-send-string "*wolfram*" code)
    uuid))


(provide 'sci-wolfram-repl)
;;; sci-wolfram-repl.el ends here
