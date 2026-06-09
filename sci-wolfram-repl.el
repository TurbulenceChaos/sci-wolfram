;;; sci-wolfram-repl.el --- Wolfram Mathematica Repl -*- lexical-binding: t -*-

(require 'comint)

(defun wolfram-repl ()
  (unless (comint-check-proc "*wolfram*")
    (make-comint-in-buffer "wolfram" "*wolfram*" "wolframscript" nil "-rawterm")))

(defun run-wolfram ()
  (interactive)
  (wolfram-repl)
  (switch-to-buffer "*wolfram*"))

(defun org-babel-execute:wolfram (body params)
  (wolfram-repl)

  (with-current-buffer "*wolfram*"
    (setq-local comint-prompt-regexp "^In\\[[0-9]+\\]:= *"))

  (let* ((eoe (format "ORG_BABEL_EOE_%s" (float-time)))
         (result
          (org-babel-comint-with-output
              ("*wolfram*" eoe)
            (comint-send-string "*wolfram*" (format "%s\n" body))
            (comint-send-string "*wolfram*" (format "WriteString[\"stdout\", \"\n%s\n\"]\n" eoe)))))
    (mapconcat 'identity (cl-remove-if (lambda (x) (string-match-p "ORG_BABEL_EOE" x)) result))))


(provide 'sci-wolfram-repl)
;;; sci-wolfram-repl.el ends here
