(* Export wolfram kernel location to elisp *)

dir =
	Which[
		SameQ[$InputFileName, ""],
			Quiet @ Check[NotebookDirectory[], Directory[]]
		,
		StringContainsQ[$InputFileName, "WolframLanguageForJupyter"],
			Directory[]
		,
		True,
			DirectoryName[$InputFileName]
	];

sciWolframKernel = First[$CommandLine];

elisp = StringTemplate[
";;; sci-wolfram-kernel-location.el --- Wolfram kernel location -*- lexical-binding: t -*-\n
;;; Commentary:
;;; Code:\n
(defcustom sci-wolfram-kernel
  \"`1`\"
  \"Wolfram kernel location\"
  :type 'string
  :group 'sci-wolfram-mode)\n\n
(provide 'sci-wolfram-kernel-location)
;;; sci-wolfram-kernel-location.el ends here\n"
][sciWolframKernel];

Export[FileNameJoin[{dir, "sci-wolfram-kernel-location.el"}], elisp, "Text"];

