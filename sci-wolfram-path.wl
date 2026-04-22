sciWolframPlayer = FileNames["*wolframplayer*", $InstallationDirectory, 2][[1]]

sciWolframKernel = First[$CommandLine]

sciWolframPath = TemplateApply[
";;; sci-wolfram-path.el --- Path to wolfram kernel and player -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

(defcustom sci-wolfram-player
  \"`1`\"

  \"sci-wolfram-player

Path to the Wolfram Player, which is set as

(string-trim-right
   (shell-command-to-string
    \\\"wolframscript -code 'FileNames[\\\"*wolframplayer*\\\", $InstallationDirectory, 2][[1]]'\\\"))

You can also specify the path by

for linux:

(custom-set-variables
 '(sci-wolfram-player \\\"/path/to/wolframplayer\\\"))

for windows:
(custom-set-variables
 '(sci-wolfram-player \\\"/path/to/wolframplayer.exe\\\"))
\"
  :type 'string
  :group 'sci-wolfram-mode)

(defcustom sci-wolfram-kernel
  \"`2`\"
  \"Path to WolframKernel executable for wolfram LSPServer usage\"
  :type 'string
  :group 'sci-wolfram-mode)


(provide 'sci-wolfram-path)
;;; sci-wolfram-path.el ends here",
  {sciWolframPlayer, sciWolframKernel}
  ]

Export[FileNameJoin[{DirectoryName[$InputFileName], "sci-wolfram-path.el"}], sciWolframPath, "Text"];
