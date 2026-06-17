(* Convert Wolfram LSPServer symbols to emacs lisp *)

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


sciWolframLSPServer = FileNameJoin[{ParentDirectory@DirectoryName[FindFile["LSPServer`"]], "Resources", "Data"}];

sciWolframLSPSymbols[fileName_, split_] :=
	Module[{wolframFile, symbols, symbolsSplit, symbolsFormat, elisp, elispFile},
		wolframFile = FileNameJoin[{sciWolframLSPServer, fileName <> ".wl"}];
		symbols = Import[wolframFile];
		symbolsSplit = Partition[symbols, UpTo @ Ceiling[Length @ symbols /split]];
		Do[
			lispVar =
				If[split == 1,
					fileName
					,
					fileName <> "-" <> ToString[i]
				];
			symbolsFormat = StringRiffle[symbolsSplit[[i]], {"\"", "\"\n\"", "\""}];
			elisp = StringTemplate[
";;; `1`.el --- Wolfram LSPServer symbols -*- lexical-binding: t -*-\n
;;; Commentary:\n
;;; Code:\n
(defvar `1` nil)
(setq `1` '(
`2`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"
				][lispVar, symbolsFormat];
			elispFile = FileNameJoin[{dir, "LSPSymbols", lispVar <> ".el"}];
			Export[elispFile, elisp, "Text"];
			WriteString["stdout", StringTemplate["Convert `1` -> `2`"][wolframFile, elispFile], "\n\n"];
			,
			{i, split}
		];
	]

sciWolframLSPSymbols["BuiltinFunctions", 5];

sciWolframLSPSymbols["Constants", 1];

sciWolframLSPSymbols["SystemLongNames", 1];

sciWolframLSPSymbols["SpecialLongNames", 1];

sciWolframLSPSymbols["UndocumentedLongNames", 1];

sciWolframLSPSymbols["FreeLongNames", 1];

sciWolframLSPSymbols["SystemCharacters", 1];

sciWolframLSPSymbols["SpecialCharacters", 1];

sciWolframLSPSymbols["UndocumentedCharacters", 1];

sciWolframLSPSymbols["FreeCharacters", 1];

sciWolframLSPSymbols["Options", 1];

sciWolframLSPSymbols["SessionSymbols", 1];

sciWolframLSPSymbols["ExperimentalSymbols", 1];

sciWolframLSPSymbols["UndocumentedSymbols", 1];

sciWolframLSPSymbols["ObsoleteSymbols", 1];

sciWolframLSPSymbols["BadSymbols", 1];

sciWolframLSPSymbols["UnsupportedCharacters", 1];

sciWolframLSPSymbols["UnsupportedLongNames", 1];

