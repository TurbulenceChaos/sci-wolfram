(* Convert wolfram LSPServer symbols to emacs symbols *)

dir = Which[
    SameQ[$InputFileName, ""],
        Quiet @ Check[NotebookDirectory[], Directory[]]
    ,
    StringContainsQ[$InputFileName, "WolframLanguageForJupyter"],
        Directory[]
    ,
    True,
        DirectoryName[$InputFileName]
];

sciWolframLSPServer = PacletFind["LSPServer"][[1]];

sciWolfram2Emacs[wolframFileName_, split_] := Module[
    {wolframFile, wolframSymbols, wolframSymbolsSplit, emacsSymbolsFormat, emacsSymbols, emacsFile}
    ,
    wolframFile = FileNameJoin[{sciWolframLSPServer["Location"], "Resources", "Data", wolframFileName <> ".wl"}];
    wolframSymbols = Import[wolframFile];
    wolframSymbolsSplit = Partition[wolframSymbols, UpTo @ Ceiling[Length @ wolframSymbols / split]];
    Do[
        emacsFileName = StringTemplate["sci-wolfram-lsp-symbols-`1`"][ToLowerCase @ StringRiffle[StringCases[wolframFileName, RegularExpression["[A-Z][a-z]*"]], "-"]];
        If[split > 1,
            emacsFileName = StringTemplate["`1`-`2`"][emacsFileName, i]
        ];
        emacsSymbolsFormat = StringRiffle[wolframSymbolsSplit[[i]], {"\"", "\"\n\"", "\""}];
        emacsSymbols = StringTemplate[";;; `1`.el --- Wolfram LSPServer symbols -*- lexical-binding: t -*-\n
;;; Commentary:\n
;; AUTO GENERATED FILE\n
;; GENERATED WITH: `3` `4`\n
;; LSPServer `5`\n
;;; Code:\n
(defvar `1` '(
`2`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"][emacsFileName, emacsSymbolsFormat, "ProductIDName" /. $ProductInformation, $Version, sciWolframLSPServer["Version"]];
        emacsFile = FileNameJoin[{dir, "LSPSymbols", emacsFileName <> ".el"}];
        Export[emacsFile, emacsSymbols, "Text"];
        WriteString["stdout", StringTemplate["Convert `1` -> `2`"][wolframFile, emacsFile], "\n\n"];
        ,
        {i, split}
    ];
]

sciWolfram2Emacs["BuiltinFunctions", 5];

sciWolfram2Emacs["Constants", 1];

sciWolfram2Emacs["Options", 1];

sciWolfram2Emacs["SessionSymbols", 1];

sciWolfram2Emacs["ExperimentalSymbols", 1];

sciWolfram2Emacs["UndocumentedSymbols", 1];

sciWolfram2Emacs["ObsoleteSymbols", 1];

sciWolfram2Emacs["BadSymbols", 1];

sciWolfram2Emacs["SystemLongNames", 1];

sciWolfram2Emacs["FreeLongNames", 1];

sciWolfram2Emacs["SpecialLongNames", 1];

sciWolfram2Emacs["UndocumentedLongNames", 1];

sciWolfram2Emacs["UnsupportedLongNames", 1];
