(* Convert Wolfram LSPServer symbols to Emacs symbols *)

dir = If[SameQ[$InputFileName, ""],
    Directory[]
    ,
    DirectoryName[$InputFileName]
];

lsp = PacletFind["LSPServer"][[1]];

wolfram2elisp[wolframFileName_, split_] := Module[
    {wolframFile, symbols, emacsFileName, elisp, emacsFile}
    ,
    wolframFile = FileNameJoin[{lsp["Location"], "Resources", "Data", wolframFileName <> ".wl"}];
    symbols = Import[wolframFile];
    symbols = Partition[symbols, UpTo @ Ceiling[Length @ symbols / split]];
    Do[
        emacsFileName = StringTemplate["sci-wolfram-lsp-symbols-`1`"][ToLowerCase @ StringRiffle[StringCases[wolframFileName, RegularExpression["[A-Z][a-z]*"]], "-"]];
        If[split > 1,
            emacsFileName = StringTemplate["`1`-`2`"][emacsFileName, i]
        ];
        symbols[[i]] = StringRiffle[symbols[[i]], {"\"", "\"\n\"", "\""}];
        elisp = StringTemplate[";;; `1`.el --- Wolfram LSPServer symbols -*- lexical-binding: t -*-\n
;;; Commentary:\n
;; AUTO GENERATED WITH: `2`\n
;;; Code:\n
(defvar `1` '(
`3`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"][emacsFileName, "ProductKernelName" /. $ProductInformation, symbols[[i]]];
        emacsFile = FileNameJoin[{dir, "LSPSymbols", emacsFileName <> ".el"}];
        Export[emacsFile, elisp, "Text"];
        WriteString["stdout", StringTemplate["Convert `1` -> `2`"][wolframFile, emacsFile], "\n"];
        ,
        {i, split}
    ];
]

wolfram2elisp["BuiltinFunctions", 5];

wolfram2elisp["Constants", 1];

wolfram2elisp["Options", 1];

wolfram2elisp["SessionSymbols", 1];

wolfram2elisp["ExperimentalSymbols", 1];

wolfram2elisp["UndocumentedSymbols", 1];

wolfram2elisp["ObsoleteSymbols", 1];

wolfram2elisp["BadSymbols", 1];

wolfram2elisp["SystemLongNames", 1];

wolfram2elisp["FreeLongNames", 1];

wolfram2elisp["SpecialLongNames", 1];

wolfram2elisp["UndocumentedLongNames", 1];

wolfram2elisp["UnsupportedLongNames", 1];
