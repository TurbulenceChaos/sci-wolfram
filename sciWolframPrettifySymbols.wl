(* Get Wolfram buildin symbols and characters *)

Needs["LSPServer`ReplaceLongNamePUA`"]

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

(* systemFullSymbols = ToString[FullForm[#]]& /@ Names["System`*"]; *)

(* systemSymbols = Select[systemFullSymbols, StringFreeQ["\\["]]; *)

(* systemSymbolsLisp = StringRiffle[systemSymbols, "\n"]; *)

replacePrettify[string_] :=
    Module[{len, stringPrettify},
        len = StringLength[string];
        If[len > 1,
	    stringPrettify = StringTemplate["(`1`)"][StringRiffle[Characters[string],{"?"," (Br . Bl) ?", ""}]];
	    StringReplace[stringPrettify, {"["->"\\[","]"->"\\]"}]
            ,
            ToString[InputForm[string]]
        ]
    ]

characters = Select[Table[{ToString[FullForm[#]], #}&[FromCharacterCode[i]], {i, 65535}], StringContainsQ[#[[1]], "\\["]&];

charactersReplace = StringReplace[#, {"\\" -> "\\\\"}]& /@ MapAt[replaceLongNamePUA, characters, {All, 2}];

charactersIgnore = Select[charactersReplace,
StringFreeQ[#[[1]], {"Raw","InlinePart", "Continuation"}] &&
StringFreeQ[#[[2]], {"\n", RegularExpression[" [A-Za-z0-9]+ "]}]&
];

charactersPrettify = MapAt[replacePrettify, charactersIgnore, {All, 2}];

charactersFormat = StringRiffle[MapApply[StringTemplate["(`1` . `2`)"], charactersPrettify], "\n"];

fileName = "sci-wolfram-prettify-symbols-alist";

prettifySymbols = StringTemplate[
";;; `1`.el --- Wolfram prettify symbols alist -*- lexical-binding: t -*-\n
;;; Commentary:\n
;; AUTO GENERATED FILE\n
;; GENERATED WITH: `3` `4`\n
;;; Code:\n
(defvar `1` nil)
(setq `1` '(
`2`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"
][fileName, charactersFormat, "ProductIDName" /. $ProductInformation, $Version];

Export[FileNameJoin[{dir, fileName <> ".el"}], prettifySymbols, "Text"];

WriteString["stdout", "Finish converting Wolfram characters to Emacs prettify symbols","\n"];

