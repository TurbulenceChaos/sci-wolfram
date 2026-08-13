(* Convert wolfram characters to emacs prettify symbols *)

Needs["LSPServer`ReplaceLongNamePUA`"]

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

wolframCharactersReplace[string_] := Module[
    {stringPrettify}
    ,
    If[
        StringLength[string] > 1
        ,
        stringPrettify = StringTemplate["(`1`)"][StringRiffle[Characters[string], {"?", " (Br . Bl) ?", ""}]]; StringReplace[stringPrettify, {"[" -> "\\[", "]" -> "\\]"}]
        ,
        ToString[InputForm[string]]
    ]
]

wolframCharacters = Select[Table[{ToString[FullForm[#]], #}&[FromCharacterCode[i]], {i, 65535}], StringContainsQ[#[[1]], "\\["]&];

wolframCharacters = {StringReplace[#[[1]], {"\\" -> "\\\\"}], replaceLongNamePUA[#[[2]]]}& /@ wolframCharacters;

wolframCharacters = Select[
    wolframCharacters
    ,
    StringFreeQ[
        #[[1]]
        ,
        {"Raw", "InlinePart", "Continuation", "LineSeparator", "ParagraphSeparator", "Invisible", "Space]", "Hyphen]", "Key]"}
    ] && StringFreeQ[#[[2]], {"\n", RegularExpression[" [A-Za-z0-9]+"]}] && Not @ StringMatchQ[#[[2]], ""]&
];

wolframCharacters = MapAt[wolframCharactersReplace, wolframCharacters, {All, 2}];

emacsSymbolsFormat = StringRiffle[MapApply[StringTemplate["(`1` . `2`)"], wolframCharacters], "\n"];

emacsFileName = "sci-wolfram-prettify-symbols";

emacsSymbols = StringTemplate[
";;; `1`.el --- Wolfram prettify symbols alist -*- lexical-binding: t -*-\n
;;; Commentary:\n
;; AUTO GENERATED FILE\n
;; GENERATED WITH: `3` `4`\n
;;; Code:\n
(defvar `1` '(
`2`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"][emacsFileName, emacsSymbolsFormat, "ProductIDName" /. $ProductInformation, $Version];

Export[FileNameJoin[{dir, emacsFileName <> ".el"}], emacsSymbols, "Text"];

WriteString["stdout", "Finish converting wolfram characters to emacs prettify symbols", "\n"];
