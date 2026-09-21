(* Convert Wolfram characters to Emacs prettify symbols *)

dir = Check[
    If[$Notebooks,
        NotebookDirectory[]
        ,
        Directory[]
    ]
    ,
    $InputFileName
];

(* Remove Unicode Private Use Area (PUA): U+E000 .. U+F8FF => 57344 .. 63743 *)

characters = Select[
    Table[
        {StringReplace[ToString @ FullForm @ FromCharacterCode[i], {"\\" -> "\\\\"}], FromCharacterCode[i]}
        ,
        {i, Join[Range[1, 57343], Range[63744, 65535]]}
    ]
    ,
    StringContainsQ[#[[1]], "\\["]&
];

characters = StringRiffle[MapApply[StringTemplate["(`1` . \"`2`\")"], characters], "\n"];

file = "sci-wolfram-prettify-symbols";

elisp = StringTemplate[";;; `1`.el --- Wolfram prettify symbols alist -*- lexical-binding: t -*-\n
;;; Commentary:\n
;; AUTO GENERATED FILE\n
;; GENERATED WITH: `2`\n
;;; Code:\n
(defvar `1` '(
`3`
))\n\n
(provide '`1`)
;;; `1`.el ends here\n"][file, "ProductKernelName" /. $ProductInformation, characters];

Export[FileNameJoin[{dir, file <> ".el"}], elisp, "Text"];

WriteString["stdout", "Convert Wolfram characters to Emacs prettify symbols.", "\n"];
