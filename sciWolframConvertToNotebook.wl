(* Convert wolfram script to PDF and Mathematica notebook *)

BeginPackage["sciWolframConvertToNotebook`"];

sciWolframConvertToNotebook::usage = "sciWolframConvertToNoteBook[file];
Auto use Wolfram Player to view Mathematica notbook:
sciWolframPlay = \"yes\";
sciWolframConvertToNoteBook[file, sciWolframPlay];
Manually specify Wolfram Player path:
sciWolframPlay = \"yes\";
sciWolframPlayer = \"/path/to/wolframplayer\" for linux or \"/path/to/wolframplayer.exe\" for windows;
sciWolframConvertToNoteBook[file, sciWolframPlay, sciWolframPlayer];";

Begin["`Private`"];

sciWolframPlayer = FileNames["*wolframplayer*", $InstallationDirectory, 2][[1]];

sciWolframConvertToNotebook[file_, sciWolframPlay_: "no", sciWolframPlayer_: sciWolframPlayer] :=

Module[{fileBaseName, dir, filePDF, fileNB, exprs, cells, notebook},
SetOptions[First[$Output], FormatType -> StandardForm]; (* Ref: https://mathematica.stackexchange.com/a/133058/95308 *)

fileBaseName = FileBaseName[file];

dir = DirectoryName[AbsoluteFileName[file]];

filePDF = FileNameJoin[{dir, fileBaseName <> "-convert.pdf"}];

fileNB = FileNameJoin[{dir, fileBaseName <> "-convert.nb"}];

exprs = Import[file, "HeldExpressions"];

(* Convert exprs to notebook cells *)

cells =
    Function[expr,
            If[MatchQ[First[expr], TextCell[__]],
                With[{expr = First[expr]}, expr]
                ,
                With[{expr = expr /. {
                    HoldComplete -> Defer,
                    Manipulate -> Defer[Manipulate], (* Ref: https://community.wolfram.com/groups/-/m/t/37054 *)
                    MatrixForm -> Defer[MatrixForm],
                    TableForm -> Defer[TableForm],
                    TeXForm -> Defer[TeXForm],
                    TraditionalForm -> Defer[TraditionalForm]}},
                    ExpressionCell[expr, "Input"]
                ]
            ]
        ] /@ exprs;

UsingFrontEnd[
    notebook = CreateDocument[cells];
    NotebookEvaluate[notebook, InsertResults -> True];
    Export[filePDF, notebook];
    Export[fileNB, notebook];
    NotebookClose[notebook];
];

If[sciWolframPlay == "yes",
    StartProcess[{sciWolframPlayer, FileNameTake[fileNB]}, ProcessDirectory -> dir]
];

(* Add below code to stop frontend if you use: wolframscript -f /path/to/sci-wolfram-pdf.wl /path/to/file.wl *)

(* Developer`UninstallFrontEnd[]; *)
]

End[];

EndPackage[];
