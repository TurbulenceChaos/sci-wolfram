(* Usage: 

1. no wolframplayer
wolframscript -script /path/to/sci-wolfram-pdf.wl /path/to/file.wl 

2. with wolframplayer

Whether to use `wolframplayer` to view `.cdf` files: "yes" (Enable) or "no" (disable by default)

3. specify wolframplayer path

Wolfram Player path is set as FileNames["*wolframplayer*", $InstallationDirectory, 2][[1]] by default

You can also specify the path by

(for linux)
wolframscript -script /path/to/sci-wolfram-pdf.wl /path/to/file.wl "yes" "/path/to/wolframplayer"

(for windows)
wolframscript -script /path/to/sci-wolfram-pdf.wl /path/to/file.wl "yes" "/path/to/wolframplayer.exe"

*)

(* Options suggested by https://mathematica.stackexchange.com/a/133058/95308 *)

SetOptions[First[$Output], FormatType -> StandardForm];

(* Import script as a list of deferred expressions *)

file = $ScriptCommandLine[[2]];

base = FileBaseName[file];

dir = DirectoryName[AbsoluteFileName[file]];

filePDF = FileNameJoin[{dir, base <> "-convert.pdf"}];

fileNB = FileNameJoin[{dir, base <> "-convert.nb"}];

sciWolframPlay = Quiet[Check[$ScriptCommandLine[[3]], "no"]];

sciWolframPlayer = Quiet[Check[$ScriptCommandLine[[4]], FileNames["*wolframplayer*",
     $InstallationDirectory, 2][[1]]]];

exprs = Import[file, "HeldExpressions"];

(* Convert expressions into notebook cells *)

(* Defer manipulate: https://community.wolfram.com/groups/-/m/t/37054 *)

cells =
    Function[expr,
            If[StringContainsQ[ToString[expr], "TextCell"],
                With[{expr = First @ expr},
                    expr
                ]
                ,
                With[{expr = expr /. {HoldComplete -> Defer, Manipulate
                     -> Defer[Manipulate], MatrixForm -> Defer[MatrixForm], TableForm -> 
                    Defer[TableForm], TeXForm -> Defer[TeXForm], TraditionalForm -> Defer[
                    TraditionalForm]}},
                    ExpressionCell[expr, "Input"]
                ]
            ]
        ] /@ exprs;

UsingFrontEnd[
    nb = CreateDocument[cells];
    NotebookEvaluate[nb, InsertResults -> True];
    Export[filePDF, nb];
    Export[fileNB, nb];
    NotebookClose[nb];
];

If[sciWolframPlay == "yes",
    StartProcess[{sciWolframPlayer, FileNameTake[fileNB]}, ProcessDirectory
         -> dir]
];

(* If you use wolfram -f /path/to/sci-wolfram-pdf.wl /path/to/file.wl, add this line to stop frontend *)

Developer`UninstallFrontEnd[]
