(* Convert wolfram script to PDF and Mathematica notebook *)

BeginPackage["sciWolframConvertToNotebook`"];

sciWolframConvertToNotebook::usage = "Usage: sciWolframConvertToNoteBook[\"/path/to/file.wl\"];"

Begin["`Private`"];

systemFormSymbols = Names["*Form"];

systemManipulateSymbols = Join[Names["Manipulate*"], StringReplace[Names["Manipulate`*"], RegularExpression["^Manipulate`"] -> ""]];

systemGraphicsSymbols = Names["Graphics*"];

systemSymbols = ToExpression @ Join[systemFormSymbols, systemManipulateSymbols, systemGraphicsSymbols];

(* Ref: https://community.wolfram.com/groups/-/m/t/37054 *)

systemSymbolsReplace = # -> Defer[#]& /@ systemSymbols;

basicReplace = {HoldComplete[x_] -> Defer[x]};

sciWolframPlayer = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

sciWolframExpr[expr_] :=
    Module[{exprNB},
        If[MatchQ[First[expr], TextCell[__]],
            exprNB = First[expr];
            exprNB
            ,
	    exprNB = expr /. basicReplace /. systemSymbolsReplace;
            ExpressionCell[exprNB, "Input"]
        ]
    ];

sciWolframConvert[file_] :=
    Module[{fileBaseName, dir, filePDF, fileNB, exprs, cells, notebook},
        SetOptions[First[$Output], FormatType -> StandardForm]; (* Ref: https://mathematica.stackexchange.com/a/133058/95308 *)

	fileBaseName = FileBaseName[file];
        dir = DirectoryName[AbsoluteFileName[file]];
        filePDF = FileNameJoin[{dir, StringTemplate["`1`-convert.pdf"][fileBaseName]}];
        fileNB = FileNameJoin[{dir, StringTemplate["`1`-convert.nb"][fileBaseName]}];

	exprs = Import[file, "HeldExpressions"];
        cells = Map[sciWolframExpr, exprs];

        UsingFrontEnd[
            notebook = CreateDocument[cells];
            NotebookEvaluate[notebook, InsertResults -> True];
	    Export[filePDF, notebook];
            Export[fileNB, notebook];
            NotebookClose[notebook];

	    If[StringQ @ Environment["WSL_DISTRO_NAME"],
                StartProcess[{"explorer.exe", FileNameTake[filePDF]}, ProcessDirectory -> dir];
                If[StringQ[sciWolframPlayer],
                    StartProcess[{sciWolframPlayer, FileNameTake[fileNB]}, ProcessDirectory -> dir]
                    ,
                    WriteString["stdout", "Wolfram Player or Mathematica not found"]
                ];
                ,
                SystemOpen[filePDF];
                SystemOpen[fileNB]
            ];
        ];

        WriteString["stdout", "Convert ", file, " -> ", filePDF, "\n"];
        WriteString["stdout", "Convert ", file, " -> ", fileNB, "\n"];
    ];

sciWolframConvertToNotebook[file_] := Block[{$Post}, sciWolframConvert[file]];

End[];

EndPackage[];

