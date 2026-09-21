(* Convert wolfram script to PDF and Mathematica notebook *)

BeginPackage["sciWolframConvertToNotebook`"];

sciWolframConvertToNotebook::usage = "Usage: sciWolframConvertToNoteBook[\"/path/to/file.wl\"];"

Begin["`Private`"];

player = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

formSymbols = Names["*Form"];

manipulateSymbols = Join[Names["Manipulate*"], StringReplace[Names["Manipulate`*"], "Manipulate`" -> ""]];

graphicsSymbols = Names["Graphics*"];

symbols = ToExpression @ Join[formSymbols, manipulateSymbols, graphicsSymbols];

(* reference: https://community.wolfram.com/groups/-/m/t/37054 *)

symbols = # -> Defer[#]& /@ symbols;

sciWolframExpr[expr_] := Module[{exprNB},
    If[MatchQ[First[expr], TextCell[__]],
        exprNB = First[expr];
        exprNB
        ,
        exprNB = expr /. {HoldComplete[x_] -> Defer[x]} /. symbols;
        ExpressionCell[exprNB, "Input"]
    ]
];

sciWolframConvert[file_] := Module[{fileName, dir, pdf, nb, exprs, cells, notebook},
    SetOptions[First[$Output], FormatType -> StandardForm]; (* Ref: https://mathematica.stackexchange.com/a/133058/95308 *)
    fileName = FileBaseName[file];
    dir = DirectoryName[AbsoluteFileName[file]];
    pdf = FileNameJoin[{dir, StringTemplate["`1`-convert.pdf"][fileName]}];
    nb = StringReplace[pdf, ".pdf" -> ".nb"];
    exprs = Import[file, "HeldExpressions"];
    cells = Map[sciWolframExpr, exprs];
    UsingFrontEnd[
        notebook = CreateDocument[cells];
        NotebookEvaluate[notebook, InsertResults -> True];
        Export[pdf, notebook];
        Export[nb, notebook];
        NotebookClose[notebook];
        If[StringQ @ Environment["WSL_DISTRO_NAME"],
            StartProcess[{"explorer.exe", FileNameTake[pdf]}, ProcessDirectory -> dir];
            If[StringQ[player],
                StartProcess[{player, FileNameTake[nb]}, ProcessDirectory -> dir]
                ,
                WriteString["stdout", "Wolfram Player or Mathematica not found!"]
            ];
            ,
            SystemOpen[pdf];
            SystemOpen[nb]
        ];
    ];
    WriteString["stdout", "Convert ", file, " -> ", pdf, "\n"];
    WriteString["stdout", "Convert ", file, " -> ", nb, "\n"];
];

sciWolframConvertToNotebook[file_] := Block[{$Post},
    sciWolframConvert[file]
];

End[];

EndPackage[];
