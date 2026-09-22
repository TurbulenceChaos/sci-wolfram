(* Convert Wolfram script to Wolfram Mathematica notebook *)

BeginPackage["sciWolfram`Notebook`"];

ConvertToNotebook::usage = "Usage: ConvertToNoteBook[\"/path/to/file.wl\"];"

Begin["`Private`"];

symbols = ToExpression @ StringReplace[Names[{"*Form", "Manipulate*", "Manipulate`*", "Graphics*"}], "Manipulate`" -> ""];

(* https://community.wolfram.com/groups/-/m/t/37054 *)

symbols = # -> Defer[#]& /@ symbols;

process[expr_] := Module[{input},
    If[MatchQ[First[expr], TextCell[__]],
        input = First[expr];
        input
        ,
        input = expr /. {HoldComplete[x_] -> Defer[x]} /. symbols;
        ExpressionCell[input, "Input"]
    ]
];

player = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

convert[file_] := Module[
    {fileName, dir, pdf, nb, exprs, cells, notebook}
    ,
    (* https://mathematica.stackexchange.com/a/133058/95308 *)
    SetOptions[First[$Output], FormatType -> StandardForm];
    fileName = FileBaseName[file];
    dir      = DirectoryName[AbsoluteFileName[file]];
    pdf      = FileNameJoin[{dir, StringTemplate["`1`-convert.pdf"][fileName]}];
    nb       = StringReplace[pdf, ".pdf" -> ".nb"];
    exprs    = Import[file, "HeldExpressions"];
    cells    = Map[process, exprs];
    UsingFrontEnd[notebook = CreateDocument[cells];
        NotebookEvaluate[notebook, InsertResults -> True];
        Export[pdf, notebook];
        Export[nb, notebook];
        NotebookClose[notebook];
    ];
    WriteString["stdout", "Convert ", file, " -> ", nb, "\n"];
    If[StringQ[player],
        StartProcess[{player, FileNameTake[nb]}, ProcessDirectory -> dir]
        ,
        WriteString["stdout", "Wolfram Player or Wolfram Mathematica not found:
        FileNames[{\"*wolframplayer*\", \"*WolframNB*\"}, $InstallationDirectory, 2]", "\n"]
    ];
];

ConvertToNotebook[file_] := Block[{$Post},
    convert[file]
];

End[];

EndPackage[];
