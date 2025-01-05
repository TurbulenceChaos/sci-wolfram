(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

Begin["`Private`"];

WolframTerminalImage[g_] :=
    Module[{file, expr},
        file = FileNameJoin[$TemporaryDirectory, "Wolfram", StringReplace[
            DateString["ISODateTime"], ":" -> "-"] <> ".png"];
        expr = g;
        Export[file, Notebook[{Cell @ BoxData @ ToBoxes @ expr}]];
        Run["imgcat " <> file];
        Quiet @ DeleteFile @ file;
        expr;
    ];

$Post =
    With[{box = ToBoxes[#], plotBox = DynamicBox | DynamicModuleBox |
         GraphicsBox | Graphics3DBox, formulaBox = RowBox | SqrtBox | SuperscriptBox
         | InterpretationBox},
        If[FreeQ[formulaBox | plotBox] @ box,
            #
            ,
            WolframTerminalImage[#]
        ]
    ]&;

End[];

EndPackage[];
