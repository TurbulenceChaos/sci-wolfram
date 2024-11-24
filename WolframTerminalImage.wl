(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

Begin["`Private`"];

WolframTerminalImage[g_] :=
    Module[{file, plot},
        file = FileNameJoin[$TemporaryDirectory, "Wolfram", StringReplace[
            DateString["ISODateTime"], ":" -> "-"] <> ".png"];
        plot = g;
        Export[file, plot];
        Run["imgcat " <> file];
        Quiet @ DeleteFile @ file;
        plot;
    ];

$Post =
    With[{box = ToBoxes[#], plotBox = DynamicBox | DynamicModuleBox |
         GraphicsBox | Graphics3DBox, formulaBox = RowBox | SqrtBox | SuperscriptBox
        },
        If[FreeQ[formulaBox | plotBox] @ box,
            #
            ,
            WolframTerminalImage[#]
        ]
    ]&;

End[];

EndPackage[];
