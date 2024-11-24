(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

Begin["`Private`"];

WolframTerminalImage[g_] :=
    Module[{file},
        file = FileNameJoin[$TemporaryDirectory, "Wolfram", StringReplace[
            DateString["ISODateTime"], ":" -> "-"] <> ".png"];
        Export[file, g];
        Run["imgcat " <> file];
        Quiet @ DeleteFile @ file;
        g;
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
