(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

wolframTerminalDeleteImage = "no";

wolframTerminalImageResolution = 100;

Begin["`Private`"];

WolframTerminalImage[g_] :=
    Module[{file, expr},
        file = FileNameJoin["tmp", "wolfram", CreateUUID["wolfram-"] 
            <> ".png"];
        expr = g;
        Export[file, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], ImageResolution
             -> wolframTerminalImageResolution];
        Run["imgcat " <> file];
        If[wolframTerminalDeleteImage == "yes",
            Quiet @ DeleteFile @ file
        ];
        expr;
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
