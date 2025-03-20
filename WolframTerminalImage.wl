(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

(* Specify the terminal type for Wolfram terminal images (options: "vscode", "emacs") *)

wolframTerminalType = "vscode";

(* Set the resolution (in DPI) for Wolfram terminal images *)

wolframTerminalImageResolution = 100;

(* Enable ("yes") or disable ("no") automatic deletion of Wolfram terminal images *)

wolframTerminalDeleteImage = "no";

(* Enable ("yes") or disable ("no") playback of Wolfram terminal CDF files *)

wolframTerminalPlay = "no";

(* Specify the player application for Wolfram terminal CDF files *)

(* Options: "wolframplayer" for Linux or WSL2, "/path/to/wolframplayer.exe" for Windows or WSL2 *)

wolframplayer = "wolframplayer";

Begin["`Private`"];

WolframTerminalImage[g_, playCDF_] :=
    Module[{file, filePNG, fileCDF, expr},
        file = FileNameJoin[{"tmp", "wolfram", CreateUUID["wolfram-"]
            }];
        filePNG = FileNameJoin[{Directory[], file <> ".png"}];
        fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
        expr = g;
        Export[filePNG, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], 
            ImageResolution -> wolframTerminalImageResolution];
        Which[
            wolframTerminalType == "emacs",
                Print["[[file:" <> file <> ".png]]"]
            ,
            wolframTerminalType == "vscode",
                Run["imgcat " <> filePNG];
                If[wolframTerminalDeleteImage == "yes",
                    Quiet @ DeleteFile @ filePNG
                ];
        ];
        If[playCDF == "yes",
            Export[fileCDF, Notebook[{Cell @ BoxData @ ToBoxes @ expr
                }]];
            StartProcess[{wolframplayer, FileNameTake[fileCDF]}, ProcessDirectory
                 -> DirectoryName[fileCDF]];
        ];
        expr;
    ];

$Post =
    With[{box = ToBoxes[#], plotBox = DynamicBox | DynamicModuleBox |
         GraphicsBox | Graphics3DBox, formulaBox = RowBox | SqrtBox | SuperscriptBox
        },
        Which[
            wolframTerminalPlay == "no",
                If[FreeQ[formulaBox | plotBox][box],
                    #
                    ,
                    WolframTerminalImage[#, playCDF = "no"]
                ];
            ,
            wolframTerminalPlay == "yes",
                If[FreeQ[formulaBox | plotBox][box],
                    #
                    ,
                    With[{
                        playCDF =
                            If[!FreeQ[formulaBox][box],
                                "no"
                                ,
                                "yes"
                            ]
                    },
                        WolframTerminalImage[#, playCDF]
                    ]
                ];
        ]
    ]&;

End[];

EndPackage[];
