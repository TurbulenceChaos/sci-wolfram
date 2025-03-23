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

(* TODO: The '%' operator works only in terminal mode *)

WolframTerminalText[g_] :=
    Module[{expr},
        expr = g;
        Print[expr];
        expr;
    ];

WolframTerminalTeX[g_] :=
    Module[{expr},
        expr = g;
        Print["\\begin{equation*}\n" <> ToString[TeXForm[expr]] <> "\n\\end{equation*}"
            ];
        expr;
    ];

WolframTerminalImage[g_, playCDF_] :=
    Module[{dir, filePNG, fileCDF, expr},
        dir = FileNameJoin[{Directory[], "tmp", "wolfram"}];
        filePNG = FileNameJoin[{dir, CreateUUID["wolfram-"] <> ".png"
            }];
        fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
        expr = g;
        If[$VersionNumber < 12.2,
            If[!DirectoryQ[dir],
                CreateDirectory[dir, CreateIntermediateDirectories ->
                     True]
            ]
        ];
        Export[filePNG, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], 
            ImageResolution -> wolframTerminalImageResolution];
        Which[
            wolframTerminalType == "emacs",
                Print["[[file:" <> FileNameDrop[filePNG, FileNameDepth
                     @ Directory[]] <> "]]"]
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
        If[FreeQ[formulaBox | plotBox][box],
            If[MatchQ[#, Null],
                #
                ,
                WolframTerminalText[#]
            ]
            ,
            Which[
                wolframTerminalType == "emacs",
                    If[!FreeQ[formulaBox][box],
                        WolframTerminalTeX[#]
                        ,
                        If[wolframTerminalPlay == "no",
                            WolframTerminalImage[#, playCDF = "no"]
                            ,
                            WolframTerminalImage[#, playCDF = "yes"]
                        ]
                    ]
                ,
                wolframTerminalType == "vscode",
                    Which[
                        wolframTerminalPlay == "no",
                            WolframTerminalImage[#, playCDF = "no"]
                        ,
                        wolframTerminalPlay == "yes",
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
                    ]
            ]
        ]
    ]&;

End[];

EndPackage[];
