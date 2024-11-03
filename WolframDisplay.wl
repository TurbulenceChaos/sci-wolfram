(* ::Package:: *)

BeginPackage["WolframDisplay`"];

WolframCDF::usage = "WolframCDF[g] exports the given expression g as a CDF file and opens it with WolframPlayer.

Parameters:
g: The expression to be exported and displayed.

Return value: The original expression g.";

WolframPNG::usage = "WolframPNG[g, displayMethod] exports the given expression g to a PNG file and displays it using the specified display method.

Parameters:
g: The expression to be exported and displayed.
displayMethod: The method for displaying the PNG file. It can be either \"External\" (default) or \"Inline\".

Return value: The original expression g.";

WolframDisplay::usage = "WolframDisplay[display, img] configures $Post to display formulas and plots as images or text based on the specified display and img arguments.

Parameters:
display: The method for displaying formulas and plots. It can be either \"InlinePNG\", \"ExternalPNG\", or \"WolframCDF\".
img: The type of content to be displayed. It can be either \"None\", \"Formula\", \"Plot\", or \"FormulaAndPlot\".

Return value: The original expression #. The function sets $Post to display the specified content.";

Begin["`Private`"];

(* Parse the configuration and set variables *)

packageDir = DirectoryName[$InputFileName];

configPath = FileNameJoin[{packageDir, "config.txt"}];

config = ReadList[configPath, String];

variables = Association[StringSplit[#, "="]& /@ config /. {name_, value_
    } :> (StringTrim[name] -> StringTrim[value])];

(* WolframPlayer: the path to the Wolfram Player executable *)

WolframPlayer = variables["WolframPlayer"];

(* ExternalPNG: the path to the photo viewer for displaying PNG files *)

ExternalPNG = variables["ExternalPNG"];

(* InlinePNG: the command for displaying inline PNG files *)

InlinePNG = variables["InlinePNG"];

(* ImageDPI: PNG files DPI *)

ImgDPI = ToExpression[variables["ImgDPI"]]

WolframCDF[g_] :=
    Module[
        {fileName, box}
        ,
        (* Construct the file name for the CDF file *)
        fileName = FileNameJoin[{"tmp", StringReplace[DateString["ISODateTime"
            ], ":" -> "-"] <> ".cdf"}];
        box = ToBoxes[g];
        (* Export expression as CDF and use WolframPlayer to open it*)
            
        StartProcess[{WolframPlayer, Export[fileName, Notebook[{Cell 
            @ BoxData @ box}, WindowSize -> Automatic], "CDF"]}];
        g
    ];

WolframPNG[g_, displayMethod_:"ExternalPNG"] :=
    Module[
        {fileName}
        ,
        (* Construct the file name for the PNG file *)
        fileName = FileNameJoin[{"tmp", StringReplace[DateString["ISODateTime"
            ], ":" -> "-"] <> ".png"}];
        (* Export expression as PNG *)
        Export[fileName, g, ImageResolution -> ImgDPI];
        While[!FileExistsQ[fileName], Pause[1]];
        Which[
            displayMethod == "ExternalPNG",
                StartProcess[{ExternalPNG, fileName}]
            ,
            displayMethod == "InlinePNG",
                Run[InlinePNG <> " " <> fileName]
        ];
        g
    ];

WolframDisplay[display_, img_] :=
    {
        PlotBox = DynamicBox | DynamicModuleBox | GraphicsBox | Graphics3DBox
            ;
        FormulaBox = RowBox | SqrtBox | SuperscriptBox | FormBox;
        $Post =
            If[img == "None",
                #
                ,
                With[{box = ToBoxes[#]},
                    If[FreeQ[
                            Which[
                                img == "Formula",
                                    FormulaBox
                                ,
                                img == "Plot",
                                    PlotBox
                                ,
                                img == "FormulaAndPlot",
                                    FormulaBox | PlotBox
                            ]
                        ] @ box,
                        #
                        ,
                        Which[
                            display == "InlinePNG",
                                WolframPNG[#, "InlinePNG"]
                            ,
                            display == "ExternalPNG",
                                WolframPNG[#, "ExternalPNG"]
                            ,
                            display == "WolframCDF",
                                WolframCDF[#]
                        ]
                    ]
                ]
            ]&;
    };

WolframDisplay["InlinePNG", "FormulaAndPlot"];

End[];

EndPackage[];
