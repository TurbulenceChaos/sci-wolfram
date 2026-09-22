(* Display Wolfram script images *)

BeginPackage["sciWolfram`DisplayImage`"];

DisplayImage::usage = "Display Wolfram script images.
Usage:
Default:
$Post = DisplayImage[#] &;
All options:
$Post = DisplayImage[#,
ExprType   -> \"image\" (default) or \"latex\",
ImageDPI   -> 150 (default),
PlotPlay   -> \"yes\" or \"no\" (default) to convert dynamic plots to Wolfram Mathematica interactive files,
ShortLines -> 10 (default): use Short[code, n] to print long expressions less than n lines
] &;
Type below code to reset $Post:
$Post = .
";

Begin["`Private`"];

(* Initialize output counter *)

n = 1;

env = If[SameQ[Environment["TERM_PROGRAM"], "vscode"],
    "vscode"
    ,
    "emacs"
];

(* Display plain text output *)

write[string_] := WriteString["stdout", string, "\n"];

string[expr_, ShortLines_] := Module[{},
    If[SameQ[env, "emacs"],
        write[StringTemplate[": Out[`1`]= "][n++]]
    ];
    write[Short[expr, ShortLines]];
    (* Return original value for % calc in REPL *)
    (* https://reference.wolfram.com/language/ref/Out.html *)
    expr;
];

(* Display LaTeX output *)

latex[expr_, ShortLines_] := Module[{},
    If[SameQ[env, "emacs"],
        write[StringTemplate[": Out[`1`]= "][n++]]
    ];
    write["\\begin{equation*}"];
    write[TeXForm[Short[expr, ShortLines]]];
    write["\\end{equation*}"];
    expr;
];

(* Display Image output *)

notebook[expr_] := Notebook[{Cell[BoxData @ ToBoxes @ expr, "Output"]}]

player = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

image[expr_, ImageDPI_, PlotPlay_, ShortLines_] := Module[{plot, dir, png, nb},
    plot = notebook[Short[expr, ShortLines]];
    dir = FileNameJoin[{Directory[], "tmp", "wolfram"}];
    If[Not @ DirectoryQ[dir],
        CreateDirectory[dir, CreateIntermediateDirectories -> True]
    ];
    png = FileNameJoin[{dir, StringTemplate["plot-`1`.png"][CreateUUID[]]}];
    Export[png, plot, ImageResolution -> ImageDPI];
    Switch[env,
        "emacs",
            write[StringTemplate[": Out[`1`]= "][n++]];
            write[StringTemplate["[[file:`1`]]"][png]]
        ,
        "vscode",
            Run[StringTemplate["imgcat `1`"][png]];
    ];
    If[PlotPlay == "yes",
        nb = StringReplace[png, ".png" -> ".nb"];
        Export[nb, plot];
        If[StringQ[player],
            StartProcess[{player, FileNameTake[nb]}, ProcessDirectory -> dir]
            ,
            WriteString["stdout", "Wolfram Player or Wolfram Mathematica not found:
            FileNames[{\"*wolframplayer*\", \"*WolframNB*\"}, $InstallationDirectory, 2]", "\n"]
        ];
    ];
    expr;
];

(* Use $Post to automatically convert complex expressions and dynamic plots to images, LaTeX, or Wolfram Mathematica interactive files. *)

Options[DisplayImage] = {ExprType -> "image", ImageDPI -> 150, PlotPlay -> "no", ShortLines -> 10};

sysbox = Apply[Alternatives, ToExpression @ Select[Names["*Box"], StringFreeQ[{"RowBox", "InterpretationBox"}]]];

plotbox = Apply[Alternatives, ToExpression @ Names[{"Graphics*Box", "Dynamic*Box"}]];

DisplayImage[expr_, OptionsPattern[]] := Module[{box, isString, isPlot},
    If[SameQ[expr, Null],
        expr
        ,
        box = ToBoxes[expr];
        isString = FreeQ[box, sysbox | Cell];
        isPlot = Not @ FreeQ[box, plotbox];
        Which[
            isString,
                string[expr, OptionValue[ShortLines]]
            ,
            True,
                Switch[OptionValue[ExprType],
                    "latex",
                        If[isPlot,
                            image[expr, OptionValue[ImageDPI], OptionValue[PlotPlay], OptionValue[ShortLines]]
                            ,
                            latex[expr, OptionValue[ShortLines]]
                        ]
                    ,
                    "image",
                        image[
                            expr
                            ,
                            OptionValue[ImageDPI]
                            ,
                            If[isPlot,
                                OptionValue[PlotPlay]
                                ,
                                "no"
                            ]
                            ,
                            OptionValue[ShortLines]
                        ]
                ]
        ]
    ]
];

End[];

EndPackage[];
