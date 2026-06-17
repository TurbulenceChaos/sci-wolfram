(* Convert wolfram script to PDF and Mathematica notebook *)

BeginPackage["sciWolframConvertToNotebook`"];

sciWolframConvertToNotebook::usage = "Usage: sciWolframConvertToNoteBook[\"/path/to/file.wl\"];"

Begin["`Private`"];

sciWolframPlayer = FileNames["*wolframplayer*", $InstallationDirectory,2][[1]];

sciWolframConvertToNotebook[file_] :=
	Block[{$Post},
		Module[{fileBaseName, dir, filePDF, fileNB, exprs, cells, notebook},
			SetOptions[First[$Output], FormatType -> StandardForm]; (* Ref: https://mathematica.stackexchange.com/a/133058/95308 *)
			fileBaseName = FileBaseName[file];
			dir = DirectoryName[AbsoluteFileName[file]];
			filePDF = FileNameJoin[{dir, fileBaseName <> "-convert.pdf"}];
			fileNB = FileNameJoin[{dir, fileBaseName <> "-convert.nb"}];
			exprs = Import[file, "HeldExpressions"];
			(* Convert exprs to notebook cells *)
			cells =
				Function[expr,
						If[MatchQ[First[expr], TextCell[__]],
							With[{expr = First[expr]},
								expr
							]
							,
							With[{
								expr =
									expr /.
										{
											HoldComplete -> Defer
											,
											Manipulate -> Defer[Manipulate]
											,
											MatrixForm -> Defer[MatrixForm] (* Ref: https://community.wolfram.com/groups/-/m/t/37054 *)
											,
											TableForm -> Defer[TableForm]
											,
											TeXForm -> Defer[TeXForm]
											,
											TraditionalForm -> Defer[TraditionalForm]
										}
							},
								ExpressionCell[expr, "Input"]
							]
						]
					] /@ exprs;
			UsingFrontEnd[
				notebook = CreateDocument[cells];
				NotebookEvaluate[notebook, InsertResults -> True];
				Export[filePDF, notebook];
				Export[fileNB, notebook];
				NotebookClose[notebook];
				If[StringQ @ Environment["WSL_DISTRO_NAME"],
					StartProcess[{"explorer.exe", FileNameTake[filePDF]}, ProcessDirectory -> dir];
					StartProcess[{sciWolframPlayer, FileNameTake[fileNB]}, ProcessDirectory -> dir];
					,
					SystemOpen[filePDF];
					SystemOpen[fileNB];
				];
			];
			WriteString["stdout", "Convert ", file, " -> ", filePDF, "\n"];
			WriteString["stdout", "Convert ", file, " -> ", fileNB, "\n"];
			(* Add below code to stop frontend if you use: wolframscript -f /path/to/TestConvertToNotebook.wl *)
			(* Developer`UninstallFrontEnd[]; *)
		]
	]

End[];

EndPackage[];
