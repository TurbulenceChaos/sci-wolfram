dir = $ScriptCommandLine[[2]];

(* Function to convert Wolfram symbol files to Emacs Lisp format *)
sciwolframSymbols[fileName_, splitNum_] :=
  Module[{symbol, symbolNum, splitSymbolNum},
         symbol = Import[FileNameJoin[{dir, "Data", fileName <> ".wl"}]];
         symbolNum = Length[symbol];
         splitSymbolNum = Quotient[symbolNum, splitNum];
         symbol = If[Mod[symbolNum, splitNum]==0, 
         Partition[symbol, splitSymbolNum],
         Join[Partition[Drop[symbol, -splitSymbolNum - Mod[symbolNum, splitNum]], splitSymbolNum],
         {Drop[symbol, splitSymbolNum * (splitNum - 1)]}]
	 ];

  symbol= MapThread[StringJoin["(defvar ", fileName, If[splitNum > 1, "-" <> ToString[#1], ""], " nil)\n",
	  		       "(setq ", fileName, If[splitNum > 1, "-" <> ToString[#1], ""], " '",
       			       StringRiffle[ToString[#, InputForm]& /@ #2, {"(", "\n", ")"}], ")"]&,
			       {Range[splitNum], symbol}];
  
  symbol = StringJoin[
		   ";;; ", fileName, ".el -*- lexical-binding: t -*-\n\n",
		   ";;; Commentary:\n",
		   ";;; Code:\n\n",
		   StringRiffle[symbol,"\n\n"],
		   "\n\n(provide '", fileName, ")\n",
		   ";;; ", fileName, ".el ends here"
		   ];
		   
  Export[FileNameJoin[{dir, "Data", fileName <> ".el"}], symbol, "Text"];

  Print[StringJoin["Conversion completed: ", fileName, ".wl -> ", fileName, ".el"]];
]

(* Convert the symbol files *)
sciwolframSymbols["BuiltinFunctions", 5];

sciwolframSymbols["SystemLongNames", 1];

sciwolframSymbols["Constants", 1];
