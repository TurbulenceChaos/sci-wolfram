Needs["CodeFormatter`"];

(* input = Import["!cat", "String"]; *)

input = Import[$ScriptCommandLine[[2]], "String"];

WriteString[$Output, CodeFormat[input]];
