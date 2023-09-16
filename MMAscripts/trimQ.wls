#!/usr/bin/env wolframscript
trimQ[] := 
 Module[{return}, linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
  linknumber5 = StringSplit[linkKernel[[1]], {"@"}][[1]];
UseFrontEnd[
  CreateWindow[
   DialogNotebook[
    DynamicModule[{list = {}, linknumber5 = linknumber5}, 
     Column[{CheckboxBar[
        Dynamic[list], {1 -> 
          TextCell["Trim along boundary", 20, Gray]}], 
       TextCell["\n", 12], 
       Row[{Button[TextCell["Start", 12], 
          linkFrontEnd = 
           LinkConnect[linknumber5, LinkProtocol -> "TCPIP"];
          LinkWrite[linkFrontEnd, {1, list}];
          DialogReturn[], ImageSize -> {90, 30}], 
         Button[TextCell["Cancel", 12], 
          linkFrontEnd = 
           LinkConnect[linknumber5, LinkProtocol -> "TCPIP"];
          LinkWrite[linkFrontEnd, {0, list}];
          DialogReturn[], ImageSize -> {90, 30}]}]}]]], 
   WindowSize -> {300, 120}, WindowTitle -> "Build sphere mesh", 
   WindowFrameElements -> {}]];
  return = LinkRead[linkKernel];
  LinkClose[linkKernel];
  state = return[[1]]; If[Length[return[[2]]] > 0, True, False]]