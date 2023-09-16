#!/usr/bin/env wolframscript
addBallsQ[] := 
 Module[{return}, linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
  linknumber6 = StringSplit[linkKernel[[1]], {"@"}][[1]];
  UseFrontEnd[
   CreateWindow[
    DialogNotebook[
     DynamicModule[{list = {}, linknumber6 = linknumber6}, 
      Column[{TextCell["Add new balls?" , 20, Gray], 
        TextCell["\n", 12], 
        Row[{Button[TextCell["Yes", 12], 
           linkFrontEnd = 
            LinkConnect[linknumber6, LinkProtocol -> "TCPIP"];
           LinkWrite[linkFrontEnd, True];
           DialogReturn[], ImageSize -> {90, 30}], 
          Button[TextCell["No", 12], 
           linkFrontEnd = 
            LinkConnect[linknumber6, LinkProtocol -> "TCPIP"];
           LinkWrite[linkFrontEnd, False];
           DialogReturn[], ImageSize -> {90, 30}]}]}]]], 
    WindowSize -> {300, 120}, WindowTitle -> "Edit sphere mesh", 
    WindowFrameElements -> {}]];
  return = LinkRead[linkKernel];
  LinkClose[linkKernel];
  return];