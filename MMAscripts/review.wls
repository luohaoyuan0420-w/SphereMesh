#!/usr/bin/env wolframscript

sphereMeshCore`a;
sphereMeshCore`Xmin;sphereMeshCore`Xmax;sphereMeshCore`Ymin; sphereMeshCore`Ymax; sphereMeshCore`Zmin;   sphereMeshCore`Zmax;
review[button1_, button2_] := 
 Module[{}, linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
  linknumber3 = StringSplit[linkKernel[[1]], {"@"}][[1]];
  good = {}; bad = {};
  Scan[If[Length[#[[2]]] > 0, AppendTo[good, #[[1]]], 
     AppendTo[bad, #[[1]]]] &, sphereMeshCore`SUPPORTINGS];
  s = Map[
    Function[X, 
     ParametricPlot3D[
      X[[2]] {Sin[a] Cos[X[[3]]], Sin[a] Sin[X[[3]]], Cos[a]} + 
       X[[1]], {a, 0, X[[4]]}, PlotStyle -> Opacity[0.6]]], 
    Select[Flatten[Map[#[[2]] &, sphereMeshCore`SUPPORTINGS], 1], (#[[4]] != 0) &]];
UseFrontEnd[
  CreateWindow[
   DialogNotebook[
    DynamicModule[{sectionD = 1, optionListShow2 = {2}, 
      linknumber3 = linknumber3, sectionValX = {0, 1}, 
      sectionValY = {0, 1}, sectionValZ = {0, 1}, good = good, 
      bad = bad, Dx, Dy, Dz, XminF, XmaxF, YminF, YmaxF, ZminF, ZmaxF,
       s = s, VP = {2 \[Pi], 0}, 
      boundary = {{sphereMeshCore`Xmin, 
         sphereMeshCore`Xmax}, {sphereMeshCore`Ymin, 
         sphereMeshCore`Ymax}, {sphereMeshCore`Zmin, 
         sphereMeshCore`Zmax}}}, {{XminF, XmaxF}, {YminF, 
        YmaxF}, {ZminF, ZmaxF}} = boundary;
     Dx = XmaxF - XminF; Dy = YmaxF - YminF; Dz = ZmaxF - ZminF;
     Column[{Dynamic[
        Show[Join[{Graphics3D[
            Flatten[
             Map[{Opacity[If[MemberQ[optionListShow2, 1], 0.5, 1]], 
                Ball[#[[1 ;; 3]], #[[4]]]} &, good]]]}, {Graphics3D[
            Flatten[
             Map[{If[MemberQ[optionListShow2, 2], Red, White], 
                Opacity[If[MemberQ[optionListShow2, 1], 0.5, 1]], 
                Ball[#[[1 ;; 3]], #[[4]]]} &, bad]]]}, s], 
         PlotRange -> {{XminF, XminF} + 
            sectionValX*Dx, {YminF, YminF} + 
            sectionValY*Dy, {ZminF, ZminF} + sectionValZ*Dz}, 
         ImageSize -> {580, 580}, 
         ViewPoint -> 
          Dynamic[4 {Cos[VP[[1]]] Sin[VP[[2]]], 
             Sin[VP[[1]]] Sin[VP[[2]]], Cos[VP[[2]]]}]]], 
       Row[{Column[{Row[{TextCell[" Section:            ", 12], 
             TextCell["        ", 12], 
             CheckboxBar[
              Dynamic[
               optionListShow2], {1 -> TextCell["Translucent", 12], 
               2 -> TextCell["Show unsupported", 12]}]}], 
           IntervalSlider[Dynamic[sectionValX], 
            Appearance -> "Paired", ImageSize -> {450, 20}], 
           IntervalSlider[Dynamic[sectionValY], 
            Appearance -> "Paired", ImageSize -> {450, 20}], 
           IntervalSlider[Dynamic[sectionValZ], 
            Appearance -> "Paired", ImageSize -> {450, 20}]}], 
         Slider2D[Dynamic[VP], {{2 Pi, 0}, {0, Pi}}, 
          ImageSize -> {120, 120}, Appearance -> Small]}], 
       Row[{Button[button1, 
          linkFrontEnd = 
           LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
          LinkWrite[linkFrontEnd, 1]; DialogReturn[], 
          ImageSize -> {90, 30}], 
         Button[button2,           
          linkFrontEnd = 
           LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
          LinkWrite[linkFrontEnd, 2]; DialogReturn[], 
          ImageSize -> {90, 30}]}]}]]], WindowSize -> {600, 760}, 
   WindowTitle -> "Review", WindowFrameElements -> {}]]; 
  return = LinkRead[linkKernel];
  LinkClose[linkKernel];
  return];