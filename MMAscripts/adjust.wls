adjust[] := 
 Module[{goodlist, badlist, tempBall, tempShift, BALLSAdjust, 
   VPAdjust, nAdjust, sectionValXAdjust, sectionValYAdjust, 
   sectionValZAdjust, optionListShow2Adjust}, 
  return = {"refresh", {}, {}, {}, 0, {}, 1, {}, {}, {}, {}};
  s = Map[
    Function[X, 
     ParametricPlot3D[
      X[[2]] {Sin[a] Cos[X[[3]]], Sin[a] Sin[X[[3]]], Cos[a]} + 
       X[[1]], {a, 0, X[[4]]}, PlotStyle -> Opacity[0.6]]], 
    Select[Flatten[Map[#[[2]] &, sphereMeshCore`SUPPORTINGS], 
      1], (#[[4]] != 0) &]];
  good = {}; bad = {};
  Scan[If[Length[#[[2]]] > 0, AppendTo[good, #[[1]]], 
     AppendTo[bad, #[[1]]]] &, sphereMeshCore`SUPPORTINGS];
  goodlist = 
   Map[Position[sphereMeshCore`SUPPORTINGS, #] &, good][[1 ;; -1, 1, 
     1]];
  badlist = 
   Map[Position[sphereMeshCore`SUPPORTINGS, #] &, bad][[1 ;; -1, 1, 
     1]];
  Clear[sphereMeshCore`w];
  While[return[[1]] === "refresh", 
   If[return[[5]] != 0, tempBall = return[[3]];
    tempShift = return[[4]]; BALLSAdjust = return[[2]];
    VPAdjust = return[[6]]; nAdjust = return[[7]];
    sectionValXAdjust = return[[8]]; sectionValYAdjust = return[[9]];
    sectionValZAdjust = return[[10]];
    optionListShow2Adjust = return[[11]], 
    BALLSAdjust = sphereMeshCore`SUPPORTINGS[[1 ;; -1, 1, 1 ;; 8]];];
   linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
   linknumber3 = StringSplit[linkKernel[[1]], {"@"}][[1]];
   good = Part[BALLSAdjust, goodlist];
   bad = Part[BALLSAdjust, badlist];
   If[! Head[sphereMeshCore`stl] === Symbol, 
    graph110 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[0.5], 
             Ball[#[[1 ;; 3]], #[[4]]]} &, bad]]]}, 
       s, {Graphics3D[{Opacity[0.3], White, sphereMeshCore`stl}]}]];
    graph010 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, 
       s, {Graphics3D[{Opacity[0.3], White, sphereMeshCore`stl}]}]];
    graph111 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, 
       s, {Graphics3D[{Opacity[0.3], White, sphereMeshCore`stl}]}]];
    graph011 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           bad]]]}, 
       s, {Graphics3D[{Opacity[0.3], White, sphereMeshCore`stl}]}]];
    graph100 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[0.5], 
             Ball[#[[1 ;; 3]], #[[4]]]} &, bad]]]}, s]];
    graph000 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, s]];
    graph101 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, s]];
    graph001 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           bad]]]}, s]];, 
    graph110 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[0.5], 
             Ball[#[[1 ;; 3]], #[[4]]]} &, bad]]]}, s]];
    graph010 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, s]];
    graph111 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[
          Map[{Red, Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           bad]]]}, s]];
    graph011 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           bad]]]}, s]];
    graph100 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[0.5], 
             Ball[#[[1 ;; 3]], #[[4]]]} &, bad]]]}, s]];
    graph000 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{White, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, s]];
    graph101 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[0.5], Ball[#[[1 ;; 3]], #[[4]]]} &,
            bad]]]}, s]];
    graph001 = 
     Show[Join[{Graphics3D[
         Flatten[Map[{Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           good]]]}, {Graphics3D[
         Flatten[Map[{Red, Opacity[1], Ball[#[[1 ;; 3]], #[[4]]]} &, 
           bad]]]}, s]];];
   If[Head[sphereMeshCore`w] === Symbol, 
    UseFrontEnd[
      sphereMeshCore`w = 
       CreateWindow[
        DialogNotebook[
         DynamicModule[{optimize, sectionD = 1, 
           optionListShow2 = {2, 3}, linknumber3 = linknumber3, 
           sectionValX = {0, 1}, sectionValY = {0, 1}, 
           sectionValZ = {0, 1}, templist, VP = {2 \[Pi], 0}, 
           shiftX = 0, shiftY = 0, shiftZ = 0, shiftR = 0, 
           shiftXadd = 0, shiftYadd = 0, shiftZadd = 0, shiftRadd = 0,
            nn = 1, mm = 1, tempShiftSub = tempShift, 
           tempBallSub = tempBall, enable = True, Dx, Dy, Dz, XminF, 
           XmaxF, YminF, YmaxF, ZminF, ZmaxF, graph001 = graph001, 
           graph000 = graph000, graph010 = graph010, 
           graph011 = graph011, graph100 = graph100, 
           graph101 = graph101, graph110 = graph110, 
           graph111 = graph111, BALLSSub = BALLSAdjust, 
           boundary = {{sphereMeshCore`Xmin, 
              sphereMeshCore`Xmax}, {sphereMeshCore`Ymin, 
              sphereMeshCore`Ymax}, {sphereMeshCore`Zmin, 
              sphereMeshCore`Zmax}}}, {{XminF, XmaxF}, {YminF, 
             YmaxF}, {ZminF, ZmaxF}} = boundary;
          Dx = XmaxF - XminF; Dy = YmaxF - YminF; Dz = ZmaxF - ZminF;
          
          optimize[balls0_, ball0_] := 
           Module[{nearest0, hull, ansX, ansY, ansZ, dR}, 
            nearest0 = 
             DeleteCases[
              Nearest[balls0, ball0, 5, 
               DistanceFunction -> ((Norm[#1[[1 ;; 3]] - #2[[1 ;; 
                    3]]] - #2[[4]]) &)], ball0];
            
            hull = ConvexHullMesh[
              Append[nearest0, ball0][[1 ;; -1, 1 ;; 3]]];
            {ansX, ansY, ansZ} = 
             ArgMax[Min @@ 
               Map[(EuclideanDistance[#[[1 ;; 3]], {xx, yy, 
                    zz}] - #[[4]]) &, nearest0], {xx, yy, 
                zz} \[Element] hull];
            
            dR = Min @@ 
               Map[(EuclideanDistance[#[[1 ;; 3]], {ansX, ansY, 
                    ansZ}] - #[[4]]) &, nearest0] - ball0[[4]];
            {ansX, ansY, ansZ, ball0[[4]] + dR, 
             If[ball0[[5]] + dR > 0, ball0[[5]] + dR, 0], ball0[[6]], 
             ball0[[7]], ball0[[8]]}];
          
          Column[{Dynamic[
             Show[Which[
               MemberQ[optionListShow2, 1] && 
                MemberQ[optionListShow2, 2] && 
                MemberQ[optionListShow2, 3], 
               Show[{graph111, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
               MemberQ[optionListShow2, 1] && 
                MemberQ[optionListShow2, 
                 2] && (! MemberQ[optionListShow2, 3]), 
               Show[{graph110, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
               MemberQ[optionListShow2, 
                 1] && (! MemberQ[optionListShow2, 2]) && 
                MemberQ[optionListShow2, 3], 
               Show[{graph101, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
               MemberQ[optionListShow2, 
                 1] && (! MemberQ[optionListShow2, 2]) && (! 
                  MemberQ[optionListShow2, 3]), 
               Show[{graph100, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[
                    BALLSSub[[nn]]]}]}], (! 
                  MemberQ[optionListShow2, 1]) && 
                MemberQ[optionListShow2, 2] && 
                MemberQ[optionListShow2, 3], 
               Show[{graph011, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[
                    BALLSSub[[nn]]]}]}], (! 
               
                  MemberQ[optionListShow2, 1]) && 
                MemberQ[optionListShow2, 
                 2] && (! MemberQ[optionListShow2, 3]), 
               Show[{graph010, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[
                    BALLSSub[[nn]]]}]}], (! 
                  MemberQ[optionListShow2, 1]) && (! 
                  MemberQ[optionListShow2, 2]) && 
                MemberQ[optionListShow2, 3], 
               Show[{graph001, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[
                    BALLSSub[[nn]]]}]}], (! 
                  MemberQ[optionListShow2, 1]) && (! 
                  MemberQ[optionListShow2, 2]) && (! 
                  MemberQ[optionListShow2, 3]), 
               Show[{graph000, 
                 Graphics3D[{Green, 
                   Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}]], 
              PlotRange -> {{XminF, XminF} + 
                 sectionValX*Dx, {YminF, YminF} + 
                 sectionValY*Dy, {ZminF, ZminF} + sectionValZ*Dz}, 
              ImageSize -> {580, 580}, 
              ViewPoint -> 
               Dynamic[
                4 {Cos[VP[[1]]] Sin[VP[[2]]], 
                  Sin[VP[[1]]] Sin[VP[[2]]], Cos[VP[[2]]]}], 
              Axes -> True, 
              AxesLabel -> {Style["x", 25], Style["y", 25], 
                Style["z", 25]}]], 
            Row[{Column[{Row[{TextCell[" Section:", 12], 
                  TextCell["  ", 12], 
                  CheckboxBar[
                   Dynamic[
                    optionListShow2], {1 -> 
                    TextCell["Translucent", 12], 
                    2 -> TextCell["Show model", 12], 
                    3 -> TextCell["Show unsupported", 12]}]}], 
                IntervalSlider[Dynamic[sectionValX], 
                 Appearance -> "Paired", ImageSize -> {450, 20}], 
                IntervalSlider[Dynamic[sectionValY], 
                 Appearance -> "Paired", ImageSize -> {450, 20}], 
                IntervalSlider[Dynamic[sectionValZ], 
                 Appearance -> "Paired", ImageSize -> {450, 20}]}], 
              Slider2D[Dynamic[VP], {{2 Pi, 0}, {0, Pi}}, 
               ImageSize -> {120, 120}, Appearance -> Small]}], 
            Row[{Column[{Animator[
                 Dynamic[nn], {1, Length[BALLSSub], 1}, 
                 AnimationRunning -> False, Enabled -> enable], 
                Row[{TextCell["             the number of ball:", 12],
                   InputField[Dynamic[nn], Number, 
                   ImageSize -> {90, 18}, Enabled -> enable]}]}], 
              TextCell["  ", 12], 
              Column[{TextCell["shifting:"], 
                Row[{TextCell["\[Delta]x:", 15], 
                  InputField[Dynamic[shiftX], Number, 
                   ImageSize -> {90, 18}, Enabled -> enable]}], 
                
                Row[{TextCell["\[Delta]y:", 15], 
                  InputField[Dynamic[shiftY], Number, 
                   ImageSize -> {90, 18}, Enabled -> enable]}], 
                Row[{TextCell["\[Delta]z:", 15], 
                  InputField[Dynamic[shiftZ], Number, 
                   ImageSize -> {90, 18}, Enabled -> enable]}], 
                Row[{TextCell["\[Delta]r:", 15], 
                  InputField[Dynamic[shiftR], Number, 
                   ImageSize -> {90, 18}, Enabled -> enable]}], 
                Row[{Button[TextCell["Apply", 12], 
                   If[shiftX === Null, shiftXadd = 0, 
                    shiftXadd = shiftX];
                   
                   If[shiftY === Null, shiftYadd = 0, 
                    shiftYadd = shiftY];
                   
                   If[shiftZ === Null, shiftZadd = 0, 
                    shiftZadd = shiftZ];
                   
                   If[shiftR === Null, shiftRadd = 0, 
                    shiftRadd = shiftR];
                   number2 = nn;
                   tempShiftSub = {number2, BALLSSub[[nn]]};
                   
                   templist = 
                    BALLSSub[[nn]] + {shiftXadd, shiftYadd, shiftZadd,
                     shiftRadd, shiftRadd, 0, 0, 0};
                   If[templist[[4]] < 0, templist[[4]] = 0];
                   If[templist[[5]] < 0, templist[[5]] = 0];
                   BALLSSub[[nn]] = templist;
                   enable = False;
                   
                   linkFrontEnd = 
                    LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                   
                   LinkWrite[
                    linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                    tempShiftSub, 1, VP, nn, sectionValX, sectionValY,
                     sectionValZ, optionListShow2}];, 
                   ImageSize -> {60, 25}, Enabled -> enable], 
                  Button[TextCell["Undo", 12], 
                   If[Head[tempShiftSub] === List, 
                    BALLSSub[[tempShiftSub[[1]]]] = tempShiftSub[[2]];
                    enable = False;
                    
                    linkFrontEnd = 
                    LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                    
                    LinkWrite[
                    linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                    tempShiftSub, 2, VP, nn, sectionValX, sectionValY,
                     sectionValZ, optionListShow2}];], 
                   ImageSize -> {60, 25}, Enabled -> enable]}]}], 
              TextCell["    ", 12], 
              
              Column[{Button[TextCell["Automatic\n  attach", 12], 
                 number = nn;
                 tempBallSub = {number, BALLSSub[[nn]]};
                 enable = False;
                 BALLSSub[[nn]] = optimize[BALLSSub, BALLSSub[[nn]]];
                 
                 linkFrontEnd = 
                  LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                 
                 LinkWrite[
                  linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                   tempShiftSub, 3, VP, nn, sectionValX, sectionValY, 
                   sectionValZ, optionListShow2}];, 
                 ImageSize -> {80, 50}], 
                Button[TextCell["Undo", 12], 
                 If[Head[tempBallSub] === List, 
                  BALLSSub[[tempBallSub[[1]]]] = tempBallSub[[2]];
                  enable = False;
                  
                  linkFrontEnd = 
                   LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                  
                  LinkWrite[
                   linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                    tempShiftSub, 4, VP, nn, sectionValX, sectionValY,
                     sectionValZ, optionListShow2}];], 
                 ImageSize -> {80, 50}]}]}], 
            
            Row[{Button[TextCell["Continue"], 
               linkFrontEnd = 
                LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
               LinkWrite[linkFrontEnd, {"continue", BALLSSub}];
               DialogReturn[], ImageSize -> {90, 30}], 
              Button[TextCell["Cancel"], 
               linkFrontEnd = 
                LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
               LinkWrite[linkFrontEnd, {"finish", $Canceled}];
               DialogReturn[], ImageSize -> {90, 30}], 
              Button[TextCell["Finish"], 
               linkFrontEnd = 
                LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
               LinkWrite[linkFrontEnd, {"finish", BALLSSub}];
               DialogReturn[], ImageSize -> {90, 30}]}]}]]], 
        WindowSize -> {600, 910}, WindowTitle -> "Review", 
        WindowFrameElements -> {}]];, 
    UseFrontEnd[
      CreateWindow[
       DialogNotebook[
        DynamicModule[{optimize, sectionD = 1, 
          optionListShow2 = optionListShow2Adjust, 
          linknumber3 = linknumber3, sectionValX = sectionValXAdjust, 
          sectionValY = sectionValYAdjust, 
          sectionValZ = sectionValZAdjust, templist, VP = VPAdjust, 
          shiftX = 0, shiftY = 0, shiftZ = 0, shiftR = 0, 
          shiftXadd = 0, shiftYadd = 0, shiftZadd = 0, shiftRadd = 0, 
          nn = nAdjust, mm = 1, tempShiftSub = tempShift, 
          tempBallSub = tempBall, enable = True, Dx, Dy, Dz, XminF, 
          XmaxF, YminF, YmaxF, ZminF, ZmaxF, graph001 = graph001, 
          graph000 = graph000, graph010 = graph010, 
          graph011 = graph011, graph100 = graph100, 
          graph101 = graph101, graph110 = graph110, 
          graph111 = graph111, BALLSSub = BALLSAdjust, 
          boundary = {{sphereMeshCore`Xmin, 
             sphereMeshCore`Xmax}, {sphereMeshCore`Ymin, 
             sphereMeshCore`Ymax}, {sphereMeshCore`Zmin, 
             sphereMeshCore`Zmax}}}, {{XminF, XmaxF}, {YminF, 
            YmaxF}, {ZminF, ZmaxF}} = boundary;
         Dx = XmaxF - XminF; Dy = YmaxF - YminF; Dz = ZmaxF - ZminF;
         optimize[balls0_, ball0_] := 
          Module[{nearest0, hull, ansX, ansY, ansZ, dR}, 
           
           nearest0 = 
            DeleteCases[
             Nearest[balls0, ball0, 5, 
              DistanceFunction -> ((Norm[#1[[1 ;; 3]] - #2[[1 ;; 
                    3]]] - #2[[4]]) &)], ball0];
           
           hull = ConvexHullMesh[
             Append[nearest0, ball0][[1 ;; -1, 1 ;; 3]]];
           {ansX, ansY, ansZ} = 
            ArgMax[Min @@ 
              Map[(EuclideanDistance[#[[1 ;; 3]], {xx, yy, 
                    zz}] - #[[4]]) &, nearest0], {xx, yy, 
               zz} \[Element] hull];
           
           dR = Min @@ 
              Map[(EuclideanDistance[#[[1 ;; 3]], {ansX, ansY, 
                    ansZ}] - #[[4]]) &, nearest0] - ball0[[4]];
           {ansX, ansY, ansZ, ball0[[4]] + dR, 
            If[ball0[[5]] + dR > 0, ball0[[5]] + dR, 0], ball0[[6]], 
            ball0[[7]], ball0[[8]]}];
         Column[{Dynamic[
            Show[Which[
              MemberQ[optionListShow2, 1] && 
               MemberQ[optionListShow2, 2] && 
               MemberQ[optionListShow2, 3], 
              Show[{graph111, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
              MemberQ[optionListShow2, 1] && 
               MemberQ[optionListShow2, 
                2] && (! MemberQ[optionListShow2, 3]), 
              
              Show[{graph110, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
              MemberQ[optionListShow2, 
                1] && (! MemberQ[optionListShow2, 2]) && 
               MemberQ[optionListShow2, 3], 
              Show[{graph101, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], 
              MemberQ[optionListShow2, 
                1] && (! MemberQ[optionListShow2, 2]) && (! 
                 MemberQ[optionListShow2, 3]), 
              Show[{graph100, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], (! 
                 MemberQ[optionListShow2, 1]) && 
               MemberQ[optionListShow2, 2] && 
               MemberQ[optionListShow2, 3], 
              Show[{graph011, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], (! 
                 MemberQ[optionListShow2, 1]) && 
               MemberQ[optionListShow2, 
                2] && (! MemberQ[optionListShow2, 3]), 
              Show[{graph010, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], (! 
                 MemberQ[optionListShow2, 1]) && (! 
                 MemberQ[optionListShow2, 2]) && 
               MemberQ[optionListShow2, 3], 
              
              Show[{graph001, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}], (! 
                 MemberQ[optionListShow2, 1]) && (! 
                 MemberQ[optionListShow2, 2]) && (! 
                 MemberQ[optionListShow2, 3]), 
              Show[{graph000, 
                Graphics3D[{Green, 
                  Ball[#[[1 ;; 3]], #[[4]]] &[BALLSSub[[nn]]]}]}]], 
             PlotRange -> {{XminF, XminF} + 
                sectionValX*Dx, {YminF, YminF} + 
                sectionValY*Dy, {ZminF, ZminF} + sectionValZ*Dz}, 
             ImageSize -> {580, 580}, 
             ViewPoint -> 
              Dynamic[
               4 {Cos[VP[[1]]] Sin[VP[[2]]], 
                 Sin[VP[[1]]] Sin[VP[[2]]], Cos[VP[[2]]]}], 
             Axes -> True, 
             AxesLabel -> {Style["x", 25], Style["y", 25], 
               Style["z", 25]}]], 
           Row[{Column[{Row[{TextCell[" Section:", 12], 
                 TextCell["  ", 12], 
                 CheckboxBar[
                  Dynamic[
                   optionListShow2], {1 -> 
                    TextCell["Translucent", 12], 
                   2 -> TextCell["Show model", 12], 
                   3 -> TextCell["Show unsupported", 12]}]}], 
               IntervalSlider[Dynamic[sectionValX], 
                Appearance -> "Paired", ImageSize -> {450, 20}], 
               IntervalSlider[Dynamic[sectionValY], 
                Appearance -> "Paired", ImageSize -> {450, 20}], 
               IntervalSlider[Dynamic[sectionValZ], 
                Appearance -> "Paired", ImageSize -> {450, 20}]}], 
             Slider2D[Dynamic[VP], {{2 Pi, 0}, {0, Pi}}, 
              ImageSize -> {120, 120}, Appearance -> Small]}], 
           Row[{Column[{Animator[
                Dynamic[nn], {1, Length[BALLSSub], 1}, 
                AnimationRunning -> False, Enabled -> enable], 
               Row[{TextCell["             the number of ball:", 12], 
                 InputField[Dynamic[nn], Number, 
                  ImageSize -> {90, 18}, Enabled -> enable]}]}], 
             TextCell["  ", 12], 
             Column[{TextCell["shifting:"], 
               Row[{TextCell["\[Delta]x:", 15], 
                 InputField[Dynamic[shiftX], Number, 
                  ImageSize -> {90, 18}, Enabled -> enable]}], 
               Row[{TextCell["\[Delta]y:", 15], 
                 InputField[Dynamic[shiftY], Number, 
                  ImageSize -> {90, 18}, Enabled -> enable]}], 
               Row[{TextCell["\[Delta]z:", 15], 
                 InputField[Dynamic[shiftZ], Number, 
                  ImageSize -> {90, 18}, Enabled -> enable]}], 
               Row[{TextCell["\[Delta]r:", 15], 
                 
                 InputField[Dynamic[shiftR], Number, 
                  ImageSize -> {90, 18}, Enabled -> enable]}], 
               Row[{Button[TextCell["Apply", 12], 
                  If[shiftX === Null, shiftXadd = 0, 
                   shiftXadd = shiftX];
                  
                  If[shiftY === Null, shiftYadd = 0, 
                   shiftYadd = shiftY];
                  
                  If[shiftZ === Null, shiftZadd = 0, 
                   shiftZadd = shiftZ];
                  
                  If[shiftR === Null, shiftRadd = 0, 
                   shiftRadd = shiftR];
                  number2 = nn;
                  tempShiftSub = {number2, BALLSSub[[nn]]};
                  
                  templist = 
                   BALLSSub[[nn]] + {shiftXadd, shiftYadd, shiftZadd, 
                    shiftRadd, shiftRadd, 0, 0, 0};
                  If[templist[[4]] < 0, templist[[4]] = 0];
                  If[templist[[5]] < 0, templist[[5]] = 0];
                  BALLSSub[[nn]] = templist;
                  enable = False;
                  
                  linkFrontEnd = 
                   LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                  
                  LinkWrite[
                   linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                    tempShiftSub, 1, VP, nn, sectionValX, sectionValY,
                     sectionValZ, optionListShow2}];, 
                  ImageSize -> {60, 25}, Enabled -> enable], 
                 Button[TextCell["Undo", 12], 
                  If[Head[tempShiftSub] === List, 
                   BALLSSub[[tempShiftSub[[1]]]] = tempShiftSub[[2]];
                   enable = False;
                   
                   linkFrontEnd = 
                    LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                   
                   LinkWrite[
                    linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                    tempShiftSub, 2, VP, nn, sectionValX, sectionValY,
                     sectionValZ, optionListShow2}];], 
                  ImageSize -> {60, 25}, Enabled -> enable]}]}], 
             TextCell["    ", 12], 
             Column[{Button[TextCell["Automatic\n  attach", 12], 
                number = nn;
                tempBallSub = {number, BALLSSub[[nn]]}; enable = False;
                BALLSSub[[nn]] = optimize[BALLSSub, BALLSSub[[nn]]];
                
                linkFrontEnd = 
                 LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                
                LinkWrite[
                 linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                  tempShiftSub, 3, VP, nn, sectionValX, sectionValY, 
                  sectionValZ, optionListShow2}];, 
                ImageSize -> {80, 50}], 
               Button[TextCell["Undo", 12], 
                If[Head[tempBallSub] === List, 
                 BALLSSub[[tempBallSub[[1]]]] = tempBallSub[[2]];
                 enable = False;
                 
                 linkFrontEnd = 
                  LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
                 
                 LinkWrite[
                  linkFrontEnd, {"refresh", BALLSSub, tempBallSub, 
                   tempShiftSub, 4, VP, nn, sectionValX, sectionValY, 
                   sectionValZ, optionListShow2}];], 
                ImageSize -> {80, 50}]}]}], 
           Row[{Button[TextCell["Continue"], 
              linkFrontEnd = 
               LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
              LinkWrite[linkFrontEnd, {"continue", BALLSSub}];
              DialogReturn[], ImageSize -> {90, 30}], 
             Button[TextCell["Cancel"], 
              linkFrontEnd = 
               LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
              LinkWrite[linkFrontEnd, {"finish", $Canceled}];
              DialogReturn[], ImageSize -> {90, 30}], 
             Button[TextCell["Finish"], 
              linkFrontEnd = 
               LinkConnect[linknumber3, LinkProtocol -> "TCPIP"];
              LinkWrite[linkFrontEnd, {"finish", BALLSSub}];
              DialogReturn[], ImageSize -> {90, 30}]}]}]]], 
       sphereMeshCore`w, WindowSize -> {600, 910}, 
       WindowTitle -> "Adjust", WindowFrameElements -> {}]];];
   return = LinkRead[linkKernel];
   LinkClose[linkKernel];];
  return]