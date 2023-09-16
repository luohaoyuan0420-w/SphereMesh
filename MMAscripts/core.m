BeginPackage["sphereMeshCore`",{"JLink`"}] 

wrongdata = Null;rudRfield;stl;
 numberOfPionts;rf; a;BALLS;
s1 = 0; s2 := numberOfPionts; s3 = 0; s4 = 0; s5 = 0;
SetSharedVariable[s4, content];numberOfSupportings;
choiceImprf = True; choiceImpstl = True; choiceImpTXT = True; choiceImpWDX = True ;BALLSTEMP;
BALLS = {};SUPPORTINGS = {};Xmax;Xmin;Xmaxt;Xmint;Ymax;Ymin;Ymaxt;Ymint;Zmax;Zmin;Zmaxt;Zmint;
sys; finish = 1;
sphereMeshCore`spheremeshpath = Global`SphereMeshPath;problemQ=False;

messageWindowLHY[contentOfWindow_, title_, {m_, n_}] := 
 Module[{returnAns}, problemLink = LinkCreate[LinkProtocol -> "TCPIP"];
  linknumber2 = StringSplit[problemLink[[1]], {"@"}][[1]];
  UseFrontEnd[
   CreateWindow[
    DialogNotebook[
     DynamicModule[{linknumber2 = linknumber2}, 
      Column[{contentOfWindow, 
        Row[{Button["Ok", 
           problemLink = 
            LinkConnect[linknumber2, LinkProtocol -> "TCPIP"]; 
           LinkWrite[problemLink, True];
           DialogReturn[], ImageSize -> {80, 40}], 
          Button["Cancel", 
           problemLink = 
            LinkConnect[linknumber2, LinkProtocol -> "TCPIP"]; 
           LinkWrite[problemLink, False];
           DialogReturn[], ImageSize -> {80, 40}]}]}]]], 
    WindowSize -> {m, n}, WindowTitle -> title, 
    WindowFrameElements -> {}]];
  problemQ = True;
  returnAns = LinkRead[problemLink];
  LinkClose[problemLink];
  returnAns]

updateB[] := 
 Module[{}, 
  If[MatchQ[Xmaxt, _Symbol], Xmaxt = Xmax, Xmaxt = Max[Xmaxt, Xmax]];
  If[MatchQ[Ymaxt, _Symbol], Ymaxt = Ymax, Ymaxt = Max[Ymaxt, Ymax]];
  If[MatchQ[Zmaxt, _Symbol], Zmaxt = Zmax, Zmaxt = Max[Zmaxt, Zmax]];
  If[MatchQ[Xmint, _Symbol], Xmint = Xmin, Xmint = Min[Xmint, Xmin]];
  If[MatchQ[Ymint, _Symbol], Ymint = Ymin, Ymint = Min[Ymint, Ymin]];
  If[MatchQ[Zmint, _Symbol], Zmint = Zmin, Zmint = Min[Zmint, Zmin]];];

updateB2[] := Module[{},
  Xmax = Xmaxt;
  Ymax = Ymaxt;
  Zmax = Zmaxt;
  Xmin = Xmint;
  Ymin = Ymint;
  Zmin = Zmint;
  ];

imprf[fL_String] := 
  Quiet[Module[{out}, 
    UseFrontEnd[
     w = CreateDialog[
       ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate, 
        ImageSize -> {380, 30}], 
       WindowTitle -> "creating radius field", 
       WindowSize -> {400, 40}, WindowFrameElements -> {}]];
    out = 1; wrongdata = Null;
    If[MatchQ[Check[rudRfield = Import[fL, "Data"], "f"], _List], 
     wrongdata = Catch[If[Depth[rudRfield] == 2, Throw[{-1}]];
       For[i = 1, i <= Length[rudRfield], i++, 
        For[j = 1, j <= 8, j++, 
         If[! (Check[(MatchQ[rudRfield[[i, j]], _Integer] || 
               MatchQ[rudRfield[[i, j]], _Real]), False]), 
          Throw[{i, j}];]];
        If[! (rudRfield[[i, 4]] >= rudRfield[[i, 5]] >= 0), 
         Throw[{i}]];
        If[! (Check[rudRfield[[i, 6]] >= 0, False]), Throw[{i}]];
        If[! (Check[rudRfield[[i, 7]] > 0, False]), Throw[{i}]];
        If[! (Check[rudRfield[[i, 8]] > 0, False]), Throw[{i}]];]];
     UseFrontEnd[NotebookClose[w]];
     If[Length[wrongdata] != 0, UseFrontEnd[NotebookClose[w]];
      Which[wrongdata[[1]] == -1, 
       choiceImprf =	 
        messageWindowLHY[TextCell["Wrong format\n", 24], 
         "importing radius field", {300, 120}], 
       Length[wrongdata] == 2, 
       choiceImprf = 
        messageWindowLHY[
         TextCell[
          "Wrong at line " <> ToString[wrongdata[[1]]] <> " number " <>
            ToString[wrongdata[[2]]]<>"\n", 24], 
         "importing radius field", {400, 120}], 
       Length[wrongdata] == 1, 
       choiceImprf = 
        messageWindowLHY[
         TextCell["Wrong at line " <> ToString[wrongdata[[1]]]<>"\n", 24], 
         "importing radius field", {400, 120}]];
      0, 1], UseFrontEnd[NotebookClose[w]];
     choiceImprf = 
      messageWindowLHY[TextCell["Cannot find the file\n", 24], 
       "importing radius field", {300, 120}];
     0]]];


impstl[stlL_String] := 
  Quiet[Module[{coord}, 
    UseFrontEnd[
     w = CreateDialog[
       ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate, 
        ImageSize -> {380, 30}], WindowTitle -> "importing model", 
       WindowSize -> {400, 40}, WindowFrameElements -> {}]];
    If[MatchQ[
      Check[stl = Import[stlL, "BoundaryMeshRegion"], 
       "f"], _BoundaryMeshRegion], coord = MeshCoordinates[stl];
     Xmax = Max[Flatten[coord[[1 ;; -1, 1]]]];
     Xmin = Min[Flatten[coord[[1 ;; -1, 1]]]];
     Ymax = Max[Flatten[coord[[1 ;; -1, 2]]]];
     Ymin = Min[Flatten[coord[[1 ;; -1, 2]]]];
     Zmax = Max[Flatten[coord[[1 ;; -1, 3]]]];
     Zmin = Min[Flatten[coord[[1 ;; -1, 3]]]];
     updateB[];
     dx = (Xmax - Xmin); dy = (Ymax - Ymin); dz = (Zmax - Zmin);
     Xmax = Xmax + 0.01 dx; Xmin = Xmin - 0.01 dx;
     Ymax = Ymax + 0.01 dy; Ymin = Ymin - 0.01 dy;
     Zmax = Zmax + 0.01 dz; Zmin = Zmin - 0.01 dz;
     UseFrontEnd[NotebookClose[w]];
     1, UseFrontEnd[NotebookClose[w]];
     choiceImpstl = 
      messageWindowLHY[TextCell["Cannot import the model\n", 24], 
       "importing model", {350, 120}];
     0]]];


impTXT[fL_String] := 
  Quiet[Module[{out}, 
    UseFrontEnd[
     w = CreateDialog[
       ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate, 
        ImageSize -> {380, 30}], WindowTitle -> "Reading TXT", 
       WindowSize -> {400, 40}, WindowFrameElements -> {}]];
    out = 1; wrongdata = Null;
    If[MatchQ[Check[txtdata = Import[fL, "Data"], "f"], _List], 
     wrongdata = Catch[If[Depth[txtdata] == 2, Throw[{-1}]];
       For[i = 1, i <= Length[txtdata], i++, 
        For[j = 1, j <= 8, j++, 
         If[! (Check[(MatchQ[txtdata[[i, j]], _Integer] || 
               MatchQ[txtdata[[i, j]], _Real]), False]), 
          Throw[{i, j}];]];
        If[! (txtdata[[i, 4]] >= txtdata[[i, 5]] >= 0), Throw[{i}]];
        If[! (Check[txtdata[[i, 6]] >= 0, False]), Throw[{i}]];
        If[! (Check[txtdata[[i, 7]] > 0, False]), Throw[{i}]];
        If[! (Check[txtdata[[i, 8]] > 0, False]), Throw[{i}]];]];
     UseFrontEnd[NotebookClose[w]];
     If[Length[wrongdata] != 0, UseFrontEnd[NotebookClose[w]];
      Which[wrongdata[[1]] == -1, 
       choiceImpTXT = 
        messageWindowLHY[TextCell["Wrong format\n", 24], 
         "Reading TXT", {300, 120}], Length[wrongdata] == 2, 
       choiceImpTXT = 
        messageWindowLHY[
         TextCell[
          "Wrong at line " <> ToString[wrongdata[[1]]] <> " number " <>
            ToString[wrongdata[[2]]]<>"\n", 24], "Reading TXT", {400, 120}],
        Length[wrongdata] == 1, 
       choiceImpTXT= 
        messageWindowLHY[
         TextCell["Wrong at line " <> ToString[wrongdata[[1]]]<>"\n", 24], 
         "Reading TXT", {400, 120}]];
      0, BALLS = txtdata; 1], UseFrontEnd[NotebookClose[w]];
     choiceImpTXT = 
      messageWindowLHY[TextCell["Cannot find the file\n", 24], 
       "Reading TXT", {300, 120}];
     0]]];

impWDX[WDXPath_String] := Module[{in}, Check[in = Import[WDXPath];
    If[! (Check[in[[5]] === "OKAY", False]), Goto["wrong"]]; 
    SUPPORTINGS = DeleteDuplicates[in[[3]]];
    {Xmax, Xmin, Ymax, Ymin, Zmax, Zmin} = in[[4]];
    BALLS = SUPPORTINGS[[1 ;; -1, 1,1;;8]];
    , Label["wrong"];
    choiceImpWDX = 
     messageWindowLHY[TextCell["Corrupted file\n", 24], 
      "Reading WDX", {300, 120}]
    ]];

createrf[] := 
  Module[{p, d, list, out, Nx, Ny, Nz, nearestf, Nmax, rud2, dxh, dyh,
     dzh, Xmaxh, Xminh, Ymaxh, Yminh, Zmaxh, Zminh, t, toShow, 
    toShowtemp}, 
   If[(numberOfPionts > 0 && IntegerQ[numberOfPionts]), 
    UseFrontEnd[
     w = CreateDialog[
       ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate, 
        ImageSize -> {380, 30}], 
       WindowTitle -> "creating radius field", 
       WindowSize -> {400, 40}, WindowFrameElements -> {}]];
    rudRfield = 
     DeleteDuplicates[
      rudRfield, ((#1[[1]] == #2[[1]]) && (#1[[2]] == #2[[
            2]]) && (#1[[3]] == #2[[3]])) &];
    rud2 = Flatten[rudRfield, {{2}, {1}}];
    Xmaxh = Max[rud2[[1]]];
    Xminh = Min[rud2[[1]]];
    Ymaxh = Max[rud2[[2]]];
    Yminh = Min[rud2[[2]]];
    Zmaxh = Max[rud2[[3]]];
    Zminh = Min[rud2[[3]]];
    Xmaxh = Max[Xmax, Xmaxh];
    Ymaxh = Max[Ymax, Ymaxh];
    Zmaxh = Max[Zmax, Zmaxh];
    Xminh = Min[Xmin, Xminh];
    Yminh = Min[Ymin, Yminh];
    Zminh = Min[Zmin, Zminh];
    dxh = (Xmaxh - Xminh); dyh = (Ymaxh - Yminh);
    dzh = (Zmaxh - Zminh);
    Xmaxh = Xmaxh + 0.01 dxh; Xminh = Xminh - 0.01 dxh;
    Ymaxh = Ymaxh + 0.01 dyh; Yminh = Yminh - 0.01 dyh;
    Zmaxh = Zmaxh + 0.01 dzh; Zminh = Zminh - 0.01 dzh;
    d = N[(dxh dyh dzh/numberOfPionts)^(1/3)];
    Nx = Floor[(Xmaxh - Xminh)/d] + 1;
    Ny = Floor[(Ymaxh - Yminh)/d] + 1;
    Nz = Floor[(Zmaxh - Zminh)/d] + 1;
    list = Table[{}, Nx, Ny, Nz];
    Nmax = Max[Nx, Ny, Nz];
    p = RandomPoint[stl, numberOfPionts];
    Scan[Module[{cx, cy, cz}, cx = Floor[(#[[1]] - Xminh)/d] + 1;
       cy = Floor[(#[[2]] - Yminh)/d] + 1;
       cz = Floor[(#[[3]] - Zminh)/d] + 1;
       AppendTo[list[[cx, cy, cz]], #];] &, rudRfield];
    nearestf[m_, c_] := 
     Module[{rangex1, rangex2, rangey1, rangey2, rangez1, rangez2, cx,
        cy, cz, chosen, outn}, cx = Floor[(c[[1]] - Xminh)/d] + 1;
      cy = Floor[(c[[2]] - Yminh)/d] + 1;
      cz = Floor[(c[[3]] - Zminh)/d] + 1;
      Do[rangex1 = If[(cx - n) >= 1, cx - n, 1];
       rangex2 = If[(cx + n) <= Nx, cx + n, Nx];
       rangey1 = If[(cy - n) >= 1, cy - n, 1];
       rangey2 = If[(cy + n) <= Ny, cy + n, Ny];
       rangez1 = If[(cz - n) >= 1, cz - n, 1];
       rangez2 = If[(cz + n) <= Nz, cz + n, Nz];
       chosen = 
        Flatten[list[[rangex1 ;; rangex2, rangey1 ;; rangey2, 
          rangez1 ;; rangez2]], 3];
       If[Length[chosen] >= m, 
        outn = Nearest[DeleteCases[chosen, c], c, m, 
          DistanceFunction -> 
           Function[{nn1, nn2}, Norm[nn2[[1 ;; 3]] - nn1]]];
        Break[]], {n, 1, Nmax}];
      outn];(*{number of nearest points,point}*)out = {};
    s4 = Length[out];
    UseFrontEnd[NotebookClose[w]];
    UseFrontEnd[
     w = CreateDialog[
       Column[{TextCell[
          Style["Generating random points:", Gray, 20, Bold]], 
         ProgressIndicator[0, ImageSize -> {380, 30}]}], 
       WindowTitle -> "creating radius field", 
       WindowSize -> {400, 80}]];
    toShow = 0;
    out = 
     Map[Module[{nearest}, s4++; toShowtemp = N[s4/numberOfPionts];
        If[toShow + 0.001 < toShowtemp, toShow = toShowtemp;
         UseFrontEnd[
          CreateDialog[
           Column[{TextCell[
              Style["Generating random points:", Gray, 20, Bold]], 
             ProgressIndicator[toShow, ImageSize -> {380, 30}]}], w, 
           WindowTitle -> "creating radius field", 
           WindowSize -> {400, 80}]]];
        Pause[0.01];
        nearest = Flatten[nearestf[1, #]];
        {Sequence @@ #, nearest[[4]], nearest[[5]], nearest[[6]], 
         nearest[[7]], nearest[[8]]}] &, p];
    rf = Map[Module[{r1p, r2p, nhole, rhole, rsupport}, r1p = #[[4]];
        r2p = #[[5]];
        nhole = #[[6]];
        rhole = #[[7]];
        rsupport = #[[8]];
        If[r1p < 0, r1p = 0];
        If[r2p < 0, r2p = 0];
        nhole = Floor[nhole];
        If[nhole < 0, nhole = 0];
        If[rhole < 0, rhole = 0; nhole = 0;];
        If[rsupport < 0, rsupport = 0]; {#[[1]], #[[2]], #[[3]], r1p, 
         r2p, nhole, rhole, rsupport}] &, out];
    UseFrontEnd[NotebookClose[w]];
    SetDirectory[FileNameJoin[{spheremeshpath, "temp"}]];
    Export["RandomPoints.wdx", out], 
    MessageDialog["Failed", WindowTitle -> "creating radius field"];]];

createrf2[BALLSS_] := 
  Module[{p, d, list, out, Nx, Ny, Nz, nearestf, Nmax, rud2, dxh, dyh,
     dzh, Xmaxh, Xminh, Ymaxh, Yminh, Zmaxh, Zminh, t, toShow, 
    toShowtemp, BALLSLength, xP, yP, zP, rP}, 
   If[(numberOfPionts > 0 && IntegerQ[numberOfPionts]), 
    UseFrontEnd[
     w = CreateDialog[
       ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate, 
        ImageSize -> {380, 30}], 
       WindowTitle -> "creating radius field", 
       WindowSize -> {400, 40}, WindowFrameElements -> {}]];
    rudRfield = 
     DeleteDuplicates[
      rudRfield, ((#1[[1]] == #2[[1]]) && (#1[[2]] == #2[[2]]) && \
(#1[[3]] == #2[[3]])) &];
    rud2 = Flatten[rudRfield, {{2}, {1}}];
    Xmaxh = Max[rud2[[1]]];
    Xminh = Min[rud2[[1]]];
    Ymaxh = Max[rud2[[2]]];
    Yminh = Min[rud2[[2]]];
    Zmaxh = Max[rud2[[3]]];
    Zminh = Min[rud2[[3]]];
    Xmaxh = Max[Xmax, Xmaxh];
    Ymaxh = Max[Ymax, Ymaxh];
    Zmaxh = Max[Zmax, Zmaxh];
    Xminh = Min[Xmin, Xminh];
    Yminh = Min[Ymin, Yminh];
    Zminh = Min[Zmin, Zminh];
    dxh = (Xmaxh - Xminh); dyh = (Ymaxh - Yminh);
    dzh = (Zmaxh - Zminh);
    Xmaxh = Xmaxh + 0.01 dxh; Xminh = Xminh - 0.01 dxh;
    Ymaxh = Ymaxh + 0.01 dyh; Yminh = Yminh - 0.01 dyh;
    Zmaxh = Zmaxh + 0.01 dzh; Zminh = Zminh - 0.01 dzh;
    d = N[(dxh dyh dzh/numberOfPionts)^(1/3)];
    Nx = Floor[(Xmaxh - Xminh)/d] + 1;
    Ny = Floor[(Ymaxh - Yminh)/d] + 1;
    Nz = Floor[(Zmaxh - Zminh)/d] + 1;
    list = Table[{}, Nx, Ny, Nz];
    Nmax = Max[Nx, Ny, Nz];
    p = RandomPoint[stl, numberOfPionts];
    BALLSLength = Length[BALLSS];
    Do[
     xP = BALLSS[[c, 1]]; yP = BALLSS[[c, 2]]; zP = BALLSS[[c, 3]]; 
     rP = BALLSS[[c, 4]];
     p = Select[p, (
         EuclideanDistance[{#[[1]], #[[2]], #[[3]]}, {xP, yP, zP}] > 
          rP) &], {c, 1, BALLSLength}];
    p = NestWhile[Function[{P},
       Module[{tempp}, 
        tempp = RandomPoint[stl, numberOfPionts]; Do[
         xP = BALLSS[[c, 1]]; yP = BALLSS[[c, 2]]; 
         zP = BALLSS[[c, 3]]; rP = BALLSS[[c, 4]];
         tempp = Select[tempp, (             
             EuclideanDistance[{#[[1]], #[[2]], #[[3]]}, {xP, yP, 
                zP}] > rP) &], {c, 1, BALLSLength}]; Join[P, tempp]]],
       p, (Length[#] < numberOfPionts) &];
    Scan[Module[{cx, cy, cz}, cx = Floor[(#[[1]] - Xminh)/d] + 1;
       cy = Floor[(#[[2]] - Yminh)/d] + 1;
       cz = Floor[(#[[3]] - Zminh)/d] + 1;
       AppendTo[list[[cx, cy, cz]], #];] &, rudRfield];
    nearestf[m_, c_] := 
     Module[{rangex1, rangex2, rangey1, rangey2, rangez1, rangez2, cx,
        cy, cz, chosen, outn}, cx = Floor[(c[[1]] - Xminh)/d] + 1;
      cy = Floor[(c[[2]] - Yminh)/d] + 1;
      cz = Floor[(c[[3]] - Zminh)/d] + 1;
      Do[rangex1 = If[(cx - n) >= 1, cx - n, 1];
       rangex2 = If[(cx + n) <= Nx, cx + n, Nx];
       rangey1 = If[(cy - n) >= 1, cy - n, 1];
       rangey2 = If[(cy + n) <= Ny, cy + n, Ny];
       rangez1 = If[(cz - n) >= 1, cz - n, 1];
       rangez2 = If[(cz + n) <= Nz, cz + n, Nz];
       chosen = 
        Flatten[list[[rangex1 ;; rangex2, rangey1 ;; rangey2, 
           rangez1 ;; rangez2]], 3];
       If[Length[chosen] >= m, 
        outn = Nearest[DeleteCases[chosen, c], c, m, 
          DistanceFunction -> 
           Function[{nn1, nn2}, Norm[nn2[[1 ;; 3]] - nn1]]];
        Break[]], {n, 1, Nmax}];
      outn];(*{number of nearest points,point}*)out = {};
    s4 = Length[out];
    UseFrontEnd[NotebookClose[w]];
    UseFrontEnd[
     w = CreateDialog[
       Column[{TextCell[
          Style["Generating random points:", Gray, 20, Bold]], 
         ProgressIndicator[0, ImageSize -> {380, 30}]}], 
       WindowTitle -> "creating radius field", 
       WindowSize -> {400, 80}]];
    toShow = 0;
    out = 
     Map[Module[{nearest}, s4++; toShowtemp = N[s4/numberOfPionts];
        If[toShow + 0.001 < toShowtemp, toShow = toShowtemp;
         UseFrontEnd[
          CreateDialog[
           Column[{TextCell[
              Style["Generating random points:", Gray, 20, Bold]], 
             ProgressIndicator[toShow, ImageSize -> {380, 30}]}], w, 
           WindowTitle -> "creating radius field", 
           WindowSize -> {400, 80}]]];
        Pause[0.01];
        nearest = Flatten[nearestf[1, #]];
        {Sequence @@ #, nearest[[4]], nearest[[5]], nearest[[6]], 
         nearest[[7]], nearest[[8]]}] &, p];
    rf = Map[Module[{r1p, r2p, nhole, rhole, rsupport}, r1p = #[[4]];
        r2p = #[[5]];
        nhole = #[[6]];
        rhole = #[[7]];
        rsupport = #[[8]];
        If[r1p < 0, r1p = 0];
        If[r2p < 0, r2p = 0];
        nhole = Floor[nhole];
        If[nhole < 0, nhole = 0];
        If[rhole < 0, rhole = 0; nhole = 0;];
        If[rsupport < 0, rsupport = 0]; {#[[1]], #[[2]], #[[3]], r1p, 
         r2p, nhole, rhole, rsupport}] &, out];
    UseFrontEnd[NotebookClose[w]];
    SetDirectory[FileNameJoin[{spheremeshpath, "temp"}]];
    Export["RandomPoints.wdx", out], 
    MessageDialog["Failed", WindowTitle -> "creating radius field"];]];

 inipoint[ball_, points_, n_] := 
  If[n > 0, 
   Parallelize[
    Select[points, 
     Module[{rt = ball[[4]] + #[[4]], 
        d = ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5}, d < rt] &]], 
   Parallelize[  Select[points,   Module[{rt = (ball[[4]] + #[[4]])*(1 - n),  d = ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5}, d < rt] &]]];

trimpoint[points_, ball_, n_] := 
  If[n > 0, 
   Select[points, ((1 -  n) (ball[[4]] + #[[4]]) < ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5) &], 
   Select[points, ((ball[[4]] + #[[4]]) < ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5) &]];

decidepoint[balls_] := 
  Module[{T, T1, T2, m, t}, 
   T = Tally[
     Flatten[Table[balls[[i]][[2]], {i, 1, Length[balls]}], 1]];
   T2 = Table[T[[i, 2]], {i, 1, Length[T]}];
   T1 = Table[T[[i, 1]], {i, 1, Length[T]}];
   m = Max[T2];
   t = Sort[
     Map[T1[[#]] &, Flatten[Position[T2, m]]], (#2[[4]] > #1[[4]]) &];
   s1 = Length[t];
   If[Length[t] > 0, t[[1]], {}]];

clearpoint[balls_, ball_, n_] := 
  If[n > 0, 
   Map[Module[{con = #}, {con[[1]], 
       Select[con[[2]], ((1 - n) (#[[4]] + 
              ball[[4]]) < ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5) &]}] &, balls], 
   Map[Module[{con = #}, {con[[1]], 
       Select[con[[2]], ((#[[4]] + all[[4]]) < ((ball[[1]] - #[[1]])^2 + (ball[[2]] - #[[2]])^2 + (ball[[3]] - #[[3]])^2)^0.5) &]}] &, balls]];

b[p_, n_] := 
  If[And @@ Map[(#[[4]] >= 0) &, p], 
   UseFrontEnd[
    w = CreateDialog[
      Column[{Column[{TextCell[
           Style["number of balls: " <> ToString[s3], Gray, 15, 
            Bold]], 
          TextCell[
           Style["number of possible configurations: " <> 
             ToString[s1], Gray, 15, Bold]]}], 
        ProgressIndicator[1 - s2/numberOfPionts, 
         ImageSize -> {380, 30}]}], 
      WindowTitle -> "creating sphere mesh", WindowSize -> {400, 100},
       WindowFrameElements -> {}]];
   Module[{ans, out}, 
    ans = NestWhile[
       Module[{ball = decidepoint[#[[1]]], trm}, 
         If[ball != {}, trm = trimpoint[#[[2]], ball, n]];
         s2 = Length[trm];
         s3 = Length[#[[1]]];
         UseFrontEnd[
          CreateDialog[
           Column[{Column[{TextCell[
                Style["number of balls: " <> ToString[s3], Gray, 15, 
                 Bold]], 
               TextCell[
                Style["number of possible configurations: " <> 
                  ToString[s1], Gray, 15, Bold]]}], 
             ProgressIndicator[1 - s2/numberOfPionts, 
              ImageSize -> {380, 30}]}], w, 
           WindowTitle -> "creating sphere mesh", 
           WindowSize -> {400, 100}, WindowFrameElements -> {}]];
         If[
          ball != {}, {Append[
            clearpoint[#[[1]], ball, n], {ball, 
             inipoint[ball, trm, n]}], trm, True}, {#[[1]], #[[2]], 
           False}]] &, 
       Block[{ball = p[[RandomInteger[{1, Length[p]}]]], pp}, 
        pp = trimpoint[p, ball, n]; {{{ball, inipoint[ball, pp, n]}}, 
         pp, True}], (#[[3]] == True) &][[1]];
    out = Select[Flatten[ans, 1], (# != {}) &];
    UseFrontEnd[NotebookClose[w]];
    SetDirectory[FileNameJoin[{spheremeshpath, "temp"}]];
    Export["balls.wdx", out];
    out], Print["radius field invalid"]];

refill[po_, BALLSS_, n_] := 
  Module[{newpo, BandR, balls, ans, out, len, trm, ball},
s1 = 0; s2 := numberOfPionts; s3 = 0; s4 = 0; s5 = 0;
   UseFrontEnd[
    w = CreateDialog[
      Column[{Column[{TextCell[
           Style["number of balls: " <> ToString[s3], Gray, 15, 
            Bold]], 
          TextCell[
           Style["number of possible configurations: " <> 
             ToString[s1], Gray, 15, Bold]]}], 
        ProgressIndicator[1 - s2/numberOfPionts, 
         ImageSize -> {380, 30}]}], 
      WindowTitle -> "creating sphere mesh", WindowSize -> {400, 100},
       WindowFrameElements -> {}]];
   len = Length[BALLSS];
   newpo = Fold[trimpoint[#1, #2, n] &, po, BALLSS];
   BandR = Map[{#, inipoint[#, newpo, n]} &, BALLSS];
   trm = newpo;
   While[Length[trm] != 0;
    BandR = NestWhile[Module[{}, ball = decidepoint[#[[1]]];
         If[ball != {}, trm = trimpoint[#[[2]], ball, n]];
         s2 = Length[trm];
         s3 = Length[#[[1]]] - len;
         UseFrontEnd[
          CreateDialog[
           Column[{Column[{TextCell[
                Style["number of balls: " <> ToString[s3], Gray, 15, 
                 Bold]], 
               TextCell[
                Style["number of possible configurations: " <> 
                  ToString[s1], Gray, 15, Bold]]}], 
             ProgressIndicator[1 - s2/numberOfPionts, 
              ImageSize -> {380, 30}]}], w, 
           WindowTitle -> "creating sphere mesh", 
           WindowSize -> {400, 100}, WindowFrameElements -> {}]];
         If[
          ball != {}, {Append[
            clearpoint[#[[1]], ball, n], {ball, 
             inipoint[ball, trm, n]}], trm, True}, {#[[1]], #[[2]], 
           False}]] &, {BandR, newpo, True}, (#[[3]] == True) &][[1]];
    If[Length[trm] != 0, ball = First[trm];
     trm = trimpoint[trm, ball, n], Break[]];
    BandR = Append[BandR, {ball, inipoint[ball, trm, n]}]];
   out = BandR[[1 ;; Length[BandR], 1]];
   UseFrontEnd[NotebookClose[w]];
   SetDirectory[FileNameJoin[{spheremeshpath, "temp"}]];
   Export["BallsRefilled.wdx", out];
   out];

 addsupportingsArch[{x1_, x2_, y1_, y2_, z1_, z2_}, {xdiv_, ydiv_, zdiv_}, balls_, step_, num_] := 
  Module[{lists, lx, ly, lz, balls2, supportings, supportingsback, ballsnum},
   s5 = 0;  UseFrontEnd[
    w0 = CreateWindow[
      DialogNotebook[
       ProgressIndicator[s5/(Length[balls]), ImageSize -> {380, 30}], 
       WindowSize -> {400, 40}, 
       WindowTitle -> "generating supportings"]]];
   ballsnum = balls;
   Do[AppendTo[ballsnum[[i]], i], {i, 1, Length[balls]}];
   lx = (x2 - x1)/xdiv; ly = (y2 - y1)/ydiv; lz = (z2 - z1)/zdiv;
   list = Table[{}, xdiv + 1, ydiv + 1, zdiv + 1];
   balls2 = Map[Module[{nx, ny, nz}, nx = Floor[(#[[1]] - x1)/lx];
       ny = Floor[(#[[2]] - y1)/ly];
       nz = Floor[(#[[3]] - z1)/lz];
       AppendTo[list[[nx + 1, ny + 1, nz + 1]], #];
       {#[[1]], #[[2]], #[[3]], #[[4]], nx, ny, 
        nz, #[[9]], #[[5]], #[[6]], #[[7]], #[[8]]}] &, ballsnum];
   (*Therefore the elements of balls2 here are in the form of {x,y,z,
   r,cx,cy,cz,number}*)supportings = {};
   Scan[Module[{length = 0, templength = 1, temp, rangex1, rangex2, 
       rangey1, rangey2, rangez1, rangez2, x = #[[1]], y = #[[2]], 
       z = #[[3]], radius = #[[4]], order = #[[8]], out, tempv, 
       tempdot, p1, p, p3, sample, v, radius2, THETA, PHI, arch}, 
      out = {};
      temp = Max[xdiv, ydiv, zdiv]/step;
      Do[If[templength != length, 
         rangex1 = If[(#[[5]] - n step) >= 1, #[[5]] - n step, 1];
         rangex2 = If[(#[[5]] + n step) <= xdiv, #[[5]] + n step, xdiv];
         rangey1 = If[(#[[6]] - n step) >= 1, #[[6]] - n step, 1];
         rangey2 = If[(#[[6]] + n step) <= ydiv, #[[6]] + n step, ydiv];
         rangez1 = If[(#[[7]] - n step) >= 1, #[[7]] - n step, 1]; 
         rangez2 = #[[7]];
         sample = 
          Select[RandomSample[
            Flatten[
             list[[Range[rangex1, rangex2], Range[rangey1, rangey2], 
               Range[rangez1, rangez2]]], 
             3]], ((#[[1]] != x) || (#[[2]] != y) || (#[[3]] != z)) &];
         p3 = {x, y, z - radius};
         Scan[(If[
             RegionMember[Ball[{#[[1]], #[[2]], #[[3]]}, #[[4]]], p3],
              out={};AppendTo[out, {{0, 0, 0}, 0, 0, 0, #[[5]]}];
             Break[]];            
            If[((tempdot = 
                  Dot[v = {#[[1]], #[[2]], #[[3]] - #[[4]]} - {x, y, 
                    z - radius};
                   tempv = Normalize[v], {0, 0, -1}]) >= 
                0.7071067811865475) && (radius2 = 
                1/(2 tempdot) (Sqrt[v[[1]]^2 + v[[2]]^2 + v[[3]]^2] - 2 #[[4]] tempdot);               
               p1 = {x, y,  z - radius - radius2};(*the center of the arch*)
               THETA = Pi - 2 ArcCos[tempdot];               
               And @@ Map[(((x - #[[1]])^2 + (y - #[[2]])^2 + (z - 
                    radius - #[[3]])^2) >= #[[4]]^2) &, 
                 sample]) && (PHI = 
                If[Sqrt[v[[1]]^2 + v[[2]]^2] > 0, 
                 If[v[[2]] > 0, 
                  ArcCos[v[[1]]/Sqrt[v[[1]]^2 + v[[2]]^2]], -ArcCos[
                    v[[1]]/Sqrt[v[[1]]^2 + v[[2]]^2]]], 0];               
               arch = Line[
                 Table[radius2 {Sin[a] Cos[PHI], Sin[a] Sin[PHI], 
                    Cos[a]} + p1, {a, 0, THETA, Pi/10}]];               
               And @@ Map[(RegionDistance[
                    arch, {#[[1]], #[[2]], #[[3]]}] >= #[[4]]) &, 
                 sample]) && (! 
                MemberQ[
                 out, {p1, radius2, PHI, 
                  THETA, #[[9]]}]) && (radius2 >= #[[-2]]), 
             AppendTo[out, {p1, radius2, PHI, THETA, #[[9]]}];
             If[Length[out] >= num, Break[]]]) &, sample];
         templength = length;
         length = (rangex2 - rangex1 + 1) (rangey2 - rangey1 + 
             1) (rangez2 - rangez1 + 1), Break[]];, {n, 1, temp + 1}];
      AppendTo[
       supportings, {{#[[1]], #[[2]], #[[3]], #[[4]], #[[9]], #[[10]], #[[11]], #[[12]], #[[8]]}, out}];
      s5 = Length[supportings];
      UseFrontEnd[
       CreateWindow[
        DialogNotebook[
         ProgressIndicator[s5/(Length[balls]), 
          ImageSize -> {380, 30}], WindowSize -> {400, 40}, 
         WindowTitle -> "generating supportings"], w0]]
      (*{{x,y,z,r1,r2,nhole,rhole,rsupporting,
      number},{{{center of arch},radius2,PHI,THETA,
      number of the ball attached to}...}}*)] &, balls2];
   SetDirectory[FileNameJoin[{spheremeshpath, "temp"}]];
   Export["supportings.wdx", out];
   UseFrontEnd[NotebookClose[w0]];
   supportings];

EndPackage[]