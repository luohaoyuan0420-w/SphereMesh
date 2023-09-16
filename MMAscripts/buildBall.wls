#!/usr/bin/env wolframscript

type;  suppAndBall; data; POINTS;   near = {};
SetSharedVariable[listtan];
const = 0.6180339887498949;
sort[] := Module[{supportings, temp, out, i},
type=1;
   SUPPORTINGS = 
    Map[{#[[1]], Select[#[[2]], Function[{var}, var[[-2]] != 0]]} &, 
     SUPPORTINGS];
   BALLS = SUPPORTINGS[[1 ;; -1, 1, 1 ;; 8]];
   data = {{}, BALLS, SUPPORTINGS, {}, {}};
   supportings = {};
   Scan[(temp = Join[supportings, #[[2]]]; supportings = temp) &, 
    data[[3]]];
   out = {};
   Scan[AppendTo[out, {#[[1]], {}}] &, data[[3]]];
   out = Sort[out, (#1[[1, -1]] < #2[[1, -1]]) &];
   Scan[AppendTo[out[[#[[-1]], 2]], #] &, supportings];
   suppAndBall = out;
   BALLS = Map[#[[1]] &, data[[3]]];
   BALLS = Sort[BALLS, (#1[[-1]] < #2[[-1]]) &]];

findNear[] := 
  Module[{XmaxRR, XminRR, YmaxRR, YminRR, ZmaxRR, ZminRR, dxRR, dyRR, 
    dzRR, Rmax, cellnum1d, tx, ty, tz, txn, tyn, tzn, tdxRR, tdyRR, 
    tdzRR, NXX, NYY, 
    NZZ}, {XmaxRR, XminRR, YmaxRR, YminRR, ZmaxRR, ZminRR} = {0, 0, 0,
      0, 0, 0};
   Scan[(If[#[[1, 1]] > XmaxRR, XmaxRR = #[[1, 1]]];
      If[#[[1, 1]] < XminRR, XminRR = #[[1, 1]]];
      If[#[[1, 2]] > YmaxRR, YmaxRR = #[[1, 2]]];
      If[#[[1, 2]] < YminRR, YminRR = #[[1, 2]]];
      If[#[[1, 3]] > ZmaxRR, ZmaxRR = #[[1, 3]]];
      If[#[[1, 3]] < ZminRR, ZminRR = #[[1, 3]]];) &, suppAndBall];
 Rmax = 0;
Scan[If[#[[1, 4]] > Rmax, Rmax = #[[1, 4]]] &, suppAndBall];
Rmax = Rmax*1.05;
dxRR = XmaxRR - XminRR;
dyRR = YmaxRR - YminRR;
dzRR = ZmaxRR - ZminRR;
XmaxRR = XmaxRR + 0.01 (dxRR + Rmax);
XminRR = XminRR - 0.01 (dxRR + Rmax);
YmaxRR = YmaxRR + 0.01 (dxRR + Rmax);
YminRR = YminRR - 0.01 (dxRR + Rmax);
ZmaxRR = ZmaxRR + 0.01 (dxRR + Rmax);
ZminRR = ZminRR - 0.01 (dxRR + Rmax);
dxRR = XmaxRR - XminRR;
dyRR = YmaxRR - YminRR;
dzRR = ZmaxRR - ZminRR;
   cellnum1d = Floor[(Length[suppAndBall])^(1/3)];
   tdxRR = dxRR/cellnum1d;
   tdyRR = dyRR/cellnum1d;
   tdzRR = dzRR/cellnum1d;
   listtan = Table[{}, cellnum1d, cellnum1d, cellnum1d];
   NXX = Floor[2 Rmax/tdxRR] + 1;
   NYY = Floor[2 Rmax/tdyRR] + 1;
   NZZ = Floor[2 Rmax/tdzRR] + 1;
   Parallelize[
    Scan[Module[{}, tx = #[[1, 1]]; ty = #[[1, 2]]; tz = #[[1, 3]];
       txn = Floor[(tx - XminRR)/tdxRR] + 1;
       tyn = Floor[(ty - YminRR)/tdyRR] + 1;
       tzn = Floor[(tz - ZminRR)/tdzRR] + 1;
       AppendTo[listtan[[txn, tyn, tzn]], #];] &, suppAndBall]];
   temp = 
    Parallelize[
     Map[Module[{NXXmax, NYYmax, NZZmax, NXXmin, NYYmin, NZZmin, 
         templist}, tx = #[[1, 1]]; ty = #[[1, 2]]; tz = #[[1, 3]];
        txn = Floor[(tx - XminRR)/tdxRR] + 1;
        tyn = Floor[(ty - YminRR)/tdyRR] + 1;
        tzn = Floor[(tz - ZminRR)/tdzRR] + 1;
        NXXmax = txn + NXX; NXXmin = txn - NXX;
        NYYmax = tyn + NYY; NYYmin = tyn - NYY;
        NZZmax = tzn + NZZ; NZZmin = tzn - NZZ;
        If[NXXmax > cellnum1d, NXXmax = cellnum1d];
        If[NXXmin < 1, NXXmin = 1];
        If[NYYmax > cellnum1d, NYYmax = cellnum1d];
        If[NYYmin < 1, NYYmin = 1];
        If[NZZmax > cellnum1d, NZZmax = cellnum1d];
        If[NZZmin < 1, NZZmin = 1]; 
        templist = 
         Flatten[listtan[[NXXmin ;; NXXmax, NYYmin ;; NYYmax, 
            NZZmin ;; NZZmax]], 3];
        Map[Function[{x}, x[[1, 1 ;; 4]]], templist]] &, suppAndBall]];
   near = 
    Table[Select[#2, 
        Function[{u}, (#1[[1, 1]] - u[[1]])^2 + (#1[[1, 2]] - 
              u[[2]])^2 + (#1[[1, 3]] - u[[3]])^2 < (#1[[1, 4]] + 
             u[[4]])^2]] &[suppAndBall[[i]], 
      Select[temp[[i]], (! (# === 
            suppAndBall[[i, 1, 1 ;; 4]])) &]], {i, 1, 
      Length[suppAndBall]}];];

generatePoints[suppAndBallE_] := 
  Module[{list, x, y, z, n, supp, r, rhole, vx, vy, vz, v, temp, tx, 
    ty, tz, num, tnear, j, bottom, rsupporting}, 
   tnear = near[[suppAndBallE[[1, -1]]]];
   {x, y, z, r, n, rhole, rsupporting} = 
    suppAndBallE[[1, {1, 2, 3, 4, 6, 7, 8}]];
   supp = suppAndBallE[[2]];
   list = 
    Map[(#*r + {x, y, z}) &, 
     Table[{Sqrt[1 - ((2 i - 1)/n - 1)^2] Cos[2 Pi i const], 
       Sqrt[1 - ((2 i - 1)/n - 1)^2] Sin[2 Pi i const], (2 i - 1)/n - 
        1}, {i, 1, n}]];
   list = 
    If[Length[suppAndBallE[[2]]] > 0, 
     Scan[Module[{}, vx = #[[1, 1]] - x;
        vy = #[[1, 2]] - y;
        vz = #[[1, 3]] - z;
        v = Normalize[{vx, vy, vz}]*r;
        {tx, ty, tz} = v + {x, y, z};
        temp = 
         Select[list, ((#[[1]] - tx)^2 + (#[[2]] - ty)^2 + (#[[3]] - 
                 tz)^2 > (1.01 (rsupporting + rhole))^2) &];
        list = temp;] &, supp];
     {tx, ty, tz} = {x, y, z} + {0, 0, -1}*r;
     Select[
      list, ((#[[1]] - tx)^2 + (#[[2]] - ty)^2 + (#[[3]] - tz)^2 > 
         rhole^2) &], list];
   list = 
    Select[list, (AllTrue[tnear, 
        Function[{f}, 
         Module[{p, a, b, c, h, test, th, l}, vx = f[[1]] - x;
          vy = f[[2]] - y;
          vz = f[[3]] - z;
          a = r; b = f[[4]]; c = Sqrt[(vx)^2 + (vy)^2 + (vz)^2];
          p = (a + b + c)/2;
          v = Normalize[{vx, vy, vz}]*r;
          {tx, ty, tz} = v + {x, y, z};
          th = ArcCos[(a^2 + c^2 - b^2)/(2 a c)];
          
          Sqrt[((#[[1]] - tx)^2) + ((#[[2]] - ty)^2) + ((#[[3]] - 
                 tz)^2)] > (rhole + 2*r Sin[th/2])]]]) &];
   j = 1;
   bottom = {x, y, z - r};
   If[Length[list] > 0, While[If[Length[list] == 0, Break[]];
     EuclideanDistance[First[list], bottom] < 
      1.01 (rsupporting + rhole), list = Drop[list, 1]; j++]];
   list];

generateBallsPiointsAndSupportings[] := 
  Module[{}, 
   If[type == 1, 
    SUPPORTINGS = 
     Map[Map[Function[{x}, Append[x, #[[1, -2]]]], #[[2]]] &, 
      data[[3]]];
    SUPPORTINGS = Flatten[SUPPORTINGS, 1];
    POINTS = Parallelize[Map[generatePoints, suppAndBall]];, 
    SUPPORTINGS = {};
    POINTS = Parallelize[Map[generatePoints, suppAndBall]];]];
getNumberBalls[] := Length[BALLS];
getNumberSupportings[] := Length[SUPPORTINGS];
getPointNumber[n_] := Length[POINTS[[n]]];
getPoint[n_, m_] := POINTS[[n, m]];
getSupportings[n_, m_] := Flatten[SUPPORTINGS[[n]]][[m]];
getBall[n_, m_] := BALLS[[n, m]];