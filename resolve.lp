% derive the most limiting upper bound on the package version
smallestUB(N,A1,B1,C1,D1) :- lt(N,A1,B1,C1,D1), lt(N,A2,B2,C2,D2), not isSmallerEq(A1,B1,C1,D1,A2,B2,C2,D2).
% derive the most limiting lower bound on the package version
greatestLB(N,A1,B1,C1,D1) :- get(N,A1,B1,C1,D1), get(N,A2,B2,C2,D2), isSmallerEq(A1,B1,C1,D1,A2,B2,C2,D2).

isSmallerEq(A1,B1,C1,D1,A2,B2,C2,D2) :- pkg(N,A1,B1,C1,D1), pkg(N,A2,B2,C2,D2),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).
isSmallerEq(A1,B1,C1,D1,A2,B2,C2,D2) :- get(N,A1,B1,C1,D1), get(N,A2,B2,C2,D2),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).
isSmallerEq(A1,B1,C1,D1,A2,B2,C2,D2) :- lt(N,A1,B1,C1,D1), lt(N,A2,B2,C2,D2),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).

% there should be no upper and lower bounds that are incompatible
:- smallestUB(N,AL,BL,CL,DL), greatestLB(N,AG,BG,CG,DG), isSmallerEq(AG,BG,CG,DG,AL,BL,CL,DL).

% choose a package version which satisfies the constraints
chosen(N,A,B,C,D) |  -chosen(N,A,B,C,D) :- pkg(N,A,B,C,D).
:- chosen(N,A,B,C,D), smallestUB(N,AU,BU,CU,DU), isSmallerEq(A,B,C,D,AU,BU,CU,DU).
:- chosen(N,A,B,C,D), greatestLB(N,AL,BL,CL,DL), not isSmallerEq(AL,BL,CL,DL,A,B,C,D).


% we should always choose exactly one package
%% we cant choose none
%:- not chosen(N,_,_,_,_), wanted(N).
%% we cant choose more than one
%:- chosen(N,A1,B1,C1,D1), chosen(N,A2,B2,C2,D2), A1!=A2.
%:- chosen(N,A1,B1,C1,D1), chosen(N,A2,B2,C2,D2), B1!=B2.
%:- chosen(N,A1,B1,C1,D1), chosen(N,A2,B2,C2,D2), C1!=C2.
%:- chosen(N,A1,B1,C1,D1), chosen(N,A2,B2,C2,D2), D1!=D2.

#show smallestUB/5.
