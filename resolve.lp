% derive the most limiting upper bound on the package version
smallestUB(N,V1) :- lt(N,V1), not hasSmallerUB(N,V1).

% derive the most limiting lower bound on the package version
greatestLB(N,V1) :- get(N,V1), not hasGreaterLB(N,V1).
hasSmallerUB(N,V1) :- lt(N,V1), lt(N,V2), isSmaller(V2,V1).
hasGreaterLB(N,V1) :- get(N,V1), get(N,V2), isSmaller(V1,V2).

isSmallerEq((A1,B1,C1,D1),(A2,B2,C2,D2)) :- pkg(N,(A1,B1,C1,D1)), pkg(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).
isSmallerEq((A1,B1,C1,D1),(A2,B2,C2,D2)) :- get(N,(A1,B1,C1,D1)), get(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).
isSmallerEq((A1,B1,C1,D1),(A2,B2,C2,D2)) :- lt(N,(A1,B1,C1,D1)), lt(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<=(A2*1000+B2*100+C2*10+D2).

isSmaller((A1,B1,C1,D1),(A2,B2,C2,D2)) :- pkg(N,(A1,B1,C1,D1)), pkg(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<(A2*1000+B2*100+C2*10+D2).
isSmaller((A1,B1,C1,D1),(A2,B2,C2,D2)) :- get(N,(A1,B1,C1,D1)), get(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<(A2*1000+B2*100+C2*10+D2).
isSmaller((A1,B1,C1,D1),(A2,B2,C2,D2)) :- lt(N,(A1,B1,C1,D1)), lt(N,(A2,B2,C2,D2)),(A1*1000+B1*100+C1*10+D1)<(A2*1000+B2*100+C2*10+D2).

% there should be no upper and lower bounds that are incompatible
:- smallestUB(N,Vl), greatestLB(N,Vg), isSmallerEq(Vg,Vl).

% a package version is viable if it satisfies the constraints and is wanted
viable(N,V) :- pkg(N,V), wanted(N), not nonViable(N,V).
% a version is not viable if it is greater than the upper bound
nonViable(N,V) :- pkg(N,V), smallestUB(N,Vu), isSmallerEq(Vu,V).
% a version is not viable if it is smaller than the lower bound
nonViable(N,V) :- pkg(N,V), greatestLB(N,Vl), isSmaller(V,Vl).
    
chosen(N,V) |  -chosen(N,V) :- viable(N,V).

% we should always choose exactly one package
%% we cant choose none
:- not chosen(N,_), wanted(N).
%% we cant choose more than one
:- chosen(N,(A1,_,_,_)), chosen(N,(A2,_,_,_)), A1!=A2.
:- chosen(N,(_,B1,_,_)), chosen(N,(_,B2,_,_)), B1!=B2.
:- chosen(N,(_,_,C1,_)), chosen(N,(_,_,C2,_)), C1!=C2.
:- chosen(N,(_,_,_,D1)), chosen(N,(_,_,_,D2)), D1!=D2.

% we should always choose the newest package
:- chosen(N,V1), viable(N,V2), isSmaller(V1,V2).

% the newest package is not chosen with no restrictions...
#show chosen/2.
%#show viable/5.
