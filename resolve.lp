% derive the most limiting upper bound on the package version
smallestUB(N,V1) :- lt(N,V1), not hasSmallerUB(N,V1).
% derive the most limiting lower bound on the package version
greatestLB(N,V1) :- get(N,V1), not hasGreaterLB(N,V1).
hasSmallerUB(N,V1) :- lt(N,V1), lt(N,V2), V2<V1.
hasGreaterLB(N,V1) :- get(N,V1), get(N,V2), V1<V2.
% there should be no upper and lower bounds that are incompatible
:- smallestUB(N,Vu), greatestLB(N,Vl), Vu<=Vl.

% a package version is viable if it satisfies the constraints and is wanted
viable(N,V) :- pkg(N,V), wanted(N), not nonViable(N,V).
% a version is not viable if it is greater than the upper bound
nonViable(N,V) :- pkg(N,V), smallestUB(N,Vu), Vu<=V.
% a version is not viable if it is smaller than the lower bound
nonViable(N,V) :- pkg(N,V), greatestLB(N,Vl), V<Vl.

chosen(N,V) |  -chosen(N,V) :- viable(N,V).

% we should always choose exactly one package if it is wanted
%
%% we cant choose none
:- not chosen(N,_), wanted(N).
%% we cant choose more than one
:- chosen(N,V1), chosen(N,V2), V1!=V2.

% we should always choose the newest package
:- chosen(N,V1), viable(N,V2), V1<V2.

#show chosen/2.
#show viable/2.
#show pkg/2.