convertBinToDec(0,0).
convertBinToDec(0,_,0).
convertBinToDec(X,D):-
	X\=0,
	convertBinToDec(X,0,D).
convertBinToDec(X,Acc,D):-
	X>0,
	X1 is X mod 10,
	X2 is X//10,
	Acc1 is Acc +1,
	pow(2,Acc,Y),
	convertBinToDec(X2,Acc1,D1),
	D is (Y*X1)+D1.

splitEvery(N,List,Res):-
	splitEvery(N,0,[],List,Res).
splitEvery(N,_,L,[],[L]).
splitEvery(N,N,L,[H|T],[L|T1]):-
	splitEvery(N,0,[],[H|T],T1).
splitEvery(N,Acc,L2,[H|T],L):-
	Acc<N,Acc1 is Acc+1,append(L2,[H],L3),
	splitEvery(N,Acc1,L3,T,L).

logBase2(M,N):-
	logBase2(M,0,N),!.
logBase2(M,V,V):-
	pow(2,V,N1),N1=M,!.
logBase2(M,V,N):-
	pow(2,V,N1),N1<M,V1 is V+1,
	logBase2(M,V1,N),!.
fillZeros(S,0,S).
fillZeros(S,N,R):-
	N>0,
	string_concat("0", S, S1), N1 is N-1,
	fillZeros(S1,N1,R).
	
getNumBits(_,fullyAssoc,_,0).
getNumBits(N,setAssoc,_,X):- logBase2(N,X).
getNumBits(_,directMap,L,X):-
	length(L,M),logBase2(M,X).
	
%--------------------------SET-----------.
convertAddress(Bin,SetNum,Tag,Idx,setAssoc):-
	logBase2(SetNum,M),helper(Bin,M,0,0,Idx,Tag).
helper(Bin,0,Acc1,_,Acc1,Bin).
helper(Bin,M,Acc,Acc2,Idx,T):-
	M>0,
	N is (Bin mod 10),N2 is Bin//10,
	pow(10,Acc2,K),K1 is K*N,
	Acc1 is Acc+K1,
	M1 is M-1,Acc3 is Acc2+1,
	helper(N2,M1,Acc1,Acc3,Idx,T).
	
getDataFromCache(StrAdd,C,D,HNum,setAssoc,SetsNum):-
	length(C,X),
	Y is X//SetsNum,
	getNumBits(SetsNum,setAssoc,C,N),
	 atom_number(StrAdd, Number),
	convertAddress(Number,SetsNum,Tag,Idx,setAssoc),
	string_concat("",Tag,Tag1),
	string_length(StrAdd,N1),
	string_length(Tag1,J),
	N2 is N1-N-J,
	fillZeros(Tag1,N2,Tag2),
	convertBinToDec(Idx,Idx2),
	splitEvery(Y,C,Result),
	nth0(Idx2,Result,R1),
	getDataFromCache2(Tag2,R1,D,HNum,fullyAssoc,SetsNum).

getDataFromCache2(StringAddress,[H|T],Data,HopsNum,fullyAssoc,SetsNum):-
	getDataFromCache2(StringAddress,[H|T],Data,0,HopsNum,fullyAssoc,SetsNum).

getDataFromCache2(StringAddress,[H|T],Data,Acc,HopsNum,fullyAssoc,SetsNum):-
	H=item(_,_,0,_),Acc1 is Acc+1,
	getDataFromCache2(StringAddress,T,Data,Acc1,HopsNum,fullyAssoc,SetsNum).
getDataFromCache2(StringAddress,[H|T],Data,Acc,Acc1,fullyAssoc,SetsNum):-
	H=item(tag(S),data(_),1,_),Acc1 is Acc+1,
	S\=StringAddress,getDataFromCache2(StringAddress,T,Data,Acc1,HopsNum,fullyAssoc,SetsNum).
getDataFromCache2(StringAddress,[H|T],Data,Acc,Acc,fullyAssoc,SetsNum):-
	H=item(tag(StringAddress),data(Data),1,_).

	
replaceInCache2(Tag,_,_,OldCache,NewCache,ItemData,fullyAssoc,NumOfSets):-
		logBase2(NumOfSets , U1 ),
		string_length(U1 , U2 ),
		string_length(Tag,N2),
		string_length(ItemData,N),
		N1 is 6-N2-U2,
		fillZeros(Tag,N1,TT),
		helpar(OldCache,0,P),P\=(-1),replaceIthItem(item(tag(TT),data(ItemData),1,-1),OldCache,P,NewCache1),
length(OldCache,K),
helparInc2(NewCache1,NewCache,K,0,0).
  

replaceInCache2(Tag,_,_,OldCache,NewCache,ItemData,fullyAssoc,NumOfSets):-
helpar(OldCache,0,P),P=(-1),helpar2(OldCache,0,Max),
		logBase2(NumOfSets , Q3 ),
		string_length(Q3 , Q2 ),
		string_length(Tag,N2),
		string_length(ItemData,N), 
		N1 is 6-N2 - Q2,
		fillZeros(Tag,N1,TT),
		helparInc2(OldCache,NewCache,Max,ItemData,TT).



helpar([],_,-1):- !.
helpar([item(_,_,0,_)|T],P,P):- !.
helpar([H|T],Acc,P):-
	H1=item(_,_,1,_),Acc1 is Acc+1,helpar(T,Acc1,P),!.


helpar2([],M,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,1,O),O>=Acc,M1 is O,helpar2(T,M1,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,1,O),O<Acc,helpar2(T,Acc,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,0,O),helpar2(T,Acc,M).


helparInc2([],[],_,_,_).
helparInc2([H|T],[H1|T1],Max,I,Tag):-
	
	H=item(X,D,1,O),O<Max,O1 is O+1,H1=item(X,D,1,O1),helparInc2(T,T1,Max,I,Tag).
helparInc2([H|T],[H|T1],Max,I,Tag):-
	H=item(_,D,0,O),helparInc2(T,T1,Max,I,Tag).
helparInc2([H|T],[H|T1],Max,I,Tag):-
	 H=item(E,data(D),1,O),O<Max ,O1 is O +1 ,H1 = item(E,data(D),1,O1),helparInc2(T,T1,Max,I,Tag).
helparInc2([H|T],[H1|T1],Max,I,Tag):-
	 H=item(tag(_),data(_),1,O),O=Max , H1=item(tag(Tag),data(I),1,0) ,helparInc2(T,T1,Max,I,Tag).
	
	
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
	logBase2(SetsNum,J),
	string_length(Idx,J2),
	J1 is J-J2,
	fillZeros(Idx,J1,YY),
	string_concat(Tag,YY,Idx3),
	atom_number(Idx3,Idx22),
	convertBinToDec(Idx22,Idxf),
	nth0(Idxf,Mem,ItemData),
	convertBinToDec(Idx,Idx1),
	length(OldCache,L),
	L1 is L // SetsNum ,
	splitEvery(L1,OldCache,Result),
	nth0(Idx1,Result, R1),
	replaceInCache2(Tag , Idx , Mem , R1 , NewCache1 , ItemData , fullyAssoc , NumOfSets),
	append2(Result,NewCache1,Idx1,NewCache),!.
	
	
	append2([],_,_,[]):- !.
	append2([H|T],NewCache1,Idx1,NewCache):-
	Idx1\=0, Idx11 is Idx1-1,append2(T,NewCache1,Idx11,T1),append(H ,T1 ,NewCache),!.
	append2([_|T],NewCache1,0,NewCache):-
	append2(T,NewCache1,-1,T1),append(NewCache1 , T1 , NewCache),!.
	
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_):-!.
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets),!.


%-----------------------------fullyAssoc----------------.

getDataFromCache(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_):-	
	getDataFromCache(StringAddress,[H|T],Data,0,HopsNum,fullyAssoc,_).
	
getDataFromCache(StringAddress,[H|T],Data,Acc,Acc,fullyAssoc,_):-
	H=item(tag(TTT),data(Data),1,_),
	string_length(StringAddress,N1),
	N2 is 6-N1,
	fillZeros(TTT,N2,Tag2),
	Tag2=StringAddress.
	
getDataFromCache(StringAddress,[H|T],Data,Acc,HopsNum,fullyAssoc,_):-
	H=item(_,_,0,_),
	Acc1 is Acc+1,
	getDataFromCache(StringAddress,T,Data,Acc1,HopsNum,fullyAssoc,_).
	
getDataFromCache(StringAddress,[H|T],Data,Acc,Acc1,fullyAssoc,_):-
	H=item(tag(S),data(_),1,_),
	Acc1 is Acc+1,
	S\=StringAddress,
	getDataFromCache(StringAddress,T,Data,Acc1,HopsNum,fullyAssoc,_).
	

convertAddress(Bin,_,Tag,_,fullyAssoc):-
	convertBinToDec(Bin,Dec),
	convertDecToBin(Dec,Tag).

replaceIthItem(X,[H|T],0,[X|T]).
replaceIthItem(X,[H|T],N,[H|T1]):-
	N>0,
	N1 is N-1,
	replaceIthItem(X,T,N1,T1).



	
	
getNumBits(_,fullyAssoc,_,0).
getNumBits(N,setAssoc,_,X):-
	logBase2(N,X).
	
getNumBits(_,directMap,L,X):-
	length(L,M),logBase2(M,X).


convertDecToBin(0,0).
convertDecToBin(X,Y):-
	convertDecToBin1(X,String),
	atom_number(String,Y).
	
convertDecToBin1(0,"").
convertDecToBin1(X,String):-
	X>0,
	0 is X mod 2,
	X1 is X//2,
	convertDecToBin1(X1,String1),
	string_concat(String1,"0",String).
	
convertDecToBin1(X,String):-
	X>0,
	1 is X mod 2,
	X1 is X//2,
	convertDecToBin1(X1,String1),
	string_concat(String1,"1",String).
	
replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
	convertBinToDec(Tag , D1),
	nth0(D1,Mem,ItemData),
	string_length(Tag,N2),
	string_length(ItemData,N),
	N1 is 6-N2,
	fillZeros(Tag,N1,TT),
	helpar(OldCache,0,P),
	P\=(-1),
	replaceIthItem(item(tag(TT),data(ItemData),1,-1),OldCache,P,NewCache1),
	length(OldCache,K),
	helparInc(NewCache1,NewCache,K,0,0).


replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
	convertBinToDec(Tag , D1),
	nth0(D1,Mem,ItemData),
	helpar(OldCache,0,P),P=(-1),helpar2(OldCache,0,Max),
	string_length(Tag,N2),
	string_length(ItemData,N),
	N1 is 6-N2,
	fillZeros(Tag,N1,TT),
	helparInc(OldCache,NewCache,Max,ItemData,TT).


helpar2([],M,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,1,O),O>Acc,M1 is O,helpar2(T,M1,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,1,O),O<Acc,helpar2(T,Acc,M).
helpar2([H|T],Acc,M):-
	H=item(_,_,0,O),helpar2(T,Acc,M).


helparInc([],[],_,_,_).
helparInc([H|T],[H1|T1],Max,I,Tag):-
	H=item(X,D,1,O),O<Max,O1 is O+1,H1=item(X,D,1,O1),helparInc(T,T1,Max,I,Tag).
helparInc([H|T],[H|T1],Max,I,Tag):-
	H=item(_,D,0,O),helparInc(T,T1,Max,I,Tag).
helparInc([H|T],[H1|T1],Max,I,Tag):-
	H=item(_,data(D),1,O),O=Max,H1=item(tag(Tag),data(I),1,0),helparInc(T,T1,Max,I,Tag).
	
	
	
	
%-------------------DIRECTT--------------------------.
	convertAddress(X,N,Tag,Ind,directMap):-
	G=X,
	getindx(G,N,Y),
	atom_number(Y,Ind),
	Tag is X//10**2.
getindx(_,0,"").
getindx(X,N,Y):-
	N>0,
	X1 is X mod 10,
	X2 is X//10,
	N1 is N-1,
	atom_number(F,X1),
	getindx(X2,N1,Y1),
	string_concat(Y1,F,Y).
%--------------getDataFromCache for  direct-----------------.
getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
	atom_length(StringAddress,N),
	atom_number(StringAddress,J),
	convertAddress(J,BitsNum,Tag,Ind,directMap),
	convertBinToDec(Ind,KL),
	nth0(KL,Cache,Elem),
    A is N-BitsNum-1,
	atom_number(H,Tag),
	fillZeros(Tag,A,R),
	compare(R,Elem),
	getDate(Data,Elem),
	HopsNum=0.
compare(Tag,item(tag(Tag),_,_,_)).
getDate(Date,item(_,data(Date),1,_)).
		
%------------------------replace for direct------------.
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,directMap,BitsNum):-
		atom_number(NN,Idx),
		atom_length(NN,SS),
		SS1 is BitsNum-SS,
		fillZeros(NN,SS1,IND),
		string_concat(Tag,IND,Idx1),
		atom_number(Idx1,Idx22),
		convertBinToDec(Idx22,Idxf),
		nth0(Idxf,Mem,Data),
		%atom_number(Data,Data2),
		%convertAddress(Data2,BitsNum,Tag2,Idx2,directMap),
		convertBinToDec(Idx,D2),
		%nth0(D2,OldCache,X), 
		string_concat("",Idx,T),
		string_length(T,N2),
		string_length(Data,N),
		nth0(0,OldCache,KLL),
		getlen(KLL,SH),
		atom_length(Tag,FF),
		N1 is SH-FF,
		fillZeros(Tag,N1,TT),
		X=item(tag(TT),data(Data),1,0),
		replaceIthItem(X,OldCache,D2,NewCache).
getlen(item(tag(B),_,_,_),S):-
		atom_length(B,K),
		S=K.
		