

%This is the grammar for creating the statements that the user would tell the knowledge base
%The grammar starts with a property phase, followed by an object/value phrase
s --> pp, ovp.
pp --> det, prop.
ovp --> op, vp.

%the object phrase consists of a transistion phrase (two determiners) and an object
op --> trans, obj.
%the value phrase consists of a determiner and a value
vp --> det, val.
obj --> [X].
trans --> [of], det.  
trans --> [what], det.  
det --> [the].
det --> [that].
det --> [is].
det --> [a].
prop --> [X].
%this grammer uses right recursion to allow for multi-word values
val --> [X].                         
val --> [X], val.

%This is the grammar for asking the knowledge base questions.
%This grammar consists of a property phrase, followed by an object phrase
s2 --> pp2, op2.
%This property phrase consists of a transitional phrase (ends up becoming 3 determiners) and a property.
pp2 --> trans2, prop2.

trans2 --> det2, transs2.

transs2 --> det2, det2.

prop2 --> [X].

%the object phrase consists of two determiners and an object
op2 --> transs2, obj2.

obj2 --> [X].

%The determiners are hard coded for this assignment
det2 --> [what].

det2 --> [is].

det2 --> [of].

det2 --> [the].

det2 --> [that].



%This is the code for the parse tree of the first grammar
s(s(PP, OVP)) --> pp(PP), ovp(OVP).
pp(pp(DET, PROP)) --> det(DET), prop(PROP).
prop(prop(X)) --> [X].
ovp(ovp(OP, VP)) --> oph(OP), vp(VP).
oph(oph(TRANS, OBJ)) --> trans(TRANS), object(OBJ). 
vp(vp(DET, VAL)) --> det(DET), val(VAL).
trans(trans(DET)) --> [of], det(DET).
object(object(X)) --> [X].

%notice, that unlike my other rules val takes in a list. This is done to allow for my parse tree to group multi-word values together.
val(val([X])) --> [X].
val(val([X|Y])) --> [X|Y].

det(det(the)) --> [the].
det(det(that)) --> [that].
det(det(is)) --> [is].
det(det(a)) --> [a].
det(det(of)) --> [of].



%this is the code for the parse tree for the second grammar.

s2(s2(PP2,OP2)) --> pp2(PP2), op2(OP2).

pp2(pp2(TRANS2, PROP2)) --> trans2(TRANS2), prop2(PROP2).

op2(op2(TRANSS2, OBJ2)) --> transs2(TRANSS2), obj2(OBJ2).

trans2(trans2(DET2, TRANSS2)) --> det2(DET2), transs2(TRANSS2).

transs2(transs2(DET2,DET3)) --> det2(DET2), det2(DET3). 

prop2(prop2(X)) --> [X].
 
obj2(obj2(X)) --> [X].

det2(det2(what)) --> [what].
det2(det2(is)) --> [is].
det2(det2(that)) --> [that].
det2(det2(of)) --> [of].
det2(det2(the)) --> [the].

% fact is dynamic, so that it is possible to assert new groupings of properties, Objects, and values. 
:- dynamic fact/3.


% These cases for the execute rule, handle when an input following grammar2 is entered
% execute uses the parse tree, to parse the user input. Then from the parse tree, the property and object in question is retrieved
% afterwards, it uses the fact function to look up the corresponding answer.
execute(I,ANS) :- s2(T,I,[]), T = s2(pp2(trans2(det2(DET), transs2(det2(DET2), det2(DET3))), prop2(PROP)), op2(transs2(det2(DET4), det2(DET5)), obj2(OBJ))) 
, fact([PROP],[OBJ], Z), isList(Z), ANS = Z .

%if the fact function yields false, then 'ANS' will be set to ['I','dont','know']
execute(I,ANS) :- s2(T,I,[]), T = s2(pp2(trans2(det2(DET),transs2(det2(DET2),det2(DET3))), prop2(PROP)),op2(transs2(det2(DET4),det2(DET5)),obj2(OBJ))),  ANS = ['I', 'dont', 'know'].



isList([]).
isList([X]).
isList([H|T]).


%This cases for the execute rule deal with when an input following grammar1, is entered
% execute uses the parse tree, to parse the user input. Then from the parse tree, the property, object, and value in question is retrieved
% afterwards, it uses the fact function to check if this statement is already known by the knowledge base.

execute(I,ANS) :- s(T,I,[]), T = s(pp(det(DET), prop(PROP)), ovp(oph(trans(det(DET2)), object(OBJ)), vp(det(DET3), val(VAL)))), fact([PROP], [OBJ], VAL), ANS = ['I', 'know'].

%If the statement is not known by the knowledge base, then this case is used to check if a value already corresponding to the given property and object exists.
execute(I,ANS) :- s(T,I,[]), T = s(pp(det(DET), prop(PROP)), ovp(oph(trans(det(DET2)), object(OBJ)), vp(det(DET3), val(VAL)))), fact([PROP], [OBJ], _), ANS = ['Its', 'not'].

%If the value corresponding to the given property and object does not exist, then this rule asserts the triplet as a fact.
execute(I,ANS) :- s(T,I,[]), T = s(pp(det(DET), prop(PROP)), ovp(oph(trans(det(DET2)), object(OBJ)), vp(det(DET3), val(VAL)))), ANS = ['Okay'], assert(fact([PROP],[OBJ],VAL)).





isVal([H]).
isVal([H|T],B):- isVal(T,B).


