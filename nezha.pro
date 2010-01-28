#!/usr/bin/swipl -q -t start_script -f 

% helper functions to run the interpreter
% from shell

start_script :-
    catch(main_script,E,(print_message(error,E),fail)),
    halt.
start_script :-
    halt.

% or running as a compiled binary
% swipl --goal=start_compile --stand_alone=true -o cornelius -c cornelius.pro
start_compile :-
    catch(main_compile,E,(print_message(error,E),fail)),
    halt.
start_compile :-
    halt.

% remove all swi prolog arguments
clean_arguments([--],H,[H,[],[]]).
clean_arguments([--|T],H,[H|T]).
clean_arguments([H|T],_,O) :-
    clean_arguments(T,H,O).

main_script :-
    current_prolog_flag(argv,X),
    clean_arguments(X,[],[_,File|_]),
    run_file(File).

main_compile :-
    current_prolog_flag(argv,[_,X|_]),
    run_file(X).

% run a given file
run_file(F) :-
    exec_file([],F,_).

exec_file(E,File,Eo) :- 
    prompt(_,''),
    (\+File = [] -> (
        string_to_atom(File,Name),
        open(Name,read,I)
        );
        current_input(I)
    ),
    read_file(I,[],Code),
    exec(E,Code,Eo,_).


read_file(I,Li,Lo) :-
    get_byte(I,C),(
        (C = -1,!,Lo=[]);
        Lo=[C|L1], 
        read_file(I,Li,L1)
    ).  

exec(X,O) :- exec([],X,_,O). 
% exec(+Environment, +Code, -EnvOut, Output)
exec(Ei,X,E,O) :- ( 
        parse(X,S);
        write('Syntax Error'),
        nl,
        fail
    ), !, (
        eval(Ei,E,S,O) *-> [] ;
        (write('Runtime Error'),nl,fail)
    ).

%% parser

% whitespace helpers
ws0 --> [X], {code_type(X, white)}, ws.
ws --> ws0.
ws --> [].

newline --> [10], linefeed. 
linefeed --> [13]; [].

% items are a generic token.
:- discontiguous item/3.
item(_) --> {fail}.

% containers

lookahead(X),X --> X.

%expressions
:- discontiguous exprn/4.
exprn(O,N1) --> prefix(Op, N),!, { N =< N1 }, exprn(R,N), !, build(Op,R,Z), follow(Z, O, N1).
exprn(O,N) --> item(L), !, follow(L,O,N).

% every expression is ast-fragment then a follow. the fragment is passed
% to follow, to check for infix stuff (that contains it)
follow(L,O,N1) --> (postfix(Op,N) -> {N =< N1}), wbreak, !, build(Op,L,Z), follow(Z, O, N1).
follow(L,O,N1) --> ws, (infix(Op,As,N) -> {assoc(As,N, N1)}), !,ws, exprn(R,N),!, build(Op,L,R,Z), follow(Z, O, N1).
follow(O,O,_) --> !.

assoc(right, A, B) :-  A =< B.
assoc(left, A, B) :- A < B.

build(C,R,call(C,R)) --> !.
build(C,L,R,call(C,[L,R])) --> !.

:- discontiguous infix/5, prefix/4, postfix/4.
infix(_,_,_) --> {fail}.
postfix(_,_) --> {fail}.
prefix(_,_) --> {fail}.

%helpers
expr(L) --> ws,exprn(L,100).

parse(X,S) :- phrase(expr(S),X),!. 

eval(E,Eo,call(H,T),O) :-  \+ var(H), 
    atom(H) -> (
        (builtin(H),!, eval(E,Eo,T,To), apply(H,To,O))
    );
    (!,eval(E,E1,H,Ho),\+H=Ho,eval(E1,Eo,call(Ho,T),O)).

eval(E,Eo,[H|T],[Ho|To]) :- !, eval(E,E1,H,Ho), eval(E1,Eo,T,To).
eval(E,E,X,X) :- atomic(X),!.
eval(E,E,X,X) :- var(X),!, fail.


%% language defintion of numbers with addition

% any digit (including underscores) is a valid token
number(N) --> digit(D0), digits(D), { number_codes(N, [D0|D]) },!.
digits([D|T]) --> ("_" -> !; []),digit(D), digits(T).
digits(O) --> ".",digit(D0),!, {append(".",[D0|T],O)}, digits(T).
digits([]) --> [].
digit(D) --> [D], {code_type(D, digit)},!.

item(X) --> number(X).

exprn(O,N1) --> "(" ,!, ws,  exprn(Op,100), ws, ")",!, follow(Op, O ,N1).

infix(le, right,60) --> ">=".
infix(eq, right,60) --> "==".
infix(unf, right,80) --> "=".
infix(ge,right,60) --> "=<".
infix(gt,right,60) --> ">".
infix(lt,right,60) --> "<".
infix(add,right,50) --> "+".
infix(sub,right,50) --> "-".
infix(mul,right,45) --> "*".
infix(div,right,45) --> "/".
prefix(neg,5) --> "-".

:- discontiguous builtin/1, apply/3.

builtin(add). apply(add,[X,Y],O) :-plus(X,Y,O),!.
builtin(sub). apply(sub,[X,Y],O) :- O is X-Y,!.
builtin(neg). apply(neg,[X],O) :- O is 0 - X,!.
builtin(mul). apply(mul,[X,Y],O) :- O is X*Y,!.
builtin(div). apply(div,[X,Y],O) :- O is X/Y .
builtin(lt). apply(lt,[X,Y],Y) :-  X <Y,!.
builtin(le). apply(le,[X,Y],Y) :-  X =<Y,!.
builtin(gt). apply(gt,[X,Y],Y) :-  X >Y,!.
builtin(ge). apply(ge,[X,Y],Y) :-  X >=Y,!.
builtin(number). apply(number,[X],Y) :-  cast_to_number(X,Y),!.

