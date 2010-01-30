#!/usr/bin/swipl -q -t start_script -f 

:- set_prolog_flag(double_quotes,string).

% helper functions to run the interpreter
% from shell

start_script :-
    catch(main_script,E,(print_message(error,E),fail)),
    halt.
start_script :-
    halt.

% or running as a compiled binary
% swipl --goal=start_compile --stand_alone=true -o binary -c source.pro
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

% pulling them together
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

% shorthand for exectute this string.

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
exec_s(Ei,X,E,O) :- parse(X,S),!,eval(Ei,E,S,O).

expect_fail(Code) :- findall(X,exec_s(Code,X),Output), \+ Output = []-> (writef('"%s" gave "%w" not failure\n',[Code, Output]), !, fail);[].
expect_one(Code,O) :- expect(Code,[O]).
expect(Code,Output) :-  findall(X,exec(Code,X),P), P = Output -> []; writef('"%s" is %w not %w\n',[Code, P, Output]).
:- discontiguous test/2.

test(example, O) :- (
    []
    ) -> O = pass; O = fail.

%% parser

% useful functions:
% lookahead a token.
lookahead(X),X --> X.

% we define a simple parser based around
% items, expressions and operators with a given weight 

% common parse tokens.
% whitespace helpers ws0 means specific whitespace, ws means any.
ws0 --> [X], {code_type(X, white)}, ws.
ws --> ws0.
ws --> [].

% hello cr lf.
newline --> [10], linefeed. 
linefeed --> [13]; [].


% hookable part. use item(X) --> ... to define
% new item rules. items are simple nodes that
% do not have any child nodes in the ast.

% items are a generic token.
:- discontiguous item/3.
item(_) --> {fail}.


% expressions
% an expression has an output and a weight restriction.
% i.e an exprn(A,100) can contain within it a lower weight
% expression(B,80), but not the other way around.
% this weight is used to control operator precedence

:- discontiguous exprn/4.
%an expression is simply an item, possibly with some things following it.
exprn(O,N) --> item(L), !, follow(L,O,N).

% an expression is a prefix token, at a lower weight, with a single expression
% bound to it.
exprn(O,N1) --> prefix(Op, N),!, { N =< N1 }, exprn(R,N), !, build(Op,R,Z), follow(Z, O, N1).

% expressions can be followed by operators, i.e 4 then '+'
% these rules check for trailing operators, and consume 
% more expressions as necessary

% postfix operators can follow an expression head, and can be followed
follow(L,O,N1) --> (postfix(Op,N) -> {N =< N1}), !, build(Op,L,Z), follow(Z, O, N1).

% infix expressions capture another expression to the right and can be followed.
follow(L,O,N1) --> ws, (infix(Op,As,N) -> {assoc(As,N, N1)}), !,ws, exprn(R,N),!, build(Op,L,R,Z), follow(Z, O, N1).

% the expression might not have anything following it.
follow(O,O,_) --> !.

% right associative operators bind (a + b) + c
assoc(right, A, B) :-  A =< B.
% left associative operators bind a + (b + c) 
assoc(left, A, B) :- A < B.

% operators can be re-written before being inserted into the tree.
build(C,R,call(C,R)) --> !.
build(C,L,R,call(C,[L,R])) --> !.

% operators are simply parse rules.
:- discontiguous infix/5, prefix/4, postfix/4.
% infix(name, assoc, bind) --> "token".
infix(_,_,_) --> {fail}.
% postfix(name, bind) --> "token".
postfix(_,_) --> {fail}.
% prefix(name, bind) --> "token".
prefix(_,_) --> {fail}.

% a top level expression starts at 100
expr(L) --> ws,exprn(L,100).
% to parse the dcg, parse(+String, -Structure).
parse(X,S) :- phrase(expr(S),X),!. 

% eval(+Environment,-Environment,+Code,-Output)
:- discontiguous eval/4.

evalone(Ei,Eo,X,O) :- eval(Ei,Eo,X,O),!.

% prevent unbound variables.
eval(E,E,X,X) :- var(X),!, fail.

eval(E,Eo,call(H,T),O) :-  \+ var(H), 
    atom(H) -> (
        (builtin(H),!, eval(E,Eo,T,To), apply(H,To,O));
        (!, eval_call(E,Eo,call(H,T),O))
    );
    (!,eval(E,E1,H,Ho),\+H=Ho,eval(E1,Eo,call(Ho,T),O)).

% evaluating a list
eval(E,Eo,[H|T],[Ho|To]) :- !, eval(E,E1,H,Ho), eval(E1,Eo,T,To).
eval(E,E,[],[]) :- !.

%% use builtin/1 to indicate a builtin operator
%% and use apply(name,[args],output) to implement it.
:- discontiguous builtin/1, apply/3.

%% language defintion of numbers with addition

% any digit (including underscores) is a valid token

number(N) --> digit(D0), digits(D), { number_codes(N, [D0|D]) },!.
digits([D|T]) --> ("_" -> !; []),digit(D), digits(T).
digits(O) --> ".",digit(D0),!, {append(".",[D0|T],O)}, digits(T).
digits([]) --> [].
digit(D) --> [D], {code_type(D, digit)},!.

% define a new leaf node in the ast.
item(X) --> number(X).

% add parenthesis rule for an expression.
item(X) --> "(" ,!, ws,  expr(X), ws, ")",!.

% add evaluation rule for a number
eval(E,E,X,X) :- number(X),!.

% define arithmetic and comparison operators
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

test(numbers, O) :- ( expect("1 + 1",[2]),
    expect("1 + 2 * 3", [7]),
    expect("(1 + 2) * 3", [9]) )
    -> O = pass; O = fail.

%% flow control operators.

infix(conj,right,95) --> "&". 
infix(and,right,95) --> "and".
infix(disj,right,96) --> "|".
infix(or,right,96) --> "or".
prefix(every,94) --> "every" ,ws.
prefix(once,94) --> "once",ws.


prefix(not,94) --> "not",ws.

eval_call(E,Eo,call(once,T),A) :- !,eval(E,Eo,T,A),!.
eval_call(E,Eo,call(every,X),Z) :- !,findall(A,eval(E,Eo,X,A),Z),!.
eval_call(E,Eo,call(and,[X,Y]),Z) :-!, evalone(E,E1,X,_),!,eval(E1,Eo,Y,Z).
eval_call(E,Eo,call(or,[X,Y]),Z) :- !,((evalone(E,Eo,X,Z) *-> true);eval(E,Eo,Y,Z)).
eval_call(E,E,call(not,X),[]) :- \+ eval(E,_,X,_), !.
eval_call(_,_,call(disj,[]),_) :- !, fail.
eval_call(E,Eo,call(disj,[H|T]),Z) :- !,(eval(E,E1,H,Z) ; !,eval(E1,Eo,call(disj,T),Z)).
eval_call(E,Eo,call(conj,X),Z) :- !,eval_conj(E,Eo,X,[],Z).

%eval_conj(+Env,-Env, +ConjList, +LastResult, -Result).
eval_conj(E,E,[],X,X). 
eval_conj(E,Eo,[H|T],_,X) :-  eval(E,E1,H,O), eval_conj(E1,Eo,T,O,X).

test(controlflow, O) :- (
    expect("1 | 2",[1,2]),
    expect("(1 | 2) and 3", [3]),
    expect("every (1 | 2 | 3)", [[1,2,3]]),
    expect_fail("not 1") 
    ) -> O = pass; O = fail.






% test handler, last thing in the file.
test(placeholder, O) :- (
    []
    ) -> O = pass; O = fail. 


% test runner
test_out([]).
test_out([[_,pass]|T]) :- test_out(T).
test_out([[N,O]|T]) :- writef('%w %w\n',[O,N]), test_out(T).
do :- findall([T,O],test(T,O), L),  test_out(L).



