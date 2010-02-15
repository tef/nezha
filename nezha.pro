#!/usr/bin/swipl -q -t start_script -f 

%% introduction (then quickstart and outline of code) 

% nezha aims to be a dynamic goal directed language for writing processes,
% that talk to each other using pipes and signals, each having their own 
% distinct namespaces for accesing resources using urls

% nezha aims to be built turtles all the way up, in a nanopass style.
% iteratively adding literals, parse rules, translations, evaluation,
% and allow features to be defined within the language.

%% ersatz zen 

% from python import generators, itertools, zen
% from unix.philosphy import *
% from icon import goal_direction
% from perl import braces, regex, virtues
% from prolog.craft import elegance, understanding 
% from lua import metatables, coroutines
% from moose import roles
% from erlang import supervision,processes

% little languages should be embedded within the language, not buried in strings
% languages need to grow, language extentions should be first class, and distinct from libraries
% urls are useful, we should use them to refer to things, and handlers for them are useful too
% some special cases happen often enough to bend the rules.
% it is better to have different things for different intents, even if they are similar underneath.
% patterns indicate a fault in the language - boilerplate is worth avoiding when possible.
% expressions can succeed or fail. coroutines can return a number of values or fail.

%% running

% run it as ./nezha.pro which reads files from arguments or from stdin.
% or (preferred) load it into a swi prolog interpeter.

% tests are run every time this program is loaded or run.

%% quick start for using swipl

% $ swipl
% ....
% ?-    % this is the prompt.
% ?- consult('nezha.pro').
% this tells it to load the file, use it again to reload, which will run tests

% remember: in prolog, 'nezha.pro' is an atom, "foo" is a string.

% then call exec("1 + 2",X). 

% the full stop is important. in prolog it terminates expressions.

% ?- exec("1 or 2",X).
% X = 1 ;
% false.

% hitting ; means try again.
% hitting enter means you're done.

% see the swiprolog manual for things like firing up your editor
% or debugging commands.

% finally, to compile a prolog file
% swipl --goal=start_compile --stand_alone=true -o binary -c source.pro

%% outline of interpreter

    % framework for language
    %% basic runtime skeleton
    %% test runner
    %% parser skeleton
    %% eval skeleton

    % language specific features
    %% language definitions
    %% todo section and future work

    % runtime
    %% engine, prolog main section, auxillary functions
    %% testrunner

    %% bibliography

%% runtime settings
% useful setting, but this breaks norgg's older swi prolog
% :- set_prolog_flag(double_quotes,string).


%% basic runtime skeleton

% we start by defining how to run a string.

% shorthand for exectute this string.
exec(X,O) :- environment(E), exec(E,X,_,O).

% the initial environment is a pair of empty lists
environment(e([],[])).
locals(e(_,L),L).
globals(e(G,_),G).

% run a given string, with an evironment
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

% note: A *-> B; Cis the soft cut operator, if A succeeds, it is A,B 
%                                           if A fails, it is \+A,C 
% either B or C will be run, never both.

exec_s(X,O) :- parse(X,S),!,eval([],_,S,O).

% now we define a number of useful testing predicates
% in terms of exec.
expect_fail(Code) :- findall(X,exec_s(Code,X),Output), \+ Output = []-> (writef('"%s" gave "%w" not failure\n',[Code, Output]), !, fail);[].
expect(Code,Output) :-  exec(Code,X),!, (Output = X -> []; writef('"%s" is %w not %w\n',[Code, X, Output])).
expect(Code,Output) :-  !, writef('"%s" failed, not %w\n',[Code, Output]).

% we can define test(name, -Output) where Output is pass or fail
% these can be defined anywhere, and there is a test runner at the
% bottom
:- discontiguous test/2.

% here is an example test.
test(example, O) :- (
    []
    ) -> O = pass; O = fail.


%% parser skeleton

% there are some helper methods defined later for
% whitespace (ws=/ +/ ws0=/ */) newlines and lookahead

% we define a simple parser based around
% items, expressions and operators with a given weight 

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
exprn(O,N1) --> prefix(Op, N),!, { N =< N1 }, exprn(R,N), !, build_(Op,R,Z), follow(Z, O, N1).

% expressions can be followed by operators, i.e 4 then '+'
% these rules check for trailing operators, and consume 
% more expressions as necessary

% postfix operators can follow an expression head, and can be followed
follow(L,O,N1) --> (postfix(Op,N) -> {N =< N1}), !, build_(Op,L,Z), follow(Z, O, N1).
follow(L,O,N1) --> postfix_expr(Op,I,N) -> {N =< N1}, !, build_(Op,L,I,Z), follow(Z, O, N1).

% lazy infix operators can follow an expression head, and can be followed
follow(L,O,N1) --> ws0, (lazyinfix(Op,_,N) -> {N =< N1}), !,  follow_lazy_infix(L,Op,N,Z), follow(Z,O,N1).

% infix expressions capture another expression to the right and can be followed.
follow(L,O,N1) --> ws0, (infix(Op,As,N) -> {assoc(As,N, N1)}), !,ws0, exprn(R,N),!, build_(Op,L,R,Z), follow(Z, O, N1).

% the expression might not have anything following it.
follow(O,O,_) --> !.

% lazy infix expressions can optionally capture another expression to the right and can be followed.
follow_lazy_infix(L,Op,N,Z) --> ws0, exprn(R,N),!, build_(Op,L,R,Z).
follow_lazy_infix(L,Op,_,Z) --> build_(Op,L,Z).

:- discontiguous postfix_expr/7.

postfix_expr(_,_,_,_) --> {fail}.

% right associative operators bind a + (b + c) 
assoc(right, A, B) :-  A =< B.
% left associative operators bind (a + b) + c
assoc(left, A, B) :- A < B.

% operators can be re-written before being inserted into the tree.

:- discontiguous build/3, build/4.
build_(C,R,O) --> {build(C,R,O)}.
build_(C,R,call(C,[R])) --> !.
build(_,_,_) --> {fail}.

build_(C,L,R,O) --> {build(C,L,R,O)}.
build_(C,L,R,call(C,[L,R])) --> !.
build(_,_,_,_) --> {fail}.

% operators are simply parse rules.
:- discontiguous infix/5, prefix/4, postfix/4.
% infix(name, assoc, bind) --> "token".
infix(_,_,_) --> {fail}.
% postfix(name, bind) --> "token".
postfix(_,_) --> {fail}.
% prefix(name, bind) --> "token".
prefix(_,_) --> {fail}.

% a top level expression starts at 100
expr(L) --> ws0,exprn(L,100).
% to parse the dcg, parse(+String, -Structure).
parse(X,S) :- phrase(expr(S),X),!. 

%% evaluator skeleton

% eval(+Environment,-Environment,+Code,-Output)
:- discontiguous eval/4, index(eval(0,0,1,0)).

% prevent unbound variables.
eval(E,E,X,X) :- var(X),!, fail.
eval(_,_,fail,_) :- !, fail.

% evaluating a function call
eval(E,Eo,call(H,T),O) :-  \+ var(H), !,eval_call(E,Eo,H,T,O).

% this is seperate to allow overloading
:- discontiguous eval_call/5, index(eval_call(0,0,1,0,0)).
eval_call(E,Eo,H,T,O) :- builtin(H),!, eval_list(E,Eo,T,To), !, apply(H,To,O).

% evaluating a list
eval_list(E,Eo,[H|T],[Ho|To]) :- !, eval(E,E1,H,Ho),!, eval_list(E1,Eo,T,To).
eval_list(E,E,[],[]) :- !.

%% use builtin/1 to indicate a builtin operator
%% and use apply(name,[args],output) to implement it.
:- discontiguous builtin/1, apply/3.


%% language skeleton

comment --> "#", comment_tail.

comment_tail --> newline,!.
comment_tail --> [_], comment_tail,!.
comment_tail --> [].

:- discontiguous reserved/1.


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
item(X) --> "(" ,!, ws0,  expr(X), ws0, ")",!.

% add evaluation rule for a number

eval(E,E,X,X) :- number(X),!.

% define arithmetic and comparison operators
% these can be compounded i.e 1 < 2 < 3

infix(O,A,N) --> infix_(O,A,N).
infix(le, left,60) --> "=<".
infix(ge,left,60) --> ">=".
infix(eq, left,60) --> "==".
infix(gt, left,60) --> ">".
infix(lt,left,60) --> "<".

infix(add,left,50) --> "+".
infix(sub,left,50) --> "-".
infix(mul,left,45) --> "*".
infix(div,left,45) --> "/".
prefix(neg,5) --> "-".

builtin(add). apply(add,[X,Y],O) :-plus(X,Y,O),!.
builtin(sub). apply(sub,[X,Y],O) :- O is X - Y,!.
builtin(neg). apply(neg,[X],O) :- O is 0 - X,!.
builtin(mul). apply(mul,[X,Y],O) :- O is X * Y,!.
builtin(div). apply(div,[X,Y],O) :- O is X  /Y .
builtin(lt). apply(lt,[X,Y],Y) :-  X < Y,!.
builtin(le). apply(le,[X,Y],Y) :-  X =< Y,!.
builtin(gt). apply(gt,[X,Y],Y) :-  X > Y,!.
builtin(ge). apply(ge,[X,Y],Y) :-  X >= Y,!.
builtin(eq). apply(eq,[X,Y],Y) :-  X == Y,!.
builtin(number). apply(number,[X],Y) :-  cast_to_number(X,Y),!.

test(numbers, O) :- (
    expect("1 + 1",2),
    expect("1 + 2 * 3", 7),
    expect("(1 + 2) * 3", 9),
    parse("(1 + 2) + 3",X),
    parse("1 + 2 + 3",X),
    expect("1 < 2 < 3", 3), 
    expect("(1 + 2) == (5 -2)", 3), 
    [])-> O = pass; O = fail.

%% next, identifiers allow us to inroduce builtin values/operators:

local_identifier(A) -->  csym(C),csyms(N), {string_to_list(A,[C|N])},!. 
csyms([H|T]) --> csym_(H), csyms(T).
csyms([]) --> [].
csym(C) --> [C], {code_type(C, csymf)}.
csym_(C) --> [C], {code_type(C, csym)}.

global_identifier(A) --> "$", !, csym(C),csyms(N), {string_to_list(A,[C|N])},!. 

% identifiers are matched after operators
% fixme? use same matching.
exprn(O,N) --> local_identifier(X), !, idbuild(X,O1), !, follow(O1,O,N). 
exprn(O,N) --> global_identifier(X), !, follow(global(X),O,N). 

idbuild(X,_) --> {reserved(X),!, fail}.
idbuild(X,O) --> {nofix(X,O)},!.
idbuild(X,id(X)) --> !.

% nofix operators take no arguments.
% useful for special builtin values.

:- discontiguous nofix/2.
nofix(F,fail) :- string_to_atom(F,fail),!.

%% strings

string(A) --> "\"", chars(S), {string_to_list(A,S)},!.
chars([]) --> "\"".
chars(O) --> "\\",!, escapes(O). 
chars([H|T]) --> [H], chars(T).

escapes(O) --> "\"", {append("\"",T,O)},chars(T).
escapes(O) --> "n", {append("\n",T,O)},chars(T).
escapes(O) --> "t", {append("\t",T,O)},chars(T).
escapes(O) --> newline, chars(O).

item(string(I)) --> string(I).
eval(E,E,string(X), string(X)).


% debugging
nofix(T,call(trace,[])) :- string_to_atom(T,trace),!.
eval_call(E,E,trace,_,nil) :- trace,!.

%% flow control operators.

% A and B     do A, then do B. 
infix(and,right,96) --> "and".
eval_call(E,Eo,and,[X,Y],Z) :-!, eval(E,E1,X,_),!,eval(E1,Eo,Y,Z).
reserved(And) :- string_to_list(And,"and").

% A or B      do A but if A  fails do B.
infix(or,right,98) --> "or".
eval_call(E,Eo,or,[X,Y],Z) :- !,((eval(E,Eo,X,Z) *-> true);eval(E,Eo,Y,Z)).
reserved(Or) :- string_to_list(Or,"or").

% not A       see if A fails.
prefix(not,94) --> "not",ws0.
eval_call(E,E,not,[X],[]) :- \+ eval(E,_,X,_), !.
reserved(Not) :- string_to_list(Not,"not").

test(controlflow, O) :- (
    expect("1 or 2",1),
    expect("(1 or 2) and 3", 3),
    expect_fail("not 1"),
    expect("fail or 3 or 4", 3),
    []) -> O = pass; O = fail.

%% variables and assignment

infix(assign,right,80) --> "=". 

eval_call(E,Eo,assign,[id(T),I],O1) :-
    !,
    eval(E,E1,I,O),
    !,
    do_assign(E1,Eo,id(T),O,O1).

:- discontiguous do_assign/5.

do_assign(E,Eo,id(T),O,O) :-
    env_set_var(E,Eo,T,O),!.

eval_call(E,Eo,assign,[global(T),I],O1) :-
    !,
    eval(E,E1,I,O),
    do_assign(E1,Eo,global(T),O,O1).

do_assign(E,Eo,global(T),O,O) :-
    env_set_global_var(E,Eo,T,O),!.

eval(E,E,id(X),O) :-  env_get_var(E,X,O),!.
eval(E,E,global(X),O) :-  env_get_global_var(E,X,O),!.

env_set_var(e(G,E), e(G,Eo), N, O) :- 
    select(var(N,V),E,_) -> (E=Eo,nb_setarg(1,V,O),!); Eo=[var(N,v(O))|E].

env_get_var(e(_,E), N, O) :-
    member(var(N,v(O)), E).

env_set_global_var(e(E,L), e(Eo,L), N, O) :- 
    select(var(N,V),E,_) -> (E=Eo,nb_setarg(1,V,O),!); Eo=[var(N,v(O))|E].

env_get_global_var(e(E,_), N, O) :-
    member(var(N,v(O)), E).

test(variables, O) :- (
    expect("x = 1 and x",1),
    expect("x = -1 and x",-1),
    expect("x = 1 and y = x and y",1),
    expect("x = 1 and x = x + 1",2),
    expect("$x = 1 and x = $x + 1",2),

    []) -> O = pass; O = fail.

% collections

% key value pairs
infix_(strkeyvalue, right, 20) --> "=>".
build(strkeyvalue,id(X),O,keyvalue(string(X),O)).
infix(keyvalue, right, 20) --> ":".
build(keyvalue,X,O,keyvalue(X,O)).

% list seperator
lazyinfix(pair, left, 70) --> ",".

% tuple constuction
build(pair,keyvalue(K,V),keyvalue(K1,V1),O) :- O=tuple([],[K-V,K1-V1]).
build(pair,B,keyvalue(K,V),O) :- O=tuple([B],[K-V]).
build(pair,keyvalue(K,V),B,O) :- B=tuple(L,P) -> O=tuple(L,[K-V|P]); O=tuple([B],[K-V]).
build(pair,A,B,O) :- B=tuple(L,P) -> O=tuple([A|L],P); O=tuple([A,B],[]).
build(pair,keyvalue(K,V),tuple([],[K-V])).
build(pair,A,tuple([A],[])).

eval(E,E,tuple(T,P),tuple(T,P)).

% table construction
empty_table(table([],[])).
eval(E,E,table(T,P),table(T,P)).

item(O) --> "[", ws0, expr(I), ws0, "]", build_(table, I,O).
item(O) --> "[", ws0, "]", build_(table, [],O).

build(table, tuple(L), call(table,L)).
build(table, [], call(table,[])).
build(table, L, call(table,[L])).

eval_call(E,Eo,table,[tuple(L,P)],T) :- eval_list(E,E1,L,Lo),!, eval_pairs(E1,Eo,P,Po), !, make_table(T,Lo,Po).
eval_call(E,E,table,[],table([],[])) :-!.

make_table(table([],[]),[],[]).
make_table(table([var(Hl)|A],[var(Hp)|B]),[Hl|L],[Hp|P]) :- make_table(table(A,B),L,P).
make_table(table([var(Hl)|A],B),[Hl|L],[]) :- make_table(table(A,B),L,[]).
make_table(table(A,[var(Hp)|B]),[],[Hp|P]) :- make_table(table(A,B),[],P).


eval_pairs(E,E,[],[]) :- !.
eval_pairs(E,Eo,[K-V|T],[K-Vo|To]) :- eval(E,E1,V,Vo),!, eval_pairs(E1,Eo, T,To),!.

table_append(T,K-V) :- T = table(_,P),nb_setarg(1,T,[K-V|P]),!. 
table_append(T,I) :- T = table(L,_), append(L,[I],L1), nb_setarg(1,T,L1),!. 

% index operator
postfix_expr(index,I,5) --> "[", ws0, expr(I), ws0, "]",!.

index(table(L,_),K,O) :- number(K), nth0(K,L,var(O)),!.
index(table(_,P),K,O) :- memberchk(var(K-O),P).
index(tuple(L,_),K,O) :- number(K), nth0(K,L,O),!.
index(tuple(_,P),K,O) :- memberchk(K-O,P).


eval_call(E,Eo,index,[T,K],O) :-
    eval(E,E1,T,T1),!,
    eval(E1,Eo,K,K1),!,
    index(T1,K1,O).


% fixme, start writing index_assign ?
eval_call(E,Eo, assign,[call(index,A),R], R1) :-
    eval(E,E1,A,[I,V]), !,
    eval(E1,E2,R,R1) , !,
    index_assign(E2,Eo,I,V,R1).

%index_assign(_,_,I,_,_) :- number(I), !, fail. 
%index_assign(E,Eo,K,T,V) :- select 

% fixme - introduce settable arguments?or just cheap append/pop

eval_call(E,Eo,assign,[tuple(L,P),Lexp],O1) :- 
    !,
    eval(E,E1,Lexp,O),
    !,
    do_assign(E1,Eo,tuple(L,P),O,O1).

eval_call(E,Eo,assign,[call(index,[T,I]),R], Rexp) :-
    eval(E,E1,T,Texp),
    eval(E1,E2,I,Iexp),
    !,
    get_index_var(Texp,Iexp,V),
    eval(E2,Eo,R,Rexp),
    !,
    set_index_var(I,V,Rexp).

set_index_var(I,V,Rexp) :-
    V = var(I-_),!,
    nb_setarg(1,V,I-Rexp),
    !.

set_index_var(_,V,Rexp) :-
    nb_setarg(1,V,Rexp),
    !.

do_assign(E,Eo, tuple(L,P),O, tuple([A|T],P1)) :- 
    get_list_iterable(O,R,Rt),
    !,get_list_iterable(tuple(L,P),H,Lt), 
    !,
    do_assign(E,E1,H,R,A),!,
    do_assign(E1,Eo,Lt,Rt, tuple(T,P1)).

do_assign(E,Eo, tuple(L,P),O, tuple(L,[K1-A|T])) :- 
    get_pair_iterable(tuple(L,P),keyvalue(K,V),Lt), 
    eval(E,E1,K,K1),
    get_pair_iterable(O,keyvalue(K1,V1),Rt),
    !,
    do_assign(E1,E2,V,V1,A),!,
    do_assign(E2,Eo,Lt,Rt, tuple(L,T)).

do_assign(E,E, tuple([],[]), A, tuple([],[])) :- empty_iterable(A).

:- discontiguous get_iterable/3, empty_iterable/1.

empty_iterable(tuple([],[])).
empty_iterable(table([],[])).

get_index_var(table(L,_), I,V) :- number(I), nth0(I,L,V),!.
get_index_var(table(_,P), K, var(K-V)) :- select(var(K-V),P,_),!.
get_index_var(T, K, O) :- T=table(_,P), O = var(K-_), nb_setarg(2,T,[O|P]),!.

get_list_iterable(tuple([H|T],P), H, tuple(T,P)).
get_list_iterable(table([var(H)|T],P), H, table(T,P)).
get_pair_iterable(tuple([],P), keyvalue(K,V), tuple([],Pt)) :- select(K-V,P,Pt).
get_pair_iterable(table([],P), keyvalue(K,V), table([],Pt)) :- select(var(K-V),P,Pt).

test(collections, O) :- (
    expect("x,y,z = 1,2,3 and x == 1",1),
    expect("x,y,z = 1,2,3, and x == 1",1),
    expect("x,y,z, = 1,2,3, and x == 1",1),
    expect("x = 1,2,3, and x[1] == 2",2),
    expect("x,y = [1,2] and y == 2",2),
    expect("x = 1,2,3 and x[0] == 1",1),
    expect("x = a=>1,b=>2,c=>3 and x[\"a\"] == 1",1),
    expect("x = a=>1,b=>2,c=>3 and x[\"a\"] == 1",1),
    expect("z = \"a\" and  z:x,b=>y = b=>1,a=>2, and y",1),
    expect("x = [1,2] and x[0]=2 and x[0]",2),
    expect("x = [] and x[\"a\"] = 1 and x[\"a\"]",1),
    []) -> O = pass; O = fail.




%% dev log
% done, initial sekelton recovered from haklog 
% done, reordered code + documentation
% done, handle backtracking assignment properly
% done, remove backtracking - use return or fail as expressions
% done, adding $foo identifiers 
% done, string literals
% done, tuple and table syntax, keyvalue operators and lazy to seperate/terminate lists.
% done, tuple assignment over lists
% done, tuple assignment over key-values, 
% done, indexing key-values

% todo, improve test coverage, measure test coverage, profile, etc.

% todo, iterative interface ? one for reading one for writing?
% todo, index assignment for tables.

% needs doing
% todo, collections a :b,a =>b, tuples , 
% todo, if case and other flow control
% todo, infix for at high operator precedence, i.e 110 or summat.

% needs thinking
% todo lexical scope via stack
% todo bytecode - lua, python?
% todo, implement functions, closures ala lua.
    
% todo, $stdin, $stdout, $stderr, $args
%       print would be good?

% todo, test defintions, assertions, performance tests
%       quickcheck like universals, fuzzing?



% todo, string ops, collection ops
% todo, list comprehensions
% todo, generators
% todo, itertools like operations on lists and generators.
% todo, regular expressions, pattern matching
% todo, aggregate operators - reduction i.e (a,b :- a+b) over l
%                             maybe l = aggregate x {
%                                         a+b -> a+b
%                                    }     

% todo, exceptions
% todo, modules
% todo, signals
% todo, datetime handling, calendars, timezones
% toto, prelude
% todo, pipes
% todo, join like operator on select (see jerlang)
% todo, queries
% todo, processes
% todo, urls, file://
% todo, http access, sockets
% todo, file operations, pipe operations

% milestones:
%    some built ins written in nezha
%    bytecode format
%    vm? pypy heh


% future
% maybe, output language: scheme, prolog, etc
% maybe, optional types (mutated from go) - interfaces and duck type inference
% todo,  'use' and 'feature' to define new langauge features. essentially
%        a meta system to extend the language,
% maybe, language toolkit as library, using features.
% maybe, self hosting

% thoughts: build parser and runtime into databases to allow mutability.    
% link these up in a pipline, assert(main(in, out) :-...), then
% recorded(..,main(A,B)),recorded(..,main(B,C))...

%% new features go here.


% placeholder test
test(placeholder, O) :- (
    []
    ) -> O = pass; O = fail. 

%% language engine starts here.

% useful functions for parsing:
% lookahead a token.
lookahead(X),X --> X.

% common parse tokens.
% whitespace helpers ws means specific whitespace, ws0 means any.
ws --> [X], {code_type(X, white)}, ws0.
ws0 --> ws.
ws0 --> [].

% hello cr lf.
newline --> [10], linefeed. 
linefeed --> [13]; [].

% helper functions to run the interpreter
% from shell - either loads a file or reads from stdin
% and calls exec(String, _).

start_script :-
    catch(main_script,E,(print_message(error,E),fail)),
    halt.
start_script :-
    halt.

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
run_file(File) :- 
    prompt(_,''),
    (\+File = [] -> (
        string_to_atom(File,Name),
        open(Name,read,I)
        );
        current_input(I)
    ),
    read_file(I,[],Code),
    exec(Code,_).


read_file(I,Li,Lo) :-
    get_byte(I,C),(
        (C = -1,!,Lo=[]);
        Lo=[C|L1], 
        read_file(I,Li,L1)
    ).  

% test runner is the last thing to run

test_out([]).
test_out([[_,pass]|T]) :- test_out(T).
test_out([[N,O]|T]) :- writef('%w %w\n',[O,N]), test_out(T).
:- findall([T,O],test(T,O), L),  test_out(L).


%%%% Bibliography
% Use of Prolog for developing a new programming language
% The Implementation of Lua 5.0
% A Nanopass Framework for Compiler Education
% xoc, ddc aomop
%
