% -*- prolog -*-

%---------------------------------------------------------------------------
% Prelude
%---------------------------------------------------------------------------

% This file is the basis of an interactive tool a la SQL to query
% information about the structure of a codebase, for instance "What
% are all the children of class Foo?". The data is the code. The query
% language is Prolog (http://en.wikipedia.org/wiki/Prolog), a
% logic-based programming language used mainly in AI but also popular
% in database (http://en.wikipedia.org/wiki/Datalog). The particular
% Prolog implementation we use is SWI-prolog
% (http://www.swi-prolog.org/pldoc/refman/). We've chosen Prolog over
% SQL because it's really easy to define recursive predicates
% like children (see below) in Prolog, and such predicates are necessary
% when dealing with object-oriented codebase.
%
% This tool is inspired by a similar tool for Java called JQuery
% (http://jquery.cs.ubc.ca/, nothing to do with the JS library), itself
% inspired by CIA (C Information Abstractor). The code below is mostly
% generic (programming language agnostic) but it was tested only
% on PHP code for now.
%
% This file assumes the presence of another file, facts.pl, containing
% the actual "database" of facts about a codebase.
% Here are the predicates that should be defined in facts.pl:
%
%  - entities: kind/2 with the function/class/method/constant/field/... atoms.
%    ex: kind('array_map', function). kind(('Preparable', 'gen'), method).
%    The identifier for a function is its name in a string and for
%    class members a pair with the name of the class and then the member name,
%    both in a string. We don't differentiate methods from static methods;
%    the static/1 predicate below can be used for that (same for fields).
%
%  - callgraph: docall/3 with the function/method/class atoms to differentiate
%    regular function calls, method calls, and class instantiations via new.
%    ex: docall('foo', 'bar', function). docall(('A', 'foo'), 'toInt', method).
%    Note that for method calls we actually don't resolve to which class
%    the method belongs to (that would require to leverage results from
%    an interprocedural static analysis).
%    We use 'docall' and not 'call' because call is a reserved predicate
%    in Prolog.
%
%  - function/method arity (number of parameters): arity/2
%    ex: arity('foobar', 3). arity(('Preparable', 'gen'), 0).
%
%  - TODO: parameter information (types, ref)
%
%  - class/method properties: static/1, abstract/1, final/1
%    ex: static(('Filesystem', 'readFile')). abstract('AbstractTestCase').
%
%  - class members visibility: is_public/1, is_private/1, is_protected/1,
%    ex: is_public(('Preparable', 'gen')).
%    We use 'is_public' and not 'public' because public is a reserved name
%    in Prolog.
% 
%  - inheritance: extends/2, implements/2
%    ex: extends('EntPhoto', 'Ent'). implements('MyTest', 'NeedSqlShim').
%
%  - include/require: include/2
%    ex: include('wap/index.php', 'flib/core/__init__.php').
%    We don't differentiate 'include' from 'require'. Note that it works
%    on desugared flib code so the require_module() are translated in
%    their equivalent includes. Finally path are resolved statically
%    when we can, so for instance include $THRIFT_ROOT . '...' is resolved
%    in its final path form 'lib/thrift/...'.
%
%  - position: at/3
%    ex: at(('Preparable', 'gen'), 'flib/core/preparable.php', 10).
%
%  - file information: file/2
%    ex: file('wap/index.php', ['wap','index.php']).
%
% related work:
%  - jquery, tyruba
%  - CIA
%  - LFS
%  - ODASA

%---------------------------------------------------------------------------
% How to run/compile
%---------------------------------------------------------------------------

% Generates a /tmp/facts.pl for your codebase for your programming language
% (e.g. with pfff_db_heavy -gen_prolog_db /tmp/pfff_db /tmp/facts.pl)
% and then:
%
%   $ swipl -s /tmp/facts.pl -f rules.pl
%
% or compiles a version with
%
%   $ swipl -c /tmp/facts.pl rules.pl; ./a.out
%
% or use a precompiled version with
%
%   $ cmf --prolog or /home/engshare/pfff/prolog_www
%

%---------------------------------------------------------------------------
% Inheritance
%---------------------------------------------------------------------------

extends_or_implements(Child, Parent) :- 
        extends(Child, Parent).
extends_or_implements(Child, Parent) :- 
        implements(Child, Parent).

public_or_protected(X) :-
        is_public(X).
public_or_protected(X) :-
        is_protected(X).

method_or_field(method).
method_or_field(field).


children(Child, Parent) :-
        extends_or_implements(Child, Parent).
children(GrandChild, Parent) :-
        extends_or_implements(GrandChild, Child),
        children(Child, Parent).

% only extends
inherits(Child, Parent) :-
        extends(Child, Parent).
inherits(GrandChild, Parent) :-
        extends(GrandChild, Child),
        inherits(Child, Parent).


parent(X, Y) :-
        children(Y, X).

% bidirectional
related(X, Y) :-
        children(X, Y).
related(X, Y) :-
        children(Y, X).

%---------------------------------------------------------------------------
% Class information
%---------------------------------------------------------------------------

% one can use the same predicate in many ways in Prolog :)
method_in_class(X, Method) :- 
        kind((X, Method), method).
class_defining_method(Method, X) :- 
        kind((X, Method), method).

% get all methods/fields accessible from a class
method(Class, (Class, Method)) :- kind((Class, Method), method).
method(Class, (Class2, Method)) :- 
        extends(Class, Parent), 
        method(Parent, (Class2, Method)),
        % don't care about the one we have actually overriden here
        \+ kind((Class, Method), method),
        public_or_protected((Class2, Method)).

field(Class, (Class, Field)) :- kind((Class, Field), field).
field(Class, (Class2, Field)) :- 
        extends(Class, Parent),
        field(Parent, (Class2, Field)),
        % don't care about the one we have actually overriden here
        \+ kind((Class, Field), field),
        public_or_protected((Class2, Field)).

all_methods(Class) :- findall(X, method(Class, X), XS), writeln(XS).
all_fields(Class) :- findall(X, field(Class, X), XS), writeln(XS).

% for aran
at_method((Class, Method), File, Line) :-
        method(Class, (Class2, Method)), 
        at((Class2, Method), File, Line).

overrides(ChildClass, Method) :-
        kind((ChildClass, Method), method),
        inherits(ChildClass, ParentClass),
        kind((ParentClass, Method), method).

overrides(ChildClass, ParentClass, Method) :-
        kind((ChildClass, Method), method),
        inherits(ChildClass, ParentClass),
        kind((ParentClass, Method), method).

%---------------------------------------------------------------------------
% Statistics
%---------------------------------------------------------------------------

% does not work very well with big data :(
%:- use_module(library('R')).
%load_r :- r_open([with(non_interactive)]).

%---------------------------------------------------------------------------
% Reporting
%---------------------------------------------------------------------------


%---------------------------------------------------------------------------
% Clown code
%---------------------------------------------------------------------------

too_many_params(X) :-
        arity(X, N), N > 20.

%todo: histogram for kent of function arities :)

include_not_www_code(X, Y) :-
        include(X, Y),
        \+ file(Y, _).


% this is what makes the callgraph for methods more complicated
same_method_in_unrelated_classes(Method, Class1, Class2) :-
        kind((Class1, Method), method),
        kind((Class2, Method), method),
        Method \= '__construct',
        Class1 \= Class2,
        \+ related(Class1, Class2). 

%classes with more than 10 public methods: http://en.wikipedia.org/wiki/.QL
too_many_public_methods(X) :-
        kind(X, class), 
        findall(M, (kind((X, M), method), public((X,M))), Res), 
        length(Res, N), 
        N > 10.

could_be_final(Class) :-
  kind(Class, class),
  not(final(Class)),
  not(extends(_Child, Class)).

could_be_final(Class, Method) :-
  kind(Class, class),
  kind((Class, Method), method),
  not(final((Class, Method))),
  not(overrides(_ChildClass, Class, Method)).

%---------------------------------------------------------------------------
% checks
%---------------------------------------------------------------------------

check_duplicated_entity(X, File1, File2, Kind) :-
        kind(X, Kind), 
        at(X, File1, _),
        at(X, File2, _),
        File1 \= File2.

check_duplicated_field(Class, Class2, Var) :-
        kind((Class,Var), field),
        public_or_protected((Class, Var)),
        Class \= 'Exception',
        children(Class2, Class), 
        kind((Class2, Var), field).
