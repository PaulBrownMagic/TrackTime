:- op(800, xfy, and).
:- op(850, xfy, or).
:- op(870, xfy, implies).
:- op(880, xfy, equivalentTo).
:- op(200, fy, not).


:- category(reprc).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'Instance representations'
            , remarks is ['Note'-'`repr_props/1` must be a current predicate in the instance']
            ]).

    :- public(repr/1).
    :- mode(repr(-term), zero_or_one).
    :- info(repr/1,
        [ comment is 'Generate instance representation'
        , argnames is ['Repr']
        ]).
    repr(instance(Self, Class, Props)) :-
        self(Self),
        instantiates_class(Self, Class),
        ::repr_props(Props).

    :- public(repr_props/1).
    :- mode(repr_props(-list), zero_or_one).
    :- info(repr_props/1,
        [ comment is 'Declare such that it is a current predicate on the instance being represented'
        , argnames is ['Props']
        , remarks is ['Prop Representation'-'Recommend to do this as terms to mirror those in the object: `[foo(bar), num(1), ...]`']
        ]).
:- end_category.


:- category(restore_reprc).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/4
            , comment is 'Restore objects from instance representations'
            , remarks is ['Note'-'`restore_repr/1` must be a current predicate in the class']
            ]).

    :- public(restore_repr/2).
    :- mode(restore_repr(+term, -object), zero_or_one).
    :- info(restore_repr/2,
        [ comment is 'Create action instance from repr if it does not exist'
        , argnames is ['Repr', 'Action']
        ]).
    % Already exists, don't recreate just return
    restore_repr(instance(A, Class, Props), A) :-
        Class::instance(A), A::repr_props(Props).
    % Already exists with different name, don't recreate just return
    restore_repr(instance(A, Class, Props), B) :-
        Class::instance(B), A \= B, B::repr_props(Props).
    % Name available, use it
    restore_repr(instance(A, Class, Props), A) :-
        atom(A), \+ current_object(A),
        Class::instantiate(A, Props).
    % Name taken, return new name
    restore_repr(instance(A, Class, Props), B) :-
        atom(A), current_object(A), \+ A::repr_props(Props),
        Class::instantiate(B, Props).

:- end_category.


:- object(metaclass,
    instantiates(metaclass),
    imports(class_hierarchy)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'An object describing a class with an instantiate method for creating instances and importing methods to traverse the subsumption heirarchy.'
            ]).

    :- public(instantiate/2).
    :- mode(instantiate(-object, +list), zero_or_one).
    :- info(instantiate/2,
        [ comment is 'Create a new instance of self'
        , argnames is ['Instance', 'Clauses']
        ]).
    instantiate(Instance, Clauses) :-
        self(Class),
        create_object(Instance, [instantiates(Class)], [], Clauses).

:- end_object.


:- object(fluent,
    imports(proto_hierarchy)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A fluent is a prototype. It is identified by the relationship that can hold in some situations. The value(s) this relationship holds between in a situation, if any, depend upon the situation'
            ]).

    :- public(holds/1).
    :- mode(holds(-object), zero_or_more).
    :- mode(holds(+object), zero_or_one).
    :- info(holds/1,
        [ comment is 'What situations, if any, the fluent holds in. If the fluent holds, it is true in that situation.'
        , argnames is ['Situation']
        ]).
    holds(S) :-
        instantiates_class(S, situation),
        self(Self),
        S::holds(Self).

    :- public(value/1).
    :- mode(value(+object), zero_or_more).
    :- info(value/1,
        [ comment is 'Ground the value(s) of the fluent for a given situation.'
        , argnames is ['Situation']
        , remarks is ['Note'-'value/1 needs to be defined in the extension of fluent']
        ]).
:- end_object.


:- object(meta_action,
    imports(class_hierarchy),
    specializes(metaclass)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A specialization of metaclass for action classes.'
            ]).

   :- public(new/1).
   :- mode(new(+object), zero_or_one).
   :- info(new/1,
       [ comment is 'Create an instance of the action at the current time, with no specific actor doing the action.'
       , argnames is ['Action']
       ]).
   new(Action) :-
       os::date_time(Y, Mo, D, H, Mi, S, Ms),
       ::instantiate(Action, [time_(datetime(date(Y, Mo, D), time(H, Mi, S, Ms)))]).

   :- public(new/2).
   :- mode(new(+object, +object), zero_or_one).
   :- info(new/2,
       [ comment is 'Create an instance of the action at the current time, with a specific actor doing the action.'
       , argnames is ['Actor', 'Action']
       ]).
   new(Actor, Action) :-
       os::date_time(Y, Mo, D, H, Mi, S, Ms),
       ::instantiate(Action, [actor_(Actor), time_(datetime(date(Y, Mo, D), time(H, Mi, S, Ms)))]).

   :- public(poss/1).
   :- mode(poss(+object), zero_or_one).
   :- mode(poss(-object), zero_or_more).
   :- info(poss/1,
       [ comment is 'Determine if this kind of action is possible in a/the situation. Does not take into account actors.'
       , argnames is ['Situation']
       ]).
    poss(S) :-
        new(A),
        A::poss(S),
        abolish_object(A).

    % poss/2 ? A::poss(Actor, S) is it possible for actor to do action in S?

:- end_object.


:- object(action,
    instantiates(meta_action),
    imports(reprc)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'An action class to be specialized by domain actions.'
            ]).

     :- private([ poss_when/1
                , time_/1
                , actor_/1
                ]).

    :- public(actor/1).
    :- mode(actor(?object), zero_or_one).
    :- info(actor/1,
        [ comment is 'The one who to whom the execution of the action is attributed.'
        , argnames is ['Actor']
        ]).
    actor(A) :-
        ::actor_(A).

    :- public(at_datetime/1).
    :- mode(at_datetime(?term), zero_or_one).
    :- info(at_datetime/1,
        [ comment is 'The date and time at which the action was done, assumes actions have no duration.'
        , argnames is ['DateTime']
        ]).
    at_datetime(DT) :-
        ::time_(DT).

    :- public(at_time/1).
    :- mode(at_time(?term), zero_or_one).
    :- info(at_time/1,
        [ comment is 'The time at which the action was done, assumes actions have no duration.'
        , argnames is ['Time']
        ]).
    at_time(T) :-
        ::time_(datetime(_, T)).

    :- public(at_date/1).
    :- mode(at_date(?term), zero_or_one).
    :- info(at_date/1,
        [ comment is 'The date on which the action was done, assumes actions have no duration.'
        , argnames is ['Date']
        ]).
    at_date(D) :-
        ::time_(datetime(D, _)).

    :- public(poss/1).
    :- mode(poss(+object), zero_or_one).
    :- mode(poss(-object), zero_or_more).
    :- info(poss/1,
       [ comment is 'Determine if this instance of action is possible in a/the situation.'
       , argnames is ['Situation']
       ]).
    poss(S) :-
        instantiates_class(S, situation),
        ::poss_when(Cond),
        S::holds(Cond).

    repr_props(Props) :-
         ::time_(T),
         ( ::actor(A), Props = [actor(A), time_(T)]
         ; \+ ::actor(A), Props = [time_(T)]
         ).

:- end_object.


:- object(metasituation,
    specializes(metaclass)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A specialization of metaclass for situation classes.'
            ]).


    :- public(new/2).
    :- mode(new(+list, ?object), zero_or_one).
    :- info(new/2,
        [ comment is 'Create an instance of an initial situation in which the provided fluents hold.'
        , argnames is ['Fluents', 'Situation']
        , remarks is ['Note:'-'Fluents should mirror those fluent prototype extensions in the domain and are expected to be ground.']
        ]).
    new(Fluents, Sit) :-
        meta::map([F, G]>>(G = holds_(F)), Fluents, HoldingFluents),
        ::instantiate(Sit, HoldingFluents).


    :- public(do/3).
    :- mode(do(+object, +object, ?object), zero_or_one).
    :- info(do/3,
        [ comment is 'If the action is possible in the first situation, the second is a result of doing that action. The action is not attributed to any actor.'
        , argnames is ['Action', 'Situation1', 'Situation2']
        ]).
    do(A, Sit, NewSit) :-
        A::poss(Sit), % Change to abolish the action only if not possible, same for do/4
        A::new(Action),
        !, ::instantiate(NewSit, [action(Action), prior_(Sit)]).

     :- public(do/4).
     :- mode(do(+object, +object, +object, ?object), zero_or_one).
     :- info(do/4,
         [ comment is 'If the action is possible in the first situation, the second is a result of the actor doing that action.'
         , argnames is ['Actor', 'Action', 'Situation1', 'Situation2']
         ]).
    do(Actor, A, Sit, NewSit) :-
        A::poss(Sit),
        A::new(Actor, Action),
        !, ::instantiate(NewSit, [action(Action), prior_(Sit)]).

    extension(F, O) :-
        extends_object(F, O).
    extension(F, O) :-
        extends_object(Z, O),
        extension(F, Z).

:- end_object.


:- object(situation,
    instantiates(metasituation),
    imports(reprc)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'A situation is defined by the fluents that hold in it and its history of actions.'
            ]).

   :- private([ holds_/1
              , prior_/1
              ]).

   :- public(action/1).
   :- mode(action(?object), zero_or_one).
   :- info(action/1,
       [ comment is 'The action done to transition from the prior situation to the current one'
       , argnames is ['Action']
       ]).

   :- public(action_class/1).
   :- mode(action_class(?object), zero_or_one).
   :- info(action_class/1,
       [ comment is 'The class the action done to transition from the prior situation to the current one instantiates'
       , argnames is ['Action']
       ]).
   action_class(C) :-
       ::action(A),
       instantiates_class(A, C).

   :- public(history/1).
   :- mode(history(-object), zero_or_one).
   :- info(history/1,
       [ comment is 'A transitive closure of prior situations. If Situation is not ground, results are returned by most recent first.'
       , argnames is ['Situation']
       ]).
   history(Sit) :-
       ::prior_(Sit).
   history(Sit) :-
       ::prior_(S),
       S::history(Sit).

   :- public(prior/1).
   :- mode(prior(?object), zero_or_more).
   :- info(prior/1,
       [ comment is 'Prior situation to the current one.'
       , argnames is ['Situation']
       ]).
   prior(Sit) :-
       ::prior_(Sit).

   :- public(holds/1).
   :- mode(holds(?object), zero_or_more).
   :- mode(holds(+term), zero_or_more).
   :- info(holds/1,
       [ comment is 'What fluents hold in the situation. Can also be provided with a logical query term provided all fluents are nonvar: `s0::holds(power(X) and position(Y)).`'
       , argnames is ['Holding']
       ]).
   holds(Q) :-
       ( var(Q) ; \+ compound_query(Q) ),
       ::holds_(Q).
   holds(Q) :-
       nonvar(Q),
       compound_query(Q),
       query(Q).

   holds_(F) :-
       ::prior(_),
       self(Self),
       fluent::descendant(F),
       F::value(Self).
   holds_(F) :-
       nonvar(F),
       \+ fluent::descendant(F),
       meta::call(F).

   :- public(poss/1).
   :- mode(poss(+object), zero_or_one).
   :- mode(poss(-object), zero_or_more).
   :- info(poss/1,
       [ comment is 'True iff. Action is an action and it is possible in the situation.'
       , argnames is ['Action']
       ]).
   poss(A) :-
       self(Self),
       action::descendant_class(A),
       A::poss(Self).
       % Will need to make action and abolish to allow for more efficient metasituation::do/3-4


   % Transform holds query into single fluent subgoals and see if they hold
   query(P and Q) :- nonvar(P), nonvar(Q),
       query(P), query(Q).
   query(P or Q) :- nonvar(P), nonvar(Q),
       query(P); query(Q).
   query(P implies Q) :- nonvar(P), nonvar(Q),
       query(not P or Q).
   query(P equivalentTo Q) :- nonvar(P), nonvar(Q),
       query((P implies Q) and (Q implies P)).
   query(not (not P)) :- nonvar(P),
       query(P).
   query(not (P and Q)) :- nonvar(P), nonvar(Q),
       query(not P or not Q).
   query(not (P or Q)) :- nonvar(P), nonvar(Q),
       query(not P and not Q).
   query(not (P implies Q)) :- nonvar(P), nonvar(Q),
       query(not (not P or Q)).
   query(not (P equivalentTo Q)) :- nonvar(P), nonvar(Q),
       query(not ((P implies Q) and (Q implies P))).
   query(not P) :- nonvar(P), \+ compound_query(P),
       \+ ::holds_(P).
   query(P) :- \+ compound_query(P),
       ::holds_(P).

   % test if arg is a query term that requires transformation
   compound_query(not _).
   compound_query(_ and _).
   compound_query(_ or _).
   compound_query(_ implies _).
   compound_query(_ equivalentTo _).

   repr_props(Props) :-
      ( ::prior_(Prior), ::action(A), Props = [action(A), prior_(Prior)]
      ; \+ ::prior(_), findall(holds_(F), ::holds(F), Props)
      ).

:- end_object.


:- object(actor,
    instantiates(metaclass),
    imports(reprc)).

    :- info([ version is 1.0
            , author is 'Paul Brown'
            , date is 2019/10/2
            , comment is 'An actor is the class of entities that can the execution of actions can be attributed to.'
            ]).

    :- public([ name/1
              , do/3
              ]).

    :- mode(do(+object, +object, ?object), zero_or_one).
    :- info(do/3,
        [ comment is 'Situation2 is the result of the actor doing Action in Situation1'
        , argnames is ['Action', 'Situation1', 'Situation2']
        ]).
    do(A, S1, S2) :-
        self(Self),
        situation::do(Self, A, S1, S2).

    repr_props([name(Name)]) :-
        ::name(Name).

:- end_object.
