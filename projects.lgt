:- object(create_project(_Project_),
    instantiates(meta_action),
    specializes(action)).
   poss_when(not existing_project(_Project_)).
:- end_object.

:- object(delete_project(_Project_),
    instantiates(meta_action),
    specializes(action)).
   poss_when(existing_project(_Project_)).
:- end_object.

:- object(start_session(_Project_),
    instantiates(meta_action),
    specializes(action)).
   poss_when(existing_project(_Project_)).
:- end_object.

:- object(end_session(_Project_),
    instantiates(meta_action),
    specializes(action)).
   poss_when(working_on(_Project_)).
:- end_object.

:- object(existing_project(_Project_),
    extends(fluent)).
   value(Sit) :-
       Sit::action_class(A),
       Sit::prior(Prior),
       ( A = create_project(_Project_)
       ; Prior::holds(existing_project(_Project_)),
         A \= delete_project(_Project_)
       ).
:- end_object.

:- object(working_on(_Project_),
    extends(fluent)).
   value(Sit) :-
       Sit::action_class(A),
       Sit::prior(Prior),
       ( A = start_session(_Project_)
       ; Prior::holds(existing_project(_Project_)),
         A \= end_session(_Project_)
       ).
:- end_object.

:- object(project_manager).

    :- public(sit/1).
    :- dynamic(sit/1).

    :- public(init/0).
    init :-
        persistency::restore(Sits),
        list::member(S, Sits),
        instantiates_class(S, situation), !,
        asserta(sit(S)).
    init :-
        situation::new([], s0),
        asserta(sit(s0)).

    :- public(do/1).
    do(Action) :-
        ::sit(S),
        situation::do(Action, S, NS),
        NS::action(A),
        persistency::persist(A),
        persistency::persist(NS),
        retractall(sit(_)),
        asserta(sit(NS)).

    :- public(current_projects/1).
    current_projects(Projects) :-
        ::sit(S),
        findall(P, S::holds(existing_project(P)), Projects).

    :- public(print_project/1).
    print_project(Project) :-
        ::sit(S),
        write('project, action, year, month, date, hour, minute\n'),
        print_project(Project, S).

    print_project(_, s0).
    print_project(Project, Sit) :-
        Sit::prior(Prior),
        Sit::action(A),
        print_project(Project, Prior),
        print_action(Project, A, Sit).

    print_action(Project, A, S) :-
        S::action_class(AC),
        arg(1, AC, Project),
        functor(AC, Action, _),
        A::at_datetime(datetime(date(YY, MM, DD), time(HH, Mi, _, _))),
        meta::map([F]>>(write(F), write(', ')), [Project, Action, YY, MM, DD, HH]),
        write(Mi), write('\n'), !.
    print_action(Project, _, S) :-
        S::action_class(AC),
        \+ arg(1, AC, Project).

:- end_object.
