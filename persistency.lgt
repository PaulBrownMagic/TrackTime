:- object(persistency,
    imports(restore_reprc)).

   :- info([ version is 1.0
           , author is 'Paul Brown'
           , date is 2019/10/4
           , comment is 'Persist instances of objects to a file'
           ]).

   :- private(file/1).
   file('persistent_storage.pl').

   :- public(persist/1).
   :- mode(persist(+object), zero_or_one).
   :- info(persist/1,
       [ comment is 'Persist the object to the file provided the object has a repr predicate'
       , argnames is ['Object']
       ]).
   persist(Object) :-
       atom(Object), current_object(Object), Object::current_predicate(repr/1),
       Object::repr(Repr),
       setup_call_cleanup(open_file(Stream), (writeq(Stream, Repr), write(Stream, '.\n')), close(Stream)), !.

   % open_file in write mode if it doesn't exist, else append mode
   open_file(Stream) :-
       ::file(File),
       ( os::file_exists(File) -> Mode = append ; Mode = write ),
       open(File, Mode, Stream).

   :- public(restore/1).
   :- mode(restore(-list), one).
   :- info(restore/1,
       [ comment is 'Restore all persisted objects. Names of objects cannot be guaranteed, therefore the restored object names are returned in reverse order.'
       , argnames is ['Objects']
       ]).
   restore(Objects) :- var(Objects),
       read_terms(Terms),
       restore_terms(Terms, [], [], Objects), !.

   % read in a list of repr Terms from persistent file
   read_terms(Terms) :-
       ::file(File), os::file_exists(File),
       setup_call_cleanup(open(File, read, Stream), read_stream_to_terms(Stream, Terms), close(Stream)).

   % For each term read, restore the object it represented
   restore_terms([], _, Objects, Objects).
   restore_terms([instance(ID, Class, Props)|T], Mapping, Acc, Objects) :-
       map_props(Props, Mapping, MappedProps),
       ::restore_repr(instance(ID, Class, MappedProps), NewID),
       ( ID == NewID
       -> restore_terms(T, Mapping, [ID|Acc], Objects)
       ; restore_terms(T, [map(ID, NewID)|Mapping], [NewID|Acc], Objects)
       ).

   % Traverse a list of properties to map the objects found therein
   map_props([], _, []).
   map_props([H|T], Mapping, [NH|MappedProps]) :-
       map_prop(H, Mapping, NH),
       map_props(T, Mapping, MappedProps).

   % Map a property, drill down to the atomic terms and replace them if they're in Mapped
       % case list
   map_prop(Prop, Mapping, NewProp) :-
       is_list(Prop),
       map_props(Prop, Mapping, NewProp).
       % case compound term
   map_prop(Prop, Mapping, NewProp) :-
       compound(Prop),
       functor(Prop, Functor, Arity),
       functor(NewProp, Functor, Arity),
       Prop =.. [F|Args],
       NewProp =.. [F|NewArgs],
       map_props(Args, Mapping, NewArgs).
      % case atomic
   map_prop(Prop, Mapping, NewProp) :-
       atomic(Prop),
       ( list::member(map(Prop, NewProp), Mapping)
       ; \+ list::memberchk(map(Prop, _), Mapping), Prop = NewProp
       ).


   % read in a list of terms from a stream
   read_stream_to_terms(Stream, Terms) :-
       read(Stream, Case),
       read_stream_to_terms(Case, Stream, H-H, Terms).

   read_stream_to_terms(end_of_file, _Stream, Terms-[], Terms) :- !.
   read_stream_to_terms(Case, Stream, Acc-[Case|Hole], Terms) :-
       read(Stream, NextCase),
       read_stream_to_terms(NextCase, Stream, Acc-Hole, Terms).

:- end_object.
