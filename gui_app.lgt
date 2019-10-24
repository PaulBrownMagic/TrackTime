:- object(xpce).
   :- include('xpce_includes.lgt').

   :- public(init/0).

   :- public(id/1).
   id(@ID) :-
       self(Self),
       functor(Self, ID, _).

   :- public(size/2).
   size(W, H) :-
       ::get(size, size(W, H)).

   :- public(new/1).
   new(O) :-
       ::id(ID),
       xnew(ID, O).

   :- public(get/2).
   get(P, O) :-
       ::id(ID),
       xget(ID, P, O).

   :- public(selected_key/1).
   selected_key(O) :-
       ::get(selection, S),
       xget(S, key, O).

   :- public(send/1).
   send(M) :-
       ::id(ID),
       xsend(ID, M).

   :- public(send/2).
   send(P, O) :-
       ::id(ID),
       xsend(ID, P, O).

   :- public(send/3).
   send(P, O, M) :-
       ::id(ID),
       xsend(ID, P, O, M).

   :- public(free/0).
   free :-
       ::id(ID),
       xfree(ID).

:- end_object.


:- object(window,
    extends(xpce)).

   init :-
       ^^new(frame('TrackTime')),
       ^^send(open).

   :- public(append/1).
   append(O) :-
       O::id(ID),
       ^^send(append, ID).

   :- public(append/3).
   append(O, Dir, Ref) :-
       O::id(OID),
       ^^send(append, OID),
       Ref::id(Rid),
       Where =.. [Dir, Rid],
       ^^send(OID, Where).

:- end_object.

:- object(project_browser,
    extends(xpce)).

   init :-
       ^^new(browser),
       ::update,
       self(Self),
       window::append(Self).

   :- public(update/0).
   update :-
       project_manager::current_projects(Projects),
       ^^send(members(Projects)).

:- end_object.

:- object(app_dialog,
    extends(xpce)).
    btn(button(new_project, logtalk(new_project_dialog, init))).
    btn(button(delete_project, logtalk(app, delete_project))).
    btn(button(start_work, logtalk(app, start_work))).
    btn(button(stop_work, logtalk(app, stop_work))).
    btn(button(exit, logtalk(app, close))).

    init :-
        ^^new(dialog),
        forall(btn(B), ^^send(append(B))),
        self(Self),
        window::append(Self),
        ^^send(below(@project_browser)).

:- end_object.


:- object(project_name,
    extends(xpce)).
   init :-
       ^^new(text_item(new_project_name)).
:- end_object.

:- object(new_project_dialog,
    extends(xpce)).
    btn(button(save, logtalk(new_project_dialog, save))).
    btn(button(cancel, logtalk(new_project_dialog, close))).

    init :-
        ^^new(dialog('New Project')),
        project_name::init,
        ::append(project_name),
        forall(btn(B), ^^send(append(B))),
        ^^send(open).

    :- public(close/0).
    close :-
        project_name::free,
        ::free.

    :- public(save/0).
    save :-
        project_name::get(selection, ProjectName),
        project_manager::do(create_project(ProjectName)),
        project_browser::update,
        ::close.

   :- public(append/1).
   append(O) :-
       O::id(ID),
       ^^send(append, ID).

:- end_object.

:- object(app).

   :- public(init/0).
   init :-
       project_manager::init,
       window::init,
       project_browser::init,
       app_dialog::init, !.

   :- public(close/0).
   close :-
       app_dialog::free,
       project_browser::free,
       window::free,
       project_manager::close_out.

   :- public(delete_project/0).
   delete_project :-
       project_browser::selected_key(P),
       project_manager::do(delete_project(P)),
       project_browser::update.

   :- public(start_work/0).
   start_work :-
       project_browser::selected_key(P),
       writeln(P),
       project_manager::do(start_session(P)).

   :- public(stop_work/0).
   stop_work :-
       project_browser::selected_key(P),
       writeln(P),
       project_manager::do(end_session(P)).

:- end_object.
