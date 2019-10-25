:- object(xpce).
   :- include('xpce_includes.lgt').

   :- public(init/0).

   :- public(id/1).
   id(@ID) :-
       self(Self),
       functor(Self, ID, _).

   :- public(object/0).
   object :-
       ::id(ID),
       xobject(ID).

   :- public(object/1).
   object(O) :-
       xobject(O).

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
    btn(button(select_project, logtalk(project_dialog, init))).
    btn(button(new_project, logtalk(new_project_dialog, init))).
    btn(button(exit, logtalk(app, close))).

    init :-
        ^^new(dialog),
        forall(btn(B), ^^send(append(B))),
        self(Self),
        window::append(Self),
        ^^send(below(@project_browser)).

:- end_object.

:- object(project_title(_P_),
    extends(xpce)).
    init :-
        ^^new(text(_P_)),
        ^^send(font, font(times, bold, 24)).

    :- public(update/0).
    update :-
        ^^free,
        init.

:- end_object.

:- object(start_time,
    extends(xpce)).

    init :-
        clear,
        project_manager::sit(S),
        S::action(A),
        A::at_time(time(H, M, _, _)),
        ( M < 10, atomic_list_concat(['Start-time ', H, ':0', M], Msg)
        ; M >= 10, atomic_list_concat(['Start-time ', H, ':', M], Msg)
        ),
        ^^new(text(Msg)),
        ^^send(font, font(times, normal, 18)).

    :- public(clear/0).
    clear :-
        ^^object,
        ^^free.
    clear.

:- end_object.

:- object(project_dialog_btns(_Mode_),
    extends(xpce)).

    init :-
        ^^new(dialog_group(buttons, group)),
        ^^send(gap, size(20, 30)),
        append_btns(_Mode_),
        ^^send(layout_dialog).

    append_btns(ready_start) :-
        ^^send(append, button(start_work, logtalk(app, start_work))),
        ^^send(append, button(delete_project, logtalk(app, delete_project)), right).

    append_btns(ready_stop) :-
        ^^send(append, button(stop, logtalk(app, stop_work))),
        ^^send(append, button(delete_project, logtalk(app, delete_project)), right),
        start_time::init,
        ^^send(append, @start_time, below).

    :- public(update/0).
    update :-
        ^^free,
        init.

:- end_object.

:- object(project_dialog,
    extends(xpce)).

    init :-
        % Exists, and working
        ^^object, project_manager::sit(S),
        S::holds(working_on(P)),
        project_manager::do(end_session(P)),
        update, !.
    init :-
        % Exists
        ^^object,
        update, !.
    init :-
        % Make new
        \+ ^^object,
        project_browser::selected_key(P),
        ^^new(dialog('Current Project')),
        project_title(P)::init,
        ^^send(display, @project_title),
        project_dialog_btns(ready_start)::init,
        ^^send(append, @project_dialog_btns),
        self(Self),
        window::append(Self),
        ^^send(right(@project_browser)).

    update :-
        project_browser::selected_key(P),
        project_title(P)::update,
        ^^send(display, @project_title),
        project_dialog_btns(ready_start)::update,
        ^^send(append, @project_dialog_btns),
        ^^send(layout_dialog).

    :- public(update/1).
    update(Mode) :-
        project_dialog_btns(Mode)::update,
        ^^send(append, @project_dialog_btns),
        ^^send(layout_dialog).

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
       project_manager::close_out,
       project_dialog_btns(_)::free,
       project_title(_)::free.

   :- public(delete_project/0).
   delete_project :-
       project_browser::selected_key(P),
       project_manager::do(delete_project(P)),
       project_browser::update,
       window::send(delete, @project_dialog).

   :- public(start_work/0).
   start_work :-
       project_browser::selected_key(P),
       project_manager::do(start_session(P)),
       project_dialog::update(ready_stop).

   :- public(stop_work/0).
   stop_work :-
       project_browser::selected_key(P),
       project_manager::do(end_session(P)),
       project_dialog::update(ready_start).

:- end_object.
