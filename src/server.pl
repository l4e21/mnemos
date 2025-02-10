:- module(server, [start_server/1, shutdown_server/1]).

:- use_module(render, [render_notes/2]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

%% Root Handling
% https://www.swi-prolog.org/pldoc/man?section=httpdispatch
:- http_handler(root(notes/Doc), render_notes_page(Doc), []).

render_notes_page(Doc, _Req) :-
    render_notes(Doc, Html),
    format("Content-type: text/html~n~n"),
    format(Html).

start_server(Port) :- http_server(http_dispatch, [port(Port)]).

shutdown_server(Port) :- http_stop_server(Port, _).

%% Debugging Aids
%% https://www.swi-prolog.org/howto/http/Developing.md
%% Sweeprolog doesn't play nicely with these (not capturing channel outputs/inputs)
%% That is another reason to not use it!


% ?- start_server(4000).
%@ % Started server at http://localhost:4000/
%@ true.

% ?- shutdown_server(4000).
