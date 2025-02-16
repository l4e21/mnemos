:- module(server, [start_server/1, shutdown_server/1]).

:- use_module(render, [render_page/2]).

:- use_module(library(http/http_files)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

%% Root Handling
% https://www.swi-prolog.org/pldoc/man?section=httpdispatch
:- http_handler(root('static/'), http_reply_from_files('resources/static/', []), [prefix]).
:- http_handler(root(Doc), render_page_webpage(Doc), []).

render_page_webpage(Doc, _Req) :-
    render_page(Doc, Html),
    format("Content-type: text/html~n~n"),
    format(Html).

render_page_webpage(_Doc, _Req) :-
    render_page(404, Html),
    format("Status: 404~n"),
    format("Content-type: text/html~n~n"),
    format(Html).

start_server(Port) :- http_server(http_dispatch, [port(Port)]).

shutdown_server(Port) :- http_stop_server(Port, []).

%% Debugging Aids
%% https://www.swi-prolog.org/howto/http/Developing.md
%% Sweeprolog doesn't play nicely with these (not capturing channel outputs/inputs)
%% That is another reason to not use it!

% ?- start_server(4000).

% ?- shutdown_server(4000).

