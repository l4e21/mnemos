:- use_module(src/style, [elem_style/2, class_style/2, css_style_atom/3]).
:- use_module(src/core, [notes/2, book/2, elem_overrides/2]).
:- use_module(src/render, [render_notes/2, write_notes_to_html/1]).
:- use_module(src/server, [start_server/1, shutdown_server/1]).

% ?- start_server(4000).

% ?- debug(http(request)).

% ?- shutdown_server(4000).

% ?- write_notes_to_html(X).

% ?- render_notes_page(simulation_and_simulacra, _).
