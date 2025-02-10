
:- use_module(src/style, [elem_style/2, class_style/2, css_style_atom/3]).
:- use_module(src/core, [notes/2, book/2, elem_overrides/2]).
:- use_module(src/render, [render_notes/2, write_notes_to_html/1]).
:- use_module(src/server, [start_server/1, shutdown_server/1]).

read_file_terms(File, Terms) :-
    open(File, read, Stream),
    read_terms(Stream, Terms),
    close(Stream).

read_terms(Stream, [Term | Terms]) :-
    read(Stream, Term),
    Term \= end_of_file,
    read_terms(Stream, Terms).

read_terms(Stream, []) :-
    at_end_of_stream(Stream).

% ?- start_server(4000).

% ?- debug(http(request)).

% ?- shutdown_server(4000).

% ?- write_notes_to_html(X).

% ?- render_notes_page(simulation_and_simulacra, _).

% ?- trace(read/2).

% ?- read_file_terms("resources/supplement.pl", Terms).

% ?- listing(notes/2).

% ?- ["resources/supplement.pl"].

% ?- notes(X, _).
