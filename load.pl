
:- use_module(src/core, [page/2, book/2, elem_overrides/2]).
:- use_module(src/encryption, [encrypt_files/2, decrypt_files/2, mnemos_aes_key/1]).
:- use_module(src/render, [render_page/2, write_page_to_html/1]).
:- use_module(src/server, [start_server/1, shutdown_server/1]).
:- use_module(src/style, [elem_style/2, class_style/2, css_style_atom/3]).

supplementary_files(["resources/core_supplement.pl"-"resources/core_supplement.enc"]) :- !.

% ?- setenv('MNEMOS_AES_KEY', "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").
%@ true.

% ?- supplementary_files(Xs), mnemos_aes_key(Key), encrypt_files(Key, Xs).
% ?- supplementary_files(Xs), mnemos_aes_key(Key), decrypt_files(Key, Xs).

% ?- start_server(4000).

% ?- debug(http(request)).

% ?- shutdown_server(4000).

% ?- write_page_to_html(X).

% ?- render_page(simulation_and_simulacra, _).

% ?- trace(read/2).

% ?- read_file_terms("resources/supplement.pl", Terms).

% ?- listing(page/2).

% ?- ["resources/core_supplement.pl"].

