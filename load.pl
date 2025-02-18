:- use_module(src/core, [page/2]).
:- use_module(src/encryption, [encrypt_files/2, decrypt_files/2, mnemos_aes_key/1]).
:- use_module(src/render, [render_page/2, write_page_to_html/1]).
:- use_module(src/server, [start_server/1, shutdown_server/1]).
:- use_module(src/style, [css_style_atom/3, styles/2]).

supplementary_files(["resources/supplements/core.pl"-"resources/supplements/core.enc",
                     "resources/supplements/style.pl"-"resources/supplements/style.enc"]) :- !.

% ?- setenv('MNEMOS_AES_KEY', "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa").

% ?- supplementary_files(Xs), mnemos_aes_key(Key), encrypt_files(Key, Xs).

% ?- supplementary_files(Xs), mnemos_aes_key(Key), decrypt_files(Key, Xs).

% ?- ["resources/supplements/core.pl", "resources/supplements/style.pl"].

% ?- start_server(4000).

% ?- debug(http(request)).

% ?- trace(shutdown_server/1).

% ?- shutdown_server(4000).

% ?- write_page_to_html(X).

% ?- trace(core:styles/2).

% ?- render_page(simulation_and_simulacra, H).

% ?- trace(read/2).

% ?- read_file_terms("resources/supplements/supplement.pl", Terms).

% ?- listing(page/2).

% ?- dict_pairs(_{a:3}, _, Xs).
