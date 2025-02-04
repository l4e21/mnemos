:- module(render, [render/3, write_notes_to_html/1]).

%% Global Styles
style(h2, _{fontsize:18}).
style(h1, _{color:red, fontsize: 18}).
style(text, _{}).
style(quote, _{implements:text, fontstyles:[italics]}).
style(img, _{}).

%% Priority: Meta > Style Predicate > Context
css_style_atom(fontsize-V, "font-size", V).
css_style_atom(color-V, "color", V).

style_opts(Id, Meta, ContextOpts, StyleOpts) :-
    (style(Id, Opts1); Opts1 = _{}),
    (get_dict(style, Meta, Opts2); Opts2 = _{}),
    put_dict(Opts2, Opts1, Opts3),
    put_dict(Opts3, ContextOpts, StyleOpts).

render_as_css_aux(K-V, Acc, AccNew) :-
    css_style_atom(K-V, KS, VS),
    format(string(AccNew), "~w  ~w: ~w;\n", [Acc, KS, VS]),
    !.

render_as_css_aux(_-_, Acc, Acc).

render_as_css(N, StyleOpts, CSS) :-
    atom_string(N, S),
    dict_pairs(StyleOpts, _, StylePairs),
    foldl(render_as_css_aux, StylePairs, "", StyleString),
    format(string(CSS), "~w {\n~w}\n", [S, StyleString]).

html_header_aux(StyleList, StyleString) :-
    foldl(string_concat, StyleList, "", StyleString).
    
html_header(Header) :-
    findall(CSS, (style(N, StyleOpts), render_as_css(N, StyleOpts, CSS)), StyleList),
    write(StyleList),
    html_header_aux(StyleList, StyleString),
    format(string(Header),
           "<html>\n<head>\n<style>\n~w</style>\n</head>\n<body>\n",
           [StyleString]).

%% Main entry for rendering notes
render(Name, Html, CtxStyleOpts) :-
    book(Name, Meta),
    notes(Name, Nodes),
    style_opts(Name, Meta, CtxStyleOpts, StyleOpts),
    render(Nodes, HtmlBody, StyleOpts),
    html_header(S),
    format(string(Html), "~w~w</body\n</html>", [S, HtmlBody]).

%% Specific Nodes
render(h1(S, Meta), Html, CtxStyleOpts) :-
    style_opts(h1, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    format(string(Html), "<h1>\n~w</h1>\n", [SubHtml]). 

render(h1(S), Html, CtxStyleOpts) :-
    render(h1(S, _{}), Html, CtxStyleOpts).

render(text(S, Meta), Html, CtxStyleOpts) :-
    style_opts(text, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    format(string(Html), "<p class=\"text\">\n~w</p>\n", [SubHtml]). 

render(text(S), Html, CtxStyleOpts) :-
    render(text(S, _{}), Html, CtxStyleOpts).

render(quote(S, Meta), Html, CtxStyleOpts) :-
    style_opts(quote, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    format(string(Html), "<a>\n~w</a>\n", [SubHtml]). 

render(quote(S), Html, CtxStyleOpts) :-
    render(quote(S, _{}), Html, CtxStyleOpts).

%% Lists
render([], "", _).
render([H|T], HtmlBody, CtxStyleOpts) :-
    render(H, Acc, CtxStyleOpts),
    render(T, HtmlBody2, CtxStyleOpts),
    string_concat(Acc, HtmlBody2, HtmlBody).

%% Basic strings
render(S, SS, _) :- string(S), string_concat(S, "\n", SS).

%% Default is an error in the html
render(S, E, _) :-
    term_string(S, SS),
    format(string(E), "Failed to render: ~w\n", [SS]).

write_notes_to_html(Name) :-
    render(Name, Html, _{}),
    format(string(Filename), "resources/~w.html", [Name]),
    open(Filename, write, Stream),
    write(Stream, Html),
    close(Stream).
