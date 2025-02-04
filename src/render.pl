:- module(render, [render/3, write_notes_to_html/1]).

%% Global Styles
style(h2, _{fontsize:"18px"}).
style(h1, _{color:"red", fontsize: "18px"}).
style(text, _{fontsize:"8px"}).
style(quote, _{implements:text, fontstyles:[italics]}).
style(img, _{}).

enclose_in_tags(Tag, Depth, Content, Result) :-
    format(string(Result),
           "~*c<~w>\n~w~*c</~w>\n",
           [Depth, 32, Tag, Content, Depth, 32, Tag]).

enclose_in_tags(Tag, Class, Depth, Content, Result) :-
    format(string(Result),
           "~*c<~w class=\"~w\">\n~w~*c</~w>\n",
           [Depth, 32, Tag, Class, Content, Depth, 32, Tag]).

enclose_in_tags(Tag, Class, StyleString, Depth, Content, Result) :-
    format(string(Result),
           "~*c<~w class=\"~w\" style=\"~w\">\n~w~*c</~w>\n",
           [Depth, 32, Tag, Class, StyleString, Content, Depth, 32, Tag]).


%% Priority: Meta > Style Predicate > Context
css_style_atom(fontsize-V, "font-size", V) :- !.
css_style_atom(color-V, "color", V) :- !.

style_opts(Meta, ContextOpts, StyleOpts) :-
    (get_dict(style, Meta, MetaOpts); MetaOpts = _{}),
    put_dict(MetaOpts, ContextOpts, StyleOpts).

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
    
html_header(HtmlHeader) :-
    findall(CSS, (style(N, StyleOpts), render_as_css(N, StyleOpts, CSS)), StyleList),
    html_header_aux(StyleList, StyleString),
    enclose_in_tags("style", 0, StyleString, HtmlHeader).

%% Main entry for rendering notes
render(Name, Html, CtxStyleOpts) :-
    book(Name, Meta),
    notes(Name, Nodes),
    style_opts(Meta, CtxStyleOpts, StyleOpts),
    render(Nodes, HtmlBody, StyleOpts, 0),
    html_header(HtmlHeader),
    format(string(Html), "<html>\n<head>\n~w</head>\n<body>\n~w</body>\n</html>",
           [HtmlHeader, HtmlBody]).

%% Specific Nodes
render(h1(S, Meta), Html, CtxStyleOpts, Depth) :-
    style_opts(Meta, CtxStyleOpts, StyleOpts),
    Depth1 is Depth+1,
    render(S, SubHtml, StyleOpts, Depth1),
    enclose_in_tags("h1", Depth, SubHtml, Html).

render(h1(S), Html, CtxStyleOpts, Depth) :-
    render(h1(S, _{}), Html, CtxStyleOpts, Depth).

render(text(S, Meta), Html, CtxStyleOpts, Depth) :-
    style_opts(Meta, CtxStyleOpts, StyleOpts),
    Depth1 is Depth+1,
    render(S, SubHtml, StyleOpts, Depth1),
    enclose_in_tags("p", "text", Depth, SubHtml, Html).

render(text(S), Html, CtxStyleOpts, Depth) :-
    render(text(S, _{}), Html, CtxStyleOpts, Depth).

render(quote(S, Meta), Html, CtxStyleOpts, Depth) :-
    style_opts(Meta, CtxStyleOpts, StyleOpts),
    Depth1 is Depth+1,
    render(S, SubHtml, StyleOpts, Depth1),
    enclose_in_tags("a", Depth, SubHtml, Html).

render(quote(S), Html, CtxStyleOpts, Depth) :-
    render(quote(S, _{}), Html, CtxStyleOpts, Depth).

%% Lists
render([], "", _, _).
render([H|T], HtmlBody, CtxStyleOpts, Depth) :-
    render(H, Acc, CtxStyleOpts, Depth),
    render(T, HtmlBody2, CtxStyleOpts, Depth),
    string_concat(Acc, HtmlBody2, HtmlBody).

%% Basic strings
render(S, SS, _, _) :- string(S), string_concat(S, "\n", SS).

%% Default is an error in the html
render(S, E, _, _) :-
    term_string(S, SS),
    format(string(E), "Failed to render: ~w\n", [SS]).

write_notes_to_html(Name) :-
    render(Name, Html, _{}),
    format(string(Filename), "resources/~w.html", [Name]),
    open(Filename, write, Stream),
    write(Stream, Html),
    close(Stream).
