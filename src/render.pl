:- module(render, [render/3, write_notes_to_html/2]).

style(h2, _{fontsize:18, subnodes:inline}).
style(h1, _{implements:h2, colour:red, indent:2}).
style(text, _{subnodes:inline}).
style(quote, _{implements:text, fontstyles:[italics]}).
style(img, _{}).

%% Priority: Meta > Style Predicate > Context
style_opts(Id, Meta, ContextOpts, StyleOpts) :-
    (style(Id, Opts1); Opts1 = _{}),
    (get_dict(style, Meta, Opts2); Opts2 = _{}),
    put_dict(Opts2, Opts1, Opts3),
    put_dict(Opts3, ContextOpts, StyleOpts).

render_as_css_aux(K-_, Acc, AccNew) :-
    atom_string(K, KS),
    string_concat(Acc, KS, Acc1),
    string_concat(Acc1, ":;\n", AccNew).

render_as_css(N, StyleOpts, CSS) :-
    atom_string(N, S),
    dict_pairs(StyleOpts, _, StylePairs),
    foldl(render_as_css_aux, StylePairs, "", StyleString),
    string_concat(S, "{\n", S1),
    string_concat(S1, StyleString, S2),
    string_concat(S2, "}\n", CSS).

html_header_aux(StyleList, Styles) :-
    foldl(string_concat, StyleList, "", Styles).
    
html_header(Header) :-
    findall(CSS, (style(N, StyleOpts), render_as_css(N, StyleOpts, CSS)), StyleList),
    html_header_aux(StyleList, Styles),
    string_concat("<html>\n<head>\n<style>\n", Styles, Header1),
    string_concat(Header1, "</style>\n</head>\n<body>\n", Header).

%% Main entry for rendering notes
render(Name, Html, CtxStyleOpts) :-
    book(Name, Meta),
    notes(Name, Nodes),
    style_opts(Name, Meta, CtxStyleOpts, StyleOpts),
    render(Nodes, HtmlBody, StyleOpts),
    html_header(S),
    string_concat(S, HtmlBody, HtmlWithBody),
    string_concat(HtmlWithBody, "<\body>\n</html>", Html).

%% Specific Nodes
render(h1(S, Meta), Html, CtxStyleOpts) :-
    style_opts(h1, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    string_concat("<h1>\n", SubHtml, Html1),
    string_concat(Html1, "</h1>\n", Html).

render(h1(S), Html, CtxStyleOpts) :-
    render(h1(S, _{}), Html, CtxStyleOpts).

render(text(S, Meta), Html, CtxStyleOpts) :-
    style_opts(text, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    string_concat("<p>\n", SubHtml, Html1),
    string_concat(Html1, "</p>\n", Html).

render(text(S), Html, CtxStyleOpts) :-
    render(text(S, _{}), Html, CtxStyleOpts).

render(quote(S, Meta), Html, CtxStyleOpts) :-
    style_opts(quote, Meta, CtxStyleOpts, StyleOpts),
    render(S, SubHtml, StyleOpts),
    string_concat("<a>\n", SubHtml, Html1),
    string_concat(Html1, "</a>\n", Html).

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
    string_concat("Failed Render: ", SS, WithoutNewline),
    string_concat(WithoutNewline, "\n", E).

write_notes_to_html(Filename, Name) :-
    render(Name, Html, _{}),
    open(Filename, write, Stream),
    write(Stream, Html),
    close(Stream).
