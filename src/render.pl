:- module(render, [render_notes/2, write_notes_to_html/1]).
:- use_module(core, [notes/2, elem_overrides/2]).
:- use_module(style, [elem_style/2, class_style/2, css_style_atom/3]).


% This one is needed to resolve predicates when loading modules dynamically
:- meta_predicate(render).

%% CSS
%%% Rendering
render_as_css_aux(K-V, Acc, AccNew) :-
    css_style_atom(K-V, KS, VS),
    format(string(AccNew), "~w  ~w: ~w;\n", [Acc, KS, VS]),
    !.

render_as_css_aux(_-_, Acc, Acc).

render_as_css_aux_no_whitespace(K-V, Acc, AccNew) :-
    css_style_atom(K-V, KS, VS),
    format(string(AccNew), "~w~w: ~w;", [Acc, KS, VS]),
    !.

render_as_css_aux_no_whitespace(_-_, Acc, Acc).

meta_to_style_string(StyleMeta, StyleString) :-
    dict_pairs(StyleMeta, _, StylePairs),
    foldl(render_as_css_aux, StylePairs, "", StyleString).    

render_as_css(StyleName, StyleMeta, CSS) :-
    meta_to_style_string(StyleMeta, StyleString),
    format(string(CSS), "~w {\n~w}\n\n", [StyleName, StyleString]).

%% HTML

%%% Utilities
inline_css(Meta, StyleString) :-
    dict_pairs(Meta, _, MetaPairs),
    foldl(render_as_css_aux_no_whitespace, MetaPairs, "", StyleString).

enclose_in_tags(Tag, Depth, Content, Result) :-
    format(string(Result),
           "~*c<~w>\n~w~*c</~w>\n",
           [Depth, 32, Tag, Content, Depth, 32, Tag]).

enclose_in_tags(Tag, NodeMeta, Depth, Content, Result) :-
    (get_dict(class, NodeMeta, ClassName) ->
         format(string(ClassString), " class=\"~w\"", [ClassName]);
     ClassString = ""),
    (inline_css(NodeMeta, CSS), CSS \= "" ->
         format(string(StyleString), " style=\"~w\"", [CSS]);
     StyleString = ""),
    (get_dict(ref, NodeMeta, HRef) ->
         format(string(HRefString), " href=\"~w\"", [HRef]);
     HRefString = ""),
    format(string(Result),
           "~*c<~w~w~w~w>\n~w~*c</~w>\n",
           [Depth, 32, Tag, ClassString, StyleString, HRefString, Content, Depth, 32, Tag]).

%%% Html Header
elem_override(DocName, ElemName, ElemMeta) :-
    elem_overrides(DocName, StyleOverrides),
    get_dict(ElemName, StyleOverrides, ElemMeta).

replace_style_with_override(DocName, ElemName, _, ElemNewMeta) :-
    elem_override(DocName, ElemName, ElemNewMeta),
    !.

replace_style_with_override(_, _, ElemMeta, ElemMeta).

all_styles_for_doc(DocName, StyleList) :-
    findall(StyleName-StyleMeta,
            (class_style(StyleName, StyleMeta); elem_style(StyleName, StyleMeta)),
            StyleList1),
    maplist([StyleName-StyleOldMeta, StyleName-StyleNewMeta]>>replace_style_with_override(DocName, StyleName, StyleOldMeta, StyleNewMeta),
            StyleList1,
            StyleList).    

html_header(DocName, HtmlHeader) :-
    all_styles_for_doc(DocName, StyleList),
    maplist([StyleName-StyleMeta, CSS]>>render_as_css(StyleName, StyleMeta, CSS),
            StyleList,
            CSSList),
    foldl(string_concat, CSSList, "", StyleString),
    enclose_in_tags("style", 0, StyleString, HtmlHeader).

%%% Body
html_body(Nodes, HtmlBody) :-
    render(Nodes, HtmlBody, _{}, 0).

%%%% Main entry for rendering notes
render_notes(Name, Html) :-
    notes(Name, Nodes),
    html_header(Name, HtmlHeader),
    html_body(Nodes, HtmlBody),
    format(string(Html),
           "<html>\n<head>\n<link href=\"https://fonts.googleapis.com/css2?family=Tangerine&display=swap\" rel=\"stylesheet\">\n<title>\n~w\n</title>\n~w</head>\n<body>\n~w</body>\n</html>",
           [Name, HtmlHeader, HtmlBody]).


%%%% Lists
render([], "", _, _) :- !.
render([H|T], HtmlBody, Ctx, Depth) :-
    render(H, Acc, Ctx, Depth),
    render(T, HtmlBody2, Ctx, Depth),
    string_concat(Acc, HtmlBody2, HtmlBody),
    !.

%%%% Nodes
render(Term, Html, Ctx, Depth) :-
    %% Find element, and the metadata
    compound(Term),
    compound_name_arguments(Term, Element, [S, NodeMeta]),

    %% Render subnodes
    Depth1 is Depth+1,
    render(S, SubHtml, Ctx, Depth1),

    %% Render tags
    enclose_in_tags(Element, NodeMeta, Depth, SubHtml, Html),
    !.

render(Term, Html, Ctx, Depth) :-
    compound(Term),
    compound_name_arguments(Term, P, [S]),    
    compound_name_arguments(NewTerm, P, [S, _{}]),
    render(NewTerm, Html, Ctx, Depth),
    !.

%%%% Basic strings
render(S, SS, _, _) :- string(S), string_concat(S, "\n", SS), !.

%%%% Default is an error in the html
render(S, E, _, _) :-
    term_string(S, SS),
    format(string(E), "Failed to render: ~w\n", [SS]),
    !.

%%%% Write to file
write_notes_to_html(Name) :-
    render_notes(Name, Html),
    format(string(Filename), "resources/~w.html", [Name]),
    open(Filename, write, Stream),
    write(Stream, Html),
    close(Stream).

% ?- write_notes_to_html(X).
