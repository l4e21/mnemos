:- module(render, [render/2, write_notes_to_html/1]).

%% Global Styles
style(h2,
      _{fontsize:"18px", element: "h2"}).
style(h1,
      _{color:"red", inherits:h2, element: "h1"}).
style(text,
      _{fontsize:"12px", type:class, element: "p", classname: ".text"}).
style(quote,
      _{implements:text, fontstyles:[italics], element:"a", classname:".quote"}).
style(img,
      _{element:"img"}).


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

meta_with_inherit(Meta, WithInheritedMeta) :-
    get_dict(inherits, Meta, InheritedTag),
    style(InheritedTag, InheritedMeta),
    put_dict(Meta, InheritedMeta, WithInheritedMeta),
    !.

meta_with_inherit(Meta, Meta).

meta_to_style_string(Meta, StyleString) :-
    dict_pairs(Meta, _, StylePairs),
    foldl(render_as_css_aux, StylePairs, "", StyleString).    

render_as_css(Meta, CSS) :-
    meta_with_inherit(Meta, WithInheritedMeta),
    (get_dict(classname, WithInheritedMeta, StyleName), !;
     get_dict(element, WithInheritedMeta, StyleName), !),
    meta_to_style_string(WithInheritedMeta, StyleString),
    format(string(CSS), "~w {\n~w}\n", [StyleName, StyleString]).

html_header_aux(StyleList, StyleString) :-
    foldl(string_concat, StyleList, "", StyleString).
    
html_header(HtmlHeader) :-
    findall(CSS, (style(_, Meta), render_as_css(Meta, CSS)), StyleList),
    html_header_aux(StyleList, StyleString),
    enclose_in_tags("style", 0, StyleString, HtmlHeader).

html_body(Nodes, HtmlBody) :-
    render(Nodes, HtmlBody, _{}, 0).

%% Main entry for rendering notes
render(Name, Html) :-
    notes(Name, Nodes),
    html_header(HtmlHeader),
    html_body(Nodes, HtmlBody),
    format(string(Html),
           "<html>\n<head>\n~w</head>\n<body>\n~w</body>\n</html>",
           [HtmlHeader, HtmlBody]).

inline_css(Meta, StyleString) :-
    dict_pairs(Meta, _, MetaPairs),
    foldl(render_as_css_aux_no_whitespace, MetaPairs, "", StyleString).

render(Term, Html, Ctx, Depth) :-
    compound(Term),
    compound_name_arguments(Term, P, [S, NodeMeta]),
    style(P, StyleMeta),
    meta_with_inherit(StyleMeta, StyleMetaWithInherit),
    get_dict(element, StyleMetaWithInherit, Element),
    put_dict(NodeMeta, Ctx, CtxNew),
    inline_css(CtxNew, CSS),
    Depth1 is Depth+1,
    render(S, SubHtml, CtxNew, Depth1),
    
    (get_dict(classname, StyleMetaWithInherit, ClassNameWithDots) ->
         split_string(ClassNameWithDots, ".", "", ClassNameSplitList),
         last(ClassNameSplitList, ClassName),
         (CSS = "" ->
              enclose_in_tags(Element, ClassName, Depth, SubHtml, Html), !;
          enclose_in_tags(Element, ClassName, CSS, Depth, SubHtml, Html), !);
     enclose_in_tags(Element, Depth, SubHtml, Html), !),
    !.

render(Term, Html, Ctx, Depth) :-
    compound(Term),
    compound_name_arguments(Term, P, [S]),    
    compound_name_arguments(NewTerm, P, [S, _{}]),    
    render(NewTerm, Html, Ctx, Depth),
    !.

%% Lists
render([], "", _, _) :- !.
render([H|T], HtmlBody, Ctx, Depth) :-
    render(H, Acc, Ctx, Depth),
    render(T, HtmlBody2, Ctx, Depth),
    string_concat(Acc, HtmlBody2, HtmlBody),
    !.

%% Basic strings
render(S, SS, _, _) :- string(S), string_concat(S, "\n", SS), !.

%% Default is an error in the html
render(S, E, _, _) :-
    term_string(S, SS),
    format(string(E), "Failed to render: ~w\n", [SS]),
    !.

write_notes_to_html(Name) :-
    render(Name, Html),
    format(string(Filename), "resources/~w.html", [Name]),
    open(Filename, write, Stream),
    write(Stream, Html),
    close(Stream).
