:- module(render, [render/2, write_notes_to_html/1]).

%% CSS

%%% Global Styles
elem_style(h2, _{fontsize:"18px"}).
elem_style(h1, _{color:"black", inherits:h2}).
elem_style(img, _{element:"img"}).

class_style(".text", _{fontsize:"14px"}).
class_style(".quote", _{inherits:text, fontstyles:[italics]}).

%%% Conversion of CSS names from atoms
css_style_atom(fontsize-V, "font-size", V) :- !.
css_style_atom(color-V, "color", V) :- !.

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

meta_to_style_string(Meta, StyleString) :-
    dict_pairs(Meta, _, StylePairs),
    foldl(render_as_css_aux, StylePairs, "", StyleString).    

render_style_as_css(Name, Meta, CSS) :-
    atom_string(Name, StyleName),
    meta_to_style_string(Meta, StyleString),
    format(string(CSS), "~w {\n~w}\n", [StyleName, StyleString]).

%% HTML
inline_css(Meta, StyleString) :-
    dict_pairs(Meta, _, MetaPairs),
    foldl(render_as_css_aux_no_whitespace, MetaPairs, "", StyleString).

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

html_header_aux(StyleList, StyleString) :-
    foldl(string_concat, StyleList, "", StyleString).
    
html_header(HtmlHeader) :-
    findall(CSS,
            (elem_style(Name, Meta),
             render_style_as_css(Name, Meta, CSS)),
            ElemStyleList),
    findall(CSS,
            (class_style(Name, Meta),
             render_style_as_css(Name, Meta, CSS)),
            ClassStyleList),
    append(ClassStyleList, ElemStyleList, StyleList),
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


%% Lists
render([], "", _, _) :- !.
render([H|T], HtmlBody, Ctx, Depth) :-
    render(H, Acc, Ctx, Depth),
    render(T, HtmlBody2, Ctx, Depth),
    string_concat(Acc, HtmlBody2, HtmlBody),
    !.

% Nodes
render(Term, Html, Ctx, Depth) :-
    %% Find element, and the metadata
    compound(Term),
    compound_name_arguments(Term, P, [S, NodeMeta]),
    atom_string(P, Element),

    %% Enrich meta with context, inline the styling into stylestring
    put_dict(NodeMeta, Ctx, CtxNew),
    inline_css(CtxNew, CSS),

    %% Render subnodes
    Depth1 is Depth+1,
    render(S, SubHtml, CtxNew, Depth1),

    %% Provide class, render
    (get_dict(class, CtxNew, ClassName) ->
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
