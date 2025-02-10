:- module(style, [elem_style/2, class_style/2, css_style_atom/3]).

%%% Global Styles
elem_style('body', _{backgroundcolor:"#fff",
                     backgroundimage:"radial-gradient(circle, transparent 20%, #f8f8f8 20%, #f8f8f8 80%, transparent 80%, transparent),
                                      radial-gradient(circle, transparent 20%, #f8f8f8 20%, #f8f8f8 80%, transparent 80%, transparent)",
                     backgroundsize:"50px 50px",
                     backgroundposition: "0 0 25px 25px",
                     fontfamily:"'Montserrat', sans-serif",
                     margin:"0",
                     padding:"0"}).

elem_style('header', _{fontsize:"1.5em",
                       textalign:"center",
                       padding:"10px",
                      fontfamily:"'Tangerine', cursive"}).

elem_style('h2', _{fontsize:"1.5em"}).
elem_style('h1:visited', _{color:"#000"}).
elem_style('img', _{}).
elem_style('a', _{}).
elem_style('a:visited', _{}).
elem_style('footer', _{}).

elem_style('section', _{position:"relative",
                       backgroundcolor:"rgba(255, 255, 255, 0.8)",
                       width:"80%",
                       margin: "20px auto",
                       padding: "10px",
                       border: "2px dotted #000",
                       color: "#000"}).

class_style('.text', _{fontsize:"1em"}).
class_style('.quote', _{fontstyle:italics}).

class_style('.dream-date', _{}).
class_style('.dream-content', _{}).
class_style('.dream-title', _{}).
class_style('.dream-image', _{}).

%%% Conversion of CSS names from atoms
css_style_atom(margin-V, "margin", V).
css_style_atom(padding-V, "padding", V).
css_style_atom(width-V, "width", V).
css_style_atom(border-V, "border", V).
css_style_atom(position-V, "position", V).

css_style_atom(textalign-V, "text-align", V).

css_style_atom(fontsize-V, "font-size", V).
css_style_atom(fontfamily-V, "font-family", V).

css_style_atom(color-V, "color", V).

css_style_atom(backgroundcolor-V, "background-color", V).
css_style_atom(backgroundsize-V, "background-size", V).
css_style_atom(backgroundimage-V, "background-image", V).
css_style_atom(backgroundposition-V, "background-position", V).
