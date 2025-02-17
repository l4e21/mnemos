:- module(style, [css_style_atom/3, styles/2]).

styles(default, _{classes: _{'.dream-title': _{marginleft:"10px",
                                                            fontsize:"1.5em",
                                                            color:"#000",
                                                            marginbottom:"10px",
                                                            textdecoration: "underline dotted"},
                                          '.dream-image':_{width:"100px",
                                                           height:"100px",
                                                           border: "1px solid #ccc",
                                                           borderradius: "5px",
                                                           objectfit:"cover"},
                                          '.flower-icon':_{position: "absolute",
                                                           right: "5px",
                                                           fontsize:"1.5em"}
                                         },
                               elements: _{'body': _{backgroundcolor:"#fff",
                                                     backgroundimage:"radial-gradient(circle, transparent 20%, #f8f8f8 20%, #f8f8f8 80%, transparent 80%, transparent),
                                      radial-gradient(circle, transparent 20%, #f8f8f8 20%, #f8f8f8 80%, transparent 80%, transparent)",
                                                     backgroundsize:"50px 50px",
                                                     backgroundposition: "0 0 25px 25px",
                                                     fontfamily:"'Montserrat', sans-serif",
                                                     margin:"0",
                                                     padding:"0"},
                                           'header': _{fontsize:"2em",
                                                       textalign:"center",
                                                       padding:"10px",
                                                       fontfamily:"'Tangerine', cursive"},
                                           'h2':_{},
                                           'h1':_{color:"#000"},
                                           'img':_{},
                                           'a':_{color:"#000", textdecoration:"none"},
                                           'a:visited':_{color:"#555"},
                                           'footer':_{},
                                           'section':_{position:"relative",
                                                       backgroundcolor:"rgba(255, 255, 255, 0.8)",
                                                       width:"80%",
                                                       margin:"20px auto",
                                                       padding:"10px",
                                                       border:"2px dotted #000",
                                                       color:"#000"}}}).

%%% Conversion of CSS names from atoms
css_style_atom(margin-V, "margin", V).
css_style_atom(marginbottom-V, "margin-bottom", V).
css_style_atom(marginleft-V, "margin-left", V).
css_style_atom(padding-V, "padding", V).

css_style_atom(width-V, "width", V).
css_style_atom(height-V, "height", V).

css_style_atom(border-V, "border", V).
css_style_atom(borderradius-V, "border-radius", V).
css_style_atom(borderbottom-V, "border-bottom", V).
css_style_atom(objectfit-V, "object-fit", V).

css_style_atom(position-V, "position", V).
css_style_atom(right-V, "right", V).
css_style_atom(top-V, "top", V).
css_style_atom(left-V, "left", V).
css_style_atom(bottom-V, "bottom", V).
css_style_atom(transform-V, "transform", V).

css_style_atom(textalign-V, "text-align", V).
css_style_atom(textdecoration-V, "text-decoration", V).

css_style_atom(fontsize-V, "font-size", V).
css_style_atom(fontfamily-V, "font-family", V).

css_style_atom(color-V, "color", V).

css_style_atom(backgroundcolor-V, "background-color", V).
css_style_atom(backgroundsize-V, "background-size", V).
css_style_atom(backgroundimage-V, "background-image", V).
css_style_atom(backgroundposition-V, "background-position", V).
