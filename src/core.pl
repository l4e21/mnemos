:- module(core, [page/2]).
    
:- discontiguous page/2.
:- multifile page/2.

%% Simulation and Simulacra by Jean Baudrillard
page(simulation_and_simulacra,
      [
          section([
                         header(h1(a("Simulation and Simulacra", _{ref: "/"}))),
                         div(a("The Procession of Simulacra"), _{class: "dream-title"}),
                         p([a("The simulacrum is never what hides the truth -
                it is truth that hides the fact that there is none. The
                simulacrum is true."),
                            a("- Ecclesiastes", _{fontsize:"12px"})],
                           _{tags:["christianity"], class: "text"}),
                         p("Coextensivity of the real and its concept is imaginary"),
                         p("The real is produced from miniaturized cells, matrices, and 
memory banks, models of control - and it can be reproduced an indefinite number of times from these. It no longer needs to be rational, because it no longer measures itself against either an ideal or negative instance. It is no longer anything but operational."),
                         p("Interpretation-- The system is the point from which we produce 'reality', and as long as the system is functional, what happens in reality does not matter, except for purposes of orientation."),
                         div(a("The Divine Irreverence of Images"), _{class: "dream-title"})
          ])
      ]).

%% Simultaneous Statistical Inference
page(simultaneous_statistical_inference,
      [
          h1("Intro"),
          p("In simultaneous statistical inference, we have less hypotheses than data-sets, I.E., a singular hypothesis may talk about multiple datasets."),
          p("This can mean that in multiple comparison, a decision is not only made between H_0 and not H_0, but also between all possible combinations")
      ]).

page(404, [h1("Not Found!")]).
