:- module(core, [notes/2, book/2, elem_overrides/2]).
    
:- discontiguous book/2.
:- discontiguous notes/2.

%% Simulation and Simulacra by Jean Baudrillard
book(simulation_and_simulacra,
     _{author:"Jean Baudrillard",
       tags:["philosophy"],
       style:_{fontsize:14}}).

author("Jean Baudrillard", []).

elem_overrides(simulation_and_simulacra, _{'h1':_{color:"red"}}).

notes(simulation_and_simulacra,
      [
          header(h1("The Procession of Simulacra", _{}), _{}),
          p([a("The simulacrum is never what hides the truth -
                it is truth that hides the fact that there is none. The
                simulacrum is true."),
             a("- Ecclesiastes", _{fontsize:"12px"})],
               _{tags:["christianity"], class: "text"}),
          p("Coextensivity of the real and its concept is imaginary"),
          p("The real is produced from miniaturized cells, matrices, and 
memory banks, models of control - and it can be reproduced an indefinite number of times from these. It no longer needs to be rational, because it no longer measures itself against either an ideal or negative instance. It is no longer anything but operational."),
          p("Interpretation-- The system is the point from which we produce 'reality', and as long as the system is functional, what happens in reality does not matter except in the same way that it might suck if a map is wrong"),
          h1("The Divine Irreverence of Images")
      ]).

%% Simultaneous Statistical Inference
book(simultaneous_statistical_inference, _{tags:["maths"]}).
notes(simultaneous_statistical_inference,
      [
          h1("Intro"),
          p("In simultaneous statistical inference, we have less hypotheses than data-sets, I.E., a singular hypothesis may talk about multiple datasets."),
          p("This can mean that in multiple comparison, a decision is not only made between H_0 and not H_0, but also between all possible combinations")
      ]).
