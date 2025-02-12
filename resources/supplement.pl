module(supplement, []).

%% Example of supplementary packages best practice

:- ensure_loaded('../src/core').

:- multifile core:notes/2.

core:notes('ab', []).
