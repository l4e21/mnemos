:- module(supplement, []).

%% Example of supplementary packages best practice

:- use_module('../src/core', [notes/2]).

:- multifile core:notes/2.

core:notes('ab', []).

%% Now notes can be called as either supplement or core notes
