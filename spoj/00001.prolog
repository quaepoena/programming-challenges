#!/usr/bin/swipl

%% https://www.spoj.com/problems/TEST/

%% Your program is to use the brute-force approach in order to find
%% the Answer to Life, the Universe, and Everything. More precisely...
%% rewrite small numbers from input to output. Stop processing input
%% after reading in the number 42. All numbers at input are integers
%% of one or two digits.

%% Example

%% Input:
%% 1
%% 2
%% 88
%% 42
%% 99

%% Output:
%% 1
%% 2
%% 88

:- initialization(spoj_00001).

forty_two(end_of_file) :- !.
forty_two("42") :- !.
forty_two(L) :-
    write(L), nl,
    read_line_to_string(user_input, NewL),
    forty_two(NewL).

spoj_00001 :-
    read_line_to_string(user_input, L),
    forty_two(L),
    halt.
