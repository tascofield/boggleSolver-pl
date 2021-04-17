:- use_module(library(readutil)).

main :- 
    filename_to_lines("words.txt",Words),
    writeln("Hello!"),
    writeln("Welcome to the Boggle solver!"),
    writeln("Input your own board? (y/n)"),
    input_yes_or_no(YN),
    get_board_of_yn(YN,B),
    writeln("Your Board: "),
    writeln(""),
    writeln(""),
    print_board(B),
    writeln(""),
    writeln(""),
    writeln("Searching..."),
    all_matching_words(Words,B,Matches),
    length(Matches,NumMatches),
    write("Found "),
    write(NumMatches),
    writeln(" matches:"),
    writeln(""),
    sort_matches_by_length(Matches,SortedMatches),
    print_matches(SortedMatches).

filename_to_lines(FN,LS) :-
    open(FN,read,Stream),
    stream_as_lines(Stream,LS).

%stream_as_lines(Stream,Lines) is true when Stream contains Lines, separating by newline characters
stream_as_lines(S,[]) :- at_end_of_stream(S).
stream_as_lines(S,[X|L]) :- 
    read_line_to_string(S,X),
    stream_as_lines(S,L).

example_board([['e','e','c','a'],
               ['a','l','e','p'],
               ['h','n','b','o'],
               ['q','t','t','y']]).

letter_at0(Row,Col,Board,Letter) :- 
    nth0(Row,Board,R),
    nth0(Col,R,Letter).

notmember(_,[]).
notmember(X,[H|T]) :- X \= H, notmember(X,T).

neighborsOf(X,Y,[(Xp,Yp),(X,Yp),(Xm,Yp),(Xp,Y),(Xm,Y),(Xp,Ym),(X,Ym),(Xm,Ym)]) :-
    Xp is X + 1,
    Xm is X - 1,
    Yp is Y + 1,
    Ym is Y - 1.
    
isneighbor(X,Y,X2,Y2) :- 
    neighborsOf(X,Y,NS),
    member((X2,Y2),NS). %Preconditions need to be in this order
    
% board_contains_word_starting_from(Charlist, Board, Row, Col) is true when the boggle board contains the word encoded as a list of chars, 
% starting at the row at column specified
board_contains_char_list_starting_from(Charlist, Board, Row, Col) :- 
    board_contains_char_list_starting_from(Charlist, Board, Row, Col,[]).

board_contains_char_list_starting_from([],_,_,_,_).
board_contains_char_list_starting_from([Char|Rest], Board, Row, Col, Visited) :-
    notmember((Row,Col),Visited),
    letter_at0(Row,Col,Board,Char),
    isneighbor(Row,Col,Row2,Col2),
    board_contains_char_list_starting_from(Rest,Board,Row2,Col2,[(Row,Col)|Visited]).

board_has_word(Word,Board,Where) :-
    Where = (Row,Col),
    string_chars(Word,Chars),
    board_contains_char_list_starting_from(Chars,Board,Row,Col).

print_row([]).
print_row([C|CS]) :-
    write(C),
    write(' '),
    print_row(CS).

print_board([]).
print_board([R|RS]) :- 
    print_row(R),
    writeln(""),
    print_board(RS).

input_char(C) :-
    get_char(C),
    char_type(C,alpha).
input_char(C) :- input_char(C).

input_chars([],0).
input_chars([C|Cs],Numleft) :-
    Numleft \= 0,
    Acc is Numleft - 1,
    input_char(C),
    input_chars(Cs,Acc).

boardify([],[]).
boardify([A,B,C,D|R],[[A,B,C,D]|R2]) :-
    boardify(R,R2).

input_board(B) :-
    writeln("Non-alphabetical characters will be ignored. Input your board here:"),
    input_chars(C,16),
    boardify(C,B).

is_y_or_n(y).
is_y_or_n(n).

input_yes_or_no(YN) :-
    get_char(YN),
    is_y_or_n(YN).
input_yes_or_no(YN) :- input_yes_or_no(YN).

all_matching_words([],_,[]).
all_matching_words([Match|Rest],B,[(Match,Where)|Matching]) :-
    board_has_word(Match,B,Where),
    all_matching_words(Rest,B,Matching).
all_matching_words([Word|Rest],B,Matching) :-
    \+ board_has_word(Word,B,_),
    all_matching_words(Rest,B,Matching).

get_board_of_yn(n,B) :-
    example_board(B).
get_board_of_yn(y,B) :-
    input_board(B).

print_matches([]).
print_matches([(Word,Where)|Rest]) :-
    write("At ("),
    write(Where),
    write("): "),
    writeln(Word),
    print_matches(Rest).

divide_list([],[],[]).
divide_list([X],[],[X]).
divide_list([X,Y|L],[X|L1],[Y|L2]) :-
    divide_list(L,L1,L2).

%implementation of merge sort
sort_matches_by_length([],[]).
sort_matches_by_length([M],[M]).
sort_matches_by_length(List,Sorted) :-
    divide_list(List,L1,L2), 
    sort_matches_by_length(L1,L1S),
    sort_matches_by_length(L2,L2S),
    merge(L1S,L2S,Sorted).
merge([],L,L).
merge(L,[],L).
merge([(Match1,Where1)|R1],[(Match2, Where2)|R2],[(Match1,Where1)|R]) :-
    string_length(Match1,Len1),
    string_length(Match2,Len2),
    Len1 > Len2,
    merge(R1,[(Match2, Where2)|R2],R).
merge([(Match1,Where1)|R1],[(Match2, Where2)|R2],[(Match2,Where2)|R]) :-
    string_length(Match1,Len1),
    string_length(Match2,Len2),
    Len1 =< Len2,
    merge([(Match1,Where1)|R1],R2,R).