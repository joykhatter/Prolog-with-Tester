%% Problem 1
fmp_helper([],C,C).
fmp_helper([H|_],C,C):- not(number(H)).
fmp_helper([H|T],C,Final):- H =< 0, fmp_helper(T,C,Final).
fmp_helper(List1,C,Final) :- List1 = [H|_] , not(H = C),Final = C.
fmp_helper(List1,C,Final) :- C1 is C+1, List1 = [H|T] , H = C, fmp_helper(T,C1,Final).

first_missing_positive(List,Final) :- msort(List,Lis), fmp_helper(Lis,1,Final).

%% Problem 2
three_summers(Items,Goal,A,B,C):- member(A,Items), member(B,Items),member(C,Items),
    nth1(I1,Items,A), nth1(I2,Items,B),nth1(I3,Items,C), not(I1=I2),not(I1=I3),not(I2=I3), Goal is A+B+C , C>=B , B>=A,C>=A.

%% Problem 3
domino_cycle(Lis):- [(S,_)|_] = Lis, reverse(Lis,RLis), [(_,S)|_] = RLis, domino_helper(Lis).

domino_nums(1).
domino_nums(2).
domino_nums(3).
domino_nums(4).
domino_nums(5).
domino_nums(6).

domino_helper([_|[]]).
domino_helper([(_,S2),(S2,E2)|T]) :- domino_nums(S2),domino_nums(E2), domino_helper([(S2,E2)|T]).

%% Problem 4
only_odd_digits(1).
only_odd_digits(3).
only_odd_digits(5).
only_odd_digits(7).
only_odd_digits(9).

only_odd_digits(Num):- L is mod(Num,10),M is mod(L,2), M = 1, New is div(Num,10), only_odd_digits(New). 

%% Problem 5
group_and_skip(N,Out,_,[N]):- N =< Out.
group_and_skip(N,Out,In,Leftovers):- N > Out, A is mod(N,Out), B is div(N,Out), NewN is B*In,  group_and_skip(NewN,Out,In,LeftoversN), append(LeftoversN,[A],Leftovers). 

%% Problem 6
give_change_helper(_,[],_):- fail.
give_change_helper(Amount,[H|_],Res):- Amount >= H, Res = H.
give_change_helper(Amount,[H|T],Res):- Amount < H, give_change_helper(Amount,T,Res).

give_change(0,_,[]).
give_change(Amount, Lis, Change):- give_change_helper(Amount,Lis,H), New is Amount-H, give_change(New, Lis, ChangeT), append([H],ChangeT,Change).

%% Problem 7
running_median([A,B],[A,B]).
running_median(Items, Medians):- reverse(Items,Rev), Rev = [A,B,C|_], Rev = [A|T1], reverse(T1,T), running_median(T, MediansT), msort([A,B,C],Lis), Lis = [_,M,_], append(MediansT,[M],Medians). 

%% Problem 8
sum_of_two_squares(Num, A, B) :- H is floor(sqrt(Num)), sum_of_two_squares_helper(H, H, H, Num, A, B), A>=B,!.

sum_of_two_squares_helper(N1, N2, H, Num, A, B):- N1 > 0, sots_helper2(N1,N2,H,Num,B), A = N1.
sum_of_two_squares_helper(N1, N2, H, Num, A, B):- N1 > 0, NN1 is N1 -1, sum_of_two_squares_helper(NN1, N2, H, Num, A, B).
    
sots_helper2(A,B,_,Num,B):- B > 0, A > 0, Num is (A*A) + (B*B).
sots_helper2(A,B,H,Num,Res):- B > 0, A > 0, B1 is B-1, sots_helper2(A,B1,H,Num,Res).

%% Problem 9
safe_squares_rooks(Rooks, N, S):- findall(X,ssr_helper(Rooks, N, X),Lis), length(Lis,S).
 
ssr_helper(Rooks, N, [R,C]):- between(1,N,C), between(1,N,R), not(member((C,_),Rooks)), not(member((_,R),Rooks)). 

%% Problem 10
count_dominators([], 0).
count_dominators([H|T], Result):- count_dominators(T, ResultT), cd_helper(H, T), Result is ResultT + 1,!.
count_dominators([_|T], Result):- count_dominators(T, Result).

cd_helper(_, []).
cd_helper(H, [A|B]):- H > A, cd_helper(H, B).

%% Testing

all :- 
    consult('tester721.pl'),nl,
    write('Testing'),nl, write('Testing first_missing_positive'),nl,
    test_first_missing_positive, write('Testing three_summers'),nl,
    test_three_summers, write('Testing domino_cycle'),nl,
    test_domino_cycle, write('Testing only_odd_digits'),nl,
    test_only_odd_digits, write('Testing group_and_skip'),nl,
    test_group_and_skip, write('Testing give_change'),nl,
    test_give_change, write('Testing running_median'),nl,
    test_running_median, write('Testing sum_of_two_squares'),nl,
    test_sum_of_two_squares, write('Testing safe_squares_rooks'),nl,
    test_safe_squares_rooks, write('Testing count_dominators'),nl,
    test_count_dominators.

:- all.






