:-use_module(library(random)).

test_strategy(N,Strat1,Strat2) :-
	N > 0,
	statistics(walltime,[WT0|_]),
	statistics(runtime, [T0|_]),
	calc_everything(N,Strat1,Strat2,NoDraws,NoP1Wins,NoP2Wins,Longest,Shortest,TotalMoves),
	statistics(walltime,[WT1|_]),
        statistics(runtime, [T1|_]),
	TotalWallTime is (WT1-WT0)/1000,  %%converted from ms to seconds
	TotalRunTime is (T1-T0)/1000,
	AvgWallTime is TotalWallTime/N,
	AvgRunTime is TotalRunTime/N,
	AvgMoves is TotalMoves/N,
	nl,
	write(Strat1),write(' vs '),write(Strat2),
	nl,
	write('P1Wins: '),write(NoP1Wins),
	nl,
	write('P2Wins: '),write(NoP2Wins),
	nl,
	write('Draws: '),write(NoDraws),
	nl,
	write('Longest: '),write(Longest),
	nl,
	write('Shortest: '),write(Shortest),
	nl,
	write('AvgMoves: '),write(AvgMoves),
	nl,
	write('TotalWallTime: '),write(TotalWallTime),write(' seconds'),
	nl,
	write('TotalRunTime: '),write(TotalRunTime),write(' seconds'),
	nl,
	write('AvgWallTime: '),write(AvgWallTime),write(' seconds'),
	nl,
	write('AvgRunTime: '),write(AvgRunTime),write(' seconds'),
	nl.

calc_everything(0,_,_,0,0,0,0,250,0).

calc_everything(N,Strat1,Strat2,NoDraws,NoP1Wins,NoP2Wins,Longest,Shortest,TotalMoves):-
	N > 0,
	M is N-1,
	calc_everything(M,Strat1,Strat2,NoD,NoP1,NoP2,L,S,Moves),
	play(quiet,Strat1,Strat2,NumMoves,WinningPlayer),
	write(N),write(' '),
	(WinningPlayer == 'b' -> NoP1Wins is NoP1 + 1; NoP1Wins = NoP1),
	(WinningPlayer == 'r' -> NoP2Wins is NoP2 + 1; NoP2Wins = NoP2),
	(WinningPlayer \= 'r' ->
	    (WinningPlayer \= 'b' ->  NoDraws is NoD + 1; NoDraws = NoD );
	    NoDraws = NoD),
	(NumMoves > L -> Longest = NumMoves; Longest = L),
	(NumMoves < S -> Shortest = NumMoves; Shortest = S),
	TotalMoves is Moves + NumMoves.

% getPossibleMove(player1Alive, player2Alive, player1's possible moves).
%  an empty list will be returned if there are no possible moves.
getPossibleMoves(Alive,OtherPlayerAlive,PossMoves) :-
	findall([A,B,MA,MB],
		(
		  member([A,B], Alive), neighbour_position(A,B,[MA,MB]),
		  \+member([MA,MB],Alive),\+member([MA,MB],OtherPlayerAlive)
		),
		PossMoves).

% winning_position(winner's pieces, loser's pieces).
winning_position([_|_],[]).

% choose_smaller(AMove,AScore,BMove,BScore,ChosenMove,ChosenMoveScore)
choose_smaller(AMove,AScore,_,BScore,AMove,AScore) :-
	AScore < BScore, !.

choose_smaller(_,_,BMove,BScore,BMove,BScore).

% choose_bigger(AMove,AScore,BMove,BScore,ChosenMove,ChosenMoveScore)
choose_bigger(AMove,AScore,_,BScore,AMove,AScore) :-
	AScore > BScore, !.

choose_bigger(_,_,BMove,BScore,BMove,BScore).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% BLOODLUST STRATEGY

bloodlust_move(Alive, OtherPlayerAlive, Move) :-
	getPossibleMoves(Alive,OtherPlayerAlive,PossMoves),
	random_permutation(PossMoves,PossMovesPerm), !,
	bloodlust_search(PossMovesPerm,Alive,OtherPlayerAlive,Move,_).

bloodlust_search([H|T],Alive,OtherPlayerAlive,Move,N):-
	alter_board(H,Alive,NewAlive),
	% next_generation is symmetric so it is not important whether red or blue are
	% passed in first or second. It returns the same order.
	next_generation([NewAlive,OtherPlayerAlive],[_,PostNextGenOtherPlayerAlive]),
	length(PostNextGenOtherPlayerAlive,M),
	(
	  M = 0,
	  Move = H,
	  N = M,
	  !
	;
	  bloodlust_recurse(Alive,OtherPlayerAlive,H,M,T,Move,N)
	).

% bloodlust_recurse(Alive,OtherPlayerAlive,
%                   Current_Move,Current_Move_Score,
%                   RestPossMoves,BestMove,BestMoveScore)
bloodlust_recurse(Alive, OtherPlayerAlive,
		  Current_Move, Current_Move_Score,
		  RestPossMoves, Best_Move, Best_Move_Score) :-
	bloodlust_search(RestPossMoves, Alive, OtherPlayerAlive,
			 Best_Tail_Move, Best_Tail_Move_Score),
	choose_smaller(Current_Move, Current_Move_Score,
		       Best_Tail_Move, Best_Tail_Move_Score,
		       Best_Move, Best_Move_Score).

bloodlust_recurse(_,_,Current_Move,Current_Move_Score,
		  [],Current_Move,Current_Move_Score).

bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	bloodlust_move(AliveBlues, AliveReds, Move),
	alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	bloodlust_move(AliveReds, AliveBlues, Move),
	alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SELF PRESERVATION STRATEGY

self_preservation_move(Alive, OtherPlayerAlive, Move) :-
	getPossibleMoves(Alive,OtherPlayerAlive,PossMoves),
	random_permutation(PossMoves,PossMovesPerm), !,
	self_preservation_search(PossMovesPerm,Alive,OtherPlayerAlive,Move,_).

self_preservation_search([H|T],Alive,OtherPlayerAlive,Move,N):-
	alter_board(H,Alive,NewAlive),
	next_generation([NewAlive,OtherPlayerAlive],
			[PostNextGenAlive,PostNextGenOtherPlayerAlive]),
	(
	  winning_position(PostNextGenAlive,PostNextGenOtherPlayerAlive),
	  Move = H,
	  N = 99, % if winning position, no need for further recursion.
	          % 99 ensures this move is returned recursively.
	  !
	;
	  length(PostNextGenAlive,M),
	  self_preservation_recurse(Alive,OtherPlayerAlive,H,M,T,Move,N)
	).

% self_preservation_recurse(Alive,OtherPlayerAlive,
%                           Current_Move,Current_Move_Score,
%                           RestPossMoves,BestMove,BestMoveScore)
self_preservation_recurse(Alive, OtherPlayerAlive,
			  Current_Move, Current_Move_Score,
			  RestPossMoves, Best_Move, Best_Move_Score) :-
	self_preservation_search(RestPossMoves, Alive, OtherPlayerAlive,
				 Best_Tail_Move, Best_Tail_Move_Score),
	choose_bigger(Current_Move, Current_Move_Score,
		      Best_Tail_Move, Best_Tail_Move_Score,
		      Best_Move, Best_Move_Score).

self_preservation_recurse(_,_,Current_Move,Current_Move_Score,
			  [],Current_Move,Current_Move_Score).
 
	
self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	self_preservation_move(AliveBlues, AliveReds, Move),
	alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	self_preservation_move(AliveReds, AliveBlues, Move),
	alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% LAND GRAB STRATEGY

length_difference(List1, List2, Diff) :-
	length(List1,A),
	length(List2,B),
	Diff is A - B.

land_grab_move(Alive, OtherPlayerAlive, Move) :-
	getPossibleMoves(Alive,OtherPlayerAlive,PossMoves),
	random_permutation(PossMoves,PossMovesPerm), !,
	land_grab_search(PossMovesPerm,Alive,OtherPlayerAlive,Move,_).

land_grab_search([H|T],Alive,OtherPlayerAlive,Move,N):-
	alter_board(H,Alive,NewAlive),
	next_generation([NewAlive,OtherPlayerAlive],
			[PostNextGenAlive,PostNextGenOtherPlayerAlive]),
	(
	  winning_position(PostNextGenAlive,PostNextGenOtherPlayerAlive),
	  Move = H,
	  N = 99, % if winning position, no need for further recursion.
		 % 99 ensures this move is returned recursively.
	  !
	;
	  length_difference(PostNextGenAlive,PostNextGenOtherPlayerAlive,M),
	  land_grab_recurse(Alive,OtherPlayerAlive,H,M,T,Move,N)
	).

% land_grab_recurse(Alive,OtherPlayerAlive,
%                   Current_Move,Current_Move_Score,
%                   RestPossMoves,BestMove,BestMoveScore)
land_grab_recurse(Alive, OtherPlayerAlive,
		  Current_Move, Current_Move_Score,
		  RestPossMoves, Best_Move, Best_Move_Score) :-
	land_grab_search(RestPossMoves, Alive, OtherPlayerAlive,
			 Best_Tail_Move, Best_Tail_Move_Score),
	choose_bigger(Current_Move, Current_Move_Score,
		      Best_Tail_Move, Best_Tail_Move_Score,
		      Best_Move, Best_Move_Score).

land_grab_recurse(_,_,Current_Move,Current_Move_Score,
		  [],Current_Move,Current_Move_Score).
	  	
land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	land_grab_move(AliveBlues, AliveReds, Move),
	alter_board(Move, AliveBlues, NewAliveBlues).

land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	land_grab_move(AliveReds, AliveBlues, Move),
	alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% MINIMAX STRATEGY

minimax_move(Alive, OtherPlayerAlive, Move) :-
	getPossibleMoves(Alive,OtherPlayerAlive,PossMoves),
	random_permutation(PossMoves,PossMovesPerm), !,
	minimize_search(PossMovesPerm,Alive,OtherPlayerAlive,Move,_).

minimize_search([H|T],Alive,OtherPlayerAlive,Move,N) :-
	alter_board(H,Alive,NewAlive),
	next_generation([NewAlive,OtherPlayerAlive],[PostNextGenAlive,PostNextGenOtherPlayerAlive]),
	(
	  winning_position(PostNextGenAlive,PostNextGenOtherPlayerAlive),
	  Move = H,
	  N = -99, % if winning position, no need for further recursion.
	           % -99 ensures this move is returned recursively.
	  !
	;
	  minimize_search_recurse(Alive,OtherPlayerAlive,H,T,Move,N,
				  PostNextGenAlive,PostNextGenOtherPlayerAlive)
	).

% minimize_search_recurse(Alive,OtherPlayerAlive,
%                   Current_Move,RestPossMoves,
%                   BestMove,BestMoveScore,
%                   PostMoveAlive, PostMoveOtherPlayerAlive )
minimize_search_recurse(Alive, OtherPlayerAlive,
			Current_Move, RestPossMoves,
			Best_Move, Best_Move_Score,
			PostMoveAlive, PostMoveOtherPlayerAlive ) :-
	% minimize_search succeeds unless RestPossMoves is []
	minimize_search(RestPossMoves, Alive, OtherPlayerAlive,
			Best_Tail_Move, Best_Tail_Move_Score), 
	(
	  Best_Tail_Move_Score = -99,
	  Best_Move = Best_Tail_Move,
	  Best_Move_Score = Best_Tail_Move_Score,
	  !
	;
	  maximizeOtherPlayerMove(PostMoveOtherPlayerAlive,PostMoveAlive,MaxM),
	  choose_smaller(Current_Move,MaxM,Best_Tail_Move,Best_Tail_Move_Score,
			 Best_Move,Best_Move_Score)
	).
	% choose move which allows opponent to make the minimum Maximum move,
        % i.e. choose the move which returns has minimum MaxM

% for the case when RestPossMoves is []
minimize_search_recurse(_,_,Current_Move,[],Current_Move,MaxM,
			PostMoveAlive, PostMoveOtherPlayerAlive ) :-
	maximizeOtherPlayerMove(PostMoveOtherPlayerAlive,PostMoveAlive,MaxM).

maximizeOtherPlayerMove(Alive, OtherPlayerAlive, Maximum) :-
	(
	  Alive = [],
	  OtherPlayerAlive = [],
	  Maximum = 0 %% Draw position
	;
	  getPossibleMoves(Alive,OtherPlayerAlive,PossMoves),
	  land_grab_search(PossMoves,Alive,OtherPlayerAlive,_,Maximum)
	).
	% other player makes the best land_grab move, Maximizing (it's pieces - my pieces).

minimax('b',[AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	minimax_move(AliveBlues,AliveReds,Move),
	alter_board(Move, AliveBlues, NewAliveBlues).

minimax('r',[AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	minimax_move(AliveReds, AliveBlues, Move),
	alter_board(Move, AliveReds, NewAliveReds).