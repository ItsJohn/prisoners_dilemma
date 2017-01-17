-module(prisoner).
-export([create/3, getName/1, getStrategy/1, getState/1]).
-include_lib("eunit/include/eunit.hrl").

% creates a prisoner
-spec create(atom(), atom(), list())->pid().
create(Name,Strategy,State)->
      spawn(fun() -> prisoner({Name,Strategy,State}) end).

% retrieves the name of the prisoner
-spec getName(pid())->atom().
getName(PID)->
      PID!{self(),name},
      receive
            {PID,name,Name} ->
                  Name
      end.

% retrieves the strategy the prisoner has chosen
-spec getStrategy(pid())->atom().
getStrategy(PID)->
      PID!{self(),strategy},
      receive
            {PID,strategy,Strategy} ->
                  Strategy
      end.

% retrieves a the previous interactions that a prisoner had
-spec getState(pid())->list().
getState(PID)->
      PID!{self(),state},
      receive
            {PID,state,State} ->
                  State
      end.


% stores a prisoner
-spec prisoner(tuple())->none().
prisoner({Name,Strategy,State})->
      receive
            {Sender, name} ->
                  Sender!{self(),name,Name},
                  prisoner({Name,Strategy,State});
            {Sender, state} ->
                  Sender!{self(),state,State},
                  prisoner({Name,Strategy,State});
            {Sender, strategy} ->
                  Sender!{self(),strategy,Strategy},
                  prisoner({Name,Strategy,State});
            {Sender, choice, OpponentsName} ->
                  MyChoice = case Strategy of
                        coop ->
                              coop;
                        defect->
                              defect;
                        random->
                              random();
                        titForTat->
                              titForTat(filterOpponent(OpponentsName, State));
                        grudger->
                              grudger(filterOpponent(OpponentsName, State));
                        suspiciousTitForTat->
                              suspiciousTitForTat(filterOpponent(OpponentsName, State))
                  end,
                  Sender!{self(),choice,MyChoice},
                  receive
                        {_,result,TheirChoice} ->
                              prisoner({Name,Strategy,[{OpponentsName,TheirChoice,MyChoice}|State]})
                  end
      end.

% Number: 1
% creates the current choice but starts by cooperating
-spec titForTat(list())->atom().
titForTat(State) ->
      case State of
            [{_,defect, _}|_]->
                  defect;
            _ ->
                  coop
      end.

% Number: 9
-spec random()->atom().
random() ->
      List = ['defect', 'coop'],
      Index = rand:uniform(length(List)),
      lists:nth(Index,List).

% Number: 12
% creates the current choice depending whether the opponent has defected
-spec grudger(list())->atom().
grudger(State) ->
      case State of
            [{_,coop, _}|Rest]->
                  grudger(Rest);
            [{_,defect, _}|_]->
                  defect;
            _ ->
                  coop
      end.

% Number: 17
% creates the current choice to the depending on the opponents last choice
-spec suspiciousTitForTat(list())->atom().
suspiciousTitForTat(State) ->
      case State of
            [{_,coop, _}|_]->
                  coop;
            _ ->
                  defect
      end.

% Filters the list to the interactions that only include the interactions with the OpponentName but starts by defecting
-spec filterOpponent(atom(), list())->list().
filterOpponent(OpponentName, State) ->
      lists:filter(fun(Turn) ->
            case Turn of
                  {OpponentName,_,_}->
                        true;
                  _->
                        false
            end
      end, State).


%%%%%%%%%%%%%%%%%%%%
%                        Tests                         %
%%%%%%%%%%%%%%%%%%%%


prisoner_name_test() ->
      PID = create(y,coop,[]),
      PID!{self(), name},
      receive
            {PID, name, Name} ->
                  ?assert(y=:=Name)
      end.


prisoner_stategy_test() ->
      PID = create(y,random,[]),
      PID!{self(), strategy},
      receive
            {PID, strategy, Strategy} ->
                  ?assert(random=:=Strategy)
      end.


prisoner_state_test() ->
      PID = create(y,coop,[{x,coop,defect},{a,defect,defect},{b,coop,coop}]),
      PID!{self(), state},
      receive
            {PID, state, State} ->
                  ?assert([{x,coop,defect},{a,defect,defect},{b,coop,coop}]=:=State)
      end.


% Strategy Tests
always_coop_test() ->
      PID = create(y,coop,[{x,coop,defect}]),
      PID!{self(), choice, x},
      receive
            {PID, choice, Choice} ->
                  ?assert(coop=:=Choice)
      end.


always_defect_test() ->
      PID = create(y,defect,[{x,defect,defect}]),
      PID!{self(), choice, x},
      receive
            {PID, choice, Choice} ->
                  ?assert(defect=:=Choice)
      end.


titForTat_test() ->
      ?assert(coop=:=titForTat([{x,coop,coop}, {x,coop,coop}, {x,coop,coop}, {x,coop,coop}])).
titForTat_second_test() ->
      ?assert(defect=:=titForTat([{x,defect,coop}, {x,defect,coop}, {x,coop,coop}, {x,coop,coop}])).
titForTat_third_test() ->
      ?assert(coop=:=titForTat([])).


random_test() ->
      case random() of
            coop ->
                  ?assert(coop=:=coop);
            defect ->
                  ?assert(defect=:=defect)
      end.


grudger_test() ->
      ?assert(coop=:=grudger([{x,coop,coop}, {x,coop,coop}, {x,coop,coop}, {x,coop,coop}])).
grudger_second_test() ->
      ?assert(defect=:=grudger([{x,coop,coop}, {x,defect,coop}, {x,coop,coop}, {x,coop,coop}])).
grudger_third_test() ->
      ?assert(coop=:=grudger([])).


suspiciousTitForTat_test() ->
      ?assert(coop=:=suspiciousTitForTat([{x,coop,coop}, {x,coop,coop}, {x,coop,coop}, {x,coop,coop}])).
suspiciousTitForTat_second_test() ->
      ?assert(defect=:=suspiciousTitForTat([{x,defect,coop}, {x,coop,defect}])).
suspiciousTitForTat_three_test() ->
      ?assert(defect=:=suspiciousTitForTat([])).


filterOpponent_test() ->
      State = filterOpponent(x, [{x, coop, defect}, {a, defect, defect}, {x, coop, coop}, {y, coop, defect}]),
      ?assert([{x,coop,defect},{x,coop,coop}]=:=State).
filterOpponent_second_test() ->
      State = filterOpponent(b, [{x, coop, defect}, {a, defect, defect}, {x, coop, coop}, {y, coop, defect}]),
      ?assert([]=:=State).
