-module(prisoner).
-export([create/3, getName/1, getStrategy/1, getState/1, prisoner/1]).

% creates a prisoner
-spec create(atom(), atom(), list())->pid().
create(Name,Strategy,State)->
      PID=spawn(?MODULE, prisoner, [{Name,Strategy,State}]),
      PID.

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
