-module(warden).
-export([start/1,add/2,stats/1,run/2,supervisor/1]).

% creates a supervisor
-spec start(list())->pid().
start(State)->
      ets:new(summary, [set, named_table, public]),
      PID=spawn(?MODULE, supervisor, [State]),
      PID.

% Adds prisoner to the supervisor
-spec add(pid(), pid())->integer().
add(SupervisorPID,PID)->
      SupervisorPID!{self(),add,PID},
      receive
            {SupervisorPID,done,Total} ->
                  Total
      end.

% retrieves the interaction list for all prisoners
-spec stats(pid()) ->list().
stats(SupervisorPID)->
      SupervisorPID!{self(),stats},
      receive
            {SupervisorPID,Summary} ->
                  lists:keysort(2, Summary)
      end.

% runs the interactions with each prisoner N times
-spec run(pid(), integer())->atom().
run(SupervisorPID,Iterations)->
      SupervisorPID!{self(),run,Iterations},
      receive
            {SupervisorPID,done} ->
                  ok
      end.



% keeps track of the prisoners and the interactions
-spec supervisor(tuple())->none().
supervisor({PrisonerList,History})->
      receive
            {Sender,add,PID}->
                  Sender!{self(),done,length(PrisonerList)+1},
                  supervisor({[PID|PrisonerList],History});
            {Sender,stats} ->
                  Sender!{self(),createList(ets:first(summary), [])},
                  supervisor({PrisonerList,History});
            {Sender,run,Count} ->
                  {NewHistory}=iterate(PrisonerList,History,Count),
                  Sender!{self(),done},
                  supervisor({PrisonerList,NewHistory})
      end.

% creates a list from ets
-spec createList(atom(), list())->list().
createList(Name, List) when Name == '$end_of_table' ->
      lists:append(List);
createList(Name, List) ->
      createList(ets:next(summary, Name), [ets:lookup(summary, Name)|List]).

% runs the interaction between prisoners N times
-spec iterate(list(), list(), integer())->none().
iterate(_,History,0)->
      {History};

iterate(Prisoners,History,N) ->
      {NewHistory}=doOneRun(Prisoners,History),
      iterate(Prisoners,NewHistory,N-1).

% executes each interaction between prisoners once
-spec doOneRun(list(), list())->none().
doOneRun([],History)->
      {History};
doOneRun([First|Rest],History) ->
      {NewHistory}=doOnce(First,Rest,History),
      doOneRun(Rest,NewHistory).

% executes the interaction
-spec doOnce(pid(), list(), list())->none().
doOnce(_,[],History)->
      {History};
doOnce(Agent,[OtherAgent|Rest],History) ->
      Agent!{self(),name},
      receive
            {Agent,name,MyName}->
                  ok
      end,
      OtherAgent!{self(),name},
      receive
            {OtherAgent,name,OtherName}->
                  ok
      end,
      OtherAgent!{self(),choice,MyName},
      receive
            {OtherAgent,choice,OtherChoice}->
                  ok
      end,
      Agent!{self(),choice,OtherName},
      receive
            {Agent,choice,MyChoice}->
                  ok
      end,
      OtherAgent!{self(),result,MyChoice},
      Agent!{self(),result,OtherChoice},
      recordScore({MyChoice, OtherChoice},MyName),
      recordScore({OtherChoice, MyChoice},OtherName),
      doOnce(Agent,Rest,[{MyName,MyChoice,OtherName,OtherChoice}|History]).

% Filters the summary list and calculates the sentance
-spec recordScore(tuple(), atom()) -> none().
recordScore(CurrentInteraction, TheName)->
      case ets:lookup(summary, TheName) of
            [] ->
                  ets:insert(summary, {TheName, calculateSentence(CurrentInteraction, 0)});
            [{Name, PrisonSentance}] ->
                  ets:delete(summary, Name),
                  ets:insert(summary, {Name, calculateSentence(CurrentInteraction, PrisonSentance)})
      end.

% Calculates the length of the sentence
-spec calculateSentence(tuple(), integer())->integer().
calculateSentence(CurrentInteraction, MyPrisonSentance) ->
      case CurrentInteraction of
            {coop, defect} ->
                  MyPrisonSentance;
            {defect, defect} ->
                  MyPrisonSentance + 1;
            {coop, coop} ->
                  MyPrisonSentance + 2;
            {defect, coop} ->
                  MyPrisonSentance + 3
      end.
