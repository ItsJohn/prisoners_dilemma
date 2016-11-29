-module(warden).
-export([start/1,add/2,stats/1,run/2,supervisor/1]).

% creates a supervisor
-spec start(list())->pid().
start(State)->
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
            {SupervisorPID,State} ->
                  State
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
supervisor({PrisonerList,Summary,History})->
      receive
            {Sender,add,PID}->
                  Sender!{self(),done,length(PrisonerList)+1},
                  supervisor({[PID|PrisonerList],Summary,History});
            {Sender,stats} ->
                  Sender!{self(),History},
                  supervisor({PrisonerList,Summary,History});
            {Sender,run,Count} ->
                  {NewSummary,NewHistory}=iterate(PrisonerList,Summary,History,Count),
                  Sender!{self(),done},
                  supervisor({PrisonerList,NewSummary,NewHistory})
      end.

% runs the interaction between prisoners N times
-spec iterate(list(), list(), list(), integer())->none().
iterate(_,Summary,History,0)->
      {Summary,History};

iterate(Prisoners,Summary,History,N) ->
      {NewSummary,NewHistory}=doOneRun(Prisoners,Summary,History),
      iterate(Prisoners,NewSummary,NewHistory,N-1).

% executes each interaction between prisoners once
-spec doOneRun(list(), list(), list())->none().
doOneRun([],Summary,History)->
      {Summary,History};
doOneRun([First|Rest],Summary,History) ->
      {NewSummary,NewHistory}=doOnce(First,Rest,Summary,History),
      doOneRun(Rest,NewSummary,NewHistory).

% executes the interaction
-spec doOnce(pid(), list(), list(), list())->none().
doOnce(_,[],Summary,History)->
      {Summary,History};
doOnce(Agent,[OtherAgent|Rest],Summary,History) ->
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
      doOnce(Agent,Rest,Summary,[{MyName,MyChoice,OtherName,OtherChoice}|History]).
