-module(warden).
-export([start/1,add/2,stats/1,run/2]).
-include_lib("eunit/include/eunit.hrl").

% creates a supervisor
-spec start(list())->pid().
start({SummaryScore, History})->
      ets:new(summary, [named_table, bag, public]),
      ets:new(history, [named_table, duplicate_bag, public]),
      createEtsFromList(summary, SummaryScore),
      createEtsFromList(history, History),
      spawn(fun() -> supervisor([]) end).

% Creates an ETS table from a given list
-spec createEtsFromList(atom(), list()) -> none().
createEtsFromList(_, []) ->
      ok;
createEtsFromList(TableName, [First|Rest]) ->
      ets:insert(TableName, First),
      createEtsFromList(TableName, Rest).

% Adds prisoner to the supervisor
-spec add(pid(), pid())->integer().
add(SupervisorPID,PID)->
      SupervisorPID!{self(),add,PID},
      receive
            {SupervisorPID,done,Total} ->
                  Total
      end.

% Filters the prisoner list to only include PID and Name
-spec filterPrisoners(list(), list())  -> list().
filterPrisoners([], Enemies) ->
      Enemies;
filterPrisoners([{PID, Name, _}|List], Enemies) ->
      filterPrisoners(List, [{PID, Name}|Enemies]).


% retrieves the interaction list for all prisoners
-spec stats(pid()) ->list().
stats(SupervisorPID)->
      SupervisorPID!{self(),stats},
      receive
            {SupervisorPID,stats, Summary} ->
                  lists:keysort(2, Summary) % sorts list
      end.

% asks the supervisor to run the interactions N times
-spec run(pid(), integer())->atom().
run(SupervisorPID,Iterations)->
      SupervisorPID!{self(),run,Iterations},
      receive
            {SupervisorPID,done} ->
                  ok
      end.

% keeps track of the prisoners and the interactions
-spec supervisor(list())->none().
supervisor(PrisonerList)->
      receive
            {Sender,add,PID}->
                  PID!{self(), name},
                  receive
                        {PID, name, Name} ->
                              ok
                  end,
                  Sender!{self(),done,length(PrisonerList)+1},
                  supervisor([{PID, Name, filterPrisoners(PrisonerList, [])}|PrisonerList]); % adds prisoner and adds the prisoners they have to  interact with
            {Sender,stats} ->
                  Sender!{self(), stats, ets:tab2list(summary)}, % creates a list from ets
                  supervisor(PrisonerList);
            {Sender,run,Count} ->
                  PID=spawn(fun() -> checkIfFinished() end),
                  seperatePrisoner(PrisonerList, 0, PID, Count),
                  Sender!{self(),done},
                  supervisor(PrisonerList)
      end.

% Checks if a prisoner has finished all the iterations, if not make it continue
-spec checkIfFinished(none()) -> none().
checkIfFinished() ->
      MyID = self(),
      receive
            {_, done, {ID, 0, _}} ->
                  ID!{self(), finished},
                  checkIfFinished();
            {_, done, {ID, Count, Prisoner}} ->
                  _=spawn(fun() -> prepareInteraction(ID, Prisoner, 0, MyID, Count - 1, Prisoner) end), % spawns a function without having to export the function
                  checkIfFinished()
      end.

% extracts the prisoner out of the list, spawns a prisoner process
-spec seperatePrisoner(list(), integer(), pid(), integer())->none().
seperatePrisoner([Prisoner|[]], Acc, CheckerID, Count)->
      receive
            {_, finished} when Acc == 1->
                  ok;
            {_, finished} ->
                  seperatePrisoner([Prisoner|[]], Acc - 1, CheckerID, Count) % makes sure all processes are returned
      end;
seperatePrisoner([Prisoner|Rest], Acc, CheckerID, Count) ->
      ID = self(),
      _=spawn(fun() -> prepareInteraction(ID, Prisoner, 0, CheckerID, Count, Prisoner) end),
      seperatePrisoner(Rest, Acc + 1, CheckerID, Count).

% extracts the prisoners out of enemy list, and spawns process for interaction
-spec prepareInteraction(pid(), tuple(), integer(), pid(), integer(), tuple())->none().
prepareInteraction(ID, {_, _, []}, Acc, CheckerID, Count, Prisoner) ->
      receive
            {_, done} when Acc == 1 ->
                  CheckerID!{self(), done, {ID,Count, Prisoner}};
            {_, done} ->
                  prepareInteraction(ID, {x, y, []}, Acc - 1, CheckerID, Count, Prisoner)
      end;
prepareInteraction(ID, {PID, Name, [{OID, OtherName}|List]}, Acc, CheckerID, Count, Prisoner) ->
      ThisID = self(),
      _ = spawn(fun() -> generateGame(ThisID, PID, Name, OID, OtherName) end),
      prepareInteraction(ID, {PID, Name, List}, Acc + 1, CheckerID, Count, Prisoner).

% Creates a game
-spec generateGame(pid(), pid(), atom(), pid(), atom())->none().
generateGame(ID, PID, Name, OID, OtherName) ->
      NameChoice = runInteraction(PID, OtherName),
      OtherChoice = runInteraction(OID, Name),
      handleResults(PID, Name, NameChoice, OID, OtherName, OtherChoice),
      ID!{self(), done}.

% Handles the results, storing them in history, sending to prisoners and records the score
-spec handleResults(pid(), atom(), atom(), pid(), atom(), atom())->none().
handleResults(PID, Name, NameChoice, OID, OtherName, OtherChoice) ->
      sendResult(PID, NameChoice, OID, OtherChoice),
      recordScore({NameChoice, OtherChoice}, Name, PID),
      recordScore({OtherChoice, NameChoice}, OtherName, OID),
      ets:insert(history, {Name, NameChoice, OtherName, OtherChoice}).

% Asks Prisoners for there choice
-spec runInteraction(pid(), atom())->atom().
runInteraction(PID, OtherName) ->
      PID!{self(), choice, OtherName},
      receive
            {PID, choice, Choice} ->
                  Choice
      end.

% Returns the results to the prisoners
-spec sendResult(pid(), atom(), pid(), atom())->none().
sendResult(PID, PrisonerChoice, OID, OtherChoice) ->
      PID!{self(), result, OtherChoice},
      OID!{self(), result, PrisonerChoice}.

% Filters the summary list and calculates the sentance
-spec recordScore(tuple(), atom(), pid()) -> none().
recordScore(CurrentInteraction, TheName, PID)->
      List = ets:lookup(summary, TheName),
      case List of
            [] ->
                  ets:insert(summary, {TheName, calculateSentence(CurrentInteraction, 0), PID});
            [{Name, PrisonSentance, PID}|_] ->
                  ets:delete(summary, Name),
                  ets:insert(summary, {Name, calculateSentence(CurrentInteraction, PrisonSentance), PID})
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


%%%%%%%%%%%%%%%%%%%%
%                        Tests                         %
%%%%%%%%%%%%%%%%%%%%


filterPrisoners_test() ->
      ?assert([]=:=filterPrisoners([], [])).
filterPrisoners_second_test() ->
      ?assert([{x, self()}]=:=filterPrisoners([{x, self(), []}], [])).


recordScore_test() ->
      checkForTable(summary),
      recordScore({coop,defect}, x, self()),
      ?assert(1=:=length(ets:tab2list(summary))).
recordScore_second_test() ->
      checkForTable(summary),
      recordScore({coop,defect}, x, self()),
      recordScore({defect,coop}, x, self()),
      ?assert(1=:=length(ets:tab2list(summary))).
recordScore_third_test() ->
      checkForTable(summary),
      recordScore({coop,coop}, x, self()),
      recordScore({defect,coop}, y, self()),
      ?assert(2=:=length(ets:tab2list(summary))).


handleResults_test() ->
      checkForTable(history),
      handleResults(self(), x, coop, self(), y, coop),
      receive
            {_, result, _} ->
                  ok
      end,
      ?assert(1=:=length(ets:tab2list(history))).


runInteraction_test() ->
      PrisonerID = spawn(fun() -> test_prisoner() end),
      ?assert(coop=:=runInteraction(PrisonerID, x)).


sendResult_test() ->
      sendResult(self(), coop, self(), coop),
      receive
            {_, result, Choice} ->
                  ?assert(coop=:=Choice)
      end.


calculateSentence_test() ->
      ?assert(7=:=calculateSentence({coop, defect}, 7)).
calculateSentence_second_test() ->
      ?assert(8=:=calculateSentence({defect, defect}, 7)).
calculateSentence_third_test() ->
      ?assert(9=:=calculateSentence({coop, coop}, 7)).
calculateSentence_fourth_test() ->
      ?assert(10=:=calculateSentence({defect, coop}, 7)).


time_test() ->
      StartTime = erlang:timestamp(),
      Warden = createPrisoners(),
      run(Warden, 50),
      EndTime = erlang:timestamp(),
      Microseconds = timer:now_diff(EndTime,StartTime),
      Seconds = Microseconds /1000000,
      ?debugFmt("Completed in... ~p~n", [Seconds]).



checkForTable(Table) ->
      case ets:info(Table) of
            undefined ->
                  ets:new(Table, [named_table, duplicate_bag, public]);
            _ ->
                  ets:delete(Table),
                  ets:new(Table, [named_table, duplicate_bag, public])
      end.

stopTable(Table) ->
      case ets:info(Table) of
            undefined ->
                  ok;
            _ ->
                  ets:delete(Table)
      end.

createPrisoners() ->
      P1 = prisoner:create(a, coop,[]),
      P2 = prisoner:create(b,defect,[]),
      P3 = prisoner:create(c,random,[]),
      P4 = prisoner:create(d,titForTat,[]),
      stopTable(history),
      stopTable(summary),
      W = start({[],[]}),
      add(W, P1),
      add(W, P2),
      add(W, P3),
      add(W, P4),
      W.


test_prisoner() ->
      receive
            {PID, choice, _} ->
                  PID!{self(), choice, coop}
      end.
