# Prisoner's Dilemma
The Prisoner's Dilemma is a problem where a warden/supervisor asks two prisoners separately, whether they want to betray(co-operate) or stay silent(defect). If the two prisoners stay silent they both get a number of years in jail. If one prisoner betrays the other one, they are let go and the other gets put in jail for a greater number of years. If they both betray each other, they both stay in jail for a longer time than if they both stayed silent but not as long as if one betray's the other. This project implements a number of strategies for this dilemma.    
* `coop` - Always co-operate
* `defect` - Always defect
* `random` - Random choice
* `titForTat` - Copies the other prisoners last choice
* `grudger` - If the other prisoner has ever betrayed you, always defect
* `suspiciousTitForTat` -  Same as tit for tat but starts by defecting    

## How to run
First make sure you have Erlang installed   
* Open a terminal in the directory
* Type `erl` to open the Erlang console
* `c(warden,[{outdir, "beam/"}]).` to compile the warden file
* `c(prisoner,[{outdir, "beam/"}]).` to compile the prisoner file
* `P1 = prisoner:create(nameA,random,[]).` to create the first prisoner, change `nameA` to any name and `random` to any strategy mentioned above
* `P2 = prisoner:create(nameB,titForTat,[]).` to create another prisoner
* `W = warden:start({[],[]}).` to start the warden
* `warden:add(W, P1).` adds the warden as the supervisor for the first prisoner
* `warden:add(W, P2).` adds the warden as the supervisor for the second prisoner
* `warden:run(W, 3).` to get the warden to ask each prisoner to whether they co-operate or betray the other prisoner `3` times
* `warden:stats(W).` to display the results

## Running Tests
To run the prisoners tests:
* `c(prisoner,[{outdir, "beam/"}]).` to compile the prisoner file
* `prisoner:test().` to run the tests

To run the warden tests:
* `c(prisoner,[{outdir, "beam/"}]).` to compile the prisoner file
* `c(warden,[{outdir, "beam/"}]).` to compile the warden file
* `warden:test().` to run the tests
