-module(supvisor).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod,Args) ->
  spawn(?MODULE, init, [{Mod, Args}]).

start_link(Mod,Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod,Args}) ->
  process_flag(trap_exit, true),
  loop({Mod,start_link,Args}).

loop({M,F,A}) ->
  Pid = apply(M,F,A),
  receive
    {'EXIT', _From, shutdown} ->
      exit(shutdown); % will kill the child too
    {'EXIT', Pid, Reason} ->
      io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
      loop({M,F,A})
    end.

% It will restart the process it watches indefinitely, unless the supervisor itself is terminated with a shutdown exit signal. Here it is in use:

% make:all([load]).
% c(eventserver), c(supvisor).
% SupPid = supvisor:start(eventserver, []).
% whereis(eventserver).
% exit(whereis(eventserver), die).
% exit(whereis(eventserver), die).
% exit(SupPid, shutdown).
% whereis(eventserver).
