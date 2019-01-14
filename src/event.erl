-module(event).
-author("Navneet Gupta").
-export([loop/1,loop2/1,start/2,start_link/2,init/3,cancel/1, normalize/1]).

-record(state, {server, name = "",to_go=0}).

loop(State = #state{server=Server}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
    after State#state.to_go*1000 ->
      Server ! {done, State#state.name}
    end.

%% Test Till this
% c(event).   //compile the module
% rr(event,state). // load the record
% spawn(event, loop, [#state{server=self(), name="test", to_go=5}]). // create a process using spawn/3 method taking module_name,method and argument.
% flush().    // if called before 5 sec nthing happens after 5 sec will output the timeout scenario.

% spawn(event, loop, [#state{server=self(), name="test", to_go=365*24*60*60}]). this will thorw error since erlang only support timeout for 50days in milliseconds.

% To test the cancel feature (with an ample 500 seconds to type it). I created the reference, sent the message and got a reply with the same reference so I know the ok I received was coming from this process and not any other on the system.

% Pid = spawn(event, loop, [#state{server=self(), name="test", to_go=500}]).
% ReplyRef = make_ref().
% Pid ! {self(), ReplyRef, cancel}.
% flush().

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

init(Server, EventName, DateTime) ->
  loop2(#state{server=Server,name=EventName,
      to_go=time_to_go(DateTime)}).

% event:normalize(365*24*60*60).

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

loop2(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
      after T*1000 ->
        if Next =:= [] ->
          Server ! {done, S#state.name};
        Next =/= [] ->
          loop2(S#state{to_go=Next})
        end
      end.

cancel(Pid) ->
  %% Monitor in case the process is already dead
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
    end.

time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
    calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0  -> ToGo;
            ToGo =< 0 -> 0
          end,
  normalize(Secs).
