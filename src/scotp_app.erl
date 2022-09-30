%%%-------------------------------------------------------------------
%% @doc scotp public API
%% @end
%%%-------------------------------------------------------------------

-module(scotp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Pid} = scotp_sup:start_link(),

  {ok, DefaultClock} = clock:new(default),
  global:register_name(defaultClock, DefaultClock),

  seq:new(DefaultClock, s1),

  {ok, Pid}.

stop(_State) ->
  ok.

%% internal functions
