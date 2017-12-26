-module(vutils_sup).
-behaviour(supervisor).
-author ("Hisham Ismail <mhishami@gmail.com>").

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [],
  {ok, {{one_for_one, 1, 5}, Procs}}.
