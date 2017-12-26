-module(vkey_app).
-behaviour(application).
-author ("Hisham Ismail <mhishami@gmail.com>").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  vkey_sup:start_link().

stop(_State) ->
  ok.
