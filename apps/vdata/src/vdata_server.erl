-module(vdata_server).
-behaviour(gen_server).
-author ("Hisham Ismail <mhishami@gmail.com>").

-include ("vdata.hrl").

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define (SERVER, ?MODULE).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
  DBs = [
    {v_txin, record_info(fields, v_txin)},
    {v_txout, record_info(fields, v_txout)},
    {v_transaction, record_info(fields, v_transaction)}
  ],
  setup_db(DBs),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
setup_db(Databases) ->
  setup_db(Databases, false).

setup_db(Databases, Reset) ->
  application:stop(mnesia),
  case Reset of
    true ->
      mnesia:delete_schema([node()|nodes()]);
    _ ->
      ok
  end,
  mnesia:create_schema([node()|nodes()]),
  ok = application:start(mnesia),
  mnesia_eleveldb:register(),
  create(Databases).

create([{Table, Fields}|Rest]) ->
  create_table(Table, Fields),
  create(Rest);
create([]) -> ok.

create_table(Table, Fields) ->
  mnesia:create_table(Table, [
    {attributes, Fields},
    {leveldb_copies, [node()]},
    {type, ordered_set}]),
  mnesia:wait_for_tables([Table], 1000).
