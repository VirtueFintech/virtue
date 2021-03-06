-module(vnet_ring).
-behaviour(gen_server).
-author ("Hisham Ismail <mhishami@gmail.com>").

-include ("vnet.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([start_link/0]).
-export ([list/0]).
-export ([list/1]).
-export ([add/2]).
-export ([remove/2]).

%% ------------------------------------------------------------------
%% gen_server.
%% ------------------------------------------------------------------
%% MIT License
%%
%% Copyright (c) 2017 VirtueFintech
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%% 
%% 
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec list() -> list().
list() ->
  gen_server:call(?SERVER, {list, all}).

-spec list(Type :: head | tail) -> list().
list(Type) ->
  gen_server:call(?SERVER, {list, Type}).

-spec add(Type :: head | tail,
          Node :: atom()) -> ok.
add(Type, Node) ->
  gen_server:cast(?SERVER, {add, Type, Node}).

-spec remove(Type :: head | tail,
             Node :: atom()) -> ok.
remove(Type, Node) ->
  gen_server:cast(?SERVER, {remove, Type, Node}).

%% gen_server.

init([]) ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  process_flag(trap_exit, true),
  ets:new(?MODULE, [set, named_table]),
  {ok, #state{}}.

handle_call({list, Type}, _From, State) ->
  Res = case Type of
    all ->
      List = ets:match(?MODULE, {'$1', '$2'}),
      [{Key, node_list(Val)} || [Key, Val] <- List];
    Key ->
      case ets:lookup(?MODULE, Key) of
        [{Key, Val}] ->
          [{Key, node_list(Val)}];
        [] -> []
      end
  end,
  {reply, {ok, Res}, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({add, Key, Node}, State) ->
  N = hash_ring_node:make(Node),
  ?DEBUG("Adding node ~p -> ~p", [N, Key]),

  % add to the ring
  case ets:lookup(?MODULE, Key) of
    [] ->
      Ring = make_hash_ring(),
      ets:insert(?MODULE, {Key, hash_ring:add_nodes([N], Ring)});
    [{Key, CRing}] ->
      ets:insert(?MODULE, {Key, hash_ring:add_nodes([N], CRing)})
  end,
  {noreply, State};

handle_cast({remove, Key, Node}, State) ->
  ?DEBUG("Removing node ~p from ~p...", [Node, Key]),
  case ets:lookup(?MODULE, Key) of
    [] ->
      ok;
    [{Key, Ring}] ->
      ets:insert(?MODULE, {Key, hash_ring:remove_nodes([Node], Ring)})
  end,
  {noreply, State};

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
make_hash_ring() ->
  hash_ring:make(hash_ring:list_to_nodes([])).

node_list(Ring) ->
  hash_ring:get_node_list(Ring).
