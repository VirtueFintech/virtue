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
-module(vdag_node).
-behaviour(gen_server).
-author ("Hisham Ismail <mhishami@gmail.com>").

-include ("vdag.hrl").
-include_lib ("vdata/include/vdata.hrl").

%% API.
-export ([start_link/0]).
-export ([sign_txout/2]).
-export ([verify_hash_txout/1]).
-export ([hash_trx/1]).
-export ([verify_hash_trx/1]).
-export ([verify_hash_list/2]).
-export ([parse_txout/1]).

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
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec sign_txout(TxOut :: v_txout(), PrivKey :: binary()) -> {ok, v_txout()}.
sign_txout(TxOut, PrivKey) ->
  gen_server:call(?SERVER, {sign_txout, TxOut, PrivKey}).

-spec verify_hash_txout(TxOut :: v_txout()) -> {ok, boolean()}.
verify_hash_txout(TxOut) ->
  gen_server:call(?SERVER, {verify_hash_txout, TxOut}).

-spec hash_trx(Trx :: v_transaction()) -> {ok, v_transaction()}.
hash_trx(Trx) ->
  gen_server:call(?SERVER, {hash_trx, Trx}).

-spec verify_hash_list(List :: list(), Hash :: binary) -> {ok, boolean()}.
verify_hash_list(List, Hash) ->
  gen_server:call(?SERVER, {verify_hash_list, List, Hash}).

-spec verify_hash_trx(Trx :: v_transaction()) -> {ok, boolean()}.
verify_hash_trx(Trx) ->
  gen_server:call(?SERVER, {verify_hash_trx, Trx}).

-spec parse_txout(TxOuts :: v_txouts()) -> v_txins().
parse_txout(TxOuts) ->
  gen_server:call(?SERVER, {parse_txout, TxOuts}).

%% gen_server.
init([]) ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({sign_txout, TxOut, Priv}, _From, State) ->
  % create the signature first
  {ok, SigHash} = vkey_server:sign(do_encode_tx(TxOut), Priv),
  {ok, OutSigHash} = vkey_server:hash(do_encode_signature(TxOut, SigHash)),
  FinalTxOut = TxOut#v_txout{hash = OutSigHash, signature = SigHash},
  {reply, {ok, FinalTxOut}, State};

handle_call({verify_hash_txout, TxOut}, _From, State) ->
  Message = do_encode_tx(TxOut),
  {ok, Result} = vkey_server:verify(Message,
                                    TxOut#v_txout.signature,
                                    TxOut#v_txout.signingPubKey),
  {reply, {ok, Result}, State};

handle_call({hash_trx, Trx}, _From, State) ->
  {ok, Time} = vtime_server:time(),
  ReplyTrx = do_hash_trx(Trx, Time),
  {reply, {ok, ReplyTrx}, State};

handle_call({verify_hash_trx, Trx}, _From, State) ->
  ReplyTrx = do_hash_trx(Trx, Trx#v_transaction.timestamp),
  TxHashRes = ReplyTrx#v_transaction.txHash =:= Trx#v_transaction.txHash,
  InputHashRes = ReplyTrx#v_transaction.inputHash =:= Trx#v_transaction.inputHash,
  OutputHashRes = ReplyTrx#v_transaction.outputHash =:= Trx#v_transaction.outputHash,
  AllThree = TxHashRes andalso InputHashRes andalso OutputHashRes,
  {reply, {ok, AllThree}, State};

handle_call({verify_hash_list, List, Hash}, _From, State) ->
  {reply, {ok, do_hash_list(List) =:= Hash}, State};

handle_call({parse_txout, TxOuts}, _From, State) ->
  F = fun(X, Accu) ->
        In = #v_txin{index = length(Accu),
                     prevHash = X#v_txout.hash,
                     from = X#v_txout.from,
                     to = X#v_txout.to,
                     value = X#v_txout.value},
        [In | Accu]
      end,
  Reply = lists:foldr(F, [], TxOuts),
  {reply, {ok, lists:flatten(lists:reverse(Reply))}, State};

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

%% =============================================================================
%% Private functions
%%
do_encode_tx(TxOut) ->
  bert:encode(#{from => TxOut#v_txout.from,
                to => TxOut#v_txout.to,
                value => TxOut#v_txout.value}).

do_encode_signature(TxOut, SigHash) ->
  bert:encode(#{index => TxOut#v_txout.index,
                signingPubKey => TxOut#v_txout.signingPubKey,
                signature => SigHash}).

-spec do_hash_trx(Trx :: v_transaction(), Time :: integer()) -> v_transaction().
do_hash_trx(Trx, Time) ->
  % get the hash of the appended hashes, if any
  InputHash = do_hash_list(Trx#v_transaction.inputs),
  OutputHash = do_hash_list(Trx#v_transaction.outputs),
  Tmp = bert:encode(#{msgType => Trx#v_transaction.msgType,
                      inputHash => InputHash,
                      outputHash => OutputHash,
                      fee => Trx#v_transaction.fee,
                      sequence => Trx#v_transaction.sequence,
                      timestamp => Time}),
  {ok, TrxHash} = vkey_server:hash(Tmp),
  Trx#v_transaction{txHash = TrxHash,
                    inputHash = InputHash,
                    outputHash = OutputHash,
                    timestamp = Time}.

-spec do_hash_list(L :: list()) -> binary().
do_hash_list(L) ->
  F = fun(X, Accu) ->
      {ok, H} = vkey_server:hash(bert:encode(X)),
      <<H/binary, Accu/binary>>
    end,
  lists:foldr(F, <<"">>, L).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Insert tests here.
dummy_test_() ->
  [
    ?_assert(1 =:= 1)
  ].

prepare() ->
  ok = application:ensure_started(base58),
  ok = application:ensure_started(bert),
  ok = application:ensure_started(vtime),
  ok = application:ensure_started(vkey),
  ok.

gen_dag_test_() ->
  prepare(),
  %% start vkey_server
  vdag_node:start_link(),

  % generate addresses
  {ok, [{pub, Pub0}, {priv, Priv0}]} = vkey_server:gen_keys(),
  {ok, [{pub, Pub1}, {priv, Priv1}]} = vkey_server:gen_keys(),
  {ok, [{pub, Pub2}, {priv, Priv2}]} = vkey_server:gen_keys(),
  {ok, [{pub, Pub3}, {priv, Priv3}]} = vkey_server:gen_keys(),
  {ok, [{pub, Pub4}, {priv, Priv4}]} = vkey_server:gen_keys(),
  {ok, [{pub, Pub5}, {priv, _Priv5}]} = vkey_server:gen_keys(),
  {ok, Addr0} = vkey_server:gen_address(Pub0),
  {ok, Addr1} = vkey_server:gen_address(Pub1),
  {ok, Addr2} = vkey_server:gen_address(Pub2),
  {ok, Addr3} = vkey_server:gen_address(Pub3),
  {ok, Addr4} = vkey_server:gen_address(Pub4),
  {ok, Addr5} = vkey_server:gen_address(Pub5),

  Genesis = <<"A new begining - trust is earned, not given">>,
  {ok, GHash} = vkey_server:hash(Genesis),

  InitialValue = 1.0e+24,

  %% ok, now create some dags
  %%                 +----+
  %%          +-------| V1 |------------------------+
  %%  +----+  |       +----+                        |
  %%  | V0 |--+                                     |
  %%  +----+  |       +----+         +----+       +----+      +----+
  %%          +-------| V2 |---------| V3 |-------| V4 |------| V5 |
  %%                  +----+         +----+       +----+      +----+
  %%

  % Create V0 - genesis block. No tx_in.
  Tx0_In = #v_txin{prevHash = GHash},
  %% create the genesis txout.
  Tx0_Out_tmp = #v_txout{index = 0,
                         from = Addr0, to = Addr0,
                         value = InitialValue,
                         signingPubKey = Pub0},
  {ok, Tx0_Out} = vdag_node:sign_txout(Tx0_Out_tmp, Priv0),

  Tx0_tmp = #v_transaction{msgType = tx, inputs = [Tx0_In],
                           outputs = [Tx0_Out],
                           fee = 0, sequence = 0},
  {ok, Tx0} = vdag_node:hash_trx(Tx0_tmp),
  {ok, Tx0_verify} = vdag_node:verify_hash_trx(Tx0),

  %% Create V1 from V0, for 10k
  {ok, Tx1_Ins} = vdag_node:parse_txout(Tx0#v_transaction.outputs),
  Tx1_Out_tmp = #v_txout{index = 0,
                         from = Addr0, to = Addr1,
                         value = 10000,
                         signingPubKey = Pub0},
  {ok, Tx1_Out} = vdag_node:sign_txout(Tx1_Out_tmp, Priv0),
  Tx1_tmp = #v_transaction{msgType = tx, inputs = Tx1_Ins,
                           outputs = [Tx1_Out],
                           fee = 0, sequence = 1},
  {ok, Tx1} = vdag_node:hash_trx(Tx1_tmp),
  {ok, Tx1_verify} = vdag_node:verify_hash_trx(Tx1),

  %% Create V2 from V0, for 90k
  {ok, Tx2_Ins} = vdag_node:parse_txout(Tx0#v_transaction.outputs),
  Tx2_Out_tmp = #v_txout{index = 0,
                         from = Addr0, to = Addr2,
                         value = 90000,
                         signingPubKey = Pub0},
  {ok, Tx2_Out} = vdag_node:sign_txout(Tx2_Out_tmp, Priv0),
  Tx2_tmp = #v_transaction{msgType = tx, inputs = Tx2_Ins,
                           outputs = [Tx2_Out],
                           fee = 0, sequence = 2},
  {ok, Tx2} = vdag_node:hash_trx(Tx2_tmp),
  {ok, Tx2_verify} = vdag_node:verify_hash_trx(Tx2),

  %% Create V3 from V2, for 20k
  {ok, Tx3_Ins} = vdag_node:parse_txout(Tx2#v_transaction.outputs),
  Tx3_Out_tmp = #v_txout{index = 0,
                         from = Addr2, to = Addr3,
                         value = 20000,
                         signingPubKey = Pub2},
  {ok, Tx3_Out} = vdag_node:sign_txout(Tx3_Out_tmp, Priv2),
  Tx3_tmp = #v_transaction{msgType = tx, inputs = Tx3_Ins,
                           outputs = [Tx3_Out],
                           fee = 0, sequence = 3},
  {ok, Tx3} = vdag_node:hash_trx(Tx3_tmp),
  {ok, Tx3_verify} = vdag_node:verify_hash_trx(Tx3),


  %% Create V4 from V3, for 10k
  {ok, Tx4_Ins} = vdag_node:parse_txout(Tx3#v_transaction.outputs),
  Tx4_Out_tmp = #v_txout{index = 0,
                         from = Addr3, to = Addr4,
                         value = 10000,
                         signingPubKey = Pub3},
  {ok, Tx4_Out} = vdag_node:sign_txout(Tx4_Out_tmp, Priv3),
  Tx4_tmp = #v_transaction{msgType = tx, inputs = Tx4_Ins,
                           outputs = [Tx4_Out],
                           fee = 0, sequence = 4},
  {ok, Tx4} = vdag_node:hash_trx(Tx4_tmp),
  {ok, Tx4_verify} = vdag_node:verify_hash_trx(Tx4),


  %% Create V4 from V1, for 5k
  {ok, Tx41_Ins} = vdag_node:parse_txout(Tx1#v_transaction.outputs),
  Tx41_Out_tmp = #v_txout{index = 0,
                          from = Addr1, to = Addr4,
                          value = 10000,
                          signingPubKey = Pub3},
  {ok, Tx41_Out} = vdag_node:sign_txout(Tx41_Out_tmp, Priv1),
  Tx41_tmp = #v_transaction{msgType = tx, inputs = Tx41_Ins,
                            outputs = [Tx41_Out],
                            fee = 0, sequence = 4},
  {ok, Tx41} = vdag_node:hash_trx(Tx41_tmp),
  {ok, Tx41_verify} = vdag_node:verify_hash_trx(Tx41),


  %% Create V5 from V4, for 5k
  {ok, Tx5_Ins} = vdag_node:parse_txout(Tx3#v_transaction.outputs),
  Tx5_Out_tmp = #v_txout{index = 0,
                         from = Addr4, to = Addr5,
                         value = 5000,
                         signingPubKey = Pub4},
  {ok, Tx5_Out} = vdag_node:sign_txout(Tx5_Out_tmp, Priv4),
  Tx5_tmp = #v_transaction{msgType = tx, inputs = Tx5_Ins,
                           outputs = [Tx5_Out],
                           fee = 0, sequence = 5},
  {ok, Tx5} = vdag_node:hash_trx(Tx5_tmp),
  {ok, Tx5_verify} = vdag_node:verify_hash_trx(Tx5),


  G = digraph:new([acyclic]),
  V0 = digraph:add_vertex(G, Tx0),
  V1 = digraph:add_vertex(G, Tx1),
  V2 = digraph:add_vertex(G, Tx2),
  V3 = digraph:add_vertex(G, Tx3),
  V4 = digraph:add_vertex(G, Tx4),
  V41 = digraph:add_vertex(G, Tx41),
  V5 = digraph:add_vertex(G, Tx5),

  %% make the graph
  digraph:add_edge(G, V0, V1),
  digraph:add_edge(G, V1, V41),
  digraph:add_edge(G, V0, V2),
  digraph:add_edge(G, V2, V3),
  digraph:add_edge(G, V3, V4),
  digraph:add_edge(G, V4, V5),

  [
    ?_assert(44 =:= size(GHash)),
    ?_assert(true =:= Tx0_verify),
    ?_assert(true =:= Tx1_verify),
    ?_assert(true =:= Tx2_verify),
    ?_assert(true =:= Tx3_verify),
    ?_assert(true =:= Tx4_verify),
    ?_assert(true =:= Tx5_verify)
  ].

%% =============================================================================
%% private functions
%%

-endif.
