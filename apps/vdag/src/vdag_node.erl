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
  ?_assert(1 =:= 1).

prepare() ->
  ok = application:ensure_started(base58),
  ok = application:ensure_started(bert),
  ok = application:ensure_started(vtime),
  ok = application:ensure_started(vkey),
  ok.

gen_genesis_test_() ->
  prepare(),
  %% start vkey_server
  vdag_node:start_link(),

  % generate addresses
  {ok, [{pub, Pub1}, {priv, Priv1}]} = vkey_server:gen_keys(),
  {ok, Addr1} = vkey_server:gen_address(Pub1),

  Genesis = <<"A new begining - trust is earned, not given">>,
  {ok, GHash} = vkey_server:hash(Genesis),
  % ?_assert(32 =:= size(GHash)),

  InitialValue = 1.0e+24,
  TxIn = #v_txin{index = 0, prevHash = GHash, 
                 from = Addr1, to = Addr1, 
                 value = InitialValue},
  %% create the genesis txout.
  Tmp = #v_txout{index = 0,
                 from = Addr1, to = Addr1, 
                 value = InitialValue,
                 signingPubKey = Pub1},
  {ok, TxOut} = vdag_node:sign_txout(Tmp, Priv1),

  Tx1_tmp = #v_transaction{msgType = tx, inputs = [TxIn],
                           outputs = [TxOut],
                           fee = 0, sequence = 0},
  {ok, Tx1} = vdag_node:hash_trx(Tx1_tmp),

  % do verification
  {ok, AllThree} = vdag_node:verify_hash_trx(Tx1),
  [
    ?_assert(Tx1 =/= #v_transaction{}),
    ?_assert(Tx1#v_transaction.txHash =/= <<"">>),
    ?_assert(AllThree =:= true)
  ].
  
  % G = digraph:new([acyclic]),

-endif.
