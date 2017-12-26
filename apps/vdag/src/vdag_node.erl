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
-export ([sign_trx/1]).

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

-spec sign_trx(Trx :: v_transaction()) -> {ok, v_transaction()}.
sign_trx(Trx) ->
  gen_server:call(?SERVER, {sign_trx, Trx}).

%% gen_server.

init([]) ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  process_flag(trap_exit, true),
  ok = application:ensure_started(vtime),
  ok = application:ensure_started(vkey),
  {ok, #state{}}.

handle_call({sign_txout, TxOut, Priv}, _From, State) ->
  % create the signature first
  Sig = bert:encode(#{from => TxOut#v_txout.from,
                      to => TxOut#v_txout.to,
                      value => TxOut#v_txout.value}),
  {ok, SigHash} = vkey_server:sign(Sig, Priv),
  OutSig = bert:encode(#{index => TxOut#v_txout.index, 
                         signingPubKey => TxOut#v_txout.signingPubKey, 
                         signature => SigHash}),
  {ok, OutSigHash} = vkey_server:hash(OutSig),
  FinalTxOut = TxOut#v_txout{hash = OutSigHash, signature = SigHash},
  {reply, {ok, FinalTxOut}, State};

handle_call({sign_trx, Trx}, _From, State) ->
  {ok, Time} = vtime_server:time(),

  % get the hash of the appended list, if any
  F = fun(X, Accu) ->
        {ok, H} = vkey_server:hash(bert:encode(X)),
        <<H/binary, Accu/binary>>
      end,
  InputHash = lists:foldr(F, <<"">>, Trx#v_transaction.inputs),
  OutputHash = lists:foldr(F, <<"">>, Trx#v_transaction.outputs),
  Tmp = bert:encode(#{
      msgType => Trx#v_transaction.msgType,
      inputHash => InputHash,
      outputHash => OutputHash,
      fee => Trx#v_transaction.fee,
      sequence => Trx#v_transaction.sequence,
      timestamp => Time
    }),
  {ok, TrxHash} = vkey_server:hash(Tmp),
  ReplyTrx = Trx#v_transaction{
    txHash = TrxHash, inputHash = InputHash, outputHash = OutputHash,
    timestamp = Time
  },
  {reply, {ok, ReplyTrx}, State};

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

