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
-module(vdag_pow).
-behaviour(gen_server).
-author ("Hisham Ismail <mhishami@gmail.com>").

-include("vdag.hrl").

%% API.
-export([start_link/0]).
-export ([find_nonce/1]).
-export ([verify_nonce/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define (SERVER, ?MODULE).
-define (BITS, 20).

-record(state, {}).

%% This POW is purely based on Hashcash by Adam Beck, modified of course.
%% The header contains:
%%  version: 1
%%  bits: 20 (fixed)
%%  hash: base58 encoded of the hash
%%  nonce: base58 encoded of the nonce
%%
%% The full record looks like:
%%    <<"1:20:3yMApqCuCjXDWPrbjfR5mjCPTHqFG8Pux1TxQrEM35jj:2dhj5F">>
%%

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

find_nonce(Hash) when is_binary(Hash) ->
  case catch gen_server:call(?SERVER, {find_nonce, Hash}, 5000) of
    {'EXIT', _} -> {error, timeout};
    Result       -> Result
  end.

verify_nonce(Hash, Work) when is_binary(Hash), is_binary(Work) ->
  gen_server:call(?SERVER, {verify_nonce, Hash, Work}).

%% gen_server.

init([]) ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  process_flag(trap_exit, true),
  {ok, #state{}}.

handle_call({find_nonce, Hash}, _From, State) ->
  % {ok, Date} = tempo:format(<<"%Y%d%m">>, {now, erlang:timestamp()}),
  Res = <<"1:", ?BITS, ":", Hash/binary, ":">>,
  % ?INFO("find_nonce: Res = ~p", [Res]),
  {reply, {ok, calc_nonce(Res)}, State};

handle_call({verify_nonce, Hash, Work}, _From, State) ->
  Res = <<"1:", ?BITS, ":", Hash/binary, ":", Work/binary>>,
  Proof =
    case crypto:hash(sha256, Res) of
      <<0:?BITS, _/bitstring>> -> {ok, true};
      _ -> {error, false}
    end,
  {reply, Proof, State};

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
%% private functions
%% =============================================================================
%%
calc_nonce(Res) ->
  Counter = vutils_b58:encode(crypto:strong_rand_bytes(4)),
  R1 = << Res/binary, Counter/binary >>,
  H1 = crypto:hash(sha256, R1),
  case H1 of
    <<0:?BITS, _/bitstring>> -> Counter;
    _  -> calc_nonce(Res)
  end.