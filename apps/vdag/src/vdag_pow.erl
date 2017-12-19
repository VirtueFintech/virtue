-module(vdag_pow).
-behaviour(gen_server).

-include("vdag.hrl").

%% API.
-export([start_link/0]).
-export ([find_nonce/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { pow :: vdag_pow() }).

%% This POW is purely based on Hashcash by Adam Beck.
%% 
%% X-Hashcash: 1:20:1303030600:adam@cypherspace.org::McMybZIhxKXu57jd:ckvi
%% The header contains:
%%   ver: Hashcash format version, 1 (which supersedes version 0).
%%   bits: Number of "partial pre-image" (zero) bits in the hashed code.
%%   date: The time that the message was sent, in the format YYMMDD[hhmm[ss]].
%%   resource: Resource data string being transmitted, e.g., an IP address or email address.
%%   ext: Extension (optional; ignored in version 1).
%%   rand: String of random characters, encoded in base-64 format.
%%   counter: Binary counter (up to 220), encoded in base-64 format.
%%
%% > echo -n 1:52:380119:calvin@comics.net:::9B760005E92F0DAE | openssl sha1
%% > 0000000000000756af69e2ffbdb930261873cd71
%%
%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

find_nonce(Hash, Bits) when is_binary(Hash), is_integer(Bits) ->
  case catch gen_server:call(?SERVER, {find_nonce, Hash, Bits}, 5000) of
    {'EXIT', _} -> {error, timeout};
    Result       -> Result
  end.

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({find_nonce, Hash, Bits}, _From, State) ->
  % {ok, Date} = tempo:format(<<"%Y%d%m">>, {now, erlang:timestamp()}),
  Res = <<"1:", Bits, ":", Hash/binary, ":">>,
  % ?INFO("find_nonce: Res = ~p", [Res]),
  {reply, {ok, calc_nonce(Res, Bits)}, State};

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
%%
calc_nonce(Res, Bits) ->
  Counter = vutils_b58:encode(crypto:strong_rand_bytes(4)),
  % ?INFO("Res: ~p, Counter: ~p", [Res, Counter]),
  R1 = << Res/binary, Counter/binary >>,
  % ?INFO("R1: ~p", [R1]), %vutils_list:from_bin(R1)]),
  H1 = crypto:hash(sha256, R1),
  % ?INFO("Hash of Resource: ~p", [H1]),
  case H1 of
    <<0:Bits, _/bitstring>> -> {Counter};
    _    -> calc_nonce(Res, Bits)
  end.