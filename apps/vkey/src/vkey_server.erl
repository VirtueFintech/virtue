-module(vkey_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([gen_keys/0]).
-export([gen_keys/1]).
-export([sign/2]).
-export([verify/3]).
-export([parse_address/1]).
-export([gen_address/1]).
-export([from_wif/1]).
-export([to_wif/1]).
-export([hash/1]).
-export([dhash/1]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-define(CURVE_SPEC, secp256k1).
-define(SHA, sha256).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

gen_keys() ->
  gen_server:call(?SERVER, {gen_keys}).

gen_keys(PrivKey) ->
  gen_server:call(?SERVER, {gen_keys, PrivKey}).

sign(Message, Key) ->
  gen_server:call(?SERVER, {sign, Message, Key}).

verify(Message, Signature, Key) ->
  gen_server:call(?SERVER, {verify, Message, Signature, Key}).

parse_address(Addr) ->
  gen_server:call(?SERVER, {parse_address, Addr}).

gen_address(PubKey) ->
  gen_server:call(?SERVER, {gen_address, PubKey}).

to_wif(PrivKey) ->
  gen_server:call(?SERVER, {to_wif, PrivKey}).

from_wif(WIF) ->
  gen_server:call(?SERVER, {from_wif, WIF}).

hash(Data) when is_list(Data) ->
  gen_server:call(?SERVER, {hash, vutils_list:to_bin(Data)});

hash(Data) when is_binary(Data) ->
  gen_server:call(?SERVER, {hash, Data}).

dhash(Data) when is_list(Data) ->
  gen_server:call(?SERVER, {hash, vutils_list:to_bin(Data)});

dhash(Data) when is_binary(Data) ->
  gen_server:call(?SERVER, {dhash, Data}).

%% gen_server.

init([]) ->
  {ok, #state{}}.

handle_call({gen_keys}, _From, State) ->
  {Pub, Priv} = crypto:generate_key(ecdh, ?CURVE_SPEC),
  Pub58 = to_base58(Pub),
  Priv58 = to_base58(Priv),
  {reply, {ok, [{pub, Pub58}, {priv, Priv58}]}, State};

handle_call({gen_keys, PrivKey}, _From, State) ->
  Key = from_base58(PrivKey),
  {Pub, Priv} = crypto:generate_key(ecdh, ?CURVE_SPEC, Key),
  Pub58 = to_base58(Pub),
  Priv58 = to_base58(Priv),
  {reply, {ok, [{pub, Pub58}, {priv, Priv58}]}, State};

handle_call({sign, Message, Priv}, _From, State) ->
  Key = from_base58(Priv),
  Msg = crypto:sign(ecdsa, sha256, crypto:hash(sha256, Message), [Key, ?CURVE_SPEC]),
  Msg58 = to_base58(Msg),
  {reply, {ok, Msg58}, State};

handle_call({verify, Message, Signature, Pub}, _From, State) ->
  Bin = from_base58(Signature),
  Key = from_base58(Pub),
  M2 = crypto:hash(sha256, Message),
  Status = crypto:verify(ecdsa, sha256, M2, Bin, [Key, ?CURVE_SPEC]),
  {reply, {ok, Status}, State};

handle_call({parse_address, Pub}, _From, State) ->
  % it's 0x04 header, 32/bytes X and Y
  Bin = from_base58(Pub),
  65 = size(Bin),
  <<4, X:32/bytes, Y:32/bytes>> = Bin,
  {reply, {ok, [{x, to_base58(X)}, {y, to_base58(Y)}]}, State};

handle_call({gen_address, Pub}, _From, State) ->
  Bin = from_base58(Pub),
  B1 = crypto:hash(ripemd160, crypto:hash(sha256, Bin)),

  %% starts with V
  B2 = <<70, B1/binary>>,

  %% do Base58Check 
  %  double hash it, and take the checksum
  <<Chk:4/bytes, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, B2)),
  B3 = <<B2/binary, Chk/binary>>,
  {reply, {ok, to_base58(B3)}, State};

handle_call({to_wif, PrivKey}, _From, State) ->
  Bin = from_base58(PrivKey),
  %% 0x80 - main net, 0xef - testnet
  B1 = <<80, Bin/binary>>,
  <<Chk:4/bytes, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, B1)),
  B2 = <<B1/binary, Chk/binary>>,
  {reply, {ok, to_base58(B2)}, State};

handle_call({from_wif, WIF}, _From, State) ->
  <<80, Key:32/bytes, _/binary>> = from_base58(WIF),
  {reply, {ok, to_base58(Key)}, State};

handle_call({hash, Data}, _From, State) ->
  B = crypto:hash(?SHA, Data),
  B2 = to_base58(B),
  {reply, {ok, B2}, State};

handle_call({dhash, Data}, _From, State) ->
  B = crypto:hash(?SHA, crypto:hash(?SHA, Data)),
  B2 = to_base58(B),
  {reply, {ok, B2}, State};

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

%% ============================================================================
%% Private functions
%% ============================================================================
%%
to_base58(Bin) -> list_to_binary(base58:binary_to_base58(Bin)).
from_base58(Base58) -> base58:base58_to_binary(binary_to_list(Base58)).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

genkey_test_() ->
  {Pub1, Priv1} = crypto:generate_key(ecdh, ?CURVE_SPEC),
  {Pub2, Priv2} = crypto:generate_key(ecdh, ?CURVE_SPEC),
  ?_assert(Priv1 =/= Priv2),

  % generate shared secret
  SS1 = crypto:compute_key(ecdh, Pub2, Pub1, ?CURVE_SPEC),
  SS2 = crypto:compute_key(ecdh, Pub2, Pub1, ?CURVE_SPEC),
  ?_assert(SS1 =:= SS2).


hash_data_test_() ->
  A1 = crypto:hash(?SHA, <<"foo">>),
  A2 = crypto:hash(?SHA, <<"fo0">>),
  ?_assert(A1 =/= A2),

  B1 = crypto:hash(?SHA, crypto:hash(?SHA, <<"foo">>)),
  ?_assert(A1 =/= B1).

dummy_test_() ->
  A = 1,
  ?_assert(A =:= 1).

-endif.
