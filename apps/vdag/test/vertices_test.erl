-module (vertices_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("vdata/include/vdata.hrl").

-compile(export_all).

gen_genesis_test() ->
  %% start vkey_server
  vdag_node:start_link(),

  % generate addresses
  {ok, [{pub, Pub1}, {priv, Priv1}]} = vkey_server:gen_keys(),
  {ok, Addr1} = vkey_server:gen_address(Pub1),

  Genesis = <<"A new begining - trust is earned, not given">>,
  {ok, GHash} = vkey_server:hash(Genesis),
  ?_assert(32 =:= size(GHash)),

  InitialValue = 1.0e+24,
  TxIn = #v_txin{index = 0, prevHash = GHash, from = Addr1, to = Addr1, value = InitialValue},
  %% create the genesis txout.
  Tmp = #v_txout{index = 0,
                 from = Addr1, to = Addr1, value = InitialValue,
                 signingPubKey = Pub1},
  {ok, TxOut} = vdag_node:sign_txout(Tmp, Priv1),

  Tx1_tmp = #v_transaction{
    msgType = tx,
    inputs = [TxIn],
    outputs = [TxOut],
    fee = 0, sequence = 0
  },
  {ok, Tx1} = vdag_node:sign_trx(Tx1_tmp),
  [
    ?_assert(Tx1 =/= #v_transaction{}),
    ?_assert(Tx1#v_transaction.txHash =/= <<"">>)
  ].
  
  % G = digraph:new([acyclic]),
