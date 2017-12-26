%% ----------------------------------------------------------------------------- 
%% Virtue Data Model
%%
%%                     +------------------------+
%%                     | MessageType            |
%%                     +------------------------+
%%             +-------| id                     |
%%             |       | enum (msg, tx, data)   |
%%             |       +------------------------+
%%             |
%%  +----------v--------------+            +------------------------+
%%  | Transaction             |            | TxOutput               |
%%  +-------------------------+            +------------------------+
%%  | 1. txHash (2,3,5,7,8,9) |            | 1. index               |
%%  | 2. msgType              |        +---+ 2. hash (1,6,7)        |
%%  | 3. inputHash            |        |   | 3. from                |
%%  | 4. inputs               <-----+  |   | 4. to                  |
%%  | 5. outputHash           |     |  |   | 5. value               |
%%  | 6. outputs              <-----+--+   | 6. signingPubKey       |
%%  | 7. fee                  |     |      | 7. signature (3,4,5)   |
%%  | 8. sequence             |     |      +------------------------+
%%  | 9. timestamp            |     |
%%  +-------------------------+     |      +------------------------+
%%                                  |      | TxInput                |
%%                                  |      +------------------------+
%%                                  |      | 1. index               |
%%                                  +------+ 2. prevHash            |
%%                                         | 3. from                |
%%                                         | 4. to                  |
%%                                         | 5. value               |
%%                                         +------------------------+
%%

-type v_txtype() :: transfer | message | data.
-export_type([v_txtype/0]).

-record (v_txin, {
  index         :: integer(),
  prevHash      :: binary(),
  from          :: binary(),
  to            :: binary(),
  value         :: integer()
}).

-type v_txin() :: #v_txin{}.
-export_type([v_txin/0]).

-type v_txins() :: [v_txin].
-export_type([v_txins/0]).

-record (v_txout, {
  index         :: integer(),
  hash          :: binary(),
  from          :: binary(),
  to            :: binary(),
  value         :: integer(),
  signingPubKey :: binary(),
  signature     :: binary()
}).

-type v_txout() :: #v_txout{}.
-export_type([v_txout/0]).

-type v_txouts() :: [v_txout()].
-export_type([v_txouts/0]).

-record (v_transaction, {
  txHash        :: binary(),
  msgType       :: atom(),
  inputHash     :: binary(), 
  inputs        :: v_txins(),
  outputHash    :: binary(), 
  outputs       :: v_txouts(),
  fee           :: integer(), 
  sequence      :: integer(),
  timestamp     :: integer()
}).
-type v_transaction() :: #v_transaction{}.
-export_type([v_transaction/0]).


