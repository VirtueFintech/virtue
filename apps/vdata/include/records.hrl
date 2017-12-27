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
%%  +----------v------------------+            +------------------------+
%%  | Transaction                 |            | TxOutput               |
%%  +-----------------------------+            +------------------------+
%%  | 1.  txHash (3,4,5,7,8,9,10) |            | 1. index               |
%%  | 2.  nonce                   |        +---+ 2. hash (1,6,7)        |
%%  | 3.  msgType                 |        |   | 3. from                |
%%  | 4.  inputHash               <-----+  |   | 4. to                  |
%%  | 5.  inputs                  |     |  |   | 5. value               |
%%  | 6.  outputHash              <-----+--+   | 6. signingPubKey       |
%%  | 7.  outputs                 |     |      | 7. signature (3,4,5)   |
%%  | 8.  fee                     |     |      +------------------------+
%%  | 9.  sequence                |     |
%%  | 10. timestamp               |     |
%%  +-----------------------------+     |      +------------------------+
%%                                      |      | TxInput                |
%%                                      |      +------------------------+
%%                                      |      | 1. index               |
%%                                      +------+ 2. prevHash            |
%%                                             | 3. from                |
%%                                             | 4. to                  |
%%                                             | 5. value               |
%%                                             +------------------------+
%%

-type v_txtype() :: transfer | message | data.
-export_type([v_txtype/0]).

-record (v_txin, {
  index         :: integer(),
  prevHash      :: binary(),
  from          :: binary(),
  to            :: binary(),
  value         :: integer(),
  in_backpack = #{}
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
  signature     :: binary(),
  out_backpack = #{}
}).

-type v_txout() :: #v_txout{}.
-export_type([v_txout/0]).

-type v_txouts() :: [v_txout()].
-export_type([v_txouts/0]).

-record (v_transaction, {
  txHash        :: binary(),
  nonce         :: binary(),
  msgType       :: atom(),
  inputHash     :: binary(), 
  inputs        :: v_txins(),
  outputHash    :: binary(), 
  outputs       :: v_txouts(),
  fee           :: integer(), 
  sequence      :: integer(),
  timestamp     :: integer(),
  tx_backpack = #{}
}).
-type v_transaction() :: #v_transaction{}.
-export_type([v_transaction/0]).


