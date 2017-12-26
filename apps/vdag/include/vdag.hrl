
-include ("logger.hrl").

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
-record (vdag_pow, {
    version   :: integer(),
    bits      :: integer(),
    resource  :: binary(), 
    nonce     :: binary()         
}).

-type vdag_pow() :: #vdag_pow{}.
-export_type([vdag_pow/0]).
