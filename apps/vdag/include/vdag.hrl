
-include ("logger.hrl").

-record (vdag_pow, {
    version   :: integer(),
    bits      :: integer(),
    date      :: binary(),    %% in YYMMDD[hhmm[ss]]
    resource  :: binary(), 
    ext       :: binary(),
    random    :: binary(),
    counter   :: binary()         
}).

-type vdag_pow() :: #vdag_pow{}.
-export_type([vdag_pow/0]).
