PROJECT = virtue
PROJECT_DESCRIPTION = Virtue Trust Coin
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = 2

DEPS = lager sync
LOCAL_DEPS = vdag vnet vkey vdata vtime \
						 mnesia crypto public_key inets sasl

include erlang.mk
