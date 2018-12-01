PROJECT = emqx_auth_http
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with HTTP API

DEPS = clique
dep_clique = git-emqx https://github.com/emqx/clique develop

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx emqx30
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish emqx30

ERLC_OPTS += +debug_info

NO_AUTOPATCH = cuttlefish

TEST_DEPS = emqx_retainer cowboy
dep_emqx_retainer = git-emqx https://github.com/emqx/emqx-retainer emqx30
dep_cowboy = git-emqx https://github.com/ninenines/cowboy.git 2.4.0

TEST_ERLC_OPTS += +debug_info

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_http.conf -i priv/emqx_auth_http.schema -d data
