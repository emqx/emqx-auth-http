PROJECT = emqx_auth_http
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with HTTP API
PROJECT_VERSION = 2.4.1

DEPS = clique
dep_clique  = git https://github.com/emqtt/clique

BUILD_DEPS = emqx cuttlefish
dep_emqx = git git@github.com:emqx/emqx-enterprise
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +debug_info
ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = cuttlefish

TEST_DEPS = emqttc emqx_retainer
dep_emqttc = git https://github.com/emqtt/emqttc.git master
dep_emqx_retainer  = git https://github.com/emqtt/emqx-retainer enterprise

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_http.conf -i priv/emqx_auth_http.schema -d data

