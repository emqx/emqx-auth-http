PROJECT = emq_auth_http
PROJECT_DESCRIPTION = Authentication/ACL with HTTP API
PROJECT_VERSION = 2.1

BUILD_DEPS = emqttd cuttlefish

dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

NO_AUTOPATCH = cuttlefish

TEST_ERLC_OPTS += +debug_info
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_http.conf -i priv/emq_auth_http.schema -d data

