PROJECT = emq_auth_http
PROJECT_DESCRIPTION = Authentication/ACL with HTTP API
PROJECT_VERSION = 3.0

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq30

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_http.conf -i priv/emq_auth_http.schema -d data

