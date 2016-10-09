PROJECT = emq_auth_http
PROJECT_DESCRIPTION = Authentication/ACL with HTTP API
PROJECT_VERSION = 3.0

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd master

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
