
PROJECT = emqttd_auth_http
PROJECT_DESCRIPTION = Authentication/ACL with HTTP API
PROJECT_VERSION = 2.0

DEPS = emqttd 

dep_emqttd = git https://github.com/emqtt/emqttd plus

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config
