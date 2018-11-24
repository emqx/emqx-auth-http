PROJECT = emqx_auth_http
PROJECT_DESCRIPTION = EMQ X Authentication/ACL with HTTP API
PROJECT_VERSION = 3.0

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

define dep_fetch_git-emqx
	git clone -q --depth 1 -b $(call dep_commit,$(1)) -- $(call dep_repo,$(1)) $(DEPS_DIR)/$(call dep_name,$(1)) > /dev/null 2>&1; \
	cd $(DEPS_DIR)/$(call dep_name,$(1));
endef

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emqx_auth_http.conf -i priv/emqx_auth_http.schema -d data
