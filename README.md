
emqttd_auth_http
================

Authenticate by HTTP API

Build
-----

```
make && make tests
```

Configuration
-------------

File: etc/emqttd_auth_http.conf

```erlang

%% Variables: %u = username, %c = clientid, %a = ipaddress, %t = topic

{super_req, [
  {method, post},
  {url, "http://localhost:8080/mqtt/superuser"},
  {params, [
    {username, "%u"},
    {clientid, "%c"}
  ]}
]}.

{auth_req, [
  {method, post},
  {url, "http://localhost:8080/mqtt/auth"},
  {params, [
    {clientid, "%c"},
    {username, "%u"},
    {password, "%P"}
  ]}
]}.

%% 'access' parameter: sub = 1, pub = 2

{acl_req, [
  {method, post},
  {url, "http://localhost:8080/mqtt/acl"},
  {params, [
    {access,   "%A"},
    {username, "%u"},
    {clientid, "%c"},
    {ipaddr,   "%a"},
    {topic,    "%t"}
  ]}
]}.

```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emqttd_auth_http
```

HTTP API
--------

200 if ok

4xx if unauthorized

License
-------

Apache License Version 2.0

Author
------

feng at emqtt.io

