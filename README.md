emqx_auth_http
=============

EMQ X HTTP Auth/ACL Plugin

Build
-----

```
make && make tests
```

Configure the Plugin
--------------------

File: etc/emqx_auth_http.conf

```
## Variables: %u = username, %c = clientid, %a = ipaddress, %P = password, %t = topic

auth.http.auth_req = http://127.0.0.1:8080/mqtt/auth
auth.http.auth_req.method = post
auth.http.auth_req.params = clientid=%c,username=%u,password=%P

auth.http.super_req = http://127.0.0.1:8080/mqtt/superuser
auth.http.super_req.method = post
auth.http.super_req.params = clientid=%c,username=%u

## 'access' parameter: sub = 1, pub = 2
auth.http.acl_req = http://127.0.0.1:8080/mqtt/acl
auth.http.acl_req.method = get
auth.http.acl_req.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t
```

Load the Plugin
---------------

```
./bin/emqx_ctl plugins load emqx_auth_http
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

EMQ X-Men Team.

