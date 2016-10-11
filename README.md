
emq_auth_http
=============

HTTP Auth/ACL Plugin for EMQ 3.0

Build
-----

```
make && make tests
```

Configuration
-------------

File: etc/emq_auth_http.conf

```
## Variables: %u = username, %c = clientid, %a = ipaddress, %P = password, %t = topic

auth.http.authreq = http://localhost:8080/mqtt/auth
auth.http.authreq.method = post
auth.http.authreq.params = clientid=%c,username=%u,password=%P

auth.http.super_req = http://localhost:8080/mqtt/superuser
auth.http.super_req.method = post
auth.http.super_req.params = clientid=%c,username=%u

## 'access' parameter: sub = 1, pub = 2
auth.http.aclreq = http://localhost:8080/mqtt/acl
auth.http.aclreq.method = get
auth.http.aclreq.params = access=%A,username=%u,clientid=%c,ipaddr=%a,topic=%t

```

Load the Plugin
---------------

```
./bin/emqttd_ctl plugins load emq_auth_http
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

