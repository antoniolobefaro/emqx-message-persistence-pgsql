emqx_message_persistence_pgsql
===============

Authentication/ACL with PostgreSQL Database.

Build Plugin
------------

make && make tests

Configuration
-------------

File: etc/emqx_message_persistence_pgsql.conf

```
## PostgreSQL server address.
##
## Value: Port | IP:Port
##
## Examples: 5432, 127.0.0.1:5432, localhost:5432
persmessage.pgsql.server = 127.0.0.1:5432

## PostgreSQL pool size.
##
## Value: Number
persmessage.pgsql.pool = 8

## PostgreSQL username.
##
## Value: String
persmessage.pgsql.username = root

## PostgreSQL password.
##
## Value: String
## persmessage.pgsql.password =

## PostgreSQL database.
##
## Value: String
persmessage.pgsql.database = mqtt

## PostgreSQL database encoding.
##
## Value: String
persmessage.pgsql.encoding = utf8

## Whether to enable SSL connection.
##
## Value: true | false
persmessage.pgsql.ssl = false

## SSL keyfile.
##
## Value: File
## persmessage.pgsql.ssl_opts.keyfile =

## SSL certfile.
##
## Value: File
## persmessage.pgsql.ssl_opts.certfile =

## SSL cacertfile.
##
## Value: File
## persmessage.pgsql.ssl_opts.cacertfile =

## Authentication query.
##
## Value: SQL
##
## Variables:
##  - %u: username
##  - %c: clientid
##
persmessage.pgsql.auth_query = select password from mqtt_user where username = '%u' limit 1

## Password hash.
##
## Value: plain | md5 | sha | sha256 | bcrypt
persmessage.pgsql.password_hash = sha256

## sha256 with salt prefix
## persmessage.pgsql.password_hash = salt,sha256

## sha256 with salt suffix
## persmessage.pgsql.password_hash = sha256,salt

## bcrypt with salt prefix
## persmessage.pgsql.password_hash = salt,bcrypt

## pbkdf2 with macfun iterations dklen
## macfun: md4, md5, ripemd160, sha, sha224, sha256, sha384, sha512
## persmessage.pgsql.password_hash = pbkdf2,sha256,1000,20

## Superuser query.
##
## Value: SQL
##
## Variables:
##  - %u: username
##  - %c: clientid
persmessage.pgsql.super_query = select is_superuser from mqtt_user where username = '%u' limit 1

## ACL query. Comment this query, the ACL will be disabled.
##
## Value: SQL
##
## Variables:
##  - %a: ipaddress
##  - %u: username
##  - %c: clientid
persmessage.pgsql.acl_query = select allow, ipaddr, username, clientid, access, topic from mqtt_acl where ipaddr = '%a' or username = '%u' or username = '$all' or clientid = '%c'
```

Load Plugin
-----------

./bin/emqx_ctl plugins load emqx_message_persistence_pgsql

Auth Table
----------

Notice: This is a demo table. You could authenticate with any user table.

```sql
CREATE TABLE mqtt_user (
  id SERIAL primary key,
  is_superuser boolean,
  username character varying(100),
  password character varying(100),
  salt character varying(40)
)
```

ACL Table
---------

```sql
CREATE TABLE mqtt_acl (
  id SERIAL primary key,
  allow integer,
  ipaddr character varying(60),
  username character varying(100),
  clientid character varying(100),
  access  integer,
  topic character varying(100)
)

INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)
VALUES
	(1,1,NULL,'$all',NULL,2,'#'),
	(2,0,NULL,'$all',NULL,1,'$SYS/#'),
	(3,0,NULL,'$all',NULL,1,'eq #'),
	(5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),
	(6,1,'127.0.0.1',NULL,NULL,2,'#'),
	(7,1,NULL,'dashboard',NULL,1,'$SYS/#');
```
**allow:** Client's permission to access a topic. '0' means that the client does not have permission to access the topic, '1' means that the client have permission to access the topic.

**ipaddr:** Client IP address. For all ip addresses it can be '$all' or 'NULL'. 

**username:** Client username. For all users it can be '$all' or 'NULL'. 

**clientid:** Client id. For all client ids it can be '$all' or 'NULL'. 
	
**access:** Operations that the client can perform. '1' means that the client can subscribe to a topic, '2' means that the client can publish to a topic, '3' means that the client can subscribe and can publish to a topic.

**topic:** Topic name. Topic wildcards are supported. 

**Notice that only one value allowed for ipaddr, username and clientid fields.**

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.

