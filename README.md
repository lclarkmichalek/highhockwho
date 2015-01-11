HighHockWho!
============

A skydns2 compatible Docker watcher and etcd inserter! Written in Haskell!

Configuration
-------------

HighHock takes a single configuration file, which can be passed via the
`--config` argument variable, or will otherwise be assumed to be at
`/etc/highhock.json`. Its format is defined below:

    {
      "etcdUrl": "http://localhost:4001/", # The url etcd can be found at
      "etcdTTL": 60, # The ttl of the keys inserted into etcd
      "skydnsDomain": "highhock.local", # The domain prefix to insert services under
      "skydnsTTL": 60, # The ttl of skydns records. Ignored if lower than etcdTTL
      "dockerVersion": "v1.16", # The version of the doker API. Prob should be autodetected
      "dockerUrl": "http://localhost:2375", # The url of the Docker API

      "publicHost": "10.77.66.0", # The host to insert records as. Not required
      "publicInterface": "eth0", # The interface to try and find the public host from. Not required
    }

This format will probably change when I get better at writing `FromJSON`
instances.

How it works
------------

HighHock periodically polls the Docker api to see what containers are running.
It then looks up Docker metadata for that container, works out what (if any)
ports the container is exposing and wants discovered, and then puts the relevant
host:port pair in etcd, in a format skydns understands.

### Discovery

By default, HighHock looks for environment variables in the form
`DISCOVERY=serv1:1000,serv2:302`. Seeing this environment variable, HighHock
would try and index 2 services, `serv1` at whatever port `1000` is exposed as
externally, and `serv2` at whatever port `302` is exposed as externally.

For example, if we have a container that was started using the following
command:

    docker run --rm -e "DISCOVER=httpd:80" -p 8080:80 nginx

Then HighHock would see the discover environment variable, extract the fact that
`httpd` is being exposed internally on `80`, then resolve the external port to
`8080`, and thus insert a record for `(httpd, 80)`.

I plan to support other methods of discovery apart from the environment variable
approach.

### Insertion

Having retrieved a pair of `(name, port)`, HighHock will then try and work out
what domain name it represents, and then insert it accordingly. This works by
taking the service name, prepending it to the `skydnsDomain` config entry split
by `.`s, reversing the log, joining it together with `/`s and then prepending
the global skydns prefix (currently hard coded to `skydns/`). Taking our earlier
`httpd` example, with the config given as an example in the Config section:

    "httpd"
    -> ["httpd", "highhock", "local"]
    -> ["local", "highhock", "httpd"]
    -> "local/highhock/httpd"
    -> "skydns/local/highhock/httpd"

This key will then have `{"host": "10.77.66.0", "port": 8080, "ttl": 60}`
inserted. This should result, with the correct skydns setup, with
`dig A httpd.highhock.local` returning `10.77.66.0` (and the `SRV` doing the
same, though who cares about `SRV` anyway).

### Host detection

HighHock has a number of ways to determine which host to insert as. Firstly, you
can manually configure the host to insert as by either setting the `publicHost`
configuration parameter, or by setting the `EXTERNAL_IP` environment variable
(A+ for consistency, eh?). If both of these fail, HighHock will attempt to work
out the correct IP itself, which is usually a terrible thing, and should be
avoided, but eh.

The first way HighHock will attempt to find your IP is by looking through your
network interfaces for a likely customer. By default, HighHock will take the IP
of the `eth0` interface if it is availible, though the interface looked at can
be configured via the `publicInterface` configuration parameter.

TODO: AWS/DO/GCE metadata service

If all of these fail, then HighHock will error out! Kinda shit, eh? Must do
something about that

Internals
=========

HighHock is a simple Haskell program. It is not complicated! It uses the Conduit
library to define the program flow. The program is composed of two main pipes:

### The container watcher

Each running container being monitored has a single pipeline dedicated to it.
This consists of a producer that polls the Docker metadata periodically
(Watcher.hs, watchContainer), passing the metadata on to a conduit that extracts
Domain objects from it (Extractor.hs, envVarExtract), which are then inserted
into etcd by the inserter (Inserter.hs, inserter). The pipeline is controlled by
a Controller object (Controller.hs) which is essentially an MVar that is
empty/triggered/stopped.

### The global watcher

The main function starts a producter that polls the Docker API for the Container
list. This is coupled to a sink that maintains an up to date registry (Registry.hs)
of the running containers, and the watcher pipes (or at least, the Controllers)
for them. When a container is missing from the Docker API's list, but present in
the Registry, its Controller is stopped, and it is discarded. Similarly, when a
Container is in the Docker API's list but not the registry, it is added to the
registry, a new watcher is started and a seperate thread to periodically trigger
the controller is started.

License
=======

HighHock is AGPL-3. If that's totally impossible for you to deal with, send me
an email.
