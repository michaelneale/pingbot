# Pinger bot for radalert

The pingbot takes a checks.json (see example one) and then regularly heartbeats a status to HEART_URL. By default it will check about each minute, and allow about 10 seconds for a response. There are a few modes of checking - the default will just issue a HEAD request (efficient on the site as no content needs to be transferred).

Rad.

# Usage

To use: ```export HEART_URL=http://requestb.in/15ejt921" && ./pinger```.

Also set `CONSOLE_URL` and `CONSOLE_AUTH` and it will automatically load config from the appropriate url (has hard coded defaults)

This will read from checks.json in the same directory (see the one in the root of this project for examples). 

The heartbeat will look something like (raw http body): 
```
{
  "ttl" : 400,
  "state" : "OK",
  "tags" : ["blah"],
  "summary" : "OK",
  "api_key" : "1234",
  "check" : "https://www.google.com.au",
  "metric" :42
}
```


And arrive every minute. requestb.in is a handy utility site - create an endpoint and you can have test POSTs go to it. 

The list of URLs to check will come from whatever CONSOLE_URL is set to (ie will fetch periodically what it is meant to check). Look at checks.json for the JSON format of check configuration. In DEV mode this will be served up automatically from localhost (so you can just edit the file and it will pick up the changes).


By default, any < 500 code is considered success. You can specify an exact code, or optionally a body pattern.

Rad.


## building

### development
```
make shell # opens a shell in a docker container
OPTIONAL: make cabal-init - if brand new setup
cabal run Main # runs the program from source
```

### binary

```
make install
```
$PWD/pinger is the binary you can run. 
