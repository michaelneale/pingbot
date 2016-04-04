#!/usr/bin/env python
import SocketServer
import BaseHTTPServer


testurl = "http://www.vimeostatus.com/metrics-display/xz8v6gh243wt/day.json"

import urllib2
import json


def fetchMetrics (url):
    response = urllib2.urlopen(url)
    parsed = json.loads(response.read())

    metric_list = parsed['metrics']
    metrics = {}
    for m in metric_list:
        m_name = m['metric']['name']
        m_val = m['summary']['mean']
        metrics[m_name] = m_val
    return { "metrics" : metrics }




class MyHandler(BaseHTTPServer.BaseHTTPRequestHandler):
    """ convert from statusio format to rad """

    def do_GET(s):
        print s.path
        s.send_response(200)
        s.send_header("Content-type", "application/json")
        s.end_headers()
        print "ok"
        resp = fetchMetrics(s.path[1:])
        print resp
        s.wfile.write(json.dumps(resp))




if __name__ == "__main__":
    port = 7070
    httpd = SocketServer.TCPServer(("", port), MyHandler)
    print "Serving at port %d" %  port
    httpd.serve_forever()
