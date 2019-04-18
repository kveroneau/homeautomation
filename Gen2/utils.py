from gevent import spawn
import urllib

def __send_event(event, message):
    urllib.urlopen('https://maker.ifttt.com/trigger/%s/with/key/***IFTTT_KEY***' % event, urllib.urlencode({'value1':message}))

def send_event(event, message):
    return spawn(__send_event, event, message)

def watch_notify(message):
    send_event('log_event',message)
