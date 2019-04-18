#!/usr/bin/python

import dbus, datetime, os, sys, signal, socket, struct, StringIO, zmq

pid = os.fork()
if pid > 0:
    sys.stdout.write("Forked process: %d\n" % pid)
    sys.exit(0)
null = open(os.devnull,'r+')
sys.stdout = null
sys.stderr = null

def handler(signum, frame):
    raise KeyboardInterrupt
signal.signal(signal.SIGTERM, handler)

notify = dbus.SessionBus().get_object('org.kde.knotify','/Notify')

def knotify(title, message):
    """ Queue a notification message to KNotify. """
    notify.event('warning','kde',[],title,message,[],[],0,0,dbus_interface='org.kde.KNotify')
    return True

knotify('Commander Daemon', 'Remote Notification daemon ready.')
try:
    ctx = zmq.Context()
    sock = ctx.socket(zmq.SUB)
    sock.connect('tcp://pi:9898')
    sock.setsockopt(zmq.SUBSCRIBE, 'say')
    sock.setsockopt(zmq.SUBSCRIBE, 'log')
    while True:
        data = sock.recv()
        channel, message = data.split(' ',1)
        if channel == 'log':
            knotify('Commander Daemon', message)
        elif channel == 'say':
            knotify('Home Control Unit', message)
except KeyboardInterrupt:
    knotify('Commander Daemon', 'Remote Notification daemon stopped.')
