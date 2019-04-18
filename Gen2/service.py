#!/usr/bin/python

import sys, os

if len(sys.argv) > 1 and sys.argv[1] == '-d':
    pid = os.fork()
    if pid > 0:
        open('service.pid', 'w').write(str(pid))
        #sys.stdout.write('PID: %s\n' % pid)
        sys.exit(0)
    null = open(os.devnull, 'w')
    sys.stdout = null
    sys.stderr = null

from gevent import monkey
monkey.patch_all()
from pubserver import pub
from bottle import run
from endpoints import app
from automation import cron
from scheduler import tm
from health import monitor

pub.start()
pub.say('Publisher started.')
cron.start()
tm.start()
monitor.start()

run(app=app, host='0.0.0.0', server='gevent', port=8080, debug=False)
