from gevent import Greenlet, sleep
from pubserver import pub
from automation import cron
from scheduler import tm

class HealthChecker(Greenlet):
    def _run(self):
        while True:
            sleep(60*5)
            if not cron:
                pub.say('The system automation thread has died!')
                pub.hud_log('Automation thread dead.')
            if not tm:
                pub.say('The time manager thread has died!')
                pub.hud_log('Time manager thread dead.')

monitor = HealthChecker()
