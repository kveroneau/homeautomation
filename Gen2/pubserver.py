import zmq.green as zmq
from gevent import Greenlet, queue

class PublishServer(Greenlet):
    def __init__(self):
        Greenlet.__init__(self)
        self.__queue = queue.Queue()
    def say(self, message):
        self.__queue.put('say %s' % message)
    def hud_log(self, message):
        self.__queue.put('log %s' % message)
    def hud(self, num, message):
        self.__queue.put('hud%s %s' % (num, message))
    def sayAndCmd(self, message, cmd):
        self.say(message)
        self.__queue.put('cmd %s' % cmd)
    def hud_off(self):
        self.hud(1, 'OFF')
        self.hud(2, 'OFF')
    def playurl(self, message, url):
        self.say(message)
        self.__queue.put('cmd playurl %s' % url)
    def _run(self):
        ctx = zmq.Context()
        sock = ctx.socket(zmq.PUB)
        sock.bind('tcp://*:9898')
        while True:
            message = self.__queue.get()
            sock.send(message)

pub = PublishServer()
