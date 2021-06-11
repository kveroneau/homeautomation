import zmq.green as zmq
from gevent import Greenlet, queue
from tcpserver import TCPServer

class PublishServer(Greenlet):
    def __init__(self):
        Greenlet.__init__(self)
        self.__queue = queue.Queue()
    def say(self, message):
        self.__queue.put('say %s' % message)
        self.tcp.say(message)
    def hud_log(self, message):
        self.__queue.put('log %s' % message)
        self.tcp.hud_log(message)
    def hud(self, num, message):
        self.__queue.put('hud%s %s' % (num, message))
        self.tcp.hud(num, message)
    def sayAndCmd(self, message, cmd):
        self.say(message)
        self.__queue.put('cmd %s' % cmd)
        self.tcp.sayAndCmd(message, cmd)
    def hud_off(self):
        self.hud(1, 'OFF')
        self.hud(2, 'OFF')
        self.tcp.hud_off()
    def playurl(self, message, url):
        self.say(message)
        self.__queue.put('cmd playurl %s' % url)
        self.tcp.playurl(message, url)
    def _run(self):
        ctx = zmq.Context()
        sock = ctx.socket(zmq.PUB)
        sock.bind('tcp://*:9898')
        self.tcp = TCPServer(('0.0.0.0', 5199))
        self.tcp.start()
        while True:
            message = self.__queue.get()
            sock.send(message)

pub = PublishServer()
