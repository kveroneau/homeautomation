from gevent.server import StreamServer
from gevent import spawn_later
import struct

req = struct.Struct('IB80p')

HUD_MAP = ['192.168.xxx.xxx', '192.168.xxx.xxx']

class CUProtocol(object):
    def __init__(self, srv, conn):
        self.srv = srv
        self.conn = conn
        self.cname = None
        self.curId = 0
    def push(self, op, data):
        self.curId +=1
        self.conn.sendall(req.pack(self.curId, op, data))
    def parse(self, i, op, data):
        if op == 10:
            self.cname = data
            self.srv.clients[data] = self
            self.push(op, 'CNAME Set.')
        elif op == 20:
            #spawn_later(30, pushtest, self.srv.clients[data])
            self.push(op, 'Done.')
        else:
            self.push(254, 'ERR')
    def handle(self):
        self.push(1, 'Test Server!')
        while True:
            pkt = self.conn.recv(128)
            if len(pkt) == 0:
                break
            i, op, data = req.unpack(pkt[:req.size])
            if op == 255:
                self.push(op,'Good bye!')
                break
            self.parse(i, op, data)
        if self.cname is not None:
            del self.srv.clients[self.cname]

class TCPServer(StreamServer):
    def __init__(self, addr, *args, **kwargs):
        StreamServer.__init__(self, addr, *args, **kwargs)
        self.clients = {}
    def handle(self, sock, addr):
        self.clients[addr[0]] = CUProtocol(self, sock)
        self.clients[addr[0]].handle()
        if self.clients.has_key(addr[0]):
            del self.clients[addr[0]]
    def pushAll(self, op, data):
        for c in self.clients.values():
            c.push(op, data)
    def say(self, message):
        self.pushAll(10, message)
    def hud_log(self, message):
        self.pushAll(20, message)
    def hud(self, num, message):
        hip = HUD_MAP[num-1]
        if hip in self.clients.keys():
            self.clients[hip].push(30, message)
    def sayAndCmd(self, message, cmd):
        self.say(message)
        self.pushAll(50, cmd)
    def hud_off(self):
        self.hud(1, 'OFF')
        self.hud(2, 'OFF')
    def playurl(self, message, url):
        self.sayAndCmd(message, 'playurl %s' % url)

if __name__ == '__main__':
    server = TCPServer(('0.0.0.0', 5199))
    print 'TCPServer started on port 5199.'
    server.serve_forever()
