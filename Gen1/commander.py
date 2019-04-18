import os

"""
This Python module used to be the main module before my home automation project exploded with ideas!

If you wish to try out this project, please use server.py instead.
"""
if __name__ == '__main__':
  import sys
  pid = os.fork()
  if pid > 0:
    sys.stdout.write('PID: %s\n' % pid)
    open('commander.pid','w').write(str(pid))
    sys.exit(0)
  null = open(os.devnull, 'r+')
  sys.stdout = null
  sys.stderr = null

import threading, Queue, struct, socket, subprocess
import cStringIO as StringIO

class CommanderThread(threading.Thread):
  def __init__(self):
    threading.Thread.__init__(self)
    self.queue = Queue.Queue()
    self.running = threading.Event()
    self.__proc = None
  def run(self):
    self.say('Commander started.')
    self.running.set()
    while self.running.is_set():
      cmd = self.queue.get()
      if cmd == 'quit':
        break
      if cmd.startswith('espeak'):
        words = cmd[8:].rstrip('"')
        epk = subprocess.Popen(['espeak','--stdout',words], stdout=subprocess.PIPE)
        subprocess.Popen(['aplay'], stdin=epk.stdout, stderr=null).wait()
        epk.wait()
      elif cmd.startswith('play'):
        self.__proc = subprocess.Popen(cmd.split(' '), stdout=null, stderr=null)
        self.__proc.wait()
      else:
        self.say('I do not support that command.')
    self.running.clear()
  def addCommand(self, cmd):
    self.queue.put(cmd)
  def say(self, words):
    self.queue.put('espeak "%s"' % words)
  def shutdown(self):
    self.running.clear()
    self.say('Good bye.')
    self.queue.put('quit')
  def terminate(self):
    if self.__proc:
      self.say('Last command terminated.')
      self.__proc.terminate()

def unpack(data):
  buf = StringIO.StringIO(data)
  op, tlen, flen = struct.unpack('BBB', buf.read(3))
  return op, buf.read(tlen), buf.read(flen)

def pack(text, cmd, op=1):
  buf = StringIO.StringIO()
  buf.write(struct.pack('BBB', op, len(text), len(cmd)))
  buf.write(text+cmd)
  return buf.getvalue()

class CommanderClient(threading.Thread):
  def __init__(self):
    threading.Thread.__init__(self)
    self.queue = Queue.Queue()
    self.running = threading.Event()
  def __hudsend(self, ip_addr, text, cmd, op):
      try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((ip_addr, 9898))
        s.send(pack(text, cmd, op)+'\0')
        s.close()
      except:
        pass    
  def run(self):
    self.say('Command client started.')
    self.__sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    ttl_bin = struct.pack('@i', 1)
    self.__sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, ttl_bin)
    self.running.set()
    while self.running.is_set():
      cmd = self.queue.get()
      text, cmd, op = cmd
      try:
        self.__sock.sendto(pack(text, cmd, op)+'\0', ('224.0.0.69', 9898))
      except:
        self.say('I had a problem encoding the network packets.')
      for hud_ip in (2,3):
	self.__hudsend('192.168.1.%s' % hud_ip, text, cmd, op)
      if op == 254:
	break
    self.running.clear()
  def __send(self, text, cmd, op=1):
    self.queue.put([text, cmd, op])
  def shutdown(self):
    self.__send('X','X',254)
  def addCommand(self, cmd):
    self.__send('NA', cmd, 2)
  def say(self, words):
    self.__send(words, 'NA', 1)
  def sayAndCmd(self, words, cmd):
    self.__send(words, cmd, 3)
  def terminate(self):
    self.__send('X','X',4)
  def hudbd_on(self):
    self.__send('\1','\1',200)
  def hudbd_off(self):
    self.__send('\1','\2',200)
  def hudx_on(self):
    self.__send('\1','\3',200)
  def hudx_off(self):
    self.__send('\1','\4',200)
  def hud_off(self):
    self.__send('\1',chr(254),200)
  def hud_brightness(self, bri):
    self.__send('\2',chr(bri),200)
  def hud_log(self, message):
    self.__send('\3',message,200)
  def hud_alarm(self):
    self.__send('\4','X',200)
  def hud_voice(self, text):
    self.__send('\5', text, 200)
  def playurl(self, text, url):
    self.__send(text, url, 5)
  def hud_still(self):
    self.__send(chr(80),'X',200)
  def hud_video(self, secs=60):
    self.__send(chr(81),chr(secs),200)

def run_server():
  sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  sock.bind(('', 9898))
  group_bin = socket.inet_pton(socket.AF_INET, '224.0.0.69')
  mreq = group_bin+struct.pack('=I', socket.INADDR_ANY)
  sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
  while True:
    data, sender = sock.recvfrom(1500)
    try:
      op, text, cmd = unpack(data.rstrip('\0'))
      if op == 1:
        commander.say(text)
      elif op == 2:
        commander.addCommand(cmd)
      elif op == 3:
        commander.say(text)
        commander.addCommand(cmd)
      elif op == 4:
	commander.terminate()
      elif op == 200:
	pass # We don't care about this opcode.
      elif op == 254:
	commander.shutdown()
      else:
        commander.say('Unknown operation code given to me.')
    except:
      commander.say('Error parsing UDP packet.')

if __name__ == '__main__':
  import signal
  def handler(signum, frame):
    raise KeyboardInterrupt
  signal.signal(signal.SIGTERM, handler)
  commander = CommanderThread()
  commander.start()
  try:
    run_server()
  except KeyboardInterrupt:
    commander.shutdown()
