#!/usr/bin/python

import sys, os

if len(sys.argv) > 1 and sys.argv[1] == '-d':
  pid = os.fork()
  if pid > 0:
    open('server.pid', 'w').write(str(pid))
    #sys.stdout.write('PID: %s\n' % pid)
    sys.exit(0)
  null = open(os.devnull, 'w')
  sys.stdout = null
  sys.stderr = null

from phue import Bridge, PhueRequestTimeout
from twilio.rest import TwilioRestClient
from bottle import view, run, get, request, post
from commander import CommanderClient
from voice import VoiceAction
from ecweather import weather_cache, WeatherException
from sunrise_sunset import SunriseSunset, CIVIL_ZENITH
import datetime, shelve, threading, urllib, xbmclib, threading, socket, json, time, signal

SID = '***TWILIO_SID***'
TOKEN = '***TWILIO_TOKEN***'

BAN_LIST = []

hue = Bridge('hue') # Uses Static DHCP and DNS to properly resolve.
commander = CommanderClient()
commander.start()
xbmc = xbmclib.XBMC()

class HueClient(threading.Thread):
  def get_lighting(self):
    self.__lights = {}
    for l in self.__hue.lights:
      self.__lights[l.name] = {'on':l.on,
                               'hue':l.hue,
                               'brightness':l.brightness,
                               'saturation':l.saturation,
                               'reachable':l.reachable}
  def set_light(self, name, on, hue, brightness, saturation):
    self.__light[name].on = on
    self.__light[name].hue = hue
    self.__light[name].brightness = brightness
    self.__light[name].saturation = saturation
  def ensure_lighting(self):
    for lgt in self.__lights.keys():
      try:
        self.set_light(lgt, self.__lights[lgt]['on'], self.__lights[lgt]['hue'], self.__lights[lgt]['brightness'], self.__lights[lgt]['saturation'])
      except PhueRequestTimeout:
        self.set_light(lgt, self.__lights[lgt]['on'], self.__lights[lgt]['hue'], self.__lights[lgt]['brightness'], self.__lights[lgt]['saturation'])
  def __getitem__(self, key):
    #self.get_lighting()
    return self.__lights[key]
  def __setitem__(self, key, value):
    pass
  def run(self):
    self.__hue = Bridge('hue')
    self.__light = hue.get_light_objects('name')
    self.__event = threading.Event()
    self.__running = threading.Event()
    self.get_lighting()
    self.__running.set()
    while self.__running.is_set():
      self.__event.wait()

class ScheduleThread(threading.Thread):
  AUTO_OFF = {
    'always': ('Upstairs hallway', 'Entrance', 'Iris', 'Kitchen', 'Candle', 'Backdoor',),
    'Study': ('Living room', 'Bedroom',),
    'Living room': ('Study', 'Bedroom',),
    'Bedroom': ('Study', 'Living room',),
  }
  ALWAYS_ON = ('Bathroom',)
  TIME = 5
  HEARTBEAT_URLS = {
    'Upstairs switch': 'http://upstairs/',
    "Hacker's Edge website": 'http://www.hackers-edge.com/',
    'Python Dairy': 'http://www.pythondiary.com/',
    'Veroneau dot net': 'https://www.veroneau.net/',
  }
  HEARTBEAT_TCP = {
    'sys1': ('sys1',22),
    'sys2': ('sys2',22),
    'bedroom hud': ('alarm',9898),
    'living room hud': ('livingroom',9898),
    'kitchen hud': ('kitchen',22),
    'node1': ('node1.veroneau.net',22),
    'node2': ('node2.veroneau.net',22),
    "Hacker's Edge server": ('hackers-edge.com',1337),
  }
  def __init__(self):
    threading.Thread.__init__(self)
    self.finish = threading.Event()
    self.checker = {}
    self.allow_rooms = False
    self.__pause = False
    self.__config = {}
    self.__lock = threading.Lock()
    self.__lostnet = None
    self.__tenit = False
    self.__wakecount = 0
    self.__reminders = {}
    self.visiting = None
    self.__offcount = 0
    self.__sunrise = None
    self.__sunset = None
    self.__darkmode = False
    self.wakeme = True
    self.__available = []
    for name in self.HEARTBEAT_URLS.keys():
      self.__available.append(name)
    for name in self.HEARTBEAT_TCP.keys():
      self.__available.append(name)
  @property
  def pause(self):
    return self.__pause
  @pause.setter
  def pause(self, value):
    self.__pause = value
    if value == False:
      self.__offcount = 0
  def cancel(self):
    self.finish.set()
  def add_reminder(self, when, message):
    with self.__lock:
      self.__reminders[when] = message
  def __get_config(self):
    try:
      cfg = json.loads(urllib.urlopen('https://www.veroneau.net/***URL_MASKED**?calls=no').read())
      with self.__lock:
        self.__config = cfg
    except:
      if not self.is_sleeping:
        commander.say('I have detected a problem with your Internet Kevin.')
      self.__lostnet = datetime.datetime.now()
      commander.hud_log('Internet outage at %s' % self.__lostnet.strftime('%H:%M'))
      time.sleep(2)
  def __update_sun(self):
    now = datetime.datetime.now()
    if now.hour == 2 and self.__sunrise.day != now.day:
      self.__sunrise = None
    if self.__sunrise == None:
      commander.hud_log('Updating sunset and sunrise...')
      r = SunriseSunset(now, 51.0486, -114.0708, -7, CIVIL_ZENITH)
      self.__sunrise, self.__sunset = r.calculate()
      del r
      commander.hud_log('Sunrise: %s, Sunset: %s' % (self.__sunrise, self.__sunset))
  def __schedule(self):
    if not self.is_home:
      self.__tenit = False
      self.__darkmode = False
      return
    try:
      if self.is_dark and not self.__darkmode:
	if not self.is_sleeping:
	  commander.hud_log('Setting dark mode.')
	  commander.say('It is after sunset Kevin, setting dark mode.')
	  urllib.urlopen('http://localhost:8080/strips')
	  self.__darkmode = True
      elif self.__darkmode and not self.is_dark:
	if not self.is_sleeping:
	  commander.say('It is now sunrise Kevin, turning off lights.')
	commander.hud_log('Setting day mode.')
        for l in hue.lights:
          set_light(l, False)
        self.__darkmode = False
      if not self.wakeme:
        return
      hr = datetime.datetime.now().hour
      wd = datetime.datetime.now().isoweekday()
      now = datetime.datetime.now().strftime('%H:%M')
      if now in self.__reminders.keys():
	commander.say(self.__reminders[now])
	del self.__reminders[now]
	time.sleep(5)
      RELAX_HR, WAKEUP_HR, WORK_HR = 20,7,9
      #if wd == 3:
      #  WAKEUP_HR, WORK_HR = 6,8
      if wd == 7 or wd == 6:
	WAKEUP_HR = 9
      if not self.__tenit and hr == RELAX_HR:
        if self.is_sleeping:
          return
        self.__tenit = True
        urllib.urlopen('https://www.veroneau.net/***URL_MASKED***')
        #urllib.urlopen('https://www.veroneau.net/***URL_MASKED***')
        rhr = RELAX_HR-12
        if cron.visiting:
          commander.say('%s, it is now after %s.  Are you planning on sleeping over tonight?' % (cron.visiting, rhr))
        else:
          commander.say('Kevin, it is now after %s.  I have enabled sleep mode for you.' % rhr)
        if self.is_dark:
          time.sleep(5)
          urllib.urlopen('http://localhost:8080/relax')
      elif self.__tenit and hr == RELAX_HR+1:
        self.__tenit = False
        if len(self.checker.keys()) == 0:
          return
        rhr = RELAX_HR-12+1
        commander.say('It is now after %s.  It is time for you to get to bed Kevin.' % rhr)
        if self.is_dark:
          time.sleep(5)
          if 'Bedroom' not in self.checker.keys():
            urllib.urlopen('http://localhost:8080/bedroom')
      elif self.is_sleeping and hr == WAKEUP_HR:
        if self.__wakecount == 0:
          commander.say('Wake up Kevin, it is time to keep your schedule.')
          #c = TwilioRestClient(SID, TOKEN)
          #call = c.calls.create(url='https://www.veroneau.net/***URL_MASKED***', to='+1587#######', from_='+1587#######')
          time.sleep(5)
          if self.is_dark:
            urllib.urlopen('http://localhost:8080/allbright')
            self.checker['Bedroom'] = 0
        elif self.__wakecount == 5:
          if 'Bedroom' not in self.checker.keys():
	    if self.is_dark:
              urllib.urlopen('http://localhost:8080/allbright')
              self.checker['Bedroom'] = 0
            time.sleep(5)
          commander.sayAndCmd('Did you seriously fall back asleep Kevin!', 'play /home/***/alarm.mp3')
        elif self.__wakecount > 0 and 'Bedroom' not in self.checker.keys():
          commander.say('Kevin!  Do I need to tell you %s times to get your butt out of that bed!' % self.__wakecount)
          time.sleep(5)
          if self.is_dark:
            urllib.urlopen('http://localhost:8080/allbright')
            self.checker['Bedroom'] = 0
	self.__wakecount+=1
      elif hr == WAKEUP_HR and self.__wakecount > 0:
        commander.say('Finally your up, it took me %s times to yell at you.' % self.__wakecount)
        self.__wakecount = 0
        time.sleep(5)
        if self.__lostnet is not None:
          commander.say('I have some important news for you Kevin.  I lost connection to the Internet at %s while you were sleeping.' % self.lostnet)
          time.sleep(5)
      elif wd > 0 and wd < 6:
        if not self.__tenit and hr == WORK_HR:
          self.__tenit = True
          commander.say('Kevin, should you not be leaving for work soon?')
          time.sleep(2)
      elif self.__tenit and hr == 11:
        self.__tenit = False
    except:
      commander.say('Kevin, there was an exception in your new schedule code.')
      open('exc.log','w').write('[%s] %s' % (sys.exc_info()[0], sys.exc_info()[1]))
      time.sleep(2)
  @property
  def available(self):
    return ', '.join(self.__available)
  def __heartbeat(self):
    for name, url in self.HEARTBEAT_URLS.items():
      try:
	if urllib.urlopen(url).code != 200:
	  raise
	if name not in self.__available:
	  self.__available.append(name)
	  commander.say('Kevin, %s is now online.' % name)
	  commander.hud_log('%s is now online.' % name)
      except:
	if name in self.__available:
	  if self.is_home:
	    commander.say('Kevin, I am currently unable to contact %s.' % name)
	  else:
	    send_event('phone_notify','Unable to contact %s.' % name)
	  commander.hud_log('Unable to contact %s.' % name)
	  self.__available.remove(name)
    for name, addr in self.HEARTBEAT_TCP.items():
      try:
	s = socket.socket()
	s.connect(addr)
	s.close()
	del s
	if name not in self.__available:
	  self.__available.append(name)
	  commander.say('Kevin, %s is now online.' % name)
	  commander.hud_log('%s is now online.' % name)
      except:
	s.close()
	del s
	if name in self.__available:
	  if self.is_home:
	    commander.say('Kevin, I am currently unable to contact %s.' % name)
	  else:
	    send_event('phone_notify','Unable to contact %s.' % name)
	  commander.hud_log('Unable to contact %s.' % name)
	  self.__available.remove(name)
  @property
  def is_home(self):
    with self.__lock:
      return self.__config['HOME']
  @property
  def is_sleeping(self):
    with self.__lock:
      return self.__config['SLEEPING']
  @property
  def is_dark(self):
    now = datetime.datetime.now()
    if now < self.__sunrise or now > self.__sunset:
      return True
    return False
  @property
  def offcount(self):
    with self.__lock:
      return self.__offcount
  @property
  def lostnet(self):
    with self.__lock:
      if self.__lostnet is None:
        return None
      result = self.__lostnet.strftime('%H:%M')
      self.__lostnet = None
    return result
  def real_run(self):
    lights_out = []
    main_light = None
    lights = self.__hue.get_light_objects('name')
    for l in self.__hue.lights:
      if l.name in self.ALWAYS_ON:
        continue
      if l.on:
        if l.name not in self.checker.keys():
          self.checker[l.name] = 0
	self.checker[l.name]+=1
	if self.checker[l.name] > self.TIME-1:
	  if l.name in self.AUTO_OFF['always']:
	    lights_out.append(l.name)
	    l.on = False
	    del self.checker[l.name]
	  elif not self.allow_rooms:
	    if l.name in self.AUTO_OFF.keys():
	      for lgt in self.AUTO_OFF[l.name]:
		if lgt in self.checker.keys() and self.checker[lgt] > self.TIME-1:
		  if len(lights_out) == 0:
		    main_light = l.name
		  lights_out.append(lgt)
		  lights[lgt].on = False
      else:
        if l.name in self.checker.keys():
          del self.checker[l.name]
    if len(lights_out) > 0:
      ctx = 'them'
      if len(lights_out) == 1:
	lgts = 'the %s light' % lights_out[0]
	ctx = 'it'
      elif len(lights_out) == 2:
	lgts = 'the %s and %s lights' % tuple(lights_out)
      else:
	lgts = '%s lights' % len(lights_out)
      if main_light is None:
	commander.say('Kevin, I noticed you left %s on.  I am turning %s off for you now.' % (lgts, ctx))
      else:
	commander.say('Kevin, I noticed you have the %s light on.  I will turn off %s for you now.' % (main_light, lgts))
      return
      if 'Entrance' in lights_out:
	if not self.is_home:
	  send_event('arm_away','entrance')
      elif 'Backdoor' in lights_out:
	if not self.is_home:
	  send_event('arm_away','backdoor')
  def run(self):
    self.__hue = Bridge('hue')
    self.__get_config()
    while not self.finish.is_set():
      self.finish.wait(60.0)
      if not self.finish.is_set():
	self.__get_config()
	self.__update_sun()
	self.__heartbeat()
	self.__schedule()
	try:
	  if not self.pause:
	    self.real_run()
	  else:
	    if not self.is_home:
	      self.__offcount = 0
	      commander.say('Kevin, I can see that the lights are paused and you are not home.  I am enabling light automation now.')
	      self.pause = False
	    else:
	      self.__offcount+=1
	      if self.__offcount > 60:
	        commander.say('Light automation has been disabled for over an hour now Kevin.  I am enabling light automation now.')
	        self.__offcount = 0
	        self.pause = False
	        self.real_run()
	      elif self.__offcount == 50:
		commander.say('I see that light automation has been off for almost an hour now Kevin.')
	except PhueRequestTimeout:
	  pass # Most likely a networking issue, we can ignore and try again in a minute.
	except socket.error:
	  if not self.is_sleeping:
	    commander.say('I am having trouble talking with your lights.')
        except:
          commander.say('An exception has occurred in schedule thread.')
          open('/tmp/exc.log','w').write('[%s] %s' % (sys.exc_info()[0], sys.exc_info()[1]))

class ReminderThread(threading.Thread):
  def __init__(self, reminder_list):
    threading.Thread.__init__(self)
    self.reminder_list = reminder_list
  def run(self):
    evt = threading.Event()
    evt.wait(60.0*3)
    if len(self.reminder_list) == 1:
      word = 'reminder'
    else:
      word = 'reminders'
    speech = 'You have %s %s Kevin.  ' % (len(self.reminder_list), word)
    for reminder in self.reminder_list:
      speech+='%s.  ' % reminder
    lostnet = cron.lostnet
    if lostnet is not None:
      speech+='I also lost my Internet connection at %s.' % lostnet
    commander.say(speech)

va = VoiceAction(commander)
cron = ScheduleThread()
cron.start()

def handler(signum, frame):
  commander.shutdown()
  cron.cancel()
  sys.exit(0)
signal.signal(signal.SIGTERM, handler)

wake_reminders = []
home_reminders = []

def set_light(light, on, hue=None, brightness=None, saturation=None):
  if light.name in cron.ALWAYS_ON:
    if on is False:
      return
  if cron.allow_rooms:
    if light.name in cron.AUTO_OFF.keys():
      if on is False:
        return
  tries = 0
  while True:
    try:
      light.on = on
      if hue is not None:
        light.hue = hue
        light.brightness = brightness
        light.saturation = saturation
      break
    except PhueRequestTimeout:
      commander.say('Bridge has timed out.')
      tries+=1
      if tries > 5:
	break

def light_checker(lights):
  for l in hue.lights:
    if l.name in lights.keys():
      print "Checking %s... %s" % (l.name, l.on)
      if l.on != lights[l.name]:
	commander.say('%s was fixed.' % l.name)
	l.on = lights[l.name]

def check_lights(lights):
  t = threading.Timer(15.0, light_checker, args=[lights])
  t.start()  

def loadtheme(name):
  db = shelve.open('/home/***/hue.db')
  data = db[name]
  db.close()
  chk = {}
  lights = hue.get_light_objects('name')
  for light in data:
    l = lights[data[light]['name']]
    set_light(l, data[light]['state']['on'], data[light]['state']['hue'], data[light]['state']['bri'], data[light]['state']['sat'])
    #l.on = data[light]['state']['on']
    #l.xy = data[light]['state']['xy']
    #l.brightness = data[light]['state']['bri']
    #l.saturation = data[light]['state']['sat']
    #chk[data[light]['name']] = data[light]['state']['on']
  return chk

def send_event(event, message):
  urllib.urlopen('https://maker.ifttt.com/trigger/%s/with/key/***IFTTT_KEY***' % event, urllib.urlencode({'value1':message}))

def watch_notify(message):
  send_event('log_event',message)

@get('/')
def hello():
  if request.remote_addr not in BAN_LIST:
    BAN_LIST.append(request.remote_addr)
    commander.hud_log('IP Added to BAN_LIST: %s' % request.remote_addr)
  return 'Hello'

@get('/hoff')
def lights_off():
  cron.pause = False
  commander.hud_off()
  hr = datetime.datetime.now().hour
  if cron.is_sleeping:
    commander.say('Good night Kevin.')
  else:
    commander.say('Good bye Kevin.')
  lights = {}
  for l in hue.lights:
    set_light(l, False)
    #l.on = False
    #lights[l.name] = False
  #check_lights(lights)
  return 'OK'

@get('/living')
def living_room():
  lights = hue.get_light_objects('name')
  set_light(lights['Living room'], True)
  return 'OK'

@get('/lights')
def show_lights():
  lights = hue.get_light_objects('name')
  return '<br/>'.join(lights.keys())

@get('/hue')
def set_hue():
  h = request.query.get('hue',None)
  if h is None:
    return '?'
  for l in hue.lights:
    if l.on:
      l.hue = h
  return 'OK'

@get('/home')
def coming_home():
  inside = False if request.query.get('inside',None) is None else True
  if inside:
    commander.hudx_on()
    t = ReminderThread(home_reminders)
    t.start()
  else:
    send_event('disarm_security','coming_home')
  hr = datetime.datetime.now().hour
  if hr > 7:
    #lights = hue.get_light_objects('name')
    #set_light(lights['Entrance'], True, 34494, 128, 232)
    if cron.is_dark:
      commander.say('Welcome home Kevin, activating lights.')
      loadtheme('ComingHome')
    else:
      commander.say('Welcome home Kevin!')
    return 'OK'
  else:
    commander.say('Setting romantic theme for your guests Kevin.')
    loadtheme('Romantic')
    return 'OK'

@get('/theme')
def set_theme():
  theme = request.query.get('name',None)
  if theme is None:
    db = shelve.open('hue.db')
    theme_list = ', '.join(db.keys())
    db.close()
    return theme_list
  loadtheme(theme)
  return 'OK'

@get('/save_theme')
def save_theme():
  theme = request.query.get('name',None)
  if theme is None:
    return 'OK'
  db = shelve.open('hue.db')
  db[theme] = hue.get_light()
  db.close()
  commander.say('I have saved the light theme Kevin.')
  return 'SAVED'

@get('/allbright')
def all_bright():
  cron.pause = True
  commander.hudbd_on()
  commander.hudx_on()
  commander.hud_brightness(100)
  commander.say('Setting maximum light brightness Kevin.')
  for l in hue.lights:
    set_light(l, True, 34494, 254, 232)
  return 'OK'

@get('/bath_alert')
def bath_alert():
  who = request.query.get('who',None)
  cron.visiting = who
  lights = hue.get_light_objects('name')
  set_light(lights['Bathroom'], True, 14910, 254)
  set_light(lights['Entrance'], True, 34494, 232)
  if who and cron.is_home:
    cron.pause = True
    commander.say('Kevin, %s has just been let in the front door downstairs.' % who)
    send_event('disarm_security','bath_alert')
  return 'OK'

@get('/on_call')
def on_call():
  cron.pause = True
  commander.hudbd_on()
  lights = hue.get_light_objects('name')
  for lgt in ('Bedroom', 'Kitchen', 'Study', 'Upstairs hallway',):
    set_light(lights[lgt], True, 34494, 232)
  commander.sayAndCmd('Kevin, you are currently receiving a call from work!', 'play /home/kveroneau/alarm.mp3')
  send_event('on_call', 'On call event')
  return 'OK'

@get('/stop_alarm')
def stop_alarm():
  commander.terminate()
  return 'OK'

@get('/relax')
def relax():
  commander.say('Setting relax mode.')
  for l in hue.lights:
    if l.on:
      set_light(l, True, 13088, 144, 213)
  return 'OK'

@get('/strips')
def strips():
  lights = hue.get_light_objects('name')
  set_light(lights['LightStrips'], True, 13088, 144, 213)
  return 'OK'

@get('/bedroom')
def bedroom():
  cron.pause = False
  if cron.is_sleeping:
    b, s, h = 32, 213, 13088
  else:
    b, s, h = 254, 232, 34494
  lights = hue.get_light_objects('name')
  if lights['Bedroom'].on:
    commander.hudbd_off()
    commander.hudx_on()
    if cron.is_dark:
      lgts = ('Living room', 'Upstairs hallway', 'Kitchen', 'LightStrips',)
    else:
      lgts = ()
  elif b == 32:
    if cron.visiting:
      commander.say("%s are you planning on staying the night?  It is now Kevin's bed time." % cron.visiting)
    else:
      commander.say('Are you ready to go to bed now Kevin?')
    commander.hudx_off()
    commander.hudbd_on()
    lgts = ('Bedroom', 'Upstairs hallway', 'LightStrips',)
  else:
    commander.hudx_off()
    commander.hudbd_on()
    if cron.is_dark:
      lgts = ('Bedroom', 'Upstairs hallway', 'LightStrips',)
    else:
      lgts = ('Bedroom',)
  for l in hue.lights:
    if l.name in lgts:
      set_light(l, True, h, b, s)
    elif l.on:
      set_light(l, False)
  return 'OK'

def ivr_shower(enable):
  if enable:
    params = {'enable':'yes', 'message':'in the shower'}
    send_event('taking_shower', 'Having a shower')
  else:
    params = {'enable':'no'}
  return urllib.urlopen('https://www.veroneau.net/***URL_MASKED***', urllib.urlencode(params)).read()

def shower_timer():
  lights = hue.get_light_objects('name')
  set_light(lights['Bathroom'], True, 34494, 254, 232)
  if cron.is_dark:
    set_light(lights['Bedroom'], True, 34494, 254, 232)
    set_light(lights['Upstairs hallway'], True, 34494, 254, 232)

@get('/bathroom')
def bathroom():
  lights = hue.get_light_objects('name')
  #cron.pause = True
  hr=datetime.datetime.now().hour
  if hr<6:
    ivr_shower(True)
    commander.say('Preparing a romantic shower.')
    b, s, h = 35, 253, 400
  else:
    ivr_shower(True)
    commander.say('Have a good shower Kevin.')
    tmr = threading.Timer(60.0*15, shower_timer)
    tmr.start()
    b, s, h = 233, 241, 47417
  for l in hue.lights:
    if l.name == 'Bathroom':
      set_light(l, True, h, b, s)
  return 'OK'

@get('/blacklight')
def blacklight():
  for l in hue.lights:
    if l.on:
      set_light(l, True, 47417, 233, 241)
  return 'OK'

@get('/study')
def study():
  lights = hue.get_light_objects('name')
  if lights['Study'].on:
    commander.hudx_on()
    commander.say('I have turned off the study light Kevin.')
    if cron.is_dark:
      lgts = ('Kitchen', 'Upstairs hallway', 'Living room', 'LightStrips')
    else:
      lgts = ()
  else:
    commander.hud_off()
    commander.say('Study light has been turned on.')
    lgts = ('Study',)
  for l in hue.lights:
    if l.name in lgts:
      set_light(l, True, 34494, 254, 232)
    elif l.on:
      set_light(l, False)
  return 'OK'

@get('/entrance')
def enterance():
  cron.pause = False
  lights = hue.get_light_objects('name')
  if lights['Entrance'].on:
    if cron.visiting:
      commander.say('Hello %s, how are you doing today?' % cron.visiting)
    else:
      commander.say('I have turned off the entrance light Kevin.')
    set_light(lights['Entrance'], False)
    #send_event('arm_security','enterance')
  else:
    try:
      if cron.visiting:
        commander.say('Are you going somewhere %s?  Keep warm outside, it is currently %s celsius.' % (cron.visiting, weather_cache.weather['feels_like']))
      else:
        commander.say('Kevin, here is the current forecast.  %s' % weather_cache.weather['futurecast'][0]['outlook'])
    except:
      if cron.visiting:
        commander.say('Good bye %s.' % cron.visiting)
      else:
        commander.say('Are you going somewhere Kevin?')
    if cron.is_dark:
      set_light(lights['Entrance'], True, 34494, 254, 232)
      set_light(lights['Backdoor'], True, 34494, 128, 232)
    send_event('disarm_security','entrance')
    cron.visiting = None
  return 'OK'  

@get('/arm_away')
def arm_away():
  ivr_shower(False)
  tmr = threading.Timer(30.0, send_event, args=['arm_away', 'leaving'])
  tmr.start()
  #send_event('arm_away','leaving')
  return 'OK'

@get('/arm_stay')
def arm_stay():
  send_event('arm_security','nfc_tap')
  return 'OK'

@get('/kitchen')
def kitchen():
  kitchen = hue.get_light_objects('name')['Kitchen']
  if kitchen.on == False:
    set_light(kitchen, True, 34494, 128, 232)
  elif kitchen.hue == 34494:
    set_light(kitchen, True, 400, 128, 232)
  elif kitchen.hue == 400:
    set_light(kitchen, True, 10000, 128, 232)
  elif kitchen.hue == 10000:
    set_light(kitchen, True, 25000, 128, 232)
  elif kitchen.hue == 25000:
    set_light(kitchen, True, 40000, 128, 232)
  elif kitchen.hue == 40000:
    set_light(kitchen, True, 45000, 128, 232)
  elif kitchen.hue == 4500:
    set_light(kitchen, True, 55000, 128, 232)
  else:
    set_light(kitchen, False)
  return 'OK'

@get('/kitchen_hud')
def kitchen_hud():
  kc = cron.checker.has_key('Kitchen')
  cron.checker['Kitchen'] = 0
  if not kc:
    commander.say('I have turned on the Kitchen light for you Kevin.')
    kitchen = hue.get_light_objects('name')['Kitchen']
    if cron.is_sleeping:
      set_light(kitchen, True, 13088, 144, 213)
    elif cron.is_dark:
      set_light(kitchen, True, 34494, 254, 232)
  return 'OK'

@get('/piper_alert')
def all_bright():
  commander.hud_video()
  #commander.addCommand('play /home/kveroneau/karen.mp3')
  commander.say('Intruder alert! Turning on lights!')
  if cron.is_dark:
    for l in hue.lights:
      set_light(l, True, 34494, 254, 232)
    cron.pause = True
  return 'OK'

@get('/party_mode')
def party_mode():
  cron.pause = True
  for l in hue.lights:
    set_light(l, True, 47417, 233, 241)
  xbmc.party_mode()
  return 'OK'

@get('/xbmc_menu')
def xbmc_menu():
  xbmc.open_menu()
  return 'OK'

@get('/xbmc_playlist')
def xbmc_playlist():
  xbmc.show_playlist()
  return 'OK'

@get('/trance_party')
def trance_party():
  cron.pause = True
  for l in hue.lights:
    set_light(l, True, 47417, 233, 241)
  xbmc.play('http://ca.ah.fm:443')
  return 'OK'

@get('/mpd')
def mpd():
  xbmc.play('http://pi:8000/mpd.ogg')
  return 'OK'

@get('/log')
def hud_log():
  if request.remote_addr in BAN_LIST:
    return 'OK'
  entry = request.query.get('entry', None)
  if len(entry) > 64:
    if request.remote_addr not in BAN_LIST:
      BAN_LIST.append(request.remote_addr)
      commander.hud_log('Buffer overflow from %s, added to BAN_LIST.' % request.remote_addr)
    return 'OK'
  if entry:
    watch_notify(entry)
    commander.hud_log(entry)
  return 'OK'

@get('/hud_alarm')
def hud_alarm():
  commander.hud_alarm()
  return 'OK'

@get('/hud_still')
def hud_still():
  commander.hud_still()
  return 'OK'

@get('/hud_video')
def hud_video():
  commander.hud_video()
  return 'OK'

@post('/voice_action')
def voice_action():
  text = request.body.read()
  if va.parse(text):
    return 'OK'
  commander.say('Kevin, you have not told me how to %s.' % text)
  return 'OK'

@post('/say')
def say():
  text = request.body.read()
  commander.say(text)
  return 'OK'

@get('/shutdown')
def shutdown():
  commander.say('Shutting down commander thread.')
  commander.shutdown()
  cron.cancel()
  return 'OK'

@get('/toggle_rooms')
def toggle_rooms():
  if cron.allow_rooms:
    cron.allow_rooms = False
    commander.say('Multi room disabled.')
  else:
    cron.allow_rooms = True
    commander.say('Multi room enabled.')
  return 'OK'

@get('/debug_info')
def debug_info():
  checker_data = ''
  for light in cron.checker.items():
    checker_data+='%s: %s<br/>' % light
  resp = 'Threads: %s<br/>Paused: %s<br/>Multi room: %s<br/>Off counter: %s<br/>%s' % (threading.active_count(), cron.pause, cron.allow_rooms, cron.offcount, checker_data)
  resp+= 'Is Dark: %s<br/>' % cron.is_dark
  resp+= 'Wake Me: %s<br/>' % cron.wakeme
  resp+= 'Available: %s<br/>' % cron.available
  resp+= 'BAN_LIST: %s<br/>' % ', '.join(BAN_LIST)
  return resp

@get('/toggle_pause')
def toggle_pause():
  if cron.pause:
    cron.pause = False
    commander.say('Light automation resumed.')
    return 'Unpaused'
  else:
    cron.pause = True
    commander.say('Light automation paused.')
    return 'Paused'

@get('/toggle_wakeme')
def toggle_wakeme():
  if cron.wakeme:
    cron.wakeme = False
    commander.say('I will not wake you up in the morning, Kevin.')
    return 'No wake'
  else:
    cron.wakeme = True
    commander.say('Kevin, I will be sure to wake you up in the morning.')
    return 'Wake mode'

@post('/play_vmail')
def play_vmail():
  if not cron.is_sleeping:
    url = request.body.read()
    commander.playurl('Playing voice message.', url)
  return 'OK'

@post('/add_reminder')
def add_reminder():
  typ = request.forms.get('type','home')
  when = request.forms.get('when',None)
  text = request.forms.get('text',None)
  if text is None:
    return 'ERR'
  if typ == 'home':
    home_reminders.append(text)
    commander.say('I will remind you to %s Kevin, when you get home next.' % text)
  elif typ == 'wake':
    wake_reminders.append(text)
    commander.say('I will remind you to %s Kevin, when you wake up in the morning.' % text)
  elif typ == 'timed':
    cron.add_reminder(when, text)
    commander.say('I will remind you to %s at %s Kevin.' % (text, when))
  return 'OK'

@get('/clear_reminders')
def clear_reminders():
  commander.say('Clearing reminders.')
  for x in range(0,len(wake_reminders)):
    wake_reminders.pop()
  for x in range(0,len(home_reminders)):
    home_reminders.pop()
  return 'OK'

@get('/wake_reminders')
def wake_reminder():
  cron.wakeme = True
  commander.terminate()
  send_event('disarm_security','wake_up')
  t = ReminderThread(wake_reminders)
  t.start()
  return 'OK'

@get('/set_timer')
def set_timer():
  length = request.query.get('minutes', 5)
  text = request.query.get('text','Timer triggered!')
  tmr=datetime.datetime.now()+datetime.timedelta(minutes=int(length))
  cron.add_reminder(tmr.strftime('%H:%M'), text)
  commander.say('I will remind you in %s minutes %s.' % (length, text))
  return 'OK'

@get('/forecast')
def forecast():
  commander.say(weather_cache.weather['futurecast'][0]['outlook'])
  return 'OK'

@get('/set_visiting')
def set_visiting():
  cron.visiting = request.query.get('who', None)
  return 'Visiting: %s' % cron.visiting

@get('/login_notify')
def login_notify():
  logname = request.query.get('logname', 'nobody')
  location = request.remote_addr
  if location == '192.168.xxx.xxx':
    location = 'Main computer'
  elif location == '127.0.0.1':
    location = 'Home gateway'
  elif location == '192.241.163.154':
    location = 'Node one'
  elif location == '162.243.43.151':
    location = 'Node two'
  message = '%s just logged into %s' % (logname, location)
  if cron.is_home:
    commander.say(message)
  else:
    c = TwilioRestClient(SID, TOKEN)
    c.messages.create(to='+1587#######', from_='+1587#######', body=message)
  return 'OK'

@get('/config')
def automation_config():
  return {'sleeping':cron.is_sleeping, 'home':cron.is_home, 'dark':cron.is_dark}

@get('/health')
def health_monitor():
  config = {'sleeping':cron.is_sleeping, 'home':cron.is_home, 'dark':cron.is_dark, 'paused':cron.pause, 'rooms':cron.allow_rooms}
  available = {}
  services = list(cron.HEARTBEAT_URLS.keys())
  services.extend(cron.HEARTBEAT_TCP.keys())
  for srv in services:
    if srv in cron.available:
      available[srv] = True
    else:
      available[srv] = False
  return {'weather':weather_cache.weather, 'config':config, 'lights':cron.checker.keys(), 'available':available}

@get('/summon_taxi')
def summon_taxi():
  if cron.is_home:
    if cron.is_sleeping:
      commander.say('I refuse to summon a taxi while you are in sleep mode.')
      return 'NO'
    c = TwilioRestClient(SID, TOKEN)
    c.messages.create(to='+14032999999', from_='+1587#######', body='110 Fake Ave N.W.')
    commander.say('Kevin, I have summoned a taxi here.')
  else:
    commander.say('I have not summoned a taxi, as you are not at home Kevin.')
  return 'OK'

run(host='0.0.0.0')
