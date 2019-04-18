from gevent import Greenlet, sleep, spawn, joinall
from phue import Bridge, PhueRequestTimeout
from pubserver import pub
from sunrise_sunset import SunriseSunset, CIVIL_ZENITH
from utils import send_event
import json, urllib, socket, datetime, sys

class Scheduler(Greenlet):
    AUTO_OFF = {
        'always': ('Brazier', 'Living room 2', 'Iris', 'Candle',),
        'Study': ('Living room', 'Bedroom',),
        'Living room': ('Study', 'Bedroom',),
        'Bedroom': ('Study', 'Living room',),
    }
    ALWAYS_ON = ()
    TIME = 5
    HEARTBEAT_URLS = {
        "Hacker's Edge website": 'http://www.hackers-edge.com/',
        'Python Diary': 'http://www.pythondiary.com/',
        'Veroneau dot net': 'https://www.veroneau.net/',
        'home proxy': 'https://www.veroneau.net/home/',
        'hue lights': 'http://hue/',
        'Canon printer': 'http://printer/',
    }
    HEARTBEAT_TCP = {
        'sys1': ('sys1',22),
        'sys2': ('sys2',22),
        'sys3': ('sys3',22),
        'bedroom hud': ('alarm',22),
        'living room hud': ('livingroom',22),
        'automation hud': ('hud',22),
        'node1': ('node1.veroneau.net',22),
        'cloud1': ('cloud01.veroneau.net',22),
        "Hacker's Edge server": ('hackers-edge.com',1337),
        'cody': ('sonytv', 9090),
        'video database': ('127.0.0.1',7070),
    }
    def __init__(self):
        Greenlet.__init__(self)
        self.checker = {}
        self.allow_rooms = False
        self.__pause = False
        self.__config = {}
        self.__lostnet = None
        self.__tenit = False
        self.__sixit = False
        self.__wakecount = 0
        self.__reminders = {}
        self.visiting = None
        self.__offcount = 0
        self.__sunrise = None
        self.__sunset = None
        self.__darkmode = False
        self.wakeme = True
        self.oncall = False
        self.out_late = False
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
    def add_reminder(self, when, message):
        self.__reminders[when] = message
    def __get_config(self):
        try:
            cfg = json.loads(urllib.urlopen('https://www.veroneau.net/***URL_MASKED***?calls=no').read())
            self.__config = cfg
        except:
            if not self.is_sleeping:
                pub.say('I have detected a problem with your Internet Kevin.')
            self.__lostnet = datetime.datetime.now()
            pub.hud_log('Internet outage at %s' % self.__lostnet.strftime('%H:%M'))
    def __update_sun(self):
        now = datetime.datetime.now()
        if self.__sunrise:
            if now.hour == 2 and self.__sunrise.day != now.day:
                self.__sunrise = None
        if self.__sunrise == None:
            pub.hud_log('Updating sunset and sunrise...')
            r = SunriseSunset(now, 51.0486, -114.0708, -6, CIVIL_ZENITH)
            self.__sunrise, self.__sunset = r.calculate()
            del r
            pub.hud_log('Sunrise: %s, Sunset: %s' % (self.__sunrise, self.__sunset))
    def __schedule(self):
        if not self.is_home:
            self.__tenit = False
            self.__sixit = False
            self.__darkmode = False
            return
        try:
            if self.is_dark and not self.__darkmode:
                if not self.is_sleeping:
                    pub.hud_log('Setting dark mode.')
                    pub.say('It is after sunset Kevin, setting dark mode.')
                    urllib.urlopen('http://localhost:8080/strips')
                    self.__darkmode = True
            elif self.__darkmode and not self.is_dark:
                if not self.is_sleeping:
                    pub.say('It is now sunrise Kevin, turning off lights.')
                pub.hud_log('Setting day mode.')
                for l in self.__hue.lights:
                    if l.name in self.ALWAYS_ON:
                        continue
                    l.on = False
                self.__darkmode = False
            if not self.wakeme:
                return
            hr = datetime.datetime.now().hour
            wd = datetime.datetime.now().isoweekday()
            now = datetime.datetime.now().strftime('%H:%M')
            if now in self.__reminders.keys():
                pub.say(self.__reminders[now])
                del self.__reminders[now]
            RELAX_HR, WAKEUP_HR, WORK_HR = 21,7,8
            #if wd == 3:
            #    WAKEUP_HR, WORK_HR = 7,8
            if wd == 7 or wd == 6:
                WAKEUP_HR = 9
            if not self.__tenit and hr == RELAX_HR:
                if self.is_sleeping:
                    return
                self.__tenit = True
                urllib.urlopen('https://www.veroneau.net/***URL_MASKED***')
                #urllib.urlopen('https://www.veroneau.net/***URL_MASKED***')
                rhr = RELAX_HR-12
                if self.visiting:
                    pub.say('%s, it is now after %s.  Are you planning on sleeping over tonight?' % (self.visiting, rhr))
                else:
                    pub.say('Kevin, it is now after %s.  I have enabled sleep mode for you.' % rhr)
                if self.is_dark:
                    urllib.urlopen('http://localhost:8080/relax')
            elif self.__tenit and hr == RELAX_HR+1:
                self.__tenit = False
                if len(self.checker.keys()) == 0:
                    return
                rhr = RELAX_HR-12+1
                pub.say('It is now after %s.  It is time for you to get to bed Kevin.' % rhr)
                if self.is_dark:
                    if 'Bedroom' not in self.checker.keys():
                        urllib.urlopen('http://localhost:8080/bedroom')
            elif self.__sixit and hr == 6:
                self.__sixit = True
                send_event('disarm_security','jason_wakeup')
                pub.say('I have disarmed the security system for you Jason.')
                urllib.urlopen('http://localhost:8080/jason')
                urllib.urlopen('http://localhost:8080/living')
                urllib.urlopen('http://localhost:8080/strips')
            elif self.is_sleeping and hr == WAKEUP_HR:
                if self.__wakecount == 0:
                    pub.say('Wake up Kevin, it is time to keep your schedule.')
                    #c = TwilioRestClient(SID, TOKEN)
                    #call = c.calls.create(url='https://www.veroneau.net/**URL_MASKED**', to='+1587#######', from_='+1587#######')
                    if self.is_dark:
                        urllib.urlopen('http://localhost:8080/allbright')
                        self.checker['Bedroom'] = 0
                elif self.__wakecount == 5:
                    if 'Bedroom' not in self.checker.keys():
                        if self.is_dark:
                            urllib.urlopen('http://localhost:8080/allbright')
                            self.checker['Bedroom'] = 0
                    pub.sayAndCmd('Did you seriously fall back asleep Kevin!', 'play /home/kveroneau/alarm.mp3')
                elif self.__wakecount > 0 and 'Bedroom' not in self.checker.keys():
                    pub.say('Kevin!  Do I need to tell you %s times to get your butt out of that bed!' % self.__wakecount)
                    if self.is_dark:
                        urllib.urlopen('http://localhost:8080/allbright')
                        self.checker['Bedroom'] = 0
                self.__wakecount+=1
            elif hr == WAKEUP_HR and self.__wakecount > 0:
                pub.say('Finally your up, it took me %s times to yell at you.' % self.__wakecount)
                self.__wakecount = 0
                if self.__lostnet is not None:
                    pub.say('I have some important news for you Kevin.  I lost connection to the Internet at %s while you were sleeping.' % self.lostnet)
            elif wd > 0 and wd < 6:
                if not self.__tenit and hr == WORK_HR:
                    self.__tenit = True
                    pub.say('Kevin, should you not be leaving for work soon?')
            elif self.__tenit and hr == 11:
                self.__tenit = False
        except:
            pub.say('Kevin, there was an exception in your new schedule code.')
            open('/tmp/exc.log','w').write('[%s] %s' % (sys.exc_info()[0], sys.exc_info()[1]))
    @property
    def available(self):
        return ', '.join(self.__available)
    def __test_http(self, name, url):
        try:
            if urllib.urlopen(url).code != 200:
                raise
            if name not in self.__available:
                self.__available.append(name)
                pub.say('Kevin, %s is now online.' % name)
                pub.hud_log('%s is now online.' % name)
        except:
            if name in self.__available:
                if self.is_home:
                    pub.say('Kevin, I am currently unable to contact %s.' % name)
                else:
                    send_event('phone_notify','Unable to contact %s.' % name)
                pub.hud_log('Unable to contact %s.' % name)
                self.__available.remove(name)
    def __test_tcp(self, name, addr):
        try:
            s = socket.socket()
            s.connect(addr)
            if addr[1] == 1337:
                sleep(1)
                buf = s.recv(1024)
                if 'hackersedge login:' not in buf:
                    open('/tmp/hebuf','w').write(buf)
                    raise
            s.close()
            del s
            if name not in self.__available:
                self.__available.append(name)
                pub.say('Kevin, %s is now online.' % name)
                pub.hud_log('%s is now online.' % name)
        except:
            s.close()
            del s
            if name in self.__available:
                if self.is_home:
                    pub.say('Kevin, I am currently unable to contact %s.' % name)
                else:
                    send_event('phone_notify','Unable to contact %s.' % name)
                pub.hud_log('Unable to contact %s.' % name)
                self.__available.remove(name)
    def __heartbeat(self):
        chklst = []
        for name, url in self.HEARTBEAT_URLS.items():
            chklst.append(spawn(self.__test_http, name, url))
        for name, addr in self.HEARTBEAT_TCP.items():
            chklst.append(spawn(self.__test_tcp, name, addr))
        joinall(chklst)
    @property
    def is_home(self):
        return self.__config['HOME']
    @property
    def is_sleeping(self):
        return self.__config['SLEEPING']
    @property
    def is_dark(self):
        now = datetime.datetime.now()
        if now < self.__sunrise or now > self.__sunset:
            return True
        return False
    @property
    def offcount(self):
        return self.__offcount
    @property
    def lostnet(self):
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
                pub.say('Kevin, I noticed you left %s on.  I am turning %s off for you now.' % (lgts, ctx))
            else:
                pub.say('Kevin, I noticed you have the %s light on.  I will turn off %s for you now.' % (main_light, lgts))
            return
            if 'Entrance' in lights_out:
                if not self.is_home:
                    send_event('arm_away','entrance')
            elif 'Backdoor' in lights_out:
                if not self.is_home:
                    send_event('arm_away','backdoor')
    def _run(self):
        self.__hue = Bridge('hue')
        self.__get_config()
        while True:
            sleep(60)
            self.__get_config()
            self.__update_sun()
            hb = spawn(self.__heartbeat)
            schd = spawn(self.__schedule)
            try:
                if not self.pause:
                    self.real_run()
                else:
                    if not self.is_home:
                        self.__offcount = 0
                        pub.say('Kevin, I can see that the lights are paused and you are not home.  I am enabling light automation now.')
                        self.pause = False
                    else:
                        self.__offcount+=1
                        if self.__offcount > 60:
                            pub.say('Light automation has been disabled for over an hour now Kevin.  I am enabling light automation now.')
                            self.__offcount = 0
                            self.pause = False
                            self.real_run()
                        elif self.__offcount == 50:
                            pub.say('I see that light automation has been off for almost an hour now Kevin.')
            except PhueRequestTimeout:
                pass # Most likely a networking issue, we can ignore and try again in a minute.
            except socket.error:
                if not self.is_sleeping:
                    pub.say('I am having trouble talking with your lights.')
            except:
                pub.say('An exception has occurred in schedule thread.')
                open('/tmp/exc.log','w').write('[%s] %s' % (sys.exc_info()[0], sys.exc_info()[1]))
            joinall([hb, schd])

def say_reminders(reminder_list):
    if len(reminder_list) == 1:
        word = 'reminder'
    else:
        word = 'reminders'
    pub.say('You have %s %s Kevin.' % (len(reminder_list), word))
    for reminder in reminder_list:
        pub.say('%s.' % reminder)
    lostnet = cron.lostnet
    if lostnet is not None:
        pub.say('I also lost my Internet connection at %s.' % lostnet)

cron = Scheduler()
