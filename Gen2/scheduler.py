from gevent import Greenlet, sleep, spawn, joinall
from pubserver import pub
from automation import cron
from twilio.rest import TwilioRestClient
from ecweather import weather_cache
import datetime, os, random, urllib

SID = '***TWILIO_SID***'
TOKEN = '***TWILIO_TOKEN***'

"""
This source file contained a lot of personal scheduling information about my personal life, so much of it has been removed/replaced.
"""

MORNING_ROUTINE = (
    ('x:05',1,'Have you gotten out of bed yet?'),
    ('x:12',1,'Get out of bed and have some coffee.'),
    # Further entries has been removed, use your imagination here. :)
)

def notify(message):
    if cron.is_home:
        pub.say(message)
    else:
        c = TwilioRestClient(SID, TOKEN)
        c.messages.create(to='+1587#######', from_='+1587#######', body='Time Manager: %s' % message)

def temperature():
    try:
        return float(weather_cache.weather['feels_like'])
    except:
        return None

class TimeManager(Greenlet):
    def __init__(self):
        Greenlet.__init__(self)
        self.__schedule = []
    def __random_list(self, table, count=20):
        result = []
        nl = []
        x=0
        for itm in table:
            nl.extend([x for i in range(0,itm[1])])
            x+=1
        random.shuffle(nl)
        for i in range(0,count):
            result.append(table[random.choice(nl)][0])
        return result
    def append(self, when, action, param):
        now = datetime.datetime.now()
        h,m = when.split(':')
        dt = datetime.datetime(now.year,now.month,now.day,int(h),int(m))
        if h < 2:
            dt=dt+datetime.timedelta(days=1)
        tgr = True if dt < now else False
        if action == 1:
            self.__schedule.append({'dt':dt,'triggered':tgr,'notify':param})
        elif action == 2:
            self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Begin project work for %s.' % param})
        elif action == 3:
            self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Time to relax and %s' % param})
        elif action == 99:
            self.__schedule.append({'dt':dt,'triggered':tgr,'special':param})
    def __morning_routine(self, leisure):
        now = datetime.datetime.now()
        for itm in MORNING_ROUTINE:
            h,m = itm[0].split(':')
            dt = datetime.datetime(now.year,now.month,now.day,int(h),int(m))
            if dt < now:
                tgr = True
            else:
                tgr = False
            if itm[1] == 1:
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':itm[2]})
            elif itm[1] == 3:
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'play some %s while you wake up.' % leisure})
    def __generate_schedule(self):
        projects, leisure = [],[]
        f = open('tables','rb')
        for i in range(0,ord(f.read(1))):
            projects.append((f.read(ord(f.read(1))), ord(f.read(1))))
        for i in range(0,ord(f.read(1))):
            leisure.append((f.read(ord(f.read(1))), ord(f.read(1))))
        f.close()
        rprojects = self.__random_list(projects)
        rleisure = self.__random_list(leisure)
        now = datetime.datetime.now()
        wd = now.isoweekday()
        dt = datetime.datetime(now.year,now.month,now.day,5,0)
        out_late = cron.out_late
        cron.out_late = False
        if cron.oncall and wd == 3:
            cron.oncall = False
            pub.hud_log('On-call mode has been turned off.')
        self.__schedule = [{'dt':dt+datetime.timedelta(days=1), 'triggered':False,'special':'update_schedule'}]
        if wd < 6:
            if wd in (2,5,):
                # All times have been x'd out.  Format: 05:00/24-hour
                self.append('x:xx',99,'wakeup')
                self.append('x:xx',99,'forecast')
                self.append('x:xx',1,'You need to jump into the shower this morning Kevin')
                self.append('x:xx',1,'Have a quick glass of your boost shake')
                self.append('x:xx',99,'toggle_pause')
                self.append('x:xx',99,'shower')
                self.append('x:xx',1,'Have a cup of coffee')
            else:
                self.append('x:xx',1,'Have you gotten out of bed yet?')
                self.append('x:xx',99,'forecast')
                self.append('x:xx',1,'Have a glass of your boost shake and wake up')
                self.append('x:xx',1,'Have a cup of coffee while getting ready to go')
            self.append('x:xx',99,'weather_check')
            self.append('x:xx',99,'entrance')
            self.append('x:xx',1,'You should leave for the train immediately')
            if wd == 2:
                self.append('xx:xx',1,'It is taco Tueday today at Bookers')
                self.append('xx:xx',1,'Are you still at Bookers?')
                self.append('xx:00',1,'Make your lunch for work tomorrow')
                self.append('xx:30',3,'play some %s' % rleisure.pop())
            elif wd == 5:
                self.append('xx:xx',3,'watch the latest episode of the Orville.')
                self.append('xx:xx',3,'watch the latest episode of Discovery.')
                self.append('xx:xx',3,'play some %s' % rleisure.pop())
            else:
                self.append('xx:xx',3,'play some %s' % rleisure.pop())
                self.append('xx:xx',1,'Are you hungry Kevin?')
                self.append('xx:xx',2,rprojects.pop())
                self.append('xx:xx',1,'Make your lunch for work tomorrow')
        elif wd in (6,7,):
            if cron.oncall or not out_late:
                self.append('x:xx',1,'Have you gotten out of bed yet?')
                self.append('x:xx',1,'Have a glass of your boost shake and wake up')
                self.append('xx:xx',1,'Have a cup of coffee and relax')
                self.append('xx:xx',2,rprojects.pop())
            else:
                cron.wakeme = False
                self.append('x:xx',1,'I see you got home late last night, sleep in')
                self.append('xx:xx',99,'wakeup')
                self.append('xx:xx',1,'Have a glass of your boost shake and wake up')
                self.append('xx:xx',1,'Have a cup of coffee and relax')
            self.append('12:00',1,'It is now lunch time')
            self.append('xx:00',3,'play some %s' % rleisure.pop())
            self.append('xx:00',2,rprojects.pop())
            self.append('xx:00',3,'watch some TV while maybe eating something')
            self.append('xx:00',2,rprojects.pop())
            self.append('xx:15',3,'play some %s' % rleisure.pop())
            if wd == 6:
                if not out_late:
                    # Didn't go out Friday night?  Lets see if I should go out Saturday!
                    self.append('xx:30',99,'saturday_check')
            else:
                self.append('xx:30',1,'Make your lunch for work tomorrow')
        return
        # Below here is some older unused code which completely randomized my day or evening schedule. ;)
        self.__morning_routine(rleisure.pop())
        idx = 2
        for hm in ('17:30','18:30','19:30','21:15',):
            h,m = hm.split(':')
            dt = datetime.datetime(now.year,now.month,now.day,int(h),int(m))
            if dt < now:
                tgr = True
            else:
                tgr = False
            if idx == 3:
                idx = 2
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Time to relax and play some %s.' % rleisure.pop()})
            elif idx == 2:
                idx = 3
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Begin project work for %s.' % rprojects.pop()})
    def __update_schedule(self):
        now = datetime.datetime.now()
        dt = datetime.datetime(now.year,now.month,now.day,2,0)
        self.__schedule = [{'dt':dt+datetime.timedelta(days=1), 'triggered':False,'special':'update_schedule'}]
        wd = now.strftime('%A')
        if not os.path.exists('schedule/%s.dat' % wd):
            self.__generate_schedule()
            #pub.hud_log('Unable to find schedule for %s!' % wd)
            return
        data = open('schedule/%s.dat' % wd,'r').read().split('\n')
        pub.hud_log('Found %s scheduled items for today!' % len(data))
        for itm in data:
            sch = itm.split(',')
            h,m = sch[0].split(':')
            dt = datetime.datetime(now.year,now.month,now.day,int(h),int(m))
            if dt < now:
                tgr = True
            else:
                tgr = False
            if sch[1] == '1':
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':sch[2]})
            elif sch[1] == '2':
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Begin project work for %s' % sch[2]})
            elif sch[1] == '3':
                self.__schedule.append({'dt':dt,'triggered':tgr,'notify':'Time to relax and %s' % sch[2]})
            elif sch[1] == '99':
                self.__schedule.append({'dt':dt,'triggered':tgr,'special':sch[2]})
    def get_schedule(self):
        return list(self.__schedule)
    def reload_schedule(self):
        try:
            self.__update_schedule()
        except:
            pub.say('An exception occurred while attempting to update your schedule.')
            self.__generate_schedule()
    def _run(self):
        try:
            #self.__generate_schedule()
            self.__update_schedule()
        except:
            pub.say('An exception occurred while starting time manager.')
            return
        sch_upd = False
        while True:
            sleep(60)
            dt = datetime.datetime.now()
            if len(self.__schedule) > 0:
                for itm in self.__schedule:
                    if not itm['triggered']:
                        if itm['dt'] < dt:
                            if itm.has_key('special'):
                                if itm['special'] == 'update_schedule':
                                    sch_upd = True
                                elif itm['special'] == 'wakeup':
                                    itm['triggered'] = True
                                    if cron.is_home and cron.is_sleeping:
                                        pub.sayAndCmd('Wake up Kevin, it is time to keep your schedule.', 'play /home/kveroneau/alarm.mp3')
                                        if cron.is_dark:
                                            urllib.urlopen('http://localhost:8080/allbright')
                                elif itm['special'] == 'shower':
                                    itm['triggered'] = True
                                    urllib.urlopen('http://localhost:8080/bathroom')
                                elif itm['special'] == 'toggle_wakeme':
                                    itm['triggered'] = True
                                    urllib.urlopen('http://localhost:8080/toggle_wakeme')
                                elif itm['special'] == 'toggle_pause':
                                    itm['triggered'] = True
                                    urllib.urlopen('http://localhost:8080/toggle_pause')
                                elif itm['special'] == 'forecast':
                                    itm['triggered'] = True
                                    urllib.urlopen('http://localhost:8080/forecast')
                                elif itm['special'] == 'entrance':
                                    itm['triggered'] = True
                                    urllib.urlopen('http://localhost:8080/entrance')
                                elif itm['special'] == 'weather_check':
                                    itm['triggered'] = True
                                    temp = temperature()
                                    if temp > 18:
                                        notify('It is really nice out there today, you just need maybe a hoody.')
                                    elif temp > 10:
                                        notify('It is going to be a bit cool, wear your light leather jacket today.')
                                    elif temp > 0:
                                        notify('It will be pretty chilly out today, wear your light leather jacket.')
                                    elif temp > -10:
                                        notify('It is below freezing outside, wear your leather jacket with attached hoody.')
                                    elif temp > -20:
                                        notify('It is not freezing out, but you should dress warm.')
                                    else:
                                        notify('Today is a day for many layers, bundle up it is %s outside.' % temp)
                                elif itm['special'] == 'friday_check':
                                    itm['triggered'] = True
                                    temp = temperature()
                                    if temp > -20:
                                        notify('The weather currently feels like %s outside.' % temp)
                                        self.append('16:35',1,'You could go to Brewsters this evening for dinner')
                                    else:
                                        notify('It is too cold out to do anything, it feels like %s outside.' % temp)
                                        self.append('xx:30',3,'play some %s' % rleisure.pop())
                                        self.append('xx:00',1,'Are you hungry Kevin?')
                                        self.append('xx:00',2,rprojects.pop())
                                elif itm['special'] == 'saturday_check':
                                    itm['triggered'] = True
                                    temp = temperature()
                                    if temp > -20:
                                        notify('The weather currently feels like %s outside.' % temp)
                                        self.append('19:35',1,'You could go to Greta bar this evening for enjoyment')
                                    else:
                                        notify('It is too cold out to do anything, it feels like %s outside.' % temp)
                                        self.append('xx:00',3,'have a smoke')
                                        self.append('xx:05',99,'lockdown')
                                        self.append('xx:30',3,'watch some videos online')                                    
                            elif itm.has_key('notify'):
                                notify(itm['notify'])
                                itm['triggered'] = True
            if sch_upd:
                sch_upd = False
                try:
                    self.__update_schedule()
                except:
                    pub.say('An exception occurred while attempting to update your schedule.')
                    self.__generate_schedule()

tm = TimeManager()
