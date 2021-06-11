from bottle import view, get, request, post, default_app
from pubserver import pub
from utils import send_event, watch_notify
from huelib import set_light, loadtheme
from automation import cron, say_reminders
from gevent import spawn, spawn_later, idle
from ecweather import weather_cache, WeatherException
from phue import Bridge, PhueRequestTimeout
from twilio.rest import TwilioRestClient
from scheduler import tm
import urllib, datetime, shelve, xbmclib, os.path

# Comment to test beacons are working again.

LOCATIONS = {
    '192.168.xxx.xxx':'Old computer',
    '192.168.xxx.xxx':'Main computer',
    '127.0.0.1':'Automation server',
    '192.241.163.154':'Node one',
    '159.89.126.75':'Cloud one',
    '192.168.xxx.xxx':'Home gateway',
}

hue = Bridge('hue')

xbmc = xbmclib.XBMC()

SID = '***TWILIO_SID***'
TOKEN = '***TWILIO_TOKEN***'

BAN_LIST = []

home_reminders = []
wake_reminders = []

default_app.push()

@get('/')
def hello():
    if request.remote_addr not in BAN_LIST:
        BAN_LIST.append(request.remote_addr)
        pub.hud_log('IP Added to BAN_LIST: %s' % request.remote_addr)
    return 'Hello'

@get('/hoff')
def lights_off():
    cron.pause = False
    pub.hud(1,'OFF')
    pub.hud(2,'OFF')
    if cron.is_sleeping:
        pub.say('Good night Kevin.')
    else:
        pub.say('Good bye Kevin.')
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
    h = int(h)
    for l in hue.lights:
        if l.on:
            l.hue = h
    return 'OK'

@get('/brightness')
def set_brightness():
    b = request.query.get('value',None)
    if b is None:
        return '?'
    b = int(b)
    for l in hue.lights:
        if l.on:
            l.brightness = int(b*255/100.0)
    return 'OK'

@get('/home')
def coming_home():
    if cron.is_home:
        return 'NO'
    inside = False if request.query.get('inside',None) is None else True
    if inside:
        pub.hud(2, 'ON')
        spawn_later(60, say_reminders, home_reminders)
        tm.evening_schedule()
        #t = ReminderThread(home_reminders)
        #t.start()
    else:
        send_event('disarm_security','coming_home')
    hr = datetime.datetime.now().hour
    if hr > 7:
        if cron.is_dark:
            pub.say('Welcome home Kevin, activating lights.')
            loadtheme('ComingHome')
        else:
            pub.say('Welcome home Kevin!')
    else:
        cron.out_late = True
        pub.say('Setting romantic theme for your guests Kevin.')
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
    pub.say('I have saved the light theme Kevin.')
    return 'SAVED'

@get('/allbright')
def all_bright():
    inc = request.query.get('inc',None)
    if inc == 'Jay':
        lgt = 'xxx'
    else:
        lgt = 'Jason'
    cron.pause = True
    pub.hud(1, 'ON')
    pub.hud(2, 'ON')
    pub.hud(1, 'BRIGHT')
    pub.hud(2, 'BRIGHT')
    pub.say('Setting maximum light brightness Kevin.')
    for l in hue.lights:
        if l.name != lgt:
            set_light(l, True, 34494, 254, 232)
    return 'OK'

@get('/jason')
def jason_light():
    lights = hue.get_light_objects('name')
    set_light(lights['Jason'], True, 34494, 254, 232)
    return 'OK'

@get('/bath_alert')
def bath_alert():
    who = request.query.get('who',None)
    if who != 'Set':
        cron.visiting = who
    lights = hue.get_light_objects('name')
    set_light(lights['Candle'], True, 14910, 254)
    set_light(lights['Study'], True, 34494, 232)
    if who and cron.is_home:
        #cron.pause = True
        if who == 'Set':
            pub.say('Kevin, %s is currently buzzing up.' % cron.visiting)
        else:
            pub.say('Kevin, %s has just been let in the front door downstairs.' % who)
        send_event('disarm_security','bath_alert')
    return 'OK'

@get('/on_call')
def on_call():
    cron.pause = True
    pub.hud(1, 'ON')
    lights = hue.get_light_objects('name')
    for lgt in ('Bedroom', 'LightStrips', 'Study',):
        set_light(lights[lgt], True, 34494, 232)
    pub.sayAndCmd('Kevin, you are currently receiving a call from work!', 'play /home/***/alarm.mp3')
    send_event('on_call', 'On call event')
    return 'OK'

@get('/stop_alarm')
def stop_alarm():
    pub.hud(1, 'TERM')
    pub.hud(2, 'TERM')
    return 'OK'

@get('/relax')
def relax():
    pub.say('Setting relax mode.')
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
        pub.hud(1, 'OFF')
        pub.hud(2, 'ON')
        if cron.allow_rooms:
            lights['Bedroom'].on = False
            return 'OK'
        if cron.is_dark:
            lgts = ('Living room', 'LightStrips',)
        else:
            lgts = ()
    elif b == 32:
        if cron.visiting:
            pub.say("%s are you planning on staying the night?  It is now Kevin's bed time." % cron.visiting)
        else:
            pub.say('Are you ready to go to bed now Kevin?')
        pub.hud(2, 'OFF')
        pub.hud(1, 'ON')
        open('/run/saltctl','w').write('fw|ipset')
        lgts = ('Bedroom', 'LightStrips',)
    else:
        pub.hud(2, 'OFF')
        pub.hud(1, 'ON')
        if cron.is_dark:
            lgts = ('Bedroom', 'LightStrips',)
        else:
            lgts = ('Bedroom',)
    for l in hue.lights:
        if l.name in lgts:
            set_light(l, True, h, b, s)
        elif l.on:
            if not cron.allow_rooms:
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
    set_light(lights['Candle'], True, 34494, 254, 232)
    if cron.is_dark:
        set_light(lights['Bedroom'], True, 34494, 254, 232)

@get('/bathroom')
def bathroom():
    spawn(ivr_shower, True)
    lights = hue.get_light_objects('name')
    cron.pause = True
    hr=datetime.datetime.now().hour
    if hr<6:
        pub.say('Preparing a romantic shower.')
        b, s, h = 35, 253, 400
    else:
        pub.say('Have a good shower Kevin.')
        spawn_later(60*15, shower_timer)
        b, s, h = 233, 241, 47417
    for l in hue.lights:
        if l.name == 'Candle':
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
        pub.hud(2, 'ON')
        pub.say('I have turned off the study light Kevin.')
        if cron.allow_rooms:
            lights['Study'].on = False
            return 'OK'
        if cron.is_dark:
            lgts = ('Brazier', 'Living room', 'LightStrips')
        else:
            lgts = ()
    else:
        pub.hud(1, 'OFF')
        pub.hud(2, 'OFF')
        pub.say('Study light has been turned on.')
        lgts = ('Study',)
    for l in hue.lights:
        if l.name in lgts:
            set_light(l, True, 34494, 254, 232)
        elif l.on:
            if not cron.allow_rooms:
                set_light(l, False)
    return 'OK'

@get('/entrance')
def enterance():
    if not cron.is_home:
        return 'NO'
    cron.pause = False
    lights = hue.get_light_objects('name')
    if lights['Study'].on:
        if cron.visiting:
            pub.say('Hello %s, how are you doing today?' % cron.visiting)
        else:
            pub.say('I have turned off the light Kevin.')
        set_light(lights['Study'], False)
        #send_event('arm_security','enterance')
    else:
        try:
            if cron.visiting:
                pub.say('Are you going somewhere %s?  Keep warm outside, it is currently %s celsius.' % (cron.visiting, weather_cache.weather['feels_like']))
            else:
                pub.say('Kevin, here is the current forecast.  %s' % weather_cache.weather['futurecast'][0]['outlook'])
        except:
            if cron.visiting:
                pub.say('Good bye %s.' % cron.visiting)
            else:
                pub.say('Are you going somewhere Kevin?')
        set_light(lights['Study'], True, 34494, 254, 232)
        if cron.is_dark:
            set_light(lights['Brazier'], True, 34494, 128, 232)
        send_event('disarm_security','entrance')
        cron.visiting = None
    return 'OK'  

@get('/arm_away')
def arm_away():
    spawn(ivr_shower, False)
    spawn_later(30, send_event, 'arm_away', 'leaving')
    #send_event('arm_away','leaving')
    return 'OK'

@get('/arm_stay')
def arm_stay():
    send_event('arm_security','nfc_tap')
    return 'OK'

@get('/kitchen')
def kitchen():
    kitchen = hue.get_light_objects('name')['LightStrips']
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

@get('/bedroom_hud')
def kitchen_hud():
    kc = cron.checker.has_key('Bedroom')
    cron.checker['Bedroom'] = 0
    if not kc:
        pub.say('I have turned on the Bedroom light for you Kevin.')
        kitchen = hue.get_light_objects('name')['Bedroom']
        if cron.is_sleeping:
            set_light(kitchen, True, 13088, 144, 213)
        elif cron.is_dark:
            set_light(kitchen, True, 34494, 254, 232)
    return 'OK'

@get('/piper_alert')
def all_bright():
    #commander.hud_video()
    #commander.addCommand('play /home/kveroneau/karen.mp3')
    pub.say('Intruder alert!')
    if cron.is_dark:
        pub.say('Turning on lights!')
        for l in hue.lights:
            set_light(l, True, 34494, 254, 232)
        cron.pause = True
    return 'OK'

@get('/log')
def hud_log():
    if request.remote_addr in BAN_LIST:
        return 'OK'
    entry = request.query.get('entry', None)
    if len(entry) > 64:
        if request.remote_addr not in BAN_LIST:
            BAN_LIST.append(request.remote_addr)
            pub.hud_log('Buffer overflow from %s, added to BAN_LIST.' % request.remote_addr)
        return 'OK'
    if entry:
        watch_notify(entry)
        pub.hud_log(entry)
    return 'OK'

@post('/say')
def say():
    text = request.body.read()
    pub.say(text)
    return 'OK'

@get('/shutdown')
def shutdown():
    pub.say('Shutting down commander thread.')
    idle()
    pub.kill()
    cron.kill()
    return 'OK'

@get('/toggle_rooms')
def toggle_rooms():
    if cron.allow_rooms:
        cron.allow_rooms = False
        pub.say('Multi room disabled.')
    else:
        cron.allow_rooms = True
        pub.say('Multi room enabled.')
    return 'OK'

@get('/debug_info')
def debug_info():
    checker_data = ''
    for light in cron.checker.items():
        checker_data+='%s: %s<br/>' % light
    resp = 'PubServer: %s<br/>Scheduler: %s<br/>' % (bool(pub), bool(cron))
    resp+= 'Paused: %s<br/>Multi room: %s<br/>Off counter: %s<br/>%s' % (cron.pause, cron.allow_rooms, cron.offcount, checker_data)
    resp+= 'Is Dark: %s<br/>' % cron.is_dark
    resp+= 'Wake Me: %s<br/>' % cron.wakeme
    resp+= 'Available: %s<br/>' % cron.available
    resp+= 'BAN_LIST: %s<br/>' % ', '.join(BAN_LIST)
    return resp

@get('/toggle_pause')
def toggle_pause():
    if cron.pause:
        cron.pause = False
        pub.say('Light automation resumed.')
        return 'Unpaused'
    else:
        cron.pause = True
        pub.say('Light automation paused.')
        return 'Paused'

@get('/toggle_wakeme')
def toggle_wakeme():
    if cron.wakeme:
        cron.wakeme = False
        pub.say('I will not wake you up in the morning, Kevin.')
        return 'No wake'
    else:
        cron.wakeme = True
        pub.say('Kevin, I will be sure to wake you up in the morning.')
        return 'Wake mode'

@post('/play_vmail')
def play_vmail():
    if not cron.is_sleeping:
        url = request.body.read()
        pub.playurl('Playing voice message.', url)
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
        pub.say('I will remind you to %s Kevin, when you get home next.' % text)
    elif typ == 'wake':
        wake_reminders.append(text)
        pub.say('I will remind you to %s Kevin, when you wake up in the morning.' % text)
    elif typ == 'timed':
        cron.add_reminder(when, text)
        pub.say('I will remind you to %s at %s Kevin.' % (text, when))
    return 'OK'

@get('/clear_reminders')
def clear_reminders():
    pub.say('Clearing reminders.')
    for x in range(0,len(wake_reminders)):
        wake_reminders.pop()
    for x in range(0,len(home_reminders)):
        home_reminders.pop()
    return 'OK'

@get('/wake_reminders')
def wake_reminder():
    cron.wakeme = True
    pub.hud(1, 'TERM')
    pub.hud(2, 'TERM')
    send_event('disarm_security','wake_up')
    spawn_later(60, say_reminders, wake_reminders)
    return 'OK'

def timed_reminder(text):
    pub.say(text)

@post('/set_timer')
def set_timer():
    length = int(request.forms.get('minutes', 5))
    text = request.forms.get('text','Timer triggered!')
    spawn_later(60*length, timed_reminder, text)
    pub.say('I will remind you in %s minutes %s.' % (length, text))
    return 'OK'

@get('/forecast')
def forecast():
    pub.say(weather_cache.weather['futurecast'][0]['outlook'])
    return 'OK'

@get('/set_visiting')
def set_visiting():
    cron.visiting = request.query.get('who', None)
    return 'Visiting: %s' % cron.visiting

@get('/login_notify')
def login_notify():
    logname = request.query.get('logname', 'nobody')
    location = request.remote_addr
    if LOCATIONS.has_key(location):
        location = LOCATIONS[location]
    message = '%s just logged into %s' % (logname, location)
    if cron.is_home:
        pub.say(message)
    else:
        #c = TwilioRestClient(SID, TOKEN)
        #c.messages.create(to='+1587#######', from_='+1587#######', body=message)
        send_event('login_notify', message)
    return 'OK'

@get('/config')
def automation_config():
    return {'sleeping':cron.is_sleeping, 'home':cron.is_home, 'dark':cron.is_dark}

@get('/health')
def health_monitor():
    config = {'sleeping':cron.is_sleeping, 'home':cron.is_home, 'dark':cron.is_dark, 'paused':cron.pause, 'rooms':cron.allow_rooms,
    'wakeme':cron.wakeme}
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
            pub.say('I refuse to summon a taxi while you are in sleep mode.')
            return 'NO'
        c = TwilioRestClient(SID, TOKEN)
        c.messages.create(to='+14032999999', from_='+1587#######', body='123 Fake Rd SW')
        pub.say('Kevin, I have summoned a taxi here.')
    else:
        pub.say('I have not summoned a taxi, as you are not at home Kevin.')
        return 'NOT_HOME'
    return 'OK'

@get('/party_mode')
def party_mode():
    cron.pause = True
    for l in hue.lights:
        set_light(l, True, 47417, 233, 241)
    if xbmc.party_mode() == False:
        return 'ERR'
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
    x = xbmclib.XBMC()
    x.play('http://pi:8000/mpd.ogg')
    return 'OK'

@get('/play_video')
def play_video():
    song = request.query.get('song', None)
    if song is None:
        return 'ERR'
    if xbmc.play_video(song):
        return 'OK'
    return 'ERR'

@get('/ign_videos')
def ign_videos():
    xbmc.activateWindow('plugin://plugin.video.ign_com/?mode=list_series_episodes&url=%2fwatch%2fdaily-fix%3fcategory%3dvideos%26page%3d1')
    return 'OK'

@get('/tm')
def tm_schedule():
    r = request.query.get('reload',None)
    nd = request.query.get('append',None)
    if r == 'yes':
        tm.reload_schedule()
        pub.say('I have reloaded your schedule for you Kevin.')
    elif nd is not None:
        when,action,param = nd.split(',')
        tm.append(when,int(action),param)
        pub.say('I have appended the item to your schedule Kevin.')
    sch = tm.get_schedule()
    data = ''
    for itm in sch:
        data+='<%s> ' % itm['dt']
        if itm.has_key('special'):
            data+='%s<br/>' % itm['special']
        elif itm.has_key('notify'):
            data+='%s<br/>' % itm['notify']
    return data

@get('/tm.csv')
def tm_json():
    fname = request.get_header('X-Filename')
    if fname is None:
        return 'ERR'
    if not os.path.exists('schedule/%s.dat' % fname):
        return 'FNF'
    return open('schedule/%s.dat' % fname).read()

@post('/tm.csv')
def tm_post():
    fname = request.get_header('X-Filename')
    if fname is None:
        return 'ERR'
    try:
        data = request.body.read()
        open('schedule/%s.dat' % fname, 'w').write(data[:-1])
        return 'SAVED'
    except:
        return 'ERR'

@get('/toggle_oncall')
def toggle_oncall():
    if cron.oncall:
        cron.oncall = False
        pub.say('You are no longer in on-call mode.')
    else:
        cron.oncall = True
        pub.say('You are currently configured to be on-call.')
    return 'OK'

@get('/fw')
def fw():
    ip = request.query.get('ip', None)
    open('/run/saltctl','w').write('fw|%s' % ip)
    return 'OK'

@get('/adm')
def adm():
    machine = request.query.get('machine', None)
    open('/run/saltctl','w').write('adm|%s' % machine)
    return 'OK'

@get('/lck')
def lck():
    machine = request.query.get('machine', None)
    open('/run/saltctl','w').write('lck|%s' % machine)
    return 'OK'

app = default_app.pop()
