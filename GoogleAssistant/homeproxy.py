from bottle import get, post, request, default_app, HTTPResponse, response
import urllib, json, datetime, shelve, logging, redis

NOC = {
    'Sunday': 'Pizza, Budweiser and Bud light',
    'Monday': 'Domestic bottles',
    'Tuesday': '$20 Bottles of house wine, wild rose pints, and steak special',
    'Wednesday': 'Original 16 and Wing night',
    'Thursday': 'Pasta special and $5 glasses of house wine',
    'Friday': 'Alexander Keith\'s and fish and chips',
    'Saturday': 'Tacos special, tequila, dos equis and sol',
}

BREWSTERS = {
    'Sunday': 'Kegs and eggs brunch, seasonal brews, and caesars',
    'Monday': 'Steak sandwich and big brewskis',
    'Tuesday': 'Wing night and river city raspberry ale',
    'Wednesday': 'Gyoza night',
    'Thursday': 'British pub night, fish and chips, hawaiian coconut porter and firesale blondes',
    'Friday': 'Farmer\'s tan ale and firesale blondes',
    'Saturday': 'Hammerhead saturdays, cheap growler fills, kegs and eggs brunch, hammerhead red ale, caesars, and firesale blondes',
}

def proxy(uri):
    return urllib.urlopen('http://***ENDPOINT_WITHHELD***/%s' % uri).read()

@get('/')
def test_view():
    return 'Nothing to see here.'

@post('/say')
def say():
    text = request.body.read()
    urllib.urlopen('http://***ENDPOINT_WITHHELD***/say', text)
    return 'OK'

@get('/fw')
def fw():
    return proxy('fw?ip=%s' % request.remote_addr)

@get('/summon_taxi')
def summon_taxi():
    return proxy('summon_taxi')

@get('/toggle_pause')
def toggle_pause():
    return proxy('toggle_pause')

@get('/toggle_wakeme')
def toggle_wakeme():
    return proxy('toggle_wakeme')

@get('/toggle_rooms')
def toggle_rooms():
    return proxy('toggle_rooms')

@get('/leaving')
def leaving():
    return proxy('entrance')

@get('/study')
def study():
    return proxy('study')

@get('/blacklight')
def blacklight():
    return proxy('blacklight')

@get('/bathroom')
def bathroom():
    return proxy('bathroom')

@get('/bedroom')
def bedroom():
    return proxy('bedroom')

@get('/relax')
def relax():
    return proxy('relax')

@get('/allbright')
def allbright():
    return proxy('allbright')

@get('/home')
def coming_home():
    return proxy('home')

def df_say(message):
    return {'messages': [{'speech':message, 'type':0}, {'displayText':message, 'platform':'google', 'textToSpeech':message, 'type':'simple_response'}]}

@post('/dialogflow')
def dialogflow():
    if request.auth is None:
        raise HTTPResponse(status=401, headers={'WWW-Authenticate': 'Basic realm="Veroneau.NET HomeCU"'})
    username, password = request.auth
    if username != '****' and password != '****':
        raise HTTPResponse('Authentication failure.', status=403)
    data = request.json
    if data is None:
        raise HTTPResponse(status=400)
    open('/tmp/dialogflow.json','w').write(json.dumps(data))
    action = data['result']['action']
    if action == 'input.room':
        room = data['result']['parameters']['room']
        if room == 'study':
            proxy('study')
        elif room == 'bedroom':
            proxy('bedroom')
        return df_say('I have asked your system to activate the %s light.' % room)
    elif action == 'input.repeat':
        message = data['result']['parameters']['message']
        urllib.urlopen('http://***ENDPOINT_WITHHELD***/say', message)
        return df_say('I have asked your system to repeat the message.')
    elif action == 'input.toggle':
        automation = data['result']['parameters']['automation']
        if automation == 'lights':
            if proxy('toggle_pause') == 'Paused':
                return df_say('I have paused your light automation system.')
            else:
                return df_say('I have resumed your light automation system.')
        elif automation == 'wake me':
            if proxy('toggle_wakeme') == 'No wake':
                return df_say('Your automation system will not wake you up tomorrow.')
            else:
                return df_say('I have requested that your automation system wakes you up.')
        elif automation == 'rooms':
            proxy('toggle_rooms')
        return df_say('I have asked your system to toggle %s.' % automation)
    elif action == 'input.status':
        health = json.loads(proxy('health'))
        resp = 'Here is your home automation status Kevin.'
        if health['config']['dark']:
            resp+=' It is currently dark outside.'
        if health['config']['home']:
            resp+=' You are currently at home.'
        if health['config']['paused']:
            resp+=' The automation system is currently paused.'
        if health['config']['rooms']:
            resp+=' Multiple rooms are currently enabled.'
        if health['config']['sleeping']:
            resp+=' You are currently in sleep mode.'
        return df_say(resp)
    elif action == 'input.ecweather':
        health = json.loads(proxy('health'))
        resp = 'Your current futurecast is %s' % health['weather']['futurecast'][0]['outlook']
        resp+= ' The current conditions outside are %s, and the temperature feels like %s celsius.' % (health['weather']['conditions'], health['weather']['feels_like'])
        return df_say(resp)
    elif action == 'input.service':
        health = json.loads(proxy('health'))
        system = data['result']['parameters']['systems']
        if system not in health['available'].keys():
            return df_say('I am unable to determine what %s is.' % system)
        if health['available'][system]:
            return df_say('Yes Kevin, %s is currently online and available.' % system)
        else:
            return df_say('I am sadened to say that I cannot contact %s at all.' % system)
    elif action == 'input.active_lights':
        health = json.loads(proxy('health'))
        if len(health['lights']) == 0:
            return df_say('All of your lights are currently turned off.')
        if len(health['lights']) == 1:
            return df_say('The only light on is the %s light.' % health['lights'][0])
        resp = 'The following %s lights are current on, ' % len(health['lights'])
        for lgt in health['lights']:
            resp+= '%s and' % lgt
        resp+= 'that is it.'
        return df_say(resp)
    elif action == 'input.taxi':
        result = proxy('summon_taxi')
        if result == 'OK':
            return df_say('A taxi has been summoned Kevin.')
        elif result == 'NO':
            return df_say('You are currently in sleep mode, and as a result I will not call a taxi for you.')
        elif result == 'NOT_HOME':
            return df_say('You do not appear to be home Kevin, please call me to request a taxi.')
        return df_say('Kevin, this is rather alarming, but I got a bad response from your system.')
    elif action == 'reminder.morning':
        message = data['result']['parameters']['message']
        ctx = urllib.urlencode({'type':'wake','text':message})
        urllib.urlopen('http://***ENDPOINT_WITHHELD***/add_reminder', ctx)
        return df_say('I will remind you when you wake up to %s.' % message)
    elif action == 'reminder.home':
        message = data['result']['parameters']['message']
        ctx = urllib.urlencode({'type':'home','text':message})
        urllib.urlopen('http://***ENDPOINT_WITHHELD***/add_reminder', ctx)
        return df_say('I will remind you when you get home next to %s.' % message)
    elif action == 'reminder.clear':
        proxy('clear_reminders')
        return df_say('Reminders have been cleared.')
    elif action == 'input.shower':
        proxy('bathroom')
        return df_say('Great, have an awesome shower Kevin.')
    elif action == 'input.leaving':
        proxy('entrance')
        return df_say('I will see you when you get back then Kevin.')
    elif action == 'input.hoff':
        proxy('hoff')
        return df_say('I have turned off every light for you.')
    elif action == 'input.relax':
        proxy('relax')
        return df_say('The lights have been set to a more relaxing setting.')
    elif action == 'input.blacklight':
        proxy('blacklight')
        return df_say('The lights have been placed into blacklight mode.')
    elif action == 'input.allbright':
        proxy('allbright')
        return df_say('I will brighten up the entire house for you.')
    elif action == 'input.get_themes':
        themes = proxy('theme')
        return df_say('The current themes you have available are %s.' % themes)
    elif action == 'input.set_theme':
        message = data['result']['parameters']['theme']
        try:
            proxy('theme?name=%s' % message)
            return df_say('I have set the %s theme for you.' % message)
        except:
            return df_safe('I was unable to set the theme you requested.')
    elif action == 'input.brightness':
        level = data['result']['parameters']['percentage']
        proxy('brightness?value=%s' % level[:-1])
        return df_say('I have set the lights to %s brightness level.' % level)
    elif action == 'kodi.party_mode':
        if proxy('party_mode'):
            return df_say('Kevin, I have requested party mode for you.')
        return df_say('I was unable to initiate party mode for you.')
    elif action == 'kodi.menu':
        proxy('xbmc_menu')
        return df_say('Done.')
    elif action == 'kodi.playlist':
        proxy('xbmc_playlist')
        return df_say('Here is your playlist.')
    elif action == 'kodi.trance_party':
        proxy('trance_party')
        return df_say('A trance party has started up in here!')
    elif action == 'kodi.mpd':
        proxy('mpd')
        return df_say('Here is your media player daemon.')
    elif action == 'kodi.play_video':
        song = data['result']['parameters']['song']
        if proxy('play_video?song=%s' % song.lower()) == 'OK':
            return df_say('The video is now playing.')
        return df_say('I was unable to start playing the video requested.')
    elif action == 'kodi.ign_videos':
        proxy('ign_videos')
        return df_say('IGN Videos is now displayed.')
    elif action == 'pub.noc':
        wd = datetime.datetime.now().strftime('%A')
        resp = 'Kevin, the north on center pub on %s has %s on special today.' % (wd, NOC[wd])
        return df_say(resp)
    elif action == 'pub.brewsters':
        wd = datetime.datetime.now().strftime('%A')
        resp = 'Kevin, Brewster\'s on %s has %s on special today.' % (wd, BREWSTERS[wd])
        return df_say(resp)
    return df_say('I am unsure how to handle this request Kevin.')

application = default_app()
