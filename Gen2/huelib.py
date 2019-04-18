from phue import Bridge, PhueRequestTimeout
from automation import cron
from pubserver import pub
import shelve

hue = Bridge('hue')

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
            pub.say('Bridge has timed out.')
            tries+=1
            if tries > 5:
                break

def loadtheme(name):
    db = shelve.open('hue.db')
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
