import urllib

def send_event(event, message):
  urllib.urlopen('https://maker.ifttt.com/trigger/%s/with/key/***IFTTT_KEY***' % event, urllib.urlencode({'value1':message}))

class VoiceAction(object):
  PREFIX_MAP = (
    'say',
    'open door for',
  )
  CTX_MAP = (
    'last shower',
  )
  def __init__(self, commander):
    self.commander = commander
  def say(self, text):
    self.commander.say(text)
  def sanitize(self, cmd):
    return cmd.replace(' ', '_')
  def parse(self, text):
    for k in self.CTX_MAP:
      if k in text:
        handler = getattr(self, 'ctx_%s' % self.sanitize(k), None)
        handler()
        return True
    for k in self.PREFIX_MAP:
      if text.find(k) == 0:
        handler = getattr(self, 'prefix_%s' % self.sanitize(k), None)
        handler(text[len(k)+1:])
        return True
    handler = getattr(self, 'do_%s' % self.sanitize(text), None)
    if handler:
      handler()
    else:
      self.say("I'm sorry Kevin, but I don't know how to do %s." % text)
  def do_arm_security(self):
    send_event('arm_security','voice_action')
    self.say('I have armed the security system Kevin.')
  def do_disarm_security(self):
    send_event('disarm_security','voice_action')
    self.say('Kevin, the security system has been disarmed.')
  def do_arm_away(self):
    send_event('arm_away','voice_action')
  def prefix_say(self, text):
    self.say(text)
  def prefix_open_door_for(self, who):
    x=urllib.urlopen('https://www.veroneau.net/**URL_MASKED**', 'who=%s' % who)
    if x.read() == 'OK':
      self.say('The front door has been opened for %s.' % who)
    else:
      self.say('I was unable to open the front door Kevin.')
  def ctx_last_shower(self):
    self.say('You last had a shower today, Kevin.')

