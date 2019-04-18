import socket, xmlrpclib, json

class XBMC(object):
  def __init__(self):
    self._xbmc_sock = None
    self._xbmc_id = 0
  def xbmc_send(self, method, params=None, keep_alive=False, **kwargs):
    if self._xbmc_sock is None:
      try:
        self._xbmc_sock = socket.socket()
        self._xbmc_sock.connect(('sonytv', 9090))
      except:
        return False
    self._xbmc_id +=1
    data = {"jsonrpc": "2.0", "method": method, "id":self._xbmc_id}
    if params:
      data.update({'params':params})
    if kwargs != {}:
      data.update({'params':kwargs})
    self._xbmc_sock.send(json.dumps(data))
    if not keep_alive:
      self._xbmc_sock.close()
      self._xbmc_sock = None
    else:
      resp = self._xbmc_sock.recv(512)
      return resp
    return
  def xbmc_notification(self, title, message, keep_alive=False):
    self.xbmc_send('GUI.ShowNotification', title=title, message=message, keep_alive=keep_alive)
  def party_mode(self):
    try:
      videodb = xmlrpclib.ServerProxy('http://localhost:7070/RPC2')
      song_list = videodb.song_list()
    except:
      self.xbmc_notification("Kevin's Voice Control", "Kevin's computer cannot be contacted.")
      return False
    self.xbmc_notification("Kevin's Party Mode", 'Generating playlist, please wait...', True)
    self.xbmc_send('Playlist.Clear', keep_alive=True, playlistid=1)
    for song in song_list:
      self.xbmc_send('Playlist.Add', keep_alive=True, playlistid=1, item={'file':'smb://PI/DLNA/Music/%s' % song})
    self.xbmc_notification("Kevin's Party Mode", 'Playing songs, enjoy!', True)
    self.xbmc_send('Player.Open', item={'playlistid':1})
  def show_playlist(self):
    self.xbmc_send('GUI.ActivateWindow', window='videoplaylist')
  def open_menu(self):
    self.xbmc_send('Input.ContextMenu')
  def play(self, url):
    self.xbmc_send('Player.Open', item={'file':url})
  def play_video(self, name):
    try:
      videodb = xmlrpclib.ServerProxy('http://localhost:7070/RPC2')
      fname = videodb.video_path(name)
      if fname == False:
        return False
      self.play('smb://PI/DLNA/Music/%s' % fname)
      return True
    except:
      return False
  def activateWindow(self, uri):
    self.xbmc_send('GUI.ActivateWindow', window='videolibrary', parameters=[uri])
