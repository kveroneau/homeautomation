This is the first generation system I built, it is no longer in use and running on my home network.  It has since been replaced with the next generation one.

This code unfortunately suffered from the occational thread-lock, which was why it was rewritten to use Gevent to avoid such issues.

This code here is mainly available for education use only, I would not recommend using it in your own home automation system.

Here's a LS to see how long ago I last used it:

```kveroneau@pi:~/hue $ ls -l
total 520
-rw-r--r-- 1 kveroneau kveroneau 149580 Dec  9  2017 bottle.py
-rw-r--r-- 1 kveroneau kveroneau 164165 Dec  9  2017 bottle.pyc
-rwxr-xr-x 1 kveroneau kveroneau   1797 Dec  9  2017 cli.py
-rw-r--r-- 1 kveroneau kveroneau      5 Dec  9  2017 commander.pid
-rw-r--r-- 1 kveroneau kveroneau   5144 Dec  9  2017 commander.py
-rw-r--r-- 1 kveroneau kveroneau   9404 Dec  9  2017 commander.pyc
-rw-r--r-- 1 kveroneau kveroneau   2264 Dec  9  2017 ecweather.py
-rw-r--r-- 1 kveroneau kveroneau   2625 Dec  9  2017 ecweather.pyc
drwxr-xr-x 2 kveroneau kveroneau   4096 Dec  9  2017 httplib2
-rw-r--r-- 1 kveroneau kveroneau  36864 Jan 29 19:20 hue.db
-rw-r--r-- 1 kveroneau kveroneau  30682 Dec  9  2017 phue.py
-rw-r--r-- 1 kveroneau kveroneau  28266 Dec  9  2017 phue.pyc
-rw-r--r-- 1 kveroneau kveroneau    286 Dec  9  2017 server.log
-rwxr-xr-x 1 kveroneau kveroneau  32304 Feb  8  2018 server.py
-rw-r--r-- 1 kveroneau kveroneau   6011 Dec  9  2017 sunrise_sunset.py
-rw-r--r-- 1 kveroneau kveroneau   4643 Dec  9  2017 sunrise_sunset.pyc
drwxr-xr-x 7 kveroneau kveroneau   4096 Dec  9  2017 twilio
-rw-r--r-- 1 kveroneau kveroneau   1767 Dec  9  2017 voice.py
-rw-r--r-- 1 kveroneau kveroneau   3278 Dec  9  2017 voice.pyc
-rw-r--r-- 1 kveroneau kveroneau   2062 Dec  9  2017 xbmclib.py
-rw-r--r-- 1 kveroneau kveroneau   3057 Dec  9  2017 xbmclib.pyc
```

Not all of these files are publically available, and some of them are just Python modules such as bottle.py and twilio.
