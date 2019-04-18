# HomeAutomation

This is the source code for my home automation system currently written in Python, running on a Raspberry Pi.  It was originally written to automate and control my Hue lights, but has grown into something massive and extremely useful(at least for my personal use).  I currently use this code in my everyday life.

The code itself isn't the best around as I never originally had any intent to publically release it as I am doing now.  Thus, there is no proper center configuration file, everything was basically hardcoded in.  As a result, in order to publically release this code, I needed to mask a lot of stuff out, and as a result some of the code which will rely on my Twilio Interactive Voice Response system on a cloud server will not function unless you create your own similar IVR system with similar endpoints.  I may at one point make the Interactive Voice Response code publically available as well.

Currently only my first Generation home automation code is publically available.  Feel free to watch this repo to be notified when the Generation 2 code is available.  Only Generation 2 code is being used in my home at the moment, although a lot of code has been reused from Generation 1.  Generation 2 is much better code overall, and has many more new features, such as a daily schedule system, and reused code is now placed into a seperate module.
