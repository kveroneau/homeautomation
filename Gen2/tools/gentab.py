"""
This file (re)generations the 'tables' file which is read on-demand in the scheduler code to determine projects and leisure activities.
The reason I do this, is so that this information is not hardcoded in the scheduler, and can then be reloaded without restarting anything.

In each tuple, the first entry is the project or leisure string, the second item is how high of a priority it is for you.
"""

projects = (
    ('Server migration', 10),
    ("Hacker's Edge", 8),
    ('Home system upgrades', 7),
    ('Home automation updates', 5),
)

leisure = (
    ('Final Fantasy 9', 5),
    ('Saga Frontier', 5),
    ('Elder Scrolls Online', 8),
    ('Mario Maker', 5),
    ('Dragon Quest 11', 20),
)

f = open('tables','wb')
f.write(chr(len(projects)))
for prj in projects:
    f.write(chr(len(prj[0]))+prj[0])
    f.write(chr(prj[1]))
f.write(chr(len(leisure)))
for prj in leisure:
    f.write(chr(len(prj[0]))+prj[0])
    f.write(chr(prj[1]))
f.close()
