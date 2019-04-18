# schedule directory

This directory is purposely empty, it is used to override specific days with a completely custom schedule.  This is useful for vacation days, or very special days.

To use this feature, create a text file called **Monday.dat**, where *Monday* is the day of the week you wish to replace a schedule for.  Within this text file, place lines like this:

```13:00,1,Have you visited Otto Gelato yet?```

Which is basically a CSV file, the first column is the time the event should occur, the second is the specific control code to use, and the last one is a parameter string for use with the specific control code.  Check out *scheduler.py* to learn more about the control codes, or create your own.
