---
layout: post
title: Timezone converter utility
date: '2016-05-10T16:36:39+01:00'
---

Many times I needed a tool or an utility for quick conversion between timezones, to book a meeting with my US 
collegues, for example. And today, to automate this process, I've made a short Groovy script, doing this job for me:

```groovy
@Grab( 'joda-time:joda-time:2.3' )
import org.joda.time.*
import org.joda.time.format.*

def fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")

def time = args[0]
def timezone = args[1]

def defaultZone = DateTimeZone.getDefault()

time = fmt.parseDateTime(time).withZone(defaultZone)
timezone = DateTimeZone.forID(timezone)

println time.toDateTime(timezone).toString(fmt)
```

To make it runnable, I placed this script in my `/usr/local/bin` directory, 
changed its permissions with `sudo chmod a+rx /usr/local/bin/convert-timezone` 
and added Groovy shebang at the very beginning of the file:

```bash
#!/usr/bin/env groovy
```

And now all I need to, say, get the time in California _(UTC-08:00)_ when I'll have 19:00 on Friday is:

```bash
$ convert-timezone "2016-05-10 19:00" "Pacific/Pitcairn"
2016-05-10 09:00
```

Those strange timezone codes are listed [http://joda-time.sourceforge.net/timezones.html](here).
