# Personal Reflections on the RVHS Science Lab Undertaking Project

This repository contains code originally written in 2014–2016 by me for my high
school, River Valley High School, Singapore. It has since been decommissioned.
No new development or improvement will be made.

It was a project for the school, but it was every bit as much of a personal
project as it could be; “personal” not in the sense that it was merely an
unimportant part-time hobby project, but in the sense that the story of it had
become so intertwined with my own that they became inseparable; for the few
months during its development, it was absolutely my entire focus. I voluntarily
pulled many all-nighters for this, and I did not regret them; waking up at noon
to the sweet pandemonium of children coming home from school was one of the best
memories I had. It was, essentially, a coming-of-age for the programmer within
me. And it led me to my first job after high school.

I've been writing programs since 2010 (or if non-Turing complete languages
count, since 2005), but none were very large: none had a mix of languages, and
none had more than 5,000 lines. Many of them simply implemented a single
algorithm to solve a clear, mathematically defined problem like those you would
find in algorithm contests; some of them are what I call “automation scripts,”
those programs that simply allow me to save time on a manual task; still others
are merely experiments, unplanned excursions into this vast and beautiful field
of computer science for the purpose of my personal edification.

In retrospect, I was incredibly glad that the then Head of Science, Mr. Chow Ban
Hoe contacted me in early November 2014 for this project. The project
requirements seemed simple: the school's science department needs to conduct
safety briefings at the beginning of every year, and then have students sign a
form acknowledging that they have understood everything in the briefing. The
school wanted to automate this. As time passed, it had acquired a double duty of
acquiring certain information from students. To be reductive, it was a Yet
Another CRUD App™, but hey, we all need to start somewhere.

## What I Did Right

-  Event sourcing. It was one of those choices whose gravity had totally eluded
   me when I made it, and this project certainly did not utilize the idea to its
   fullest. I still admire the theoretical elegance of SQL and the engineering
   marvel of its implementations (though inevitably losing some of its
   elegance), but I was glad I did not just “stick my app in front of an SQL
   database.” The main, primary source of truth should not be data itself, but
   rather how the data came to be (events). SQL could play a much smaller role,
   as the non-primary source of the data, if needed. Much ink has already been
   spilled on evangelizing event sourcing so I will not repeat them here. I am
   certainly without bias, but event sourcing affords projects with a kind of
   freedom in data storage that would be sorely missed in conventional projects.

-  In-memory data. Even best practices for SQL servers recommend giving the
   server enough RAM to hold all but its least important data. And when
   everything is in memory, language-native facilities can be used to store
   data. This means as long as good data structures and algorithms are used,
   performance should not normally be a problem. And yes, RAM is cheap, and
   performance is important to the user.

-  Strong, statically typed. My previous project (a much smaller one) was in
   Python. Late in the development stage there was this fear of deployment:
   fearing that an undiscovered bug could take down everything; related was this
   fear of refactoring: fearing that refactoring could introduce a new bug. The
   more the compiler (or a separate static checker) can check for you, the more
   confident I am. It feels backward going to a language as loosely typed as
   Python. Of course, Haskell is not the best language ever—far from it, in
   fact—but it helps a lot in practice. To be sure, I am not advocating a kind
   of hyper-type-correctness that only dependently typed languages can achieve,
   but with some language extensions turned on and others turned off, Haskell is
   very close to my preferred optimal balance of typed-ness.

-  Good error messages. Error messages to the user should be clear, detailed (if
   slightly verbose), devoid of technicalities, and actionable. And they should
   have good style, not the least of which includes using proper en- and
   em-dashes and curly quotes. This is the first project in which I am satisfied
   in my handling of user-facing error messages. It still has room for
   improvement though.

## What I Should Have Done Better

-  Continuous integration and automated testing. It is now unbelievable to me,
   but yes I developed the whole project without automated testing. I push my
   commits to master and manually test them. But there is no evidence of the
   functionality being present or acceptable. Naturally the strong types of
   Haskell alleviated much of the need of unit tests, but there are many other
   kinds of automated tests that any project of this size deserves.

-  Better deployment practices. Deployment is done by a shell script, wow! And
   then the simplicity of the shell script means there are many modifications
   “by hand” to the server itself through the command line, and naturally none
   of these are documented or even remembered. In an ideal world, everything
   should be automated.

-  Backups. Backups of the production server is done “by hand,” on an irregular
   schedule. Backups should happen automatically and predictably.

-  Replication, redundancy, and multiple servers. There is a genuine cost
   concern here in this project, and the power of a single server should not be
   underestimated. However running on a single server is inherently risky. Any
   larger project should have multiple servers for redundancy.

-  Monitoring. There should be automated tools to monitor the app, its health,
   and other metrics.

-  The coupled web layer. Yesod was then the best Haskell framework for web
   apps, but I used too many of its features and made it too central a part of
   my app. Servant has many good ideas, and one of which is encouraging the
   separation between producing the actual response to requests and the mundane
   task of serving them. In retrospect, the code could be clearer and more
   maintainable (not to say faster compiles) if I had properly separated those
   concerns even without using Servant.

* * *

In the famous book *JavaScript: The Good Parts*, Douglas Crockford so remarked
in the first chapter:

> When I was a young journeyman programmer, I would learn about every feature of
> the languages I was using, and I would attempt to use all of those features
> when I wrote. I suppose it was a way of showing off, and I suppose it worked
> because I was the guy you went to if you wanted to know how to use a
> particular feature.

And I suppose I did the same. I used plenty of bad or unnecessary features of
the languages I used in this project (both Haskell and—the irony!—JavaScript),
as well as the web framework Yesod. I suppose this is also part of the
coming-of-age process: having traversed enough thorns and minefields that
avoiding them becomes an instinct. And this code that I am releasing today
contains ample evidence of stepping onto minefields and being pierced by thorns.
Let this code be a truthful, if plainly too verbose and too obscure, an account
of my growing up as a programmer: a Bildungsroman of sorts.

Zhouyu Qian<br>
January, 2017<br>
Los Angeles
