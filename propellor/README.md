[Propellor](https://propellor.branchable.com/) is a
configuration management system using Haskell and Git.
Each system has a list of properties, which Propellor ensures
are satisfied.
[Linux](http://propellor.branchable.com/Linux/) and
[FreeBSD](http://propellor.branchable.com/FreeBSD/) are supported.

Propellor is configured via a git repository, which typically lives
in `~/.propellor/` on your development machine. Propellor clones the
repository to each host it manages, in a 
[secure](http://propellor.branchable.com/security/) way. See 
[components](http://propellor.branchable.com/components/)
for details.

Properties are defined using Haskell in the file `~/.propellor/config.hs`.
There is fairly complete 
[API documentation](http://hackage.haskell.org/package/propellor/),
which includes many built-in Properties for dealing with
[Apt](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Apt.html)
and
[Apache](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Apache.html),
[Cron](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Cron.html)
and
[Commands](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Cmd.html),
[Dns](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Dns.html)
and
[Docker](http://hackage.haskell.org/package/propellor/docs/Propellor-Property-Docker.html), etc.

There is no special language as used in puppet, ansible, etc.. just
the full power of Haskell. Hopefully that power can be put to good use in
making declarative properties that are powerful, nicely idempotent, and
easy to adapt to a system's special needs!

If using Haskell to configure Propellor seems intimidating,
see [configuration for the Haskell newbie](https://propellor.branchable.com/haskell_newbie/).

## quick start

1. Get propellor installed on your development machine (ie, laptop).
     `apt-get install propellor`
          or
     `cabal install propellor`
          or
     `cabal unpack propellor; cd propellor-version; stack install`
2. Run `propellor --init` ; this will set up a `~/.propellor/` git
   repository for you.
3. Edit `~/.propellor/config.hs`, and add a host you want to manage.
   You can start by not adding any properties, or only a few.
4. Run: `propellor --spin $HOST`
5. Now you have a simple propellor deployment to a host. Continue editing
   `~/.propellor/config.hs` to further configure the host, add more hosts
   etc, and re-run `propellor --spin $HOST` after each change.  
6. Once you have a lot of hosts, and running `propellor --spin HOST` for
   each host becomes tiresome, you can
   [automate that](http://propellor.branchable.com/automated_spins/).
7. Write some neat new properties and send patches!

(Want to get your feet wet with propellor before plunging in?
[try this](http://propellor.branchable.com/forum/Simple_quickstart_without_git__44___SSH__44___GPG))
