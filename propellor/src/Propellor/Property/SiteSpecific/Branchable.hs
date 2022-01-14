module Propellor.Property.SiteSpecific.Branchable where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Borg as Borg
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.SiteSpecific.JoeySites (rsyncNetBorgRepo)

server :: [Host] -> Property (HasInfo + DebianLike)
server hosts = propertyList "branchable server" $ props
	& "/etc/timezone" `File.hasContent` ["Etc/UTC"]
	& "/etc/locale.gen" `File.containsLines`
		[ "en_GB.UTF-8 UTF-8"
		, "en_US.UTF-8 UTF-8"
		, "fi_FI.UTF-8 UTF-8"
		]
		`onChange` (cmdProperty "locale-gen" [] `assume` MadeChange)

	& Apt.installed ["etckeeper", "ssh", "popularity-contest"]
	& Apt.serviceInstalledRunning "apache2"
	& Apt.serviceInstalledRunning "ntp"

	& Apt.serviceInstalledRunning "openssh-server"
	& Ssh.passwordAuthentication False
	& Ssh.hostKeys (Context "branchable.com")
		[ (SshDsa, "ssh-dss AAAAB3NzaC1kc3MAAACBAK9HnfpyIm8aEhKuF5oz6KyaLwFs2oWeToVkqVuykyy5Y8jWDZPtkpv+1TeOnjcOvJSZ1cCqB8iXlsP9Dr5z98w5MfzsRQM2wIw0n+wvmpPmUhjVdGh+wTpfP9bcyFHhj/f1Ymdq9hEWB26bnf4pbTbJW2ip8ULshMvn5CQ/ugV3AAAAFQCAjpRd1fquRiIuLJMwej0VcyoZKQAAAIBe91Grvz/icL3nlqXYrifXyr9dsw8bPN+BMu+hQtFsQXNJBylxwf8FtbRlmvZXmRjdVYqFVyxSsrL2pMsWlds51iXOr9pdsPG5a4OgJyRHsveBz3tz6HgYYPcr3Oxp7C6G6wrzwsaGK862SgRp/bbD226k9dODRBy3ogMhk/MvAgAAAIEApfknql3vZbDVa88ZnwbNKDOv8L1hb6blbKAMt2vJbqJMvu3EP9CsP9hGyEQh5YCAl2F9KEU3bJXN1BG76b7CiYtWK95lpL1XmCCWnJBCcdEhw998GfJS424frPw7qGmXLxJKYxEyioB90/IDp2dC+WaLcLOYHM9SroCQTIK5A1g= root@pell")
		, (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA1M0aNLgcgcgf0tkmt/8vCDZLok8Xixz7Nun9wB6NqVXxfzAR4te+zyO7FucVwyTY5QHmiwwpmyNfaC21AAILhXGm12SUKSAirF9BkQk7bhQuz4T/dPlEt3d3SxQ3OZlXtPp4LzXWOyS0OXSzIb+HeaDA+hFXlQnp/gE7RyAzR1+xhWPO7Mz1q5O/+4dXANnW32t6P7Puob6NsglVDpLrMRYjkO+0RgCVbYMzB5+UnkthkZsIINaYwsNhW2GKMKbRZeyp5en5t1NJprGXdw0BqdBqd/rcBpOxmhHE1U7rw+GS1uZwCFWWv0aZbaXEJ6wY7mETFkqs0QXi5jtoKn95Gw== root@pell")
		, (SshEcdsa, "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLRRZ3Ew6eq6d8+ID1CXwF0hLjObNM2XwCIOFI4Wml2iP5NIHwtUCg2hlVUal6v1bO+VPjvx3dkf5Y00GI2BVSY= root@pell")
		, (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG5gaqToi0NtZH+vxXqW8E/reJW2tMHOEs2ycieMYkng root@pell")
		]

	& Apt.installed ["procmail", "bsd-mailx"]
	& "/etc/aliases" `File.hasPrivContentExposed` (Context "branchable.com")
		`onChange` Postfix.newaliases
	& "/etc/mailname" `File.hasContent` ["branchable.com"]
	& Postfix.installed
	& Postfix.mainCf ("mailbox_command", "procmail -a \"$EXTENSION\"")
	
	-- backup everything except the contents of sites, which are
	-- backed up by ikiwiki-hosting.
	& Borg.backup "/" (rsyncNetBorgRepo "pell.borg" []) Cron.Daily
		[ "--exclude=/proc/*"
	 	, "--exclude=/sys/*"
	 	, "--exclude=/run/*"
	 	, "--exclude=/tmp/*"
	 	, "--exclude=/var/tmp/*"
	 	, "--exclude=/var/backups/ikiwiki-hosting-web/*"
	 	, "--exclude=/var/cache/*"
	 	, "--exclude=/home/*/source/*"
	 	, "--exclude=/home/*/source.git/*"
	 	, "--exclude=/home/*/public_html/*"
	 	, "--exclude=/home/*/.git/*"
	 	]
	 	[ Borg.KeepDays 7
	 	, Borg.KeepWeeks 5
	 	, Borg.KeepMonths 3
	 	, Borg.KeepYears 1
	 	]
	& Ssh.userKeys (User "root") (Context "branchable.com")
		[ (SshEd25519, "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC20PCLAgkD6tK0vYsl0Pdpho+y7fNay8Fo8SXWrZojs root@pell")
		]
	& Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")

	& adminuser "joey"
  where
	adminuser u = propertyList ("admin user " ++ u) $ props
		& User.accountFor (User u)
		& User.hasSomePassword (User u)
		& Sudo.enabledFor (User u)
		& User.hasGroup (User u) (Group "adm")
		& User.hasGroup (User u) (Group "systemd-journal")
