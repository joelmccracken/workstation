#!/usr/bin/env ruby

initial_head = `git rev-parse HEAD`
puts "starting date/time incrementing with HEAD=#{initial_head}"
puts "rebasing on to #{ARGV[0]}"
change_picks_to_edits = <<-EDIT_PICKS.split("\n").join(';')
output = [];

File.read(ARGV[0]).each_line { |l|
  output << l.gsub(/^pick/, \\"edit\\")
}

File.write(ARGV[0], output.join(\\"\\n\\"))
EDIT_PICKS


puts `GIT_EDITOR="ruby -e '#{change_picks_to_edits}'" git rebase -i #{ARGV[0]}`

require 'time'

this_time = Time.now - 500

while `git status` =~ /rebase in progress/
  puts `git commit --amend --date="#{this_time.to_s}" --reuse-message HEAD`
  `git rebase --continue`
  this_time += 1
end
