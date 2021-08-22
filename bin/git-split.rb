#!/usr/bin/env ruby

stopped_sha = File.read(".git/rebase-merge/stopped-sha").strip
commit_message = `git log -n 1 --pretty=%B #{stopped_sha}`.gsub(/'/, '')

puts "using commit #{stopped_sha}"
puts "using message #{commit_message}"

`git reset HEAD^`

files = `git status -s`
        .split("\n")
        .map { |x|
          m = x.match(/[\s]*([\w]*)[\s](.*)/)
          case m[1]
          when "M" then true
          when "D" then true
          when "" then true
          else raise "Encountered a file I didn't know what to do with in #{m.inspect}"
          end
          m[2]
        }

files.each do |file|
  `git add #{file}`
  msg = "#{file} #{commit_message}"
  puts "committing: #{msg}"
  `git commit -m '#{msg}'`
end
