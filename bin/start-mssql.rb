#!/usr/bin/env ruby

require 'bundler/inline'
require 'bundler/ui'

ui = Bundler::UI::Shell.new.tap { |s| s.level = "confirm" }

gemfile(true, {ui: ui}) do
  source 'https://rubygems.org'

  gem 'thor'
  gem 'childprocess'
end

class StartMSSQL < Thor
  desc "list tables in DB", "say hello to NAME"
  class_options(
    { :uname => :required,
      :pass => :required,
      :db => :required,
      :server => :required})

  def list_tables()
    r, w = IO.pipe
    cp = ChildProcess.build(
      "sqlcmd",
      "-S", options[:server],
      "-U", options[:uname],
      "-d", options[:db],
      "-P", options[:pass],
      "-h-1",
      "-Q", "select TABLE_NAME from information_schema.tables where table_type = \'BASE TABLE\';\nGO"
    )
    cp.io.stdout = w
    cp.start
    w.close
    lines = r.readlines
    puts lines.reject { |l| l =~ /^MSmerge_/}
    cp.wait
  end

  desc "list columns in table", "say hello to NAME"
  def list_columns(table_name)
    cp = ChildProcess.build(
      "sqlcmd",
      "-S", options[:server],
      "-U", options[:uname],
      "-d", options[:db],
      "-P", options[:pass],
      "-h-1",
      "-Q", "select top 10 concat(column_name, \", \", data_type) from information_schema.columns where TABLE_NAME='#{table_name}';\nGO"
    )
    cp.io.inherit!
    cp.start
    cp.wait

  end

end

StartMSSQL.start(ARGV)
