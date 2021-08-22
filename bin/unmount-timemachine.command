#!/usr/bin/env ruby

VOLUMES_TO_UNMOUNT = [
  "/Volumes/LaCie",
  "'/Volumes/LACIE SHARE/'"
]


def osamsg(msg)
  system %Q{osascript -e 'display notification "#{msg}" with title "TM Unmount Status"'}
end

def display_done_notification
  osamsg "Successfully Unmounted!"
end

def display_still_running_notification
  osamsg "TM running, sleeping..."
end

def display_quitting_notification
  osamsg "QUITTING!!!!"
end

def is_tm_running?
  `tmutil status`.match /Running = 1;$/
end

def unmount
  VOLUMES_TO_UNMOUNT.each do |vol|
    `diskutil umount #{vol}`
  end
end

30.times do

  if is_tm_running?
    display_still_running_notification
    sleep 5
  else
    unmount
    display_done_notification
    exit 0
  end

end

display_quitting_notification

exit 0
