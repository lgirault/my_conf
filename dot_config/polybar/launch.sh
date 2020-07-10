#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
if [ "$(hostname)" = "karas" ] || [ "$(hostname)" = "lgt-pc" ]
then
    polybar "$(hostname)" &
else
  if ! pgrep -x "nm-applet" >/dev/null
  then
    nm-applet &
  fi
  polybar laptop &
fi

echo "Bars launched..."
