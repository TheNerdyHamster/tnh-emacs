#!/bin/sh

#picom &

xset -b

exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
