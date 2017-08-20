# A daemon to control my keyboard backlight

I recently got a new (ASUS) laptop with a backlit keyboard.  With a fresh Arch
linux install, the keys marked for keyboard brightness control were not
functional.  This is the story of how I implemented this functionality.

A quick internet search turns up an [Arch Wiki
page](https://wiki.archlinux.org/index.php/Keyboard_backlight).  There are two
noteworthy things here.

1. Maybe I can use DBus to interact with a program that controls the keyboard
	 brightness.
2. The keyboard brightness reflects the contents of files in
	 `/sys/class/leds/asus::kbd_backlight/brightness`.

The Python script on the Arch Wiki page uses DBus to communicate with UPower.
What is UPower?  It is power management middleware that is not currently
running on my system.  Now, `upower` is available in `Arch/Extra`, so I could
probably make my keyboard backlight work in the manner described by the Wiki.
However, I'm going to use this as an opportunity to learn something
communicating with a daemon using DBus.

The project consists of the following files:

* `Main.hs` - daemon source
* `kbd-backlight.conf` - dbus policy for the new bus we create
* `KbdBacklight.service` - systemd unit for autostarting the daemon

To install the project, we build the daemon binary and make it available as
`/usr/local/bin/kbdbacklightd`.  We also make the new dbus policy and systemd unit available
in `/etc/dbus-1/system.d` and `/lib/systemd/system`, respectively.

Finally, we add the following keybindings to `~/.config/i3/config`:

```
bindsym XF86KbdBrightnessUp exec dbus-send --system /localhost/KbdBacklight localhost.KbdBacklight.inc
bindsym XF86KbdBrightnessDown exec dbus-send --system /localhost/KbdBacklight localhost.KbdBacklight.dec
```
