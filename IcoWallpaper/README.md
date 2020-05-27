# Icosahedron Animation
This program projects a 3D scene using the projection method outlined in
[Inconvergent's article](https://inconvergent.net/2019/depth-of-field/).
The article describes, how to achieve a depth of field effect.
I created this project because I wanted to make myself a live wallpaper for my laptop.
Also, I used my own projection functions, so I don't guarantee efficiency.

The implementation is currently monochrome,
but I would like to, at some point, add colors.
Adding colors is also described in [another article](https://inconvergent.net/2019/colour-shift/).

## Building the video file
The animation is saved as an image sequence,
which can be turned into an avi video file using ffmpeg:
`ffmpeg -i capture/frame%04d.ppm -codec huffyuv video.avi`

## Setting the video as an animated wallpaper
Install `xwinwrap` and `mpv`.
Save the video in a directory (eg. /home/user/Videos/wall.avi).
Move the systemd service file `animated-wall.service` to `/etc/systemd/system`.
Remember to replace the path to the video in the service file.

If you are on laptop, I also suggest adding the included udev rule
that starts the service, when AC power is plugged in
and stop the service, when plugged off.
To do this, move the `90-animatedwall.rules` to `/etc/udev/rules.d`.
