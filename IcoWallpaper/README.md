# Icosahedron Animation
This program projects a 3D scene using the projection method outlined in
[Inconvergent's article](https://inconvergent.net/2019/depth-of-field/).
The article describes, how to achieve a depth of field effect.
I created this project because I wanted to make myself a live wallpaper for my laptop.

The implementation is currently monochrome,
but I would like to, at some point, add colors.
Adding colors is also described in [another article](https://inconvergent.net/2019/colour-shift/).

## Building the video file
The animation is saved as an image sequence,
which can be turned into an avi video file using ffmpeg:
`ffmpeg -i capture/frame%04d.ppm -codec huffyuv video.avi`
