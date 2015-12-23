# Creating youtube videos.

Can't believe the awesome sounds coming out of your Raspberry Pi?

Want to create youtube videos of your awesome Sonic Pi jams with your friend?

OK, let's show you. Although, I'll warn you. It's a little tough at the moment...

We're going to do three things:

1. Record the audio
2. Record the screen
3. Combine then into one video.

## Recording the audio

This is the easiest part. When you're about to start jamming simply hit
the `Rec` button in the top icons within Sonic Pi. When you've finished
jamming, hit it again and it will open up a save dialog. Choose a
filename, say foo.wav and save.

You now have a wave file called foo.wav.

## Recording the screen

This is the hardest part.

Currently the best method I've found is to use an external computer via
VNC. You'll need a home network (i.e. wifi) and an external computer
(i.e. a Mac or PC) with screen recording software and a VNC viewer.

For example, I have a Mac with QuickTme Player (pre-installed) and
Chicken v2.2b2 (freeware).

## Configuring your Pi

First, we need to know the IP address of your Pi. You can find this by
typing:

    ifconfig

This will list all the info about your Pi's network interfaces. You'll
see sections such as:

    wlan0       Link encap: Ethernet  HWaddrr FF:FF:FF:FF:FF:FF
                inet addr:192.168.10.10 Bcast: 192.168.10.255  Mask: 255.255.255.0
                UP BROADCAST RUNNING MULTICAST MTU:1500 Metric: 1

Here, the vital bit of information we're look for is on the second line:
`inet addr: 192.168.10.10` which tells us which internet address that
interface is available on. This is kind of like a telephone number. Not
all interfaces will have an `inet addr` line as not all interfaces will
be connected to something. The interfaces you're likely to find a
connection on are `wlan0` (WIFI) or `eth0` (ethernet). Note down the
long number with dots in - that's the IP address:. In this case:
`192.168.10.10`.

Next let's get your Pi set up to send the screen to your external
machine. First, install `x11vnc`:

    sudo apt-get update
    sudo apt-get install x11vnc

Now, create a password for it:

    x11vnc -storepasswd replace_with_password ~/.vncpasswd

Start up the server with:

    x11vnc -ncache 10 -rfbauth .vncpasswd -viewonly


## Configure your external computer

Fire up your VNCViewer software and type in the IP address and your
password. When you hit connect, y ou should see your Pi's screen
mirrored on your computer.

## Capture video

When you want to start your jam, start recording the screen of your
external computer with your external screen recording software. On my
Mac, I open the QuickTime Player app, hit `File -> New Screen Recording`
and follow the instructions. When I want to stop jamming, I simply stop
recording with my recording software. On my Mac I hit the little stop
button in the top tool bar.

### Combining Audio and Video

You'll need some video editing software for this. Sometimes even your
screencast software may even provide you these features. For example,
ScreenFlow, lets you drag the audio wav file into the internal editor
and you can just drag it horizontally to sync the timing.

Have fun and share your videos! :-)


