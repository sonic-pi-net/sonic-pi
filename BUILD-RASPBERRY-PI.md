# Building the Latest Sonic Pi on Raspberry Pi

## BUILDING SONIC-PI 4.5 ON RASPBERRY-PI BOOKWORM OS 64Bit

These instructions are specific to building version 4.5 of Sonic Pi to run on a 64bit version of RaspberryPi Bookworm OS
This version was released on October 10th 2023. Either the version with Desktop or the version with Desktop and Recommended Software.

It will NOT work on the 32 bit versions, or on previous versions of Raspberry Pi OS

The built application will run on P3, Pi4, P400 or Pi5 models

Because it is rather different from previous builds I have included a lot of extra information on the usage of this version over and above the mere building which is fairly straghtforward if somewhat slow.  

## Introduction

This version differs from previous ones in two main respects respects. First it use Qt6 which is available on the new OS rather than Qt5, and, since the Mac and Windows versions now use Qt6, this is now flagged as a requirement for future versions.

Secondly it runs under Wayland which is the default screen display system, and uses pipewire for routing the audio output, which again is default on the new OS

## Preparation to build

Download the sonic-pi source version tagged v4.5.0 either by direct download or by cloning
eg

```
git clone https://github.com/sonic-pi-net/sonic-pi.git
cd sonic-pi/app
git checkout v4.5.0
```
Then load in the required packages to support the build
```
sudo apt update
sudo apt-get install -y build-essential git libssl-dev ruby-dev elixir erlang-dev erlang-xmerl qt6-tools-dev qt6-tools-dev-tools libqt6svg6-dev libqt6opengl6-dev supercollider-server sc3-plugins-server alsa-utils  libasound2-dev cmake ninja-build pipewire-jack libspa-0.2-jack qt6-wayland libwayland-dev libxkbcommon-dev libegl1-mesa-dev libx11-dev libxft-dev libxext-dev qpwgraph compton
```
One feature in the build will NOT work at present under Wayland. That is the opacity slider which governs the translucency of the main window. If this is an issue for you, at present it can be worked round by changing the OS to run using X11 and Openbox Window Manager, in which case it will work. Details of how to do this are shown later.

##  The build process

This is simply achieved by running the script below in the app directory
`./pi-build-all.sh`
This will take quite a long time to complete (approx 1hr in a Pi400 but hopefully it will be quicker on a Pi5). If you want you can run the four scripts it contains separately, in which case be sure to add the flag -n with the first two. 

Once the build has completed  you can do an initial test to see if it works. It is located in a build directory inside the app directory. Frommyuour terminal in teh app directory type:
```
build/gui/qt/sonic-pi
```
All being well this will start Sonic Pi running, and you can test it by putting some code in the buffer window and pressing run. eg `play 72` or perhaps loading an example typing `load_example :compus_beats` and then pressing run twice, the first time to load the example and the second time to start it running. All being well you should hear sounds coming from your hdmi monitor speakers.
If you do NOT hear anything, perhaps because you don't have sound built in to your hdmi monitor, then you can check what is going on as follows. First see if there is activity on the Sonic Pi scope window. If you can see "sound" traces there that is great. It probably means that Sonic Pi is running, but just not connected to a sound output device.

In order to help, you can use a program which was installed with Sonic Pi called qwpgraph. You can access it from the Sound Section of the Pi Main Menu
The first time you open it with Sonic Pi running you will see rather a mess of various blocks and wires jumbled up on the screen. We are interested in the green ones which relate to audio connections. Zoom the graph window to full screen, and move the blocks around by dragging the grey bodies of the blocks (NOT the coloured bits in the blocks) so that you untangle any connecting wires. I usually align the output audio blocks with ports marked playback_FL and playback_FR to the right hand side and any blocks marked out or capture on the left. Yuu can move  midi blocks coloured red or purple to the bottom out of the way, as well as any cyan coloured blocks. Hopefully you will see one block named SuperCollider with green outputs SuperCollider:out_1 and SuperCollider:out_2  These should have wires connecting them to a block "Built in Audio Digital Stereo..." If there are no wires connected to the block, or if you don't have audio on your monitor, then you should see a block representing your current audio device, either one for headphone output (on a Pi4 or Pi3) or one representing a usb audio device you have plugged in, or one representing a bluetooth speaker or headphones. You can connect SuperCollider by dragging a wire from the SuperCollider:out ports to each of the desired device input ports.

If you can see the SuperCollider block but you cant see any playback devices, then it is possible that your Pi OS has been switched to work with pulseaudio rather than pipewire. This is done using the command line file raspi-config which you can open in a terminal typing `sudo raspi-config` In teh resiulting screen you want to select option 6 Advanced Options followed by A7 Audio Config. Select the option pipewire then select OK. Select Finish. You need to reboot the machine to reinitialse it.

If you did't see any activity on the Sonic Pi scope trace and there is no SuperCollider icon in the qpwgraph screen, then something is amiss. Check that the build script completed without any error shown (warnings are ok and can be ignored). Quit and check the logs

## Making things easier

It is rather tedious finding the start command in the build folder and you can produce a leaner portable version of Sonic Pi by extracting just what is needed from the build process as follows.

with your terminal window still open in the sonic-pi/app directory, first produce a release version by typing
```
./linux-release.sh
```
When this has completed (it will take some 30-40 seconds) you will see a message saying

app/build/linux-dist is now ready for packaging: in the terminal type:
```
ls build/linux
```
and you will see three folders: app bin and etc These comprise a relocatable install of Sonic Pi which can be copied anywhere you wish on your computer. They will still leave the original build structure, which acts as an insurance backup.  I suggest you move the linux_dist folder perhaps to your desktop and then rename it whatever you wish: for example
```
mv build/linux-dist ~/Desktop/my-sonic-pi
```
Finally you can now start your sonic-pi from the desktop graphical environment.

double click the mysonic-pi icon

double click the bin folder

double click the icon sonic-pi

You will see a popup window with options

Execute, Execute in Terminal, Open, Close

For a first run, to check all is well, choose Execute in Terminal. (Subsequently if all goes well you can choose Execute)

## Other useful information

You will notice that the Sonic Pi splash screen only appears on the first run after you have booted your Pi. This is a known problem whose cause has been identified, but has not yet been solved. Be patient and Sonic Pi will still launch.

Getting transparency to work. You need to configure your Pi so that it is not running under Wayland. To do this access raspi-config from a terminal window with `sudo raspi-config` Select  "6 Advanced Options" followed by "A6 Wayland". Then select "W1 X11  Openbox window manager with X11 backend". Select "OK", confirm the option and the select the reboot option. When your Pi restarts Sonic Pi will behave the same, but you will find that the transparency slider works. However you will lose some of the nice features of Wayland. You can reverse the process by re-running raspi-config and selecting option "W2 Wayfire  Wayfire window manager with Wayland backend".

Setting up an easier method to switch audio devices. Although qpwgraph is fine for switching the output of Sonic Pi, I have written a ruby script which will run on Sonic Pi and which you may like to install in the Sonic Pi init.rb file which is located in the folder `.sonic-pi/config` in your pi home folder. You can navigate to this file from the file manager and open it with the text editor or geany programmer's editor. Copy the code below and paste it into the file.
```

#functions to aid connecting and disconnectiog SuperCollider under pipewire
define :getCurrentData do
  #first input ports
  inputs =  `pw-link -iI`.lines
  set :hdmiL,inputs.grep(/hdmi.*playback_FL$/).first.to_i
  set :hdmiR,inputs.grep(/hdmi.*playback_FR$/).first.to_i
  set :usbL,inputs.grep(/usb.*playback_FL$/).first.to_i
  set :usbR,inputs.grep(/usb.*playback_FR$/).first.to_i
  set :bluezL,inputs.grep(/bluez.*playback_FL$/).first.to_i
  set :bluezR,inputs.grep(/bluez.*playback_FR$/).first.to_i
  set :avJackL,inputs.grep(/audio.*playback_FL$/).first.to_i
  set :avJackR,inputs.grep(/audio.*playback_FR$/).first.to_i
  #now output ports
  outputs = `pw-link -oI`.lines
  scOutPorts=[]
  16.times do |i|
    scOutPorts[i] = outputs.grep(/SuperCollider:out_#{i+1}$/).first.to_i
  end
  set :scOutPorts,scOutPorts
  #now get all current links ids
  links = `pw-link -lI`.lines
  #extract SuperCollider:out links
  linkOutputs=links.grep(/-.*SuperCollider:out/)
  #extract id of each of these links to scLinks
  scLinks=[]
  linkOutputs.length.times do |i|
    scLinks[i]=linkOutputs[i].to_i
  end
  set :scLinks,scLinks
end

define :displayCurrentID do
  #update current data
  getCurrentData
  #display current link id#
  puts "Current data. Note: a 0 signifies no ID found"
  puts "hdmi L and R",get(:hdmiL),get(:hdmiR)
  puts "usb L and R",get(:usbL),get(:usbR)
  puts "avJack L and R",get(:avJackL),get(:avJackR)
  puts "bluez L and R",get(:bluezL),get(:bluezR)
  puts "sc output ports (16 max)",get(:scOutPorts)
  puts "current SuperCollider output link Ids2",get(:scLinks)
end

#display the data
displayCurrentID

###################################################
#function to connect or disconnect SuperCollider
define :connectStereo do |output,input,type=1|
  #puts "data",output,input,type
  portlist=get(:scOutPorts)
  case output
  when :usb
    if get(:usbL) == ""
      puts "not available"
      return
    end
    o1=get(:usbL);o2=get(:usbR)
  when :hdmi
    if get(:hdmiL) == ""
      puts "not available"
      return
    end
    o1=get(:hdmiL);o2=get(:hdmiR)
  when :avjack
    if get(:avJackL) == ""
      puts "not available"
      return
    end
    o1=get(:avJackL);o2=get(:avJackR)
  when :bluez
    if get(:bluezL) == ""
      puts "not available"
      return
    end
    o1=get(:bluezL);o2=get(:bluezR)
    #puts "bluez",o1,o2
  else
    puts "not available"
    return
  end
  if input == ""
    puts "input not available"
    return
  end
  if portlist[input-1] == "9" or portlist[input] == "0"
    puts "outputs not available"
    return
  end
  i1=portlist[input-1];i2=portlist[input]
  action=["pw-link -d ","pw-link -L "]
  cmd = action[type]+i1.to_s+" "+o1.to_s
  puts cmd
  system(cmd)
  cmd = action[type]+i2.to_s+" "+o2.to_s
  puts cmd
  system(cmd)
end

#function to delete all SuperCollider output connections
define :deleteSCout do
  getCurrentData
  links=get(:scLinks)
  return if links.length==0
  links.each do |n|
    cmd= "pw-link -d #{n}"
    puts cmd
    system(cmd)
  end
end

###########################################################
#four functions that let you easily swap the main Sonic Pi output audio routing

define :gohdmi do
  getCurrentData #they may have changed
  deleteSCout
  connectStereo :hdmi,1,1
end
define :gousb do
  getCurrentData#they may have changed
  deleteSCout
  connectStereo :usb,1,1
end
define :gobluez do
  getCurrentData#they may have changed
  deleteSCout
  connectStereo :bluez, 1,1
end
define :goavjack do
  getCurrentData#they may have changed
  deleteSCout
  connectStereo :avjack, 1,1
end

#PLACE BELOW WHERE YOU WANT SONIC PI TO CONNECT WHEN IT STARTS UP
#IF YOU HAVE MORE THAN ONE USB OR BLUETOOTH IT WILL CONNECT THE FIRST ONE IT FINDS
# choose from gohdmi, gousb, goavjack or gobluez (no audiojack on Pi5 or Pi400)
gohdmi
``` 
With the script in the init.rb file, then all of its commands will be available for use in Sonic Pi once it has booted, so as well as being able to change the default sound device specified in the last line of the init.rb file, you can change the audio output easily whist Sonic Pi is running. Merely choose one of `gohdmi`, `goavjack` `gousb` or `gobluez` and it will connect Sonic Pi to the relevant output, provided of course that it is available on your system. If not, there will be no audio output and you will have to connect to an existing device. Note if you have more than one device of the same type, eg two audio usb devices, the command will choose one of them depending upon which port id is discovered first. To differentiate you may have to use `qwpgraph` instead, although you could temporarily remove the one you don't want to connect to, run the gousb command, then plug it in again. The command `connectStereo` has three parameters the third controls whether a connection is made (1) or disconnected (0) so you can you to make a connection without disconnecting others, or disconnect a connection without affecting anything else using this. This allows connection to multiple devices, although this can produce scratchy noises if they are ill-matched in characteristics. You can also print details of the current port IDs using the command `displayCurrentID`
 
The Raspberry Pi volume control: This controls the volume of the device to which it is currently connected, (shown by right clicking its icon). To control the volume of a different audio device you must select it with this volume control first. Another thing to note is that the audio output of other applications may switch automatically to follow the current device selected by the volume control, whereas the switching of devices for Sonic Pi is independant of that. Also the volume control will only control the volume of the device it has currently selected. If you find this tedios to use you can install the package pavucontrol, which is designed to work usually with pulseaudio, but the output devices tab of this control will let you adjust the volume of any of the avaliable output devices

## Adjusting samplerate and buffer for scsynth via pipewire settings ##

New setings have been added to the audio_settings.toml file in the .sonic-pi/config folder in your hime directory This will be created on your first sonic pi run. YOu can edit ait and chnage the defauilt values in the Linux Audio-Pipewire settings from the default. In particular you may find that a Pi4 or Pi400 may work well on lower settings for linux_pipewire_buff_size maybe 512. A Pi5 may be ok on 256, but I don't have one to tst this yet! Most devices work at 48000 for the Linux_pipewire_sample rate but you could try 96000 perhaps. Both these settings will affect latency.

## Portable Sonic Pi

As mentioned previously the linux_dist folder (copied to the desktop and renamed) can run in any location. It is in fact the basis of the binary deb file created for this version. If you have another micro SD card with the same OS installed, you can fairly easily transfer a working copy of Sonic Pi to it. I suggest that you compress the folder by opening a terminal on the Desktop and typing:
```
tar -zcf mysonic-pi.tar.gz mysonic-pi
```
Then transfer it to your new sd card Desktop (could use a USB stick for this). Once there uncompress it
```
tar -zxf mysonic-pi.tar.gz
```
In order to get it to run you just need to install the set of runtime packages required on the second card. Although there seem to be more of them than for the build set of packages, they actually take up less space.
```
sudo apt install -y libqt6core6, libqt6dbus6, libqt6gui6, libqt6network6, libqt6opengl6, libqt6openglwidgets6, libqt6qml6, libqt6qmlmodels6, libqt6quick6, libqt6waylandclient6, libqt6waylandcompositor6, libqt6waylandeglclienthwintegration6, libqt6waylandeglcompositorhwintegration6, libqt6widgets6, libqt6wlshellintegration6, libqt6xml6, qt6-qpa-plugins, qt6-translations-l10n, qt6-wayland, qt6-gtk-platformtheme, ruby, supercollider-server, sc3-plugins-server, libspa-0.2-jack, pipewire-jack compton qwpgraph
```
You then should have a fully working second Sonic Pi

### Notes

* If you have any issues building Sonic Pi on Raspberry Pi OS please
  open up an issue on GitHub and we'll try our best to assist you:
  https://github.com/sonic-pi-net/sonic-pi/issues
* Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net
