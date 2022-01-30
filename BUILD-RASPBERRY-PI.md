# Building the Latest Sonic Pi on Raspberry Pi

Hello there lovely Raspberry Pi user - welcome to our build instructions.


### TLDR

If you're already familiar with the terminal and running shell scripts,
etc., you can fetch, build and start Sonic Pi with the following:

```
git clone https://github.com/sonic-pi-net/sonic-pi.git
cd sonic-pi/app
sudo ./pi-setup.sh
./pi-build-all.sh
./build/gui/qt/sonic-pi

```


### Installing vs Building

These instructions are for people wanting to build/compile their own
version of Sonic Pi. If you're just looking to install and run it you
can download our latest binary deb from the Sonic Pi website here:
https://sonic-pi.net/#rp

However, if you want to use the absolute latest development version or
get involved with modifying and changing the source code, you'll need to
build things yourself and hopefully this document will help you do just
that.

OK, so just to get you prepared, we're going to do a few things:
 
1. Clone Sonic Pi's source code using git.
2. Fetch all the development dependencies
3. Initiate the build by using a shell script.
4. Start your new Sonic Pi app.

All of these steps will require the terminal which can be found by
clicking on the Raspberry Pi logo and then Accessories -> Terminal.


### Notes

* If you have any issues building Sonic Pi on Raspberry Pi OS please
  open up an issue on GitHub and we'll try our best to assist you:
  https://github.com/sonic-pi-net/sonic-pi/issues
* These build instructions assume you're running the latest
  Bullseye-based Raspberry Pi OS released in November 2021. You may
  therefore need to update your distribution before continuing.


## 1. Get the Sonic Pi Source Code

The first thing we need to do is to grab a copy of Sonic Pi's source code.

The easiest way of getting this is to clone from GitHub
into a local folder such as `~/Development/sonic-pi`:

```
git clone https://github.com/sonic-pi-net/sonic-pi.git ~/Development/sonic-pi
``` 

By default this will check out the `dev` branch which contains all the
most recent development work. If you'd like the latest stable release
you'll need to checkout the `stable` branch with `cd
~/Development/sonic-pi && git checkout stable`

From now on these instructions will assume you're in the `sonic-pi`
directory. For example, if you cloned into `~/Development/sonic-pi` you
can change into this directory with:

```
cd ~/Development/sonic-pi
```


## 2. Fetch the Development Dependencies

Now we're ready to fetch all the development dependencies. You can do
this by running the following script with `sudo` (this is necessary as
it calls `apt-get` to install your packages)

```
sudo ./app/pi-setup.sh
```


## 3. Running the Build

Now we just need to build everything. This is achieved with the
following command (which will likely take some time):

```
./app/pi-build-all.sh
```


## 4. Start Sonic Pi

Finally, you can run your newly compiled Sonic Pi app with the following command:

```
./app/build/gui/qt/sonic-pi
```

Good luck and please share your new live coding adventure with us over on:

https://in-thread.sonic-pi.net

