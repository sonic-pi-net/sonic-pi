# Building Sonic Pi on macOS - the manual way

_Note, you will have to modify the paths to point to the appropriate place(s) on your machine - both in terms of your equivalent to `~` (`/Users/sam` in this file) and the versions of the dependencies you install should there be more recent releases._

1. Download the latest version of Sonic Pi and install to `/Applications`
2. Install the latest Qt (`5.12` at the time of writing) and install to a known location e.g. `~/Qt/5.12`
3. Create a new directory for the build e.g. `mkdir ~/sp-build`
4. Clone the latest Sonic Pi source code `git clone https://github.com/samaaron/sonic-pi.git ~/sp-build/sonic-pi`
5. Download the latest Qwt (`6.1.3` at the time of writing) and copy to `~/sp-build/qwt-6.1.3`
6. Download the latest QScintilla (`2.10.8` at the time of writing) and copy to `~/sp-build/QScintilla_gpl-2.10.8`
7. Download the latest SuperCollider source into the new build dir: 
  - `git clone https://github.com/supercollider/supercollider.git ~/sp-build/supercollider`
8. Copy the native files from Sonic Pi.app to your new build dir: 
  - `cp -R /Applications/Sonic\ Pi.app/app/server/native/* ~/sp-build/sonic-pi/app/server/native/`
9. Copy the erlang beam files from Sonic Pi.app to new build dir: 
  - `cp -R /Applications/Sonic\ Pi.app/app/server/erlang/* ~/sp-build/sonic-pi/app/server/erlang/`
10. Copy the Ruby native files from Sonic Pi.app to new build dir: 
  - `cp -R /Applications/Sonic\ Pi.app/app/server/ruby/rb-native ~/sp-build/sonic-pi/app/server/ruby`
11. Build qwt: 
   - `cd ~/sp-build/qwt-6.1.3 && ~/Qt/5.12/5.12.0/clang_64/bin/qmake && make`
12. Build QScintilla:
   - `cd ~/sp-build/QScintilla_gpl-2.10.8/Qt4Qt5 && ~/Qt/5.12/5.12.0/clang_64/bin/qmake && make`
13. Edit `~/sp-build/sonic-pi/app/gui/qt/mac-build-app` to include something like the following at the top: 
    (swap out `/Users/sam` with your `~`)
```
    QSCINTILLA_QT4QT5_DIR=/Users/sam/sp-build/QScintilla_gpl-2.10.8/Qt4Qt5
    QSCINTILLA_LIB=libqscintilla2_qt5.13.2.1.dylib
    QWT_DIR=/Users/sam/sp-build/qwt-6.1.3
    QT_DIR=/Users/sam/Qt/5.12/5.12.0
```
14. Edit `~/sp-build/sonic-pi/app/gui/qt/SonicPi.pro` and add the following at the top: 
    (swap out `/Users/sam` with your `~`)
```
    LIBS += -L/Users/sam/sp-build/supercollider/external_libraries/boost/libs
    INCLUDEPATH += /Users/sam/sp-build/supercollider/external_libraries/boost/
    DEPENDPATH += /Users/sam/sp-build/supercollider/external_libraries/boost/
     
    LIBS += -L/Users/sam/sp-build/QScintilla_gpl-2.10.8/Qt4Qt5
    INCLUDEPATH += /Users/sam/sp-build/QScintilla_gpl-2.10.8/Qt4Qt5
    DEPENDPATH += /Users/sam/sp-build/QScintilla_gpl-2.10.8/Qt4Qt5
    include ( /Users/sam/sp-build/qwt-6.1.3/qwt.prf )
```


15. Build app: 
  - `cd ~/sp-build/sonic-pi/app/gui/qt && ./mac-build-app`
16. Run app:
  - `cd ~/sp-build/sonic-pi/app/gui/qt && ./build/Sonic\ Pi.app/Contents/MacOS/Sonic\ Pi`  
17. Enjoy!  

   
