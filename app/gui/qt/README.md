# OS X

## Dev Build

* Download Qt 5.3.1+ http://qt-project.org/downloads
* Run the setup wizard and install to a known location which we'll call /path/to/qt
* Grab a copy of the QScintilla libs http://www.riverbankcomputing.co.uk/software/qscintilla/download and untar into a known location which we'll call /path/to/qscintilla
* Build QScintilla:
  - cd /path/to/qscintilla/Qt4Qt5 
  - generate makefile: /path/to/qt/5.3/clang_64/bin/qmake qscintilla.pro
  - make
* Add the following to SonicPi.pro
    LIBS += -L /path/to/qscintilla/Qt4Qt5/ -lqscintilla2
    INCLUDEPATH += /path/to/qscintilla/Qt4Qt5/
    DEPENDPATH += /path/to/qscintilla/Qt4Qt5/
* Modify top of mac-build-app appropriately i.e.
    QSCINTILLA=/path/to/qscintilla/Qt4Qt5
    QTBIN=/path/to/qt/5.3/clang_64/bin
* Run `./mac-build-app`
* App should be in `build` dir    





  
  
