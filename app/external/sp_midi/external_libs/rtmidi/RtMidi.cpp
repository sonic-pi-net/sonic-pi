/**********************************************************************/
/*! \class RtMidi
    \brief An abstract base class for realtime MIDI input/output.

    This class implements some common functionality for the realtime
    MIDI input/output subclasses RtMidiIn and RtMidiOut.

    RtMidi GitHub site: https://github.com/thestk/rtmidi
    RtMidi WWW site: http://www.music.mcgill.ca/~gary/rtmidi/

    RtMidi: realtime MIDI i/o C++ classes
    Copyright (c) 2003-2023 Gary P. Scavone

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation files
    (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    Any person wishing to distribute modifications to the Software is
    asked to send the modifications to the original developer so that
    they can be incorporated into the canonical version.  This is,
    however, not a binding provision of this license.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**********************************************************************/

#include "RtMidi.h"
#include <sstream>
#if defined(__APPLE__)
#include <TargetConditionals.h>
#endif

#if (TARGET_OS_IPHONE == 1)

    #define AudioGetCurrentHostTime CAHostTimeBase::GetCurrentTime
    #define AudioConvertHostTimeToNanos CAHostTimeBase::ConvertToNanos

    #include <mach/mach_time.h>
    class CTime2nsFactor
    {
    public:
        CTime2nsFactor()
        {
            mach_timebase_info_data_t tinfo;
            mach_timebase_info(&tinfo);
            Factor = (double)tinfo.numer / tinfo.denom;
        }
        static double Factor;
    };
    double CTime2nsFactor::Factor;
    static CTime2nsFactor InitTime2nsFactor;
    #undef AudioGetCurrentHostTime
    #undef AudioConvertHostTimeToNanos
  #define AudioGetCurrentHostTime (uint64_t) mach_absolute_time
  #define AudioConvertHostTimeToNanos(t) t *CTime2nsFactor::Factor
  #define EndianS32_BtoN(n) n

#endif

// Default for Windows is to add an identifier to the port names; this
// flag can be defined (e.g. in your project file) to disable this behaviour.
//#define RTMIDI_DO_NOT_ENSURE_UNIQUE_PORTNAMES

// Default for Windows UWP is to enable a workaround to fix BLE-MIDI IN ports'
// wrong timestamps that occur at least in Windows 10 21H2;
// this flag can be defined (e.g. in your project file)
// to disable this behavior.
//#define RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS

// **************************************************************** //
//
// MidiInApi and MidiOutApi subclass prototypes.
//
// **************************************************************** //

#if !defined(__LINUX_ALSA__) && !defined(__UNIX_JACK__) && !defined(__MACOSX_CORE__) && !defined(__WINDOWS_MM__) && !defined(__WINDOWS_UWP__) && !defined(TARGET_IPHONE_OS) && !defined(__WEB_MIDI_API__)  && !defined(__AMIDI__)
  #define __RTMIDI_DUMMY__
#endif

#if defined(__MACOSX_CORE__)
#include <CoreMIDI/CoreMIDI.h>

class MidiInCore: public MidiInApi
{
 public:
  MidiInCore( const std::string &clientName, unsigned int queueSizeLimit );
  ~MidiInCore( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::MACOSX_CORE; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

 protected:
  MIDIClientRef getCoreMidiClientSingleton(const std::string& clientName) throw();
  void initialize( const std::string& clientName );
};

class MidiOutCore: public MidiOutApi
{
 public:
  MidiOutCore( const std::string &clientName );
  ~MidiOutCore( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::MACOSX_CORE; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

 protected:
  MIDIClientRef getCoreMidiClientSingleton(const std::string& clientName) throw();
  void initialize( const std::string& clientName );
};

#endif

#if defined(__UNIX_JACK__)

class MidiInJack: public MidiInApi
{
 public:
  MidiInJack( const std::string &clientName, unsigned int queueSizeLimit );
  ~MidiInJack( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::UNIX_JACK; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName);
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

 protected:
  std::string clientName;

  void connect( void );
  void initialize( const std::string& clientName );
};

class MidiOutJack: public MidiOutApi
{
 public:
  MidiOutJack( const std::string &clientName );
  ~MidiOutJack( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::UNIX_JACK; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName);
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

 protected:
  std::string clientName;

  void connect( void );
  void initialize( const std::string& clientName );
};

#endif

#if defined(__LINUX_ALSA__)

class MidiInAlsa: public MidiInApi
{
 public:
  MidiInAlsa( const std::string &clientName, unsigned int queueSizeLimit );
  ~MidiInAlsa( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::LINUX_ALSA; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName);
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

 protected:
  void initialize( const std::string& clientName );
};

class MidiOutAlsa: public MidiOutApi
{
 public:
  MidiOutAlsa( const std::string &clientName );
  ~MidiOutAlsa( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::LINUX_ALSA; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

 protected:
  void initialize( const std::string& clientName );
};

#endif

#if defined(__WINDOWS_MM__)

class MidiInWinMM: public MidiInApi
{
 public:
  MidiInWinMM( const std::string &clientName, unsigned int queueSizeLimit );
  ~MidiInWinMM( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::WINDOWS_MM; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

 protected:
  void initialize( const std::string& clientName );
};

class MidiOutWinMM: public MidiOutApi
{
 public:
  MidiOutWinMM( const std::string &clientName );
  ~MidiOutWinMM( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::WINDOWS_MM; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

 protected:
  void initialize( const std::string& clientName );
};

#endif

#if defined(__WINDOWS_UWP__)

class MidiInWinUWP : public MidiInApi
{
public:
    MidiInWinUWP(const std::string& clientName, unsigned int queueSizeLimit);
    ~MidiInWinUWP(void) override;
    RtMidi::Api getCurrentApi(void) override { return RtMidi::WINDOWS_UWP; };
    void openPort(unsigned int portNumber, const std::string& portName) override;
    void openVirtualPort(const std::string& portName) override;
    void closePort(void) override;
    void setClientName(const std::string& clientName) override;
    void setPortName(const std::string& portName) override;
    unsigned int getPortCount(void) override;
    std::string getPortName(unsigned int portNumber) override;
    double getMessage(std::vector<unsigned char>* message) override;

protected:
    void initialize(const std::string& clientName) override;
};

class MidiOutWinUWP : public MidiOutApi
{
public:
    MidiOutWinUWP(const std::string& clientName);
    ~MidiOutWinUWP(void) override;
    RtMidi::Api getCurrentApi(void) override { return RtMidi::WINDOWS_UWP; };
    void openPort(unsigned int portNumber, const std::string& portName) override;
    void openVirtualPort(const std::string& portName) override;
    void closePort(void) override;
    void setClientName(const std::string& clientName) override;
    void setPortName(const std::string& portName) override;
    unsigned int getPortCount(void) override;
    std::string getPortName(unsigned int portNumber) override;
    void sendMessage(const unsigned char* message, size_t size) override;

protected:
    void initialize(const std::string& clientName) override;
};

#endif

#if defined(__WEB_MIDI_API__)

class MidiInWeb : public MidiInApi
{
  std::string client_name{};
  std::string web_midi_id{};
  int open_port_number{-1};

 public:
  MidiInWeb(const std::string &/*clientName*/, unsigned int queueSizeLimit );
  ~MidiInWeb( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::WEB_MIDI_API; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

  void onMidiMessage( uint8_t* data, double domHishResTimeStamp );

 protected:
  void initialize( const std::string& clientName );
};

class MidiOutWeb: public MidiOutApi
{
  std::string client_name{};
  std::string web_midi_id{};
  int open_port_number{-1};

 public:
  MidiOutWeb( const std::string &clientName );
  ~MidiOutWeb( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::WEB_MIDI_API; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

 protected:
  void initialize( const std::string& clientName );
};

#endif

#if defined(__AMIDI__)

#define LOG_TAG "RtMidi"
#include <amidi/AMidi.h>
#include <android/log.h>
#include <pthread.h>
#include <atomic>
#include <string>
#include <vector>
#include <jni.h>
#include <unistd.h>

class MidiInAndroid : public MidiInApi
{
 public:
  MidiInAndroid(const std::string &/*clientName*/, unsigned int queueSizeLimit );
  ~MidiInAndroid( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::ANDROID_AMIDI; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );

  void onMidiMessage( uint8_t* data, double domHishResTimeStamp );

  void initialize( const std::string& clientName );
  void connect();
  AMidiDevice* receiveDevice = NULL;
  AMidiOutputPort* midiOutputPort = NULL;
  pthread_t readThread;
  std::atomic<bool> reading = ATOMIC_VAR_INIT(false);
  static void* pollMidi(void* context);
  double lastTime;
};

class MidiOutAndroid: public MidiOutApi
{
 public:
  MidiOutAndroid( const std::string &clientName );
  ~MidiOutAndroid( void );
  RtMidi::Api getCurrentApi( void ) { return RtMidi::ANDROID_AMIDI; };
  void openPort( unsigned int portNumber, const std::string &portName );
  void openVirtualPort( const std::string &portName );
  void closePort( void );
  void setClientName( const std::string &clientName );
  void setPortName( const std::string &portName );
  unsigned int getPortCount( void );
  std::string getPortName( unsigned int portNumber );
  void sendMessage( const unsigned char *message, size_t size );

  void initialize( const std::string& clientName );
  void connect();
  AMidiDevice* sendDevice = NULL;
  AMidiInputPort* midiInputPort = NULL;
};

#endif

#if defined(__RTMIDI_DUMMY__)

class MidiInDummy: public MidiInApi
{
 public:
 MidiInDummy( const std::string &/*clientName*/, unsigned int queueSizeLimit ) : MidiInApi( queueSizeLimit ) { errorString_ = "MidiInDummy: This class provides no functionality."; error( RtMidiError::WARNING, errorString_ ); }
  RtMidi::Api getCurrentApi( void ) { return RtMidi::RTMIDI_DUMMY; }
  void openPort( unsigned int /*portNumber*/, const std::string &/*portName*/ ) {}
  void openVirtualPort( const std::string &/*portName*/ ) {}
  void closePort( void ) {}
  void setClientName( const std::string &/*clientName*/ ) {};
  void setPortName( const std::string &/*portName*/ ) {};
  unsigned int getPortCount( void ) { return 0; }
  std::string getPortName( unsigned int /*portNumber*/ ) { return ""; }

 protected:
  void initialize( const std::string& /*clientName*/ ) {}
};

class MidiOutDummy: public MidiOutApi
{
 public:
  MidiOutDummy( const std::string &/*clientName*/ ) { errorString_ = "MidiOutDummy: This class provides no functionality."; error( RtMidiError::WARNING, errorString_ ); }
  RtMidi::Api getCurrentApi( void ) { return RtMidi::RTMIDI_DUMMY; }
  void openPort( unsigned int /*portNumber*/, const std::string &/*portName*/ ) {}
  void openVirtualPort( const std::string &/*portName*/ ) {}
  void closePort( void ) {}
  void setClientName( const std::string &/*clientName*/ ) {};
  void setPortName( const std::string &/*portName*/ ) {};
  unsigned int getPortCount( void ) { return 0; }
  std::string getPortName( unsigned int /*portNumber*/ ) { return ""; }
  void sendMessage( const unsigned char * /*message*/, size_t /*size*/ ) {}

 protected:
  void initialize( const std::string& /*clientName*/ ) {}
};

#endif

//*********************************************************************//
//  RtMidi Definitions
//*********************************************************************//

RtMidi :: RtMidi()
  : rtapi_(0)
{
}

RtMidi :: ~RtMidi()
{
  delete rtapi_;
  rtapi_ = 0;
}

RtMidi::RtMidi(RtMidi&& other) noexcept {
    rtapi_ = other.rtapi_;
    other.rtapi_ = nullptr;
}

std::string RtMidi :: getVersion( void ) throw()
{
  return std::string( RTMIDI_VERSION );
}

// Define API names and display names.
// Must be in same order as API enum.
extern "C" {
const char* rtmidi_api_names[][2] = {
  { "unspecified" , "Unknown" },
  { "core"        , "CoreMidi" },
  { "alsa"        , "ALSA" },
  { "jack"        , "Jack" },
  { "winmm"       , "Windows MultiMedia" },
  { "dummy"       , "Dummy" },
  { "web"         , "Web MIDI API" },
  { "winuwp"      , "Windows UWP" },
  { "amidi"       , "Android MIDI API" },
};
const unsigned int rtmidi_num_api_names =
  sizeof(rtmidi_api_names)/sizeof(rtmidi_api_names[0]);

// The order here will control the order of RtMidi's API search in
// the constructor.
extern "C" const RtMidi::Api rtmidi_compiled_apis[] = {
#if defined(__MACOSX_CORE__)
  RtMidi::MACOSX_CORE,
#endif
#if defined(__LINUX_ALSA__)
  RtMidi::LINUX_ALSA,
#endif
#if defined(__UNIX_JACK__)
  RtMidi::UNIX_JACK,
#endif
#if defined(__WINDOWS_MM__)
  RtMidi::WINDOWS_MM,
#endif
#if defined(__WINDOWS_UWP__)
  RtMidi::WINDOWS_UWP,
#endif
#if defined(__WEB_MIDI_API__)
  RtMidi::WEB_MIDI_API,
#endif
#if defined(__WEB_MIDI_API__)
  RtMidi::WEB_MIDI_API,
#endif
#if defined(__AMIDI__)
  RtMidi::ANDROID_AMIDI,
#endif
  RtMidi::UNSPECIFIED,
};
extern "C" const unsigned int rtmidi_num_compiled_apis =
  sizeof(rtmidi_compiled_apis)/sizeof(rtmidi_compiled_apis[0])-1;
}

// This is a compile-time check that rtmidi_num_api_names == RtMidi::NUM_APIS.
// If the build breaks here, check that they match.
template<bool b> class StaticAssert { private: StaticAssert() {} };
template<> class StaticAssert<true>{ public: StaticAssert() {} };
class StaticAssertions { StaticAssertions() {
  StaticAssert<rtmidi_num_api_names == RtMidi::NUM_APIS>();
}};

void RtMidi :: getCompiledApi( std::vector<RtMidi::Api> &apis ) throw()
{
  apis = std::vector<RtMidi::Api>(rtmidi_compiled_apis,
                                  rtmidi_compiled_apis + rtmidi_num_compiled_apis);
}

std::string RtMidi :: getApiName( RtMidi::Api api )
{
  if (api < RtMidi::UNSPECIFIED || api >= RtMidi::NUM_APIS)
    return "";
  return rtmidi_api_names[api][0];
}

std::string RtMidi :: getApiDisplayName( RtMidi::Api api )
{
  if (api < RtMidi::UNSPECIFIED || api >= RtMidi::NUM_APIS)
    return "Unknown";
  return rtmidi_api_names[api][1];
}

RtMidi::Api RtMidi :: getCompiledApiByName( const std::string &name )
{
  unsigned int i=0;
  for (i = 0; i < rtmidi_num_compiled_apis; ++i)
    if (name == rtmidi_api_names[rtmidi_compiled_apis[i]][0])
      return rtmidi_compiled_apis[i];
  return RtMidi::UNSPECIFIED;
}

void RtMidi :: setClientName( const std::string &clientName )
{
  rtapi_->setClientName( clientName );
}

void RtMidi :: setPortName( const std::string &portName )
{
  rtapi_->setPortName( portName );
}


//*********************************************************************//
//  RtMidiIn Definitions
//*********************************************************************//

void RtMidiIn :: openMidiApi( RtMidi::Api api, const std::string &clientName, unsigned int queueSizeLimit )
{
  delete rtapi_;
  rtapi_ = 0;

#if defined(__UNIX_JACK__)
  if ( api == UNIX_JACK )
    rtapi_ = new MidiInJack( clientName, queueSizeLimit );
#endif
#if defined(__LINUX_ALSA__)
  if ( api == LINUX_ALSA )
    rtapi_ = new MidiInAlsa( clientName, queueSizeLimit );
#endif
#if defined(__WINDOWS_MM__)
  if ( api == WINDOWS_MM )
    rtapi_ = new MidiInWinMM( clientName, queueSizeLimit );
#endif
#if defined(__WINDOWS_UWP__)
  if (api == WINDOWS_UWP)
      rtapi_ = new MidiInWinUWP(clientName, queueSizeLimit);
#endif
#if defined(__MACOSX_CORE__)
  if ( api == MACOSX_CORE )
    rtapi_ = new MidiInCore( clientName, queueSizeLimit );
#endif
#if defined(__WEB_MIDI_API__)
    if ( api == WEB_MIDI_API )
    rtapi_ = new MidiInWeb( clientName, queueSizeLimit );
#endif
#if defined(__AMIDI__)
    if ( api == ANDROID_AMIDI )
    rtapi_ = new MidiInAndroid( clientName, queueSizeLimit );
#endif
#if defined(__RTMIDI_DUMMY__)
  if ( api == RTMIDI_DUMMY )
    rtapi_ = new MidiInDummy( clientName, queueSizeLimit );
#endif
}

RTMIDI_DLL_PUBLIC RtMidiIn :: RtMidiIn( RtMidi::Api api, const std::string &clientName, unsigned int queueSizeLimit )
  : RtMidi()
{
  if ( api != UNSPECIFIED ) {
    // Attempt to open the specified API.
    openMidiApi( api, clientName, queueSizeLimit );
    if ( rtapi_ ) return;

    // No compiled support for specified API value.  Issue a warning
    // and continue as if no API was specified.
    std::cerr << "\nRtMidiIn: no compiled support for specified API argument!\n\n" << std::endl;
  }

  // Iterate through the compiled APIs and return as soon as we find
  // one with at least one port or we reach the end of the list.
  std::vector< RtMidi::Api > apis;
  getCompiledApi( apis );
  for ( unsigned int i=0; i<apis.size(); i++ ) {
    openMidiApi( apis[i], clientName, queueSizeLimit );
    if ( rtapi_ && rtapi_->getPortCount() ) break;
  }

  if ( rtapi_ ) return;

  // It should not be possible to get here because the preprocessor
  // definition __RTMIDI_DUMMY__ is automatically defined if no
  // API-specific definitions are passed to the compiler. But just in
  // case something weird happens, we'll throw an error.
  std::string errorText = "RtMidiIn: no compiled API support found ... critical error!!";
  throw( RtMidiError( errorText, RtMidiError::UNSPECIFIED ) );
}

RtMidiIn :: ~RtMidiIn() throw()
{
}


//*********************************************************************//
//  RtMidiOut Definitions
//*********************************************************************//

void RtMidiOut :: openMidiApi( RtMidi::Api api, const std::string &clientName )
{
  delete rtapi_;
  rtapi_ = 0;

#if defined(__UNIX_JACK__)
  if ( api == UNIX_JACK )
    rtapi_ = new MidiOutJack( clientName );
#endif
#if defined(__LINUX_ALSA__)
  if ( api == LINUX_ALSA )
    rtapi_ = new MidiOutAlsa( clientName );
#endif
#if defined(__WINDOWS_MM__)
  if ( api == WINDOWS_MM )
    rtapi_ = new MidiOutWinMM( clientName );
#endif
#if defined(__WINDOWS_UWP__)
  if (api == WINDOWS_UWP)
      rtapi_ = new MidiOutWinUWP(clientName);
#endif
#if defined(__MACOSX_CORE__)
  if ( api == MACOSX_CORE )
    rtapi_ = new MidiOutCore( clientName );
#endif
#if defined(__WEB_MIDI_API__)
    if ( api == WEB_MIDI_API )
    rtapi_ = new MidiOutWeb( clientName );
#endif
#if defined(__AMIDI__)
    if ( api == ANDROID_AMIDI )
    rtapi_ = new MidiOutAndroid( clientName );
#endif
#if defined(__RTMIDI_DUMMY__)
  if ( api == RTMIDI_DUMMY )
    rtapi_ = new MidiOutDummy( clientName );
#endif
}

RTMIDI_DLL_PUBLIC RtMidiOut :: RtMidiOut( RtMidi::Api api, const std::string &clientName)
{
  if ( api != UNSPECIFIED ) {
    // Attempt to open the specified API.
    openMidiApi( api, clientName );
    if ( rtapi_ ) return;

    // No compiled support for specified API value.  Issue a warning
    // and continue as if no API was specified.
    std::cerr << "\nRtMidiOut: no compiled support for specified API argument!\n\n" << std::endl;
  }

  // Iterate through the compiled APIs and return as soon as we find
  // one with at least one port or we reach the end of the list.
  std::vector< RtMidi::Api > apis;
  getCompiledApi( apis );
  for ( unsigned int i=0; i<apis.size(); i++ ) {
    openMidiApi( apis[i], clientName );
    if ( rtapi_ && rtapi_->getPortCount() ) break;
  }

  if ( rtapi_ ) return;

  // It should not be possible to get here because the preprocessor
  // definition __RTMIDI_DUMMY__ is automatically defined if no
  // API-specific definitions are passed to the compiler. But just in
  // case something weird happens, we'll thrown an error.
  std::string errorText = "RtMidiOut: no compiled API support found ... critical error!!";
  throw( RtMidiError( errorText, RtMidiError::UNSPECIFIED ) );
}

RtMidiOut :: ~RtMidiOut() throw()
{
}

//*********************************************************************//
//  Common MidiApi Definitions
//*********************************************************************//

MidiApi :: MidiApi( void )
  : apiData_( 0 ), connected_( false ), errorCallback_(0), firstErrorOccurred_(false), errorCallbackUserData_(0)
{
}

MidiApi :: ~MidiApi( void )
{
}

void MidiApi :: setErrorCallback( RtMidiErrorCallback errorCallback, void *userData = 0 )
{
    errorCallback_ = errorCallback;
    errorCallbackUserData_ = userData;
}

void MidiApi :: error( RtMidiError::Type type, std::string errorString )
{
  if ( errorCallback_ ) {

    if ( firstErrorOccurred_ )
      return;

    firstErrorOccurred_ = true;
    const std::string errorMessage = errorString;

    errorCallback_( type, errorMessage, errorCallbackUserData_ );
    firstErrorOccurred_ = false;
    return;
  }

  if ( type == RtMidiError::WARNING ) {
    std::cerr << '\n' << errorString << "\n\n";
  }
  else if ( type == RtMidiError::DEBUG_WARNING ) {
#if defined(__RTMIDI_DEBUG__)
    std::cerr << '\n' << errorString << "\n\n";
#endif
  }
  else {
    std::cerr << '\n' << errorString << "\n\n";
    throw RtMidiError( errorString, type );
  }
}

//*********************************************************************//
//  Common MidiInApi Definitions
//*********************************************************************//

MidiInApi :: MidiInApi( unsigned int queueSizeLimit )
  : MidiApi()
{
  // Allocate the MIDI queue.
  inputData_.queue.ringSize = queueSizeLimit;
  if ( inputData_.queue.ringSize > 0 )
    inputData_.queue.ring = new MidiMessage[ inputData_.queue.ringSize ];
}

MidiInApi :: ~MidiInApi( void )
{
  // Delete the MIDI queue.
  if ( inputData_.queue.ringSize > 0 ) delete [] inputData_.queue.ring;
}

void MidiInApi :: setCallback( RtMidiIn::RtMidiCallback callback, void *userData )
{
  if ( inputData_.usingCallback ) {
    errorString_ = "MidiInApi::setCallback: a callback function is already set!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  if ( !callback ) {
    errorString_ = "RtMidiIn::setCallback: callback function value is invalid!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  inputData_.userCallback = callback;
  inputData_.userData = userData;
  inputData_.usingCallback = true;
}

void MidiInApi :: cancelCallback()
{
  if ( !inputData_.usingCallback ) {
    errorString_ = "RtMidiIn::cancelCallback: no callback function was set!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  inputData_.userCallback = 0;
  inputData_.userData = 0;
  inputData_.usingCallback = false;
}

void MidiInApi :: ignoreTypes( bool midiSysex, bool midiTime, bool midiSense )
{
  inputData_.ignoreFlags = 0;
  if ( midiSysex ) inputData_.ignoreFlags = 0x01;
  if ( midiTime ) inputData_.ignoreFlags |= 0x02;
  if ( midiSense ) inputData_.ignoreFlags |= 0x04;
}

double MidiInApi :: getMessage( std::vector<unsigned char> *message )
{
  message->clear();

  if ( inputData_.usingCallback ) {
    errorString_ = "RtMidiIn::getNextMessage: a user callback is currently set for this port.";
    error( RtMidiError::WARNING, errorString_ );
    return 0.0;
  }

  double timeStamp;
  if ( !inputData_.queue.pop( message, &timeStamp ) )
    return 0.0;

  return timeStamp;
}

void MidiInApi :: setBufferSize( unsigned int size, unsigned int count )
{
    inputData_.bufferSize = size;
    inputData_.bufferCount = count;
}

unsigned int MidiInApi::MidiQueue::size( unsigned int *__back,
                                         unsigned int *__front )
{
  // Access back/front members exactly once and make stack copies for
  // size calculation
  unsigned int _back = back, _front = front, _size;
  if ( _back >= _front )
    _size = _back - _front;
  else
    _size = ringSize - _front + _back;

  // Return copies of back/front so no new and unsynchronized accesses
  // to member variables are needed.
  if ( __back ) *__back = _back;
  if ( __front ) *__front = _front;
  return _size;
}

// As long as we haven't reached our queue size limit, push the message.
bool MidiInApi::MidiQueue::push( const MidiInApi::MidiMessage& msg )
{
  // Local stack copies of front/back
  unsigned int _back, _front, _size;

  // Get back/front indexes exactly once and calculate current size
  _size = size( &_back, &_front );

  if ( _size < ringSize-1 )
  {
    ring[_back] = msg;
    back = (back+1)%ringSize;
    return true;
  }

  return false;
}

bool MidiInApi::MidiQueue::pop( std::vector<unsigned char> *msg, double* timeStamp )
{
  // Local stack copies of front/back
  unsigned int _back, _front, _size;

  // Get back/front indexes exactly once and calculate current size
  _size = size( &_back, &_front );

  if ( _size == 0 )
    return false;

  // Copy queued message to the vector pointer argument and then "pop" it.
  msg->assign( ring[_front].bytes.begin(), ring[_front].bytes.end() );
  *timeStamp = ring[_front].timeStamp;

  // Update front
  front = (front+1)%ringSize;
  return true;
}

//*********************************************************************//
//  Common MidiOutApi Definitions
//*********************************************************************//

MidiOutApi :: MidiOutApi( void )
  : MidiApi()
{
}

MidiOutApi :: ~MidiOutApi( void )
{
}

// *************************************************** //
//
// OS/API-specific methods.
//
// *************************************************** //

#if defined(__MACOSX_CORE__)

// The CoreMIDI API is based on the use of a callback function for
// MIDI input.  We convert the system specific time stamps to delta
// time values.

// These are not available on iOS.
#if (TARGET_OS_IPHONE == 0)
  #include <CoreAudio/HostTime.h>
  #include <CoreServices/CoreServices.h>
#endif

// A structure to hold variables related to the CoreMIDI API
// implementation.
struct CoreMidiData {
  MIDIClientRef client;
  MIDIPortRef port;
  MIDIEndpointRef endpoint;
  MIDIEndpointRef destinationId;
  unsigned long long lastTime;
  MIDISysexSendRequest sysexreq;
};

static MIDIClientRef CoreMidiClientSingleton = 0;

void RtMidi_setCoreMidiClientSingleton(MIDIClientRef client){
  CoreMidiClientSingleton = client;
}

void RtMidi_disposeCoreMidiClientSingleton(){
  if (CoreMidiClientSingleton == 0){
    return;
  }
  MIDIClientDispose( CoreMidiClientSingleton );
  CoreMidiClientSingleton = 0;
}

//*********************************************************************//
//  API: OS-X
//  Class Definitions: MidiInCore
//*********************************************************************//

static void midiInputCallback( const MIDIPacketList *list, void *procRef, void */*srcRef*/ )
{
  MidiInApi::RtMidiInData *data = static_cast<MidiInApi::RtMidiInData *> (procRef);
  CoreMidiData *apiData = static_cast<CoreMidiData *> (data->apiData);

  unsigned char status;
  unsigned short nBytes, iByte, size;
  unsigned long long time;

  bool& continueSysex = data->continueSysex;
  MidiInApi::MidiMessage& message = data->message;

  const MIDIPacket *packet = &list->packet[0];
  for ( unsigned int i=0; i<list->numPackets; ++i ) {

    // My interpretation of the CoreMIDI documentation: all message
    // types, except sysex, are complete within a packet and there may
    // be several of them in a single packet.  Sysex messages can be
    // broken across multiple packets and PacketLists but are bundled
    // alone within each packet (these packets do not contain other
    // message types).  If sysex messages are split across multiple
    // MIDIPacketLists, they must be handled by multiple calls to this
    // function.

    nBytes = packet->length;
    if ( nBytes == 0 ) {
      packet = MIDIPacketNext( packet );
      continue;
    }

    // Calculate time stamp.
    if ( data->firstMessage ) {
      message.timeStamp = 0.0;
      data->firstMessage = false;
    }
    else {
      time = packet->timeStamp;
      if ( time == 0 ) { // this happens when receiving asynchronous sysex messages
        time = AudioGetCurrentHostTime();
      }
      time -= apiData->lastTime;
      time = AudioConvertHostTimeToNanos( time );
      if ( !continueSysex )
        message.timeStamp = time * 0.000000001;
    }

    // Track whether any non-filtered messages were found in this
    // packet for timestamp calculation
    bool foundNonFiltered = false;

    iByte = 0;
    if ( continueSysex ) {
      // We have a continuing, segmented sysex message.
      if ( !( data->ignoreFlags & 0x01 ) ) {
        // If we're not ignoring sysex messages, copy the entire packet.
        for ( unsigned int j=0; j<nBytes; ++j )
          message.bytes.push_back( packet->data[j] );
      }
      continueSysex = packet->data[nBytes-1] != 0xF7;

      if ( !( data->ignoreFlags & 0x01 ) && !continueSysex ) {
        // If not a continuing sysex message, invoke the user callback function or queue the message.
        if ( data->usingCallback ) {
          RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) data->userCallback;
          callback( message.timeStamp, &message.bytes, data->userData );
        }
        else {
          // As long as we haven't reached our queue size limit, push the message.
          if ( !data->queue.push( message ) )
            std::cerr << "\nMidiInCore: message queue limit reached!!\n\n";
        }
        message.bytes.clear();
      }
    }
    else {
      while ( iByte < nBytes ) {
        size = 0;
        // We are expecting that the next byte in the packet is a status byte.
        status = packet->data[iByte];
        if ( !(status & 0x80) ) break;
        // Determine the number of bytes in the MIDI message.
        if ( status < 0xC0 ) size = 3;
        else if ( status < 0xE0 ) size = 2;
        else if ( status < 0xF0 ) size = 3;
        else if ( status == 0xF0 ) {
          // A MIDI sysex
          if ( data->ignoreFlags & 0x01 ) {
            size = 0;
            iByte = nBytes;
          }
          else size = nBytes - iByte;
          continueSysex = packet->data[nBytes-1] != 0xF7;
        }
        else if ( status == 0xF1 ) {
          // A MIDI time code message
          if ( data->ignoreFlags & 0x02 ) {
            size = 0;
            iByte += 2;
          }
          else size = 2;
        }
        else if ( status == 0xF2 ) size = 3;
        else if ( status == 0xF3 ) size = 2;
        else if ( status == 0xF8 && ( data->ignoreFlags & 0x02 ) ) {
          // A MIDI timing tick message and we're ignoring it.
          size = 0;
          iByte += 1;
        }
        else if ( status == 0xFE && ( data->ignoreFlags & 0x04 ) ) {
          // A MIDI active sensing message and we're ignoring it.
          size = 0;
          iByte += 1;
        }
        else size = 1;

        // Copy the MIDI data to our vector.
        if ( size ) {
          foundNonFiltered = true;
          message.bytes.assign( &packet->data[iByte], &packet->data[iByte+size] );
          if ( !continueSysex ) {
            // If not a continuing sysex message, invoke the user callback function or queue the message.
            if ( data->usingCallback ) {
              RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) data->userCallback;
              callback( message.timeStamp, &message.bytes, data->userData );
            }
            else {
              // As long as we haven't reached our queue size limit, push the message.
              if ( !data->queue.push( message ) )
                std::cerr << "\nMidiInCore: message queue limit reached!!\n\n";
            }
            message.bytes.clear();
          }
          iByte += size;
        }
      }
    }

    // Save the time of the last non-filtered message
    if ( foundNonFiltered ) {
      apiData->lastTime = packet->timeStamp;
      if ( apiData->lastTime == 0 ) { // this happens when receiving asynchronous sysex messages
        apiData->lastTime = AudioGetCurrentHostTime();
      }
    }

    packet = MIDIPacketNext(packet);
  }
}

MidiInCore :: MidiInCore( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit )
{
  MidiInCore::initialize( clientName );
}

MidiInCore :: ~MidiInCore( void )
{
  // Close a connection if it exists.
  MidiInCore::closePort();

  // Cleanup.
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);
  if ( data->endpoint ) MIDIEndpointDispose( data->endpoint );
  delete data;
}

MIDIClientRef MidiInCore::getCoreMidiClientSingleton(const std::string& clientName) throw() {

  if (CoreMidiClientSingleton == 0){
      // Set up our client.
      MIDIClientRef client;

      CFStringRef name = CFStringCreateWithCString( NULL, clientName.c_str(), kCFStringEncodingASCII );
      OSStatus result = MIDIClientCreate(name, NULL, NULL, &client );
      if ( result != noErr ) {
        std::ostringstream ost;
        ost << "MidiInCore::initialize: error creating OS-X MIDI client object (" << result << ").";
        errorString_ = ost.str();
        error( RtMidiError::DRIVER_ERROR, errorString_ );
        return 0;
      }
      CFRelease( name );

      CoreMidiClientSingleton = client;
  }

  return CoreMidiClientSingleton;
}

void MidiInCore :: initialize( const std::string& clientName )
{
  // Set up our client.
  MIDIClientRef client = getCoreMidiClientSingleton(clientName);

  // Save our api-specific connection information.
  CoreMidiData *data = (CoreMidiData *) new CoreMidiData;
  data->client = client;
  data->endpoint = 0;
  apiData_ = (void *) data;
  inputData_.apiData = (void *) data;
}

void MidiInCore :: openPort( unsigned int portNumber, const std::string &portName )
{
  if ( connected_ ) {
    errorString_ = "MidiInCore::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  unsigned int nSrc = MIDIGetNumberOfSources();
  if ( nSrc < 1 ) {
    errorString_ = "MidiInCore::openPort: no MIDI input sources found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  if ( portNumber >= nSrc ) {
    std::ostringstream ost;
    ost << "MidiInCore::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  MIDIPortRef port;
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);
  CFStringRef portNameRef = CFStringCreateWithCString( NULL, portName.c_str(), kCFStringEncodingASCII );
  OSStatus result = MIDIInputPortCreate( data->client,
                                         portNameRef,
                                         midiInputCallback, (void *)&inputData_, &port );
  CFRelease( portNameRef );

  if ( result != noErr ) {
    errorString_ = "MidiInCore::openPort: error creating OS-X MIDI input port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Get the desired input source identifier.
  MIDIEndpointRef endpoint = MIDIGetSource( portNumber );
  if ( endpoint == 0 ) {
    MIDIPortDispose( port );
    errorString_ = "MidiInCore::openPort: error getting MIDI input source reference.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Make the connection.
  result = MIDIPortConnectSource( port, endpoint, NULL );
  if ( result != noErr ) {
    MIDIPortDispose( port );
    errorString_ = "MidiInCore::openPort: error connecting OS-X MIDI input port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Save our api-specific port information.
  data->port = port;

  connected_ = true;
}

void MidiInCore :: openVirtualPort( const std::string &portName )
{
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);

  // Create a virtual MIDI input destination.
  MIDIEndpointRef endpoint;
  CFStringRef portNameRef = CFStringCreateWithCString( NULL, portName.c_str(), kCFStringEncodingASCII );
  OSStatus result = MIDIDestinationCreate( data->client,
                                           portNameRef,
                                           midiInputCallback, (void *)&inputData_, &endpoint );
  CFRelease( portNameRef );

  if ( result != noErr ) {
    errorString_ = "MidiInCore::openVirtualPort: error creating virtual OS-X MIDI destination.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Save our api-specific connection information.
  data->endpoint = endpoint;
}

void MidiInCore :: closePort( void )
{
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);

  if ( data->endpoint ) {
    MIDIEndpointDispose( data->endpoint );
    data->endpoint = 0;
  }

  if ( data->port ) {
    MIDIPortDispose( data->port );
    data->port = 0;
  }

  connected_ = false;
}

void MidiInCore :: setClientName ( const std::string& )
{

  errorString_ = "MidiInCore::setClientName: this function is not implemented for the MACOSX_CORE API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiInCore :: setPortName ( const std::string& )
{

  errorString_ = "MidiInCore::setPortName: this function is not implemented for the MACOSX_CORE API!";
  error( RtMidiError::WARNING, errorString_ );

}

unsigned int MidiInCore :: getPortCount()
{
  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  return MIDIGetNumberOfSources();
}

// This function was submitted by Douglas Casey Tucker and apparently
// derived largely from PortMidi.
CFStringRef CreateEndpointName( MIDIEndpointRef endpoint, bool isExternal )
{
  CFMutableStringRef result = CFStringCreateMutable( NULL, 0 );
  CFStringRef str;

  // Begin with the endpoint's name.
  str = NULL;
  MIDIObjectGetStringProperty( endpoint, kMIDIPropertyName, &str );
  if ( str != NULL ) {
    CFStringAppend( result, str );
  }

  // some MIDI devices have a leading space in endpoint name. trim
  CFStringTrim(result, CFSTR(" "));

  MIDIEntityRef entity = 0;
  MIDIEndpointGetEntity( endpoint, &entity );
  if ( entity == 0 )
    // probably virtual
    return result;

  if ( CFStringGetLength( result ) == 0 ) {
    // endpoint name has zero length -- try the entity
    str = NULL;
    MIDIObjectGetStringProperty( entity, kMIDIPropertyName, &str );
    if ( str != NULL ) {
      CFStringAppend( result, str );
    }
  }
  // now consider the device's name
  MIDIDeviceRef device = 0;
  MIDIEntityGetDevice( entity, &device );
  if ( device == 0 )
    return result;

  str = NULL;
  MIDIObjectGetStringProperty( device, kMIDIPropertyName, &str );
  if ( CFStringGetLength( result ) == 0 ) {
      CFRelease( result );
      CFRetain( str );
      return str;
  }
  if ( str != NULL ) {
    // if an external device has only one entity, throw away
    // the endpoint name and just use the device name
    if ( isExternal && MIDIDeviceGetNumberOfEntities( device ) < 2 ) {
      CFRelease( result );
      CFRetain( str );
      return str;
    } else {
      if ( CFStringGetLength( str ) == 0 ) {
        return result;
      }
      // does the entity name already start with the device name?
      // (some drivers do this though they shouldn't)
      // if so, do not prepend
      if ( CFStringCompareWithOptions( result, /* endpoint name */
                                       str /* device name */,
                                       CFRangeMake(0, CFStringGetLength( str ) ), 0 ) != kCFCompareEqualTo ) {
        // prepend the device name to the entity name
        if ( CFStringGetLength( result ) > 0 )
          CFStringInsert( result, 0, CFSTR(" ") );

        CFStringInsert( result, 0, str );
      }
    }
  }
  return result;
}

// This function was submitted by Douglas Casey Tucker and apparently
// derived largely from PortMidi.
static CFStringRef CreateConnectedEndpointName( MIDIEndpointRef endpoint )
{
  CFMutableStringRef result = CFStringCreateMutable( NULL, 0 );
  CFStringRef str;
  OSStatus err;
  int i;

  // Does the endpoint have connections?
  CFDataRef connections = NULL;
  int nConnected = 0;
  bool anyStrings = false;
  err = MIDIObjectGetDataProperty( endpoint, kMIDIPropertyConnectionUniqueID, &connections );
  if ( connections != NULL ) {
    // It has connections, follow them
    // Concatenate the names of all connected devices
    nConnected = CFDataGetLength( connections ) / sizeof(MIDIUniqueID);
    if ( nConnected ) {
      const SInt32 *pid = (const SInt32 *)(CFDataGetBytePtr(connections));
      for ( i=0; i<nConnected; ++i, ++pid ) {
        MIDIUniqueID id = EndianS32_BtoN( *pid );
        MIDIObjectRef connObject;
        MIDIObjectType connObjectType;
        err = MIDIObjectFindByUniqueID( id, &connObject, &connObjectType );
        if ( err == noErr ) {
          if ( connObjectType == kMIDIObjectType_ExternalSource  ||
               connObjectType == kMIDIObjectType_ExternalDestination ) {
            // Connected to an external device's endpoint (10.3 and later).
            str = CreateEndpointName( (MIDIEndpointRef)(connObject), true );
          } else {
            // Connected to an external device (10.2) (or something else, catch-
            str = NULL;
            MIDIObjectGetStringProperty( connObject, kMIDIPropertyName, &str );
            if ( str ) CFRetain ( str );
          }
          if ( str != NULL ) {
            if ( anyStrings )
              CFStringAppend( result, CFSTR(", ") );
            else
              anyStrings = true;
            CFStringAppend( result, str );
            CFRelease( str );
          }
        }
      }
    }
    CFRelease( connections );
  }
  if ( anyStrings )
    return result;

  CFRelease( result );

  // Here, either the endpoint had no connections, or we failed to obtain names
  return CreateEndpointName( endpoint, false );
}

std::string MidiInCore :: getPortName( unsigned int portNumber )
{
  CFStringRef nameRef;
  MIDIEndpointRef portRef;
  char name[128];

  std::string stringName;
  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  if ( portNumber >= MIDIGetNumberOfSources() ) {
    std::ostringstream ost;
    ost << "MidiInCore::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
    return stringName;
  }

  portRef = MIDIGetSource( portNumber );
  nameRef = CreateConnectedEndpointName( portRef );
  CFStringGetCString( nameRef, name, sizeof(name), kCFStringEncodingUTF8 );
  CFRelease( nameRef );

  return stringName = name;
}

//*********************************************************************//
//  API: OS-X
//  Class Definitions: MidiOutCore
//*********************************************************************//

MidiOutCore :: MidiOutCore( const std::string &clientName )
  : MidiOutApi()
{
  MidiOutCore::initialize( clientName );
}

MidiOutCore :: ~MidiOutCore( void )
{
  // Close a connection if it exists.
  MidiOutCore::closePort();

  // Cleanup.
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);
  if ( data->endpoint ) MIDIEndpointDispose( data->endpoint );
  delete data;
}

MIDIClientRef MidiOutCore::getCoreMidiClientSingleton(const std::string& clientName) throw() {

  if (CoreMidiClientSingleton == 0){
      // Set up our client.
      MIDIClientRef client;

      CFStringRef name = CFStringCreateWithCString( NULL, clientName.c_str(), kCFStringEncodingASCII );
      OSStatus result = MIDIClientCreate(name, NULL, NULL, &client );
      if ( result != noErr ) {
        std::ostringstream ost;
        ost << "MidiInCore::initialize: error creating OS-X MIDI client object (" << result << ").";
        errorString_ = ost.str();
        error( RtMidiError::DRIVER_ERROR, errorString_ );
        return 0;
      }
      CFRelease( name );

      CoreMidiClientSingleton = client;
  }

  return CoreMidiClientSingleton;
}

void MidiOutCore :: initialize( const std::string& clientName )
{
  // Set up our client.
  MIDIClientRef client = getCoreMidiClientSingleton(clientName);

  // Save our api-specific connection information.
  CoreMidiData *data = (CoreMidiData *) new CoreMidiData;
  data->client = client;
  data->endpoint = 0;
  apiData_ = (void *) data;
}

unsigned int MidiOutCore :: getPortCount()
{
  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  return MIDIGetNumberOfDestinations();
}

std::string MidiOutCore :: getPortName( unsigned int portNumber )
{
  CFStringRef nameRef;
  MIDIEndpointRef portRef;
  char name[128];

  std::string stringName;
  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  if ( portNumber >= MIDIGetNumberOfDestinations() ) {
    std::ostringstream ost;
    ost << "MidiOutCore::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
    return stringName;
  }

  portRef = MIDIGetDestination( portNumber );
  nameRef = CreateConnectedEndpointName(portRef);
  CFStringGetCString( nameRef, name, sizeof(name), kCFStringEncodingUTF8 );
  CFRelease( nameRef );

  return stringName = name;
}

void MidiOutCore :: openPort( unsigned int portNumber, const std::string &portName )
{
  if ( connected_ ) {
    errorString_ = "MidiOutCore::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  CFRunLoopRunInMode( kCFRunLoopDefaultMode, 0, false );
  unsigned int nDest = MIDIGetNumberOfDestinations();
  if (nDest < 1) {
    errorString_ = "MidiOutCore::openPort: no MIDI output destinations found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  if ( portNumber >= nDest ) {
    std::ostringstream ost;
    ost << "MidiOutCore::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  MIDIPortRef port;
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);
  CFStringRef portNameRef = CFStringCreateWithCString( NULL, portName.c_str(), kCFStringEncodingASCII );
  OSStatus result = MIDIOutputPortCreate( data->client, portNameRef, &port );
  CFRelease( portNameRef );
  if ( result != noErr ) {
    errorString_ = "MidiOutCore::openPort: error creating OS-X MIDI output port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Get the desired output port identifier.
  MIDIEndpointRef destination = MIDIGetDestination( portNumber );
  if ( destination == 0 ) {
    MIDIPortDispose( port );
    errorString_ = "MidiOutCore::openPort: error getting MIDI output destination reference.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Save our api-specific connection information.
  data->port = port;
  data->destinationId = destination;
  connected_ = true;
}

void MidiOutCore :: closePort( void )
{
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);

  if ( data->endpoint ) {
    MIDIEndpointDispose( data->endpoint );
    data->endpoint = 0;
  }

  if ( data->port ) {
    MIDIPortDispose( data->port );
    data->port = 0;
  }

  connected_ = false;
}

void MidiOutCore :: setClientName ( const std::string& )
{

  errorString_ = "MidiOutCore::setClientName: this function is not implemented for the MACOSX_CORE API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutCore :: setPortName ( const std::string& )
{

  errorString_ = "MidiOutCore::setPortName: this function is not implemented for the MACOSX_CORE API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutCore :: openVirtualPort( const std::string &portName )
{
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);

  if ( data->endpoint ) {
    errorString_ = "MidiOutCore::openVirtualPort: a virtual output port already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  // Create a virtual MIDI output source.
  MIDIEndpointRef endpoint;
  CFStringRef portNameRef = CFStringCreateWithCString( NULL, portName.c_str(), kCFStringEncodingASCII );
  OSStatus result = MIDISourceCreate( data->client, portNameRef, &endpoint );
  CFRelease( portNameRef );

  if ( result != noErr ) {
    errorString_ = "MidiOutCore::initialize: error creating OS-X virtual MIDI source.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Save our api-specific connection information.
  data->endpoint = endpoint;
}

void MidiOutCore :: sendMessage( const unsigned char *message, size_t size )
{
  // We use the MIDISendSysex() function to asynchronously send sysex
  // messages.  Otherwise, we use a single CoreMidi MIDIPacket.
  unsigned int nBytes = static_cast<unsigned int> (size);
  if ( nBytes == 0 ) {
    errorString_ = "MidiOutCore::sendMessage: no data in message argument!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  if ( message[0] != 0xF0 && nBytes > 3 ) {
    errorString_ = "MidiOutCore::sendMessage: message format problem ... not sysex but > 3 bytes?";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  MIDITimeStamp timeStamp = AudioGetCurrentHostTime();
  CoreMidiData *data = static_cast<CoreMidiData *> (apiData_);
  OSStatus result;

  ByteCount bufsize = nBytes > 65535 ? 65535 : nBytes;
  Byte buffer[bufsize+16]; // pad for other struct members
  ByteCount listSize = sizeof( buffer );
  MIDIPacketList *packetList = (MIDIPacketList*)buffer;

  ByteCount remainingBytes = nBytes;
  while ( remainingBytes ) {
    MIDIPacket *packet = MIDIPacketListInit( packetList );
    // A MIDIPacketList can only contain a maximum of 64K of data, so if our message is longer,
    // break it up into chunks of 64K or less and send out as a MIDIPacketList with only one
    // MIDIPacket. Here, we reuse the memory allocated above on the stack for all.
    ByteCount bytesForPacket = remainingBytes > 65535 ? 65535 : remainingBytes;
    const Byte* dataStartPtr = (const Byte *) &message[nBytes - remainingBytes];
    packet = MIDIPacketListAdd( packetList, listSize, packet, timeStamp, bytesForPacket, dataStartPtr );
    remainingBytes -= bytesForPacket;

    if ( !packet ) {
      errorString_ = "MidiOutCore::sendMessage: could not allocate packet list";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }

    // Send to any destinations that may have connected to us.
    if ( data->endpoint ) {
      result = MIDIReceived( data->endpoint, packetList );
      if ( result != noErr ) {
        errorString_ = "MidiOutCore::sendMessage: error sending MIDI to virtual destinations.";
        error( RtMidiError::WARNING, errorString_ );
      }
    }

    // And send to an explicit destination port if we're connected.
    if ( connected_ ) {
      result = MIDISend( data->port, data->destinationId, packetList );
      if ( result != noErr ) {
        errorString_ = "MidiOutCore::sendMessage: error sending MIDI message to port.";
        error( RtMidiError::WARNING, errorString_ );
      }
    }
  }
}

#endif  // __MACOSX_CORE__


//*********************************************************************//
//  API: LINUX ALSA SEQUENCER
//*********************************************************************//

// API information found at:
//   - http://www.alsa-project.org/documentation.php#Library

#if defined(__LINUX_ALSA__)

// The ALSA Sequencer API is based on the use of a callback function for
// MIDI input.
//
// Thanks to Pedro Lopez-Cabanillas for help with the ALSA sequencer
// time stamps and other assorted fixes!!!

// If you don't need timestamping for incoming MIDI events, define the
// preprocessor definition AVOID_TIMESTAMPING to save resources
// associated with the ALSA sequencer queues.

#include <pthread.h>
#include <sys/time.h>

// ALSA header file.
#include <alsa/asoundlib.h>

// A structure to hold variables related to the ALSA API
// implementation.
struct AlsaMidiData {
  snd_seq_t *seq;
  unsigned int portNum;
  int vport;
  snd_seq_port_subscribe_t *subscription;
  snd_midi_event_t *coder;
  unsigned int bufferSize;
  unsigned int requestedBufferSize;
  unsigned char *buffer;
  pthread_t thread;
  pthread_t dummy_thread_id;
  snd_seq_real_time_t lastTime;
  int queue_id; // an input queue is needed to get timestamped events
  int trigger_fds[2];
};

#define PORT_TYPE( pinfo, bits ) ((snd_seq_port_info_get_capability(pinfo) & (bits)) == (bits))

//*********************************************************************//
//  API: LINUX ALSA
//  Class Definitions: MidiInAlsa
//*********************************************************************//

static void *alsaMidiHandler( void *ptr )
{
  MidiInApi::RtMidiInData *data = static_cast<MidiInApi::RtMidiInData *> (ptr);
  AlsaMidiData *apiData = static_cast<AlsaMidiData *> (data->apiData);

  long nBytes;
  double time;
  bool continueSysex = false;
  bool doDecode = false;
  MidiInApi::MidiMessage message;
  int poll_fd_count;
  struct pollfd *poll_fds;

  snd_seq_event_t *ev;
  int result;
  result = snd_midi_event_new( 0, &apiData->coder );
  if ( result < 0 ) {
    data->doInput = false;
    std::cerr << "\nMidiInAlsa::alsaMidiHandler: error initializing MIDI event parser!\n\n";
    return 0;
  }
  unsigned char *buffer = (unsigned char *) malloc( apiData->bufferSize );
  if ( buffer == NULL ) {
    data->doInput = false;
    snd_midi_event_free( apiData->coder );
    apiData->coder = 0;
    std::cerr << "\nMidiInAlsa::alsaMidiHandler: error initializing buffer memory!\n\n";
    return 0;
  }
  snd_midi_event_init( apiData->coder );
  snd_midi_event_no_status( apiData->coder, 1 ); // suppress running status messages

  poll_fd_count = snd_seq_poll_descriptors_count( apiData->seq, POLLIN ) + 1;
  poll_fds = (struct pollfd*)alloca( poll_fd_count * sizeof( struct pollfd ));
  snd_seq_poll_descriptors( apiData->seq, poll_fds + 1, poll_fd_count - 1, POLLIN );
  poll_fds[0].fd = apiData->trigger_fds[0];
  poll_fds[0].events = POLLIN;

  while ( data->doInput ) {

    if ( snd_seq_event_input_pending( apiData->seq, 1 ) == 0 ) {
      // No data pending
      if ( poll( poll_fds, poll_fd_count, -1) >= 0 ) {
        if ( poll_fds[0].revents & POLLIN ) {
          bool dummy;
          int res = read( poll_fds[0].fd, &dummy, sizeof(dummy) );
          (void) res;
        }
      }
      continue;
    }

    // If here, there should be data.
    result = snd_seq_event_input( apiData->seq, &ev );
    if ( result == -ENOSPC ) {
      std::cerr << "\nMidiInAlsa::alsaMidiHandler: MIDI input buffer overrun!\n\n";
      continue;
    }
    else if ( result <= 0 ) {
      std::cerr << "\nMidiInAlsa::alsaMidiHandler: unknown MIDI input error!\n";
      perror("System reports");
      continue;
    }

    // This is a bit weird, but we now have to decode an ALSA MIDI
    // event (back) into MIDI bytes.  We'll ignore non-MIDI types.
    if ( !continueSysex ) message.bytes.clear();

    doDecode = false;
    switch ( ev->type ) {

    case SND_SEQ_EVENT_PORT_SUBSCRIBED:
#if defined(__RTMIDI_DEBUG__)
      std::cout << "MidiInAlsa::alsaMidiHandler: port connection made!\n";
#endif
      break;

    case SND_SEQ_EVENT_PORT_UNSUBSCRIBED:
#if defined(__RTMIDI_DEBUG__)
      std::cerr << "MidiInAlsa::alsaMidiHandler: port connection has closed!\n";
      std::cout << "sender = " << (int) ev->data.connect.sender.client << ":"
                << (int) ev->data.connect.sender.port
                << ", dest = " << (int) ev->data.connect.dest.client << ":"
                << (int) ev->data.connect.dest.port
                << std::endl;
#endif
      break;

    case SND_SEQ_EVENT_QFRAME: // MIDI time code
      if ( !( data->ignoreFlags & 0x02 ) ) doDecode = true;
      break;

    case SND_SEQ_EVENT_TICK: // 0xF9 ... MIDI timing tick
      if ( !( data->ignoreFlags & 0x02 ) ) doDecode = true;
      break;

    case SND_SEQ_EVENT_CLOCK: // 0xF8 ... MIDI timing (clock) tick
      if ( !( data->ignoreFlags & 0x02 ) ) doDecode = true;
      break;

    case SND_SEQ_EVENT_SENSING: // Active sensing
      if ( !( data->ignoreFlags & 0x04 ) ) doDecode = true;
      break;

    case SND_SEQ_EVENT_SYSEX:
      if ( (data->ignoreFlags & 0x01) ) break;
      if ( ev->data.ext.len > apiData->bufferSize ) {
        apiData->bufferSize = ev->data.ext.len;
        free( buffer );
        buffer = (unsigned char *) malloc( apiData->bufferSize );
        if ( buffer == NULL ) {
          data->doInput = false;
          std::cerr << "\nMidiInAlsa::alsaMidiHandler: error resizing buffer memory!\n\n";
          break;
        }
      }
      doDecode = true;
      break;

    default:
      doDecode = true;
    }

    if ( doDecode ) {

      nBytes = snd_midi_event_decode( apiData->coder, buffer, apiData->bufferSize, ev );
      if ( nBytes > 0 ) {
        // The ALSA sequencer has a maximum buffer size for MIDI sysex
        // events of 256 bytes.  If a device sends sysex messages larger
        // than this, they are segmented into 256 byte chunks.  So,
        // we'll watch for this and concatenate sysex chunks into a
        // single sysex message if necessary.
        if ( !continueSysex )
          message.bytes.assign( buffer, &buffer[nBytes] );
        else
          message.bytes.insert( message.bytes.end(), buffer, &buffer[nBytes] );

        continueSysex = ( ( ev->type == SND_SEQ_EVENT_SYSEX ) && ( message.bytes.back() != 0xF7 ) );
        if ( !continueSysex ) {

          // Calculate the time stamp:
          message.timeStamp = 0.0;

          // Method 1: Use the system time.
          //(void)gettimeofday(&tv, (struct timezone *)NULL);
          //time = (tv.tv_sec * 1000000) + tv.tv_usec;

          // Method 2: Use the ALSA sequencer event time data.
          // (thanks to Pedro Lopez-Cabanillas!).

          // Using method from:
          // https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html

          // Perform the carry for the later subtraction by updating y.
          // Temp var y is timespec because computation requires signed types,
          // while snd_seq_real_time_t has unsigned types.
          snd_seq_real_time_t &x( ev->time.time );
          struct timespec y;
          y.tv_nsec = apiData->lastTime.tv_nsec;
          y.tv_sec = apiData->lastTime.tv_sec;
          if ( x.tv_nsec < y.tv_nsec ) {
              int nsec = (y.tv_nsec - (int)x.tv_nsec) / 1000000000 + 1;
              y.tv_nsec -= 1000000000 * nsec;
              y.tv_sec += nsec;
          }
          if ( x.tv_nsec - y.tv_nsec > 1000000000 ) {
              int nsec = ((int)x.tv_nsec - y.tv_nsec) / 1000000000;
              y.tv_nsec += 1000000000 * nsec;
              y.tv_sec -= nsec;
          }

          // Compute the time difference.
          time = (int)x.tv_sec - y.tv_sec + ((int)x.tv_nsec - y.tv_nsec)*1e-9;

          apiData->lastTime = ev->time.time;

          if ( data->firstMessage == true )
            data->firstMessage = false;
          else
            message.timeStamp = time;
        }
        else {
#if defined(__RTMIDI_DEBUG__)
          std::cerr << "\nMidiInAlsa::alsaMidiHandler: event parsing error or not a MIDI event!\n\n";
#endif
        }
      }
    }

    snd_seq_free_event( ev );
    if ( message.bytes.size() == 0 || continueSysex ) continue;

    if ( data->usingCallback ) {
      RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) data->userCallback;
      callback( message.timeStamp, &message.bytes, data->userData );
    }
    else {
      // As long as we haven't reached our queue size limit, push the message.
      if ( !data->queue.push( message ) )
        std::cerr << "\nMidiInAlsa: message queue limit reached!!\n\n";
    }
  }

  if ( buffer ) free( buffer );
  snd_midi_event_free( apiData->coder );
  apiData->coder = 0;
  apiData->thread = apiData->dummy_thread_id;
  return 0;
}

MidiInAlsa :: MidiInAlsa( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit )
{
  MidiInAlsa::initialize( clientName );
}

MidiInAlsa :: ~MidiInAlsa()
{
  // Close a connection if it exists.
  MidiInAlsa::closePort();

  // Shutdown the input thread.
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( inputData_.doInput ) {
    inputData_.doInput = false;
    int res = write( data->trigger_fds[1], &inputData_.doInput, sizeof( inputData_.doInput ) );
    (void) res;
    if ( !pthread_equal(data->thread, data->dummy_thread_id) )
      pthread_join( data->thread, NULL );
  }

  // Cleanup.
  close ( data->trigger_fds[0] );
  close ( data->trigger_fds[1] );
  if ( data->vport >= 0 ) snd_seq_delete_port( data->seq, data->vport );
#ifndef AVOID_TIMESTAMPING
  snd_seq_free_queue( data->seq, data->queue_id );
#endif
  snd_seq_close( data->seq );
  delete data;
}

void MidiInAlsa :: initialize( const std::string& clientName )
{
  // Set up the ALSA sequencer client.
  snd_seq_t *seq;
  int result = snd_seq_open( &seq, "default", SND_SEQ_OPEN_DUPLEX, SND_SEQ_NONBLOCK );
  if ( result < 0 ) {
    errorString_ = "MidiInAlsa::initialize: error creating ALSA sequencer client object.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Set client name.
  snd_seq_set_client_name( seq, clientName.c_str() );

  // Save our api-specific connection information.
  AlsaMidiData *data = (AlsaMidiData *) new AlsaMidiData;
  data->seq = seq;
  data->portNum = -1;
  data->vport = -1;
  data->subscription = 0;
  data->dummy_thread_id = pthread_self();
  data->thread = data->dummy_thread_id;
  data->trigger_fds[0] = -1;
  data->trigger_fds[1] = -1;
  data->bufferSize = inputData_.bufferSize;
  apiData_ = (void *) data;
  inputData_.apiData = (void *) data;

  if ( pipe(data->trigger_fds) == -1 ) {
    errorString_ = "MidiInAlsa::initialize: error creating pipe objects.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Create the input queue
#ifndef AVOID_TIMESTAMPING
  data->queue_id = snd_seq_alloc_named_queue( seq, "RtMidi Queue" );
  // Set arbitrary tempo (mm=100) and resolution (240)
  snd_seq_queue_tempo_t *qtempo;
  snd_seq_queue_tempo_alloca( &qtempo );
  snd_seq_queue_tempo_set_tempo( qtempo, 600000 );
  snd_seq_queue_tempo_set_ppq( qtempo, 240 );
  snd_seq_set_queue_tempo( data->seq, data->queue_id, qtempo );
  snd_seq_drain_output( data->seq );
#endif
}

// This function is used to count or get the pinfo structure for a given port number.
unsigned int portInfo( snd_seq_t *seq, snd_seq_port_info_t *pinfo, unsigned int type, int portNumber )
{
  snd_seq_client_info_t *cinfo;
  int client;
  int count = 0;
  snd_seq_client_info_alloca( &cinfo );

  snd_seq_client_info_set_client( cinfo, -1 );
  while ( snd_seq_query_next_client( seq, cinfo ) >= 0 ) {
    client = snd_seq_client_info_get_client( cinfo );
    if ( client == 0 ) continue;
    // Reset query info
    snd_seq_port_info_set_client( pinfo, client );
    snd_seq_port_info_set_port( pinfo, -1 );
    while ( snd_seq_query_next_port( seq, pinfo ) >= 0 ) {
      unsigned int atyp = snd_seq_port_info_get_type( pinfo );
      if ( ( ( atyp & SND_SEQ_PORT_TYPE_MIDI_GENERIC ) == 0 ) &&
           ( ( atyp & SND_SEQ_PORT_TYPE_SYNTH ) == 0 ) &&
           ( ( atyp & SND_SEQ_PORT_TYPE_APPLICATION ) == 0 ) ) continue;

      unsigned int caps = snd_seq_port_info_get_capability( pinfo );
      if ( ( caps & type ) != type ) continue;
      if ( count == portNumber ) return 1;
      ++count;
    }
  }

  // If a negative portNumber was used, return the port count.
  if ( portNumber < 0 ) return count;
  return 0;
}

unsigned int MidiInAlsa :: getPortCount()
{
  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );

  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  return portInfo( data->seq, pinfo, SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ, -1 );
}

std::string MidiInAlsa :: getPortName( unsigned int portNumber )
{
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;
  snd_seq_client_info_alloca( &cinfo );
  snd_seq_port_info_alloca( &pinfo );

  std::string stringName;
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( portInfo( data->seq, pinfo, SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ, (int) portNumber ) ) {
    int cnum = snd_seq_port_info_get_client( pinfo );
    snd_seq_get_any_client_info( data->seq, cnum, cinfo );
    std::ostringstream os;
    os << snd_seq_client_info_get_name( cinfo );
    os << ":";
    os << snd_seq_port_info_get_name( pinfo );
    os << " ";                                    // These lines added to make sure devices are listed
    os << snd_seq_port_info_get_client( pinfo );  // with full portnames added to ensure individual device names
    os << ":";
    os << snd_seq_port_info_get_port( pinfo );
    stringName = os.str();
    return stringName;
  }

  // If we get here, we didn't find a match.
  errorString_ = "MidiInAlsa::getPortName: error looking for port name!";
  error( RtMidiError::WARNING, errorString_ );
  return stringName;
}

void MidiInAlsa :: openPort( unsigned int portNumber, const std::string &portName )
{
  if ( connected_ ) {
    errorString_ = "MidiInAlsa::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  unsigned int nSrc = this->getPortCount();
  if ( nSrc < 1 ) {
    errorString_ = "MidiInAlsa::openPort: no MIDI input sources found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  snd_seq_port_info_t *src_pinfo;
  snd_seq_port_info_alloca( &src_pinfo );
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( portInfo( data->seq, src_pinfo, SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ, (int) portNumber ) == 0 ) {
    std::ostringstream ost;
    ost << "MidiInAlsa::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  snd_seq_addr_t sender, receiver;
  sender.client = snd_seq_port_info_get_client( src_pinfo );
  sender.port = snd_seq_port_info_get_port( src_pinfo );
  receiver.client = snd_seq_client_id( data->seq );

  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );
  if ( data->vport < 0 ) {
    snd_seq_port_info_set_client( pinfo, 0 );
    snd_seq_port_info_set_port( pinfo, 0 );
    snd_seq_port_info_set_capability( pinfo,
                                      SND_SEQ_PORT_CAP_WRITE |
                                      SND_SEQ_PORT_CAP_SUBS_WRITE );
    snd_seq_port_info_set_type( pinfo,
                                SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                                SND_SEQ_PORT_TYPE_APPLICATION );
    snd_seq_port_info_set_midi_channels(pinfo, 16);
#ifndef AVOID_TIMESTAMPING
    snd_seq_port_info_set_timestamping( pinfo, 1 );
    snd_seq_port_info_set_timestamp_real( pinfo, 1 );
    snd_seq_port_info_set_timestamp_queue( pinfo, data->queue_id );
#endif
    snd_seq_port_info_set_name( pinfo,  portName.c_str() );
    data->vport = snd_seq_create_port( data->seq, pinfo );

    if ( data->vport < 0 ) {
      errorString_ = "MidiInAlsa::openPort: ALSA error creating input port.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
    data->vport = snd_seq_port_info_get_port( pinfo );
  }

  receiver.port = data->vport;

  if ( !data->subscription ) {
    // Make subscription
    if ( snd_seq_port_subscribe_malloc( &data->subscription ) < 0 ) {
      errorString_ = "MidiInAlsa::openPort: ALSA error allocation port subscription.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
    snd_seq_port_subscribe_set_sender( data->subscription, &sender );
    snd_seq_port_subscribe_set_dest( data->subscription, &receiver );
    if ( snd_seq_subscribe_port( data->seq, data->subscription ) ) {
      snd_seq_port_subscribe_free( data->subscription );
      data->subscription = 0;
      errorString_ = "MidiInAlsa::openPort: ALSA error making port connection.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
  }

  if ( inputData_.doInput == false ) {
    // Start the input queue
#ifndef AVOID_TIMESTAMPING
    snd_seq_start_queue( data->seq, data->queue_id, NULL );
    snd_seq_drain_output( data->seq );
#endif
    // Start our MIDI input thread.
    pthread_attr_t attr;
    pthread_attr_init( &attr );
    pthread_attr_setdetachstate( &attr, PTHREAD_CREATE_JOINABLE );
    pthread_attr_setschedpolicy( &attr, SCHED_OTHER );

    inputData_.doInput = true;
    int err = pthread_create( &data->thread, &attr, alsaMidiHandler, &inputData_ );
    pthread_attr_destroy( &attr );
    if ( err ) {
      snd_seq_unsubscribe_port( data->seq, data->subscription );
      snd_seq_port_subscribe_free( data->subscription );
      data->subscription = 0;
      inputData_.doInput = false;
      errorString_ = "MidiInAlsa::openPort: error starting MIDI input thread!";
      error( RtMidiError::THREAD_ERROR, errorString_ );
      return;
    }
  }

  connected_ = true;
}

void MidiInAlsa :: openVirtualPort( const std::string &portName )
{
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( data->vport < 0 ) {
    snd_seq_port_info_t *pinfo;
    snd_seq_port_info_alloca( &pinfo );
    snd_seq_port_info_set_capability( pinfo,
                                      SND_SEQ_PORT_CAP_WRITE |
                                      SND_SEQ_PORT_CAP_SUBS_WRITE );
    snd_seq_port_info_set_type( pinfo,
                                SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                                SND_SEQ_PORT_TYPE_APPLICATION );
    snd_seq_port_info_set_midi_channels( pinfo, 16 );
#ifndef AVOID_TIMESTAMPING
    snd_seq_port_info_set_timestamping( pinfo, 1 );
    snd_seq_port_info_set_timestamp_real( pinfo, 1 );
    snd_seq_port_info_set_timestamp_queue( pinfo, data->queue_id );
#endif
    snd_seq_port_info_set_name( pinfo, portName.c_str() );
    data->vport = snd_seq_create_port( data->seq, pinfo );

    if ( data->vport < 0 ) {
      errorString_ = "MidiInAlsa::openVirtualPort: ALSA error creating virtual port.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
    data->vport = snd_seq_port_info_get_port( pinfo );
  }

  if ( inputData_.doInput == false ) {
    // Wait for old thread to stop, if still running
    if ( !pthread_equal( data->thread, data->dummy_thread_id ) )
      pthread_join( data->thread, NULL );

    // Start the input queue
#ifndef AVOID_TIMESTAMPING
    snd_seq_start_queue( data->seq, data->queue_id, NULL );
    snd_seq_drain_output( data->seq );
#endif
    // Start our MIDI input thread.
    pthread_attr_t attr;
    pthread_attr_init( &attr );
    pthread_attr_setdetachstate( &attr, PTHREAD_CREATE_JOINABLE );
    pthread_attr_setschedpolicy( &attr, SCHED_OTHER );

    inputData_.doInput = true;
    int err = pthread_create( &data->thread, &attr, alsaMidiHandler, &inputData_ );
    pthread_attr_destroy( &attr );
    if ( err ) {
      if ( data->subscription ) {
        snd_seq_unsubscribe_port( data->seq, data->subscription );
        snd_seq_port_subscribe_free( data->subscription );
        data->subscription = 0;
      }
      inputData_.doInput = false;
      errorString_ = "MidiInAlsa::openPort: error starting MIDI input thread!";
      error( RtMidiError::THREAD_ERROR, errorString_ );
      return;
    }
  }
}

void MidiInAlsa :: closePort( void )
{
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);

  if ( connected_ ) {
    if ( data->subscription ) {
      snd_seq_unsubscribe_port( data->seq, data->subscription );
      snd_seq_port_subscribe_free( data->subscription );
      data->subscription = 0;
    }
    // Stop the input queue
#ifndef AVOID_TIMESTAMPING
    snd_seq_stop_queue( data->seq, data->queue_id, NULL );
    snd_seq_drain_output( data->seq );
#endif
    connected_ = false;
  }

  // Stop thread to avoid triggering the callback, while the port is intended to be closed
  if ( inputData_.doInput ) {
    inputData_.doInput = false;
    int res = write( data->trigger_fds[1], &inputData_.doInput, sizeof( inputData_.doInput ) );
    (void) res;
    if ( !pthread_equal( data->thread, data->dummy_thread_id ) )
      pthread_join( data->thread, NULL );
  }
}

void MidiInAlsa :: setClientName( const std::string &clientName )
{

  AlsaMidiData *data = static_cast<AlsaMidiData *> ( apiData_ );
  snd_seq_set_client_name( data->seq, clientName.c_str() );

}

void MidiInAlsa :: setPortName( const std::string &portName )
{
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );
  snd_seq_get_port_info( data->seq, data->vport, pinfo );
  snd_seq_port_info_set_name( pinfo, portName.c_str() );
  snd_seq_set_port_info( data->seq, data->vport, pinfo );
}

//*********************************************************************//
//  API: LINUX ALSA
//  Class Definitions: MidiOutAlsa
//*********************************************************************//

MidiOutAlsa :: MidiOutAlsa( const std::string &clientName ) : MidiOutApi()
{
  MidiOutAlsa::initialize( clientName );
}

MidiOutAlsa :: ~MidiOutAlsa()
{
  // Close a connection if it exists.
  MidiOutAlsa::closePort();

  // Cleanup.
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( data->vport >= 0 ) snd_seq_delete_port( data->seq, data->vport );
  if ( data->coder ) snd_midi_event_free( data->coder );
  if ( data->buffer ) free( data->buffer );
  snd_seq_close( data->seq );
  delete data;
}

void MidiOutAlsa :: initialize( const std::string& clientName )
{
  // Set up the ALSA sequencer client.
  snd_seq_t *seq;
  int result1 = snd_seq_open( &seq, "default", SND_SEQ_OPEN_OUTPUT, SND_SEQ_NONBLOCK );
  if ( result1 < 0 ) {
    errorString_ = "MidiOutAlsa::initialize: error creating ALSA sequencer client object.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Set client name.
  snd_seq_set_client_name( seq, clientName.c_str() );

  // Save our api-specific connection information.
  AlsaMidiData *data = (AlsaMidiData *) new AlsaMidiData;
  data->seq = seq;
  data->portNum = -1;
  data->vport = -1;
  data->bufferSize = 32;
  data->coder = 0;
  data->buffer = 0;
  int result = snd_midi_event_new( data->bufferSize, &data->coder );
  if ( result < 0 ) {
    delete data;
    errorString_ = "MidiOutAlsa::initialize: error initializing MIDI event parser!\n\n";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }
  data->buffer = (unsigned char *) malloc( data->bufferSize );
  if ( data->buffer == NULL ) {
    delete data;
    errorString_ = "MidiOutAlsa::initialize: error allocating buffer memory!\n\n";
    error( RtMidiError::MEMORY_ERROR, errorString_ );
    return;
  }
  snd_midi_event_init( data->coder );
  apiData_ = (void *) data;
}

unsigned int MidiOutAlsa :: getPortCount()
{
  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );

  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  return portInfo( data->seq, pinfo, SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE, -1 );
}

std::string MidiOutAlsa :: getPortName( unsigned int portNumber )
{
  snd_seq_client_info_t *cinfo;
  snd_seq_port_info_t *pinfo;
  snd_seq_client_info_alloca( &cinfo );
  snd_seq_port_info_alloca( &pinfo );

  std::string stringName;
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( portInfo( data->seq, pinfo, SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE, (int) portNumber ) ) {
    int cnum = snd_seq_port_info_get_client( pinfo );
    snd_seq_get_any_client_info( data->seq, cnum, cinfo );
    std::ostringstream os;
    os << snd_seq_client_info_get_name( cinfo );
    os << ":";
    os << snd_seq_port_info_get_name( pinfo );
    os << " ";                                    // These lines added to make sure devices are listed
    os << snd_seq_port_info_get_client( pinfo );  // with full portnames added to ensure individual device names
    os << ":";
    os << snd_seq_port_info_get_port( pinfo );
    stringName = os.str();
    return stringName;
  }

  // If we get here, we didn't find a match.
  errorString_ = "MidiOutAlsa::getPortName: error looking for port name!";
  error( RtMidiError::WARNING, errorString_ );
  return stringName;
}

void MidiOutAlsa :: openPort( unsigned int portNumber, const std::string &portName )
{
  if ( connected_ ) {
    errorString_ = "MidiOutAlsa::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  unsigned int nSrc = this->getPortCount();
  if ( nSrc < 1 ) {
    errorString_ = "MidiOutAlsa::openPort: no MIDI output sources found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( portInfo( data->seq, pinfo, SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE, (int) portNumber ) == 0 ) {
    std::ostringstream ost;
    ost << "MidiOutAlsa::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  snd_seq_addr_t sender, receiver;
  receiver.client = snd_seq_port_info_get_client( pinfo );
  receiver.port = snd_seq_port_info_get_port( pinfo );
  sender.client = snd_seq_client_id( data->seq );

  if ( data->vport < 0 ) {
    data->vport = snd_seq_create_simple_port( data->seq, portName.c_str(),
                                              SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ,
                                              SND_SEQ_PORT_TYPE_MIDI_GENERIC|SND_SEQ_PORT_TYPE_APPLICATION );
    if ( data->vport < 0 ) {
      errorString_ = "MidiOutAlsa::openPort: ALSA error creating output port.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
  }

  sender.port = data->vport;

  // Make subscription
  if ( snd_seq_port_subscribe_malloc( &data->subscription ) < 0 ) {
    snd_seq_port_subscribe_free( data->subscription );
    errorString_ = "MidiOutAlsa::openPort: error allocating port subscription.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }
  snd_seq_port_subscribe_set_sender( data->subscription, &sender );
  snd_seq_port_subscribe_set_dest( data->subscription, &receiver );
  snd_seq_port_subscribe_set_time_update( data->subscription, 1 );
  snd_seq_port_subscribe_set_time_real( data->subscription, 1 );
  if ( snd_seq_subscribe_port( data->seq, data->subscription ) ) {
    snd_seq_port_subscribe_free( data->subscription );
    errorString_ = "MidiOutAlsa::openPort: ALSA error making port connection.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  connected_ = true;
}

void MidiOutAlsa :: closePort( void )
{
  if ( connected_ ) {
    AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
    snd_seq_unsubscribe_port( data->seq, data->subscription );
    snd_seq_port_subscribe_free( data->subscription );
    data->subscription = 0;
    connected_ = false;
  }
}

void MidiOutAlsa :: setClientName( const std::string &clientName )
{

    AlsaMidiData *data = static_cast<AlsaMidiData *> ( apiData_ );
    snd_seq_set_client_name( data->seq, clientName.c_str() );

}

void MidiOutAlsa :: setPortName( const std::string &portName )
{
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  snd_seq_port_info_t *pinfo;
  snd_seq_port_info_alloca( &pinfo );
  snd_seq_get_port_info( data->seq, data->vport, pinfo );
  snd_seq_port_info_set_name( pinfo, portName.c_str() );
  snd_seq_set_port_info( data->seq, data->vport, pinfo );
}

void MidiOutAlsa :: openVirtualPort( const std::string &portName )
{
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  if ( data->vport < 0 ) {
    data->vport = snd_seq_create_simple_port( data->seq, portName.c_str(),
                                              SND_SEQ_PORT_CAP_READ|SND_SEQ_PORT_CAP_SUBS_READ,
                                              SND_SEQ_PORT_TYPE_MIDI_GENERIC|SND_SEQ_PORT_TYPE_APPLICATION );

    if ( data->vport < 0 ) {
      errorString_ = "MidiOutAlsa::openVirtualPort: ALSA error creating virtual port.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
    }
  }
}

void MidiOutAlsa :: sendMessage( const unsigned char *message, size_t size )
{
  long result;
  AlsaMidiData *data = static_cast<AlsaMidiData *> (apiData_);
  unsigned int nBytes = static_cast<unsigned int> (size);
  if ( nBytes > data->bufferSize ) {
    data->bufferSize = nBytes;
    result = snd_midi_event_resize_buffer( data->coder, nBytes );
    if ( result != 0 ) {
      errorString_ = "MidiOutAlsa::sendMessage: ALSA error resizing MIDI event buffer.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
    free (data->buffer);
    data->buffer = (unsigned char *) malloc( data->bufferSize );
    if ( data->buffer == NULL ) {
      errorString_ = "MidiOutAlsa::initialize: error allocating buffer memory!\n\n";
      error( RtMidiError::MEMORY_ERROR, errorString_ );
      return;
    }
  }

  for ( unsigned int i=0; i<nBytes; ++i ) data->buffer[i] = message[i];

  unsigned int offset = 0;
  while (offset < nBytes) {
    snd_seq_event_t ev;
    snd_seq_ev_clear( &ev );
    snd_seq_ev_set_source( &ev, data->vport );
    snd_seq_ev_set_subs( &ev );
    snd_seq_ev_set_direct( &ev );
    result = snd_midi_event_encode( data->coder, data->buffer + offset,
                                    (long)(nBytes - offset), &ev );
    if ( result < 0 ) {
      errorString_ = "MidiOutAlsa::sendMessage: event parsing error!";
      error( RtMidiError::WARNING, errorString_ );
      return;
    }

    if ( ev.type == SND_SEQ_EVENT_NONE ) {
      errorString_ = "MidiOutAlsa::sendMessage: incomplete message!";
      error( RtMidiError::WARNING, errorString_ );
      return;
    }

    offset += result;

    // Send the event.
    result = snd_seq_event_output( data->seq, &ev );
    if ( result < 0 ) {
      errorString_ = "MidiOutAlsa::sendMessage: error sending MIDI message to port.";
      error( RtMidiError::WARNING, errorString_ );
      return;
    }
  }
  snd_seq_drain_output( data->seq );
}

#endif // __LINUX_ALSA__


//*********************************************************************//
//  API: Windows Multimedia Library (MM)
//*********************************************************************//

// API information deciphered from:
//  - http://msdn.microsoft.com/library/default.asp?url=/library/en-us/multimed/htm/_win32_midi_reference.asp

// Thanks to Jean-Baptiste Berruchon for the sysex code.

#if defined(__WINDOWS_MM__)

// The Windows MM API is based on the use of a callback function for
// MIDI input.  We convert the system specific time stamps to delta
// time values.

// Windows MM MIDI header files.
#include <windows.h>
#include <mmsystem.h>

// Convert a null-terminated wide string or ANSI-encoded string to UTF-8.
static std::string ConvertToUTF8(const TCHAR *str)
{
  std::string u8str;
  const WCHAR *wstr = L"";
#if defined( UNICODE ) || defined( _UNICODE )
  wstr = str;
#else
  // Convert from ANSI encoding to wide string
  int wlength = MultiByteToWideChar( CP_ACP, 0, str, -1, NULL, 0 );
  std::wstring wstrtemp;
  if ( wlength )
  {
    wstrtemp.assign( wlength - 1, 0 );
    MultiByteToWideChar( CP_ACP, 0, str, -1, &wstrtemp[0], wlength );
    wstr = &wstrtemp[0];
  }
#endif
  // Convert from wide string to UTF-8
  int length = WideCharToMultiByte( CP_UTF8, 0, wstr, -1, NULL, 0, NULL, NULL );
  if ( length )
  {
    u8str.assign( length - 1, 0 );
    length = WideCharToMultiByte( CP_UTF8, 0, wstr, -1, &u8str[0], length, NULL, NULL );
  }
  return u8str;
}

// A structure to hold variables related to the CoreMIDI API
// implementation.
struct WinMidiData {
  HMIDIIN inHandle;    // Handle to Midi Input Device
  HMIDIOUT outHandle;  // Handle to Midi Output Device
  DWORD lastTime;
  MidiInApi::MidiMessage message;
  std::vector<LPMIDIHDR> sysexBuffer;
  CRITICAL_SECTION _mutex; // [Patrice] see https://groups.google.com/forum/#!topic/mididev/6OUjHutMpEo
};

//*********************************************************************//
//  API: Windows MM
//  Class Definitions: MidiInWinMM
//*********************************************************************//

static void CALLBACK midiInputCallback( HMIDIIN /*hmin*/,
                                        UINT inputStatus,
                                        DWORD_PTR instancePtr,
                                        DWORD_PTR midiMessage,
                                        DWORD timestamp )
{
  if ( inputStatus != MIM_DATA && inputStatus != MIM_LONGDATA && inputStatus != MIM_LONGERROR ) return;

  //MidiInApi::RtMidiInData *data = static_cast<MidiInApi::RtMidiInData *> (instancePtr);
  MidiInApi::RtMidiInData *data = (MidiInApi::RtMidiInData *)instancePtr;
  WinMidiData *apiData = static_cast<WinMidiData *> (data->apiData);

  // Calculate time stamp.
  if ( data->firstMessage == true ) {
    apiData->message.timeStamp = 0.0;
    data->firstMessage = false;
  }
  else apiData->message.timeStamp = (double) ( timestamp - apiData->lastTime ) * 0.001;

  if ( inputStatus == MIM_DATA ) { // Channel or system message

    // Make sure the first byte is a status byte.
    unsigned char status = (unsigned char) (midiMessage & 0x000000FF);
    if ( !(status & 0x80) ) return;

    // Determine the number of bytes in the MIDI message.
    unsigned short nBytes = 1;
    if ( status < 0xC0 ) nBytes = 3;
    else if ( status < 0xE0 ) nBytes = 2;
    else if ( status < 0xF0 ) nBytes = 3;
    else if ( status == 0xF1 ) {
      if ( data->ignoreFlags & 0x02 ) return;
      else nBytes = 2;
    }
    else if ( status == 0xF2 ) nBytes = 3;
    else if ( status == 0xF3 ) nBytes = 2;
    else if ( status == 0xF8 && ( data->ignoreFlags & 0x02 ) ) {
      // A MIDI timing tick message and we're ignoring it.
      return;
    }
    else if ( status == 0xFE && ( data->ignoreFlags & 0x04 ) ) {
      // A MIDI active sensing message and we're ignoring it.
      return;
    }

    // Copy bytes to our MIDI message.
    unsigned char *ptr = (unsigned char *) &midiMessage;
    for ( int i=0; i<nBytes; ++i ) apiData->message.bytes.push_back( *ptr++ );
  }
  else { // Sysex message ( MIM_LONGDATA or MIM_LONGERROR )
    MIDIHDR *sysex = ( MIDIHDR *) midiMessage;
    if ( !( data->ignoreFlags & 0x01 ) && inputStatus != MIM_LONGERROR ) {
      // Sysex message and we're not ignoring it
      for ( int i=0; i<(int)sysex->dwBytesRecorded; ++i )
        apiData->message.bytes.push_back( sysex->lpData[i] );
    }

    // The WinMM API requires that the sysex buffer be requeued after
    // input of each sysex message.  Even if we are ignoring sysex
    // messages, we still need to requeue the buffer in case the user
    // decides to not ignore sysex messages in the future.  However,
    // it seems that WinMM calls this function with an empty sysex
    // buffer when an application closes and in this case, we should
    // avoid requeueing it, else the computer suddenly reboots after
    // one or two minutes.
    if ( apiData->sysexBuffer[sysex->dwUser]->dwBytesRecorded > 0 ) {
      //if ( sysex->dwBytesRecorded > 0 ) {
      EnterCriticalSection( &(apiData->_mutex) );
      MMRESULT result = midiInAddBuffer( apiData->inHandle, apiData->sysexBuffer[sysex->dwUser], sizeof(MIDIHDR) );
      LeaveCriticalSection( &(apiData->_mutex) );
      if ( result != MMSYSERR_NOERROR )
        std::cerr << "\nRtMidiIn::midiInputCallback: error sending sysex to Midi device!!\n\n";

      if ( data->ignoreFlags & 0x01 ) return;
    }
    else return;
  }

  // Save the time of the last non-filtered message
  apiData->lastTime = timestamp;

  if ( data->usingCallback ) {
    RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) data->userCallback;
    callback( apiData->message.timeStamp, &apiData->message.bytes, data->userData );
  }
  else {
    // As long as we haven't reached our queue size limit, push the message.
    if ( !data->queue.push( apiData->message ) )
      std::cerr << "\nMidiInWinMM: message queue limit reached!!\n\n";
  }

  // Clear the vector for the next input message.
  apiData->message.bytes.clear();
}

MidiInWinMM :: MidiInWinMM( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit )
{
  MidiInWinMM::initialize( clientName );
}

MidiInWinMM :: ~MidiInWinMM()
{
  // Close a connection if it exists.
  MidiInWinMM::closePort();

  WinMidiData *data = static_cast<WinMidiData *> (apiData_);
  DeleteCriticalSection( &(data->_mutex) );

  // Cleanup.
  delete data;
}

void MidiInWinMM :: initialize( const std::string& /*clientName*/ )
{
  // We'll issue a warning here if no devices are available but not
  // throw an error since the user can plugin something later.
  unsigned int nDevices = midiInGetNumDevs();
  if ( nDevices == 0 ) {
    errorString_ = "MidiInWinMM::initialize: no MIDI input devices currently available.";
    error( RtMidiError::WARNING, errorString_ );
  }

  // Save our api-specific connection information.
  WinMidiData *data = (WinMidiData *) new WinMidiData;
  apiData_ = (void *) data;
  inputData_.apiData = (void *) data;
  data->message.bytes.clear();  // needs to be empty for first input message

  if ( !InitializeCriticalSectionAndSpinCount( &(data->_mutex), 0x00000400 ) ) {
    errorString_ = "MidiInWinMM::initialize: InitializeCriticalSectionAndSpinCount failed.";
    error( RtMidiError::WARNING, errorString_ );
  }
}

void MidiInWinMM :: openPort( unsigned int portNumber, const std::string &/*portName*/ )
{
  if ( connected_ ) {
    errorString_ = "MidiInWinMM::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  unsigned int nDevices = midiInGetNumDevs();
  if (nDevices == 0) {
    errorString_ = "MidiInWinMM::openPort: no MIDI input sources found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  if ( portNumber >= nDevices ) {
    std::ostringstream ost;
    ost << "MidiInWinMM::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  WinMidiData *data = static_cast<WinMidiData *> (apiData_);
  MMRESULT result = midiInOpen( &data->inHandle,
                                portNumber,
                                (DWORD_PTR)&midiInputCallback,
                                (DWORD_PTR)&inputData_,
                                CALLBACK_FUNCTION );
  if ( result != MMSYSERR_NOERROR ) {
    errorString_ = "MidiInWinMM::openPort: error creating Windows MM MIDI input port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Allocate and init the sysex buffers.
  data->sysexBuffer.resize( inputData_.bufferCount );
  for ( unsigned int i=0; i < inputData_.bufferCount; ++i ) {
    data->sysexBuffer[i] = (MIDIHDR*) new char[ sizeof(MIDIHDR) ];
    data->sysexBuffer[i]->lpData = new char[ inputData_.bufferSize ];
    data->sysexBuffer[i]->dwBufferLength = inputData_.bufferSize;
    data->sysexBuffer[i]->dwUser = i; // We use the dwUser parameter as buffer indicator
    data->sysexBuffer[i]->dwFlags = 0;

    result = midiInPrepareHeader( data->inHandle, data->sysexBuffer[i], sizeof(MIDIHDR) );
    if ( result != MMSYSERR_NOERROR ) {
      midiInClose( data->inHandle );
      data->inHandle = 0;
      errorString_ = "MidiInWinMM::openPort: error starting Windows MM MIDI input port (PrepareHeader).";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }

    // Register the buffer.
    result = midiInAddBuffer( data->inHandle, data->sysexBuffer[i], sizeof(MIDIHDR) );
    if ( result != MMSYSERR_NOERROR ) {
      midiInClose( data->inHandle );
      data->inHandle = 0;
      errorString_ = "MidiInWinMM::openPort: error starting Windows MM MIDI input port (AddBuffer).";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }
  }

  result = midiInStart( data->inHandle );
  if ( result != MMSYSERR_NOERROR ) {
    midiInClose( data->inHandle );
    data->inHandle = 0;
    errorString_ = "MidiInWinMM::openPort: error starting Windows MM MIDI input port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  connected_ = true;
}

void MidiInWinMM :: openVirtualPort( const std::string &/*portName*/ )
{
  // This function cannot be implemented for the Windows MM MIDI API.
  errorString_ = "MidiInWinMM::openVirtualPort: cannot be implemented in Windows MM MIDI API!";
  error( RtMidiError::WARNING, errorString_ );
}

void MidiInWinMM :: closePort( void )
{
  if ( connected_ ) {
    WinMidiData *data = static_cast<WinMidiData *> (apiData_);
    EnterCriticalSection( &(data->_mutex) );
    midiInReset( data->inHandle );
    midiInStop( data->inHandle );

    for ( size_t i=0; i < data->sysexBuffer.size(); ++i ) {
      int result = midiInUnprepareHeader(data->inHandle, data->sysexBuffer[i], sizeof(MIDIHDR));
      delete [] data->sysexBuffer[i]->lpData;
      delete [] data->sysexBuffer[i];
      if ( result != MMSYSERR_NOERROR ) {
        midiInClose( data->inHandle );
        data->inHandle = 0;
        errorString_ = "MidiInWinMM::openPort: error closing Windows MM MIDI input port (midiInUnprepareHeader).";
        error( RtMidiError::DRIVER_ERROR, errorString_ );
        return;
      }
    }

    midiInClose( data->inHandle );
    data->inHandle = 0;
    connected_ = false;
    LeaveCriticalSection( &(data->_mutex) );
  }
}

void MidiInWinMM :: setClientName ( const std::string& )
{

  errorString_ = "MidiInWinMM::setClientName: this function is not implemented for the WINDOWS_MM API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiInWinMM :: setPortName ( const std::string& )
{

  errorString_ = "MidiInWinMM::setPortName: this function is not implemented for the WINDOWS_MM API!";
  error( RtMidiError::WARNING, errorString_ );

}

unsigned int MidiInWinMM :: getPortCount()
{
  return midiInGetNumDevs();
}

std::string MidiInWinMM :: getPortName( unsigned int portNumber )
{
  std::string stringName;
  unsigned int nDevices = midiInGetNumDevs();
  if ( portNumber >= nDevices ) {
    std::ostringstream ost;
    ost << "MidiInWinMM::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
    return stringName;
  }

  MIDIINCAPS deviceCaps;
  midiInGetDevCaps( portNumber, &deviceCaps, sizeof(MIDIINCAPS));
  stringName = ConvertToUTF8( deviceCaps.szPname );

  // Next lines added to add the portNumber to the name so that
  // the device's names are sure to be listed with individual names
  // even when they have the same brand name
#ifndef RTMIDI_DO_NOT_ENSURE_UNIQUE_PORTNAMES
  std::ostringstream os;
  os << " ";
  os << portNumber;
  stringName += os.str();
#endif

  return stringName;
}

//*********************************************************************//
//  API: Windows MM
//  Class Definitions: MidiOutWinMM
//*********************************************************************//

MidiOutWinMM :: MidiOutWinMM( const std::string &clientName ) : MidiOutApi()
{
  MidiOutWinMM::initialize( clientName );
}

MidiOutWinMM :: ~MidiOutWinMM()
{
  // Close a connection if it exists.
  MidiOutWinMM::closePort();

  // Cleanup.
  WinMidiData *data = static_cast<WinMidiData *> (apiData_);
  delete data;
}

void MidiOutWinMM :: initialize( const std::string& /*clientName*/ )
{
  // We'll issue a warning here if no devices are available but not
  // throw an error since the user can plug something in later.
  unsigned int nDevices = midiOutGetNumDevs();
  if ( nDevices == 0 ) {
    errorString_ = "MidiOutWinMM::initialize: no MIDI output devices currently available.";
    error( RtMidiError::WARNING, errorString_ );
  }

  // Save our api-specific connection information.
  WinMidiData *data = (WinMidiData *) new WinMidiData;
  apiData_ = (void *) data;
}

unsigned int MidiOutWinMM :: getPortCount()
{
  return midiOutGetNumDevs();
}

std::string MidiOutWinMM :: getPortName( unsigned int portNumber )
{
  std::string stringName;
  unsigned int nDevices = midiOutGetNumDevs();
  if ( portNumber >= nDevices ) {
    std::ostringstream ost;
    ost << "MidiOutWinMM::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
    return stringName;
  }

  MIDIOUTCAPS deviceCaps;
  midiOutGetDevCaps( portNumber, &deviceCaps, sizeof( MIDIOUTCAPS ) );
  stringName = ConvertToUTF8( deviceCaps.szPname );

  // Next lines added to add the portNumber to the name so that
  // the device's names are sure to be listed with individual names
  // even when they have the same brand name
  std::ostringstream os;
#ifndef RTMIDI_DO_NOT_ENSURE_UNIQUE_PORTNAMES
  os << " ";
  os << portNumber;
  stringName += os.str();
#endif

  return stringName;
}

void MidiOutWinMM :: openPort( unsigned int portNumber, const std::string &/*portName*/ )
{
  if ( connected_ ) {
    errorString_ = "MidiOutWinMM::openPort: a valid connection already exists!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  unsigned int nDevices = midiOutGetNumDevs();
  if ( nDevices < 1 ) {
    errorString_ = "MidiOutWinMM::openPort: no MIDI output destinations found!";
    error( RtMidiError::NO_DEVICES_FOUND, errorString_ );
    return;
  }

  if ( portNumber >= nDevices ) {
    std::ostringstream ost;
    ost << "MidiOutWinMM::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::INVALID_PARAMETER, errorString_ );
    return;
  }

  WinMidiData *data = static_cast<WinMidiData *> (apiData_);
  MMRESULT result = midiOutOpen( &data->outHandle,
                                 portNumber,
                                 (DWORD)NULL,
                                 (DWORD)NULL,
                                 CALLBACK_NULL );
  if ( result != MMSYSERR_NOERROR ) {
    errorString_ = "MidiOutWinMM::openPort: error creating Windows MM MIDI output port.";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  connected_ = true;
}

void MidiOutWinMM :: closePort( void )
{
  if ( connected_ ) {
    WinMidiData *data = static_cast<WinMidiData *> (apiData_);
    // Disabled because midiOutReset triggers 0x7b (if any note was ON) and 0x79 "Reset All
    // Controllers" (to all 16 channels) CC messages which is undesirable (see issue #222)
    // midiOutReset( data->outHandle );

    midiOutClose( data->outHandle );
    data->outHandle = 0;
    connected_ = false;
  }
}

void MidiOutWinMM :: setClientName ( const std::string& )
{

  errorString_ = "MidiOutWinMM::setClientName: this function is not implemented for the WINDOWS_MM API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutWinMM :: setPortName ( const std::string& )
{

  errorString_ = "MidiOutWinMM::setPortName: this function is not implemented for the WINDOWS_MM API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutWinMM :: openVirtualPort( const std::string &/*portName*/ )
{
  // This function cannot be implemented for the Windows MM MIDI API.
  errorString_ = "MidiOutWinMM::openVirtualPort: cannot be implemented in Windows MM MIDI API!";
  error( RtMidiError::WARNING, errorString_ );
}

void MidiOutWinMM :: sendMessage( const unsigned char *message, size_t size )
{
  if ( !connected_ ) return;

  unsigned int nBytes = static_cast<unsigned int>(size);
  if ( nBytes == 0 ) {
    errorString_ = "MidiOutWinMM::sendMessage: message argument is empty!";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  MMRESULT result;
  WinMidiData *data = static_cast<WinMidiData *> (apiData_);
  if ( message[0] == 0xF0 ) { // Sysex message

    // Allocate buffer for sysex data.
    char *buffer = (char *) malloc( nBytes );
    if ( buffer == NULL ) {
      errorString_ = "MidiOutWinMM::sendMessage: error allocating sysex message memory!";
      error( RtMidiError::MEMORY_ERROR, errorString_ );
      return;
    }

    // Copy data to buffer.
    for ( unsigned int i=0; i<nBytes; ++i ) buffer[i] = message[i];

    // Create and prepare MIDIHDR structure.
    MIDIHDR sysex{};
    sysex.lpData = (LPSTR) buffer;
    sysex.dwBufferLength = nBytes;
    sysex.dwFlags = 0;
    result = midiOutPrepareHeader( data->outHandle,  &sysex, sizeof( MIDIHDR ) );
    if ( result != MMSYSERR_NOERROR ) {
      free( buffer );
      errorString_ = "MidiOutWinMM::sendMessage: error preparing sysex header.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }

    // Send the message.
    result = midiOutLongMsg( data->outHandle, &sysex, sizeof( MIDIHDR ) );
    if ( result != MMSYSERR_NOERROR ) {
      free( buffer );
      errorString_ = "MidiOutWinMM::sendMessage: error sending sysex message.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
      return;
    }

    // Unprepare the buffer and MIDIHDR.
    while ( MIDIERR_STILLPLAYING == midiOutUnprepareHeader( data->outHandle, &sysex, sizeof ( MIDIHDR ) ) ) Sleep( 1 );
    free( buffer );
  }
  else { // Channel or system message.

    // Make sure the message size isn't too big.
    if ( nBytes > 3 ) {
      errorString_ = "MidiOutWinMM::sendMessage: message size is greater than 3 bytes (and not sysex)!";
      error( RtMidiError::WARNING, errorString_ );
      return;
    }

    // Pack MIDI bytes into double word.
    DWORD packet;
    unsigned char *ptr = (unsigned char *) &packet;
    for ( unsigned int i=0; i<nBytes; ++i ) {
      *ptr = message[i];
      ++ptr;
    }

    // Send the message immediately.
    result = midiOutShortMsg( data->outHandle, packet );
    if ( result != MMSYSERR_NOERROR ) {
      errorString_ = "MidiOutWinMM::sendMessage: error sending MIDI message.";
      error( RtMidiError::DRIVER_ERROR, errorString_ );
    }
  }
}

#endif  // __WINDOWS_MM__


//*********************************************************************//
//  API: Universal Windows Platform (UWP)
//*********************************************************************//

// C++/WinRT
//   https://github.com/microsoft/cppwinrt
//
// UWP MIDI API
//   https://docs.microsoft.com/en-us/windows/uwp/audio-video-camera/midi
//
// Example implementation using UWP MIDI in C++/WinRT
//   https://github.com/trueroad/uwp_midiio

#if defined(__WINDOWS_UWP__)

#include <algorithm>
#include <chrono>
#include <regex>
#include <string_view>
#include <mutex>

#include <windows.h>
#include <winrt/base.h>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.Foundation.Collections.h>
#include <winrt/Windows.Devices.Enumeration.h>
#include <winrt/Windows.Devices.Midi.h>
#include <winrt/Windows.Storage.Streams.h>
#include <winrt/Windows.Security.Cryptography.h>

using namespace winrt;
using namespace Windows::Foundation;
using namespace Windows::Devices::Enumeration;
using namespace Windows::Devices::Midi;
using namespace Windows::Storage::Streams;
using namespace Windows::Security::Cryptography;

// Class for initializing C++/WinRT
class UWPMidiInit
{
public:
    UWPMidiInit()
    {
        winrt::init_apartment();
    }
};

// Class for handling UWP MIDI
class UWPMidiClass
{
public:
    // Structure to store MIDI port name and ID
    struct port
    {
        std::string name;
        std::wstring id;
        std::string hex_id;
        std::string display_name;
    };

    UWPMidiClass(MidiApi& midi_api) :
        midi_api_(midi_api)
    {
    }

    ~UWPMidiClass()
    {
        close();
    }

    // Initialize for MIDI IN
    void in_init(MidiInApi::RtMidiInData* input_data)
    {
        input_data_ = input_data;

        try
        {
            ports_ = list_ports(MidiInPort::GetDeviceSelector());
        }
        catch (hresult_error const& ex)
        {
            raise_hresult_error("UWPMidiClass::in_init: ", ex);
        }
        sort_display_name(ports_);
    }

    // Initialize for MIDI OUT
    void out_init()
    {
        try
        {
            ports_ = list_ports(MidiOutPort::GetDeviceSelector());
            fix_display_name(list_ports(MidiInPort::GetDeviceSelector()), ports_);
        }
        catch (hresult_error const& ex)
        {
            raise_hresult_error("UWPMidiClass::out_init: ", ex);
        }
        sort_display_name(ports_);
    }

    size_t get_num_ports()
    {
        return ports_.size();
    }

    std::string get_port_name(size_t n)
    {
        return ports_[n].display_name;
    }

    bool in_open(size_t port_number);
    bool out_open(size_t port_number);
    void close();

    void midi_in_callback(const MidiInPort&, const MidiMessageReceivedEventArgs& e);
    bool send_buffer(const unsigned char* buff, size_t len);

    // Raise RtMidi error for hresult error
    void raise_hresult_error(std::string_view message, hresult_error const& ex)
    {
        std::ostringstream ss;
        ss << message << "exception HRESULT 0x" << std::hex << ex.code() << ", "
            << utf16_to_utf8(static_cast<std::wstring_view>(ex.message()))
            << "\n";
        midi_api_.error(RtMidiError::DRIVER_ERROR, ss.str());
    }

    // Mutex for MIDI port open/close
    std::mutex mtx_open_close_;
    // Mutex for MIDI IN message queue access
    std::mutex mtx_queue_;

private:
    std::vector<port> list_ports(winrt::hstring device_selector);
    void fix_display_name(const std::vector<port>& in_ports,
        std::vector<port>& out_ports);
    void sort_display_name(std::vector<port>& ports);
    std::string utf16_to_utf8(const std::wstring_view wstr);

    template<class MidiPort_T, class IMidiPort_T>
    IMidiPort_T open(size_t port_number);

    // MidiApi class
    MidiApi& midi_api_;

    // List of MIDI ports
    std::vector<port> ports_;
    // MIDI IN port
    MidiInPort in_port_{ nullptr };
    // MIDI OUT port
    IMidiOutPort out_port_{ nullptr };
    // Backup initial MessageReceived event token
    winrt::event_token before_token_;
    // Input data
    MidiInApi::RtMidiInData* input_data_{ nullptr };
    // Last timestamp
    std::chrono::duration<TimeSpan::rep, TimeSpan::period> last_time_{ 0 };

    // C++/WinRT initializer
    static UWPMidiInit uwp_midi_init_;
    // Regex pattern to extract 8 hex digits from UWP MIDI ID string
    static const std::wregex hex_id_pattern_;

#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
    // QueryPerformanceFrequency
    LONGLONG qpc_freq_{ 0 };
    // Last QueryPerformanceCounter
    LONGLONG before_qpc_;
    // Weather overflow low occurred or not
    bool b_overflow_low_{ false };

    // BLE-MIDI timestamp periods
    inline constexpr static std::chrono::duration<TimeSpan::rep, TimeSpan::period> ble_midi_period_low_{ std::chrono::milliseconds{128} };
    inline constexpr static std::chrono::duration<TimeSpan::rep, TimeSpan::period> ble_midi_period_high_{ std::chrono::milliseconds{8192} };
    // QPC threshold 4096 ms
    inline constexpr static LONGLONG qpc_threshold_{ 4096 };

    // Regex pattern to detect BLE-MIDI IN
    static const std::wregex ble_midi_pattern_;
#endif
};

// C++/WinRT initializer
UWPMidiInit UWPMidiClass::uwp_midi_init_;
// Regex pattern to extract 8 hex digits from UWP MIDI ID string
const std::wregex UWPMidiClass::hex_id_pattern_{ std::wregex(L"#MIDII_([0-9A-F]{8})\\..+#") };

#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
const std::wregex UWPMidiClass::ble_midi_pattern_{ std::wregex(L"#MIDII_[0-9A-F]{8}\\.BLE[0-9]{2}#") };
#endif

// Find and create a list of UWP MIDI ports
std::vector<UWPMidiClass::port> UWPMidiClass::list_ports(winrt::hstring device_selector)
{
    const auto devs{ DeviceInformation::FindAllAsync(device_selector).get() };

    std::vector<port> retval;
    for (const auto& d : devs)
    {
        port p;
        p.name = utf16_to_utf8(d.Name());
        p.id = d.Id();

        std::wsmatch m;
        if (std::regex_search(p.id, m, hex_id_pattern_))
        {
            // Ordinary MIDI ports
            // Append hex digits extracted from the UWP MIDI ID string to the port name.
            p.hex_id = utf16_to_utf8(m[1].str());

            std::ostringstream ss;
            ss << p.name
                << " [ "
                << p.hex_id
                << " ]";
            p.display_name = ss.str();
        }
        else
        {
            // Microsoft GS Wavetable Synth etc.
            // Unable to extract hex digits from UWP MIDI ID string.
            // Use the device name as the port name.
            p.display_name = p.name;
        }

        retval.push_back(p);
    }
    return retval;
}

// Fix MIDI OUT port names starting with `MIDI` to MIDI IN port names with similar ID strings
void UWPMidiClass::fix_display_name(const std::vector<port>& in_ports,
    std::vector<port>& out_ports)
{
    for (auto& outp : out_ports)
    {
        if (outp.hex_id.empty() ||
            std::string_view{ outp.name }.substr(0, 4) != "MIDI")
            continue;

        for (const auto& inp : in_ports)
        {
            if (outp.hex_id == inp.hex_id)
            {
                outp.display_name = inp.display_name;
                break;
            }
        }
    }
}

void UWPMidiClass::sort_display_name(std::vector<port>& ports)
{
    std::sort(ports.begin(), ports.end(),
        [](const auto& lhs, const auto& rhs)
    {
        return lhs.display_name < rhs.display_name;
    });
}

std::string UWPMidiClass::utf16_to_utf8(const std::wstring_view wstr)
{
    auto len{ WideCharToMultiByte(CP_UTF8, 0, wstr.data(), static_cast<int>(wstr.size()), nullptr, 0, nullptr, nullptr) };
    std::string u8str(len, '\0');
    if (len)
        WideCharToMultiByte(CP_UTF8, 0, wstr.data(), static_cast<int>(wstr.size()), u8str.data(), len, nullptr, nullptr);
    return u8str;
}

// Open MIDI IN/OUT port
template<class MidiPort_T, class IMidiPort_T>
IMidiPort_T UWPMidiClass::open(size_t port_number)
{
    try
    {
        auto async{ MidiPort_T::FromIdAsync(ports_[port_number].id) };
        // Timeout 3 seconds
        if (async.wait_for(std::chrono::seconds(3)) == AsyncStatus::Completed)
            return async.GetResults();
    }
    catch (hresult_error const& ex)
    {
        raise_hresult_error("UWPMidiClass::open: ", ex);
    }
    return nullptr;
}

// Open MIDI IN port
bool UWPMidiClass::in_open(size_t port_number)
{
    if (in_port_)
        in_port_.Close();

    in_port_ = open<MidiInPort, MidiInPort>(port_number);
    if (!in_port_)
        return false;

#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
    std::wsmatch m;
    if (std::regex_search(ports_[port_number].id, m, ble_midi_pattern_))
    {
        // BLE-MIDI IN port
        LARGE_INTEGER li;
        if (::QueryPerformanceFrequency(&li))
            qpc_freq_ = li.QuadPart;
    }
#endif

    try
    {
        before_token_ = in_port_.MessageReceived({ this, &UWPMidiClass::midi_in_callback });
    }
    catch (hresult_error const& ex)
    {
        raise_hresult_error("UWPMidiClass::in_open: ", ex);
    }

    return true;
}

// Open MIDI Out port
bool UWPMidiClass::out_open(size_t port_number)
{
    if (out_port_)
        out_port_.Close();

    out_port_ = open<MidiOutPort, IMidiOutPort>(port_number);
    if (!out_port_)
        return false;

    return true;
}

// Close MIDI IN/OUT port
void UWPMidiClass::close()
{
    if (in_port_)
    {
        if (before_token_)
            in_port_.MessageReceived(before_token_);

        in_port_.Close();
        in_port_ = nullptr;
    }
    if (out_port_)
    {
        out_port_.Close();
        out_port_ = nullptr;
    }
}

// MessageReceived event handler
void UWPMidiClass::midi_in_callback(const MidiInPort&, const MidiMessageReceivedEventArgs& e)
{
#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
    LARGE_INTEGER qpc;
    if (qpc_freq_)
    {
        if (!::QueryPerformanceCounter(&qpc))
            qpc_freq_ = 0;
    }
#endif

    const auto& m{ e.Message() };
    if (!m)
        return;

    MidiInApi::MidiMessage message;
    const std::chrono::duration<TimeSpan::rep, TimeSpan::period> duration{ m.Timestamp() };

    // Calculate time stamp.
    if (input_data_->firstMessage == true)
    {
        message.timeStamp = 0.0;
        input_data_->firstMessage = false;
        last_time_ = duration;

#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
        if (qpc_freq_)
            before_qpc_ = qpc.QuadPart;
#endif
    }
    else
    {
        auto delta{ duration - last_time_ };

#ifndef RTMIDI_DO_NOT_ENABLE_WORKAROUND_UWP_WRONG_TIMESTAMPS
        if (qpc_freq_)
        {
            if (b_overflow_low_)
            {
                if (delta >= ble_midi_period_low_)
                {
                    // Fix after overflow low
                    // https://github.com/trueroad/BLE_MIDI_packet_data_set#page-7-overflow-both
                    delta -= ble_midi_period_low_;
                    b_overflow_low_ = false;
                }
            }
            else
            {
                if ((ble_midi_period_high_ - ble_midi_period_low_) < delta && delta < ble_midi_period_high_ &&
                    ((before_qpc_ - qpc.QuadPart) * 1000 / qpc_freq_) < qpc_threshold_)
                {
                    // Fix overflow low
                    // https://github.com/trueroad/BLE_MIDI_packet_data_set#page-7-overflow-low
                    delta = delta - ble_midi_period_high_ + ble_midi_period_low_;
                    b_overflow_low_ = true;
                }
            }

            before_qpc_ = qpc.QuadPart;
        }
#endif

        const std::chrono::duration<double> sec{ delta };
        message.timeStamp = sec.count();
    }

    if (((input_data_->ignoreFlags & 0x01) &&
            (m.Type() == MidiMessageType::SystemExclusive || m.Type() == MidiMessageType::EndSystemExclusive)) ||
        ((input_data_->ignoreFlags & 0x02) &&
            (m.Type() == MidiMessageType::MidiTimeCode || m.Type() == MidiMessageType::TimingClock)) ||
        ((input_data_->ignoreFlags & 0x04) &&
            m.Type() == MidiMessageType::ActiveSensing))
    {
        return;
    }

    const auto& raw_data{ m.RawData() };
    const size_t len{ raw_data.Length() };

    if (len)
        message.bytes.assign(raw_data.data(), raw_data.data() + len);

    last_time_ = duration;

    if (input_data_->usingCallback)
    {
        (input_data_->userCallback)(message.timeStamp, &message.bytes, input_data_->userData);
    }
    else
    {
        std::lock_guard<std::mutex> lock(mtx_queue_);

        if (!input_data_->queue.push(message))
        {
            std::cerr << "\nMidiInWinUWP: message queue limit reached!!\n\n";
        }
    }
}

// Send MIDI message
bool UWPMidiClass::send_buffer(const unsigned char* buff, size_t len)
{
    if (!out_port_)
        return false;

    try
    {
        out_port_.SendBuffer(CryptographicBuffer::CreateFromByteArray(array_view(buff, buff + len)));
    }
    catch (hresult_error const& ex)
    {
        raise_hresult_error("UWPMidiClass::send_buffer: ", ex);
    }

    return true;
}

//*********************************************************************//
//  API: Windows UWP
//  Class Definitions: MidiInWinUWP
//*********************************************************************//

MidiInWinUWP::MidiInWinUWP(const std::string& clientName, unsigned int queueSizeLimit)
    : MidiInApi(queueSizeLimit)
{
    MidiInWinUWP::initialize(clientName);
}

MidiInWinUWP :: ~MidiInWinUWP()
{
    // Close a connection if it exists.
    MidiInWinUWP::closePort();

    // Cleanup.
    UWPMidiClass *data = static_cast<UWPMidiClass*> (apiData_);
    delete data;
}

void MidiInWinUWP::initialize(const std::string& /*clientName*/)
{
    // Save our api-specific connection information.
    UWPMidiClass* data{ new UWPMidiClass(*this) };
    data->in_init(&inputData_);
    apiData_ = static_cast<void*>(data);

    // We'll issue a warning here if no devices are available but not
    // throw an error since the user can plugin something later.
    const auto nDevices{ data->get_num_ports() };
    if (nDevices == 0)
    {
        errorString_ = "MidiInWinUWP::initialize: no MIDI input devices currently available.";
        error(RtMidiError::WARNING, errorString_);
    }
}

void MidiInWinUWP::openPort(unsigned int portNumber, const std::string&/*portName*/)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    std::lock_guard<std::mutex> lock(data->mtx_open_close_);

    if (connected_)
    {
        errorString_ = "MidiInWinUWP::openPort: a valid connection already exists!";
        error(RtMidiError::WARNING, errorString_);
        return;
    }

    if (data->get_num_ports() == 0)
    {
        errorString_ = "MidiInWinUWP::openPort: no MIDI input sources found!";
        error(RtMidiError::NO_DEVICES_FOUND, errorString_);
        return;
    }

    if (portNumber >= data->get_num_ports())
    {
        std::ostringstream ost;
        ost << "MidiInWinUWP::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
        errorString_ = ost.str();
        error(RtMidiError::INVALID_PARAMETER, errorString_);
        return;
    }

    if (!data->in_open(portNumber))
    {
        errorString_ = "MidiInWinUWP::openPort: error creating Windows UWP MIDI input port.";
        error(RtMidiError::DRIVER_ERROR, errorString_);
        return;
    }

    connected_ = true;
}

void MidiInWinUWP::openVirtualPort(const std::string&/*portName*/)
{
    // This function cannot be implemented for the Windows UWP MIDI API.
    errorString_ = "MidiInWinUWP::openVirtualPort: cannot be implemented in Windows UWP MIDI API!";
    error(RtMidiError::WARNING, errorString_);
}

void MidiInWinUWP::closePort(void)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    std::lock_guard<std::mutex> lock(data->mtx_open_close_);

    if (connected_)
    {
        data->close();
        connected_ = false;
    }
}

void MidiInWinUWP::setClientName(const std::string&)
{
    errorString_ = "MidiInWinUWP::setClientName: this function is not implemented for the WINDOWS_UWP API!";
    error(RtMidiError::WARNING, errorString_);
}

void MidiInWinUWP::setPortName(const std::string&)
{
    errorString_ = "MidiInWinUWP::setPortName: this function is not implemented for the WINDOWS_UWP API!";
    error(RtMidiError::WARNING, errorString_);
}

unsigned int MidiInWinUWP::getPortCount()
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    return static_cast<unsigned int>(data->get_num_ports());
}

std::string MidiInWinUWP::getPortName(unsigned int portNumber)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };

    const auto nDevices{ data->get_num_ports() };
    if (portNumber >= nDevices)
    {
        std::ostringstream ost;
        ost << "MidiInWinUWP::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
        errorString_ = ost.str();
        error(RtMidiError::WARNING, errorString_);
        return "";
    }

    return data->get_port_name(portNumber);
}

double MidiInWinUWP::getMessage(std::vector<unsigned char>* message)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    std::lock_guard<std::mutex> lock(data->mtx_queue_);

    return MidiInApi::getMessage(message);
}

//*********************************************************************//
//  API: Windows UWP
//  Class Definitions: MidiOutWinUWP
//*********************************************************************//

MidiOutWinUWP::MidiOutWinUWP(const std::string& clientName) : MidiOutApi()
{
    MidiOutWinUWP::initialize(clientName);
}

MidiOutWinUWP :: ~MidiOutWinUWP()
{
    // Close a connection if it exists.
    MidiOutWinUWP::closePort();

    // Cleanup.
    UWPMidiClass* data = static_cast<UWPMidiClass*> (apiData_);
    delete data;
}

void MidiOutWinUWP::initialize(const std::string& /*clientName*/)
{
    // Save our api-specific connection information.
    UWPMidiClass* data{ new UWPMidiClass(*this) };
    data->out_init();
    apiData_ = static_cast<void*>(data);

    // We'll issue a warning here if no devices are available but not
    // throw an error since the user can plug something in later.
    const auto nDevices{ data->get_num_ports() };
    if (nDevices == 0)
    {
        errorString_ = "MidiOutWinUWP::initialize: no MIDI output devices currently available.";
        error(RtMidiError::WARNING, errorString_);
    }
}

unsigned int MidiOutWinUWP::getPortCount()
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    return static_cast<unsigned int>(data->get_num_ports());
}

std::string MidiOutWinUWP::getPortName(unsigned int portNumber)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };

    const auto nDevices{ data->get_num_ports() };
    if (portNumber >= nDevices)
    {
        std::ostringstream ost;
        ost << "MidiOutWinUWP::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
        errorString_ = ost.str();
        error(RtMidiError::WARNING, errorString_);
        return "";
    }

    return data->get_port_name(portNumber);
}

void MidiOutWinUWP::openPort(unsigned int portNumber, const std::string&/*portName*/)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    std::lock_guard<std::mutex> lock(data->mtx_open_close_);

    if (connected_)
    {
        errorString_ = "MidiOutWinUWP::openPort: a valid connection already exists!";
        error(RtMidiError::WARNING, errorString_);
        return;
    }

    if (data->get_num_ports() == 0)
    {
        errorString_ = "MidiOutWinUWP::openPort: no MIDI output destinations found!";
        error(RtMidiError::NO_DEVICES_FOUND, errorString_);
        return;
    }

    if (portNumber >= data->get_num_ports())
    {
        std::ostringstream ost;
        ost << "MidiOutWinUWP::openPort: the 'portNumber' argument (" << portNumber << ") is invalid.";
        errorString_ = ost.str();
        error(RtMidiError::INVALID_PARAMETER, errorString_);
        return;
    }

    if (!data->out_open(portNumber))
    {
        errorString_ = "MidiOutWinUWP::openPort: error creating Windows UWP MIDI output port.";
        error(RtMidiError::DRIVER_ERROR, errorString_);
        return;
    }

    connected_ = true;
}

void MidiOutWinUWP::closePort(void)
{
    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    std::lock_guard<std::mutex> lock(data->mtx_open_close_);

    if (connected_)
    {
        data->close();
        connected_ = false;
    }
}

void MidiOutWinUWP::setClientName(const std::string&)
{
    errorString_ = "MidiOutWinUWP::setClientName: this function is not implemented for the WINDOWS_UWP API!";
    error(RtMidiError::WARNING, errorString_);
}

void MidiOutWinUWP::setPortName(const std::string&)
{
    errorString_ = "MidiOutWinUWP::setPortName: this function is not implemented for the WINDOWS_UWP API!";
    error(RtMidiError::WARNING, errorString_);
}

void MidiOutWinUWP::openVirtualPort(const std::string&/*portName*/)
{
    // This function cannot be implemented for the Windows UWP MIDI API.
    errorString_ = "MidiOutWinUWP::openVirtualPort: cannot be implemented in Windows UWP MIDI API!";
    error(RtMidiError::WARNING, errorString_);
}

void MidiOutWinUWP::sendMessage(const unsigned char* message, size_t size)
{
    if (!connected_)
        return;

    if (size == 0)
    {
        errorString_ = "MidiOutWinUWP::sendMessage: message argument is empty!";
        error(RtMidiError::WARNING, errorString_);
        return;
    }

    UWPMidiClass* data{ static_cast<UWPMidiClass*>(apiData_) };
    if (!data->send_buffer(message, size))
    {
        errorString_ = "MidiOutWinUWP::sendMessage: error sending message.";
        error(RtMidiError::DRIVER_ERROR, errorString_);
    }
}

#endif  // __WINDOWS_UWP__


//*********************************************************************//
//  API: UNIX JACK
//
//  Written primarily by Alexander Svetalkin, with updates for delta
//  time by Gary Scavone, April 2011.
//
//  *********************************************************************//

#if defined(__UNIX_JACK__)

// JACK header files
#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/ringbuffer.h>
#include <pthread.h>
#include <sched.h>
#ifdef HAVE_SEMAPHORE
  #include <semaphore.h>
#endif

#define JACK_RINGBUFFER_SIZE 16384 // Default size for ringbuffer

struct JackMidiData {
  jack_client_t *client;
  jack_port_t *port;
  jack_ringbuffer_t *buff;
  int buffMaxWrite; // actual writable size, usually 1 less than ringbuffer
  jack_time_t lastTime;
#ifdef HAVE_SEMAPHORE
  sem_t sem_cleanup;
  sem_t sem_needpost;
#endif
  MidiInApi :: RtMidiInData *rtMidiIn;
  };

//*********************************************************************//
//  API: JACK
//  Class Definitions: MidiInJack
//*********************************************************************//

static int jackProcessIn( jack_nframes_t nframes, void *arg )
{
  JackMidiData *jData = (JackMidiData *) arg;
  MidiInApi :: RtMidiInData *rtData = jData->rtMidiIn;
  jack_midi_event_t event;
  jack_time_t time;

  // Is port created?
  if ( jData->port == NULL ) return 0;

  void *buff = jack_port_get_buffer( jData->port, nframes );
  bool& continueSysex = rtData->continueSysex;
  unsigned char& ignoreFlags = rtData->ignoreFlags;

  // We have midi events in buffer
  int evCount = jack_midi_get_event_count( buff );
  for (int j = 0; j < evCount; j++) {
    MidiInApi::MidiMessage& message = rtData->message;
    jack_midi_event_get( &event, buff, j );

    // Compute the delta time.
    time = jack_get_time();
    if ( rtData->firstMessage == true ) {
      message.timeStamp = 0.0;
      rtData->firstMessage = false;
    } else
      message.timeStamp = ( time - jData->lastTime ) * 0.000001;

    jData->lastTime = time;

    if ( !continueSysex )
      message.bytes.clear();

    if ( !( ( continueSysex || event.buffer[0] == 0xF0 ) && ( ignoreFlags & 0x01 ) ) ) {
      // Unless this is a (possibly continued) SysEx message and we're ignoring SysEx,
      // copy the event buffer into the MIDI message struct.
      for ( unsigned int i = 0; i < event.size; i++ )
        message.bytes.push_back( event.buffer[i] );
    }

    switch ( event.buffer[0] ) {
      case 0xF0:
        // Start of a SysEx message
        continueSysex = event.buffer[event.size - 1] != 0xF7;
        if ( ignoreFlags & 0x01 ) continue;
        break;
      case 0xF1:
      case 0xF8:
        // MIDI Time Code or Timing Clock message
        if ( ignoreFlags & 0x02 ) continue;
        break;
      case 0xFE:
        // Active Sensing message
        if ( ignoreFlags & 0x04 ) continue;
        break;
      default:
        if ( continueSysex ) {
          // Continuation of a SysEx message
          continueSysex = event.buffer[event.size - 1] != 0xF7;
          if ( ignoreFlags & 0x01 ) continue;
        }
        // All other MIDI messages
    }

    if ( !continueSysex ) {
      // If not a continuation of a SysEx message,
      // invoke the user callback function or queue the message.
      if ( rtData->usingCallback ) {
        RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) rtData->userCallback;
        callback( message.timeStamp, &message.bytes, rtData->userData );
      }
      else {
        // As long as we haven't reached our queue size limit, push the message.
        if ( !rtData->queue.push( message ) )
          std::cerr << "\nMidiInJack: message queue limit reached!!\n\n";
      }
    }
  }

  return 0;
}

MidiInJack :: MidiInJack( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit )
{
  MidiInJack::initialize( clientName );
}

void MidiInJack :: initialize( const std::string& clientName )
{
  JackMidiData *data = new JackMidiData;
  apiData_ = (void *) data;

  data->rtMidiIn = &inputData_;
  data->port = NULL;
  data->client = NULL;
  this->clientName = clientName;

  connect();
}

void MidiInJack :: connect()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  if ( data->client )
    return;

  // Initialize JACK client
  if (( data->client = jack_client_open( clientName.c_str(), JackNoStartServer, NULL )) == 0) {
    errorString_ = "MidiInJack::initialize: JACK server not running?";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  jack_set_process_callback( data->client, jackProcessIn, data );
  jack_activate( data->client );
}

MidiInJack :: ~MidiInJack()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  MidiInJack::closePort();

  if ( data->client )
    jack_client_close( data->client );
  delete data;
}

void MidiInJack :: openPort( unsigned int portNumber, const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  connect();

  // Creating new port
  if ( data->port == NULL )
    data->port = jack_port_register( data->client, portName.c_str(),
                                     JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0 );

  if ( data->port == NULL ) {
    errorString_ = "MidiInJack::openPort: JACK error creating port";
    if (portName.size() >= (size_t)jack_port_name_size())
        errorString_ += " (port name too long?)";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Connecting to the output
  std::string name = getPortName( portNumber );
  jack_connect( data->client, name.c_str(), jack_port_name( data->port ) );

  connected_ = true;
}

void MidiInJack :: openVirtualPort( const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  connect();
  if ( data->port == NULL )
    data->port = jack_port_register( data->client, portName.c_str(),
                                     JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0 );

  if ( data->port == NULL ) {
    errorString_ = "MidiInJack::openVirtualPort: JACK error creating virtual port";
    if (portName.size() >= (size_t)jack_port_name_size())
        errorString_ += " (port name too long?)";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
  }
}

unsigned int MidiInJack :: getPortCount()
{
  int count = 0;
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  connect();
  if ( !data->client )
    return 0;

  // List of available ports
  const char **ports = jack_get_ports( data->client, NULL, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput );

  if ( ports == NULL ) return 0;
  while ( ports[count] != NULL )
    count++;

  free( ports );

  return count;
}

std::string MidiInJack :: getPortName( unsigned int portNumber )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  std::string retStr( "" );

  connect();

  // List of available ports
  const char **ports = jack_get_ports( data->client, NULL,
                                       JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput );

  // Check port validity
  if ( ports == NULL ) {
    errorString_ = "MidiInJack::getPortName: no ports available!";
    error( RtMidiError::WARNING, errorString_ );
    return retStr;
  }

  unsigned int i;
  for ( i=0; i<portNumber && ports[i]; i++ ) {}
  if ( i < portNumber || !ports[portNumber] ) {
    std::ostringstream ost;
    ost << "MidiInJack::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
  }
  else retStr.assign( ports[portNumber] );

  jack_free( ports );
  return retStr;
}

void MidiInJack :: closePort()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  if ( data->port == NULL ) return;
  jack_port_unregister( data->client, data->port );
  data->port = NULL;

  connected_ = false;
}

void MidiInJack:: setClientName( const std::string& )
{

  errorString_ = "MidiInJack::setClientName: this function is not implemented for the UNIX_JACK API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiInJack :: setPortName( const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
#ifdef JACK_HAS_PORT_RENAME
  jack_port_rename( data->client, data->port, portName.c_str() );
#else
  jack_port_set_name( data->port, portName.c_str() );
#endif
}

//*********************************************************************//
//  API: JACK
//  Class Definitions: MidiOutJack
//*********************************************************************//

// Jack process callback
static int jackProcessOut( jack_nframes_t nframes, void *arg )
{
  JackMidiData *data = (JackMidiData *) arg;
  jack_midi_data_t *midiData;
  int space;

  // Is port created?
  if ( data->port == NULL ) return 0;

  void *buff = jack_port_get_buffer( data->port, nframes );
  jack_midi_clear_buffer( buff );

  while ( jack_ringbuffer_peek( data->buff, (char *) &space, sizeof( space ) ) == sizeof(space) &&
          jack_ringbuffer_read_space( data->buff ) >= sizeof(space) + space ) {
    jack_ringbuffer_read_advance( data->buff, sizeof(space) );

    midiData = jack_midi_event_reserve( buff, 0, space );
    if ( midiData )
        jack_ringbuffer_read( data->buff, (char *) midiData, (size_t) space );
    else
        jack_ringbuffer_read_advance( data->buff, (size_t) space );
  }

#ifdef HAVE_SEMAPHORE
  if ( !sem_trywait( &data->sem_needpost ) )
    sem_post( &data->sem_cleanup );
#endif

  return 0;
}

MidiOutJack :: MidiOutJack( const std::string &clientName ) : MidiOutApi()
{
  MidiOutJack::initialize( clientName );
}

void MidiOutJack :: initialize( const std::string& clientName )
{
  JackMidiData *data = new JackMidiData;
  apiData_ = (void *) data;

  data->port = NULL;
  data->client = NULL;
#ifdef HAVE_SEMAPHORE
  sem_init( &data->sem_cleanup, 0, 0 );
  sem_init( &data->sem_needpost, 0, 0 );
#endif
  this->clientName = clientName;

  connect();
}

void MidiOutJack :: connect()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  if ( data->client )
    return;

  // Initialize output ringbuffers
  data->buff = jack_ringbuffer_create( JACK_RINGBUFFER_SIZE );
  data->buffMaxWrite = (int) jack_ringbuffer_write_space( data->buff );

  // Initialize JACK client
  if ( ( data->client = jack_client_open( clientName.c_str(), JackNoStartServer, NULL ) ) == 0 ) {
    errorString_ = "MidiOutJack::initialize: JACK server not running?";
    error( RtMidiError::WARNING, errorString_ );
    return;
  }

  jack_set_process_callback( data->client, jackProcessOut, data );
  jack_activate( data->client );
}

MidiOutJack :: ~MidiOutJack()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  MidiOutJack::closePort();

  // Cleanup
  jack_ringbuffer_free( data->buff );
  if ( data->client ) {
    jack_client_close( data->client );
  }

#ifdef HAVE_SEMAPHORE
  sem_destroy( &data->sem_cleanup );
  sem_destroy( &data->sem_needpost );
#endif

  delete data;
}

void MidiOutJack :: openPort( unsigned int portNumber, const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  connect();

  // Creating new port
  if ( data->port == NULL )
    data->port = jack_port_register( data->client, portName.c_str(),
                                     JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0 );

  if ( data->port == NULL ) {
    errorString_ = "MidiOutJack::openPort: JACK error creating port";
    if (portName.size() >= (size_t)jack_port_name_size())
        errorString_ += " (port name too long?)";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
    return;
  }

  // Connecting to the output
  std::string name = getPortName( portNumber );
  jack_connect( data->client, jack_port_name( data->port ), name.c_str() );

  connected_ = true;
}

void MidiOutJack :: openVirtualPort( const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  connect();
  if ( data->port == NULL )
    data->port = jack_port_register( data->client, portName.c_str(),
                                     JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0 );

  if ( data->port == NULL ) {
    errorString_ = "MidiOutJack::openVirtualPort: JACK error creating virtual port";
    if (portName.size() >= (size_t)jack_port_name_size())
        errorString_ += " (port name too long?)";
    error( RtMidiError::DRIVER_ERROR, errorString_ );
  }
}

unsigned int MidiOutJack :: getPortCount()
{
  int count = 0;
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  connect();
  if ( !data->client )
    return 0;

  // List of available ports
  const char **ports = jack_get_ports( data->client, NULL,
                                       JACK_DEFAULT_MIDI_TYPE, JackPortIsInput );

  if ( ports == NULL ) return 0;
  while ( ports[count] != NULL )
    count++;

  free( ports );

  return count;
}

std::string MidiOutJack :: getPortName( unsigned int portNumber )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
  std::string retStr("");

  connect();

  // List of available ports
  const char **ports = jack_get_ports( data->client, NULL,
                                       JACK_DEFAULT_MIDI_TYPE, JackPortIsInput );

  // Check port validity
  if ( ports == NULL ) {
    errorString_ = "MidiOutJack::getPortName: no ports available!";
    error( RtMidiError::WARNING, errorString_ );
    return retStr;
  }

  if ( ports[portNumber] == NULL ) {
    std::ostringstream ost;
    ost << "MidiOutJack::getPortName: the 'portNumber' argument (" << portNumber << ") is invalid.";
    errorString_ = ost.str();
    error( RtMidiError::WARNING, errorString_ );
  }
  else retStr.assign( ports[portNumber] );

  free( ports );
  return retStr;
}

void MidiOutJack :: closePort()
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  if ( data->port == NULL ) return;

#ifdef HAVE_SEMAPHORE
  struct timespec ts;
  if ( clock_gettime( CLOCK_REALTIME, &ts ) != -1 ) {
    ts.tv_sec += 1; // wait max one second
    sem_post( &data->sem_needpost );
    sem_timedwait( &data->sem_cleanup, &ts );
  }
#endif

  jack_port_unregister( data->client, data->port );
  data->port = NULL;

  connected_ = false;
}

void MidiOutJack:: setClientName( const std::string& )
{

  errorString_ = "MidiOutJack::setClientName: this function is not implemented for the UNIX_JACK API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutJack :: setPortName( const std::string &portName )
{
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);
#ifdef JACK_HAS_PORT_RENAME
  jack_port_rename( data->client, data->port, portName.c_str() );
#else
  jack_port_set_name( data->port, portName.c_str() );
#endif
}

void MidiOutJack :: sendMessage( const unsigned char *message, size_t size )
{
  int nBytes = static_cast<int>(size);
  JackMidiData *data = static_cast<JackMidiData *> (apiData_);

  if ( size + sizeof(nBytes) > (size_t) data->buffMaxWrite )
      return;

  while ( jack_ringbuffer_write_space(data->buff) < sizeof(nBytes) + size )
      sched_yield();

  // Write full message to buffer
  jack_ringbuffer_write( data->buff, ( char * ) &nBytes, sizeof( nBytes ) );
  jack_ringbuffer_write( data->buff, ( const char * ) message, nBytes );
}

#endif  // __UNIX_JACK__

//*********************************************************************//
//  API: Web MIDI
//
//  Written primarily by Atsushi Eno, February 2020.
//
//  *********************************************************************//

#if defined(__WEB_MIDI_API__)

#include <emscripten.h>

//*********************************************************************//
//  API: WEB MIDI
//  Class Definitions: WebMidiAccessShim
//*********************************************************************//

class WebMidiAccessShim
{
public:
  WebMidiAccessShim();
  ~WebMidiAccessShim();
  std::string getPortName( unsigned int portNumber, bool isInput );
};

std::unique_ptr<WebMidiAccessShim> shim{nullptr};

void ensureShim()
{
  if ( shim.get() != nullptr )
    return;
  shim.reset( new WebMidiAccessShim() );
}

bool checkWebMidiAvailability()
{
  ensureShim();

  return MAIN_THREAD_EM_ASM_INT( {
    if ( typeof window._rtmidi_internals_waiting === "undefined" ) {
      console.log ( "Attempted to use Web MIDI API without trying to open it." );
      return false;
    }
    if ( window._rtmidi_internals_waiting ) {
      console.log ( "Attempted to use Web MIDI API while it is being queried." );
      return false;
    }
    if ( _rtmidi_internals_midi_access == null ) {
      console.log ( "Attempted to use Web MIDI API while it already turned out to be unavailable." );
      return false;
    }
    return true;
  } );
}

WebMidiAccessShim::WebMidiAccessShim()
{
  MAIN_THREAD_ASYNC_EM_ASM( {
    if( typeof window._rtmidi_internals_midi_access !== "undefined" )
      return;
    if( typeof window._rtmidi_internals_waiting !== "undefined" ) {
       console.log( "MIDI Access was requested while another request is in progress." );
       return;
    }

    // define functions
    window._rtmidi_internals_get_port_by_number = function( portNumber, isInput ) {
      var midi = window._rtmidi_internals_midi_access;
      var devices = isInput ? midi.inputs : midi.outputs;
      var i = 0;
      for (var device of devices.values()) {
        if ( i == portNumber )
          return device;
        i++;
      }
      console.log( "MIDI " + (isInput ? "input" : "output") + " device of portNumber " + portNumber + " is not found.");
      return null;
    };

    window._rtmidi_internals_waiting = true;
    window.navigator.requestMIDIAccess( {"sysex": true} ).then( (midiAccess) => {
      window._rtmidi_internals_midi_access = midiAccess;
      window._rtmidi_internals_latest_message_timestamp = 0.0;
      window._rtmidi_internals_waiting = false;
      if( midiAccess == null ) {
        console.log ( "Could not get access to MIDI API" );
      }
    } );
  } );
}

WebMidiAccessShim::~WebMidiAccessShim()
{
}

std::string WebMidiAccessShim::getPortName( unsigned int portNumber, bool isInput )
{
  if( !checkWebMidiAvailability() )
    return "";
  char *ret = (char*) MAIN_THREAD_EM_ASM_INT( {
    var port = window._rtmidi_internals_get_port_by_number($0, $1);
    if( port == null)
      return null;
    var length = lengthBytesUTF8(port.name) + 1;
    var ret = _malloc(length);
    stringToUTF8(port.name, ret, length);
    return ret;
  }, portNumber, isInput);
  if (ret == nullptr)
      return "";
  std::string s = ret;
  free(ret);
  return s;
}

//*********************************************************************//
//  API: WEB MIDI
//  Class Definitions: MidiInWeb
//*********************************************************************//

MidiInWeb::MidiInWeb( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit )
{
  initialize( clientName );
}

MidiInWeb::~MidiInWeb( void )
{
  closePort();
}

extern "C" void EMSCRIPTEN_KEEPALIVE rtmidi_onMidiMessageProc( MidiInApi::RtMidiInData* data, uint8_t* inputBytes, int32_t length, double domHighResTimeStamp )
{
  auto &message = data->message;
  message.bytes.resize(message.bytes.size() + length);
  memcpy(message.bytes.data(), inputBytes, length);
  // FIXME: handle timestamp
  if ( data->usingCallback ) {
    RtMidiIn::RtMidiCallback callback = (RtMidiIn::RtMidiCallback) data->userCallback;
    callback( message.timeStamp, &message.bytes, data->userData );
  }
}

void MidiInWeb::openPort( unsigned int portNumber, const std::string &portName )
{
  if( !checkWebMidiAvailability() )
    return;
  if (open_port_number >= 0)
    return;

  MAIN_THREAD_EM_ASM( {
    // In Web MIDI API world, there is no step to open a port, but we have to register the input callback instead.
    var input = window._rtmidi_internals_get_port_by_number($0, true);
    input.onmidimessage = function(e) {
      // In RtMidi world, timestamps are delta time from previous message, while in Web MIDI world
      // timestamps are relative to window creation time (i.e. kind of absolute time with window "epoch" time).
      var rtmidiTimestamp = window._rtmidi_internals_latest_message_timestamp == 0.0 ? 0.0 : e.timeStamp - window._rtmidi_internals_latest_message_timestamp;
      window._rtmidi_internals_latest_message_timestamp = e.timeStamp;
      Module.ccall( 'rtmidi_onMidiMessageProc', 'void', ['number', 'array', 'number', 'number'], [$1, e.data, e.data.length, rtmidiTimestamp] );
    };
  }, portNumber, &inputData_ );
  open_port_number = portNumber;
  connected_ = true;
}

void MidiInWeb::openVirtualPort( const std::string &portName )
{

  errorString_ = "MidiInWeb::openVirtualPort: this function is not implemented for the Web MIDI API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiInWeb::closePort( void )
{
  if( open_port_number < 0 )
    return;

  MAIN_THREAD_EM_ASM( {
    var input = _rtmidi_internals_get_port_by_number($0, true);
    if( input == null ) {
      console.log( "Port #" + $0 + " could not be found.");
      return;
    }
    // unregister event handler
    input.onmidimessage = null;
  }, open_port_number );
  open_port_number = -1;
  connected_ = false;
}

void MidiInWeb::setClientName( const std::string &clientName )
{
  client_name = clientName;
}

void MidiInWeb::setPortName( const std::string &portName )
{

  errorString_ = "MidiInWeb::setPortName: this function is not implemented for the Web MIDI API!";
  error( RtMidiError::WARNING, errorString_ );

}

unsigned int MidiInWeb::getPortCount( void )
{
  if( !checkWebMidiAvailability() )
    return 0;
  return MAIN_THREAD_EM_ASM_INT( { return _rtmidi_internals_midi_access.inputs.size; } );
}

std::string MidiInWeb::getPortName( unsigned int portNumber )
{
  if( !checkWebMidiAvailability() )
    return "";
  return shim->getPortName( portNumber, true );
}

void MidiInWeb::initialize( const std::string& clientName )
{
  ensureShim();
  setClientName( clientName );
}

//*********************************************************************//
//  API: WEB MIDI
//  Class Definitions: MidiOutWeb
//*********************************************************************//

MidiOutWeb::MidiOutWeb( const std::string &clientName )
{
  initialize( clientName );
}

MidiOutWeb::~MidiOutWeb( void )
{
  closePort();
}

void MidiOutWeb::openPort( unsigned int portNumber, const std::string &portName )
{
  if( !checkWebMidiAvailability() )
    return;
  if (open_port_number >= 0)
    return;
  // In Web MIDI API world, there is no step to open a port.

  open_port_number = portNumber;
  connected_ = true;
}

void MidiOutWeb::openVirtualPort( const std::string &portName )
{

  errorString_ = "MidiOutWeb::openVirtualPort: this function is not implemented for the Web MIDI API!";
  error( RtMidiError::WARNING, errorString_ );

}

void MidiOutWeb::closePort( void )
{
  // there is really nothing to do for output at JS side.
  open_port_number = -1;
  connected_ = false;
}

void MidiOutWeb::setClientName( const std::string &clientName )
{
  client_name = clientName;
}

void MidiOutWeb::setPortName( const std::string &portName )
{

  errorString_ = "MidiOutWeb::setPortName: this function is not implemented for the Web MIDI API!";
  error( RtMidiError::WARNING, errorString_ );

}

unsigned int MidiOutWeb::getPortCount( void )
{
  if( !checkWebMidiAvailability() )
    return 0;
  return MAIN_THREAD_EM_ASM_INT( { return _rtmidi_internals_midi_access.outputs.size; } );
}

std::string MidiOutWeb::getPortName( unsigned int portNumber )
{
  if( !checkWebMidiAvailability() )
    return "";
  return shim->getPortName( portNumber, false );
}

void MidiOutWeb::sendMessage( const unsigned char *message, size_t size )
{
  if( open_port_number < 0 )
    return;

  MAIN_THREAD_EM_ASM( {
    var output = _rtmidi_internals_get_port_by_number( $0, false );
    if( output == null ) {
      console.log( "Port #" + $0 + " could not be found.");
      return;
    }
    var buf = new ArrayBuffer ($2);
    var msg = new Uint8Array( buf );
    msg.set( new Uint8Array( Module.HEAPU8.buffer.slice( $1, $1 + $2 ) ) );
    output.send( msg );
  }, open_port_number, message, size );
}

void MidiOutWeb::initialize( const std::string& clientName )
{
  if ( shim.get() != nullptr )
    return;
  shim.reset( new WebMidiAccessShim() );
  setClientName( clientName );
}

#endif  // __WEB_MIDI_API__


//*********************************************************************//
//  API: ANDROID AMIDI
//
//  Written by Yellow Labrador, May 2023.
//  https://github.com/YellowLabrador/rtmidi
//  *********************************************************************//

#if defined(__AMIDI__)

#include <cstdint>

#define LOGE(...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
#define LOGW(...) __android_log_print(ANDROID_LOG_WARN, LOG_TAG, __VA_ARGS__)
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, LOG_TAG, __VA_ARGS__)
#define LOGD(...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

static std::string androidClientName;
static std::vector<jobject> androidMidiDevices;

//*********************************************************************//
//  API: Android AMIDI
//  Class Definitions: MidiInAndroid
//*********************************************************************//

static JNIEnv* androidGetThreadEnv() {
  // Every Android app has only one JVM. Calling JNI_GetCreatedJavaVMs
  // will retrieve the JVM running the app.
  jsize jvmsFound = 0;
  JavaVM jvms[1];
  JavaVM* pjvms = jvms;
  jint result = JNI_GetCreatedJavaVMs(&pjvms, 1, &jvmsFound);

  // Something went terribly wrong, no JVM was found
  if (jvmsFound != 1 || result != JNI_OK) {
      LOGE("No JVM found");
      return NULL;
  }

  // Get the JNIEnv for the current thread
  JNIEnv* env = NULL;
  int rc = pjvms->GetEnv((void**)&env, JNI_VERSION_1_6);

  // The current thread was not attached to the JVM. Add it to the JVM
  if (rc == JNI_EDETACHED) {
      pjvms->AttachCurrentThreadAsDaemon(&env, NULL);
  }

  // Neither way to retrieve the JNIEnv worked
  if (env == NULL) {
      LOGE("Unable to retrieve JNI environment");
  }

  return env;
}

static jobject androidGetContext(JNIEnv *env) {
  auto activityThread = env->FindClass("android/app/ActivityThread");
  auto currentActivityThread = env->GetStaticMethodID(activityThread, "currentActivityThread", "()Landroid/app/ActivityThread;");
  auto at = env->CallStaticObjectMethod(activityThread, currentActivityThread);
  if (at == NULL) {
      LOGE("Unable to locate the global ActivityThread");
      return NULL;
  }

  auto getApplication = env->GetMethodID(activityThread, "getApplication", "()Landroid/app/Application;");
  auto context = env->CallObjectMethod(at, getApplication);
  if (context == NULL) {
      LOGE("Application context was NULL");
  }

  return context;
}

static jobject androidGetMidiManager(JNIEnv *env, jobject context) {
  // MidiManager midiManager = (MidiManager) getSystemService(Context.MIDI_SERVICE);
  auto contextClass = env->FindClass("android/content/Context");
  auto getServiceMethod = env->GetMethodID(contextClass, "getSystemService", "(Ljava/lang/String;)Ljava/lang/Object;");
  return env->CallObjectMethod(context, getServiceMethod, env->NewStringUTF("midi"));
}

static void androidRefreshMidiDevices(JNIEnv *env, jobject context, bool isOutput) {
  // Remove all midi devices
  for (jobject jMidiDevice : androidMidiDevices) {
    env->DeleteGlobalRef(jMidiDevice);
  }
  androidMidiDevices.clear();

  auto midiService = androidGetMidiManager(env, context);

  // MidiDeviceInfo[] devInfos = mMidiManager.getDevices();
  auto midiMgrClass = env->FindClass("android/media/midi/MidiManager");
  auto getDevicesMethod = env->GetMethodID(midiMgrClass, "getDevices", "()[Landroid/media/midi/MidiDeviceInfo;");
  auto jDevices = (jobjectArray) env->CallObjectMethod(midiService, getDevicesMethod);

  auto deviceInfoClass = env->FindClass("android/media/midi/MidiDeviceInfo");
  auto getInputPortCountMethod = env->GetMethodID(deviceInfoClass, "getInputPortCount", "()I");
  auto getOutputPortCountMethod = env->GetMethodID(deviceInfoClass, "getOutputPortCount", "()I");

  jsize len = env->GetArrayLength((jarray)jDevices);
  for (int i=0; i<len; i++) {
      auto jDeviceInfo = env->GetObjectArrayElement(jDevices, i);

      int numPorts = env->CallIntMethod(jDeviceInfo, isOutput ? getOutputPortCountMethod : getInputPortCountMethod);
      if (numPorts > 0) {
          androidMidiDevices.push_back(env->NewGlobalRef(jDeviceInfo));
      }
  }
}


extern "C"
JNIEXPORT void JNICALL
Java_com_yellowlab_rtmidi_MidiDeviceOpenedListener_midiDeviceOpened(JNIEnv *env, jclass clazz,
                                                                    jobject midi_device, jlong targetPtr, jboolean isOutput) {
  if (isOutput) {
    auto midiOut = reinterpret_cast<MidiOutAndroid*>(targetPtr);
    AMidiDevice_fromJava(env, midi_device, &midiOut->sendDevice);
    AMidiInputPort_open(midiOut->sendDevice, 0, &midiOut->midiInputPort);
  } else {
    auto midiIn = reinterpret_cast<MidiInAndroid*>(targetPtr);
    AMidiDevice_fromJava(env, midi_device, &midiIn->receiveDevice);
    AMidiOutputPort_open(midiIn->receiveDevice, 0, &midiIn->midiOutputPort);
    pthread_create(&midiIn->readThread, NULL, MidiInAndroid::pollMidi, midiIn);
  }
}

static void androidOpenDevice(jobject deviceInfo, void* target, bool isOutput) {
    auto env = androidGetThreadEnv();
    auto context = androidGetContext(env);
    auto midiMgr = androidGetMidiManager(env, context);

    // openDevice(MidiDeviceInfo deviceInfo, OnDeviceOpenedListener listener, Handler handler)
    auto midiMgrClass = env->GetObjectClass(midiMgr);
    auto openDevicesMethod = env->GetMethodID(midiMgrClass, "openDevice", "(Landroid/media/midi/MidiDeviceInfo;Landroid/media/midi/MidiManager$OnDeviceOpenedListener;Landroid/os/Handler;)V");

    auto handlerClass = env->FindClass("android/os/Handler");
    auto handlerCtor = env->GetMethodID(handlerClass, "<init>", "()V");
    auto handler = env->NewObject(handlerClass, handlerCtor);

    auto listenerClass = env->FindClass("com/yellowlab/rtmidi/MidiDeviceOpenedListener");
    if (!listenerClass) {
      LOGE("Midi listener class not found com.yellowlab.rtmidi.MidiDeviceOpenedListener. Did you forget to add it to your APK?");
      return;
    }

    auto targetPtr = reinterpret_cast<jlong>(target);
    auto listenerCtor = env->GetMethodID(listenerClass, "<init>", "(JZ)V");
    auto listener = env->NewObject(listenerClass, listenerCtor, targetPtr, isOutput);

    env->CallVoidMethod(midiMgr, openDevicesMethod, deviceInfo, listener, handler);
    env->DeleteLocalRef(handler);
}

static std::string androidPortName(JNIEnv *env, unsigned int portNumber) {
  if (portNumber >= androidMidiDevices.size()) {
    LOGE("androidPortName: Invalid port number");
    return "";
  }

  // String deviceName = devInfo.getProperties().getString(MidiDeviceInfo.PROPERTY_NAME);
  auto deviceInfoClass = env->FindClass("android/media/midi/MidiDeviceInfo");
  auto getPropsMethod = env->GetMethodID(deviceInfoClass, "getProperties", "()Landroid/os/Bundle;");
  auto bundle = env->CallObjectMethod(androidMidiDevices[portNumber], getPropsMethod);

  auto bundleClass = env->FindClass("android/os/Bundle");
  auto getStringMethod = env->GetMethodID(bundleClass, "getString", "(Ljava/lang/String;)Ljava/lang/String;");
  auto jPortName = (jstring) env->CallObjectMethod(bundle, getStringMethod, env->NewStringUTF("name"));

  auto portNameChars = env->GetStringUTFChars(jPortName, NULL);
  auto name = std::string(portNameChars);
  env->ReleaseStringUTFChars(jPortName, portNameChars);

  return name;
}

MidiInAndroid :: MidiInAndroid( const std::string &clientName, unsigned int queueSizeLimit )
  : MidiInApi( queueSizeLimit ) {
  MidiInAndroid::initialize( clientName );
}

void MidiInAndroid :: initialize( const std::string& clientName ) {
  androidClientName = clientName;
  connect();
}

void MidiInAndroid :: connect() {
  auto env = androidGetThreadEnv();
  auto context = androidGetContext(env);
  androidRefreshMidiDevices(env, context, true);

  env->DeleteLocalRef(context);
}

MidiInAndroid :: ~MidiInAndroid() {
  auto env = androidGetThreadEnv();

  // Remove all midi devices
  for (jobject jMidiDevice : androidMidiDevices) {
    env->DeleteGlobalRef(jMidiDevice);
  }
  androidMidiDevices.clear();

  androidClientName = "";
}

void MidiInAndroid :: openPort(unsigned int portNumber, const std::string &portName) {
  if (portNumber >= androidMidiDevices.size()) {
    errorString_ = "MidiInAndroid::openPort: Invalid port number";
    error( RtMidiError::INVALID_PARAMETER, errorString_ );

    return;
  }

  if (reading) {
    errorString_ = "MidiInAndroid::openPort: A port is already open";
    error( RtMidiError::INVALID_USE, errorString_ );

    return;
  }

  androidOpenDevice(androidMidiDevices[portNumber], this, false);
}

void MidiInAndroid :: openVirtualPort(const std::string &portName) {
  errorString_ = "MidiInAndroid::openVirtualPort: this function is not implemented for the Android API!";
  error( RtMidiError::WARNING, errorString_ );
}

unsigned int MidiInAndroid :: getPortCount() {
  connect();
  return androidMidiDevices.size();
}

std::string MidiInAndroid :: getPortName(unsigned int portNumber) { 
  auto env = androidGetThreadEnv();
  return androidPortName(env, portNumber);
}

void MidiInAndroid :: closePort() {
  // Don't try to close a port before it was open
  if (!reading) {
    return;
  }

  reading = false;
  pthread_join(readThread, NULL);

  AMidiDevice_release(receiveDevice);
  receiveDevice = NULL;
  midiOutputPort = NULL;
}

void MidiInAndroid:: setClientName(const std::string& clientName) {
  androidClientName = clientName;
}

void MidiInAndroid :: setPortName(const std::string &portName) {
  errorString_ = "MidiInAndroid::setPortName: this function is not implemented for the Android API!";
  error( RtMidiError::WARNING, errorString_ );
}

void* MidiInAndroid :: pollMidi(void* context) {
  auto self = (MidiInAndroid*) context;
  self->reading = true;

  const size_t MAX_BYTES_TO_RECEIVE = 128;
  uint8_t incomingMessage[MAX_BYTES_TO_RECEIVE];

  while (self->reading) {
    // AMidiOutputPort_receive is non-blocking, must poll with some sleep
    usleep(2000);
    auto ignoreFlags = self->inputData_.ignoreFlags;
    bool& continueSysex = self->inputData_.continueSysex;

    int32_t opcode;
    size_t numBytesReceived;
    int64_t timestamp;
    ssize_t numMessagesReceived = AMidiOutputPort_receive(
        self->midiOutputPort, &opcode, incomingMessage, MAX_BYTES_TO_RECEIVE,
        &numBytesReceived, &timestamp);

    if (numMessagesReceived < 0) {
      self->errorString_ = "MidiInAndroid::pollMidi: error receiving MIDI data";
      self->error( RtMidiError::SYSTEM_ERROR, self->errorString_ );
      self->reading = false;
      break;
    }

    switch (incomingMessage[0]) {
      case 0xF0:
        // Start of a SysEx message
        continueSysex = incomingMessage[numBytesReceived - 1] != 0xF7;
            if (ignoreFlags & 0x01) continue;
            break;
      case 0xF1:
      case 0xF8:
        // MIDI Time Code or Timing Clock message
        if (ignoreFlags & 0x02) continue;
            break;
      case 0xFE:
        // Active Sensing message
        if (ignoreFlags & 0x04) continue;
            break;
      default:
        if (continueSysex) {
          // Continuation of a SysEx message
          continueSysex = incomingMessage[numBytesReceived - 1] != 0xF7;
          if (ignoreFlags & 0x01) continue;
        }
            // All other MIDI messages
    }

    if (numMessagesReceived > 0 && numBytesReceived >= 0) {
      auto message = self->inputData_.message;

      if (self->inputData_.firstMessage == true) {
        message.timeStamp = 0.0;
        self->inputData_.firstMessage = false;
      } else {
        message.timeStamp = (timestamp * 0.000001) - self->lastTime;
      }
      self->lastTime = (timestamp * 0.000001);

      if (!continueSysex) message.bytes.clear();

      if ( !( ( continueSysex || incomingMessage[0] == 0xF0 ) && ( ignoreFlags & 0x01 ) ) ) {
        // Unless this is a (possibly continued) SysEx message and we're ignoring SysEx,
        // copy the event buffer into the MIDI message struct.
        for (unsigned int i=0; i<numBytesReceived; i++)
          message.bytes.push_back(incomingMessage[i]);
      }

      if (!continueSysex) {
        if (self->inputData_.usingCallback) {
          auto callback = (RtMidiIn::RtMidiCallback) self->inputData_.userCallback;
          callback(message.timeStamp, &message.bytes, self->inputData_.userData);
        } else {
          if (!self->inputData_.queue.push(message))
            std::cerr << "\nMidiInAndroid: message queue limit reached!!\n\n";
        }
      }
    }
  }

  return NULL;
}


//*********************************************************************//
//  API: Android AMIDI
//  Class Definitions: MidiOutAndroid
//*********************************************************************//


MidiOutAndroid :: MidiOutAndroid( const std::string &clientName ) : MidiOutApi() {
  MidiOutAndroid::initialize( clientName );
}

void MidiOutAndroid :: initialize( const std::string& clientName ) {
  androidClientName = clientName;
  connect();
}

void MidiOutAndroid :: connect() {
  auto env = androidGetThreadEnv();
  auto context = androidGetContext(env);
  androidRefreshMidiDevices(env, context, false);

  env->DeleteLocalRef(context);
}

MidiOutAndroid :: ~MidiOutAndroid() {
  auto env = androidGetThreadEnv();

  // Remove all midi devices
  for (jobject jMidiDevice : androidMidiDevices) {
    env->DeleteGlobalRef(jMidiDevice);
  }
  androidMidiDevices.clear();

  androidClientName = "";
}

void MidiOutAndroid :: openPort( unsigned int portNumber, const std::string &portName ) {
  if (portNumber >= androidMidiDevices.size()) {
    errorString_ = "MidiOutAndroid::openPort: Invalid port number";
    error( RtMidiError::INVALID_PARAMETER, errorString_ );

    return;
  }

  androidOpenDevice(androidMidiDevices[portNumber], this, true);
}

void MidiOutAndroid :: openVirtualPort( const std::string &portName ) {
  errorString_ = "MidiOutAndroid::openVirtualPort: this function is not implemented for the Android API!";
  error( RtMidiError::WARNING, errorString_ );
}

unsigned int MidiOutAndroid :: getPortCount() {
  connect();
  return androidMidiDevices.size();
}

std::string MidiOutAndroid :: getPortName( unsigned int portNumber ) {
  auto env = androidGetThreadEnv();
  return androidPortName(env, portNumber);
}

void MidiOutAndroid :: closePort() {
  AMidiDevice_release(sendDevice);
  sendDevice = NULL;
  midiInputPort = NULL;
}

void MidiOutAndroid:: setClientName( const std::string& name ) {
  androidClientName = name;
}

void MidiOutAndroid :: setPortName( const std::string &portName ) {
  errorString_ = "MidiOutAndroid::setPortName: this function is not implemented for the Android API!";
  error( RtMidiError::WARNING, errorString_ );
}

void MidiOutAndroid :: sendMessage( const unsigned char *message, size_t size ) {
  AMidiInputPort_send(midiInputPort, (uint8_t*)message, size);
}

#endif  // __AMIDI__
