#include "aubio_priv.h"

#if defined(HAVE_SOURCE_APPLE_AUDIO) || defined(HAVE_SINK_APPLE_AUDIO)

// CFURLRef, CFURLCreateWithFileSystemPath, ...
#include <CoreFoundation/CoreFoundation.h>
// ExtAudioFileRef, AudioStreamBasicDescription, AudioBufferList, ...
#include <AudioToolbox/AudioToolbox.h>

int createAubioBufferList(AudioBufferList *bufferList, int channels, int segmentSize);
void freeAudioBufferList(AudioBufferList *bufferList);
CFURLRef getURLFromPath(const char * path);
char_t *getPrintableOSStatusError(char_t *str, OSStatus error);

int createAudioBufferList(AudioBufferList * bufferList, int channels,
    int max_source_samples) {
  bufferList->mNumberBuffers = 1;
  bufferList->mBuffers[0].mNumberChannels = channels;
  bufferList->mBuffers[0].mData = AUBIO_ARRAY(smpl_t, max_source_samples);
  bufferList->mBuffers[0].mDataByteSize = max_source_samples * sizeof(smpl_t);
  return 0;
}

void freeAudioBufferList(AudioBufferList *bufferList) {
  UInt32 i = 0;
  if (!bufferList) return;
  for (i = 0; i < bufferList->mNumberBuffers; i++) {
    if (bufferList->mBuffers[i].mData) {
      AUBIO_FREE(bufferList->mBuffers[i].mData);
      bufferList->mBuffers[i].mData = NULL;
    }
  }
  bufferList = NULL;
}

CFURLRef createURLFromPath(const char * path) {
  CFStringRef cfTotalPath = CFStringCreateWithCString (kCFAllocatorDefault,
      path, kCFStringEncodingUTF8);

  CFURLRef url = CFURLCreateWithFileSystemPath(kCFAllocatorDefault, cfTotalPath,
      kCFURLPOSIXPathStyle, false);
  CFRelease(cfTotalPath);
  return url;
}

char_t *getPrintableOSStatusError(char_t *str, OSStatus error)
{
    // see if it appears to be a 4-char-code
    *(UInt32 *)(str + 1) = CFSwapInt32HostToBig(error);
    if (isprint(str[1]) && isprint(str[2]) && isprint(str[3]) && isprint(str[4])) {
        str[0] = str[5] = '\'';
        str[6] = '\0';
    } else
        // no, format it as an integer
        sprintf(str, "%d", (int)error);
    return str;
}

#endif /* defined(HAVE_SOURCE_APPLE_AUDIO) || defined(HAVE_SINK_APPLE_AUDIO) */
