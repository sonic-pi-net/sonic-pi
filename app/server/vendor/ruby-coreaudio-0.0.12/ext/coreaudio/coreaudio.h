#ifndef COREAUDIO_H
#define COREAUDIO_H 1

#include <ruby.h>

#include "narray.h"
#include "extconf.h"

extern VALUE rb_mCoreAudio;
extern VALUE rb_mAudioFile;

extern void Init_coreaudio_audiofile(void);

/*-- Utility Macros --*/
#define CROPF(F) ((F) > 1.0 ? 1.0 : (((F) < -1.0) ? -1.0 : (F)))
#define FLOAT2SHORT(F) ((short)(CROPF(F)*0x7FFF))
#define SHORT2FLOAT(S) ((float)(S) / (float)32767.0)

/*-- prototypes for missing functions --*/

#ifndef HAVE_RB_ALLOC_TMP_BUFFER
extern void *rb_alloc_tmp_buffer(volatile VALUE *store, long len);
#endif

#ifndef HAVE_RB_FREE_TMP_BUFFER
extern void rb_free_tmp_buffer(volatile VALUE *store);
#endif


#endif
