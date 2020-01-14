/*
 * Copyright (c) 2007 Wayne Meissner. All rights reserved.
 *
 * For licensing, see LICENSE.SPECS
 */


#define MEMSET(buf, value, size) do { \
    int i; for (i = 0; i < size; ++i) buf[i] = value; \
} while(0)
#define MEMCPY(dst, src, size) do { \
    int i; for (i = 0; i < size; ++i) dst[i] = src[i]; \
} while(0)

#define FILL(JTYPE, CTYPE) \
void fill##JTYPE##Buffer(CTYPE* buf, CTYPE value, int size) { MEMSET(buf, value, size); }

#define COPY(JTYPE, CTYPE) \
void copy##JTYPE##Buffer(CTYPE* dst, CTYPE* src, int size) { MEMCPY(dst, src, size); }

#define FUNC(JTYPE, CTYPE) \
    FILL(JTYPE, CTYPE); \
    COPY(JTYPE, CTYPE)
            
FUNC(Byte, char);
FUNC(Short, short);
FUNC(Int, int);
FUNC(Long, long long);
FUNC(Float, float);
FUNC(Double, double);

