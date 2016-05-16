/*
 * Copyright (c) 2012 Mark McCurry
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * @file rtosc.h
 */
#ifndef RTOSC_H
#define RTOSC_H
#include <stdarg.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    int32_t len;
    uint8_t *data;
} rtosc_blob_t;

typedef union {
    int32_t       i;   //i,c,r
    char          T;   //I,T,F,N
    float         f;   //f
    double        d;   //d
    int64_t       h;   //h
    uint64_t      t;   //t
    uint8_t       m[4];//m
    const char   *s;   //s,S
    rtosc_blob_t  b;   //b
} rtosc_arg_t;

/**
 * Write OSC message to fixed length buffer
 *
 * On error, buffer will be zeroed.
 * When buffer is NULL, the function returns the size of the buffer required to
 * store the message
 *
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * //Example messages
 * char buffer[128];
 * rtosc_message(buffer,128,"/path","TFI");
 * rtosc_message(buffer,128,"/path","s","foobar");
 * rtosc_message(buffer,128,"/path","i",128);
 * rtosc_message(buffer,128,"/path","f",128.0);
 * const char blob[4] = {'a','b','c','d'};
 * rtosc_message(buffer,128,"/path","b",4,blob);
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * @param buffer    Memory to write to
 * @param len       Length of buffer
 * @param address   OSC pattern to send message to
 * @param arguments String consisting of the types of the following arguments
 * @param ...       OSC arguments to pass forward
 * @returns length of resulting message or zero if bounds exceeded
 */
size_t rtosc_message(char   *buffer,
        size_t      len,
        const char *address,
        const char *arguments,
        ...);

/**
 * @see rtosc_message()
 */
size_t rtosc_vmessage(char   *buffer,
        size_t      len,
        const char *address,
        const char *arguments,
        va_list va);

/**
 * @see rtosc_message()
 */
size_t rtosc_amessage(char        *buffer,
                      size_t       len,
                      const char  *address,
                      const char  *arguments,
                      const rtosc_arg_t *args);

/**
 * Returns the number of arguments found in a given message
 *
 * @param msg well formed OSC message
 * @returns number of arguments in message
 */
unsigned rtosc_narguments(const char *msg);

/**
 * @param msg well formed OSC message
 * @param i   index of argument
 * @returns the type of the ith argument in msg
 */
char rtosc_type(const char *msg, unsigned i);

typedef struct {
    const char    *type_pos;
    const uint8_t *value_pos;
} rtosc_arg_itr_t;

typedef struct {
    char type;
    rtosc_arg_t val;
} rtosc_arg_val_t;

/**
 * Create an argument iterator for a message
 * @param msg OSC message
 * @returns an initialized iterator
 */
rtosc_arg_itr_t rtosc_itr_begin(const char *msg);

/**
 * Gets the next argument in a message
 * @param itr OSC message iterator
 * @returns a type value pair from the message
 */
rtosc_arg_val_t rtosc_itr_next(rtosc_arg_itr_t *itr);

/**
 * Determines if the iterator is at the end of the argument list
 * @param itr OSC message iterator
 * @returns 1 if there are no more elements, 0 otherwise
 */
int rtosc_itr_end(rtosc_arg_itr_t itr);

/**
 * Blob data may be safely written to
 * @param msg OSC message
 * @param i   index of argument
 * @returns an argument by value via the rtosc_arg_t union
 */
rtosc_arg_t rtosc_argument(const char *msg, unsigned i);

/**
 * @param msg OSC message
 * @param len Message length upper bound
 * @returns the size of a message given a chunk of memory.
 */
size_t rtosc_message_length(const char *msg, size_t len);

typedef struct {
    char *data;
    size_t len;
} ring_t;

/**
 * Finds the length of the next message inside a ringbuffer structure.
 * 
 * @param ring The addresses and lengths of the split buffer, in a compatible
 *             format to jack's ringbuffer
 * @returns size of message stored in ring datastructure
 */
size_t rtosc_message_ring_length(ring_t *ring);


/**
 * Validate if an arbitrary byte sequence is an OSC message.
 * @param msg pointer to memory buffer
 * @param len length of buffer
 */
bool rtosc_valid_message_p(const char *msg, size_t len);

/**
 * @param OSC message
 * @returns the argument string of a given message
 */
const char *rtosc_argument_string(const char *msg);

/**
 * Generate a bundle from sub-messages
 *
 * @param buffer Destination buffer
 * @param len    Length of buffer
 * @param tt     OSC time tag
 * @param elms   Number of sub messages
 * @param ...    Messages
 * @returns legnth of generated bundle or zero on failure
 */
size_t rtosc_bundle(char *buffer, size_t len, uint64_t tt, int elms, ...);

/**
 * Find the elements in a bundle
 *
 * @param msg OSC bundle
 * @param len Upper bound on the length of the bundle
 * @returns The number of messages contained within the bundle
 */
size_t rtosc_bundle_elements(const char *msg, size_t len);

/**
 * Fetch a message within the bundle
 *
 * @param msg OSC bundle
 * @param i      index of sub-message
 * @returns The ith message within the bundle
 */
const char *rtosc_bundle_fetch(const char *msg, unsigned i);

/**
 * Get the size of a particular bundle element
 *
 * @param msg OSC bundle
 * @param i   Index of sub-message
 * @returns   The size of the ith sub-message in bytes
 */
size_t rtosc_bundle_size(const char *msg, unsigned i);

/**
 * Test if the buffer contains a bundle
 *
 * @param msg OSC message
 * @returns true if message is a bundle
 */
int rtosc_bundle_p(const char *msg);

/**
 * @returns Time Tag for a bundle
 */
uint64_t rtosc_bundle_timetag(const char *msg);


/**
 * This is a non-compliant pattern matcher for dispatching OSC messages
 *
 * Overall the pattern specification is
 *   (normal-path)(\#digit-specifier)?(/)?(:argument-restrictor)*
 *
 * @param pattern The pattern string stored in the Port
 * @param msg     The OSC message to be matched
 * @returns true if a normal match and false if unmatched
 */
bool rtosc_match(const char *pattern, const char *msg);


/**
 * Attempt to match a rtosc style path while ignoring arguments
 *
 * @param pattern rtosc pattern
 * @param msg a normal C string or a rtosc message
 */
const char *rtosc_match_path(const char *pattern, const char *msg);

#ifdef __cplusplus
};
#endif
#endif
