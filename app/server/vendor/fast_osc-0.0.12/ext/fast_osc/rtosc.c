#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

// #include <rtosc.h>

const char *rtosc_argument_string(const char *msg)
{
    assert(msg && *msg);
    while(*++msg); //skip pattern
    while(!*++msg);//skip null
    return msg+1;  //skip comma
}

unsigned rtosc_narguments(const char *msg)
{
    const char *args = rtosc_argument_string(msg);
    int nargs = 0;
    while(*args++)
        nargs += (*args == ']' || *args == '[') ? 0 : 1;
    return nargs;
}

static int has_reserved(char type)
{
    switch(type)
    {
        case 'i'://official types
        case 's':
        case 'b':
        case 'f':

        case 'h'://unofficial
        case 't':
        case 'd':
        case 'S':
        case 'r':
        case 'm':
        case 'c':
            return 1;
        case 'T':
        case 'F':
        case 'N':
        case 'I':
        case '[':
        case ']':
            return 0;
    }

    //Should not happen
    return 0;
}

static unsigned nreserved(const char *args)
{
    unsigned res = 0;
    for(;*args;++args)
        res += has_reserved(*args);

    return res;
}

char rtosc_type(const char *msg, unsigned nargument)
{
    assert(nargument < rtosc_narguments(msg));
    const char *arg = rtosc_argument_string(msg);
    while(1) {
        if(*arg == '[' || *arg == ']')
            ++arg;
        else if(!nargument || !*arg)
            return *arg;
        else
            ++arg, --nargument;
    }
}

static unsigned arg_start(const char *msg_)
{
    const uint8_t *msg = (const uint8_t*)msg_;
    //Iterate to the right position
    const uint8_t *args = (const uint8_t*) rtosc_argument_string(msg_);
    const uint8_t *aligned_ptr = args-1;
    const uint8_t *arg_pos = args;

    while(*++arg_pos);
    //Alignment
    arg_pos += 4-(arg_pos-aligned_ptr)%4;
    return arg_pos-msg;
}

static unsigned arg_size(const uint8_t *arg_mem, char type)
{
    if(!has_reserved(type))
        return 0;
    const uint8_t  *arg_pos=arg_mem;
    uint32_t blob_length = 0;
    switch(type)
    {
        case 'h':
        case 't':
        case 'd':
            return 8;
        case 'm':
        case 'r':
        case 'f':
        case 'c':
        case 'i':
            return 4;
        case 'S':
        case 's':
            while(*++arg_pos);
            arg_pos += 4-(arg_pos-arg_mem)%4;
            return arg_pos-arg_mem;
        case 'b':
            blob_length |= (*arg_pos++ << 24);
            blob_length |= (*arg_pos++ << 16);
            blob_length |= (*arg_pos++ << 8);
            blob_length |= (*arg_pos++);
            if(blob_length%4)
                blob_length += 4-blob_length%4;
            arg_pos += blob_length;
            return arg_pos-arg_mem;
        default:
            assert("Invalid Type");
    }
    return -1;
}

static unsigned arg_off(const char *msg, unsigned idx)
{
    if(!has_reserved(rtosc_type(msg,idx)))
        return 0;

    //Iterate to the right position
    const uint8_t *args = (const uint8_t*) rtosc_argument_string(msg);
    const uint8_t *aligned_ptr = args-1;
    const uint8_t *arg_pos = args;

    while(*++arg_pos);
    //Alignment
    arg_pos += 4-(arg_pos-((uint8_t*)aligned_ptr))%4;

    //ignore any leading '[' or ']'
    while(*args == '[' || *args == ']')
        ++args;

    while(idx--) {
        char type = *args++;
        if(type == '[' || type == ']')
            idx++;//not a valid arg idx
        else
            arg_pos += arg_size(arg_pos, type);
    }
    return arg_pos-(uint8_t*)msg;
}

size_t rtosc_message(char   *buffer,
                     size_t      len,
                     const char *address,
                     const char *arguments,
                     ...)
{
    va_list va;
    va_start(va, arguments);
    size_t result = rtosc_vmessage(buffer, len, address, arguments, va);
    va_end(va);
    return result;
}

//Calculate the size of the message without writing to a buffer
static size_t vsosc_null(const char        *address,
                         const char        *arguments,
                         const rtosc_arg_t *args)
{
    unsigned pos = 0;
    pos += strlen(address);
    pos += 4-pos%4;//get 32 bit alignment
    pos += 1+strlen(arguments);
    pos += 4-pos%4;

    unsigned toparse = nreserved(arguments);
    unsigned arg_pos = 0;

    //Take care of varargs
    while(toparse)
    {
        char arg = *arguments++;
        assert(arg);
        int i;
        const char *s;
        switch(arg) {
            case 'h':
            case 't':
            case 'd':
                ++arg_pos;
                pos += 8;
                --toparse;
                break;
            case 'm':
            case 'r':
            case 'c':
            case 'f':
            case 'i':
                ++arg_pos;
                pos += 4;
                --toparse;
                break;
            case 's':
            case 'S':
                s = args[arg_pos++].s;
                assert(s && "Input strings CANNOT be NULL");
                pos += strlen(s);
                pos += 4-pos%4;
                --toparse;
                break;
            case 'b':
                i = args[arg_pos++].b.len;
                pos += 4 + i;
                if(pos%4)
                    pos += 4-pos%4;
                --toparse;
                break;
            default:
                ;
        }
    }

    return pos;
}
size_t rtosc_vmessage(char   *buffer,
                      size_t      len,
                      const char *address,
                      const char *arguments,
                      va_list ap)
{
    const unsigned nargs = nreserved(arguments);
    if(!nargs)
        return rtosc_amessage(buffer,len,address,arguments,NULL);

    rtosc_arg_t args[nargs];

    unsigned arg_pos = 0;
    const char *arg_str = arguments;
    uint8_t *midi_tmp;
    while(arg_pos < nargs)
    {
        switch(*arg_str++) {
            case 'h':
            case 't':
                args[arg_pos++].h = va_arg(ap, int64_t);
                break;
            case 'd':
                args[arg_pos++].d = va_arg(ap, double);
                break;
            case 'c':
            case 'i':
            case 'r':
                args[arg_pos++].i = va_arg(ap, int);
                break;
            case 'm':
                midi_tmp = va_arg(ap, uint8_t *);
                args[arg_pos].m[0] = midi_tmp[0];
                args[arg_pos].m[1] = midi_tmp[1];
                args[arg_pos].m[2] = midi_tmp[2];
                args[arg_pos++].m[3] = midi_tmp[3];
                break;
            case 'S':
            case 's':
                args[arg_pos++].s = va_arg(ap, const char *);
                break;
            case 'b':
                args[arg_pos].b.len = va_arg(ap, int);
                args[arg_pos].b.data = va_arg(ap, unsigned char *);
                arg_pos++;
                break;
            case 'f':
                args[arg_pos++].f = va_arg(ap, double);
                break;
            default:
                ;
        }
    }

    return rtosc_amessage(buffer,len,address,arguments,args);
}

size_t rtosc_amessage(char              *buffer,
                      size_t             len,
                      const char        *address,
                      const char        *arguments,
                      const rtosc_arg_t *args)
{
    const size_t total_len = vsosc_null(address, arguments, args);

    if(!buffer)
        return total_len;

    //Abort if the message cannot fit
    if(total_len>len) {
        memset(buffer, 0, len);
        return 0;
    }

    memset(buffer, 0, total_len);

    unsigned pos = 0;
    while(*address)
        buffer[pos++] = *address++;

    //get 32 bit alignment
    pos += 4-pos%4;

    buffer[pos++] = ',';

    const char *arg_str = arguments;
    while(*arg_str)
        buffer[pos++] = *arg_str++;

    pos += 4-pos%4;

    unsigned toparse = nreserved(arguments);
    unsigned arg_pos = 0;
    while(toparse)
    {
        char arg = *arguments++;
        assert(arg);
        int32_t i;
        int64_t d;
        const uint8_t *m;
        const char *s;
        const unsigned char *u;
        rtosc_blob_t b;
        switch(arg) {
            case 'h':
            case 't':
            case 'd':
                d = args[arg_pos++].t;
                buffer[pos++] = ((d>>56) & 0xff);
                buffer[pos++] = ((d>>48) & 0xff);
                buffer[pos++] = ((d>>40) & 0xff);
                buffer[pos++] = ((d>>32) & 0xff);
                buffer[pos++] = ((d>>24) & 0xff);
                buffer[pos++] = ((d>>16) & 0xff);
                buffer[pos++] = ((d>>8) & 0xff);
                buffer[pos++] = (d & 0xff);
                --toparse;
                break;
            case 'r':
            case 'f':
            case 'c':
            case 'i':
                i = args[arg_pos++].i;
                buffer[pos++] = ((i>>24) & 0xff);
                buffer[pos++] = ((i>>16) & 0xff);
                buffer[pos++] = ((i>>8) & 0xff);
                buffer[pos++] = (i & 0xff);
                --toparse;
                break;
            case 'm':
                //TODO verify ordering of spec
                m = args[arg_pos++].m;
                buffer[pos++] = m[0];
                buffer[pos++] = m[1];
                buffer[pos++] = m[2];
                buffer[pos++] = m[3];
                --toparse;
                break;
            case 'S':
            case 's':
                s = args[arg_pos++].s;
                while(*s)
                    buffer[pos++] = *s++;
                pos += 4-pos%4;
                --toparse;
                break;
            case 'b':
                b = args[arg_pos++].b;
                i = b.len;
                buffer[pos++] = ((i>>24) & 0xff);
                buffer[pos++] = ((i>>16) & 0xff);
                buffer[pos++] = ((i>>8) & 0xff);
                buffer[pos++] = (i & 0xff);
                u = b.data;
                if(u) {
                    while(i--)
                        buffer[pos++] = *u++;
                }
                else
                    pos += i;
                if(pos%4)
                    pos += 4-pos%4;
                --toparse;
                break;
            default:
                ;
        }
    }

    return pos;
}

static rtosc_arg_t extract_arg(const uint8_t *arg_pos, char type)
{
    rtosc_arg_t result = {0};
    //trivial case
    if(!has_reserved(type)) {
        switch(type)
        {
            case 'T':
                result.T = true;
                break;
            case 'F':
                result.T = false;
                break;
            default:
                ;
        }
    } else {
        switch(type)
        {
            case 'h':
            case 't':
            case 'd':
                result.t |= (((uint64_t)*arg_pos++) << 56);
                result.t |= (((uint64_t)*arg_pos++) << 48);
                result.t |= (((uint64_t)*arg_pos++) << 40);
                result.t |= (((uint64_t)*arg_pos++) << 32);
                result.t |= (((uint64_t)*arg_pos++) << 24);
                result.t |= (((uint64_t)*arg_pos++) << 16);
                result.t |= (((uint64_t)*arg_pos++) << 8);
                result.t |= (((uint64_t)*arg_pos++));
                break;
            case 'r':
            case 'f':
            case 'c':
            case 'i':
                result.i |= (*arg_pos++ << 24);
                result.i |= (*arg_pos++ << 16);
                result.i |= (*arg_pos++ << 8);
                result.i |= (*arg_pos++);
                break;
            case 'm':
                result.m[0] = *arg_pos++;
                result.m[1] = *arg_pos++;
                result.m[2] = *arg_pos++;
                result.m[3] = *arg_pos++;
                break;
            case 'b':
                result.b.len |= (*arg_pos++ << 24);
                result.b.len |= (*arg_pos++ << 16);
                result.b.len |= (*arg_pos++ << 8);
                result.b.len |= (*arg_pos++);
                result.b.data = (unsigned char *)arg_pos;
                break;
            case 'S':
            case 's':
                result.s = (char *)arg_pos;
                break;
        }
    }

    return result;
}

static const char *advance_past_dummy_args(const char *args)
{
    while(*args == '[' || *args == ']')
        args++;
    return args;
}

rtosc_arg_itr_t rtosc_itr_begin(const char *msg)
{
    rtosc_arg_itr_t itr;
    itr.type_pos  = advance_past_dummy_args(rtosc_argument_string(msg));
    itr.value_pos = (uint8_t*)(msg+arg_start(msg));

    return itr;
}

rtosc_arg_val_t rtosc_itr_next(rtosc_arg_itr_t *itr)
{
    //current position provides the value
    rtosc_arg_val_t result = {0,{0}};
    result.type = *itr->type_pos;
    if(result.type)
        result.val = extract_arg(itr->value_pos, result.type);

    //advance
    itr->type_pos = advance_past_dummy_args(itr->type_pos+1);
    char type = result.type;
    int size  = arg_size(itr->value_pos, type);
    itr->value_pos += size;


    return result;
}

int rtosc_itr_end(rtosc_arg_itr_t itr)
{
    return !itr.type_pos  || !*itr.type_pos;
}

rtosc_arg_t rtosc_argument(const char *msg, unsigned idx)
{
    char type = rtosc_type(msg, idx);
    uint8_t *arg_mem = (uint8_t*)msg + arg_off(msg, idx);
    return extract_arg(arg_mem, type);
}

static unsigned char deref(unsigned pos, ring_t *ring)
{
    return pos<ring[0].len ? ring[0].data[pos] :
        ((pos-ring[0].len)<ring[1].len ? ring[1].data[pos-ring[0].len] : 0x00);
}

static size_t bundle_ring_length(ring_t *ring)
{
    unsigned pos = 8+8;//goto first length field
    uint32_t advance = 0;
    do {
        advance = deref(pos+0, ring) << (8*3) |
                  deref(pos+1, ring) << (8*2) |
                  deref(pos+2, ring) << (8*1) |
                  deref(pos+3, ring) << (8*0);
        if(advance)
            pos += 4+advance;
    } while(advance);

    return pos <= (ring[0].len+ring[1].len) ? pos : 0;
}

//Zero means no full message present
size_t rtosc_message_ring_length(ring_t *ring)
{
    //Check if the message is a bundle
    if(deref(0,ring) == '#' &&
            deref(1,ring) == 'b' &&
            deref(2,ring) == 'u' &&
            deref(3,ring) == 'n' &&
            deref(4,ring) == 'd' &&
            deref(5,ring) == 'l' &&
            deref(6,ring) == 'e' &&
            deref(7,ring) == '\0')
        return bundle_ring_length(ring);

    //Proceed for normal messages
    //Consume path
    unsigned pos = 0;
    while(deref(pos++,ring));
    pos--;

    //Travel through the null word end [1..4] bytes
    for(int i=0; i<4; ++i)
        if(deref(++pos, ring))
            break;

    if(deref(pos, ring) != ',')
        return 0;

    unsigned aligned_pos = pos;
    int arguments = pos+1;
    while(deref(++pos,ring));
    pos += 4-(pos-aligned_pos)%4;

    unsigned toparse = 0;
    {
        int arg = arguments-1;
        while(deref(++arg,ring))
            toparse += has_reserved(deref(arg,ring));
    }

    //Take care of varargs
    while(toparse)
    {
        char arg = deref(arguments++,ring);
        assert(arg);
        uint32_t i;
        switch(arg) {
            case 'h':
            case 't':
            case 'd':
                pos += 8;
                --toparse;
                break;
            case 'm':
            case 'r':
            case 'c':
            case 'f':
            case 'i':
                pos += 4;
                --toparse;
                break;
            case 'S':
            case 's':
                while(deref(++pos,ring));
                pos += 4-(pos-aligned_pos)%4;
                --toparse;
                break;
            case 'b':
                i = 0;
                i |= (deref(pos++,ring) << 24);
                i |= (deref(pos++,ring) << 16);
                i |= (deref(pos++,ring) << 8);
                i |= (deref(pos++,ring));
                pos += i;
                if((pos-aligned_pos)%4)
                    pos += 4-(pos-aligned_pos)%4;
                --toparse;
                break;
            default:
                ;
        }
    }


    return pos <= (ring[0].len+ring[1].len) ? pos : 0;
}

size_t rtosc_message_length(const char *msg, size_t len)
{
    ring_t ring[2] = {{(char*)msg,len},{NULL,0}};
    return rtosc_message_ring_length(ring);
}

bool rtosc_valid_message_p(const char *msg, size_t len)
{
    //Validate Path Characters (assumes printable characters are sufficient)
    if(*msg != '/')
        return false;
    const char *tmp = msg;
    for(unsigned i=0; i<len; ++i) {
        if(*tmp == 0)
            break;
        if(!isprint(*tmp))
            return false;
        tmp++;
    }

    //tmp is now either pointing to a null or the end of the string
    const size_t offset1 = tmp-msg;
    size_t       offset2 = tmp-msg;
    for(; offset2<len; offset2++) {
        if(*tmp == ',')
            break;
        tmp++;
    }

    //Too many NULL bytes
    if(offset2-offset1 > 4)
        return false;

    if((offset2 % 4) != 0)
        return false;

    size_t observed_length = rtosc_message_length(msg, len);
    return observed_length == len;
}
static uint64_t extract_uint64(const uint8_t *arg_pos)
{
    uint64_t arg = 0;
    arg |= (((uint64_t)*arg_pos++) << 56);
    arg |= (((uint64_t)*arg_pos++) << 48);
    arg |= (((uint64_t)*arg_pos++) << 40);
    arg |= (((uint64_t)*arg_pos++) << 32);
    arg |= (((uint64_t)*arg_pos++) << 24);
    arg |= (((uint64_t)*arg_pos++) << 16);
    arg |= (((uint64_t)*arg_pos++) << 8);
    arg |= (((uint64_t)*arg_pos++));
    return arg;
}

static uint32_t extract_uint32(const uint8_t *arg_pos)
{
    uint32_t arg = 0;
    arg |= (((uint32_t)*arg_pos++) << 24);
    arg |= (((uint32_t)*arg_pos++) << 16);
    arg |= (((uint32_t)*arg_pos++) << 8);
    arg |= (((uint32_t)*arg_pos++));
    return arg;
}

static void emplace_uint64(uint8_t *buffer, uint64_t d)
{
    buffer[0] = ((d>>56) & 0xff);
    buffer[1] = ((d>>48) & 0xff);
    buffer[2] = ((d>>40) & 0xff);
    buffer[3] = ((d>>32) & 0xff);
    buffer[4] = ((d>>24) & 0xff);
    buffer[5] = ((d>>16) & 0xff);
    buffer[6] = ((d>>8)  & 0xff);
    buffer[7] = ((d>>0)  & 0xff);
}

static void emplace_uint32(uint8_t *buffer, uint32_t d)
{
    buffer[0] = ((d>>24) & 0xff);
    buffer[1] = ((d>>16) & 0xff);
    buffer[2] = ((d>>8)  & 0xff);
    buffer[3] = ((d>>0)  & 0xff);
}

size_t rtosc_bundle(char *buffer, size_t len, uint64_t tt, int elms, ...)
{
    char *_buffer = buffer;
    memset(buffer, 0, len);
    strcpy(buffer, "#bundle");
    buffer += 8;
    emplace_uint64((uint8_t*)buffer, tt);
    buffer += 8;
    va_list va;
    va_start(va, elms);
    for(int i=0; i<elms; ++i) {
        const char   *msg  = va_arg(va, const char*);
        //It is assumed that any passed message/bundle is valid
        size_t        size = rtosc_message_length(msg, -1);
        emplace_uint32((uint8_t*)buffer, size);
        buffer += 4;
        memcpy(buffer, msg, size);
        buffer+=size;
    }
    va_end(va);

    return buffer-_buffer;
}


#define POS ((size_t)(((const char *)lengths) - buffer))
size_t rtosc_bundle_elements(const char *buffer, size_t len)
{
    const uint32_t *lengths = (const uint32_t*) (buffer+16);
    size_t elms = 0;
    while(POS < len && extract_uint32((const uint8_t*)lengths)) {
        lengths += extract_uint32((const uint8_t*)lengths)/4+1;

        if(POS > len)
            break;
        ++elms;
    }
    return elms;
}
#undef POS

const char *rtosc_bundle_fetch(const char *buffer, unsigned elm)
{
    const uint32_t *lengths = (const uint32_t*) (buffer+16);
    size_t elm_pos = 0;
    while(elm_pos!=elm && extract_uint32((const uint8_t*)lengths)) {
        ++elm_pos;
        lengths += extract_uint32((const uint8_t*)lengths)/4+1;
    }

    return (const char*) (elm==elm_pos?lengths+1:NULL);
}

size_t rtosc_bundle_size(const char *buffer, unsigned elm)
{
    const uint32_t *lengths = (const uint32_t*) (buffer+16);
    size_t elm_pos = 0;
    size_t last_len = 0;
    while(elm_pos!=elm && extract_uint32((const uint8_t*)lengths)) {
        last_len = extract_uint32((const uint8_t*)lengths);
        ++elm_pos, lengths+=extract_uint32((const uint8_t*)lengths)/4+1;
    }

    return last_len;
}

int rtosc_bundle_p(const char *msg)
{
    return !strcmp(msg,"#bundle");
}

uint64_t rtosc_bundle_timetag(const char *msg)
{
    return extract_uint64((const uint8_t*)msg+8);
}
