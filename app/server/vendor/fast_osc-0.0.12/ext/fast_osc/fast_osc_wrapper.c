#include <ruby.h>
#include <ruby/encoding.h>
#include <rtosc.h>
#include <rtosc.c>


// Allocate VALUE variables to hold the modules we'll create. Ruby values
// are all of type VALUE. Qnil is the C representation of Ruby's nil.
VALUE FastOsc = Qnil;

// Declare a couple of functions. The first is initialization code that runs
// when this file is loaded, and the second is the actual business logic we're
// implementing.
void Init_fast_osc();
VALUE method_fast_osc_decode_single_message(VALUE self, VALUE msg);
VALUE method_fast_osc_encode_single_message(int argc, VALUE* argv, VALUE self);
VALUE method_fast_osc_encode_single_bundle(int argc, VALUE* argv, VALUE self);

// Initial setup function, takes no arguments and returns nothing. Some API
// notes:
//
// * rb_define_module() creates and returns a top-level module by name
//
// * rb_define_module_under() takes a module and a name, and creates a new
//   module within the given one
//
// * rb_define_singleton_method() take a module, the method name, a reference to
//   a C function, and the method's arity, and exposes the C function as a
//   single method on the given module
//
void Init_fast_osc() {
  FastOsc = rb_define_module("FastOsc");
  rb_define_singleton_method(FastOsc, "decode_single_message", method_fast_osc_decode_single_message, 1);
  rb_define_singleton_method(FastOsc, "encode_single_message", method_fast_osc_encode_single_message, -1);
  rb_define_singleton_method(FastOsc, "encode_single_bundle", method_fast_osc_encode_single_bundle, -1);
}

const char *rtosc_path(const char *msg)
{
  return msg;
}

#define JAN_1970 2208988800.0     /* 2208988800 time from 1900 to 1970 in seconds */

uint64_t ruby_time_to_osc_timetag(VALUE rubytime) {
  uint64_t timetag;
  double floattime;
  uint32_t sec;
  uint32_t frac;

  switch(TYPE(rubytime)) {
    case T_NIL:
      timetag = 1;
      break;
    default:
      // convert Time object to ntp
      floattime = JAN_1970 + NUM2DBL(rb_funcall(rubytime, rb_intern("to_f"), 0));

      sec = floor(floattime);
      frac = (uint32_t)(fmod(floattime, 1.0) * 4294967296); // * (2 ** 32)
      // printf("\nsec: %04x\n", sec);
      // printf("\nfrac: %04x\n", frac);
      timetag = (uint64_t)((uint64_t)sec << 32 | (uint64_t)frac);
      // printf("\ntimetag: %08llx\n", timetag);
      break;
  }

  return timetag;
}


VALUE method_fast_osc_decode_single_message(VALUE self, VALUE msg) {
  rtosc_arg_itr_t itr;
  char* data = StringValuePtr(msg);
  itr = rtosc_itr_begin(data);
  VALUE output = rb_ary_new();
  VALUE args_output = rb_ary_new();
  VALUE string_arg;
  int enc;

  VALUE path = rb_str_new2(rtosc_path(data));

  rtosc_arg_val_t next_val;

  // for timestamp arg decoding
  uint64_t tt, secs, frac;

  while(!rtosc_itr_end(itr)) {

    next_val = rtosc_itr_next(&itr);

    switch(next_val.type) {
      case 'i' :
        // INT2FIX() for integers within 31bits.
        rb_ary_push(args_output, INT2FIX(next_val.val.i));
        break;
      case 'f' :
        rb_ary_push(args_output, rb_float_new(next_val.val.f));
        break;
      case 's' :
        string_arg = rb_str_new2(next_val.val.s);
        enc = rb_enc_find_index("UTF-8");
        rb_enc_associate_index(string_arg, enc);

        rb_ary_push(args_output, string_arg);
        break;
      case 'b' :
        rb_ary_push(args_output, rb_str_new((const char*)next_val.val.b.data, next_val.val.b.len));
        break;
      case 'h' :
        // INT2NUM() for arbitrary sized integers
        rb_ary_push(args_output, INT2NUM(next_val.val.h));
        break;
      case 't' :
        // OSC time tag
        // need to decode OSC (ntp style time) to unix timestamp
        // then call Time.now with that
        tt = next_val.val.t;
        secs = (tt >> 32) - JAN_1970;
        // taken from this SO post on how to convert NTP to Unix epoch
        // http://stackoverflow.com/a/29138806
        frac = ((tt & 0xFFFFFFFF) * 1000000) >> 32;
        // example call from grpc ruby extension
        // https://github.com/grpc/grpc/blob/master/src/ruby/ext/grpc/rb_grpc.c
        //   return rb_funcall(rb_cTime, id_at, 2, INT2NUM(real_time.tv_sec),
        //                       INT2NUM(real_time.tv_nsec / 1000));
        // printf("\noutsec: %08llx\n", secs);
        // printf("\noutfrac: %08llx\n", frac);
        // printf("\nouttimetag: %08llx\n", tt);
        rb_ary_push(args_output, rb_funcall(rb_cTime, rb_intern("at"), 2, LONG2NUM(secs), LONG2NUM(frac)));
        break;
      case 'd' :
        rb_ary_push(args_output, rb_float_new(next_val.val.d));
        break;
      case 'S' :
        rb_ary_push(args_output, ID2SYM(rb_intern(next_val.val.s)));
        break;
      case 'c' :
        rb_ary_push(args_output, rb_str_concat(rb_str_new2(""), INT2FIX(next_val.val.i)));
        break;
    }

  }

  rb_ary_push(output, path);
  rb_ary_push(output, args_output);
  return output;
}

int buffer_size_for_ruby_string(VALUE rstring) {
  int str_bytesize = FIX2INT(LONG2FIX(RSTRING_LEN(rstring)));
  int bufsize = (int)((str_bytesize + sizeof(int) - 1) & ~(sizeof(int) - 1));
  return (bufsize + 4) & ~3u;
}

VALUE method_fast_osc_encode_single_message(int argc, VALUE* argv, VALUE self) {
  VALUE address, args;

  rb_scan_args(argc, argv, "11", &address, &args);

  if (NIL_P(args)) args = rb_ary_new();

  // Ruby C API only really allows methods that slurp in all the args
  // Since we want the method to look like
  //
  // def encode_single_message(path, args=[])
  //
  // we need to muck around with the args option a bit
  // VALUE* brings in args as a C array
  char* c_address = StringValueCStr(address);

  int no_of_args = NUM2INT(LONG2NUM(RARRAY_LEN(args)));
  int i;
  VALUE current_arg, strval;

  //output tags and args list
  VALUE tagstring = rb_str_new2(""); //rtosc will handle comma
  rtosc_arg_t output_args[no_of_args];

  for(i = 0; i < no_of_args; i++) {
    current_arg = rb_ary_entry(args, i);

    switch(TYPE(current_arg)) {
      case T_FIXNUM:
        if(FIX2LONG(current_arg) < ~(1 << 31)) {
          rb_str_concat(tagstring, rb_str_new2("i"));
          output_args[i].i = FIX2INT(current_arg);
        } else {
          rb_str_concat(tagstring, rb_str_new2("h"));
          output_args[i].h = FIX2LONG(current_arg);
        }
        break;
      case T_FLOAT:
        rb_str_concat(tagstring, rb_str_new2("f"));
        output_args[i].f = NUM2DBL(current_arg);
        break;
      case T_STRING:
        rb_str_concat(tagstring, rb_str_new2("s"));
        output_args[i].s = StringValueCStr(current_arg);
        break;
      case T_SYMBOL:
        // now align to 4 byte boundary for sizing output buffer
        strval = rb_sym_to_s(current_arg);

        // encode as a string because not all implementation understand S as
        // alternative string tag
        rb_str_concat(tagstring, rb_str_new2("s"));
        output_args[i].s = StringValueCStr(strval);
        break;
      case T_DATA:
        if (CLASS_OF(current_arg) == rb_cTime) {
          // at present I only care about the Time as an object arg
          rb_str_concat(tagstring, rb_str_new2("t"));
          output_args[i].t = ruby_time_to_osc_timetag(current_arg);
        }
        break;
    }
  }

  unsigned long int len;
  // When buffer is NULL, the function returns the size of the buffer required to store the message
  if(RSTRING_LEN(tagstring)) {
    len = rtosc_amessage(NULL, 0, c_address, StringValueCStr(tagstring), output_args);
  } else {
    len = rtosc_message(NULL, 0, c_address, "");
  }

  // duplicate if/else due to compiler errors
  char buffer[len];
  if(RSTRING_LEN(tagstring)) {
    rtosc_amessage(buffer, len, c_address, StringValueCStr(tagstring), output_args);
  } else {
    rtosc_message(buffer, len, c_address, "");
  }

  VALUE output = rb_str_new(buffer, len);

  return output;
}

VALUE method_fast_osc_encode_single_bundle(int argc, VALUE* argv, VALUE self) {
  VALUE timetag, path, args;
  rb_scan_args(argc, argv, "21", &timetag, &path, &args);

  if (NIL_P(args)) args = rb_ary_new();

  VALUE combined_args_holder = rb_ary_new();
  rb_ary_push(combined_args_holder, path);
  rb_ary_push(combined_args_holder, args);
  VALUE* combined_path_and_args = RARRAY_PTR(combined_args_holder);

  // There are always 2 args here - [path, [args...]]
  VALUE message = method_fast_osc_encode_single_message(2, combined_path_and_args, self);
  int bufsize = buffer_size_for_ruby_string(message) + 16;
  int no_of_elems = 1;
  uint64_t tt = ruby_time_to_osc_timetag(timetag);
  char output_buffer[bufsize];

  unsigned long int len = rtosc_bundle(output_buffer, bufsize, tt, no_of_elems, StringValuePtr(message));

  VALUE output = rb_str_new(output_buffer, len);

  return output;
}
