/**********************************************************************
* proctable.c
*
* HP-UX specific code for the Ruby ps extension. Some code has been
* copied directly from Dan Urist's Proc::ProcessTable Perl module.
*
* Author: Daniel Berger
**********************************************************************/
#include "ruby.h"

#define _PSTAT64

#ifdef __cplusplus
extern "C"
{
#endif

#include <sys/param.h>
#include <sys/pstat.h>
#include <devnm.h>

#define BURST 30 /* pstat structs to get per syscall */

/* Process state strings */
#define SLEEP  "sleep"
#define RUN    "run"
#define STOP   "stop"
#define ZOMBIE "zombie"
#define IDLE   "idle"
#define OTHER  "other"

VALUE sProcStruct, cProcError;

/*****************************************************************************
 * This array's only purpose is for the 'fields()' class method.  If there's
 * a way to report fields out of our defined struct (sProc), please tell me
 * how!
 *****************************************************************************/
char *fields[] = {
   "comm",
   "uid",
   "pid",
   "ppid",
   "dsize",
   "tsize",
   "ssize",
   "nice",
   "ttydev",
   "pgrp",
   "pri",
   "addr",
   "cpu",
   "utime",
   "stime",
   "start",
   "flag",
   "stat",
   "wchan",
   "procnum",
   "cmd",
   "cmdline",
   "time",
   "cpticks",
   "cptickstotal",
   "fss",
   "pctcpu",
   "rssize",
   "suid",
   "shmsize",
   "mmsize",
   "usize",
   "iosize",
   "vtsize",
   "vdsize",
   "vssize",
   "vshmsize",
   "vmmsize",
   "vusize",
   "viosize",
   "minorfaults",
   "majorfaults",
   "nswap",
   "nsignals",
   "msgrcv",
   "msgsnd",
   "maxrss",
   "sid",
   "schedpolicy",
   "ticksleft",
   "euid",
   "egid",
   "gid",
   "sgid"
};


/*
 * Private method that converts a psinfo struct into a Ruby struct.
 */
static VALUE proctable_getprocstruct(struct pst_status *p)
{
   char state[10];
   char flag[12];
   char ttydev[MAXPATHLEN+1];
   VALUE v_tty, v_struct;

   switch( p->pst_stat )
   {
      case PS_SLEEP:
         strcpy(state,SLEEP);
         break;
      case PS_RUN:
         strcpy(state,RUN);
         break;
      case PS_STOP:
         strcpy(state,STOP);
         break;
      case PS_ZOMBIE:
         strcpy(state,ZOMBIE);
         break;
      case PS_IDLE:
         strcpy(state,IDLE);
         break;
      case PS_OTHER:
         strcpy(state,OTHER);
         break;
      default:
         strcpy(state,"unknown");
   }

   /* If the major number is -1, there is no associated tty */
   if(p->pst_term.psd_major != -1){
      devnm(
         S_IFCHR,
         (dev_t)((p->pst_term.psd_major << 24) | p->pst_term.psd_minor),
         ttydev,
         sizeof(ttydev),
         1
      );
      v_tty = rb_str_new2(ttydev);
   }
   else{
      v_tty = rb_str_new2("");
   }

   v_struct = rb_struct_new(sProcStruct,
      rb_str_new2(p->pst_ucomm),
      INT2NUM(p->pst_uid),
      INT2NUM(p->pst_pid),
      INT2NUM(p->pst_ppid),
      INT2NUM(p->pst_dsize),
      INT2NUM(p->pst_tsize),
      INT2NUM(p->pst_ssize),
      INT2NUM(p->pst_nice),
      v_tty,
      INT2NUM(p->pst_pgrp),
      INT2NUM(p->pst_pri),
      INT2NUM(p->pst_addr),
      INT2NUM(p->pst_cpu),
      INT2NUM(p->pst_utime),
      INT2NUM(p->pst_stime),
      rb_time_new(p->pst_start,0),
      INT2NUM(p->pst_flag),
      rb_str_new2(state),
      INT2NUM(p->pst_wchan),
      INT2NUM(p->pst_procnum),
      rb_str_new2(p->pst_cmd),
      rb_str_new2(p->pst_cmd),
      INT2NUM(p->pst_time),
      INT2NUM(p->pst_cpticks),
      INT2NUM(p->pst_cptickstotal),
      INT2NUM(p->pst_fss),
      rb_float_new(p->pst_pctcpu),
      INT2NUM(p->pst_rssize),
      INT2NUM(p->pst_suid),
      INT2NUM(p->pst_shmsize),
      INT2NUM(p->pst_mmsize),
      INT2NUM(p->pst_usize),
      INT2NUM(p->pst_iosize),
      INT2NUM(p->pst_vtsize),
      INT2NUM(p->pst_vdsize),
      INT2NUM(p->pst_vssize),
      INT2NUM(p->pst_vshmsize),
      INT2NUM(p->pst_vmmsize),
      INT2NUM(p->pst_vusize),
      INT2NUM(p->pst_viosize),
      UINT2NUM(p->pst_minorfaults),
      UINT2NUM(p->pst_majorfaults),
      UINT2NUM(p->pst_nswap),
      UINT2NUM(p->pst_nsignals),
      UINT2NUM(p->pst_msgrcv),
      UINT2NUM(p->pst_msgsnd),
      INT2NUM(p->pst_maxrss),
      INT2NUM(p->pst_sid),
      INT2NUM(p->pst_schedpolicy),
      INT2NUM(p->pst_ticksleft),
      INT2NUM(p->pst_euid),
      INT2NUM(p->pst_egid),
      INT2NUM(p->pst_gid),
      INT2NUM(p->pst_sgid)
   );

   OBJ_FREEZE(v_struct);

   return v_struct;
}

/*
 * call-seq:
 *    ProcTable.ps(pid=nil)
 *    ProcTable.ps(pid=nil){ |ps| ... }
 *
 * In block form, yields a ProcTableStruct for each process entry that you
 * have rights to.  This method returns an array of ProcTableStruct's in
 * non-block form.
 *
 * If a +pid+ is provided, then only a single ProcTableStruct is yielded or
 * returned, or nil if no process information is found for that +pid+.
 */
static VALUE proctable_ps(int argc, VALUE *argv)
{
   struct pst_status pst[BURST];
   int i, count;
   int idx = 0;
   int found = 0;
   VALUE v_pid = Qnil;
   VALUE v_arr = rb_ary_new();

   rb_scan_args(argc, argv, "01", &v_pid);

   if(v_pid != Qnil){
      struct pst_status pst_single;
      int n = NUM2INT(v_pid);
      if(pstat_getproc(&pst_single, sizeof(pst_single), 0, n) > 0){
         if(rb_block_given_p()){
            rb_yield(proctable_getprocstruct(&pst_single));
            return Qnil;
         }
         else{
            return(proctable_getprocstruct(&pst_single));
         }
      }
      else{
         return Qnil; /* not found */
      }
   }

   while ((count = pstat_getproc(pst, sizeof(pst[0]), BURST, idx)) > 0){
      for (i = 0; i < count; i++){
         if( rb_block_given_p() )
            rb_yield(proctable_getprocstruct(&pst[i]));
         else
            rb_ary_push(v_arr, proctable_getprocstruct(&pst[i]) );
      }
      idx = pst[count-1].pst_idx + 1;
   }

   if(rb_block_given_p())
      return Qnil;

   return v_arr;
}

/*
 * call-seq:
 *    ProcTable.fields
 *
 * Returns an array of fields that each ProcTableStruct will contain.  This
 * may be useful if you want to know in advance what fields are available
 * without having to perform at least one read of the /proc table.
 */
static VALUE proctable_fields()
{
   int i;
   VALUE v_farray = rb_ary_new();

   for(i = 0; i < (sizeof(fields) / sizeof(fields[0])); i++)
      rb_ary_push(v_farray,rb_str_new2(fields[i]));

   return v_farray;
}

/*
 * A Ruby interface for gathering process table information.
 */
void Init_proctable()
{
   VALUE mSys, cProcTable;

   /* The Sys module serves as a namespace only */
   mSys = rb_define_module("Sys");

   /* The ProcTable class encapsulates process table information */
   cProcTable = rb_define_class_under(mSys, "ProcTable", rb_cObject);

   /* The error typically raised if any of the ProcTable methods fail */
   cProcError = rb_define_class_under(cProcTable, "Error", rb_eStandardError);

   /* Singleton methods */

   rb_define_singleton_method(cProcTable, "fields", proctable_fields, 0);
   rb_define_singleton_method(cProcTable, "ps", proctable_ps, -1);

   /* There is no constructor */
   rb_funcall(cProcTable, rb_intern("private_class_method"), 1, ID2SYM(rb_intern("new")));

   /* 1.1.2: The version of the sys-proctable library. */
   rb_define_const(cProcTable, "VERSION", rb_str_new2("1.1.3"));

   /* Structs */

   sProcStruct = rb_struct_define("ProcTableStruct",
      "comm","uid","pid","ppid","dsize","tsize","ssize","nice","ttydev","pgrp",
      "pri","addr","cpu","utime","stime","start","flag","stat","wchan",
      "procnum","cmd","cmdline","time","cpticks","cptickstotal","fss","pctcpu",
      "rssize","suid","shmsize","mmsize", "usize","iosize","vtsize","vdsize",
      "vssize","vshmsize","vmmsize","vusize","viosize","minorfaults",
      "majorfaults","nswap","nsignals","msgrcv","msgsnd","maxrss","sid",
      "schedpolicy","ticksleft","euid","egid","gid","sgid",NULL
   );
}

#ifdef __cplusplus
}
#endif
