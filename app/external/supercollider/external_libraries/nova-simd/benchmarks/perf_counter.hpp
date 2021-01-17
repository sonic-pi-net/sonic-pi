//  Copyright (C) 2008 Tim Blechmann
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
//  Boston, MA 02110-1301, USA.

#ifndef PERF_COUNTER_HPP
#define PERF_COUNTER_HPP

#include <cassert>
#include <cstring>
#include <exception>
#include <iostream>
#include <iomanip>


#include <boost/array.hpp>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>


#include <linux/types.h>

#ifdef PERF_COUNTER_H

#include <linux/perf_counter.h>
#include <linux/syscalls.h>

#else
/*    taken from perf_counter.h, kernel 2.6.31
 *    Copyright (C) 2008-2009, Thomas Gleixner <tglx@linutronix.de>
 *    Copyright (C) 2008-2009, Red Hat, Inc., Ingo Molnar
 *    Copyright (C) 2008-2009, Red Hat, Inc., Peter Zijlstra
 */

#include <linux/types.h>
#include <linux/ioctl.h>
#include <asm/byteorder.h>

#if defined(__ia64__) || defined(__alpha__)
#include <asm-generic/unistd.h>
#endif


/*
 * User-space ABI bits:
 */

/*
 * attr.type
 */
enum perf_type_id {
        PERF_TYPE_HARDWARE                      = 0,
        PERF_TYPE_SOFTWARE                      = 1,
        PERF_TYPE_TRACEPOINT                    = 2,
        PERF_TYPE_HW_CACHE                      = 3,
        PERF_TYPE_RAW                           = 4,

        PERF_TYPE_MAX,                          /* non-ABI */
};

/*
 * Generalized performance counter event types, used by the
 * attr.event_id parameter of the sys_perf_counter_open()
 * syscall:
 */
enum perf_hw_id {
        /*
         * Common hardware events, generalized by the kernel:
         */
        PERF_COUNT_HW_CPU_CYCLES                = 0,
        PERF_COUNT_HW_INSTRUCTIONS              = 1,
        PERF_COUNT_HW_CACHE_REFERENCES          = 2,
        PERF_COUNT_HW_CACHE_MISSES              = 3,
        PERF_COUNT_HW_BRANCH_INSTRUCTIONS       = 4,
        PERF_COUNT_HW_BRANCH_MISSES             = 5,
        PERF_COUNT_HW_BUS_CYCLES                = 6,

        PERF_COUNT_HW_MAX,                      /* non-ABI */
};

/*
 * Generalized hardware cache counters:
 *
 *       { L1-D, L1-I, LLC, ITLB, DTLB, BPU } x
 *       { read, write, prefetch } x
 *       { accesses, misses }
 */
enum perf_hw_cache_id {
        PERF_COUNT_HW_CACHE_L1D                 = 0,
        PERF_COUNT_HW_CACHE_L1I                 = 1,
        PERF_COUNT_HW_CACHE_LL                  = 2,
        PERF_COUNT_HW_CACHE_DTLB                = 3,
        PERF_COUNT_HW_CACHE_ITLB                = 4,
        PERF_COUNT_HW_CACHE_BPU                 = 5,

        PERF_COUNT_HW_CACHE_MAX,                /* non-ABI */
};

enum perf_hw_cache_op_id {
        PERF_COUNT_HW_CACHE_OP_READ             = 0,
        PERF_COUNT_HW_CACHE_OP_WRITE            = 1,
        PERF_COUNT_HW_CACHE_OP_PREFETCH         = 2,

        PERF_COUNT_HW_CACHE_OP_MAX,             /* non-ABI */
};

enum perf_hw_cache_op_result_id {
        PERF_COUNT_HW_CACHE_RESULT_ACCESS       = 0,
        PERF_COUNT_HW_CACHE_RESULT_MISS         = 1,

        PERF_COUNT_HW_CACHE_RESULT_MAX,         /* non-ABI */
};

/*
 * Special "software" counters provided by the kernel, even if the hardware
 * does not support performance counters. These counters measure various
 * physical and sw events of the kernel (and allow the profiling of them as
 * well):
 */
enum perf_sw_ids {
        PERF_COUNT_SW_CPU_CLOCK                 = 0,
        PERF_COUNT_SW_TASK_CLOCK                = 1,
        PERF_COUNT_SW_PAGE_FAULTS               = 2,
        PERF_COUNT_SW_CONTEXT_SWITCHES          = 3,
        PERF_COUNT_SW_CPU_MIGRATIONS            = 4,
        PERF_COUNT_SW_PAGE_FAULTS_MIN           = 5,
        PERF_COUNT_SW_PAGE_FAULTS_MAJ           = 6,

        PERF_COUNT_SW_MAX,                      /* non-ABI */
};

/*
 * Bits that can be set in attr.sample_type to request information
 * in the overflow packets.
 */
enum perf_counter_sample_format {
        PERF_SAMPLE_IP                          = 1U << 0,
        PERF_SAMPLE_TID                         = 1U << 1,
        PERF_SAMPLE_TIME                        = 1U << 2,
        PERF_SAMPLE_ADDR                        = 1U << 3,
        PERF_SAMPLE_READ                        = 1U << 4,
        PERF_SAMPLE_CALLCHAIN                   = 1U << 5,
        PERF_SAMPLE_ID                          = 1U << 6,
        PERF_SAMPLE_CPU                         = 1U << 7,
        PERF_SAMPLE_PERIOD                      = 1U << 8,
        PERF_SAMPLE_STREAM_ID                   = 1U << 9,
        PERF_SAMPLE_RAW                         = 1U << 10,

        PERF_SAMPLE_MAX = 1U << 11,             /* non-ABI */
};

/*
 * The format of the data returned by read() on a perf counter fd,
 * as specified by attr.read_format:
 *
 * struct read_format {
 *      { u64           value;
 *        { u64         time_enabled; } && PERF_FORMAT_ENABLED
 *        { u64         time_running; } && PERF_FORMAT_RUNNING
 *        { u64         id;           } && PERF_FORMAT_ID
 *      } && !PERF_FORMAT_GROUP
 *
 *      { u64           nr;
 *        { u64         time_enabled; } && PERF_FORMAT_ENABLED
 *        { u64         time_running; } && PERF_FORMAT_RUNNING
 *        { u64         value;
 *          { u64       id;           } && PERF_FORMAT_ID
 *        }             cntr[nr];
 *      } && PERF_FORMAT_GROUP
 * };
 */
enum perf_counter_read_format {
        PERF_FORMAT_TOTAL_TIME_ENABLED          = 1U << 0,
        PERF_FORMAT_TOTAL_TIME_RUNNING          = 1U << 1,
        PERF_FORMAT_ID                          = 1U << 2,
        PERF_FORMAT_GROUP                       = 1U << 3,

        PERF_FORMAT_MAX = 1U << 4,              /* non-ABI */
};

#define PERF_ATTR_SIZE_VER0     64      /* sizeof first published struct */

/*
 * Hardware event to monitor via a performance monitoring counter:
 */
struct perf_counter_attr {

        /*
         * Major type: hardware/software/tracepoint/etc.
         */
        __u32                   type;

        /*
         * Size of the attr structure, for fwd/bwd compat.
         */
        __u32                   size;

        /*
         * Type specific configuration information.
         */
        __u64                   config;

        union {
                __u64           sample_period;
                __u64           sample_freq;
        };

        __u64                   sample_type;
        __u64                   read_format;

        __u64                   disabled       :  1, /* off by default        */
                                inherit        :  1, /* children inherit it   */
                                pinned         :  1, /* must always be on PMU */
                                exclusive      :  1, /* only group on PMU     */
                                exclude_user   :  1, /* don't count user      */
                                exclude_kernel :  1, /* ditto kernel          */
                                exclude_hv     :  1, /* ditto hypervisor      */
                                exclude_idle   :  1, /* don't count when idle */
                                mmap           :  1, /* include mmap data     */
                                comm           :  1, /* include comm data     */
                                freq           :  1, /* use freq, not period  */
                                inherit_stat   :  1, /* per task counts       */
                                enable_on_exec :  1, /* next exec enables     */
                                task           :  1, /* trace fork/exit       */

                                __reserved_1   : 50;

        __u32                   wakeup_events;  /* wakeup every n events */
        __u32                   __reserved_2;

        __u64                   __reserved_3;
};

/*
 * Ioctls that can be done on a perf counter fd:
 */
#define PERF_COUNTER_IOC_ENABLE         _IO ('$', 0)
#define PERF_COUNTER_IOC_DISABLE        _IO ('$', 1)
#define PERF_COUNTER_IOC_REFRESH        _IO ('$', 2)
#define PERF_COUNTER_IOC_RESET          _IO ('$', 3)
#define PERF_COUNTER_IOC_PERIOD         _IOW('$', 4, u64)

enum perf_counter_ioc_flags {
        PERF_IOC_FLAG_GROUP             = 1U << 0,
};

/*
 * Structure of the page that can be mapped via mmap
 */
struct perf_counter_mmap_page {
        __u32   version;                /* version number of this structure */
        __u32   compat_version;         /* lowest version this is compat with */

        /*
         * Bits needed to read the hw counters in user-space.
         *
         *   u32 seq;
         *   s64 count;
         *
         *   do {
         *     seq = pc->lock;
         *
         *     barrier()
         *     if (pc->index) {
         *       count = pmc_read(pc->index - 1);
         *       count += pc->offset;
         *     } else
         *       goto regular_read;
         *
         *     barrier();
         *   } while (pc->lock != seq);
         *
         * NOTE: for obvious reason this only works on self-monitoring
         *       processes.
         */
        __u32   lock;                   /* seqlock for synchronization */
        __u32   index;                  /* hardware counter identifier */
        __s64   offset;                 /* add to hardware counter value */
        __u64   time_enabled;           /* time counter active */
        __u64   time_running;           /* time counter on cpu */

                /*
                 * Hole for extension of the self monitor capabilities
                 */

        __u64   __reserved[123];        /* align to 1k */

        /*
         * Control data for the mmap() data buffer.
         *
         * User-space reading the @data_head value should issue an rmb(), on
         * SMP capable platforms, after reading this value -- see
         * perf_counter_wakeup().
         *
         * When the mapping is PROT_WRITE the @data_tail value should be
         * written by userspace to reflect the last read data. In this case
         * the kernel will not over-write unread data.
         */
        __u64   data_head;              /* head in the data section */
        __u64   data_tail;              /* user-space written tail */
};

#define PERF_EVENT_MISC_CPUMODE_MASK            (3 << 0)
#define PERF_EVENT_MISC_CPUMODE_UNKNOWN         (0 << 0)
#define PERF_EVENT_MISC_KERNEL                  (1 << 0)
#define PERF_EVENT_MISC_USER                    (2 << 0)
#define PERF_EVENT_MISC_HYPERVISOR              (3 << 0)

struct perf_event_header {
        __u32   type;
        __u16   misc;
        __u16   size;
};

enum perf_event_type {

        /*
         * The MMAP events record the PROT_EXEC mappings so that we can
         * correlate userspace IPs to code. They have the following structure:
         *
         * struct {
         *      struct perf_event_header        header;
         *
         *      u32                             pid, tid;
         *      u64                             addr;
         *      u64                             len;
         *      u64                             pgoff;
         *      char                            filename[];
         * };
         */
        PERF_EVENT_MMAP                 = 1,

        /*
         * struct {
         *      struct perf_event_header        header;
         *      u64                             id;
         *      u64                             lost;
         * };
         */
        PERF_EVENT_LOST                 = 2,

        /*
         * struct {
         *      struct perf_event_header        header;
         *
         *      u32                             pid, tid;
         *      char                            comm[];
         * };
         */
        PERF_EVENT_COMM                 = 3,

        /*
         * struct {
         *      struct perf_event_header        header;
         *      u32                             pid, ppid;
         *      u32                             tid, ptid;
         * };
         */
        PERF_EVENT_EXIT                 = 4,

        /*
         * struct {
         *      struct perf_event_header        header;
         *      u64                             time;
         *      u64                             id;
         *      u64                             stream_id;
         * };
         */
        PERF_EVENT_THROTTLE             = 5,
        PERF_EVENT_UNTHROTTLE           = 6,

        /*
         * struct {
         *      struct perf_event_header        header;
         *      u32                             pid, ppid;
         *      u32                             tid, ptid;
         * };
         */
        PERF_EVENT_FORK                 = 7,

        /*
         * struct {
         *      struct perf_event_header        header;
         *      u32                             pid, tid;
         *
         *      struct read_format              values;
         * };
         */
        PERF_EVENT_READ                 = 8,

        /*
         * struct {
         *      struct perf_event_header        header;
         *
         *      { u64                   ip;       } && PERF_SAMPLE_IP
         *      { u32                   pid, tid; } && PERF_SAMPLE_TID
         *      { u64                   time;     } && PERF_SAMPLE_TIME
         *      { u64                   addr;     } && PERF_SAMPLE_ADDR
         *      { u64                   id;       } && PERF_SAMPLE_ID
         *      { u64                   stream_id;} && PERF_SAMPLE_STREAM_ID
         *      { u32                   cpu, res; } && PERF_SAMPLE_CPU
         *      { u64                   period;   } && PERF_SAMPLE_PERIOD
         *
         *      { struct read_format    values;   } && PERF_SAMPLE_READ
         *
         *      { u64                   nr,
         *        u64                   ips[nr];  } && PERF_SAMPLE_CALLCHAIN
         *
         *      #
         *      # The RAW record below is opaque data wrt the ABI
         *      #
         *      # That is, the ABI doesn't make any promises wrt to
         *      # the stability of its content, it may vary depending
         *      # on event, hardware, kernel version and phase of
         *      # the moon.
         *      #
         *      # In other words, PERF_SAMPLE_RAW contents are not an ABI.
         *      #
         *
         *      { u32                   size;
         *        char                  data[size];}&& PERF_SAMPLE_RAW
         * };
         */
        PERF_EVENT_SAMPLE               = 9,

        PERF_EVENT_MAX,                 /* non-ABI */
};

enum perf_callchain_context {
        PERF_CONTEXT_HV                 = (__u64)-32,
        PERF_CONTEXT_KERNEL             = (__u64)-128,
        PERF_CONTEXT_USER               = (__u64)-512,

        PERF_CONTEXT_GUEST              = (__u64)-2048,
        PERF_CONTEXT_GUEST_KERNEL       = (__u64)-2176,
        PERF_CONTEXT_GUEST_USER         = (__u64)-2560,

        PERF_CONTEXT_MAX                = (__u64)-4095,
};

#endif

/* perf_types.h:
 * We define u64 as unsigned long long for every architecture
 * so that we can print it with %Lx without getting warnings.
 */
typedef unsigned long long u64;
typedef signed long long   s64;
typedef unsigned int       u32;
typedef signed int         s32;
typedef unsigned short     u16;
typedef signed short       s16;
typedef unsigned char      u8;
typedef signed char        s8;

/* parts from perf.h */

#if defined(__i386__)
#include "asm/unistd.h"
#define rmb()           asm volatile("lock; addl $0,0(%%esp)" ::: "memory")
#define cpu_relax()     asm volatile("rep; nop" ::: "memory");
#endif

#if defined(__x86_64__)
#include "asm/unistd.h"
#define rmb()           asm volatile("lfence" ::: "memory")
#define cpu_relax()     asm volatile("rep; nop" ::: "memory");
#endif

#ifdef __powerpc__
#include "asm/unistd.h"
#define rmb()           asm volatile ("sync" ::: "memory")
#define cpu_relax()     asm volatile ("" ::: "memory");
#endif

#ifdef __s390__
#include "asm/unistd.h"
#define rmb()           asm volatile("bcr 15,0" ::: "memory")
#define cpu_relax()     asm volatile("" ::: "memory");
#endif

#ifdef __sh__
#include "asm/unistd.h"
#if defined(__SH4A__) || defined(__SH5__)
# define rmb()          asm volatile("synco" ::: "memory")
#else
# define rmb()          asm volatile("" ::: "memory")
#endif
#define cpu_relax()     asm volatile("" ::: "memory")
#endif

#ifdef __hppa__
#include "../../arch/parisc/include/asm/unistd.h"
#define rmb()           asm volatile("" ::: "memory")
#define cpu_relax()     asm volatile("" ::: "memory");
#endif

#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/syscall.h>

#include "linux/types.h"

/*
 * prctl(PR_TASK_PERF_COUNTERS_DISABLE) will (cheaply) disable all
 * counters in the current task.
 */
#define PR_TASK_PERF_COUNTERS_DISABLE   31
#define PR_TASK_PERF_COUNTERS_ENABLE    32

#ifndef NSEC_PER_SEC
# define NSEC_PER_SEC                   1000000000ULL
#endif

static inline unsigned long long rdclock(void)
{
        struct timespec ts;

        clock_gettime(CLOCK_MONOTONIC, &ts);
        return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

/*
 * Pick up some kernel type conventions:
 */
#define __user
#define asmlinkage

#define __used          __attribute__((__unused__))

#define unlikely(x)     __builtin_expect(!!(x), 0)

#ifndef __NR_perf_event_open
#define __NR_perf_event_open __NR_perf_counter_open
#endif

static inline int
sys_perf_counter_open(struct perf_counter_attr *attr,
                      pid_t pid, int cpu, int group_fd,
                      unsigned long flags)
{
        attr->size = sizeof(*attr);
        return syscall(__NR_perf_event_open, attr, pid, cpu,
                       group_fd, flags);
}

#define MAX_COUNTERS                    256
#define MAX_NR_CPUS                     256

struct ip_callchain {
        u64 nr;
        u64 ips[0];
};


/* our class */

class perf_counter
{
    boost::array<int, 7> fd;

    static int open_performance_counter(int type)
    {
        perf_counter_attr attr;
        memset(&attr, 0, sizeof(perf_counter_attr));
        attr.type = PERF_TYPE_HARDWARE;
        attr.config = type;

        int ret = sys_perf_counter_open(&attr, 0, -1, -1, 0);

        if (ret < 0)
            throw std::exception();
        return ret;
    }

public:
    perf_counter(void)
    {
        for (int i = 0; i != 7; ++i)
            fd[i] = open_performance_counter(i);
    }

    ~perf_counter(void)
    {
        for (int i = 0; i != 7; ++i)
            close(fd[i]);
    }

    void reset(void)
    {}

    void start(void)
    {
        prctl(PR_TASK_PERF_COUNTERS_ENABLE);
    }

    void stop(void)
    {
        prctl(PR_TASK_PERF_COUNTERS_DISABLE);
    }

    void dump(bool verbose = true)
    {
        static const char *hw_event_names [] = {
            "cycles",
            "instructions",
            "cache references",
            "cache misses",
            "branches",
            "branch misses",
            "bus cycles",
        };

        boost::array<u64, 7> count;
        using namespace std;

        for (int i = 0; i != 7; ++i)
        {
            u64 cnt;
            ssize_t res = read(fd[i], (char*)&cnt, sizeof(cnt));
            if (res != sizeof(cnt))
                throw std::runtime_error("read error");
            count[i] = cnt;
        }

        if (verbose)
        {
            for (int i = 0; i != 7; ++i)
                cout << hw_event_names[i] << ": " << count[i] << endl;
            cout << endl;
        }
        else
        {
            cout << setw(12) << setfill(' ') << count[0] << " "
                 << setw(12) << setfill(' ') << count[1] << " "
                 << setw(7)  << setfill(' ') << count[2] << " "
                 << setw(5)  << setfill(' ') << count[3] << " "
                 << setw(10) << setfill(' ') << count[4] << " "
                 << setw(8)  << setfill(' ') << count[5] << " "
                 << setw(12)  << setfill(' ') << count[6] << " "
/*                  << setw(8)  << setfill(' ') << soft_count[0] << " " */
/*                  << setw(8)  << setfill(' ') << soft_count[1] */
                 << endl;
        }
    }
};

#undef unlikely
#endif /* PERF_COUNTER_HPP */
