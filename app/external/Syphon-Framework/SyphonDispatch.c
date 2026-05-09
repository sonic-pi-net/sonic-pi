/*
	SyphonDispatch.c
	Syphon
 
	Copyright 2010-2011 bangnoise (Tom Butterworth) & vade (Anton Marini).
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:

	* Redistributions of source code must retain the above copyright
	notice, this list of conditions and the following disclaimer.

	* Redistributions in binary form must reproduce the above copyright
	notice, this list of conditions and the following disclaimer in the
	documentation and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
	ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
	DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SyphonDispatch.h"
// stdlib for malloc
#include <stdlib.h>
// time.h for gettimeofday in finalizer()
#include <sys/time.h>
// the rest are used throughout
#include <Block.h>
#include <pthread.h>
#include <libkern/OSAtomic.h>
#include <stdatomic.h>
#include <dispatch/dispatch.h>

//#define SYPHON_DISPATCH_DEBUG_LOGGING

#ifdef SYPHON_DISPATCH_DEBUG_LOGGING
#include <stdio.h> // for printf()
#endif

// TODO: think about error handling here to catch exceptions in a source's block(s)

#pragma mark Private Functions, Defines and Types

/*
 kSyphonDispatchUnloadTimeout
	The time in seconds we wait for firing sources to finish when the code is unloaded (eg at application quit)
	TODO: think about how long this should be
 */
#define kSyphonDispatchUnloadTimeout 1.0

typedef struct SyphonDispatchChannel SyphonDispatchChannel;

static void _SyphonDispatchSourceRelease(SyphonDispatchSourceRef source, bool onChannel);

/*
 _SyphonDispatchChannelLaunchFromPool
	Used by SyphonDispatchFire to launch source on a channel, using an existing one from the pool if available, otherwise making a new one
 */
static inline void _SyphonDispatchChannelLaunchFromPool(SyphonDispatchSourceRef source);

/*
 _SyphonDispatchChannelReleaseToPool
	Return channel to the pool. channel must not be NULL.
 */
#define _SyphonDispatchChannelReturnToPool(channel) OSAtomicEnqueue(&mChanPool, (channel), offsetof(SyphonDispatchChannel, next))

/*
 _SyphonDispatchChannelTryFromPool
	Returns an existing channel from the pool if available, otherwise NULL
 */
#define _SyphonDispatchChannelTryFromPool() OSAtomicDequeue(&mChanPool, offsetof(SyphonDispatchChannel, next))

/*
 _SyphonDispatchChannelDestroy
	Signals a channel to destroy itself
 */
static void _SyphonDispatchChannelDestroy(SyphonDispatchChannel *channel);

/*
 _SyphonDispatchGetWorkSemaphore()
	Returns the global work semaphore to signal work done
 */
static dispatch_semaphore_t _SyphonDispatchGetWorkSemaphore(void);

typedef struct SyphonDispatchSource
{
	atomic_int_fast32_t				retainc;
	void (^fblock)(void);
    atomic_int_fast32_t				firec;
    atomic_uintptr_t                cblock;
} SyphonDispatchSource;

struct SyphonDispatchChannel
{
	void							*next;
	SyphonDispatchSourceRef			activeSource;
	dispatch_semaphore_t			signal;
	atomic_bool						done;
};

#pragma mark Dispatch Globals

static OSQueueHead mChanPool = OS_ATOMIC_QUEUE_INIT;
static atomic_int_fast32_t mSourceC = 0;
static atomic_int_fast32_t mChannelC = 0;
static atomic_int_fast32_t mActiveC = 0;
static atomic_uintptr_t mWorkDoneSignal = (uintptr_t)NULL;

#pragma mark Constructor and Destructor

__attribute__((destructor))
static void finalizer(void)
{
	struct timeval start;
	if(gettimeofday(&start, NULL) == 0)
	{
		uint64_t elapsed = 0; // in usec
		while (atomic_load(&mActiveC) && elapsed < (kSyphonDispatchUnloadTimeout * USEC_PER_SEC)) {
			dispatch_time_t timeout = dispatch_time(DISPATCH_TIME_NOW, (kSyphonDispatchUnloadTimeout * USEC_PER_SEC) - elapsed);
			dispatch_semaphore_wait(_SyphonDispatchGetWorkSemaphore(), timeout);
			struct timeval now;
			if (gettimeofday(&now, NULL) != 0) break;
			elapsed = ((now.tv_sec - start.tv_sec) * USEC_PER_SEC) + (now.tv_usec - start.tv_usec);
		}
	}
	dispatch_release(_SyphonDispatchGetWorkSemaphore());
}

#pragma mark Channel Loop
static void *_SyphonDispatchChannelLoop(SyphonDispatchChannel *channel)
{
#ifdef SYPHON_DISPATCH_DEBUG_LOGGING
	uint64_t tid;
	pthread_threadid_np(NULL,&tid);
	printf("channel %llu - start\n", tid);
#endif
	pthread_setname_np("info.v002.syphon.dispatch"); // shows up in gdb
	dispatch_semaphore_t workDoneSem = _SyphonDispatchGetWorkSemaphore();
	while (!atomic_load(&channel->done))
	{
		SyphonDispatchSourceRef source = channel->activeSource;
		channel->activeSource = NULL;
		
		int32_t firec = source->firec;
		while (firec > 0)
		{
#ifdef SYPHON_DISPATCH_DEBUG_LOGGING
//			printf("channel %llu - fire\n", tid);
#endif			
			source->fblock();
			firec = atomic_fetch_sub(&source->firec, 1) - 1;
		}
		// return to pool and then release
		// so the channel can be destroyed in release if necessary
		_SyphonDispatchChannelReturnToPool(channel);
		_SyphonDispatchSourceRelease(source, true);
		
		// signal done work so app can exit
		atomic_fetch_sub(&mActiveC, 1);
		dispatch_semaphore_signal(workDoneSem);
		
#ifdef SYPHON_DISPATCH_DEBUG_LOGGING
//		printf("channel %llu - wait\n", tid);
#endif
		// wait for something to happen
		dispatch_semaphore_wait(channel->signal, DISPATCH_TIME_FOREVER);
	}
	dispatch_release(channel->signal);
	free(channel);
#ifdef SYPHON_DISPATCH_DEBUG_LOGGING
	printf("channel %llu - finish\n", tid);
#endif
	return NULL;
}

static dispatch_semaphore_t _SyphonDispatchGetWorkSemaphore(void)
{
    dispatch_semaphore_t sem = (dispatch_semaphore_t)atomic_load(&mWorkDoneSignal);
	if (!sem)
	{
		sem = dispatch_semaphore_create(0);
        uintptr_t expected = (uintptr_t)NULL;
        if (!atomic_compare_exchange_strong(&mWorkDoneSignal, &expected, (uintptr_t)sem))
		{
			// setting failed, some other thread must have got there first
			dispatch_release(sem);
            sem = (dispatch_semaphore_t)expected;
		}
	}
	return sem;
}

#pragma mark Sources
SyphonDispatchSourceRef SyphonDispatchSourceCreate(void (^block)(void))
{
	if (block)
	{
		SyphonDispatchSourceRef source = malloc(sizeof(SyphonDispatchSource));
		if (source)
		{
			source->retainc = 1;
			source->fblock = Block_copy(block);
			source->firec = 0;
			atomic_store(&source->cblock, (uintptr_t)NULL);
			atomic_fetch_add(&mSourceC, 1);
		}
		return source;
	}
	else
	{
		return NULL;
	}

}

void SyphonDispatchSourceSetCompletionBlock(SyphonDispatchSourceRef source, void (^block)(void))
{
    void (^copied)(void) = Block_copy(block);
    uintptr_t old;
	bool result;
	do {
        old = atomic_load(&source->cblock);
        result = atomic_compare_exchange_strong(&source->cblock, &old, (uintptr_t)copied);
	} while (!result);
	if (old) Block_release((void (^)(void))old);
}

SyphonDispatchSourceRef SyphonDispatchSourceRetain(SyphonDispatchSourceRef source)
{
	if (source)
	{
		atomic_fetch_add(&source->retainc, 1);
	}
	return source;
}

void SyphonDispatchSourceRelease(SyphonDispatchSourceRef source)
{
	_SyphonDispatchSourceRelease(source, false);
}

static void _SyphonDispatchSourceRelease(SyphonDispatchSourceRef source, bool onChannel)
{
	if (source && (atomic_fetch_sub(&source->retainc, 1) == 1))
	{
        void (^cblock)(void) = (void (^)(void))atomic_load(&source->cblock);
		if (cblock)
		{
			if (onChannel)
			{
				// fire the completion block
				cblock();
			}
			else
			{
				// fire the completion block on a new source so it too happens in the background
				SyphonDispatchSourceRef csource = SyphonDispatchSourceCreate(cblock);
				SyphonDispatchSourceFire(csource);
				SyphonDispatchSourceRelease(csource);
			}

			Block_release(cblock);
		}
		Block_release(source->fblock);
		free(source);
		atomic_fetch_sub(&mSourceC, 1);
		bool overLimit;
        int_fast32_t channelC = atomic_load(&mChannelC);
		do {
			overLimit = (channelC > atomic_load(&mSourceC));
			if (overLimit)
			{
				SyphonDispatchChannel *channel = _SyphonDispatchChannelTryFromPool();
				if (channel)
				{
                    if (atomic_compare_exchange_strong(&mChannelC, &channelC, channelC - 1))
					{
						_SyphonDispatchChannelDestroy(channel);
                        channelC = channelC - 1;
					}
					else
					{						
						// otherwise the channel-count changed so return it to the pool and try again
                        // channelC has been updated by atomic_compare_exchange_strong()
						_SyphonDispatchChannelReturnToPool(channel);
					}
				}
				else
                {
                    // another thread may have deleted a channel, so update the count
                    channelC = atomic_load(&mChannelC);
                }
			}
			
		} while (overLimit);
	}
}

void SyphonDispatchSourceFire(SyphonDispatchSourceRef source)
{
	if (source)
	{
		if (atomic_fetch_add(&source->firec, 1) == 0)
		{
			// if we incremented to 1 then this source is not currently on a channel
			// so launch it
			atomic_fetch_add(&mActiveC, 1);
			_SyphonDispatchChannelLaunchFromPool(source);
		}
	}
}

#pragma mark Channels

static inline void _SyphonDispatchChannelLaunchFromPool(SyphonDispatchSourceRef source)
{
	// we retain the source until it has finished this and any subsequent fires
	SyphonDispatchSourceRetain(source);
	// look for an existing free channel
	SyphonDispatchChannel *channel = _SyphonDispatchChannelTryFromPool();
	if (channel)
	{
		// we found an existing channel in the pool
		channel->activeSource = source;
				
		// signal the channel to wake
		dispatch_semaphore_signal(channel->signal);
	}
	else
	{
		// we didn't find a free channel, so create a new one
		channel = malloc(sizeof(SyphonDispatchChannel));
		if (channel)
		{
			channel->next = NULL;
			channel->activeSource = source;
			channel->signal = dispatch_semaphore_create(0);
			atomic_store(&channel->done, 0);
			
			// create a detached thread so it will clean itself up when it exits
			pthread_t thread;
			pthread_attr_t attr;
			pthread_attr_init(&attr);
			pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
			if (pthread_create(&thread, &attr, (void *(*)(void *))_SyphonDispatchChannelLoop, channel) != 0)
			{
				// we couldn't create a new thread
				dispatch_release(channel->signal);
				free(channel);
				SyphonDispatchSourceRelease(source);
			}
			else
			{
				atomic_fetch_add(&mChannelC, 1);
			}
		}
	}
}

static void _SyphonDispatchChannelDestroy(SyphonDispatchChannel *channel)
{
	atomic_store(&channel->done, true);
	dispatch_semaphore_signal(channel->signal);
	// channel will be freed on its own thread
}

