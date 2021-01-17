/*  -*- c++ -*-
    Abstract interpreter interface.
    Copyright (c) 2003 2004 stefan kersten.
    Copyright (c) 2013 tim blechmann.

    ====================================================================

    SuperCollider real time audio synthesis system
    Copyright (c) 2002 James McCartney. All rights reserved.
    http://www.audiosynth.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#pragma once

#include "SC_Export.h"
#include <cstdio>
#include <cstdarg>

// =====================================================================
// SC_LanguageClient - abstract sclang client.
// =====================================================================

SCLANG_DLLEXPORT class SC_LanguageClient* createLanguageClient(const char* name);
SCLANG_DLLEXPORT void destroyLanguageClient(class SC_LanguageClient*);

class SCLANG_DLLEXPORT SC_LanguageClient {
public:
    struct Options {
        Options(): mMemSpace(2 * 1024 * 1024), mMemGrow(256 * 1024), mPort(57120), mRuntimeDir(0) {}

        int mMemSpace; // memory space in bytes
        int mMemGrow; // memory growth in bytes
        int mPort; // network port number
        char* mRuntimeDir; // runtime directory
    };

protected:
    // create singleton instance
    SC_LanguageClient(const char* name);
    virtual ~SC_LanguageClient();
    friend void destroyLanguageClient(class SC_LanguageClient*);

public:
    // singleton instance access locking
    static void lockInstance();
    static void unlockInstance();

    // return the singleton instance
    static SC_LanguageClient* instance();
    static SC_LanguageClient* lockedInstance() {
        lockInstance();
        return instance();
    }

    // initialize language runtime
    void initRuntime(const Options& opt = Options());
    void shutdownRuntime();

    // return application name
    const char* getName() const;

    // library startup/shutdown
    bool isLibraryCompiled();
    void compileLibrary(bool standalone);
    void shutdownLibrary();
    void recompileLibrary(bool standalone);

    // interpreter access
    void lock();
    bool trylock();
    void unlock();

    struct VMGlobals* getVMGlobals();

    void setCmdLine(const char* buf, size_t size);
    void setCmdLine(const char* str);
    void setCmdLinef(const char* fmt, ...);
    void runLibrary(const char* methodName);
    void interpretCmdLine();
    void interpretPrintCmdLine();
    void executeFile(const char* fileName);
    void runMain();
    void stopMain();

    // post file access
    FILE* getPostFile();
    void setPostFile(FILE* file);

    // run (in case of a terminal client)
    virtual int run(int argc, char** argv);

    // post buffer output (subclass responsibility)
    //     should be thread-save.
    virtual void postText(const char* str, size_t len) = 0;
    virtual void postFlush(const char* str, size_t len) = 0;
    virtual void postError(const char* str, size_t len) = 0;
    // flush post buffer contents to screen.
    //     only called from the main language thread.
    virtual void flush() = 0;

    // command line argument handling utilities
    static void snprintMemArg(char* dst, size_t size, int arg);
    static bool parseMemArg(const char* arg, int* res);
    static bool parsePortArg(const char* arg, int* res);

    // AppClock driver
    //    to be called from client mainloop.
    void tick();
    // AppClock driver. WARNING: Must be called locked!
    // Returns whether there is anything scheduled,
    // and writes the scheduled absolute time, if any, into nextTime.
    bool tickLocked(double* nextTime);

protected:
    // language notifications, subclasses can override

    // called after language runtime has been initialized
    virtual void onInitRuntime();
    // called after the library has been compiled
    virtual void onLibraryStartup();
    // called before the library is shut down
    virtual void onLibraryShutdown();
    // called after the interpreter has been started
    virtual void onInterpStartup();

    void runLibrary(struct PyrSymbol* pyrSymbol);

private:
    friend void closeAllGUIScreens();
    friend void initGUIPrimitives();
    friend void initGUI();

private:
    class HiddenLanguageClient* mHiddenClient;
};

// =====================================================================
// library functions
// =====================================================================

extern void setPostFile(FILE* file);
extern "C" int vpost(const char* fmt, va_list vargs);
extern void post(const char* fmt, ...);
extern void postfl(const char* fmt, ...);
extern void postText(const char* text, long length);
extern void postChar(char c);
extern void error(const char* fmt, ...);
extern void flushPostBuf();
