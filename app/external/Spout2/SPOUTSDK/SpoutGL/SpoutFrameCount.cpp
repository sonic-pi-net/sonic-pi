//
//		SpoutFrameCount
//
//		Semaphore frame counter
//
// ====================================================================================
//		Revisions :
//
//		07.10.18	- project start
//		01.11.18	- Add GetRefreshRate() to set default sender fps to system refresh rate
//		16.11.18	- Profile UpdateSenderFps
//		23.11.18	- Change semaphore access functions to operate within a mutex lock
//		26.11.18	- Add application disable frame counting to avoid variable frame rate problems
//		27.11.18	- Add IsFrameNew
//		02.12.18	- Add sender GetWidth, GetHeight, GetFps and GetFrame
//		23.12.18	- More log warnings
//		26.02.19	- Add IsFrameCountEnabled
//		14.03.19	- CleanupFrameCount - return if no semaphore handle
//					- Remove wait warning from CheckAccess()
//		02.04.19	- Profile timing functions
//		24.04.19	- Add HoldFps
//		19.05.19	- Clean up
//		05.06.19	- HoldFps - use std::chrono if VS2015 or greater
//		03.03.20	- Introduce DX11 keyed mutex locks in addition to named mutex
//		11.03.20	- General cleanup
//					  Result switch for WaitForSingleObject
//		05.05.20	- Mutex access timing tests documented within functions
//		18.06.20	- Update comments
//		06.09.20	- Add more notice logs to EnableFrameCount
//		23.09.20	- Initialize m_lastFrame, m_FrameStart, m_bIsNewFrame
//		24.09.20	- Remove m_FrameStartPtr and m_FrameEndPtr null checks in destructor
//		14.12.10	- independent std::chrono timing for sender frame count and HoldFps
//					  Testing and code optimization
//		18.12.20	- Add SetFrameCount for registry setting
//		04.02.21	- Reset timers in EnableFrameCount
//		02.04.21	- Add sync event functions
//					  SetFrameSync/WaitFrameSync/OpenFrameSync/CloseFrameSync
//		07.04.21	- CloseFrameSync public for use by other classes
//		17.04.21	- WaitFrameSync - close handle on error
//		21.07.21	- Remove debug comment
//		10.08.21	- Default m_bIsNewFrame true to allow for apps without frame count
//		05.10.21	- HoldFps - correct start time
//		24.10.21	- If registry frame count key is not present or disabled,
//					  set the new frame flag m_bIsNewFrame true.
//					- Set default new frame true in GetNewFrame(),
//					  false only if the frame number equals the last.
//		25.10.21	- HoldFps change from int to double.
//					  Use monitor refresh rate if no argument is specified.
//		13.11.21	- Revise UpdateSenderFps
//					  Zero frame counter variables on reset and init
//		25.01.22	- Clean up logs in CreateAccessMutex and EnableFrameCount
//		21.02.22	- Change "_uuidof" to "__uuidof" in CheckKeyedAccess. PR#81
//		15.05.22	- CheckKeyedAccess - change WAIT_OBJECT_0 to S_OK
//		27.07.22	- Change "_uuidof" to "__uuidof" in AllowKeyedAccess. PR#84
//		29.07.22	- Correct "case case" typo in CheckKeyedAccess
//					  Add case E_FAIL
//		28.10.22	- Code documentation
//		10.11.22	- Revise HoldFps
//					  Remove m_millisForFrame
//					  Include TimeBeginPeriod/TimeEndPeriod 
//					  to reduce Windows timing period to minimum.
//		18.11.22	- Move performance counter functions to SpoutUtils
//		21.11.22	- Extend CleanupFrameCount and use in destructor
//					- Correct average frame rate in UpdateSenderFps
//					- Correct GetNewFrame for receiver started.
//		17.12.22	- Use smart pointers for m_FrameStartPtr etc to avoid using new/delete
//		18.12.22	- Change back to new/delete due to incompatibility with SpoutLibrary
//		22.12.22	- Compiler compatibility check
//		06.01.23	- CheckKeyedAccess - switch on hr to avoid narrowing cast to DWORD
//					  Avoid c-style cast where possible
//		08.01.23	- CheckTextureAccess/AllowTextureAccess 
//					  remove texture check for default null texture
//					  Code review - Use Microsoft Native Recommended rules
//		19.03.23	- WaitFrameSync - do not block if the sender has not created a sync event
//
// ====================================================================================
//
/*
	Copyright (c) 2019-2023. Lynn Jarvis. All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, 
	are permitted provided that the following conditions are met:

		1. Redistributions of source code must retain the above copyright notice, 
		   this list of conditions and the following disclaimer.

		2. Redistributions in binary form must reproduce the above copyright notice, 
		   this list of conditions and the following disclaimer in the documentation 
		   and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"	AND ANY 
	EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
	OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE	ARE DICLAIMED. 
	IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
	PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "SpoutFrameCount.h"

//
// Class: spoutFrameCount
//
// Semaphore frame counter.
//
// Refer to source code for documentation.
//

// -----------------------------------------------
spoutFrameCount::spoutFrameCount()
{
	m_hAccessMutex = NULL;
	m_hCountSemaphore = NULL;
	m_hSyncEvent = NULL;
	m_SenderName[0] = 0;
	m_CountSemaphoreName[0] = 0;
	
	m_FrameCount = 0L;
	m_LastFrameCount = 0L;
	m_FrameTimeTotal = 0.0;
	m_FrameTimeNumber = 0.0;
	m_lastFrame = 0.0;
	m_SenderFps = GetRefreshRate(); // Default sender fps is system refresh rate
	m_PeriodMin = 0; // For setting Windows time period
	m_bIsNewFrame = true; // Default true for apps without frame count

	// Check the registry setting for frame counting between sender and receiver
	m_bFrameCount = false; // default not set
	DWORD dwFrame = 0;
	if (ReadDwordFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "Framecount", &dwFrame)) {
		m_bFrameCount = (dwFrame == 1);
	}

	// If frame counting is disabled, set the new frame flag true
	if (!m_bFrameCount)
		m_bIsNewFrame = true;

	// Frame counting not disabled specifically for this application.
	// This can be set by the application if required.
	m_bDisabled = false;

#ifdef USE_CHRONO

	// For HoldFps
	m_FrameStartPtr = new std::chrono::steady_clock::time_point;
	m_FrameEndPtr = new std::chrono::steady_clock::time_point;

	// Sender fps
	m_FpsStartPtr = new std::chrono::steady_clock::time_point;
	m_FpsEndPtr = new std::chrono::steady_clock::time_point;

	// Reset both counts
	*m_FrameStartPtr = *m_FrameEndPtr = std::chrono::steady_clock::now();
	*m_FpsStartPtr = *m_FpsEndPtr = std::chrono::steady_clock::now();

#else
	// Initialize PC msec frequency counter
	StartCounter();
#endif

}

// -----------------------------------------------
spoutFrameCount::~spoutFrameCount()
{

#ifdef USE_CHRONO
	if(m_FrameStartPtr)	delete m_FrameStartPtr;
	if(m_FrameEndPtr) delete m_FrameEndPtr;
	if(m_FpsStartPtr) delete m_FpsStartPtr;
	if(m_FpsEndPtr) delete m_FpsEndPtr;
#endif

	if (m_hCountSemaphore) CloseHandle(m_hCountSemaphore);
	if (m_hAccessMutex) CloseHandle(m_hAccessMutex);
	if (m_hSyncEvent) CloseHandle(m_hSyncEvent);

}


// ======================================================================
//								Public
// ======================================================================

//
// Group: Frame counting
//

// -----------------------------------------------
// Function: SetFrameCount
// Enable or disable frame counting globally by registry setting
void spoutFrameCount::SetFrameCount(bool bEnable)
{
	if (bEnable) {
		// Frame counting not already set to registry
		if (!m_bFrameCount) {
			WriteDwordToRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "Framecount", 1);
			m_bFrameCount = true;
			m_bDisabled = false; // Application disable flag
		}
		// Do nothing if already set
	}
	else {
		// Frame counting already set to registry
		if (m_bFrameCount) {
			// Clean up existing objects
			if (IsFrameCountEnabled())
				CleanupFrameCount();
		}
		WriteDwordToRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "Framecount", 0);
		m_bFrameCount = false;
		m_bDisabled = false;
	}
}

// -----------------------------------------------
// Function: EnableFrameCount
// Enable frame counting for this sender.
//
// Create a frame counting semaphore.
//
// Incremented by a sender, tested by a receiver to retrieve the count.
void spoutFrameCount::EnableFrameCount(const char* SenderName)
{
	if (!SenderName) {
		SpoutLogWarning("SpoutFrameCount::EnableFrameCount - no sender name");
		return;
	}

	// Return if frame counting not recorded in the registry
	// Subsequently SetNewFrame and GetNewFrame return without action
	if (!m_bFrameCount) {
		SpoutLogNotice("SpoutFrameCount::EnableFrameCount : setting not enabled");
		return;
	}

	// Return if application disabled
	if (m_bDisabled) {
		SpoutLogNotice("SpoutFrameCount::EnableFrameCount : application disabled");
		return;
	}

	// Reset frame count, comparator and fps variables
	m_FrameCount = 0L;
	m_LastFrameCount = 0L;
	m_FrameTimeTotal = 0.0;
	m_FrameTimeNumber = 0.0;
	m_SenderFps = GetRefreshRate(); // Default sender fps is system refresh rate

	// Reset timers
#ifdef USE_CHRONO
	// Reset the counts
	*m_FrameStartPtr = *m_FrameEndPtr = std::chrono::steady_clock::now();
	*m_FpsStartPtr = *m_FpsEndPtr = std::chrono::steady_clock::now();
#else
	// Initialize PC msec frequency counter
	StartCounter();
#endif

	// Return if already enabled for this sender
	// The sender name can be the same if the adapter has changed
	if (m_hCountSemaphore) {
		SpoutLogNotice("SpoutFrameCount::EnableFrameCount (%s) frame count semaphore already enabled [0x%.7X] ", SenderName, m_hCountSemaphore);
		return;
	}

	//
	// Frame count semaphore (m_hCountSemaphore) has not been created yet
	//

	// Set the new name for subsequent checks
	strcpy_s(m_SenderName, 256, SenderName);

	// Create a name for the frame count semaphore using the sender name
	sprintf_s(m_CountSemaphoreName, 256, "%s_Count_Semaphore", SenderName);

	// Create or open a named frame count semaphore with this name
	HANDLE hSemaphore = CreateSemaphoreA(
		NULL, // default security attributes
		1, // initial count
		LONG_MAX, // maximum count - LONG_MAX (2147483647) at 60fps = 2071 days
		m_CountSemaphoreName);

	const DWORD dwError = GetLastError();
	switch(dwError) {

		case ERROR_INVALID_HANDLE:
			SpoutLogError("    Invalid semaphore handle");
			break;

		case ERROR_ALREADY_EXISTS:
			SpoutLogNotice("SpoutFrameCount::EnableFrameCount - frame count semaphore [%s] exists", m_CountSemaphoreName);
			SpoutLogNotice("    Handle for access [0x%7.7X]", LOWORD(hSemaphore));
			// OK if it already exists - either the sender or receiver can create it
			break;
		
		case ERROR_SUCCESS:
			SpoutLogNotice("SpoutFrameCount::EnableFrameCount - frame count semaphore [%s] created", m_CountSemaphoreName);
			SpoutLogNotice("    Handle [0x%7.7X]", LOWORD(hSemaphore));
			break;

		default :
			SpoutLogNotice("SpoutFrameCount::EnableFrameCount - unknown error %d (0x%X)", dwError, dwError);
			break;
	}

	if (hSemaphore == NULL) {
		SpoutLogError("SpoutFrameCount::EnableFrameCount - could not create handle");
	}

	// Save the handle for access - it could be NULL
	m_hCountSemaphore = hSemaphore;

}

// -----------------------------------------------
// Function: DisableFrameCount
// Disable frame counting for this application
void spoutFrameCount::DisableFrameCount()
{
	CleanupFrameCount();
	m_bDisabled = true;
}

void spoutFrameCount::PauseFrameCount(bool bPaused)
{
	m_bDisabled = bPaused;
}


// -----------------------------------------------
// Function: IsFrameCountEnabled
// Check status of frame counting
bool spoutFrameCount::IsFrameCountEnabled()
{
	if (!m_bFrameCount || m_bDisabled)
		return false;
	else
		return true;

}


// -----------------------------------------------
// Function: IsFrameNew
//
//  Is the received frame new ?
//
// This function can be used by a receiver after ReceiveTexture
// to determine whether the frame just received is new.
// Used by an application to avoid time consuming processing
// after receiving a texture.
// Not usually required because new frame status is always 
// checked internally if frame counting is enabled.
//
bool spoutFrameCount::IsFrameNew()
{
	return m_bIsNewFrame;
}

// -----------------------------------------------
// Function: GetSenderFps
// Received frame rate
double spoutFrameCount::GetSenderFps()
{
	return m_SenderFps;
}


// -----------------------------------------------
// Function: GetSenderFrame
// Received frame count
long spoutFrameCount::GetSenderFrame()
{
	return m_FrameCount;
}

// -----------------------------------------------
// Function: HoldFps
// Frame rate control
//
// Hold a desired frame rate if the application does not already
// have frame rate control. Must be called every frame.
// The sender will then signal a new frame at the target rate.
//
// Note that this function is affected by changes to Windows timer 
// resolution since Windows 10 Version 2004 (April 2020)
// https://randomascii.wordpress.com/2020/10/04/windows-timer-resolution-the-great-rule-change/
//
// TimeBeginPeriod / TimeEndPeriod avoid loss of precision
// https://learn.microsoft.com/en-us/windows/win32/api/timeapi/nf-timeapi-timebeginperiod
// Microsoft remark :
//   Call this function immediately before using timer services, and call the timeEndPeriod
//   function immediately after you are finished using the timer services. An application 
//   can make multiple timeBeginPeriod calls as long as each call is matched with a call
//   to timeEndPeriod.
// 
void spoutFrameCount::HoldFps(int fps)
{
	// Unlikely but return anyway
	if (fps <= 0)
		return;

	// Reduce Windows timer period to minimum
	StartTimePeriod();

	// Target frame time
	const double target = (1000000.0/static_cast<double>(fps))/1000.0; // msec

#ifdef USE_CHRONO

	// Time now end point
	*m_FrameEndPtr = std::chrono::steady_clock::now();

	// Milliseconds elapsed
	const double elapsedTime = static_cast<double>(std::chrono::duration_cast<std::chrono::nanoseconds>(*m_FrameEndPtr - *m_FrameStartPtr).count()/1000000.0);

	// Sleep to reach the target frame time
	if (elapsedTime < target) {
		std::this_thread::sleep_for(std::chrono::milliseconds(static_cast<long long>(target - elapsedTime)));
	}

	// Set start time for the next frame
	*m_FrameStartPtr = std::chrono::steady_clock::now();

#else

	// Milliseconds elapsed
	double elapsedTime = EndTiming();

	// Sleep to reach the target frame time
	if (elapsedTime < target)
		Sleep((DWORD)(target - elapsedTime));

	// Set start time for the next frame
	StartTiming();

#endif

	// Reset Windows timer period
	EndTimePeriod();

}


// -----------------------------------------------
// Function: SetNewFrame
// Increment the sender frame count.
//
// Used by a sender for every update of the shared texture.
// This function is called within a sender mutex lock so that
// the receiver will not read the semaphore count while the
// sender is incrementing it.
// Used internally to set frame status if frame counting is enabled.
void spoutFrameCount::SetNewFrame()
{
	// Return silently if disabled
	if (!m_bFrameCount || m_bDisabled)
		return;

	// Access the frame count semaphore
	// Note: WaitForSingle object will always succeed because
	// the lock count (sender frame count) is greater than zero,
	// but must be called before ReleaseSemaphore can be called
	// or there is an error.
	const DWORD dwWaitResult = WaitForSingleObject(m_hCountSemaphore, 0);
	switch (dwWaitResult) {
		case WAIT_OBJECT_0:
			// Release the frame counting semaphore to increase it's count.
			// so that the receiver can retrieve the new count.
			// Increment by 2 because WaitForSingleObject decremented it.
			if (ReleaseSemaphore(m_hCountSemaphore, 2, NULL) == false) {
				SpoutLogError("spoutFrameCount::SetNewFrame - ReleaseSemaphore failed");
			}
			else {
				// Increment the sender frame count
				m_FrameCount++;
				// Update the sender fps calculations for the new frame
				UpdateSenderFps(1);
			}
			return;
		case WAIT_ABANDONED:
			SpoutLogError("SpoutFrameCount::SetNewFrame - WAIT_ABANDONED");
			break;
		case WAIT_FAILED:
			SpoutLogError("SpoutFrameCount::SetNewFrame - WAIT_FAILED");
			break;
		default:
			break;
	}

}

// -----------------------------------------------
// Function: GetNewFrame
//
// Read the semaphore count to determine if the sender
// has produced a new frame and incremented the counter.
// Counts are recorded as class variables for a receiver.
//
// This function is called within a sender mutex lock so that
// the sender will not write a frame and increment the 
// count while a receiver is reading it.
// Used internally to check frame status if frame counting is enabled.
//
bool spoutFrameCount::GetNewFrame()
{
	long framecount = 0;

	// Return silently if disabled
	if (!m_bFrameCount || m_bDisabled)
		return true;

	// A receiver creates or opens a named semaphore when it connects to a sender
	// Do not block if semaphore creation failed so that ReceiveTexture can still be called
	if (!m_hCountSemaphore) {
		return true;
	}

	// Access the frame count semaphore
	// WaitForSingleObject decrements the semaphore's count by one.
	const DWORD dwWaitResult = WaitForSingleObject(m_hCountSemaphore, 0);
	switch (dwWaitResult) {
		case WAIT_OBJECT_0:
			// Call ReleaseSemaphore with a release count of 1 to return it
			// to what it was before the wait and record the previous count.
			// The next time round it will either be the same count because
			// the receiver released it, or increased because the sender
			// released and incremented it.
			if (ReleaseSemaphore(m_hCountSemaphore, 1, &framecount) == false) {
				SpoutLogError("spoutFrameCount::GetNewFrame - ReleaseSemaphore failed");
				return true; // do not block
			}
			break;
		case WAIT_ABANDONED :
			SpoutLogWarning("SpoutFrameCount::GetNewFrame - WAIT_ABANDONED");
			break;
		case WAIT_FAILED :
			SpoutLogWarning("SpoutFrameCount::GetNewFrame - WAIT_FAILED");
			break;
		default :
			break;
	}

	// Update the global frame count
	m_FrameCount = framecount;

	// Set a new frame by default, but test below and set false if this frame and the last are the same.
	m_bIsNewFrame = true;

	// Count will still be zero for apps that do not set a frame count
	if (framecount == 0)
		return true;

	// If this count and the last are the same, the sender has not
	// produced a new frame and incremented the counter.
	// Return false if this frame and the last are the same.
	if (framecount == m_LastFrameCount) {
		m_bIsNewFrame = false;
		return false;
	}

	//
	// Update the sender fps calculations.
	//
	// The sender might have produced more than one frame if the receiver is slower.
	// Pass the number of frames produced since the last. If m_LastFrameCount = 0, 
	// the receiver has just started. Give it a frame to get the next frame count.
	if(m_LastFrameCount > 0)
		UpdateSenderFps(framecount - m_LastFrameCount);

	m_LastFrameCount = framecount;

	return true;

}


// -----------------------------------------------
// Function: CleanupFrameCount
// For class cleanup functions
void spoutFrameCount::CleanupFrameCount()
{
	SpoutLogNotice("SpoutFrameCount::CleanupFrameCount");

	try {
		// Close the frame count semaphore. If another application first
		// opened the semaphore it will not be finally closed here.
		if (m_hCountSemaphore) CloseHandle(m_hCountSemaphore);
		m_hCountSemaphore = NULL;

		// Close the texture access mutex
		if (m_hAccessMutex) CloseHandle(m_hAccessMutex);
		m_hAccessMutex = NULL;

		// Close the sync event
		// Also closed in sender/receiver release
		CloseFrameSync();

		// Clear the sender name in case the same one opens again
		m_SenderName[0] = 0;

		// Reset counters
		m_FrameCount = 0L;
		m_LastFrameCount = 0L;
		m_FrameTimeTotal = 0.0;
		m_FrameTimeNumber = 0.0;
		m_SenderFps = GetRefreshRate(); // Default sender fps is system refresh rate
	}
	catch (...) {
		SpoutLogError("SpoutFrameCount::CleanupFrameCount caused an exception");
	}

}

// =================================================================
//                     Texture access mutex
// =================================================================

//
// Texture access mutex
//

// -----------------------------------------------
// Function: CheckTextureAccess
// Test for texture access using a named sender mutex or keyed texture mutex.
//
// Checks do not block if no mutex for Spout1 apps
// or if called when the sender has closed.
//
// The DX11 texture pointer argument should always be null for DX9 mode.
//
bool spoutFrameCount::CheckTextureAccess(ID3D11Texture2D* D3D11texture)
{
	// Test for a keyed mutex.
	// If no texture was passed in, the function returns false
	if (IsKeyedMutex(D3D11texture)) {
		// Use a keyed mutex if the DX11 texture supports it
		return CheckKeyedAccess(D3D11texture);
	}
	else {
		// Texture is not keyed or no texture passed in. Use the named mutex.
		// Returns true without blocking if the mutex does not exist
		return CheckAccess();
	}
}

// -----------------------------------------------
// Function: AllowTextureAccess
// Release mutex and allow texture access
bool spoutFrameCount::AllowTextureAccess(ID3D11Texture2D* D3D11texture)
{
	// Test for a keyed mutex.
	// If no texture was passed in, the function returns false
	if (IsKeyedMutex(D3D11texture)) {
		// Use a keyed mutex if the DX11 texture supports it
		return(AllowKeyedAccess(D3D11texture));
	}
	else {
		// Texture is not keyed or no texture passed in, use the named mutex
		// Do not block if the mutex does not exist
		AllowAccess();
		return true;
	}
}

// -----------------------------------------------
// Function: CreateAccessMutex
// Create named mutex for a sender
//
// Create or open mutex depending, on whether it already exists or not
//
//  - A sender will create one.
//
//  - A receiver will open an existing one for a specific sender.
//
// A receiver should not open a mutex until a sender is found to connect to.
// If that sender does not have a mutex, one will be created
// and will always be available to the receiver.
//
bool spoutFrameCount::CreateAccessMutex(const char *SenderName)
{
	if (!SenderName)
		return false;

	DWORD errnum = 0;
	char szMutexName[512]={};
	HANDLE hMutex = NULL;

	// Create the mutex name to control access to the shared texture
	sprintf_s(szMutexName, 512, "%s_SpoutAccessMutex", SenderName);

	// Create or open a mutex
	// Sender and receiver will either open the mutex or create one
	hMutex = CreateMutexA(NULL, false, szMutexName);
	if (hMutex == NULL) {
		SpoutLogError("spoutFrameCount::CreateAccessMutex - NULL handle");
		return false;
	}
	else {
		errnum = GetLastError();
		if (errnum == ERROR_INVALID_HANDLE) {
			SpoutLogError("spoutFrameCount::CreateAccessMutex - [%s] invalid handle", szMutexName);
			return false;
		}
		// Here we can find if the mutex already exists
		else if (errnum == ERROR_ALREADY_EXISTS) {
			SpoutLogNotice("spoutFrameCount::CreateAccessMutex - texture access mutex [%s] exists", szMutexName);
			SpoutLogNotice("    Handle for access [0x%.7X]", PtrToUint(hMutex));
		}
		else {
			SpoutLogNotice("spoutFrameCount::CreateAccessMutex - texture access mutex [%s] created ", szMutexName);
			SpoutLogNotice("    Handle [0x%.7X]", PtrToUint(hMutex));
		}
	}

	// Save the handle for access
	m_hAccessMutex = hMutex;

	return true;

}

// -----------------------------------------------
// Function: CloseAccessMutex
// Close the texture access mutex.
//
// If another application first opened the mutex it will not be finally closed here.
//
void spoutFrameCount::CloseAccessMutex()
{
	SpoutLogNotice("SpoutFrameCount::CloseAccessMutex");
	if (m_hAccessMutex) CloseHandle(m_hAccessMutex);
	m_hAccessMutex = NULL;
}

// -----------------------------------------------
// Function: CheckAccess
// Test access using a named mutex
//
// Check whether any other process is holding the lock.
//
// Wait for access for up to 4 frames if so.
//
// If receiving from Spout 1 apps with no mutex lock,
// a reader will have created the mutex and will have
// sole access and rely on the interop locks.
//
bool spoutFrameCount::CheckAccess()
{
	// Don't block if no mutex for Spout1 apps or if called when the sender has closed.
	// AllowAccess also tests for a null handle before releasing the mutex.
	if (!m_hAccessMutex) {
		return true;
	}

	// Typically 1-3 microseconds.
	// 10 receivers - no increase.
	//
	// Note that NVIDIA "Threaded optimization" can cause a delay for WaitForSingleObject
	// and can be set OFF by the NVIDIA control panel or by SpoutSettings.
	//
	const DWORD dwWaitResult = WaitForSingleObject(m_hAccessMutex, 67); // timeout 4 frames at 60fps
	switch (dwWaitResult) {
		case WAIT_OBJECT_0 : // 0
			// The state of the object is signalled.
			// The thread got ownership of the mutex
			return true;
		case WAIT_ABANDONED: // 0x00000080L
			SpoutLogError("spoutFrameCount::CheckAccess - WAIT_ABANDONED");
			break;
		case WAIT_TIMEOUT: // 0x00000102L
			// The time-out interval elapsed, and the object's state is non-signalled.
			// This can happen the first time a receiver connects to a sender.
			// TODO : close mutex after some wait time
			break;
		case WAIT_FAILED: // 0xFFFFFFFF
			// Could use call GetLastError here
			SpoutLogError("spoutFrameCount::CheckAccess - WAIT_FAILED");
			break;
		default:
			SpoutLogError("spoutFrameCount::CheckAccess - unknown error");
			break;
	}

	return false;
}

// -----------------------------------------------
// Function: AllowAccess
// Release named mutex and a allow access after gaining ownership
// Do not block for no mutex
void spoutFrameCount::AllowAccess()
{
	// Don't block if no mutex for Spout1 apps or if called when the sender has closed.

	// < 1 microsecond
	// Release ownership of the mutex object.
	// The caller must call ReleaseMutex once for each time that the mutex satisfied a wait.
	// The ReleaseMutex function fails if the caller does not own the mutex object
	if (m_hAccessMutex)
		ReleaseMutex(m_hAccessMutex);

}

//
// Group: Sync events
//

// -----------------------------------------------
// Function: SetFrameSync
// Signal sync event.
//
// Creates a named sync event and sets for test.
void spoutFrameCount::SetFrameSync(const char *sendername)
{
	if (!sendername)
		return;

	// Create the sync event if not already
	if (!m_hSyncEvent)
		OpenFrameSync(sendername);

	// Set the event to signalled
	if (m_hSyncEvent) {
		if (!SetEvent(m_hSyncEvent)) {
			SpoutLogError("spoutFrameCount::SetFrameSync error (%d)", GetLastError());
		}
	}

}

// -----------------------------------------------
// Function: WaitFrameSync
// Wait or test for named sync event.
//
// Wait until the sync event is signalled or the timeout elapses.
bool spoutFrameCount::WaitFrameSync(const char *sendername, DWORD dwTimeout)
{
	if (!sendername)
		return false;

	char SyncEventName[256]={};
	sprintf_s(SyncEventName, 256, "%s_Sync_Event", sendername);

	HANDLE hSyncEvent = OpenEventA(
		EVENT_ALL_ACCESS, // security attributes 
		TRUE, // Inherit handle
		SyncEventName);

	if (!hSyncEvent) {
		SpoutLogError("spoutFrameCount::WaitFrameSync - no event");
		// Do not block if the sender has not created a sync event
		return true;
	}

	bool bSignal = false;
	const DWORD dwWaitResult = WaitForSingleObject(hSyncEvent, dwTimeout);
	switch (dwWaitResult) {
		case WAIT_OBJECT_0:
			// The state of the object is signalled.
			bSignal = true;
			break;
		case WAIT_ABANDONED:
			SpoutLogError("spoutFrameCount::WaitFrameSync - WAIT_ABANDONED");
			break;
		case WAIT_TIMEOUT: // The time-out interval elapsed, and the object's state is non-signalled.
			SpoutLogError("spoutFrameCount::WaitFrameSync - WAIT_TIMEOUT");
			break;
		case WAIT_FAILED: // Could use call GetLastError
			SpoutLogError("spoutFrameCount::WaitFrameSync - WAIT_FAILED");
			break;
		default:
			SpoutLogError("spoutFrameCount::WaitFrameSync - unknown error");
			break;
	}

	CloseHandle(hSyncEvent);

	return bSignal;

}


// -----------------------------------------------
// Function: CloseFrameSync
// Close event for sync to frame rate.
void spoutFrameCount::CloseFrameSync()
{
	if (m_hSyncEvent) {
		SpoutLogNotice("spoutFrameCount::CloseFrameSync");
		CloseHandle(m_hSyncEvent);
		m_hSyncEvent = NULL;
	}
}


// ===============================================================================
//                                Protected
// ===============================================================================

//
// Keyed mutex check
//
// Microsoft docs :
// https://learn.microsoft.com/en-us/windows/win32/api/dxgi/nf-dxgi-idxgikeyedmutex-acquiresync
// When a surface is created using the D3D10_RESOURCE_MISC_SHARED_KEYEDMUTEX value 
// of the D3D10_RESOURCE_MISC_FLAG enumeration, you must call the AcquireSync method
// before rendering to the surface. You must call the ReleaseSync method when you are
// done rendering to a surface.
//
// Testing confirms that if a DX11 texture has been created with a keyed mutex,
// AcquireSync/ReleaseSync must be used or CopyResource fails.
//
// These functions provide equivalent results to the general mutex functions
// for applications that produce keyed mutex textures.
//
bool spoutFrameCount::CheckKeyedAccess(ID3D11Texture2D* pTexture)
{
	// 85-90 microseconds
	if (pTexture) {
		IDXGIKeyedMutex* pDXGIKeyedMutex = nullptr;
		// Check the keyed mutex
		pTexture->QueryInterface(__uuidof(IDXGIKeyedMutex), (void**)&pDXGIKeyedMutex); // PR#81
		if (pDXGIKeyedMutex) {
			const HRESULT hr = pDXGIKeyedMutex->AcquireSync(0, 67);
			switch (hr) {

				case S_OK:
					// Sync was acquired
					// Return to process the texture and call AllowAccess to release sync
					pDXGIKeyedMutex->Release();
					return true;

				case static_cast<HRESULT>(WAIT_ABANDONED):
					// The shared surface and keyed mutex are no longer in a consistent state.
					SpoutLogError("spoutDirectX::CheckKeyedAccess : WAIT_ABANDONED");
					break;

				case static_cast<HRESULT>(WAIT_TIMEOUT):
					// The time-out interval elapsed before the key was released.
					// Can't access the shared texture right now, try again later
					SpoutLogError("spoutDirectX::CheckKeyedAccess : WAIT_TIMEOUT");
					break;

				case E_FAIL:
					// If the owning device attempted to create another keyed mutex 
					// on the same shared resource, AcquireSync returns E_FAIL.
					SpoutLogError("spoutDirectX::CheckKeyedAccess : E_FAIL");
					break;

				default:
					break;
			}

			// Error
			pDXGIKeyedMutex->ReleaseSync(0);
			pDXGIKeyedMutex->Release();
		}
	}
	return false;
}

// Release keyed mutex
bool spoutFrameCount::AllowKeyedAccess(ID3D11Texture2D* pTexture)
{
	// 22-24 microseconds
	if (pTexture) {
		IDXGIKeyedMutex* pDXGIKeyedMutex = nullptr;
		pTexture->QueryInterface(__uuidof(IDXGIKeyedMutex), (void**)&pDXGIKeyedMutex);
		if (pDXGIKeyedMutex) {
			pDXGIKeyedMutex->ReleaseSync(0);
			pDXGIKeyedMutex->Release();
			return true;
		}
	}
	return false;
}

bool spoutFrameCount::IsKeyedMutex(ID3D11Texture2D* D3D11texture)
{
	// Approximately 1.5 microseconds
	if (D3D11texture) {
		D3D11_TEXTURE2D_DESC desc={};
		D3D11texture->GetDesc(&desc);
		if (desc.MiscFlags & D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX) {
			return true;
		}
	}
	// Return to access by another method if no keyed mutex
	return false;
}


// -----------------------------------------------
// Calculate the sender frames per second
// Applications before 2.007 have a frame rate dependent on the system fps
void spoutFrameCount::UpdateSenderFps(long framecount)
{
	if (m_bDisabled)
		return;

	// If framecount is zero, the sender has not produced a new frame yet
	if (framecount > 0) {

		
#ifdef USE_CHRONO
		// End time since last call
		*m_FpsEndPtr = std::chrono::steady_clock::now();
		// Msecs between this frame and the last
		double frametime = static_cast<double>(std::chrono::duration_cast<std::chrono::nanoseconds>(*m_FpsEndPtr - *m_FpsStartPtr).count()/1000000.0);
#else
		// End time since last call
		double thisFrame = GetCounter();
		// Msecs between this frame and the last
		double frametime = thisFrame - m_lastFrame;
#endif
		
		if (frametime > 1.0) { // > 1 msec

			// printf("frametime = %f (%f) msec\n", frametime, 1000.0/m_SystemFps);

			// Frame time in seconds 
			frametime = frametime/1000.0;

			// Accumulate totals
			m_FrameTimeTotal = m_FrameTimeTotal + frametime;

			// Could have been more than one frame
			m_FrameTimeNumber += static_cast<double>(framecount);

			if (m_FrameTimeNumber > 8) {
				// Calculate average frames per second and m_SenderFps
				// (default fps is system refresh rate)
				const double avgframetime = m_FrameTimeTotal/m_FrameTimeNumber;
				if (avgframetime > 0.0001) {
					const double fps2 = (1.0 / avgframetime);
					// Damping to stabilise
					m_SenderFps = 0.95*m_SenderFps + 0.05*fps2;
				}
				m_FrameTimeTotal = 0.0;
				m_FrameTimeNumber = 0.0;
			}

		}

// Set the start time for the next frame
#ifdef USE_CHRONO
		*m_FpsStartPtr = std::chrono::steady_clock::now();
#else
		m_lastFrame = thisFrame;
#endif

	}
	else {

#ifdef USE_CHRONO
		// If framecount is zero, the sender has not produced a new frame yet
		*m_FpsStartPtr = std::chrono::steady_clock::now();
		*m_FpsEndPtr = std::chrono::steady_clock::now();
#endif

	}

}


// -----------------------------------------------
// Reduce Windows timing period to the minimum
// supported by the system (usually 1 msec)
void spoutFrameCount::StartTimePeriod()
{
	TIMECAPS tc={};
	m_PeriodMin = 0; // To allow for errors
	MMRESULT mres = timeGetDevCaps(&tc, sizeof(TIMECAPS));
	if (mres == MMSYSERR_NOERROR) {
		mres = timeBeginPeriod(tc.wPeriodMin);
		if (mres == TIMERR_NOERROR)
			m_PeriodMin = tc.wPeriodMin;
	}
}


// -----------------------------------------------
// Reset Windows timing period
void spoutFrameCount::EndTimePeriod()
{
	if (m_PeriodMin > 0) {
		timeEndPeriod(m_PeriodMin);
		m_PeriodMin = 0;
	}
}


// -----------------------------------------------
//
// Enable sync event
//
//   Create a named event that can be used to signal
//   from one application to another.
//
//   If used for frame sychronisation, the event name is 
//   derived from the sender name and is closed when a sender
//   closes or receiver releases connection.
//   A sender name must be established by the sender or receiver.
//   Only effective between one sender / receiver pair.
//
//   This function is used by the first call to SetFrameSync.
void spoutFrameCount::OpenFrameSync(const char* SenderName)
{
	// A sender name is required
	if (!SenderName || *SenderName) {
		SpoutLogWarning("spoutFrameCount::OpenFrameSync - no sender name");
		return;
	}

	// Return if already enabled for this sender
	if (m_hSyncEvent && strcmp(SenderName, m_SenderName) == 0) {
		SpoutLogNotice("spoutFrameCount::OpenFrameSync : already enabled [0x%.7X]", LOWORD(m_hSyncEvent));
		return;
	}

	// Close any existing event for a new sender
	if (m_hSyncEvent) {
		CloseHandle(m_hSyncEvent);
		m_hSyncEvent = NULL;
	}

	// Set the new name for subsequent checks
	strcpy_s(m_SenderName, 256, SenderName);

	// Create or open an event with this sender name
	char SyncEventName[256]={};
	sprintf_s(SyncEventName, 256, "%s_Sync_Event", SenderName);
	HANDLE hSyncEvent = CreateEventA(
		NULL,  // Attributes
		FALSE, // Auto reset
		FALSE, // Initial state non-signalled
		SyncEventName);
	SpoutLogNotice("spoutFrameCount::OpenFrameSync [%s]", SyncEventName);

	switch (GetLastError()) {
		case ERROR_INVALID_HANDLE:
			SpoutLogError("    Invalid sync event handle");
			return;
		case ERROR_ALREADY_EXISTS:
			SpoutLogNotice("    Sync event already exists");
			break;
		default:
			break;
	}

	if (hSyncEvent == NULL) {
		SpoutLogError("    Unknown error");
		return;
	}

	m_hSyncEvent = hSyncEvent;
	SpoutLogNotice("    Sync event handle [0x%.7X]", LOWORD(m_hSyncEvent));

}

// ===============================================================================


