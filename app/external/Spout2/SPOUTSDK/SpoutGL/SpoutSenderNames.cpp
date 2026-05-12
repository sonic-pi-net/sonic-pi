/*

	SpoutSenderNames.cpp

	Spout sender management

	Thanks and credit to Malcolm Bechard for modifications to this class

	https://github.com/mbechard	

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	25.04.14 - started class file
	27.05.14 - cleanup using memory map creation, open, close, lock
	05.06.14 - FindSenderName - allow for a null name entered
	08.06.14 - rebuild
	12.06.13 - major revision, included map handling
	23-07-14 - cleanup of DX9 / DX11 functions
			 - Changed CheckSender logic
	27.07-14 - changed mutex lock creation due to memory leak
	28-07-14 - major change
			 - remove handle management
			 - changed map creation and release
	30-07-14 - Map locks and cleanup
	31-07-14 - fixed duplicate names class object
	01-08-14 - fixed mutex handle leak / cleanup
	03-08-14 - fixed GetActiveSenderInfo
	-- names class revision additions --
	22-08-14 - activated event locks
	03.09.14 - cleanup
	10.10.14 - Restored CreateSender for use by DirectX apps
	01.08.15 - FindSender
				- return false if the the sender is not registered
				- if registered sender is no longer there release it
			 - CheckSender bug - Name for ReleaseSenderName was wrong
	24.08.15 - the active sender is the one selected or the last one opened by the user
			   so don't limit to the first sender
	15.09.15 - removed "using namespace std" from header
	24.02.16 - replaced #define MaxSenders with a global variable m_MaxSenders
			 - changed readSenderSetFromBuffer to create a buffer based on
			   the number of senders in the map passed
			 - changed writeBufferFromSenderSet to use the global m_MaxSenders
			 - Created SetMaxSenders to set m_MaxSenders
	03.07-16 - Use helper functions for conversion of 64bit HANDLE to uint32_t
			   and uint32_t to 64bit HANDLE
			   https://msdn.microsoft.com/en-us/library/aa384267%28VS.85%29.aspx
	22.05.17 - Initialize unused variables in SetTextureInfo
			 - __movsd for info shared memory access instead of memcpy
	09.03.18 - change to FindSender and CheckSender so that the sendernames map 
			   is not accessed every frame by a receiver.
			   Possible problem for multiple receivers and larger maps > 10 senders.
	02.08.18 - #include <intrin.h> for __movsd intrinsic for VS2017
	15.09.18 - move maxsenders registry read from the spoutGLDXinterop class
	06.06.19 - Increase default maximum sender names from 10 to 256 = 64K
			   RegisterSenderName - check for exceed maximum number of senders
			   SetMaxSenders - set max to the registry for other applications to read
	25.02.20 - Correct FindSenderName. Always returned true for one sender.
	21.07.20 - Change default max senders from 256 to 64
	28.08.20 - Correct in SpoutSettings
	24.09.20 - Add GetPartnerID and SetPartnerID
			 - Some testing of print format for HANDLE 32/64 bit
	25.09.20 - Remove GetPartnerID and SetPartnerID - not reliable
	29.09.20 - Add hasSharedInfo - to test for shared info memory map existence
	23.10.20 - Add CleanSenders
	28.12.20 - Protect against null name in SetActiveSender
	12.01.21 - Add CleanSenders to CreateSender
			   Write host path to the sender shared memory Description field in CreateSender
	13.01.21 - Remove CleanSenders until sender registration investigations are completed
			   Fix typo in host path write. To be tested.
	10.02.21 - GetActiveSender - erase the active sender memory map if the sender info is closed
	15.02.21 - Rebuild Win32 /MD for GitHub 2.007b release
	26.02.21 - Change SetSenderCPUmode to include CPU sharing mode and GLDX compatibility
	27.02.21 - Change SetSenderCPUmode name to SetSenderID
	09.04.21 - Add GetSender to retrieve class sender.
			   Remove SenderDebug
	22.06.21 - Restore 2.006 GetSenderNames function
	03.07.21 - Change GetSenderNames to GetSender to align with Spout class.
			   Change existing GetSender to FindSenderName.
			 - Change duplicate FindSenderName to FindSender overload
			   testing function
	31.07.21 - Add m_senders size check in UpdateSender
	15.12.21 - Remove noisy SpoutLogNotice from SetSenderID
	29.03.22 - change int len to size_t len in setActiveSenderName
	25.04.22 - Add create memory check and warning logs to setActiveSenderName
			   Remove duplicate shared memory creation in RegisterSenderName
			   GetActiveSender - rename temp name string to be more clear
	09.05.22 - Include SPOUT_ALREADY_CREATED for setActiveSenderName create shared memory success
	30.07.22 - Throughout - remove redundant code
	28.10.22 - Code documentation
	31.10.22 - Include SPOUT_ALREADY_EXISTS for setActiveSenderName create shared memory success
	13.12.22 - Add SpoutLogWarning to CleanSenders when an orphaned sender is removed
	17.12.22 - Some cleanup for code analysis
	22.12.22 - Compiler compatibility check
	06.01.23 - GetActiveSender, getActiveSenderName, FindActiveSender 
			   Change from fixed sendername argument to maxlength (default SpoutMaxSenderNameLen)
	08.01.23 - FindActiveSender - test max length passed
			   Code review - Use Microsoft Native Recommended rules

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Copyright (c) 2014-2023, Lynn Jarvis. All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, 
	are permitted provided that the following conditions are met:

		1. Redistributions of source code must retain the above copyright notice, 
		   this list of conditions and the following disclaimer.

		2. Redistributions in binary form must reproduce the above copyright notice, 
		   this list of conditions and the following disclaimer in the documentation 
		   and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"	AND ANY 
	EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
	OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE	ARE DISCLAIMED. 
	IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
	PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
	LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*/
#include "SpoutSenderNames.h"
#include <assert.h>

//
// Class: spoutSenderNames
//
// Spout sender management.
//
// Refer to source code for documentation.
//

spoutSenderNames::spoutSenderNames() {

	m_senders = new std::unordered_map<std::string, SpoutSharedMemory*>();

	// 15.09.18 - moved from interop class
	// 06.06.19 - increase default maximum number of senders from 10 to 256
	// 28.08.20 - decreased from 256 to 64
	// Read the registry key if it exists
	DWORD dwSenders = 64; // default maximum number of senders.
	ReadDwordFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "MaxSenders", &dwSenders);
	// If the registry read fails, the default will be used
	m_MaxSenders = (int)dwSenders;

}

spoutSenderNames::~spoutSenderNames() {

	for (auto itr = m_senders->begin(); itr != m_senders->end(); itr++)
	{
		delete itr->second;
	}
	delete m_senders;
	
}


//
// =========================
// Multiple Sender functions
// ========================= 
//

//---------------------------------------------------------
// Function: RegisterSenderName
// Register a new Sender by adding to the list of Sender names
bool spoutSenderNames::RegisterSenderName(const char* Sendername) {

	std::pair<std::set<std::string>::iterator, bool> ret;
	std::set<std::string> SenderNames; // set of names

	// Create the shared memory for the sender name set if it does not exist
	if (!CreateSenderSet()) {
		return false;
	}

	char *pBuf = m_senderNames.Lock();
	if (!pBuf) return false;

	// Register the sender name in the list of spout senders
	readSenderSetFromBuffer(pBuf, SenderNames, m_MaxSenders);

	// Check whether the sender registration will exceed the maximum number of senders
	// If this fails, just skip the registration
	if ((int)SenderNames.size() == m_MaxSenders) {
		SpoutLogWarning("spoutSenderNames::RegisterSenderName - Sender exceeds max senders (%d)\n", m_MaxSenders);
		m_senderNames.Unlock();
		return true;
	}

	//
	// Add the Sender name to the set of names
	//
	ret = SenderNames.insert(Sendername);

	if(!ret.second) {
		// See if there are any dangling entries that aren't valid anymore
		cleanSenderSet();
		readSenderSetFromBuffer(pBuf, SenderNames, m_MaxSenders);
		ret = SenderNames.insert(Sendername);
	}

	if(ret.second) {
		// write the new map to shared memory
		writeBufferFromSenderSet(SenderNames, pBuf, m_MaxSenders);
		// Set the current sender name as active.
		// The active sender is the one selected by the user or the last one 
		// opened by the user, so don't limit to the first sender in the list.
		// Thereafter the user can select an active Sender using SpoutPanel.
		SetActiveSender(Sendername);
	}
	m_senderNames.Unlock();

	return ret.second;
}

//---------------------------------------------------------
// Function: ReleaseSenderName
// Remove a Sender from the set of Sender names
bool spoutSenderNames::ReleaseSenderName(const char* Sendername) 
{
	std::set<std::string> SenderNames;
	std::string namestring;
	char name[SpoutMaxSenderNameLen]={};

	if (!Sendername)
		return false;

	// Create the shared memory for the sender name set if it does not exist
	if(!CreateSenderSet()) return false;

	// Get the current map into a buffer
	char *pBuf = m_senderNames.Lock();
	if (!pBuf) return false;

	const auto foundSender = m_senders->find(Sendername);
	if (foundSender != m_senders->end()) {
		// This also deletes the sender shared memory
		delete foundSender->second;
		m_senders->erase(Sendername);
	}

	// Read the buffer to a set to iterate through the names
	readSenderSetFromBuffer(pBuf, SenderNames, m_MaxSenders);

	// Discovered that the project properties had been set to CLI
	// Properties -> General -> Common Language Runtime Support
	// and this caused the set "find" function not to work.
	// It also disabled intellisense.

	// If the sender exists
	if(SenderNames.find(Sendername) != SenderNames.end() ) {
		SenderNames.erase(Sendername);
		// Write the sender names back to the buffer
		writeBufferFromSenderSet(SenderNames, pBuf, m_MaxSenders);
		// Is there a set left ?
		if(SenderNames.size() > 0) {
			// Was it the active sender ?
			if( (getActiveSenderName(&name[0]) && strcmp(&name[0], &Sendername[0]) == 0) || SenderNames.size() == 1) { 
				// It was, so choose the first in the list and make it active instead
				const std::set<std::string>::iterator iter = SenderNames.begin();
				namestring = *iter;
				strcpy_s(name, namestring.c_str());
				// Set it as the active sender
				setActiveSenderName(&name[0]);
			}
		}
		m_senderNames.Unlock();

		return true;
	}
	m_senderNames.Unlock();

	return false; // Sender name not in the set or no set in shared mempry

} // end ReleaseSenderName

//---------------------------------------------------------
// Function: FindSenderName
// Test to see if a Sender name exists in the sender set
bool spoutSenderNames::FindSenderName(const char* Sendername)
{
	if (!Sendername)
		return false;

	std::string namestring = "";
	std::set<std::string> SenderNames;
	// Get the current list to update the passed list
	if(GetSenderSet(SenderNames)) {
		// Does the name exist
		if (SenderNames.find(Sendername) != SenderNames.end()) {
			return true;
		}
	}

	return false;
}

//---------------------------------------------------------
// Function: cleanSenderSet
// Go through the full list of sender names and clean up
// any that shouldn't still be around
void spoutSenderNames::cleanSenderSet()
{
	if(!CreateSenderSet()) {
		return;
	}

	char *pBuf = m_senderNames.Lock();

	if (!pBuf) {
	    return;
	}

	std::set<std::string> SenderNames;
	readSenderSetFromBuffer(pBuf, SenderNames, m_MaxSenders);

	bool changed = false;

	for (auto itr = SenderNames.begin(); itr != SenderNames.end(); )
	{
		// It's one of ours, so thats fine
		if (m_senders->find(*itr) != m_senders->end())
		{
			itr++;
			continue;
		}
		SpoutSharedMemory mem;

		// This isn't found, we clean it up
		if (!mem.Open((*itr).c_str()))
		{
			changed = true;
			SenderNames.erase(itr++);
		}
		else
		{
			++itr;
		}
		
	}

	if (changed)
	{
		writeBufferFromSenderSet(SenderNames, pBuf, m_MaxSenders);
	}

	m_senderNames.Unlock();
	
}

//---------------------------------------------------------
// Function: GetSenderNames
// Return the set of Sender names in shared memory.
bool spoutSenderNames::GetSenderNames(std::set<std::string> *Sendernames)
{
	if (!Sendernames)
		return false;

	// Get the current list to update the passed list
	if (GetSenderSet(*Sendernames)) {
		return true;
	}

	return false;
}

//---------------------------------------------------------
// Function: GetSenderCount
// Number of senders in the list
int spoutSenderNames::GetSenderCount() {

	std::set<std::string> SenderSet;
	std::set<std::string>::iterator iter;
	std::string namestring;
	char name[SpoutMaxSenderNameLen]={};
	SharedTextureInfo info={};

	// Create the shared memory for the sender name set if it does not exist
	if(!CreateSenderSet()) {
		return 0;
	}

	// Doing multiple operations on the sender list, keep it locked
	if (!m_senderNames.Lock())
	{
		return 0;
	}

	// get the name list in shared memory into a local list
	GetSenderNames(&SenderSet);

	// Now we have a local set of names
	// 27.12.13 - noted that if a Processing sketch is stopped by closing the window
	// all is OK and either the "stop" or "dispose" overrides work, but if STOP is used, 
	// or the sketch is closed, neither the exit or dispose functions are called and
	// the sketch does not release the sender.
	// So here we run through again and check whether the sender exists and if it does not
	// release the sender from the local sender list
	if(SenderSet.size() > 0) {
		for(iter = SenderSet.begin(); iter != SenderSet.end(); iter++) {
			namestring = *iter; // the Sender name string
			strcpy_s(name, namestring.c_str());
			// we have the name already, so look for it's info
			if(!getSharedInfo(&name[0], &info)) {
				// Sender does not exist any more
				ReleaseSenderName(&name[0]); // release from the shared memory list
			}
		}
	}

	// Get the new set back
	if(GetSenderNames(&SenderSet)) {
		m_senderNames.Unlock();
		return((int)SenderSet.size());
	}

	m_senderNames.Unlock();

	return 0;
}

//---------------------------------------------------------
// Function: GetSender
// Sender item name
bool spoutSenderNames::GetSender(int index, char* sendername, int sendernameMaxSize)
{
	std::set<std::string> SenderNameSet;
	std::set<std::string>::iterator iter;
	std::string namestring;
	int i = 0;

	if (GetSenderNames(&SenderNameSet)) {
		if (SenderNameSet.size() < (unsigned int)index) {
			return false;
		}
		i = 0;
		for (iter = SenderNameSet.begin(); iter != SenderNameSet.end(); iter++) {
			namestring = *iter; // the name string
			if (i == index) {
				strcpy_s(sendername, sendernameMaxSize, namestring.c_str());
				break;
			}
			i++;
		}
		return true;
	}
	return false;

}

//---------------------------------------------------------
// Function: GetSenderNameInfo
// Get sender info given a sender index and knowing the sender count.
//
// index                        - in.
//
// sendername                   - out.
//
// sendernameMaxSize            - in.
//
// width, height, dxShareHandle - out.
//
bool spoutSenderNames::GetSenderNameInfo(int index, char* sendername, int sendernameMaxSize, unsigned int &width, unsigned int &height, HANDLE &dxShareHandle)
{
	std::set<std::string> SenderNameSet;
	std::set<std::string>::iterator iter;
	std::string namestring;
	int i = 0;
	DWORD format = 0;

	if(GetSenderNames(&SenderNameSet)) {
		if(SenderNameSet.size() < (unsigned int)index)
			return false;

		i = 0;
		for(iter = SenderNameSet.begin(); iter != SenderNameSet.end(); iter++) {
			namestring = *iter; // he 256 byte name string
			if(i == index) {
				strcpy_s(sendername, (rsize_t)sendernameMaxSize, namestring.c_str()); // return the name
				break;
			}
			i++;
		}
		
		// Does the retrieved sender exist or has it crashed?
		// Find out by getting the sender info and returning it
		if(GetSenderInfo(sendername, width, height, dxShareHandle, format))
			return true;

	}

	return false;

} // end GetSenderNameInfo

//---------------------------------------------------------
// Function: SetMaxSenders
// Set the maximum number of senders contained in the sender map
// Subsequently a new sender map will be created large enough for the number of senders
// but if a map is already open, it's size will not be changed
void spoutSenderNames::SetMaxSenders(int maxSenders)
{
	SpoutLogNotice("spoutSenderNames::SetMaxSenders - Setting max senders to %d", maxSenders);
	m_MaxSenders = maxSenders;
	// Set to the registry so that other applications will read the new maximum size
	WriteDwordToRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "MaxSenders", (DWORD)maxSenders);
}

int spoutSenderNames::GetMaxSenders()
{
	return m_MaxSenders;
}

//---------------------------------------------------------
// Function: GetSenderInfo
// Retrieves the info from the requested sender.
// Fails if the sender does not exist.
bool spoutSenderNames::GetSenderInfo(const char* sendername, unsigned int &width, unsigned int &height, HANDLE &dxShareHandle, DWORD &dwFormat)
{
	SharedTextureInfo info;

	// For external access to getSharedInfo - redundancy

	if(getSharedInfo(sendername, &info)) {
		width		  = (unsigned int)info.width;
		height		  = (unsigned int)info.height;
#ifdef _M_X64
		dxShareHandle = (HANDLE)(LongToHandle((long)info.shareHandle));
#else
		dxShareHandle = (HANDLE)info.shareHandle;
#endif
		dwFormat      = info.format;

		return true;
	}
	return false;
}

//---------------------------------------------------------
// Function: SetSenderInfo
// Set texture info to a sender shared memory map without affecting the 
// interop class globals used for GL/DX interop texture sharing.
bool spoutSenderNames::SetSenderInfo(const char* sendername, unsigned int width, unsigned int height, HANDLE dxShareHandle, DWORD dwFormat) 
{
	// TODO - use pointer from initial map creation

	SharedTextureInfo info={};

	const auto foundSender = m_senders->find(sendername);
	if (foundSender == m_senders->end())
	{
		return false;
	}

	auto senderInfoMap = foundSender->second;
	if (!senderInfoMap)
		return false;

	char *pBuf = senderInfoMap->Lock();
	if (!pBuf)
	{
		return false;
	}
		
	info.width       = (uint32_t)width;
	info.height      = (uint32_t)height;
#ifdef _M_X64
	info.shareHandle = (uint32_t)(HandleToLong(dxShareHandle));
#else
	info.shareHandle = (uint32_t)dxShareHandle;
#endif
	info.format      = (uint32_t)dwFormat;
	
	//
	// Initialize unused variables
	//

	// Texture usage
	info.usage = 0;

	// Description : Host path
	char exepath[256]={};
	GetModuleFileNameA(NULL, &exepath[0], sizeof(exepath));

	// Partner ID : Sender CPU sharing mode
	// Set by SetSenderID
	// TODO : combine here

	// Description is defined as wide chars, but the path is stored as byte chars
	memcpy(&info.description[0], &exepath[0], 256); // wchar 128

	// Set data to the memory map
	memcpy(pBuf, &info, sizeof(SharedTextureInfo)); // was __movsd; ARM64-portable

	senderInfoMap->Unlock();
	
	return true;

} // end SetSenderInfo


//---------------------------------------------------------
// Function: SetSenderID
//
// Set sender CPU sharing mode and hardware compatibility with GL/DX linkage
// to the two top bits of the 32 bit partnerID field in sender shared memory
//
//   bCPU  - means "using CPU sharing methods".
//
//     1000 0000 0000 0000 0000 0000 0000 0000 = 0x80000000.
//
//   bGLDX - means "compatible with OpenGL/DirectX interop".
//
//     0100 0000 0000 0000 0000 0000 0000 0000 = 0x40000000.
//
//   Both set - means "GL/DX compatible but using CPU sharing methods".
//
//     1100 0000 0000 0000 0000 0000 0000 0000 = 0xC0000000.
// 
// 2.006 senders may or may not have these bits set but will rarely have the exact values.
bool spoutSenderNames::SetSenderID(const char *sendername, bool bCPU, bool bGLDX)
{
	SharedTextureInfo info;

	if (getSharedInfo(sendername, &info)) {
		// Using CPU sharing methods - set top bit
		// 1000 0000 0000 0000 0000 0000 0000 0000
		// GL/DX compatible hardware - set next to top bit
		// 0100 0000 0000 0000 0000 0000 0000 0000
		info.partnerId = 0x00000000; // Default both bits clear
		if (bCPU)
			info.partnerId = 0x80000000;
		if (bGLDX)
			info.partnerId |= 0x40000000;

		// Save the info for this sender in the sender shared memory map
		setSharedInfo(sendername, &info);

		return true;
	}

	return false;
}

//
// Active Sender
//
// Functions to set or get the active Sender name.
// The "active" Sender is the one of the multiple Senders
// that is top of the list or is the one selected by the user from this list. 
// This active Sender information is saved in a separated shared
// memory from other Senders, identified by the name "ActiveSenderName"
// so it can be recalled at any time by clients if the user
// has selected a required Sender from a dialog or executable.
// The dialog or executable sets the info of the selected Sender
// into the ActiveSender shared memory so the clients can picks it up.
// !!! The active Sender has to be a member of the Sender list !!!

//---------------------------------------------------------
// Function: SetActiveSender
// Set the active sender, the first retrieved by a receiver
bool spoutSenderNames::SetActiveSender(const char *Sendername)
{
	std::set<std::string> SenderNames;

	if (!Sendername)
		return false;

	if (!CreateSenderSet())	{
		return false;
	}

	// Keep the sender set locked for this entire operation
	if (!m_senderNames.Lock())
	{
		return false;
	}

	// Get the current list to check whether the passed name is in it
	if(GetSenderSet(SenderNames)) {
		if(SenderNames.find(Sendername) != SenderNames.end() ) {
			if(setActiveSenderName(Sendername)) { // set the active Sender name to shared memory
				m_senderNames.Unlock();
				return true;
			}
		}
	}

	m_senderNames.Unlock();
	return false;

} // end SetActiveSender

//---------------------------------------------------------
// Function: GetActiveSender
// Retrieve the current active Sender name
bool spoutSenderNames::GetActiveSender(char *Sendername, const int maxlength)
{
	if (!Sendername)
		return false;

	// name will never be larger than this (default is 256)
	char ActiveName[2048]={};
	SharedTextureInfo info={};

	if(getActiveSenderName(&ActiveName[0])) {
		// Does it still exist ?
		if(getSharedInfo(&ActiveName[0], &info)) {
			strcpy_s(&Sendername[0], maxlength, &ActiveName[0]);
			return true;
		}
		else {
			// Erase the active sender memory map
			m_activeSender.Close();
		}
	}

	return false;

} // end GetActiveSender

//---------------------------------------------------------
// Function: GetActiveSenderInfo
// Get the shared info of the active Sender
bool spoutSenderNames::GetActiveSenderInfo(SharedTextureInfo* info)
{
	char sendername[SpoutMaxSenderNameLen]={};

	// See if the shared memory of the active Sender exists
	if(GetActiveSender(&sendername[0])) {
		if(getSharedInfo(&sendername[0], info)) {
			return true;
		}
	}
	// It should exist because it is set whenever a Sender registers
	return false;
} // end GetActiveSenderInfo

//---------------------------------------------------------
// Function: FindActiveSender
// Retrieve the texture info of the active sender
bool spoutSenderNames::FindActiveSender(char * sendername, unsigned int& theWidth, unsigned int& theHeight, HANDLE& hSharehandle, DWORD& dwFormat, const int maxlength)
{
	SharedTextureInfo TextureInfo={};

	// name will never be larger than 256
	if (maxlength > 256)
		return false;

	char sname [512]={};
    if(GetActiveSender(&sname[0])) { // there is an active sender
		if(getSharedInfo(&sname[0], &TextureInfo)) {
			strcpy_s(sendername, maxlength, &sname[0]); // pass back sender name
			theWidth        = (unsigned int)TextureInfo.width;
			theHeight       = (unsigned int)TextureInfo.height;
#ifdef _M_X64
			hSharehandle = (HANDLE)(LongToHandle((long)TextureInfo.shareHandle));
#else
			hSharehandle = (HANDLE)TextureInfo.shareHandle;
#endif
			dwFormat        = (DWORD)TextureInfo.format;
			return true;
		}
	}

    return false;

} // end FindActiveSender

// ===============================================================================
// Sender
// Functions to Create, Update and Close a sender and retrieve sender texture info
// without initializing DirectX or the GL/DX interop functions
// ===============================================================================

//---------------------------------------------------------
// Function: CreateSender
//	Create a sender
bool spoutSenderNames::CreateSender(const char *sendername, unsigned int width, unsigned int height, HANDLE hSharehandle, DWORD dwFormat)
{
	SpoutLogNotice("spoutSenderNames::CreateSender");
	SpoutLogNotice("    [%s] %dx%d, share handle = 0x%.7X, format = %u", sendername, width, height, LOWORD(hSharehandle), dwFormat);
	
	// Register the sender name
	// The function is ignored if the sender already exists
	RegisterSenderName(sendername);

	// Save the texture info for this sender
	if (!UpdateSender(sendername, width, height, hSharehandle, dwFormat))
		return false;

	return true;
		
} // end CreateSender

//---------------------------------------------------------
// Function: UpdateSender
//	Update the texture info of a sender.
//	Used when a sender's texture changes size.
bool spoutSenderNames::UpdateSender(const char *sendername, unsigned int width, unsigned int height, HANDLE hSharehandle, DWORD dwFormat)
{
	if (m_senders->size() == 0 || (m_senders->find(sendername) == m_senders->end())) { // New sender

		// Create or open a shared memory map for this sender - allocate enough for the texture info
		SpoutSharedMemory *senderInfoMem = new SpoutSharedMemory();
		const SpoutCreateResult result = senderInfoMem->Create(sendername, sizeof(SharedTextureInfo));
		if (result == SPOUT_CREATE_FAILED) {
			delete senderInfoMem;
			m_senderNames.Unlock();
			return false;
		}
		// The sender's information remains until it closes
		// and is saved in the m_senders set
		(*m_senders)[sendername] = senderInfoMem;
	}

	// Save the info for this sender in the sender shared memory map
	return SetSenderInfo(sendername, width, height, hSharehandle, dwFormat);
		
} // end UpdateSender

// ===============================================================================
//	Functions to retrieve information about the shared texture of a sender
//
//	Possible detection by the caller of DX9 or DX11 sender from the Format field
//	Format is always fixed as D3DFMT_A8R8G8B8 for a DirectX9 sender and Format is set to 0
//	For a DirectX11 sender, the format field is set to the DXGI_FORMAT texture format 
//	Usage is fixed :
//		DX9  - D3DUSAGE_RENDERTARGET
//		DX11 - D3D11_USAGE_DEFAULT 
// ===============================================================================

//
//
// 09.03.18 - change to logic so that the sendernames map is not
// accessed every frame by a receiver for ReceiveTexture
//

//---------------------------------------------------------
// Function: CheckSender
//	Check the details of an existing sender
//	1) Find the sender
//	2) Get it's texture info
//	3) Return the sharehandle, width, height, and format
//
//	Returns :
//
//		true - all OK.
//		  width and height are returned changed for sender size change.
//
//		false - sender not found or size changed.
//		  width and height are returned zero for sender not found.

bool spoutSenderNames::CheckSender(const char *sendername, unsigned int &theWidth, unsigned int &theHeight, HANDLE &hSharehandle, DWORD &dwFormat)
{
	SharedTextureInfo info={};

	// Is the given sender registered ?
	if(FindSenderName(sendername)) {
		// Does it still exist ?
		if(getSharedInfo(sendername, &info)) {
			// Return the texture info
			theWidth     = (unsigned int)info.width;
			theHeight    = (unsigned int)info.height;
#ifdef _M_X64
			hSharehandle = (HANDLE)(LongToHandle((long)info.shareHandle));
#else
			hSharehandle = (HANDLE)info.shareHandle;
#endif			
			dwFormat		= (DWORD)info.format;

			return true;
		}
		else {
			// Sender is registered but does not exist so close it
			ReleaseSenderName(sendername);
		}

	}

	// Return zero width and height to indicate sender not found
	theHeight = 0;
	theWidth  = 0;

	return false;

} // end CheckSender

//---------------------------------------------------------
// Function: FindSender
// Find a sender and return the name, width and height, sharhandle and format
bool spoutSenderNames::FindSender(char *sendername, unsigned int &width, unsigned int &height, HANDLE &hSharehandle, DWORD &dwFormat)
{
	if (!sendername)
		return false;

	SharedTextureInfo info={};

	// Check the user entered Sender name to see if it exists
	if (!*sendername) {
		// Passed name was empty, so find the active sender
		if (!GetActiveSender(sendername))
			return false;
	}
	// now we have either an existing sender name or the active sender name

	// 01.08.15 - Is the given sender registered ?
	// 09.03.18 - change to logic so that the sendernames map is not
	// accessed every frame by a receiver when looking for a named sender
	// getSharedInfo fails if the sender is not there and ReleaseSenderName 
	// is not necessary.  A sender will release it's name from the list when it closes
	// and will only not do so if the sender crashes.
	// Then SpoutPanel will clean the sender set and remove senders that do not exist
	// This is also done by RegisterSenderName for every sender that is registered

	// Try to get the sender information
	if (getSharedInfo(sendername, &info)) {
		width = (unsigned int)info.width; // pass back sender size
		height = (unsigned int)info.height;
#ifdef _M_X64
		hSharehandle = (HANDLE)(LongToHandle((long)info.shareHandle));
#else
		hSharehandle = (HANDLE)info.shareHandle;
#endif
		dwFormat = (DWORD)info.format;
		return true;
	}

	// Not there - could have closed or crashed
	return false;

} // end FindSender

//---------------------------------------------------------
// Function: FindSender
// Find a sender in the class names set
// Used for testing - may be removed
bool spoutSenderNames::FindSender(const char* sendername)
{
	std::string namestring = sendername;
	if (m_senders->find(namestring) != m_senders->end())
		return true;

	return false;
}

//---------------------------------------------------------
// Function: CleanSenders
// Release any orphaned senders if the name exists
// in the sender list but the shared memory info does not
void spoutSenderNames::CleanSenders()
{
	char name[512]={};
	std::set<std::string> Senders;
	std::set<std::string>::iterator iter;
	std::string namestring;
	SharedTextureInfo info={};

	// get the sender name list in shared memory into a local list
	GetSenderNames(&Senders);

	// Now we have a local set of names "Senders"
	// Run through the set and check whether the sender exists
	// If it does not exist, release from the sender list
	if (Senders.size() > 0) {
		for (iter = Senders.begin(); iter != Senders.end(); iter++) {
			namestring = *iter; // the Sender name string
			strcpy_s(name, namestring.c_str());
			// we have the name already, so look for it's info
			if (!getSharedInfo(&name[0], &info)) {
				SpoutLogWarning("spoutSenderNames::CleanSenders - removing [%s]", &name[0]);
				// Sender does not exist any more so remove from the names list
				ReleaseSenderName(&name[0]);
			}
		}
	}

	// Now we have cleaned up the list in shared memory
	Senders.clear();

}
// ================================================


//
// Protected
//

///////////////////////////////////////////////////
// Private functions for multiple Sender support //
///////////////////////////////////////////////////

void spoutSenderNames::readSenderSetFromBuffer(const char* buffer, std::set<std::string>& SenderNames, int maxSenders)
{
	const char* buf = buffer;
	if (!buf)
		return;

	// first empty the set
	if(SenderNames.size() > 0) {
		SenderNames.erase (SenderNames.begin(), SenderNames.end() );
	}

	char name[SpoutMaxSenderNameLen]={};		// char array to test for nulls
	int i = 0;
	do {
		// the actual string retrieved from shared memory should terminate
		// with a null within the 256 chars.
		// At the end of the map there will be a null in the data.
		// Must use a character array to ensure testing for null.
		strncpy_s(name, buf, SpoutMaxSenderNameLen);
		if(name[0] > 0) {
			// insert name into set
			SenderNames.insert(&name[0]);
		}
		// increment by 256 bytes for the next name
		buf += SpoutMaxSenderNameLen;
		i++;
	} while (name[0] > 0 && i < maxSenders); // maxSenders has to be passed because this function is static

}

void spoutSenderNames::writeBufferFromSenderSet(const std::set<std::string>& SenderNames, char* buffer, int maxSenders)
{
	char* buf = buffer; // pointer within the buffer
	if (!buf)
		return;

	std::string namestring;
	int i = 0;
	std::set<std::string>::iterator iter;

	for(iter = SenderNames.begin(); iter != SenderNames.end(); iter++) {
		namestring = *iter; // the string to copy to the buffer
		// copy it with 256 max length although only the string length will be copied
		strcpy_s(buf, SpoutMaxSenderNameLen, namestring.c_str());
		// move the buffer pointer on for the next Sender name
		buf += SpoutMaxSenderNameLen;
		i++;
		if(i >= maxSenders) break; // do not exceed the size of the local buffer
		// changed from > to >=
		// https://github.com/leadedge/Spout2/pull/38
		//
	}

	// If we haven't totally filled the sender list, 
	// then null terminate the next entry to terminate the list
	if (i < maxSenders) {
		*buf = '\0';
	}
}

//
//  Functions to read and write the list of Sender names to/from shared memory
//

// Create a shared memory map and copy the Sender names set to shared memory
bool spoutSenderNames::CreateSenderSet() 
{
	// Set up Shared Memory for all the sender names

	// The map will be created using m_MaxSenders unless a map already exists
	// in which case the map size will be the same as when it was created.
	// If it was created by a 2.004 app this will have a maximum of 10 senders.
	const SpoutCreateResult result = m_senderNames.Create("SpoutSenderNames", m_MaxSenders*SpoutMaxSenderNameLen);
	if(result == SPOUT_CREATE_FAILED) {
		SpoutLogError("spoutSenderNames::CreateSenderSet() : SPOUT_CREATE_FAILED");
		return false;
	}
	return true;

} // end CreateSenderSet

bool spoutSenderNames::GetSenderSet(std::set<std::string>& SenderNames) {

	char* pBuf = nullptr;

	// Open or create m_sendernames
	if (!CreateSenderSet())	{
		return false;
	}

	pBuf = m_senderNames.Lock();
	if (!pBuf) {
		return false;
	}

	// The data has been stored with 256 bytes reserved for each Sender name
	// and nothing will have changed with the map yet
	if(!*pBuf) { // no senders yet
		m_senderNames.Unlock();
		return true;
	}

	// Read back from the mapped memory buffer and rebuild the set that was passed in
	// The set will then contain the senders currently in the memory map
	// and allow for any that have been added or deleted
	readSenderSetFromBuffer(pBuf, SenderNames, m_MaxSenders);

	m_senderNames.Unlock();

	return true;

} // end GetSenderSet

// Create a shared memory map to set the active Sender name to shared memory
// This is a separate small shared memory with a fixed sharing name
// that clients can use to retrieve the current active Sender
bool spoutSenderNames::setActiveSenderName(const char* SenderName) 
{
	const size_t len = strlen(SenderName);
	if(len  == 0 || len + 1 > SpoutMaxSenderNameLen)
		return false;

	const SpoutCreateResult spoutres = m_activeSender.Create("ActiveSenderName", SpoutMaxSenderNameLen);
	if (spoutres    == SPOUT_CREATE_SUCCESS 
		|| spoutres == SPOUT_ALREADY_CREATED
		|| spoutres == SPOUT_ALREADY_EXISTS) {
		char* pBuf = m_activeSender.Lock();
		if (!pBuf) {
			SpoutLogWarning("spoutSenderNames::setActiveSenderName - could not lock buffer");
			return false;
		}
		// Fill it with the Sender name string
		memcpy((void*)pBuf, (void*)SenderName, len + 1); // write the Sender name string to the shared memory
		m_activeSender.Unlock();
		return true;
	}
	SpoutLogWarning("spoutSenderNames::setActiveSenderName - could not create memory");

	return false;

} // end setActiveSenderName

// Get the active Sender name from shared memory
bool spoutSenderNames::getActiveSenderName(char *SenderName, const int maxchars)
{
	if (!m_activeSender.Open("ActiveSenderName")) {
		return false;
	}

	const char *pBuf = m_activeSender.Lock();

	// Open the named memory map for the active sender and return a pointer to the memory
	if(!pBuf) {
		return false;
	}

	// memcpy(SenderName, (void *)pBuf, SpoutMaxSenderNameLen ); // get the name string from shared memory
	memcpy(SenderName, (void*)pBuf, maxchars); // get the name string from shared memory

	m_activeSender.Unlock();

	return true;

} // end getActiveSenderName

// Return current sharing handle, width and height of a Sender
// A receiver checks this all the time so it has to be compact
// Does not have to be the info of this instance
// so the creation pointer and handle may not be known
bool spoutSenderNames::getSharedInfo(const char* sharedMemoryName, SharedTextureInfo* info) 
{
	SpoutSharedMemory mem;
	// Open is possibly faster than Create because the function is called all the time
	if(mem.Open(sharedMemoryName)) {
		const char *pBuf = mem.Lock();
		if(pBuf) {
			memcpy(info, pBuf, sizeof(SharedTextureInfo)); // was __movsd; ARM64-portable
			mem.Unlock();
			return true;
		}
	}

	return false;

} // end getSharedInfo

// 12.06.15 - Added to allow direct modification of a sender's information in shared memory
bool spoutSenderNames::setSharedInfo(const char* sharedMemoryName, const SharedTextureInfo* info) 
{
	SpoutSharedMemory mem;

	if (!mem.Open(sharedMemoryName)) {
		return false;
	}

	char *pBuf = mem.Lock();

	if (!pBuf)	{
		return false;
	}

	memcpy(pBuf, info, sizeof(SharedTextureInfo)); // was __movsd; ARM64-portable

	mem.Unlock();
	
	return true;

} // end setSharedInfo


// Test for shared info memory map existence
bool spoutSenderNames::hasSharedInfo(const char* sharedMemoryName)
{
	SpoutSharedMemory mem;
	if (mem.Open(sharedMemoryName)) {
		if (mem.Lock()) {
			mem.Unlock();
			return true;
		}
	}
	return false;

} // end hasSharedInfo
