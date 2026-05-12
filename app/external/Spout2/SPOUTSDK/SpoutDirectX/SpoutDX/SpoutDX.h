/*

			SpoutDX.h

			Sender and receiver for DirectX applications

	Copyright (c) 2014-2023 Lynn Jarvis. All rights reserved.

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
 
*/
#pragma once

#ifndef __spoutDX__
#define __spoutDX__

// Change the path as required
#include "SpoutGL\SpoutCommon.h" // for dll build
#include "SpoutGL\SpoutSenderNames.h" // for sender creation and update
#include "SpoutGL\SpoutDirectX.h" // for creating DX11 textures
#include "SpoutGL\SpoutFrameCount.h" // for mutex lock and new frame signal
#include "SpoutGL\SpoutCopy.h" // for pixel copy
#include "SpoutGL\SpoutUtils.h" // Registry utiities

#include <direct.h> // for _getcwd
#include <TlHelp32.h> // for PROCESSENTRY32
#include <tchar.h> // for _tcsicmp

class SPOUT_DLLEXP spoutDX {

	public:

	spoutDX();
    ~spoutDX();

	//
	// DIRECTX
	//

	bool OpenDirectX11(ID3D11Device* pDevice = nullptr);
	ID3D11Device* GetDX11Device();
	ID3D11DeviceContext* GetDX11Context();
	void CloseDirectX11();
	bool IsClassDevice();
	void SetKeyed(bool bKeyed = true);
	bool GetKeyed();

	//
	// SENDER
	//

	// Set the sender name
	bool SetSenderName(const char* sendername = nullptr);
	// Set the sender texture format
	void SetSenderFormat(DXGI_FORMAT format);
	// Close sender and free resources
	void ReleaseSender();
	// Send the back buffer
	bool SendBackBuffer();
	// Send a texture
	bool SendTexture(ID3D11Texture2D* pTexture);
	// Send part of a texture
	bool SendTexture(ID3D11Texture2D* pTexture,
		unsigned int xoffset, unsigned int yoffset,
		unsigned int width, unsigned int height); 
	// Send an image
	bool SendImage(const unsigned char * pData, unsigned int width, unsigned int height);
	// Sender status
	bool IsInitialized();
	// Sender name
	const char * GetName();
	// Get width
	unsigned int GetWidth();
	// Get height
	unsigned int GetHeight();
	// Get frame rate
	double GetFps();
	// Get frame number
	long GetFrame();

	//
	// RECEIVER
	//

	// Set the sender to connect to
	void SetReceiverName(const char * sendername = nullptr);
	// Close receiver and free resources
	void ReleaseReceiver();
	// Receive from a sender
	bool ReceiveTexture();
	// Receive a texture from a sender
	bool ReceiveTexture(ID3D11Texture2D** ppTexture);
	// Receive an image
	bool ReceiveImage(unsigned char * pixels, unsigned int width, unsigned int height, bool bRGB = false, bool bInvert = false);
	// Open sender selection dialog
	void SelectSender();
	// Sender has changed
	bool IsUpdated();
	// Connected to a sender
	bool IsConnected();
	// Received frame is new
	bool IsFrameNew();
	// Received texture
	ID3D11Texture2D* GetSenderTexture();
	// Received sender share handle
	HANDLE GetSenderHandle();
	// Received sender texture format
	DXGI_FORMAT GetSenderFormat();
	// Received sender name
	const char * GetSenderName();
	// Received sender width
	unsigned int GetSenderWidth();
	// Received sender height
	unsigned int GetSenderHeight();
	// Received sender frame rate
	double GetSenderFps();
	// Received sender frame number
	long GetSenderFrame();
	
	//
	// COMMON
	//

	// Frame rate control
	void HoldFps(int fps);
	// Disable frame counting for this application
	void DisableFrameCount();
	// Return frame count status
	bool IsFrameCountEnabled();
	// Signal sync event 
	void SetFrameSync(const char* SenderName);
	// Wait or test for a sync event
	bool WaitFrameSync(const char *SenderName, DWORD dwTimeout = 0);

								
	//
	// Sender names
	//

	// Get number of senders
	int  GetSenderCount();
	// Get sender name for a given index
	bool GetSender(int index, char* sendername, int MaxSize = 256);
	// Get sender details
	bool GetSenderInfo(const char* sendername, unsigned int &width, unsigned int &height, HANDLE &dxShareHandle, DWORD &dwFormat);
	// Get active sender name
	bool GetActiveSender(char* sendername);
	// set active sender name
	bool SetActiveSender(const char* sendername);
	// Get maximum senders allowed
	int  GetMaxSenders();
	// Set maximum senders allowed
	void SetMaxSenders(int maxSenders);

	//
	// Adapter functions
	//

	// Get the number of graphics adapters in the system
	int GetNumAdapters();
	// Get the adapter name for a given index
	bool GetAdapterName(int index, char *adaptername, int maxchars);
	// Get the current adapter description
	bool GetAdapterInfo(char *renderdescription, char *displaydescription, int maxchars);
	// Get the current adapter index
	int  GetAdapter();
	// Set required graphics adapter for output (no args or -1 to reset)
	bool SetAdapter(int index = -1);
	// Get adapter pointer for a given adapter (-1 means current)
	IDXGIAdapter* GetAdapterPointer(int index = -1);
	// Set required graphics adapter for creating a device
	void SetAdapterPointer(IDXGIAdapter* pAdapter);
	// Get auto device switching status
	bool GetAdapterAuto();
	// Auto switch receiving device to use the same graphics adapter as the sender
	void SetAdapterAuto(bool bAuto = true);
	// Get sender adapter index and name for a given sender
	int GetSenderAdapter(const char* sendername, char* adaptername = nullptr, int maxchars = 256);

	//
	// Graphics preference
	//
// Windows 10 Vers 1803, build 17134 or later
#ifdef NTDDI_WIN10_RS4

	// Get the Windows graphics preference for an application
	//     -1 - No preference
	//      0 - Default
	//      1 - Power saving
	//      2 - High performance
	// If no path is specified, use the current application path
	int GetPerformancePreference(const char* path = nullptr);
	// Set the Windows graphics preference for an application
	//     -1 - No preference
	//      0 - Default
	//      1 - Power saving
	//      2 - High performance
	// If no path is specified, use the current application path
	bool SetPerformancePreference(int preference, const char* path = nullptr);
	// Get the graphics adapter name for a Windows preference
	bool GetPreferredAdapterName(int preference, char* adaptername, int maxchars);
	// Set graphics adapter index for a Windows preference
	bool SetPreferredAdapter(int preference);
	// Windows graphics preference availability
	bool IsPreferenceAvailable();
	// Is the path a valid application
	bool IsApplicationPath(const char* path);
#endif

	//
	// Sharing modes (2.006 compatibility)
	//

	// Get user selected DX9 mode (2.006)
	bool GetDX9();
	bool GetMemoryShareMode();

	//
	// Utility
	//

	void CheckSenderFormat(const char * sendername);
	bool CreateDX11texture(ID3D11Device* pd3dDevice,
		unsigned int width, unsigned int height,
		DXGI_FORMAT format, ID3D11Texture2D** ppTexture);

	//
	// SpoutUtils namespace functions for dll access
	void OpenSpoutConsole();
	void CloseSpoutConsole(bool bWarning = false);
	void EnableSpoutLog();
	void EnableSpoutLogFile(const char* filename, bool append = false);
	void DisableSpoutLogFile();
	void DisableSpoutLog();

	//
	// Data sharing
	//

	// Write data to shared memory
	bool WriteMemoryBuffer(const char *name, const char* data, int length);
	// Read data from shared memory
	int  ReadMemoryBuffer(const char* name, char* data, int maxlength);
	// Create a shared memory buffer
	bool CreateMemoryBuffer(const char *name, int length);
	// Delete a shared memory buffer
	bool DeleteMemoryBuffer();
	// Get the number of bytes available for data transfer
	int  GetMemoryBufferSize(const char *name);

	//
	// Public for external access
	//

	spoutSenderNames sendernames;
	spoutFrameCount frame;
	spoutDirectX spoutdx;
	spoutCopy spoutcopy;

	//
	// Options used for SpoutCam
	//

	// Mirror image
	bool m_bMirror;
	// RGB <> BGR
	bool m_bSwapRB;

protected :

	ID3D11Device* m_pd3dDevice;
	ID3D11DeviceContext* m_pImmediateContext;
	ID3D11Texture2D* m_pSharedTexture;
	ID3D11Texture2D* m_pTexture;
	ID3D11Texture2D* m_pStaging[2];
	int m_Index;
	int m_NextIndex;

	HANDLE m_dxShareHandle;
	DWORD m_dwFormat;
	SharedTextureInfo m_SenderInfo;
	char m_SenderNameSetup[256];
	char m_SenderName[256];
	unsigned int m_Width;
	unsigned int m_Height;
	bool m_bUpdated;
	bool m_bConnected;
	bool m_bSpoutInitialized;
	bool m_bSpoutPanelOpened;
	bool m_bSpoutPanelActive;
	bool m_bClassDevice;
	bool m_bAdapt;
	bool m_bMemoryShare; // Using 2.006 memoryshare methods
	bool m_bKeyed; // Keyed shared texture
	SHELLEXECUTEINFOA m_ShExecInfo; // For ShellExecute

	// For WriteMemoryBuffer/ReadMemoryBuffer
	SpoutSharedMemory memorybuffer;

	bool CheckSender(unsigned int width, unsigned int height, DWORD dwFormat);
	ID3D11Texture2D* CheckSenderTexture(char *sendername, HANDLE dxShareHandle);

	bool ReceiveSenderData();
	void CreateReceiver(const char * sendername, unsigned int width, unsigned int height, DWORD dwFormat);
	
	// Read pixels from a staging texture
	bool ReadPixelData(ID3D11Texture2D* pStagingSource, unsigned char* destpixels,
		unsigned int width, unsigned int height, bool bRGB, bool bInvert, bool bSwap);
	
	// Create or update staging textures
	bool CheckStagingTextures(unsigned int width, unsigned int height, DWORD dwFormat = DXGI_FORMAT_B8G8R8A8_UNORM);

	// Create or update class texture
	bool CheckTexture(unsigned int width, unsigned int height, DWORD dwFormat);

	void SelectSenderPanel();
	bool CheckSpoutPanel(char *sendername, int maxchars = 256);

};

#endif
