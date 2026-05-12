//
//			SpoutDX
//
//		Sender and receiver for DirectX applications
//			Send a DirectX11 shared texture
//			Receive from a Spout sender DirectX11 shared texture
//			DirectX9 not supported
//
// ====================================================================================
//		Revisions :
//
//		07.06.19	- started DirectX helper class
//		17.10.19	- For revison of SpoutCam from OpenGL to DirectX
//					  Added DirectX initialise and release functions
//					  Added support for pixel read via staging texture
//					  Added pixel copy functions
//					  Added memoryshare support
//		30.01.20	- Simplify names for Get received texture methods
//					  Revise CreateDX11Texture for user flags
//					  Add SetTextureFlags
//		07.01.20	- Correct immediate context in SendTexture
//		13.04.20	- Revise receiver methods
//		10.05.20	- Corrected format for ReceivedTexture and GetSenderFormat
//					  Cleanup Tutorial07 example code
//		05.06.20	- Get memoryshare mode from registry in constructor
//		06.06.20	- Working memoryshare
//		07.06.20	- Use application device exclusively
//					  Strip back to basic texture share only for simplicity
//					  Memoryshare use unlikely for DirectX applications
//					  Can be restored on request
//		09.06.20	- Updated 2.007 Spout SDK files
//					  Restored ReceiveRGBIMage and staging texture functions
//					  for DirectX version of SpoutCam
//		21.06.20	- Create basic windows example using SpoutDX class
//		22.06.20	- Add ReceiveImage and ReadRGBApixels - see also SpoutCopy
//		23.06.20	- SetSenderName revision and testing
//					  Clean up
//		25.06.20	- Include texture format in update checks
//		26.06.20	- Revise ReceiveTexture
//					  Remove local receiving texture
//					  Remove CreateDX11Texture
//				      More SetSenderName revision and testing
//					  General update throughout
//		26.06.20	- Restore revised CreateDX11texture
//		28.06.17	- Remove ReadRGBApixels
//					  Change to allow class or application device
//		30.06.20	- Due to hesitions with SpoutCam, move flush for staging texture map
//					  from ReceiveRGBimage to ReadRGBpixels and added to ReceiveImage.
//		03.07.20	- Change OpenDirectX11 so it can be repeatedly called (see ReceiveImage)
//		04.07.20	- OpenDirectX11 check in all sending functions
//		09.07.20	- Correct ReadRGBpixels for RGBA senders
//		02.08.20	- Remove console print from ReadRGBApixels
//		02.09.20	- Revise ReceiveRGBimage, ReadRGBpixels for SpoutCam 
//					  Only use OpenDX11shareHandle if the sender texture changes size
//					  to avoid a memory leak (https://github.com/leadedge/SpoutCam/issues/2)
//					  Release m_pSharedTexture in all close functions
//		08.09.20	- Revise ReceiveTexture and ReceiveImage to avoid repeated OpenDX11shareHandle
//					  Revise ReceiveImage due to problems if the staging texture remains mapped
//					  SendImage do not release immediate context
//					  Made all class objects public
//		18.09.20	- Add GetSenderAdapter and SetSenderAdapter
//		21.09.20	- Add sender IsInitialized and GetName
//					  ReceiveTexture, ReceiveImage and ReceiveRGBimage -
//					  retrieve a new shared texture pointer in ReceiveSenderData() rather than afterwards
//					  SendTexture - set the current adapter index after creating sender
//					  Prevent sharing if the sender texture was created on a different adapter
//					  Some protections in GetSenderAdapter
//		23.09.20	- GetSenderAdapter : return -1 on failure. Remove logs due to repeats.
//					  ReceiveSenderData() : catch exception for OpenDX11shareHandle failure
//					  Initialize m_SenderInfo and m_ShExecInfo in constructor
//		24.09.20	- Move try/catch to OpenDX11shareHandle in SpoutDirectX class
//		25.09.20	- Clean up ReceiveSenderData to allow for failure of OpenDX11shareHandle
//					  Compare share handle to detect a new sender instead of the name
//		02.10.20	- Async readback from GPU using two staging textures
//		04.10.20	- Correct CheckStagingTextures for size change
//					- Correct ReceiveSenderData to release texture before OpenSharedResource
//					- Replace ReceiveRGBimage with ReceiveImage and rgb flag
//					- Replace ReadRGBAimage, ReadRGBimage, ReadRGBApixels, ReadRGBpixels
//					  with ReadPixelData and rgb flag
//		06.10.20	- Allow for DX9 shared textures by creating receiving texture with compatible format
//					- Mirror and swap red/blue options for SpoutCam via class flags m_bMirror, and m_bSwapRB
//					  Modifications to SpoutCopy rgba2rgb and rgba2rgbResample
//		29.10.20	- Add CheckSender for SendTexture and SendImage
//		08.12.20	- Add GetDX11Context
//		09.12.20	- Rename CleanupDX11 to CloseDirectX11
//					  Add auto adapter switch into ReceiveSenderData if a class device was created.
//		10.01.21	- Add auto increment of sender name to SetSenderName if the sender already exists
//		11.01.21	- Add IsClassDevice()
//		12.01.21	- Release orphaned senders in SelectSenderPanel
//		15.01.21	- Add Flush to ReceiveTexture
//		09.02.21	- Commented out development code for graphics adapter switching
//		20.02.21	- Add SetAdapterAuto to enable graphics adapter switching
//					  Add default args to to GetSenderAdapter
//		21.02.21	- Add GetAdapterAuto
//		22.02.21	- Add missing OpenDirectX11 test in SendTexture
//		02.03.21	- SetAdapterAuto - warn if not using class device
//		08.03.21	- SetNewFrame before texture copy
//		16.03.21	- Add memoryshare struct, ReadMemoryBuffer and WriteMemoryBuffer
//					  memoryshare.CloseSenderMemory() in destructor and ReleaseSender
//		15.04.21	- Add SetFrameSync and WaitFrameSync
//		29.04.21	- Change IsFrameNew() to return frame class global.
//		10.06.21	- Remove Memoryshare struct and replace with SpoutSharedMemory object
//					  Update data functions
//		01.07.21	- ReadPixelData - add swap r/b argument
//		04.07.21	- GetSenderCount, GetSender pass through to sendernames class.
//					- Destructor : do not release receiver connected sender name.
//		20.07.21	- Correct GetSender to include max size
//					  Add SendBackBuffer
//		22.07.21	- Add OpenDirectX check to SendBackBuffer
//		12.10.21	- Add SendTexture for part of a DirectX11 texture
//		14.10.21	- ReleaseReceiver - release staging textures
//					  ReleaseSender - no staging textures used
//					  Add ReceiveTexture() function to receive to a class texture
//					  Add GetSenderTexture function to return class texture pointer
//					  Add class texture and management
//		01.11.22	- Add GetPerformancePreference, SetPerformancePreference,
//					  GetPreferredAdapterName, SetPreferredAdapter
//		08.11.22	- Add IsPreferenceAvailable, IsApplicationPath
//		16.11.22	- HoldFps double fps argument instead of int
//		26.12.22	- Initialize all arrays and structures
//					  Const arg for SendImage
//		08.01.23	- Add SpoutUtils functions
//		23.01.23	- CheckSender - Flush after shared texture release
//		17.03.23	- ReceiveSenderData - if there is a valid D3D11 format, use it.
//		18.03.23	- CreateDX11StagingTexture - use default DX11 format for zero or DX9 formats
//		19.03.23	- Remove redundant CreateDX11StagingTexture and use SpoutDirectX function
//					  ReceiveSenderData - create a DX11 receiving texture with compatible format
//					  for unknown or DX9 formats.
//		21.03.23	- ReceiveSenderData - revert to using the format of the D3D11 texture
//					  generated by OpenDX11shareHandle for incorrect sender information.
//
// ====================================================================================
/*
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

*/
#include "spoutDX.h"

//
// Class: spoutDX
//
// Functions to manage DirectX11 texture sharing.
//
// Refer to source code for further details.
//

spoutDX::spoutDX()
{
	// Initialize variables
	m_pd3dDevice = nullptr;
	m_pImmediateContext = nullptr;

	m_pTexture = nullptr;
	m_pStaging[0] = nullptr;
	m_pStaging[1] = nullptr;
	m_Index = 0;
	m_NextIndex = 0;

	m_pSharedTexture = nullptr;
	m_dxShareHandle = nullptr;
	m_SenderNameSetup[0] = 0;
	m_SenderName[0] = 0;
	m_dwFormat = DXGI_FORMAT_B8G8R8A8_UNORM; // default;
	m_Width = 0;
	m_Height = 0;
	m_bUpdated = false;
	m_bConnected = false;
	m_bSpoutInitialized = false;
	m_bSpoutPanelOpened = false;
	m_bSpoutPanelActive = false;
	m_bClassDevice = false;
	m_bMirror = false;
	m_bSwapRB = false;
	m_bAdapt = false; // Receiver switch to the sender's graphics adapter
	m_bMemoryShare = GetMemoryShareMode(); // 2.006 memoryshare mode
    m_bKeyed = false;

	ZeroMemory(&m_SenderInfo, sizeof(SharedTextureInfo));
	ZeroMemory(&m_ShExecInfo, sizeof(m_ShExecInfo));

}

spoutDX::~spoutDX()
{
	if (m_bConnected) {
		// Receiver object
		// Do not release the connected sender name
		ReleaseReceiver();
	}
	else {
		// Sender object (or receiver not connected. i.e. not initialized)
		// Release the sender name if initialized
		ReleaseSender();
	}

	CloseDirectX11();
	memorybuffer.Close();

}

//---------------------------------------------------------
// DIRECTX
//

//
// Group: DirectX
//

// Function: OpenDirectX11
// Initialize and prepare Directx 11
// Retain a class device and context
bool spoutDX::OpenDirectX11(ID3D11Device* pDevice)
{
	if (!m_pd3dDevice) {
		SpoutLogNotice("spoutDX::OpenDirectX11");
		if (pDevice) {
			// Use the device pointer if it was was passed in
			m_pd3dDevice = pDevice;
			// Get the immediate context
			pDevice->GetImmediateContext(&m_pImmediateContext);
			m_bClassDevice = false; // An existing device pointer was used
			SpoutLogNotice("    using device (0x%.7X) : context (0x%.7X)", PtrToUint(m_pd3dDevice), PtrToUint(m_pImmediateContext));
		}
		else {
			// Create a DirectX 11 device if not already
			if (spoutdx.OpenDirectX11()) {
				// Retrieve the device and context pointer
				m_pd3dDevice = spoutdx.GetDX11Device();
				m_pImmediateContext = spoutdx.GetDX11Context();
				m_bClassDevice = true; // A new class device pointer was created
				SpoutLogNotice("    created device (0x%.7X)", PtrToUint(m_pd3dDevice));
			}
		}
	}

	return true;

}

// Function: OpenDirectX11
// Return the class DirectX11 device
ID3D11Device* spoutDX::GetDX11Device()
{
	return m_pd3dDevice;
}

// Function: GetDX11Context
// Return the class DirectX11 immediate context
ID3D11DeviceContext* spoutDX::GetDX11Context()
{
	return m_pImmediateContext;
}

// Function: CloseDirectX11
// Close DirectX11 and free resources
void spoutDX::CloseDirectX11()
{
	SpoutLogNotice("spoutDX::CloseDirectX11()");

	if (m_pTexture)	spoutdx.ReleaseDX11Texture(m_pTexture);
	m_pTexture = nullptr;
	m_dxShareHandle = nullptr;

	if (m_pStaging[0]) m_pStaging[0]->Release();
	if (m_pStaging[1]) m_pStaging[1]->Release();
	m_pStaging[0] = nullptr;
	m_pStaging[1] = nullptr;
	m_Index = 0;
	m_NextIndex = 0;
	
	if (m_pd3dDevice) {
		if (m_bClassDevice) {
			// A device was created using the SpoutDirectX class
			spoutdx.CloseDirectX11();
		}
		else {
			// An application device was used
			// Release the independently created m_pImmediateContext object
			if (m_pImmediateContext)
				m_pImmediateContext->Release();
		}
	}

	m_pd3dDevice = nullptr;
	m_pImmediateContext = nullptr;

}

// Function: CloseDirectX11
// Was a device was created using the SpoutDirectX class
bool spoutDX::IsClassDevice()
{
	return m_bClassDevice;
}

// Function: SetKeyed
// Sender : create a shared texture with a keyed mutex
// Used for testing
void spoutDX::SetKeyed(bool bKeyed)
{
	m_bKeyed = bKeyed;
}

// Function: GetKeyed
// Get flag to create a shared texture with a keyed mutex
bool spoutDX::GetKeyed()
{
	return m_bKeyed;
}

//---------------------------------------------------------
// SENDER
//

//
// Group: Sender
//
// SendTexture and SendImage create or update a sender as required.
//
// - If a sender has not been created yet :
//
//    - Make sure DirectX has been initialized
//    - Create a sender using the DX11 shared texture handle
//
// - If the sender exists, test for size change :
//
//    - Update the class shared texture
//    - Update the sender and class variables	
//


//---------------------------------------------------------
// Function: SetSenderName
// Set name for sender creation
//
//   If no name is specified, the executable name is used. 
bool spoutDX::SetSenderName(const char* sendername)
{
	if (!sendername) {
		// Get executable name as default
		GetModuleFileNameA(NULL, m_SenderName, 256);
		PathStripPathA(m_SenderName);
		PathRemoveExtensionA(m_SenderName);
	}
	else {
		strcpy_s(m_SenderName, 256, sendername);
	}

	// If a sender with this name is already registered, create an incremented name
	int i = 1;
	char name[256]={};
	strcpy_s(name, 256, m_SenderName);
	if (sendernames.FindSenderName(name)) {
		do {
			sprintf_s(name, 256, "%s_%d", m_SenderName, i);
			i++;
		} while (sendernames.FindSenderName(name));
	}
	// Re-set the global sender name
	strcpy_s(m_SenderName, 256, name);

	return true;
}

//---------------------------------------------------------
// Function: SetSenderFormat
// Set the sender DX11 shared texture format
void spoutDX::SetSenderFormat(DXGI_FORMAT format)
{
	m_dwFormat = format;
}

//---------------------------------------------------------
// Function: ReleaseSender
// Close sender and release resources.
//
// A new sender is created or updated by all sending functions
void spoutDX::ReleaseSender()
{
	if (m_pSharedTexture)
		m_pSharedTexture->Release();
	m_pSharedTexture = nullptr;
	m_dxShareHandle = nullptr;

	if (m_bSpoutInitialized)
		sendernames.ReleaseSenderName(m_SenderName);

	m_Width = 0;
	m_Height = 0;
	m_SenderName[0] = 0;
	m_bSpoutInitialized = false;

	// Close shared memory buffer if used
	memorybuffer.Close();

}

//---------------------------------------------------------
// Function: SendBackbuffer
// Get the swap chain's back buffer to a texture for sending.
// Retreives a single render target.
// Refer to SendTexture for compatible formats.
//
bool spoutDX::SendBackBuffer()
{
	// Make sure DirectX is initialized
	if (!OpenDirectX11())
		return false;

	// Retrieve one render target
	ID3D11RenderTargetView *rendertarget = nullptr;
	m_pImmediateContext->OMGetRenderTargets(1, &rendertarget, nullptr);
	if (rendertarget) {
		ID3D11Resource* pBackBufferResource = nullptr;
		rendertarget->GetResource(&pBackBufferResource);
		if (pBackBufferResource) {
			// SendTexture handles sender creation and re-sizing.
			SendTexture(reinterpret_cast<ID3D11Texture2D*>(pBackBufferResource));
			return true;
		}
	}

	return false;
}

//---------------------------------------------------------
// Function: SendTexture
// Send DirectX11 texture
//
// - Default format :
//
//     - DXGI_FORMAT_B8G8R8A8_UNORM  (87)
//
// - DX9 compatible formats :
//
//     - DXGI_FORMAT_B8G8R8A8_UNORM  (87)
//     - DXGI_FORMAT_B8G8R8X8_UNORM  (88)
//
// - Other formats that work with DX11 but not with DX9 :
//
//     - DXGI_FORMAT_R8G8B8A8_UNORM  (28)
//     - DXGI_FORMAT_R8G8B8A8_UNORM_SRGB (29}
//     - DXGI_FORMAT_R16G16B16A16_FLOAT (10)
//     - DXGI_FORMAT_R16G16B16A16_SNORM (13)
//     - DXGI_FORMAT_R10G10B10A2_UNORM (24)
//
bool spoutDX::SendTexture(ID3D11Texture2D* pTexture)
{

	// Quit if no data
	if (!pTexture)
		return false;

	// Make sure DirectX is initialized
	if (!OpenDirectX11())
		return false;

	// Get the texture details
	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	pTexture->GetDesc(&desc);
	if (desc.Width == 0 || desc.Height == 0)
		return false;
	
	// Create or update the sender
	if (!CheckSender(desc.Width, desc.Height, (DWORD)desc.Format))
		return false;


	// Check the sender mutex for access the shared texture
	if (frame.CheckTextureAccess(m_pSharedTexture)) {
		// Copy the texture to the sender's shared texture
		m_pImmediateContext->CopyResource(m_pSharedTexture, pTexture);
		// Flush the command queue now because the shared texture has been updated on this device
		m_pImmediateContext->Flush();
		// Signal a new frame while the mutex is locked
		frame.SetNewFrame();
		// Allow access to the shared texture
		frame.AllowTextureAccess(m_pSharedTexture);
	}

	return true;
}


//---------------------------------------------------------
// Function: SendTexture
// Send part of a DirectX11 texture
//
// The region to be copied must be smaller than the texture
// The sender must be initialized at the width and height of the region
//
bool spoutDX::SendTexture(ID3D11Texture2D* pTexture,
	unsigned int xoffset, unsigned int yoffset,
	unsigned int width, unsigned int height)
{

	// Quit if no data
	if (!pTexture)
		return false;

	// Make sure DirectX is initialized
	if (!OpenDirectX11())
		return false;

	// Check for empty texture and get format
	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	pTexture->GetDesc(&desc);
	if (desc.Width == 0 || desc.Height == 0)
		return false;

	// Create or update the sender
	if (!CheckSender(width, height, (DWORD)desc.Format))
		return false;

	// Get the region to copy
	D3D11_BOX sourceRegion={};
	sourceRegion.left = xoffset;
	sourceRegion.right = xoffset+width;
	sourceRegion.top = yoffset;
	sourceRegion.bottom = yoffset+height;
	sourceRegion.front = 0;
	sourceRegion.back = 1;

	// Check the sender mutex for access the shared texture
	if (frame.CheckTextureAccess(m_pSharedTexture)) {
		// Copy the texture region to the sender's shared texture
		m_pImmediateContext->CopySubresourceRegion(m_pSharedTexture, 0, 0, 0, 0, pTexture, 0, &sourceRegion);
		// Flush the command queue now because the shared texture has been updated on this device
		m_pImmediateContext->Flush();
		// Signal a new frame while the mutex is locked
		frame.SetNewFrame();
		// Allow access to the shared texture
		frame.AllowTextureAccess(m_pSharedTexture);
	}

	return true;
}


//---------------------------------------------------------
// Function: SendImage
// Send pixel image
bool spoutDX::SendImage(const unsigned char * pData, unsigned int width, unsigned int height)
{
	// Quit if no data
	if (!pData)
		return false;

	// Create or update the sender
	if (!CheckSender(width, height, m_dwFormat))
		return false;

	// Check the sender mutex for access the shared texture
	if (frame.CheckTextureAccess(m_pSharedTexture)) {
		// Update the shared texture resource with the pixel buffer
		m_pImmediateContext->UpdateSubresource(m_pSharedTexture, 0, NULL, pData, m_Width * 4, 0);
		// Flush the command queue because the shared texture has been updated on this device
		m_pImmediateContext->Flush();
		// Signal a new frame while the mutex is locked
		frame.SetNewFrame();
		// Allow access to the shared texture
		frame.AllowTextureAccess(m_pSharedTexture);
	}

	return true;
}

//---------------------------------------------------------
// Function: IsInitialized
// Initialization status
bool spoutDX::IsInitialized()
{
	return m_bSpoutInitialized;
}

//---------------------------------------------------------
// Function: GetName
// Sender name
const char * spoutDX::GetName()
{
	return m_SenderName;
}

//---------------------------------------------------------
// Function: GetWidth
// Sender width
unsigned int spoutDX::GetWidth()
{
	return m_Width;
}

//---------------------------------------------------------
// Function: GetHeight
// Sender height
unsigned int spoutDX::GetHeight()
{
	return m_Height;
}

//---------------------------------------------------------
// Function: GetFps
// Sender frame rate
//    Round result to reduce variability
double spoutDX::GetFps()
{
	return (frame.GetSenderFps());
}

//---------------------------------------------------------
// Function: GetFrame
// Sender frame number
long spoutDX::GetFrame()
{
	return (frame.GetSenderFrame());
}


//---------------------------------------------------------
// RECEIVER
//

//
// Group: Receiver
//
// ReceiveTexture and ReceiveImage
//
//	- Connect to a sender
//	- Set class variables for sender name, width and height
//  - If the sender has changed size, set a flag for the application to update
//	  the receiving texture or imageif IsUpdated() returns true.
//  - Copy the sender shared texture to the user texture or image.
//

//---------------------------------------------------------
// Function: SetReceiverName
// Specify sender for connection
//
//   The application will not connect to any other unless the user selects one
//   If that sender closes, the application will wait for the nominated sender to open 
//   If no name is specified, the receiver will connect to the active sender
void spoutDX::SetReceiverName(const char * SenderName)
{
	if (SenderName && SenderName[0]) {
		strcpy_s(m_SenderNameSetup, 256, SenderName);
		strcpy_s(m_SenderName, 256, SenderName);
	}
}

//---------------------------------------------------------
// Function: ReleaseReceiver
// Close receiver and release resources ready to connect to another sender
void spoutDX::ReleaseReceiver()
{
	if (!m_bSpoutInitialized)
		return;

	SpoutLogNotice("ReleaseReceiver(%s)", m_SenderName);

	// Restore the sender name if one was specified by SetReceiverName
	if (m_SenderNameSetup[0])
		strcpy_s(m_SenderName, 256, m_SenderNameSetup);
	else
		m_SenderName[0] = 0;

	// Wait 4 frames in case the same sender opens again
	Sleep(67);

	// Sender shared texture pointer
	if (m_pSharedTexture)
		m_pSharedTexture->Release();
	m_pSharedTexture = nullptr;
	m_dxShareHandle = nullptr;

	// Class receiving texture
	if (m_pTexture)
		m_pTexture->Release();
	m_pTexture = nullptr;
	
	// Staging textures for ReceiveImage
	if (m_pStaging[0]) m_pStaging[0]->Release();
	if (m_pStaging[1]) m_pStaging[1]->Release();
	m_pStaging[0] = nullptr;
	m_pStaging[1] = nullptr;
	m_Index = 0;
	m_NextIndex = 0;

	// Close the named access mutex and frame counting semaphore.
	frame.CloseAccessMutex();
	frame.CleanupFrameCount();

	// Close shared memory buffer if used
	memorybuffer.Close();

	// Zero width and height so that they are reset when a sender is found
	m_Width = 0;
	m_Height = 0;
	
	// Initialize again when a sender is found
	m_bSpoutInitialized = false;
	m_bUpdated = false;

}

//---------------------------------------------------------
// Function: ReceiveTexture
//  Copy the sender DX11 shared texture to a class texture
//
bool spoutDX::ReceiveTexture()
{
	// Return if flagged for update
	// The update flag is reset when the receiving application calls IsUpdated()
	if (m_bUpdated)
		return true;

	// Try to receive texture details from a sender
	if (ReceiveSenderData()) {

		// Was the shared texture pointer retrieved ?
		if (!m_pSharedTexture) {
			return false;
		}

		// The sender name, width, height, format, shared texture handle and pointer have been retrieved.
		if (m_bUpdated) {
			m_bUpdated = false; // Reset for ReceiveSenderData
			// Update the receiving class texture.
			if (!CheckTexture(m_Width, m_Height, m_dwFormat))
				return false;
		}

		// The receiving texture is created on the first update above
		// ready for copy from the sender's shared texture.

		//
		// Found a sender
		//
		if (frame.CheckTextureAccess(m_pSharedTexture)) {
			// Check if the sender has produced a new frame.
			if (frame.GetNewFrame()) {
				// Copy from the sender's shared texture to the receiving class texture.
				m_pImmediateContext->CopyResource(m_pTexture, m_pSharedTexture);
				// Testing has shown that Flush is needed here for the texture
				// to be immediately available for subsequent copy.
				// May be removed if the texture is not immediately copied.
				// Test for the individual application.
				m_pImmediateContext->Flush();
			}
			// Allow access to the shared texture
			frame.AllowTextureAccess(m_pSharedTexture);
		}
		m_bConnected = true;

	} // sender exists
	else {
		// There is no sender or the connected sender closed.
		ReleaseReceiver();
		// Let the application know.
		m_bConnected = false;
	}

	// ReceiveTexture fails if there is no sender or the connected sender closed.
	return m_bConnected;

}

//---------------------------------------------------------
// Function: ReceiveTexture
//  Copy the sender DX11 shared texture
//
//    The receiving texture must be the same format
//    and must be re-allocated if IsUpdated() returns true
//
bool spoutDX::ReceiveTexture(ID3D11Texture2D** ppTexture)
{
	// No texture
	if (!ppTexture)
		return false;

	// Return if flagged for update
	// The update flag is reset when the receiving application calls IsUpdated()
	if (m_bUpdated)
		return true;

	// Try to receive texture details from a sender
	if (ReceiveSenderData()) {

		// Was the shared texture pointer retrieved ?
		if (!m_pSharedTexture) {
			return false;
		}

		// The sender name, width, height, format, shared texture handle and pointer have been retrieved.
		if (m_bUpdated) {
			// If the sender is new or changed, return to update the receiving texture.
			// The application detects the change with IsUpdated().
			return true;
		}

		// The application receiving texture is created
		// by the application on the first update above
		// ready for copy from the sender's shared texture.
		// Test for it here.
		ID3D11Texture2D* pTexture = *ppTexture;
		if (!pTexture) {
			return false;
		}

		//
		// Found a sender
		//
		if (frame.CheckTextureAccess(m_pSharedTexture)) {
			// Check if the sender has produced a new frame.
			if (frame.GetNewFrame()) {
				// Copy from the sender's shared texture to the receiving texture.
				m_pImmediateContext->CopyResource(pTexture, m_pSharedTexture);
				// Testing has shown that Flush is needed here for the texture
				// to be immediately available for subsequent copy.
				// May be removed if the texture is not immediately copied.
				// Test for the individual application.
				m_pImmediateContext->Flush();
			 }
			// Allow access to the shared texture
			frame.AllowTextureAccess(m_pSharedTexture);
		}
		m_bConnected = true;

	} // sender exists
	else {
		// There is no sender or the connected sender closed.
		ReleaseReceiver();
		// Let the application know.
		m_bConnected = false;
	}

	// ReceiveTexture fails if there is no sender or the connected sender closed.
	return m_bConnected;

}

//---------------------------------------------------------
// Function: ReceiveImage
// Receive from a sender via DX11 staging textures to an rgba or rgb buffer of variable size
// A new shared texture pointer (m_pSharedTexture) is retrieved if the sender changed
bool spoutDX::ReceiveImage(unsigned char * pixels,
	unsigned int width, unsigned int height, bool bRGB, bool bInvert)
{
	// Return if flagged for update
	// The update flag is reset when the receiving application calls IsUpdated()
	if (m_bUpdated)
		return true;

	// Try to receive texture details from a sender
	if (ReceiveSenderData()) {

		// The sender name, width, height, format, shared texture handle and pointer have been retrieved.
		if (m_bUpdated) {
			// A new sender has been found or the one connected has changed.
			// The staging textures must be the same size and format as the sender
			// Create new staging textures if it is a different size
			CheckStagingTextures(m_Width, m_Height, m_dwFormat);
			// The application detects the change with IsUpdated()
			// and the receiving buffer is updated to match the sender.
			return true;
		}

		// The receiving pixel buffer is created after the first update
		// So check here instead of at the beginning
		if (!pixels)
			return false;

		// No staging textures - no copy
		if (!m_pStaging[0] || !m_pStaging[1])
			return false;

		//
		// Found a sender
		//
		// Access the sender shared texture
		if (frame.CheckTextureAccess(m_pSharedTexture)) {
			// Check if the sender has produced a new frame.
			if (frame.GetNewFrame()) {
				// Read from the sender GPU texture to CPU pixels via two staging textures
				// One texture - approx 7 - 12 msec at 1920x1080
				// Two textures - approx 2.5 - 3.5 msec at 1920x1080
				m_Index = (m_Index + 1) % 2;
				m_NextIndex = (m_Index + 1) % 2;
				// Copy from the sender's shared texture to the first staging texture
				m_pImmediateContext->CopyResource(m_pStaging[m_Index], m_pSharedTexture);
				// Map and read from the second while the first is occupied
				ReadPixelData(m_pStaging[m_NextIndex], pixels, width, height, bRGB, bInvert, false);
			}
			// Allow access to the shared texture
			frame.AllowTextureAccess(m_pSharedTexture);
		}
		m_bConnected = true;
	} // sender exists
	else {
		// There is no sender or the connected sender closed.
		ReleaseReceiver();
		// Let the application know.
		m_bConnected = false;
	}

	// ReceiveImage fails if there is no sender or the connected sender closed.
	return m_bConnected;

}

//---------------------------------------------------------
// Function: SelectSender
// Open sender selection dialog
void spoutDX::SelectSender()
{
	SelectSenderPanel();
}

//---------------------------------------------------------
// Function: IsUpdated
// Query whether the sender has changed.
//
//   Must be checked at every cycle before receiving data. 
//
//   If this is not done, the receiving functions fail.
bool spoutDX::IsUpdated()
{
	bool bRet = m_bUpdated;
	m_bUpdated = false; // Reset the update flag before return
	return bRet;
}


//---------------------------------------------------------
// Function: IsConnected
// Query sender connection.
//
//   If the sender closes, receiving functions return false,  
//   but connection can be tested at any time.
bool spoutDX::IsConnected()
{
	return m_bConnected;
}

//---------------------------------------------------------
// Function: IsFrameNew
// Query received frame status
//
//   The receiving texture or pixel buffer is refreshed if the sender has produced a new frame  
//   This can be queried to process texture data only for new frames
bool spoutDX::IsFrameNew()
{
	return frame.IsFrameNew();
}

//---------------------------------------------------------
// Function: GetSenderTexture()
// Received class texture
//   Used together with ReceiveTexture()
//   Can also be called after receiving to an application texture
//   with ReceiveTexture(ID3D11Texture2D** ppTexture)
//
ID3D11Texture2D* spoutDX::GetSenderTexture()
{
	if (!m_bConnected)
		return nullptr;

	if (!m_pTexture) {

		// Create the class texture
		if (!CheckTexture(GetSenderWidth(), GetSenderHeight(), GetSenderFormat()))
			return nullptr;
		
		if (!m_pTexture)
			return nullptr;

		// Copy the shared texture to it
		if (frame.CheckTextureAccess(m_pSharedTexture)) {
			m_pImmediateContext->CopyResource(m_pTexture, m_pSharedTexture);
			m_pImmediateContext->Flush();
		}
		frame.AllowTextureAccess(m_pSharedTexture);

	}
	return m_pTexture;
}

//---------------------------------------------------------
// Function: GetSenderHandle
// Received sender share handle
HANDLE spoutDX::GetSenderHandle()
{
	return m_dxShareHandle;
}

//---------------------------------------------------------
// Function: GetSenderFormat
// Get sender DirectX texture format
DXGI_FORMAT spoutDX::GetSenderFormat()
{
	return (DXGI_FORMAT)m_dwFormat;
}


//---------------------------------------------------------
// Function: GetSenderName
// Get sender name
const char * spoutDX::GetSenderName()
{
	return m_SenderName;
}

//---------------------------------------------------------
// Function: GetSenderWidth
// Get sender width
unsigned int spoutDX::GetSenderWidth()
{
	return m_Width;
}

//---------------------------------------------------------
// Function: GetSenderHeight
// Get sender height
unsigned int spoutDX::GetSenderHeight()
{
	return m_Height;
}

//---------------------------------------------------------
// Function: GetSenderFps
// Get sender frame rate
double spoutDX::GetSenderFps()
{
	return frame.GetSenderFps();
}

//---------------------------------------------------------
// Function: GetSenderFrame
// Get sender frame number
long spoutDX::GetSenderFrame()
{
	return frame.GetSenderFrame();
}


//---------------------------------------------------------
// COMMON
//

//
// Group: Frame counting
//

//---------------------------------------------------------
// Function: HoldFps
// Frame rate control
//    Desired frames per second
void spoutDX::HoldFps(int fps)
{
	frame.HoldFps(fps);
}

// Function: DisableFrameCount
// Disable frame counting specifically for this application
void spoutDX::DisableFrameCount()
{
	frame.DisableFrameCount();
}

//---------------------------------------------------------
// Function: IsFrameCountEnabled
// Return frame count status
bool spoutDX::IsFrameCountEnabled()
{
	return frame.IsFrameCountEnabled();
}

// -----------------------------------------------
// Function: SetFrameSync
// Signal sync event.
//   Create a named sync event and set for test
void spoutDX::SetFrameSync(const char* SenderName)
{
	if (SenderName && SenderName[0] && m_bSpoutInitialized)
		frame.SetFrameSync(SenderName);
}

// -----------------------------------------------
// Function: WaitFrameSync
// Wait or test for named sync event.
// Wait until the sync event is signalled or the timeout elapses.
// Events are typically created based on the sender name and are
// effective between a single sender/receiver pair.
//   - For testing for a signal, use a wait timeout of zero.
//   - For synchronization, use a timeout greater than the expected delay
// 
bool spoutDX::WaitFrameSync(const char *SenderName, DWORD dwTimeout)
{
	if (!SenderName || !SenderName[0] || !m_bSpoutInitialized)
		return false;
	return frame.WaitFrameSync(SenderName, dwTimeout);
}


//---------------------------------------------------------
// SenderNames
//

//
// Group: Sender names
//

//---------------------------------------------------------
// Function: GetSenderCount
// Number of senders
int spoutDX::GetSenderCount()
{
	return sendernames.GetSenderCount();
}

//---------------------------------------------------------
// Function: GetSender
// Sender item name in the sender names set
bool spoutDX::GetSender(int index, char* sendername, int sendernameMaxSize)
{
	return sendernames.GetSender(index, sendername, sendernameMaxSize);
}

//---------------------------------------------------------
// Function: GetSenderInfo
// Sender information
bool spoutDX::GetSenderInfo(const char* sendername, unsigned int &width, unsigned int &height, HANDLE &dxShareHandle, DWORD &dwFormat)
{
	return sendernames.GetSenderInfo(sendername, width, height, dxShareHandle, dwFormat);
}

//---------------------------------------------------------
// Function: GetActiveSender
// Current active sender name
bool spoutDX::GetActiveSender(char* Sendername)
{
	return sendernames.GetActiveSender(Sendername);
}

//---------------------------------------------------------
// Function: SetActiveSender
// Set sender as active
bool spoutDX::SetActiveSender(const char* Sendername)
{
	return sendernames.SetActiveSender(Sendername);
}

//---------------------------------------------------------
// Function: GetMaxSenders
// Get user Maximum senders allowed
int spoutDX::GetMaxSenders()
{
	return(sendernames.GetMaxSenders());
}

//---------------------------------------------------------
// Function: SetMaxSenders
// Set user Maximum senders allowed
void spoutDX::SetMaxSenders(int maxSenders)
{
	sendernames.SetMaxSenders(maxSenders);
}


//
// Adapter functions
//

//
// Group: Graphics adapter
//
// Return graphics adapter number and names.
// Get and set adapter index for the DirectX device.
//
// Note that both the Sender and Receiver must use the same graphics adapter.
//


//---------------------------------------------------------
// Function: GetNumAdapters
// The number of graphics adapters in the system
int spoutDX::GetNumAdapters()
{
	return spoutdx.GetNumAdapters();
}

//---------------------------------------------------------
// Function: GetAdapterName
// Get adapter item name
bool spoutDX::GetAdapterName(int index, char *adaptername, int maxchars)
{
	return spoutdx.GetAdapterName(index, adaptername, maxchars);
}

//---------------------------------------------------------
// Function: GetAdapterInfo
// // Get the current adapter description
bool spoutDX::GetAdapterInfo(char *adapter, char *display, int maxchars)
{
	return spoutdx.GetAdapterInfo(adapter, display, maxchars);
}

//---------------------------------------------------------
// Function: GetAdapter
// Get adapter index
int spoutDX::GetAdapter()
{
	return spoutdx.GetAdapter();
}

//---------------------------------------------------------
// Function: SetAdapter
// Set graphics adapter for output
bool spoutDX::SetAdapter(int index)
{
	if (spoutdx.SetAdapter(index))
		return true;

	SpoutLogError("spoutDX::SetAdapter(%d) failed", index);
	spoutdx.SetAdapter(0); // make sure globals are reset to default

	return false;
}

//---------------------------------------------------------
// Function: GetAdapterPointer
// Get adapter pointer for a given adapter (-1 means current)
IDXGIAdapter* spoutDX::GetAdapterPointer(int index)
{
	return spoutdx.GetAdapterPointer(index);
}

//---------------------------------------------------------
// Function: SetAdapterPointer
// Set required graphics adapter for creating a device
void spoutDX::SetAdapterPointer(IDXGIAdapter* pAdapter)
{
	spoutdx.SetAdapterPointer(pAdapter);
}

//---------------------------------------------------------
// Function: GetAdapterAuto
// Get auto device switching status
bool spoutDX::GetAdapterAuto()
{
	return m_bAdapt;
}

//---------------------------------------------------------
// Function: SetAdapterAuto
// Auto switch receiving device to use the same adapter as the sender
void spoutDX::SetAdapterAuto(bool bAdapt)
{
	if (bAdapt && !IsClassDevice()) {
		SpoutLogWarning("spoutDX::SetAdapterAuto : D3D11 device created outside the class");
		return;
	}
	m_bAdapt = bAdapt;
}

//---------------------------------------------------------
// Function: GetSenderAdapter
// Get adapter index and name for a given sender
//
// OpenDX11shareHandle will fail if the share handle has been created 
// using a different graphics adapter (see spoutDirectX).
//
// This function loops though all graphics adapters in the system
// until OpenDX11shareHandle is successful and the same adapter
// index as the sender is established. 
//
// This adapter can then be used by CreateDX11device when the Spout
// DirectX device is created.
//
int spoutDX::GetSenderAdapter(const char* sendername, char* adaptername, int maxchars)
{
	if (!sendername || !sendername[0])
		return -1;

	int senderadapter = -1;
	ID3D11Texture2D* pSharedTexture = nullptr;
	ID3D11Device* pDummyDevice = nullptr;
	ID3D11DeviceContext* pContext = nullptr;
	IDXGIAdapter* pAdapter = nullptr;

	// Get the current device adapter pointer (could be null default)
	IDXGIAdapter* pCurrentAdapter = spoutdx.GetAdapterPointer();

	SpoutLogNotice("spoutDX::GetSenderAdapter - testing for sender adapter (%s)", sendername);

	SharedTextureInfo info={};
	if (sendernames.getSharedInfo(sendername, &info)) {
		const int nAdapters = spoutdx.GetNumAdapters();
		for (int i = 0; i < nAdapters; i++) {
			pAdapter = spoutdx.GetAdapterPointer(i);
			if (pAdapter) {
				SpoutLogNotice("   testing adapter %d", i);
				// Set the adapter pointer for CreateDX11device to use temporarily
				spoutdx.SetAdapterPointer(pAdapter);
				// Create a dummy device using this adapter
				pDummyDevice = spoutdx.CreateDX11device();
				if (pDummyDevice) {
					// Try to open the share handle with the device created from the adapter
					if (spoutdx.OpenDX11shareHandle(pDummyDevice, &pSharedTexture, LongToHandle((long)info.shareHandle))) {
						// break as soon as it succeeds
						SpoutLogNotice("    found sender adapter %d (0x%.7X)", i, PtrToUint(pAdapter));
						senderadapter = i;
						// Return the adapter name
						if(adaptername)
							spoutdx.GetAdapterName(i, adaptername, maxchars);
						pDummyDevice->GetImmediateContext(&pContext);
						if (pContext) pContext->Flush();
						pDummyDevice->Release();
						pAdapter->Release();
						break;
					}
					pDummyDevice->GetImmediateContext(&pContext);
					if (pContext) pContext->Flush();
					pDummyDevice->Release();
				}
				pAdapter->Release();
			}
		}
	}

	// Set the SpoutDirectX class adapter pointer back to what it was
	spoutdx.SetAdapterPointer(pCurrentAdapter);

	// Return the sender adapter index
	return senderadapter;

}

//
// Group: Graphics performance
//
// Windows Graphics performance preferences.
//
// To be effective, performance preference requires the system to have multiple graphics
// processors which provide a choice between "Power saving" and "High perfformance". 
// Typically this will be laptop systems with integrated and discrete graphics.
// Desktop systems with multiple discrete graphics cards do not provide that choice, 
// even though Windows still allows applications to be set for desired preference.
//
// Windows preferences take priority over any settings made by driver
// programs such as the NVIDIA Control Panel or AMD Control Center for that application.
// If there is no Windows preference for an application, the graphics driver <settings
// at https://www.nvidia.com/content/Control-Panel-Help/vLatest/en-us/mergedProjects/nv3d/Setting_the_Preferred_Graphics_Processor.htm> take effect.
//
// Performance prefrence settings are available from Windows 10
// April 2018 update "Redstone 4" (Version 1803, build 17134) and later.
// Windows 10 SDK required included in Visual Studio 2017 ver.15.7 
//
#ifdef NTDDI_WIN10_RS4

//---------------------------------------------------------
// Function: GetPerformancePreference
// Get the Windows graphics preference for an application
//
//	-1 - Not registered
//
//	 0 - DXGI_GPU_PREFERENCE_UNSPECIFIED
//
//	 1 - DXGI_GPU_PREFERENCE_MINIMUM_POWER
//
//	 2 - DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE
//
// If no path is specified, use the current application path
//
int spoutDX::GetPerformancePreference(const char* path)
{
	return spoutdx.GetPerformancePreference(path);
}

//---------------------------------------------------------
// Function: SetPerformancePreference
// Set the Windows graphics preference for an application
//
//     -1 - No preference
//
//      0 - Default
//
//      1 - Power saving
//
//      2 - High performance
//
// If no path is specified, use the current application path
//
bool spoutDX::SetPerformancePreference(int preference, const char* path)
{
	return spoutdx.SetPerformancePreference(preference, path);
}


//
// https://learn.microsoft.com/en-us/windows/win32/api/dxgi1_6/nf-dxgi1_6-idxgifactory6-enumadapterbygpupreference
//
// When DXGI_GPU_PREFERENCE_UNSPECIFIED is specified for the GpuPreference parameter,
// this method is equivalent to calling IDXGIFactory1::EnumAdapters1.
//
// When DXGI_GPU_PREFERENCE_MINIMUM_POWER is specified for the GpuPreference parameter,
// the order of preference for the adapter returned in ppvAdapter will be:
// 1. iGPUs (integrated GPUs)
// 2. dGPUs (discrete GPUs)
// 3. xGPUs (external GPUs)
//
// When DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE is specified for the GpuPreference parameter,
// the order of preference for the adapter returned in ppvAdapter will be:
// 1. xGPUs (external GPUs)
// 2. dGPUs (discrete GPUs)
// 3. iGPUs (integrated GPUs)
//

//---------------------------------------------------------
// Function: GetPreferredAdapterName
//
// Get the graphics adapter name for a Windows preference.
// This is the first adapter for the given preference :
//
//    DXGI_GPU_PREFERENCE_UNSPECIFIED - (0) Equivalent to EnumAdapters1
//
//    DXGI_GPU_PREFERENCE_MINIMUM_POWER - (1) Integrated GPU
//
//    DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE - (2) External GPU / Discrete GPU
//
bool spoutDX::GetPreferredAdapterName(int preference, char* adaptername, int maxchars)
{
	return spoutdx.GetPreferredAdapterName(preference, adaptername, maxchars);
}

//---------------------------------------------------------
// Function: SetPreferredAdapter
//
// Set graphics adapter index for a Windows preference
//
// Set the adapter index for a performance preference without registering the
// preference with Windows. This index is used by CreateDX11device when DirectX
// is intitialized. The function should be called before Spout is initialized.
//
//    DXGI_GPU_PREFERENCE_UNSPECIFIED - (0) Equivalent to EnumAdapters1
//
//    DXGI_GPU_PREFERENCE_MINIMUM_POWER - (1) Integrated GPU
//
//    DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE - (2) External GPU / Discrete GPU
//
// The function achieves this result without user Windows preference by getting the name
// of the preferred adapter using EnumAdapterByGpuPreference, finding the index of
// that adapter in the current list of adapters, using EnumAdapters and finally 
// retrieving a pointer to the adapter for CreateDX11device to use. To be effective,
// this requires a system with multiple graphics processors which enable a choice 
// between "Power saving" and "High performance".
//
bool spoutDX::SetPreferredAdapter(int preference)
{
	return spoutdx.SetPreferredAdapter(preference);
}


//---------------------------------------------------------
// Function: IsPreferenceAvailable()
// Availability of Windows graphics preference settings.
//
// Settings are available from Windows 10 April 2018 update 
// (Version 1803, build 17134) and later.
bool spoutDX::IsPreferenceAvailable()
{
	return spoutdx.IsPreferenceAvailable();
}

//---------------------------------------------------------
// Function: IsApplicationPath
//
// Is the path a valid application
//
// A valid application path will have a drive letter and terminate with ".exe"
bool spoutDX::IsApplicationPath(const char* path)
{
	return spoutdx.IsApplicationPath(path);
}
#endif


//
// Group: Data sharing
//
//   General purpose data exchange functions using shared memory.
//   These functions can be used in addition to texture sharing.
//   Typical uses will be for data attached to the video frame,
//   commonly referred to as "per frame Metadata".
//
//   Notes for synchronisation.
//
//   If used before sending and after receiving, the data will be 
//   associated with the same video frame, but frames may be missed 
//   if the receiver has a lower frame rate than the sender.
//
//   If strict synchronization is required, the data sharing functions
//   should be used in combination with event signal functions. The sender
//   frame rate will be matched exactly to that of the receiver and the 
//   receiver will not miss any frames.
//
//      - void SetFrameSync(const char* SenderName);
//      - bool WaitFrameSync(const char *SenderName, DWORD dwTimeout = 0);
//
//   WaitFrameSync
//   A sender should use this before rendering or sending texture or data and
//   wait for a signal from the receiver that it is ready to read another frame.
//
//   SetFrameSync
//   After receiving a texture, rendering the result and reading data
//   a receiver should signal that it is ready to read another. 
//

//---------------------------------------------------------
// Function: WriteMemoryBuffer
// Write buffer to sender shared memory.
//
//    If shared memory has not been created in advance, it will be
//    created on the first call to this function at the length specified.
//
//    This is acceptable if the data to send is fixed in length.
//    Otherwise the shared memory should be created in advance of sufficient
//    size to contain the maximum length expected (see CreateMemoryBuffer).
//
//    The map is closed when the sender is released.
//
bool spoutDX::WriteMemoryBuffer(const char *name, const char* data, int length)
{
	// Quit if 2.006 memoryshare mode
	if (m_bMemoryShare)
		return false;

	if (!name || !name[0]) {
		SpoutLogError("SpoutSharedMemory::WriteMemoryBuffer - no name");
		return false;
	}

	if (!data) {
		SpoutLogError("SpoutSharedMemory::WriteMemoryBuffer - no data");
		return false;
	}

	// Create a shared memory map for the buffer if it does not exist yet
	if (memorybuffer.Size() == 0) {
		// Create a name for the map from the sender name
		std::string namestring = name;
		namestring += "_map";
		// The first 16 bytes are reserved for the memory map size. Make the map larger to compensate. 
		if (!memorybuffer.Create(namestring.c_str(), length + 16)) {
			SpoutLogError("SpoutSharedMemory::WriteMemoryBuffer - could not create shared memory");
			return false;
		}
		char* pBuffer = memorybuffer.Lock();
		if (!pBuffer) {
			SpoutLogError("SpoutSharedMemory::WriteMemoryBuffer - no buffer lock");
			return false;
		}
		// Convert the map size to decimal digit chars directly to shared memory
		_itoa_s(length, reinterpret_cast<char *>(pBuffer), 16, 10);
		memorybuffer.Unlock();
		SpoutLogNotice("SpoutSharedMemory::WriteMemoryBuffer - created memory buffer %d bytes", length);
	}

	char* pBuffer = memorybuffer.Lock();
	if (!pBuffer) {
		SpoutLogError("SpoutSharedMemory::WriteMemoryBuffer - no buffer lock");
		return false;
	}

	// Write user data to shared memory (skip the map size)
	memcpy(reinterpret_cast<void *>(pBuffer + 16), reinterpret_cast<const void *>(data), length);

	// Terminate the shared memory data with a null.
	// The map is created larger in advance to allow for it.
	if (memorybuffer.Size() > (16 + length))
		*(pBuffer + 16 + length) = 0;

	memorybuffer.Unlock();

	return true;
}

//---------------------------------------------------------
// Function: ReadMemoryBuffer
// Read sender shared memory to a buffer.
//
//    Open a sender memory map and retain the handle.
//    The map is closed when the receiver is released.
int spoutDX::ReadMemoryBuffer(const char* name, char* data, int maxlength)
{
	// Quit if 2.006 memory share mode
	if (m_bMemoryShare)
		return 0;

	if (!name || !name[0]) {
		SpoutLogError("SpoutSharedMemory::ReadMemoryBuffer - no name");
		return 0;
	}

	if (!data) {
		SpoutLogError("SpoutSharedMemory::ReadMemoryBuffer - no data");
		return 0;
	}

	// Open a shared memory map for the buffer if it not already
	if (!memorybuffer.Name()) {
		// Create a name for the map from the sender name
		std::string namestring = name;
		namestring += "_map";
		// Open the shared memory. This also creates a mutex
		// for the reader to lock and unlock the map for reads.
		if (!memorybuffer.Open(namestring.c_str())) {
			return 0;
		}
		SpoutLogNotice("SpoutSharedMemory::ReadMemoryBuffer - opened sender memory map [%s]", memorybuffer.Name());
	}

	char* pBuffer = memorybuffer.Lock();
	if (!pBuffer) {
		SpoutLogError("SpoutSharedMemory::ReadMemoryBuffer - no buffer lock");
		return 0;
	}

	// The memory map includes it's size, saved as the first 16 bytes
	*(pBuffer + 15) = 0; // End for atoi

	// Number of bytes available for data transfer
	int nbytes = atoi(reinterpret_cast<char *>(pBuffer));

	// Reduce if the user buffer max length is less
	if (maxlength < nbytes)
		nbytes = maxlength;

	// Copy bytes from shared memory to the user buffer
	if (nbytes > 0)
		memcpy(reinterpret_cast<void *>(data), reinterpret_cast<const void *>(pBuffer + 16), nbytes);

	// Done with the shared memory buffer pointer
	memorybuffer.Unlock();

	return nbytes;

}

//---------------------------------------------------------
// Function: CreateMemoryBuffer
// Create a sender shared memory buffer.
//
//    Create a memory map and retain the handle.
//    This function should be called before any buffer write
//    if the length of the data to send will vary.
//    The map is closed when the sender is released.
bool spoutDX::CreateMemoryBuffer(const char *name, int length)
{
	// Quit if 2.006 memoryshare mode
	if (m_bMemoryShare)
		return false;

	if (!name || !name[0]) {
		SpoutLogError("spoutDX::CreateMemoryBuffer - no name");
		return false;
	}

	if (memorybuffer.Size() > 0) {
		SpoutLogError("spoutDX::CreateMemoryBuffer - shared memory already exists");
		return false;
	}

	// Create a name for the map from the sender name
	std::string namestring = name;
	namestring += "_map";

	// The first 16 bytes are reserved to record the number of bytes available
	// for data transfer. Make the map 16 bytes larger to compensate. 
	// Add another 16 bytes to allow for a null terminator.
	// (Use multiples of 16 for alignment to allow for SSE copy : TODO).
	if (!memorybuffer.Create(namestring.c_str(), length + 32)) {
		SpoutLogError("spoutGL::CreateMemoryBuffer - could not create shared memory");
		return false;
	}

	// The length requested is the number of bytes to be
	// available for data transfer (map data size).
	char* pBuffer = memorybuffer.Lock();
	if (!pBuffer) {
		SpoutLogError("spoutGL::CreateMemoryBuffer - no buffer lock");
		return false;
	}

	// Convert the map data size to decimal digit chars
	// directly to the first 16 bytes of the shared memory.
	_itoa_s(length, reinterpret_cast<char *>(pBuffer), 16, 10);

	memorybuffer.Unlock();

	SpoutLogNotice("spoutDXL::CreateMemoryBuffer - created memory buffer %d bytes", length);

	return true;
}

//---------------------------------------------------------
// Function: DeleteMemoryBuffer
// Delete a sender shared memory buffer.
//
bool spoutDX::DeleteMemoryBuffer()
{
	// Quit if 2.006 memoryshare mode
	if (m_bMemoryShare)
		return false;

	// A sender must create a map
	// A receiver must open a sender map to find the size
	if (memorybuffer.Size() == 0) {
		SpoutLogError("spoutDX::DeleteMemoryBuffer - no shared memory size");
		return false;
	}

	memorybuffer.Close();

	return true;

}


//---------------------------------------------------------
// Function: GetMemoryBufferSize
// Get the size of a sender shared memory buffer.
//
int spoutDX::GetMemoryBufferSize(const char *name)
{
	if (!m_bSpoutInitialized)
		return 0;

	// A writer has created the map and recorded the data length in the first 16 bytes.
	// Another 16 bytes is added to allow for a terminating NULL. (See CreateMemoryBuffer)
	// The remaining length is the number of bytes available for data transfer.
	if (memorybuffer.Size() > 32) {
		return memorybuffer.Size() - 32;
	}

	// A reader must read the map to get the size
	// Open a shared memory map for the buffer if it not already
	if (!memorybuffer.Name()) {
		// Create a name for the map
		std::string namestring = name;
		namestring += "_map";
		// Open the shared memory map.
		// This also creates a mutex for the receiver to lock and unlock the map for reads.
		if (!memorybuffer.Open(namestring.c_str())) {
			return 0;
		}
		SpoutLogNotice("spoutDX::GetMemoryBufferSize - opened sender memory map [%s]", memorybuffer.Name());
	}

	char* pBuffer = memorybuffer.Lock();
	if (!pBuffer) {
		SpoutLogError("spoutDX::GetMemoryBufferSize - no buffer lock");
		return 0;
	}

	// The number of bytes of the memory map available for data transfer
	// is saved in the first 16 bytes.
	*(pBuffer + 15) = 0; // End for atoi
	int nbytes = atoi(reinterpret_cast<char *>(pBuffer));

	memorybuffer.Unlock();

	return nbytes;

}

//
// Sharing modes
//

//
// Group: Retained for 2.006 compatibility
//

//---------------------------------------------------------
// Function: GetDX9
// Get user DX9 mode
bool spoutDX::GetDX9()
{
	DWORD dwDX9 = 0;
	ReadDwordFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "DX9", &dwDX9);
	return (dwDX9 == 1);
}

//---------------------------------------------------------
// Function: GetMemoryShareMode
// Get user memory share mode
bool spoutDX::GetMemoryShareMode()
{
	bool bRet = false;
	DWORD dwMem = 0;
	if (ReadDwordFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "MemoryShare", &dwMem)) {
		bRet = (dwMem == 1);
	}
	return bRet;
}

//
// Utilities
//

//---------------------------------------------------------
// Function: CreateDX11texture
// Create a texture that is not shared
bool spoutDX::CreateDX11texture(ID3D11Device* pd3dDevice,
	unsigned int width, unsigned int height,
	DXGI_FORMAT format, ID3D11Texture2D** ppTexture)
{
	return spoutdx.CreateDX11Texture(pd3dDevice, width, height, format, ppTexture);
}

//---------------------------------------------------------
// Function: CheckSenderFormat
// Find sender shared memory.
// If format is zero, try to open the sharehandle
// and write the correct format back to shared memory
// Also register the sender name if not already
void spoutDX::CheckSenderFormat(const char * sendername)
{
	SharedTextureInfo info={};

	// Correct for format 0 and un-registered
	if (sendernames.getSharedInfo(sendername, &info)) {
		if (info.format == 0) {
			ID3D11Device *pDummyDevice = nullptr;
			ID3D11Texture2D* pSharedTexture = nullptr;
			pDummyDevice = spoutdx.CreateDX11device();
			if (pDummyDevice) {

				// Try to open the share handle
				// If it's DirectX9, the handle will not open
				if (spoutdx.OpenDX11shareHandle(pDummyDevice, &pSharedTexture, LongToHandle((LONG)info.shareHandle))) {
					if (pSharedTexture) {
						// Get the texture details
						D3D11_TEXTURE2D_DESC desc;
						pSharedTexture->GetDesc(&desc);
						// Set the format to the sender info
						info.format = (DWORD)desc.Format;
						sendernames.setSharedInfo(sendername, &info);
						pSharedTexture->Release();
					}
				}
			}
		}

		// Register the sender name if not already
		sendernames.RegisterSenderName(sendername);

		// Make active if there is no active sender
		char activename[256]={};
		if (!sendernames.GetActiveSender(activename))
			sendernames.SetActiveSender(sendername);
	}
}

//
// SpoutUtils namespace functions for dll access
//

void spoutDX::OpenSpoutConsole()
{
	spoututils::OpenSpoutConsole();
}

void spoutDX::CloseSpoutConsole(bool bWarning)
{
	spoututils::CloseSpoutConsole(bWarning);
}

void spoutDX::EnableSpoutLog()
{
	spoututils::EnableSpoutLog();
}

void spoutDX::EnableSpoutLogFile(const char* filename, bool append)
{
	spoututils::EnableSpoutLogFile(filename, append);
}

void spoutDX::DisableSpoutLogFile()
{
	spoututils::DisableSpoutLogFile();
}

void spoutDX::DisableSpoutLog()
{
	spoututils::DisableSpoutLog();
}


//
// PRIVATE
//

// --------------------------------------------------------
// If a sender has not been created yet
//    o Make sure DirectX is initialized
//    o Create a shared texture for the sender
//    o Create a sender using the DX11 shared texture handle
// If the sender exists, test for size or format change
//    o Re-create the class shared texture to the new size
//    o Update the sender and class variables
bool spoutDX::CheckSender(unsigned int width, unsigned int height, DWORD dwFormat)
{
	if (width == 0 || height == 0)
		return false;

	// The sender needs a name
	// Default is the executable name
	if (!m_SenderName[0]) {
		if (!SetSenderName())
			return false;
	}

	if (!m_bSpoutInitialized) {

		// Make sure DirectX is initialized
		if (!OpenDirectX11())
			return false;

		// Save width and height to test for sender size changes
		m_Width = width;
		m_Height = height;
		m_dwFormat = dwFormat;

		// Create a shared texture for the sender
		// A sender creates a new texture with a new share handle
		m_dxShareHandle = nullptr;
		spoutdx.CreateSharedDX11Texture(m_pd3dDevice, m_Width, m_Height, (DXGI_FORMAT)m_dwFormat, &m_pSharedTexture, m_dxShareHandle, m_bKeyed);

		// Create a sender using the DX11 shared texture handle (m_dxShareHandle)
		// and specifying the same texture format
		m_bSpoutInitialized = sendernames.CreateSender(m_SenderName, m_Width, m_Height, m_dxShareHandle, m_dwFormat);
		
		// TODO
		// This could be a separate function SetHostPath
		SharedTextureInfo info = {}; // Empty structure
		if (!sendernames.getSharedInfo(m_SenderName, &info)) {
			SpoutLogWarning("spoutGL::SetHostPath(%s) - could not get sender info", m_SenderName);
			return false;
		}
		char exepath[256]={};
		GetModuleFileNameA(NULL, exepath, sizeof(exepath));

		// Description field is 256 uint8_t
		strcpy_s((char*)info.description, 256, exepath);

		if (!sendernames.setSharedInfo(m_SenderName, &info)) {
			SpoutLogWarning("spoutGL::SetHostPath(%s) - could not set sender info", m_SenderName);
		}

		// Create a sender mutex for access to the shared texture
		frame.CreateAccessMutex(m_SenderName);
		
		// Enable frame counting so the receiver gets frame number and fps
		frame.EnableFrameCount(m_SenderName);

	}
	// Initialized but has the source texture changed size ?
	else if (m_Width != width || m_Height != height || m_dwFormat != dwFormat) {

		// Re-create the class shared texture with the new size
		if (m_pSharedTexture) {
			spoutdx.ReleaseDX11Texture(m_pSharedTexture);
			// The existing shared texture is changed on this device
			m_pImmediateContext->Flush();
		}
		m_pSharedTexture = nullptr;
		m_dxShareHandle = nullptr;
		spoutdx.CreateSharedDX11Texture(m_pd3dDevice, width, height, (DXGI_FORMAT)dwFormat, &m_pSharedTexture, m_dxShareHandle, m_bKeyed);

		// Update the sender and class variables
		sendernames.UpdateSender(m_SenderName, width, height, m_dxShareHandle, dwFormat);

		m_Width = width;
		m_Height = height;
		m_dwFormat = dwFormat;

	} // endif initialization or size checks

	return true;

}


//---------------------------------------------------------
// Used when the sender was there but the texture pointer could not be retrieved from the share handle.
// Try using the sender adapter if different.
ID3D11Texture2D* spoutDX::CheckSenderTexture(char *sendername, HANDLE dxShareHandle)
{
	// If auto adapter switching not activated, return a NULL pointer
	if (!m_bAdapt) {
		SpoutLogWarning("spoutDX::CheckSenderTexture - no automatic switch to sender adapter");
		return nullptr;
	}

	ID3D11Texture2D* pTexture =  nullptr;

	// Save the current adapter index
	int receiverindex = GetAdapter();

	// Get the sender adapter index
	int senderindex = GetSenderAdapter(sendername);
		
	// If they are different, switch to the sender adapter
	if (receiverindex != senderindex) {

		CloseDirectX11(); // Close the current DX11 device

		SetAdapter(senderindex); // Change to the same adapter as the sender
		
		OpenDirectX11(); // Create a new device with that adapter

		// Try again to get the texture pointer
		if (!spoutdx.OpenDX11shareHandle(m_pd3dDevice, &pTexture, dxShareHandle)) {

			// If that didn't work, change back to the original adapter
			CloseDirectX11();
			SetAdapter(receiverindex);
			OpenDirectX11();
			SpoutLogWarning("spoutDX::CheckSenderTexture - could not change to sender adapter %d", senderindex);

			return nullptr;
		}

		// The sharehandle opened OK with the new adapter
		SpoutLogNotice("spoutDX::CheckSenderTexture - changed to sender adapter %d", senderindex);

		return pTexture;

	}

	// The adapter was not different so the sharehandle still could not be opened
	return nullptr;

}

//---------------------------------------------------------
//	o Connect to a sender and inform the application to update texture dimensions
//	o Check for user sender selection
//  o Receive texture details from the sender for write to the user texture
//  o Retrieve width, height, format, share handle and texture pointer
bool spoutDX::ReceiveSenderData()
{
	m_bUpdated = false;
	
	// Make sure DirectX is initialized
	if (!OpenDirectX11())
		return false;

	// Initialization is recorded in this class for sender or receiver
	// m_Width or m_Height are established when the receiver connects to a sender
	char sendername[256]={};
	strcpy_s(sendername, 256, m_SenderName);

	// Check the entered Sender name to see if it exists
	if (sendername[0] == 0) {
		// Passed name was null, so find the active sender
		if (!GetActiveSender(sendername))
			return false; // No sender
	}

	// If SpoutPanel has been opened, the active sender name could be different
	if (CheckSpoutPanel(sendername, 256)) {
		// Disable the setup name
		m_SenderNameSetup[0] = 0;
	}

	// Now we have either an existing sender name or the active sender name

	// Save current sender name and dimensions to test for change
	unsigned int width = m_Width;
	unsigned int height = m_Height;
	DWORD dwFormat = m_dwFormat;
	HANDLE dxShareHandle = m_dxShareHandle;

	// Try to get the sender shared memory information.
	// Retrieve width, height, sharehandle and format.
	SharedTextureInfo info={};
	if (sendernames.getSharedInfo(sendername, &info)) {

		// Memory share mode not supported (no texture share handle)
		if (info.shareHandle == 0) {
			ReleaseReceiver();
			return false;
		}

		width  = info.width;
		height = info.height;
		dxShareHandle = (HANDLE)(LongToHandle((long)info.shareHandle));
		dwFormat = info.format;

		// For DX9 sender formats, use a compatible DX11 format
		// 21 =	D3DFMT_A8R8G8B8
		// 22 = D3DFMT_X8R8G8B8
		if (dwFormat == 21 || dwFormat == 22) {
			dwFormat = (DWORD)DXGI_FORMAT_B8G8R8A8_UNORM;
		}

		// The shared texture handle will be different
		//   o for a new sender
		//   o for texture size or format change
		if (dxShareHandle != m_dxShareHandle) {

			/*
			printf("\nReceiveSenderData : %s \n", sendername);
			printf("     Width     : %d\n", info.width);
			printf("     Height    : %d\n", info.height);
			printf("     Format    : %d (0x%X)\n", info.format, info.format);
			printf("     handle    : 0x%X\n", info.shareHandle);
			printf("     Usage     : %d (0x%X)\n", info.usage, info.usage);
			printf("     Description : [%s]\n", (char*)info.description);
			printf("     PartnerId : 0x%X\n", info.partnerId);
			*/

			// Release everything and start again
			ReleaseReceiver();

			// Update the sender share handle
			m_dxShareHandle = dxShareHandle;

			// Get a new shared texture pointer (m_pSharedTexture)
			if (!spoutdx.OpenDX11shareHandle(m_pd3dDevice, &m_pSharedTexture, dxShareHandle)) {

				// If this fails, the sender graphics adapter might be different
				SpoutLogWarning("SpoutReceiver::ReceiveSenderData - could not retrieve sender texture from share handle");

				// If a device has been created within this class, we can re-create it
				// on the fly using a different graphics adapter if auto adapter switching 
				// has been activated with SetAdapterAuto()
				if (m_bClassDevice && m_bAdapt) {
					// Test to find the sender adapter.
					// If different, switch to it and retrieve the shared texture pointer.
					ID3D11Texture2D* pTexture = CheckSenderTexture(sendername, dxShareHandle);
					// CheckSenderTexture will re-create the class D3D11 device using the sender's adapter
					if (!pTexture) {
						// If that failed, retain the share handle (m_dxShareHandle) 
						// so we don't query it again. The texture pointer (m_pSharedTexture)
						// is null but will not be used. Return true and wait until another
						// sender is selected.
						return true;
					}
					// Use the new sender texture pointer retrieved by CheckSenderTexture
					m_pSharedTexture = pTexture;
				}
				else {
					// Wait until another sender is selected or the shared texture handle is valid.
					return true;
				}

			}

			// Get the texture details to check for zero size
			D3D11_TEXTURE2D_DESC desc;
			ZeroMemory(&desc, sizeof(desc));
			m_pSharedTexture->GetDesc(&desc);
			if (desc.Width == 0 || desc.Height == 0)
				return false;

			// For incorrect sender information, use the format
			// of the D3D11 texture generated by OpenDX11shareHandle
			if (dwFormat != (DWORD)desc.Format)
				dwFormat = (DWORD)desc.Format;

			// Initialize again with the newly connected sender values
			CreateReceiver(sendername, width, height, dwFormat);

			// Return to update the receiving texture or image
			// If the graphics adapter was changed, the class device will be different
			// and the texture returned will have been created using that device.
			m_bUpdated = true;

		}

		// Connected and intialized
		// Sender name, width, height, format, texture pointer and share handle have been retrieved

		// The application can now access and copy the sender texture
		return true;

		//
		// ===================================================
		

	} // end find sender

	// There is no sender or the connected sender closed
	return false;

}

// Create receiver resources for a new sender
void spoutDX::CreateReceiver(const char * SenderName, unsigned int width, unsigned int height, DWORD dwFormat)
{
	SpoutLogNotice("CreateReceiver(%s) %d x %d : format %d", SenderName, width, height, dwFormat);

	if (m_bSpoutInitialized)
		ReleaseReceiver();

	// Create a named sender mutex for access to the sender's shared texture
	frame.CreateAccessMutex(SenderName);

	// Enable frame counting to get the sender frame number and fps
	frame.EnableFrameCount(SenderName);

	// Set class globals
	strcpy_s(m_SenderName, 256, SenderName);
	m_Width = width;
	m_Height = height;
	m_dwFormat = dwFormat;

	m_bSpoutInitialized = true;

}

//
// COPY FROM A DX11 STAGING TEXTURE TO A USER RGBA/RGB/BGR PIXEL BUFFER OF GIVEN SIZE
//
// A class device and context must have been created using OpenDirectX11()
//
// bRGB - pixel data is RGB instead of RGBA
// bInvert - flip the image
// bSwap - swap red/blue (BGRA/RGBA). Not available for re-sample
//
bool spoutDX::ReadPixelData(ID3D11Texture2D* pStagingSource, unsigned char* destpixels,
	unsigned int width, unsigned int height, bool bRGB, bool bInvert, bool bSwap)
{
	if (!m_pImmediateContext || !pStagingSource || !destpixels)
		return false;

	// Map the staging texture resource so we can access the pixels
	D3D11_MAPPED_SUBRESOURCE mappedSubResource={};
	// Make sure all commands are done before mapping the staging texture
	m_pImmediateContext->Flush();
	// Map waits for GPU access
	const HRESULT hr = m_pImmediateContext->Map(pStagingSource, 0, D3D11_MAP_READ, 0, &mappedSubResource);
	if (SUCCEEDED(hr)) {

		// Copy the staging texture pixels to the user buffer
		if (!bRGB) {
			// RGBA pixel buffer
			// TODO : test rgba-rgba resample
			// TODO : rgba2bgraResample
			if (width != m_Width || height != m_Height) {
				spoutcopy.rgba2rgbaResample(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, width, height, bInvert);
			}
			else {
				if(bSwap)
					spoutcopy.rgba2bgra(mappedSubResource.pData, destpixels, width, height, mappedSubResource.RowPitch, bInvert);
				else
					spoutcopy.rgba2rgba(mappedSubResource.pData, destpixels, width, height, mappedSubResource.RowPitch, bInvert);
			}
		}
		else if (m_dwFormat == 28) { // DXGI_FORMAT_R8G8B8A8_UNORM
			// RGBA texture - RGB/BGR pixel buffer
			// If the texture format is RGBA it has to be converted to BGR by the staging texture copy
			if (width != m_Width || height != m_Height) {
				if(bSwap)
					spoutcopy.rgba2rgbResample(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, width, height, bInvert);
				else
					spoutcopy.rgba2bgrResample(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, width, height, bInvert);
			}
			else {
				if (bSwap)
					spoutcopy.rgba2rgb(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, bInvert);
				else
					spoutcopy.rgba2bgr(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, bInvert);
			}
		}
		else {
			// Used for SpoutCam to receive RGB images
			if (width != m_Width || height != m_Height) {
				spoutcopy.rgba2rgbResample(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, width, height, bInvert, m_bMirror, m_bSwapRB);
			}
			else {
				// Approx 5 msec at 1920x1080
				spoutcopy.rgba2rgb(mappedSubResource.pData, destpixels, m_Width, m_Height, mappedSubResource.RowPitch, bInvert, m_bMirror, m_bSwapRB);
			}
		}

		m_pImmediateContext->Unmap(pStagingSource, 0);

		return true;

	} // endif DX11 map OK

	return false;

} // end ReadPixelData


// Create new class staging textures if changed size or do not exist yet
bool spoutDX::CheckStagingTextures(unsigned int width, unsigned int height, DWORD dwFormat)
{
	if (!m_pd3dDevice) {
		return false;
	}

	if (m_pStaging[0] && m_pStaging[1]) {

		// Get the texture details to test for change (both textures are the same)
		D3D11_TEXTURE2D_DESC desc={0};
		m_pStaging[0]->GetDesc(&desc);

		// Return if the same size and format
		if (desc.Width == width && desc.Height == height && desc.Format == (DXGI_FORMAT)dwFormat)
			return true;

		// Drop through to create new staging textures
		m_Index = 0;
		m_NextIndex = 0;

	}

	// The SpoutDirectX function releases an existing texture and checks for zero or DX9 format
	if(spoutdx.CreateDX11StagingTexture(m_pd3dDevice, width, height, (DXGI_FORMAT)dwFormat, &m_pStaging[0])
	&& spoutdx.CreateDX11StagingTexture(m_pd3dDevice, width, height, (DXGI_FORMAT)dwFormat, &m_pStaging[1])) {
		return true;
	}

	return false;
}


// Create new class texture if changed size or does not exist yet
bool spoutDX::CheckTexture(unsigned int width, unsigned int height, DWORD dwFormat)
{
	if (!m_pd3dDevice)
		return false;

	if (m_pTexture) {
		
		// Get the texture details to test for change
		D3D11_TEXTURE2D_DESC desc = { 0 };
		m_pTexture->GetDesc(&desc);
		
		// Return if the same size and format
		if (desc.Width == width && desc.Height == height && desc.Format == (DXGI_FORMAT)dwFormat)
			return true;

		// Drop through to create new texture
	}

	// The SpoutDirectX function releases an existing texture and checks for zero or DX9 format
	return spoutdx.CreateDX11Texture(m_pd3dDevice, width, height, (DXGI_FORMAT)dwFormat, &m_pTexture);

}


//
// The following functions are adapted from equivalents in SpoutSDK.cpp
// for applications not using the entire Spout SDK.
//

// Pop up SpoutPanel to allow the user to select a sender
// Usually activated by RH click
void spoutDX::SelectSenderPanel()
{
	HANDLE hMutex1 = NULL;
	HMODULE module = NULL;
	char path[MAX_PATH]={};
	char drive[MAX_PATH]={};;
	char dir[MAX_PATH]={};
	char fname[MAX_PATH]={};

	// The selected sender is then the "Active" sender and this receiver switches to it.
	// If Spout is not installed, SpoutPanel.exe has to be in the same folder
	// as this executable. This rather complicated process avoids having to use a dialog
	// which causes problems with host GUI messaging.

	// First find if there has been a Spout installation >= 2.002 with an install path for SpoutPanel.exe
	if (!ReadPathFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\SpoutPanel", "InstallPath", path)) {
		// Path not registered so find the path of the host program
		// where SpoutPanel should have been copied
		module = GetModuleHandle(NULL);
		GetModuleFileNameA(module, path, MAX_PATH);
		_splitpath_s(path, drive, MAX_PATH, dir, MAX_PATH, fname, MAX_PATH, NULL, 0);
		_makepath_s(path, MAX_PATH, drive, dir, "SpoutPanel", ".exe");
		// Does SpoutPanel.exe exist in this path ?
		if (!PathFileExistsA(path)) {
			// Try the current working directory
			if (_getcwd(path, MAX_PATH)) {
				strcat_s(path, MAX_PATH, "\\SpoutPanel.exe");
				// Does SpoutPanel exist here?
				if (!PathFileExistsA(path)) {
					SpoutLogWarning("spoutDX::SelectSender - SpoutPanel path not found");
					return;
				}
			}
		}
	}

	// Check whether the panel is already running
	// Try to open the application mutex.
	hMutex1 = OpenMutexA(MUTEX_ALL_ACCESS, 0, "SpoutPanel");
	if (!hMutex1) {
		// No mutex, so not running, so can open it

		// First release any orphaned senders if the name exists
		// in the sender list but the shared memory info does not
		// So that the sender list is clean
		sendernames.CleanSenders();

		// Use ShellExecuteEx so we can test its return value later
		ZeroMemory(&m_ShExecInfo, sizeof(m_ShExecInfo));
		m_ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
		m_ShExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
		m_ShExecInfo.hwnd = NULL;
		m_ShExecInfo.lpVerb = NULL;
		m_ShExecInfo.lpFile = (LPCSTR)path;
		m_ShExecInfo.lpDirectory = NULL;
		m_ShExecInfo.nShow = SW_SHOW;
		m_ShExecInfo.hInstApp = NULL;
		ShellExecuteExA(&m_ShExecInfo);

		//
		// The flag "m_bSpoutPanelOpened" is set here to indicate that the user
		// has opened the panel to select a sender. This flag is local to 
		// this process so will not affect any other receiver instance
		// Then when the selection panel closes, sender name is tested
		//
		m_bSpoutPanelOpened = true;
	}
	else {
		// The mutex exists, so another instance is already running.
		// Find the SpoutPanel window and bring it to the top.
		// SpoutPanel is opened as topmost anyway but pop it to
		// the front in case anything else has stolen topmost.
		HWND hWnd = FindWindowA(NULL, (LPCSTR)"SpoutPanel");
		if (hWnd && IsWindow(hWnd)) {
			SetForegroundWindow(hWnd);
			// prevent other windows from hiding the dialog
			// and open the window wherever the user clicked
			SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_ASYNCWINDOWPOS | SWP_SHOWWINDOW | SWP_NOSIZE | SWP_NOMOVE);
		}
		else if (path[0]) {
			// If the window was not found but the mutex exists
			// and SpoutPanel is installed, it has crashed.
			// Terminate the process and the mutex or the mutex will remain
			// and SpoutPanel will not be started again.
			PROCESSENTRY32 pEntry;
			pEntry.dwSize = sizeof(pEntry);
			bool done = false;
			// Take a snapshot of all processes and threads in the system
			HANDLE hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPALL, NULL);
			if (hProcessSnap == INVALID_HANDLE_VALUE) {
				SpoutLogError("spoutDX::OpenSpoutPanel - CreateToolhelp32Snapshot error");
			}
			else {
				// Retrieve information about the first process
				BOOL hRes = Process32First(hProcessSnap, &pEntry);
				if (!hRes) {
					SpoutLogError("spoutDX::OpenSpoutPanel - Process32First error");
					CloseHandle(hProcessSnap);
				}
				else {
					// Look through all processes
					while (hRes && !done) {
						const int value = _tcsicmp(pEntry.szExeFile, _T("SpoutPanel.exe"));
						if (value == 0) {
							HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, 0, (DWORD)pEntry.th32ProcessID);
							if (hProcess != NULL) {
								// Terminate SpoutPanel and it's mutex
								TerminateProcess(hProcess, 9);
								CloseHandle(hProcess);
								done = true;
							}
						}
						if (!done)
							hRes = Process32Next(hProcessSnap, &pEntry); // Get the next process
						else
							hRes = NULL; // found SpoutPanel
					}
					CloseHandle(hProcessSnap);
				}
			}
			// Now SpoutPanel will start the next time the user activates it
		} // endif SpoutPanel crashed
	} // endif SpoutPanel already open

	// If we opened the mutex, close it now or it is never released
	if (hMutex1) CloseHandle(hMutex1);

	return;

} // end SelectSenderPanel

//
// Check whether SpoutPanel opened and return the new sender name
//
bool spoutDX::CheckSpoutPanel(char *sendername, int maxchars)
{
	// If SpoutPanel has been activated, test if the user has clicked OK
	if (m_bSpoutPanelOpened) { // User has activated spout panel

		SharedTextureInfo TextureInfo={};
		HANDLE hMutex = NULL;
		DWORD dwExitCode = 0;
		char newname[256]={};
		bool bRet = false;

		// Must find the mutex to signify that SpoutPanel has opened
		// and then wait for the mutex to close
		hMutex = OpenMutexA(MUTEX_ALL_ACCESS, 0, "SpoutPanel");

		// Has it been activated 
		if (!m_bSpoutPanelActive) {
			// If the mutex has been found, set the active flag true and quit
			// otherwise on the next round it will test for the mutex closed
			if (hMutex) m_bSpoutPanelActive = true;
		}
		else if (!hMutex) { // It has now closed
			m_bSpoutPanelOpened = false; // Don't do this part again
			m_bSpoutPanelActive = false;
			// call GetExitCodeProcess() with the hProcess member of
			// global SHELLEXECUTEINFO to get the exit code from SpoutPanel
			if (m_ShExecInfo.hProcess) {
				GetExitCodeProcess(m_ShExecInfo.hProcess, &dwExitCode);
				// Only act if exit code = 0 (OK)
				if (dwExitCode == 0) {
					// SpoutPanel has been activated and OK clicked
					// Test the active sender which should have been set by SpoutPanel
					newname[0] = 0;
					if (!sendernames.GetActiveSender(newname)) {
						// Otherwise the sender might not be registered.
						// SpoutPanel always writes the selected sender name to the registry.
						if (ReadPathFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\SpoutPanel", "Sendername", newname)) {
							// Register the sender if it exists
							if (newname[0] != 0) {
								if (sendernames.getSharedInfo(newname, &TextureInfo)) {
									// Register in the list of senders and make it the active sender
									sendernames.RegisterSenderName(newname);
									sendernames.SetActiveSender(newname);
								}
							}
						}
					}
					// Now do we have a valid sender name ?
					if (newname[0] != 0) {
						// Pass back the new name
						strcpy_s(sendername, maxchars, newname);
						bRet = true;
					} // endif valid sender name
				} // endif SpoutPanel OK
			} // got the exit code
		} // endif no mutex so SpoutPanel has closed
		// If we opened the mutex, close it now or it is never released
		if (hMutex) CloseHandle(hMutex);
		return bRet;
	} // SpoutPanel has not been opened

	return false;

}
