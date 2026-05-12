//
//
//			spoutDirectX.cpp
//
//		Functions to manage DirectX11 texture sharing
//
// ====================================================================================
//		Revisions :
//
//		22.07.14	- added option for DX9 or DX11
//		21.09.14	- included keyed mutex texture access lock in CreateSharedDX11Texture
//		23.09.14	- moved general mutex texture access lock to this class
//		23.09.14	- added DX11available() to verify operating system support for DirectX 11
//		15.10.14	- added debugging aid for texture access locks
//		17.10.14	- flush before release immediate context in CloseDX11
//		21.10.14	- removed keyed mutex lock due to reported driver problems
//					  TODO - cleanup all functions using it
//		10.02.15	- removed functions relating to DirectX 11 keyed mutex lock
//		14.02.15	- added UNREFERENCED_PARAMETER(pSharedTexture) to CheckAceess and AllowAccess
//		29.05.15	- Included SetAdapter for multiple adapters - Franz Hildgen.
//		02.06.15	- Added GetAdapter, GetNumAdapters, GetAdapterName
//		08.06.15	- removed dx9 flag from setadapter
//		04.08.15	- cleanup
//		11.08.15	- removed GetAdapterName return if Intel. For use with Intel HD4400/5000 graphics
//		22.10.15	- removed DX11available and shifted it to the interop class
//		19.11.15	- fixed return value in CreateDX9device after caps error (was false instead of NULL)
//		20.11.15	- Registry read/write moved from SpoutGLDXinterop class
//		16.02.16	- IDXGIFactory release - from https://github.com/jossgray/Spout2
//		29.02.16	- cleanup
//		05.04.16	- removed unused texture pointer from mutex access functions
//		16.06.16	- fixed null device release in SetAdapter - https://github.com/leadedge/Spout2/issues/17
//		01.07.16	- restored hFocusWindow in CreateDX9device (was set to NULL for testing)
//		04.09.16	- Add create DX11 staging texture
//		16.01.17	- Add WriteDX9surface
//		23.01.17	- pEventQuery->Release() for writeDX9surface
//		02.06.17	- Registry functions moved to SpoutUtils
//					- Added Spout error log console output
//		17.03.18	- More error log notices
//					- protect against an adapter with no outputs for SetAdapter
//		16.06.18	- change all class variable name prefix from g_ to m_
//					  Add GetImmediateContext();
//					- Add ReleaseDX11Texture and ReleaseDX11Device
//		13.11.18	- Remove staging texture functons
//		16.12.18	- Move FlushWait from interop class
//		03.01.19	- Changed to revised registry functions in SpoutUtils
//		27.06.19	- Restored release of existing texture in CreateSharedDX11Texture
//		03.07.19	- Added pointer checks in OpenDX11shareHandle
//		18.09.19	- Changed initial log from notice to to verbose
//					  for CreateSharedDX9Texture and CreateSharedDX11Texture
//		08.11.19	- removed immediate context check from OpenDX11shareHandle
//		15.03.20	- allow for zero or DX9 format passed in to CreateSharedDX11Texture for utility use
//		16.06.20	- Create separate Wait function
//					- Update comments
//		02.09.20	- CreateSharedDX11Texture - test for null pointer to the shared texture pointer
//		03.09.20	- ReleaseDX11Texture 
//					    add log warnings for null device and texture
//					    add DirectX messages to Output window for debug build
//		06.09.20	- Add output test to SetAdapter
//		08.09.20	- Release all pointers in adapter functions
//					  Remove failures if no adapter output pending testing
//					  Add immediate context test before flush in ReleaseDX11Texture
//					  In case the function is used by a different device.
//		12.09.20	- Re-introduced Optimus Enablement to enforce NVidia Optimus
//					  Incuding AMD Enduro technology
//					  Credit to https://github.com/Qlex42
//		13.09.20	- Remove Optimus enablement again due to use of extern "C"
//		15.09.20	- Changed all result !=S_OK and !=D3D_OK to FAILED macro for consistency
//					  Correct type cast in CreateDX9device and GetAdapterName
//		16.09.20	- ReleaseDX11Texture - removed log notice for no reference count
//		19.09.20	- Add success notice for SetAdapter
//					  Add DebugLog function
//					  Set passed pointer to null in ReleaseDX11Texture
//					  Clean up comments and logs throughtout
//		21.09.20	- Format specifiers for hex print
//					  SetAdapter - corrected logs
//		23.09.20	- Change warning logs to error in OpenDX11shareHandle
//		24.09.20	- Change all pointer "= NULL to "= nullptr"
//					  Change hex printf to 0x%8.8llX
//					- Introduce try/catch to OpenDX11shareHandle
//					  for the possibility of different graphics adapters
//					- Corrected compare of different enum types in CreateSharedDX11Texture
//		25.09.20	- Made GetAdapterPointer public
//					  Add SetAdapterPointer
//		08.10.20	- Re-introduced CreateDX11StagingTexture
//		09.10.20	- Moved DX9 functions to a separate class
//					  SetAdapter - only DX11 test supported
//					  Add GetDX11device
//					  Change GetImmediateContext() to GetDX11Context()
//		23.11.20	- Protect against null in GetAdapterName
//		09.12.20	- CloseDirectX11() in destructor
//		27.12.20	- Change all hex printf to 0x%.7X with PtrToUint helper
//		20.02.21	- Add zero width/height check to CreateDX11Texture
//		19.06.21	- Remove output check from FindNVIDIA
//		20.11.21	- Correctly release pOutput in GetNumAdapters()
//		21.11.21	- Remove CloseDirectX11 from destructor
//					  Clean up adapter functions
//		23.11.21	- Remove GetImmediateContext after device create
//					  Allow use of external device or class device
//		11.12.21	- Add ReleaseDX11Texture overload to use class device
//					  Allow use of external device in OpenDirectX11
//					  Revise OpenDirectX11() for external device
//		14.12.21	- OpenDirectX11 default nullptr argument in header
//		16.12.21	- Allow use of external context in OpenDirectX11
//		17.12.21	- Use passed texture pointer directly in CreateDX11Texture
//		26.12.21	- Context flush after texture release in ReleaseDX11Texture
//					  Clean up DebugLog function
//		25.01.22	- Correct log notice in ReleaseDX11Texture to show texture instead of device
//					  Move adapter pointer release from release device to destructor
//		16.03.22	- Remove unused OutputDebugString from ReleaseDX11Device
// 		29.03.22	- CreateDX11Texture, CreateSharedDX11Texture, CreateDX11StagingTexture
//					  Switch on HRESULT instead of LOWORD so that DXGI cases are recognised 
//		07.04.22	- CreateDX11Texture - cast to int the LOWORD from hresult for error report
//		15.05.22	- CreateSharedDX11Texture change Fatal logs to Warning
//		26.10.22	- Correct GetAdapterIndex maximum size for wchar convert
//		28.10.22	- Add GetPerformancePreference, SetPerformancePreference, GetPreferredAdapterName
//					  Code cleanup and documentation
//		01.11.22	- Add SetPreferredAdapter
//		02.11.22	- Add IsPreferenceAvailable to detect availability
//					  of Windows Graphics Performance settings.
//		04.11.22	- Change GetAdapterIndex argument from char* to to const char*
//					  Correct to return -1 on failure instead of "false"
//		07.11.22	- Add IsApplicationPath
//		01.12.22	- GetPerformancePreference change logs to warning
//		03.12.22	- Avoid use of "description" char array due to definition in SpoutSenderNames.h
//		17.12.22	- Corrections for code analysis
//		18.12.22	- Catch any exception by using Release in destructor
//					  Use std::string for error text in CreateDX11StagingTexture / CreateSharedDX11Texture
//		22.12.22	- Compiler compatibility check
//					  Change all {} initializations to "={}"
//		31.12.22	- SpoutDirectX.h
//					   Remove WDK_NTDDI_VERSION in case it's not defined
//					   Add comments concerning use of dxgi_6 with older DirectX SDK.
//		06.01.23	- Correct IsPreferenceAvailable() to pass array length to registry function
//		08.01.23	- CreateSharedDX11Texture - option for keyed shared texture
//		18.03.23	- CreateDX11StagingTexture - use default DX11 format for zero or DX9 formats
//
// ====================================================================================
/*

	Copyright (c) 2014-2023. Lynn Jarvis. All rights reserved.

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

#include "SpoutDirectX.h"

//
// Class: spoutDirectX
//
// Functions to manage DirectX11 texture sharing.
//
// Refer to source code for documentation.
//

spoutDirectX::spoutDirectX() {

	// DX11
	m_pd3dDevice        = nullptr;
	m_pImmediateContext = nullptr;
	m_bClassDevice		= true;
	m_driverType		= D3D_DRIVER_TYPE_NULL;
	m_featureLevel		= D3D_FEATURE_LEVEL_11_0;

	// Output graphics adapter
	// Programmer can set for an application
	m_AdapterIndex  = 0; // Adapter index
	m_pAdapterDX11  = nullptr; // DX11 adapter pointer
}

spoutDirectX::~spoutDirectX() {

	try {
		// Release adapter pointer if specified by SetAdapter
		if (m_pAdapterDX11) m_pAdapterDX11->Release();
	}
	catch (...) {
		MessageBoxA(NULL, "Exception in spoutDriectX destructor", NULL, MB_OK);
	}

}

//
// Group: DirectX11 device
//

//---------------------------------------------------------
// Function: OpenDirectX11
// Initialize and prepare Directx 11
// Retain a class device and context
bool spoutDirectX::OpenDirectX11(ID3D11Device* pDevice)
{
	// Quit if already initialized
	// Can be set externally (See also - SetDX11device)
	if (m_pd3dDevice) {
		if (m_bClassDevice) {
			SpoutLogNotice("spoutDirectX::OpenDirectX11(0x%.7X) - Class device already initialized", PtrToUint(m_pd3dDevice));
		}
		else {
			SpoutLogNotice("spoutDirectX::OpenDirectX11(0x%.7X) - External device used", PtrToUint(m_pd3dDevice));
		}
		return true;
	}

	// Create a DirectX 11 device
	SpoutLogNotice("spoutDirectX::OpenDirectX11");
	
	// Use the external device if one was passed in
	if (pDevice) {
		m_pd3dDevice = pDevice;
		// Device was created outside this class
		m_bClassDevice = false;
		// Retrieve the context pointer independently
		// For a class device it is created in CreateDX11device().
		m_pd3dDevice->GetImmediateContext(&m_pImmediateContext);
	}
	else {
		// Create a class device if none was passed in
		// m_pImmediateContext is also created
		m_pd3dDevice = CreateDX11device();
		// Device was created within this class
		m_bClassDevice = true;
	}

	if (!m_pd3dDevice) {
		SpoutLogNotice("    Could not create device\n");
		return false;
	}

	return true;

}

//---------------------------------------------------------
// Function: CloseDirectX11
// Release DirectX 11 device and context
void spoutDirectX::CloseDirectX11()
{
	// Quit if already released
	if (!m_pd3dDevice) {
		SpoutLogNotice("spoutDirectX::CloseDirectX11() - device already released");
		return;
	}

	if (m_bClassDevice) {
		// A device was created within this class (CreateDX11device)
		SpoutLogNotice("spoutDirectX::CloseDirectX11(0x%.7X)", PtrToUint(m_pd3dDevice));
		// Release class device, context and adapter pointer
		if(m_pd3dDevice)
			ReleaseDX11Device(m_pd3dDevice);
		m_pd3dDevice = nullptr;
		m_pImmediateContext = nullptr;
		m_bClassDevice = false; // Device is closed, do not release again
	}
	else {
		// An application device was used (SetDX11Device). Do not release it.
		SpoutLogNotice("spoutDirectX::CloseDirectX11 - external device used (0x%.7X)", PtrToUint(m_pd3dDevice));
		// Release adapter pointer if specified by SetAdapter
		// Normally done in destructor
		if (m_pAdapterDX11)
			m_pAdapterDX11->Release();
		m_pAdapterDX11 = nullptr;
		// Release m_pImmediateContext if created
		if(m_pImmediateContext) {
			m_pImmediateContext->Flush();
			m_pImmediateContext->Release();
		}
		m_pImmediateContext = nullptr;
	}
}

//---------------------------------------------------------
// Function: SetDX11Device
// Set the class device
bool spoutDirectX::SetDX11Device(ID3D11Device* pDevice)
{
	// Quit if already initialized
	if (m_pd3dDevice) {
		SpoutLogNotice("spoutDirectX::SetDX11Device(0x%.7X) - Device already initialized", PtrToUint(m_pd3dDevice));
		return false;
	}

	SpoutLogNotice("spoutDirectX::SetDX11Device(0x%.7X)", PtrToUint(pDevice));
	
	// Set to use the application device
	m_pd3dDevice = pDevice;
	m_bClassDevice = false; // Signal an external device

	// Retrieve the context pointer independently.
	// For a class device it is created in CreateDX11device().
	m_pd3dDevice->GetImmediateContext(&m_pImmediateContext);

	return true;
}


//
// Notes for DX11 : https://www.khronos.org/registry/OpenGL/extensions/NV/WGL_NV_DX_interop2.txt
//
// Valid device types for the <dxDevice> parameter of wglDXOpenDeviceNV and associated restrictions
// DirectX device type ID3D11Device - can only be used on WDDM operating systems; XXX Must be multithreaded
// TEXTURE_2D - ID3D11Texture2D - Usage flags must be D3D11_USAGE_DEFAULT
// wglDXSetResourceShareHandle does not need to be called for DirectX version
// 10 and 11 resources. Calling this function for DirectX 10 and 11 resources
// is not an error but has no effect.

//---------------------------------------------------------
// Function: CreateDX11Device
// Create DX11 device
//
// https://github.com/walbourn/directx-sdk-samples/tree/main/Direct3D11Tutorials/Tutorial04
//
ID3D11Device* spoutDirectX::CreateDX11device()
{
	ID3D11Device* pd3dDevice = nullptr;
	HRESULT hr = S_OK;
	const UINT createDeviceFlags = 0;

	// Adapter pointer null by default
	// or specified by SetAdapter or SetAdapterPointer
	IDXGIAdapter* pAdapterDX11 = m_pAdapterDX11;

	if (pAdapterDX11) {
		SpoutLogNotice("spoutDirectX::CreateDX11device - specified adapter %d, pAdapter (0x%.7X)", m_AdapterIndex, PtrToUint(pAdapterDX11));
	}
	else {
		SpoutLogNotice("spoutDirectX::CreateDX11device - default adapter");
	}
	
	//
	// If the project is in a debug build, enable debugging via SDK Layers with this flag.
	// https://docs.microsoft.com/en-us/windows/win32/api/d3d11/ne-d3d11-d3d11_create_device_flag
	// To use this flag, you must have D3D11_1SDKLayers.dll installed or device creation fails.
	// To resolve this you can install the Windows 10 SDK.
	// 
	// Due to this dependency problem, you have to manually remove the comments below 
	// to enable it once you have installed D3D11_1SDKLayers.dll.
	// See also : void spoutDirectX::DebugLog
	//

/*
#if defined(_DEBUG)
	createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
#endif
*/

	// GL/DX interop Spec
	// ID3D11Device can only be used on WDDM operating systems : Must be multithreaded
	// D3D11_CREATE_DEVICE_FLAG createDeviceFlags
	const D3D_DRIVER_TYPE driverTypes[] =	{
		D3D_DRIVER_TYPE_HARDWARE,
		D3D_DRIVER_TYPE_WARP,
		D3D_DRIVER_TYPE_REFERENCE,
	};

	const UINT numDriverTypes = ARRAYSIZE( driverTypes );

	// These are the feature levels that we will accept.
	// m_featureLevel is the feature level used
	// 11.0 is the highest level currently supported for Spout
	// because 11.1 limits compatibility
	// Note from D3D11 Walbourn examples :
	//	DirectX 11.0 platforms will not recognize D3D_FEATURE_LEVEL_11_1
	// Note from NVIDIA forums :
	//  Not all DirectX 11.1 features are software features.
	//  Target Independent Rasterization requires hardware support
	//  so we can not make DX11 GPUs fully DX11.1 complaint.
	const D3D_FEATURE_LEVEL featureLevels[] =	{
		// D3D_FEATURE_LEVEL_11_1, // 0xb001
		D3D_FEATURE_LEVEL_11_0, // 0xb000
		D3D_FEATURE_LEVEL_10_1, // 0xa100
		D3D_FEATURE_LEVEL_10_0, // 0xa000
	};

	const UINT numFeatureLevels = ARRAYSIZE( featureLevels );

	// To allow for multiple graphics cards we will use m_pAdapterDX11
	// Which is set by SetAdapter before initializing DirectX
	if(pAdapterDX11) {
			hr = D3D11CreateDevice( pAdapterDX11,
									D3D_DRIVER_TYPE_UNKNOWN,
									NULL,
									createDeviceFlags,
									featureLevels,
									numFeatureLevels,
									D3D11_SDK_VERSION,
									&pd3dDevice,
									&m_featureLevel,
									&m_pImmediateContext );

	} // endif adapter set
	else {
		
		// Possible Optimus problem : is the default adapter (NULL) always Intel ?
		// https://msdn.microsoft.com/en-us/library/windows/desktop/ff476082%28v=vs.85%29.aspx
		// pAdapter : a pointer to the video adapter to use when creating a device. 
		// Pass NULL to use the default adapter, which is the first adapter that is
		// enumerated by IDXGIFactory1::EnumAdapters. 
		// http://www.gamedev.net/topic/645920-d3d11createdevice-returns-wrong-feature-level/
		// Source : https://github.com/walbourn/directx-sdk-samples/blob/main/Direct3D11Tutorials/Tutorial01/Tutorial01.cpp
		for( UINT driverTypeIndex = 0; driverTypeIndex < numDriverTypes; driverTypeIndex++ ) {

			// First driver type is D3D_DRIVER_TYPE_HARDWARE which should pass
			m_driverType = driverTypes[driverTypeIndex];

			hr = D3D11CreateDevice(	NULL,
									m_driverType,
									NULL,
									createDeviceFlags,
									featureLevels,
									numFeatureLevels,
									D3D11_SDK_VERSION, 
									&pd3dDevice,
									&m_featureLevel,
									&m_pImmediateContext);

			// Break as soon as something passes
			if(SUCCEEDED(hr))
				break;
		}

	} // endif no adapter set
	
	// Quit if nothing worked
	if (FAILED(hr)) {
		SpoutLogFatal("spoutDirectX::CreateDX11device NULL device");
		return NULL;
	}

	// All OK - return the device pointer to the caller
	// m_pImmediateContext has also been created
	SpoutLogNotice("    Device (0x%.7X) - Context (0x%.7X)", PtrToUint(pd3dDevice), PtrToUint(m_pImmediateContext));

	return pd3dDevice;

} // end CreateDX11device

//---------------------------------------------------------
// Function: GetDX11Device
// Return the class device
ID3D11Device* spoutDirectX::GetDX11Device()
{
	return m_pd3dDevice;
}

// Return the device immediate context
ID3D11DeviceContext* spoutDirectX::GetDX11Context()
{
	return m_pImmediateContext;
}

//
// Group: DirectX11 texture
//

//---------------------------------------------------------
// Function: CreateSharedDX11Texture
// Create a DirectX11 shared texture
bool spoutDirectX::CreateSharedDX11Texture(ID3D11Device* pd3dDevice, 
											unsigned int width, 
											unsigned int height, 
											DXGI_FORMAT format, 
											ID3D11Texture2D** ppSharedTexture,
											HANDLE &dxShareHandle,
											bool bKeyed)
{
	if (!pd3dDevice) {
		SpoutLogWarning("spoutDirectX::CreateSharedDX11Texture NULL device");
		return false;
	}

	if (!ppSharedTexture) {
		SpoutLogWarning("spoutDirectX::CreateSharedDX11Texture NULL ppSharedTexture");
		return false;
	}
	
	//
	// Create a new shared DX11 texture
	//

	// Release the texture if it already exists
	if (*ppSharedTexture) {
		ReleaseDX11Texture(pd3dDevice, *ppSharedTexture);
	}

	SpoutLogNotice("spoutDirectX::CreateSharedDX11Texture");
	SpoutLogNotice("    pDevice = 0x%.7X, width = %d, height = %d, format = %d", PtrToUint(pd3dDevice), width, height, format);

	// Use the format passed in
	// If that is zero or DX9 format, use the default format
	DXGI_FORMAT texformat = format;
	if (format == 0 || format == 21 || format == 22) // D3DFMT_A8R8G8B8 = 21 D3DFMT_X8R8G8B8 = 22
		texformat = DXGI_FORMAT_B8G8R8A8_UNORM;

	// Textures being shared from D3D9 to D3D11 have the following restrictions (LJ - D3D11 to D3D9 ?).
	//		Textures must be 2D
	//		Only 1 mip level is allowed
	//		Texture must have default usage
	//		Texture must be write only	- ?? LJ ??
	//		MSAA textures are not allowed
	//		Bind flags must have SHADER_RESOURCE and RENDER_TARGET set
	//		Only R10G10B10A2_UNORM, R16G16B16A16_FLOAT and R8G8B8A8_UNORM formats are allowed - ?? LJ ??
	//		If a shared texture is updated on one device ID3D11DeviceContext::Flush must be called on that device **

	// http://msdn.microsoft.com/en-us/library/windows/desktop/ff476903%28v=vs.85%29.aspx
	// To share a resource between two Direct3D 11 devices the resource must have been created
	// with the D3D11_RESOURCE_MISC_SHARED flag, if it was created using the ID3D11Device interface.
	//
	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width				= width;
	desc.Height				= height;
	desc.BindFlags			= D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE;
	// This texture will be shared
	// Note that a DirectX 11 texture with D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX is not
	// compatible with DirectX 9. If compatibility is required, a general named mutex
	// should be used for all texture types. This is the default.
	if(bKeyed)
		desc.MiscFlags = D3D11_RESOURCE_MISC_SHARED_KEYEDMUTEX;
	else
		desc.MiscFlags = D3D11_RESOURCE_MISC_SHARED;
	desc.CPUAccessFlags		= 0;	
	desc.Format				= texformat;
	desc.Usage				= D3D11_USAGE_DEFAULT;
	// Multisampling quality and count
	// The default sampler mode, with no anti-aliasing, has a count of 1 and a quality level of 0.
	desc.SampleDesc.Quality = 0;
	desc.SampleDesc.Count	= 1;
	desc.MipLevels			= 1;
	desc.ArraySize			= 1;

	const HRESULT res = pd3dDevice->CreateTexture2D(&desc, NULL, ppSharedTexture);
	if (FAILED(res)) {
		const long long lres = static_cast<long long>((LOWORD(res)));
		// http://msdn.microsoft.com/en-us/library/windows/desktop/ff476174%28v=vs.85%29.aspx
		std::string str = "spoutDirectX::CreateSharedDX11Texture ERROR -[";
		str += std::to_string(lres); str += "] : ";
		switch (res) {
			case DXGI_ERROR_INVALID_CALL:
				str += "DXGI_ERROR_INVALID_CALL";
				break;
			case E_INVALIDARG:
				str += "E_INVALIDARG";
				break;
			case E_OUTOFMEMORY:
				str += "E_OUTOFMEMORY";
				break;
			default :
				str += "Unlisted error";
				break;
		}
		SpoutLogError("%s", str.c_str());
		return false;
	}

	// The DX11 texture is created OK
	// Get the texture share handle so it can be saved in shared memory for receivers to pick up.
	// When sharing a resource between two Direct3D 10/11 devices the unique handle 
	// of the resource can be obtained by querying the resource for the IDXGIResource 
	// interface and then calling GetSharedHandle.
	ID3D11Texture2D* pTexture = *ppSharedTexture;
	IDXGIResource* pOtherResource(NULL);
	if (pTexture) {
		if (FAILED(pTexture->QueryInterface(__uuidof(IDXGIResource), (void**)&pOtherResource))) {
			SpoutLogWarning("spoutDirectX::CreateSharedDX11Texture - QueryInterface error");
			return false;
		}
	}
	if (!pOtherResource) {
		SpoutLogWarning("spoutDirectX::CreateSharedDX11Texture - QueryInterface error");
		return false;
	}

	// Return the shared texture handle
	pOtherResource->GetSharedHandle(&dxShareHandle);
	pOtherResource->Release();
	pOtherResource = nullptr;
	pTexture = nullptr;

	SpoutLogNotice("    pTexture = [0x%8.8X] : dxShareHandle = [0x%8.8X]", PtrToUint(*ppSharedTexture), LOWORD(dxShareHandle) );

	return true;

}

//---------------------------------------------------------
// Function: CreateDX11Texture
// Create a DircetX texture which is not shared
bool spoutDirectX::CreateDX11Texture(ID3D11Device* pd3dDevice, 
	unsigned int width, unsigned int height,
	DXGI_FORMAT format,	ID3D11Texture2D** ppTexture)
{
	if (width == 0 || height == 0)
		return false;

	if (!pd3dDevice) {
		SpoutLogFatal("spoutDirectX::CreateDX11Texture NULL device");
		return false;
	}

	if (!ppTexture) {
		SpoutLogWarning("spoutDirectX::CreateDX11Texture NULL ppTexture");
		return false;
	}

	SpoutLogNotice("spoutDirectX::CreateDX11Texture(0x%.X, %d, %d, %d)",
		PtrToUint(pd3dDevice), width, height, format);

	if (*ppTexture)
		ReleaseDX11Texture(pd3dDevice, *ppTexture);
	
	// Use the format passed in
	DXGI_FORMAT texformat = format;
	// If that is zero or DX9 format, use the default format
	if (format == 0 || format == 21 || format == 22) // D3DFMT_A8R8G8B8 = 21 D3DFMT_X8R8G8B8 = 22
		texformat = DXGI_FORMAT_B8G8R8A8_UNORM;

	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = width;
	desc.Height = height;
	desc.BindFlags = D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE;
	desc.CPUAccessFlags = 0;
	desc.Format = texformat;
	desc.Usage = D3D11_USAGE_DEFAULT;
	desc.SampleDesc.Quality = 0;
	desc.SampleDesc.Count = 1;
	desc.MipLevels = 1;
	desc.ArraySize = 1;

	HRESULT res = 0;
	res = pd3dDevice->CreateTexture2D(&desc, NULL, ppTexture);

	if (FAILED(res)) {
		char tmp[256]={};
		const int error = static_cast<int>((LOWORD(res)));
		sprintf_s(tmp, 256, "spoutDirectX::CreateDX11Texture ERROR - %d (0x%.X) : ", error, error);
		switch (res) {
		case DXGI_ERROR_INVALID_CALL:
			strcat_s(tmp, 256, "DXGI_ERROR_INVALID_CALL");
			break;
		case E_INVALIDARG:
			strcat_s(tmp, 256, "E_INVALIDARG");
			break;
		case E_OUTOFMEMORY:
			strcat_s(tmp, 256, "E_OUTOFMEMORY");
			break;
		default:
			strcat_s(tmp, 256, "Unlisted error");
			break;
		}
		SpoutLogFatal("%s", tmp);
		return false;
	}
	
	return true;

}

//---------------------------------------------------------
// Function: CreateDX11StagingTexture
// Create a DirectX 11 staging texture for read and write
bool spoutDirectX::CreateDX11StagingTexture(ID3D11Device* pd3dDevice,
	unsigned int width,	unsigned int height, DXGI_FORMAT format, ID3D11Texture2D** ppStagingTexture)
{
	if (pd3dDevice == NULL || !ppStagingTexture)
		return false;

	SpoutLogNotice("spoutDirectX::CreateDX11StagingTexture(0x%.X, %d, %d, %d)",
		PtrToUint(pd3dDevice), width, height, format);

	// Release the texture if it already exists
	if (*ppStagingTexture) {
		ReleaseDX11Texture(pd3dDevice, *ppStagingTexture);
	}

	ID3D11Texture2D* pTexture = nullptr; // The new texture pointer

	// Use the format passed in
	DXGI_FORMAT texformat = format;
	// If that is zero or DX9 format, use the default format
	if (format == 0 || format == 21 || format == 22) // D3DFMT_A8R8G8B8 = 21 D3DFMT_X8R8G8B8 = 22
		texformat = DXGI_FORMAT_B8G8R8A8_UNORM;

	D3D11_TEXTURE2D_DESC desc;
	ZeroMemory(&desc, sizeof(desc));
	desc.Width = width;
	desc.Height = height;
	desc.MipLevels = 1;
	desc.ArraySize = 1;
	desc.Format = texformat;
	desc.SampleDesc.Count = 1;
	desc.CPUAccessFlags = D3D11_CPU_ACCESS_READ | D3D11_CPU_ACCESS_WRITE;
	desc.Usage = D3D11_USAGE_STAGING;
	desc.BindFlags = 0;

	const HRESULT res = pd3dDevice->CreateTexture2D(&desc, NULL, &pTexture);
	if (res != S_OK) {
		const long long lres = static_cast<long long>((LOWORD(res)));
		// http://msdn.microsoft.com/en-us/library/windows/desktop/ff476174%28v=vs.85%29.aspx
		std::string str = "spoutDirectX::CreateDX11StagingTexture ERROR -[";
		str += std::to_string(lres); str += "] : ";
		switch (res) {
		case DXGI_ERROR_INVALID_CALL:
			str += "DXGI_ERROR_INVALID_CALL";
			break;
		case E_INVALIDARG:
			str += "E_INVALIDARG";
			break;
		case E_OUTOFMEMORY:
			str += "E_OUTOFMEMORY";
			break;
		default:
			str += "Unlisted error";
			break;
		}
		SpoutLogFatal("%s", str.c_str());
		return false;
	}

	*ppStagingTexture = pTexture;

	SpoutLogNotice("    pTexture = 0x%.7X", PtrToUint(pTexture));

	return true;

}

//---------------------------------------------------------
// Function: OpenDX11shareHandle
// Retrieve the pointer of a DirectX11 shared texture
bool spoutDirectX::OpenDX11shareHandle(ID3D11Device* pDevice, ID3D11Texture2D** ppSharedTexture, HANDLE dxShareHandle)
{

	if (!pDevice || !ppSharedTexture || !dxShareHandle) {
		SpoutLogError("spoutDirectX::OpenDX11shareHandle - null sources");
		return false;
	}

	// To share a resource between a Direct3D 9 device and a Direct3D 11 device 
	// the texture must have been created using the pSharedHandle argument of CreateTexture.
	// The shared Direct3D 9 handle is then passed to OpenSharedResource in the hResource argument.
	//
	// Note that the resource created for use on this device must be eventually freed or there is a leak.
	//
	// This can crash if the share handle has been created using a different graphics adapter
	try {
		const HRESULT hr = pDevice->OpenSharedResource(dxShareHandle, __uuidof(ID3D11Resource), (void**)(ppSharedTexture));
		if (FAILED(hr)) {
			// Error 87 (0x75) - E_INVALIDARG
			SpoutLogError("spoutDirectX::OpenDX11shareHandle (0x%.7X) failed : error = %d (0x%.7X)", LOWORD(dxShareHandle), LOWORD(hr), LOWORD(hr) );
			return false;
		}
	}
	catch (...) {
		// Catch any exception
		SpoutLogError("spoutDirectX::OpenDX11shareHandle - exception opening share handle");
		return false;
	}

	// Can get sender format here
	// ID3D11Texture2D * texturePointer = *ppSharedTexture;
	// D3D11_TEXTURE2D_DESC td;
	// texturePointer->GetDesc(&td);
	// printf("td.Format = %d\n", td.Format); // 87
	// printf("td.Width = %d\n", td.Width);
	// printf("td.Height = %d\n", td.Height);
	// printf("td.MipLevels = %d\n", td.MipLevels);
	// printf("td.Usage = %d\n", td.Usage);
	// printf("td.ArraySize = %d\n", td.ArraySize);
	// printf("td.SampleDesc = %d\n", td.SampleDesc);
	// printf("td.BindFlags = %d\n", td.BindFlags);
	// printf("td.MiscFlags = %d\n", td.MiscFlags); // D3D11_RESOURCE_MISC_SHARED

	return true;

}

//
// Group: DirectX11 utiities
//

//---------------------------------------------------------
// Function: ReleaseDX11Texture
// Release a texture resource created with a class device
unsigned long spoutDirectX::ReleaseDX11Texture(ID3D11Texture2D* pTexture)
{
	return ReleaseDX11Texture(m_pd3dDevice, pTexture);
}

//---------------------------------------------------------
// Function: ReleaseDX11Texture
// Release a texture resource
unsigned long spoutDirectX::ReleaseDX11Texture(const ID3D11Device* pd3dDevice, ID3D11Texture2D* pTexture)
{

	if (!pd3dDevice || !pTexture) {
		if (!pd3dDevice)
			SpoutLogWarning("spoutDirectX::ReleaseDX11Texture - no device");
		if (!pTexture)
			SpoutLogWarning("spoutDirectX::ReleaseDX11Texture - no texture");
		return 0;
	}

	SpoutLogNotice("spoutDirectX::ReleaseDX11Texture (0x%.7X, 0x%.7X)", PtrToUint(pd3dDevice), PtrToUint(pTexture));
	const unsigned long refcount = pTexture->Release();
	pTexture = nullptr;

	// The device will be live, so warn if refcount > 1
	if (refcount > 1) {
		SpoutLogWarning("spoutDirectX::ReleaseDX11Texture - refcount = %lu", refcount);
		DebugLog(pd3dDevice, "spoutDirectX::ReleaseDX11Texture - refcount = %lu\n", refcount);
		// printf("spoutDirectX::ReleaseDX11Texture - refcount = %lu\n", refcount);
	}

	// Calling Flush will destroy any objects whose destruction has been deferred.
	if (m_pImmediateContext)
		m_pImmediateContext->Flush();

	// Note that if the texture is registered and linked to OpenGL using the 
	// GL/DX interop, the interop must be unregistered or the texture is not
	// released even though the reference count reported here does not increase.

	return refcount;
}

//---------------------------------------------------------
// Function: ReleaseDX11Device
// Release a device
unsigned long spoutDirectX::ReleaseDX11Device(ID3D11Device* pd3dDevice)
{
	if (!pd3dDevice)
		return 0;

	// Release the global context or there is an outstanding ref count
	// when the device is released
	if (m_pImmediateContext) {
		// Flush context to prevent deferred device release
		m_pImmediateContext->Flush();
		// Release the context
		m_pImmediateContext->Release();
		m_pImmediateContext = nullptr;
	}

	const unsigned long refcount = pd3dDevice->Release();

	// Use this for debugging if you have D3D11_1SDKLayers.dll installed.
	// See CreateDX11device.
/*
#ifdef _DEBUG
	// OutputDebugStringA(tmp);
	ID3D11Debug* DebugDevice = nullptr;
	if (m_pd3dDevice->QueryInterface(__uuidof(ID3D11Debug), (void**)&DebugDevice) == S_OK) {
		DebugDevice->ReportLiveDeviceObjects(D3D11_RLDO_DETAIL);
		DebugDevice->Release();
	}
#endif
*/

// The device should have been released. Warn if refcount > 0.
	if (refcount > 0) {
		SpoutLogWarning("spoutDirectX::ReleaseDX11Device - refcount = %lu", refcount);
		DebugLog(pd3dDevice, "spoutDirectX::ReleaseDX11Device - refcount = %lu\n", refcount);
		// printf("spoutDirectX::ReleaseDX11Device - refcount = %lu\n", refcount);
	}

	return refcount;
}

// 
// For a DX11 sender :
//   If a shared texture is updated on one device ID3D11DeviceContext::Flush must be called on that device. 
//   https://docs.microsoft.com/en-us/windows/win32/api/d3d11/nf-d3d11-id3d11device-opensharedresource
//   Only the sender updates the shared texture. It is not required for the receiver.
//   The application can either call Flush alone or combine a flush and Wait using this function.
//
// For an OpenGL sender :
//   This function is not necessary, the GL/DX interop performs the necessary flushing.
//

//---------------------------------------------------------
// Function: FlushWait
// Flush immediate context command queue and wait for completion
void spoutDirectX::FlushWait(ID3D11Device* pd3dDevice, ID3D11DeviceContext* pImmediateContext)
{
	if (!pd3dDevice || !pImmediateContext)
		return;

	pImmediateContext->Flush();

	// CopyResource and Flush are both asynchronous.
	// https://msdn.microsoft.com/en-us/library/windows/desktop/bb205132%28v=vs.85%29.aspx#Performance_Considerations
	// Here we can wait for the copy and flush to finish before accessing the texture
	// (Approx 550 microseconds 0.55 msec)
	// Practical testing recommended
	Wait(pd3dDevice, pImmediateContext);

}

//---------------------------------------------------------
// Function: Wait
// Wait for completion after flush
void spoutDirectX::Wait(ID3D11Device* pd3dDevice, ID3D11DeviceContext* pImmediateContext)
{
	if (!pd3dDevice || !pImmediateContext)
		return;

	// https://msdn.microsoft.com/en-us/library/windows/desktop/ff476578%28v=vs.85%29.aspx
	// When the GPU is finished, ID3D11DeviceContext::GetData will return S_OK.
	// When using this type of query, ID3D11DeviceContext::Begin is disabled.
	D3D11_QUERY_DESC queryDesc={};
	ID3D11Query* pQuery = nullptr;
	ZeroMemory(&queryDesc, sizeof(queryDesc));
	queryDesc.Query = D3D11_QUERY_EVENT;
	pd3dDevice->CreateQuery(&queryDesc, &pQuery);
	if (pQuery) {
		pImmediateContext->End(pQuery);
		while (S_OK != pImmediateContext->GetData(pQuery, NULL, 0, 0));
		pQuery->Release();
		pImmediateContext->Flush();
	}
}

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
// Get the number of graphics adapters in the system
int spoutDirectX::GetNumAdapters()
{
	IDXGIFactory1* _dxgi_factory1 = nullptr;
	IDXGIAdapter* adapter1_ptr = nullptr;
	UINT32 i = 0;

	// Enum Adapters first : multiple video cards
	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1))) {
		SpoutLogError("spoutDirectX::GetNumAdapters - No adapters found");
		return 0;
	}
	if (!_dxgi_factory1) return 0;

	for (i = 0; _dxgi_factory1->EnumAdapters( i, &adapter1_ptr ) != DXGI_ERROR_NOT_FOUND; i++ )	{
		if (!adapter1_ptr) break;
		DXGI_ADAPTER_DESC desc;
		adapter1_ptr->GetDesc( &desc );
		// printf(" Adapter(%d) : %S\n", i, desc.Description );
		// printf(" Vendor Id : %d\n", desc.VendorId );
		// printf(" Dedicated System Memory : %.0f MiB\n", (float)desc.DedicatedSystemMemory / (1024.f * 1024.f) );
		// printf(" Dedicated Video Memory : %.0f MiB\n", (float)desc.DedicatedVideoMemory / (1024.f * 1024.f) );
		// printf(" Shared System Memory : %.0f MiB\n", (float)desc.SharedSystemMemory / (1024.f * 1024.f) );
		// LUID  DWORD LowPart; LONG HighPart;
		// printf(" LUID : LowPart %d, HighPart %ld\n", desc.AdapterLuid.LowPart, desc.AdapterLuid.HighPart);

		// Look for outputs
		IDXGIOutput* p_output = nullptr;
		// Is there a first output on this adapter ?
		if (adapter1_ptr->EnumOutputs(0, &p_output) == DXGI_ERROR_NOT_FOUND) {
			if (!p_output) break;
			if (p_output == 0) {
				// Warning ?
				// SpoutLogWarning("spoutDirectX::GetNumAdapters - No outputs for Adapter %d : %S", i, desc.Description);
			}
			else {
				// Here we can list all the outputs of the adapter
				DXGI_OUTPUT_DESC desc_out={};
				for (UINT32 j = 0; adapter1_ptr->EnumOutputs(j, &p_output) != DXGI_ERROR_NOT_FOUND; j++) {
					p_output->GetDesc(&desc_out);
					// printf("   Output : %d\n", j );
					// printf("     Name %S\n", desc_out.DeviceName );
					// HMONITOR hMon = desc_out.Monitor;
					// printf("     Attached to desktop : (%d) %s\n", desc_out.AttachedToDesktop, desc_out.AttachedToDesktop ? "yes" : "no" );
					// printf("    Rotation : %d\n", desc_out.Rotation );
					// printf("    Left     : %d\n", desc_out.DesktopCoordinates.left );
					// printf("    Top      : %d\n", desc_out.DesktopCoordinates.top );
					// printf("    Right    : %d\n", desc_out.DesktopCoordinates.right );
					// printf("    Bottom   : %d\n", desc_out.DesktopCoordinates.bottom );
					p_output->Release();
				}
			}
		}
		adapter1_ptr->Release();
	}

	_dxgi_factory1->Release();

	return i;

}

//---------------------------------------------------------
// Function: GetAdapterName
// Get the name of an adapter index
bool spoutDirectX::GetAdapterName(int index, char *adaptername, int maxchars)
{
	if (!adaptername)
		return false;

	IDXGIFactory1* _dxgi_factory1 = nullptr;
	IDXGIAdapter* adapter1_ptr = nullptr;

	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1))) {
		SpoutLogError("spoutDirectX::GetAdapterName - Could not create CreateDXGIFactory1");
		return false;
	}
	if(!_dxgi_factory1)	return false;
	
	for (int i = 0; _dxgi_factory1->EnumAdapters(i, &adapter1_ptr) != DXGI_ERROR_NOT_FOUND; i++) {
		if (!adapter1_ptr) break;
		
		// Break if the requested index is found
		// Return the adapter name.
		if(i == index) {
			DXGI_ADAPTER_DESC desc;
			adapter1_ptr->GetDesc( &desc );
			size_t charsConverted = 0;
			const size_t maxBytes = static_cast<size_t>(maxchars);
			wcstombs_s(&charsConverted, adaptername, maxBytes, desc.Description, maxBytes - 1);
			adapter1_ptr->Release();
			_dxgi_factory1->Release();
			return true;
		}
		adapter1_ptr->Release();
	}

	_dxgi_factory1->Release();

	return false;
}

//---------------------------------------------------------
// Function: GetAdapterIndex
// Get the index of an adapter name.
// Return -1 if the adapter name was not found,
int spoutDirectX::GetAdapterIndex(const char* adaptername)
{
	if (!adaptername)
		return -1;

	IDXGIFactory1* _dxgi_factory1 = nullptr;
	IDXGIAdapter* adapter1_ptr = nullptr;
	DXGI_ADAPTER_DESC desc={};
	size_t charsConverted = 0;
	char name[256]={}; // Maximum size of desc.Description in bytes (WCHAR 128)

	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1))) {
		SpoutLogError("spoutDirectX::GetAdapterIndex - Could not create CreateDXGIFactory1");
		return -1;
	}
	if(!_dxgi_factory1) return false;

	for (UINT32 i = 0; _dxgi_factory1->EnumAdapters(i, &adapter1_ptr) != DXGI_ERROR_NOT_FOUND; i++) {
		if (adapter1_ptr) {

			adapter1_ptr->GetDesc(&desc);
			// Convert wide char description for comparison with requested name
			wcstombs_s(&charsConverted, name, 256, desc.Description, 256);
			if (charsConverted > 0) {
				// Break if the same name is found and return the adapter index
				if (strcmp(name, adaptername) == 0) {
					adapter1_ptr->Release();
					_dxgi_factory1->Release();
					return i;
				}
			}
			adapter1_ptr->Release();
		}
	}
	_dxgi_factory1->Release();

	// -1 if the adapter name was not found
	return -1;
}

//---------------------------------------------------------
// Function: GetAdapter
// Get the global adapter index
int spoutDirectX::GetAdapter()
{
	return m_AdapterIndex;
}

//---------------------------------------------------------
// Function: SetAdapter
// Set required graphics adapter for output
bool spoutDirectX::SetAdapter(int index)
{
	char adaptername[128]={};
	IDXGIAdapter* pAdapter = nullptr;

	// Reset to default
	if (index == -1)
		index = 0;

	// Is the requested adapter available
	const int n = GetNumAdapters();
	if (index > n-1) {
		SpoutLogError("spoutDirectX::SetAdapter - index %d greater than number of adapters %d", index, n);
		return false;
	}

	// Must be able to get the name
	if (!GetAdapterName(index, adaptername, 128)) {
		SpoutLogError("spoutDirectX::SetAdapter - could not get name for adapter %d", index);
		return false;
	}

	SpoutLogNotice("spoutDirectX::SetAdapter(%d) [%s]", index, adaptername);

	// Get the adapter pointer for DX11 CreateDevice to use
	if (m_pAdapterDX11) {
		m_pAdapterDX11->Release();
		m_pAdapterDX11 = nullptr;
	}

	pAdapter = GetAdapterPointer(index);
	if (!pAdapter) {
		SpoutLogError("spoutDirectX::SetAdapter - could not get pointer for adapter %d", index);
		return false;
	}

	// Set the global adapter index
	// (used for DX11 create device and to retrieve the index)
	m_AdapterIndex = index;

	// Set the adapter pointer for DX11
	m_pAdapterDX11 = pAdapter;

	// In case of incompatibility with the selected adapter, test device creation here

	// For >= 2.007 only DX11 test is supported
	SpoutLogNotice("    creating test device");

	// Try to create a DirectX 11 device for this adapter
	ID3D11Device* pd3dDevice = CreateDX11device();
	if (!pd3dDevice) {
		SpoutLogError("spoutDirectX::SetAdapter - could not create DX11 device for adapter %d", index);
		pAdapter->Release();
		m_AdapterIndex = 0;
		m_pAdapterDX11 = nullptr;
		return false;
	}

	// Close the device because this is just a test
	// See : https://github.com/leadedge/Spout2/issues/17
	ReleaseDX11Device(pd3dDevice);
	pd3dDevice = nullptr;

	// Selected adapter OK
	SpoutLogNotice("    successfully set adapter %d (0x%7.7X) [%s]",
		m_AdapterIndex, PtrToUint(m_pAdapterDX11), adaptername);

	return true;

}

//---------------------------------------------------------
// Function: GetAdapterInfo
// Get the description and output name of the current adapter
//
// Overload retained for back compatibility
bool spoutDirectX::GetAdapterInfo(char* adaptername, char* output, int maxchars)
{
	return GetAdapterInfo(0, adaptername, output, maxchars);
}

//---------------------------------------------------------
// Function: GetAdapterInfo
// Get the description and output display name for a given adapter
bool spoutDirectX::GetAdapterInfo(int index, char* adaptername, char* output, int maxchars)
{
	if (!adaptername || !output)
		return false;

	IDXGIFactory1* _dxgi_factory1 = nullptr;
	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1))) {
		SpoutLogError("spoutDirectX::GetAdapterInfo - Could not create CreateDXGIFactory1");
		return false;
	}
	if (!_dxgi_factory1) return 0;


	IDXGIAdapter* adapter1_ptr = nullptr;
	size_t charsConverted = 0;
	const size_t maxBytes = static_cast<size_t>(maxchars);
	*adaptername = 0;
	*output = 0;
	if(_dxgi_factory1->EnumAdapters(index, &adapter1_ptr) != DXGI_ERROR_NOT_FOUND) {
		if (adapter1_ptr) {
			// Adapter name
			DXGI_ADAPTER_DESC desc;
			adapter1_ptr->GetDesc(&desc);
			wcstombs_s(&charsConverted, adaptername, maxBytes, desc.Description, maxBytes-1);
			// Find the first output (index 0) on which the desktop primary is displayed.
			IDXGIOutput* p_output = nullptr;
			if (adapter1_ptr->EnumOutputs(0, &p_output) != DXGI_ERROR_NOT_FOUND) {
				if (p_output) {
					DXGI_OUTPUT_DESC desc_out;
					p_output->GetDesc(&desc_out);
					wcstombs_s(&charsConverted, output, maxBytes, desc_out.DeviceName, maxBytes - 1);
					// TODO : if (desc_out.AttachedToDesktop)
					p_output->Release();
				}
			}
			adapter1_ptr->Release();
		}

		/*
		TODO
		// Find the desktop output
		IDXGIOutput* p_output = nullptr;
		for ( UINT32 j = 0; adapter1_ptr->EnumOutputs( j, &p_output ) != DXGI_ERROR_NOT_FOUND; j++ ) {
			printf("    Output %d\n", j);
			DXGI_OUTPUT_DESC desc_out;
			if (p_output) {
				p_output->GetDesc(&desc_out);
				wcstombs_s(&charsConverted, display, maxBytes, desc_out.DeviceName, maxBytes - 1);
				printf("    Device name [%s]\n", display);
				if (desc_out.AttachedToDesktop)
					wcstombs_s(&charsConverted, display, maxBytes, desc_out.DeviceName, maxBytes - 1);
				p_output->Release();
			}
			else {
				printf("    No output\n");
			}
		}
		*/

	}
	_dxgi_factory1->Release();
	return true;
}

//---------------------------------------------------------
// Function: GetAdapterPointer
// Get adapter pointer for a given adapter (-1 means current)
IDXGIAdapter* spoutDirectX::GetAdapterPointer(int index)
{
	int adapterindex = index;

	// Return the current pointer for default if already determined
	// Otherwise get the pointer for the first adapter
	if (adapterindex < 0) {
		if (m_pAdapterDX11)
			return m_pAdapterDX11;
		else
			adapterindex = 0;
	}

	// Enum Adapters first : multiple video cards
	IDXGIFactory1* _dxgi_factory1 = nullptr;
	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1))) {
		SpoutLogError("spoutDirectX::GetAdapterPointer - Could not create CreateDXGIFactory1");
		return nullptr;
	}
	if (!_dxgi_factory1) return 0;

	IDXGIAdapter* adapter1_ptr = nullptr;
	for (int i = 0; _dxgi_factory1->EnumAdapters(i, &adapter1_ptr) != DXGI_ERROR_NOT_FOUND; i++) {
		if (!adapter1_ptr) break;
		if (adapterindex == i) {
			/*
			// TODO : Removed pending testing
			// Now we have the requested adapter (17-03-18) test for an output on the adapter
			IDXGIOutput* p_output = nullptr;
			if (adapter1_ptr->EnumOutputs(0, &p_output) == DXGI_ERROR_NOT_FOUND) {
				SpoutLogError("spoutDirectX::GetAdapterPointer(%d) :  No outputs", i);
				adapter1_ptr->Release();
				_dxgi_factory1->Release();
				return nullptr;
			}
			p_output->Release();
			*/
			_dxgi_factory1->Release();
			return adapter1_ptr;
		}
		adapter1_ptr->Release();
	}
	_dxgi_factory1->Release();

	return nullptr;
}

//---------------------------------------------------------
// Function: SetAdapterPointer
// Set required graphics adapter for CreateDX11device
void spoutDirectX::SetAdapterPointer(IDXGIAdapter* pAdapter)
{
	m_pAdapterDX11 = pAdapter;
}

//---------------------------------------------------------
// Function: FindNVIDIA
// Find the index of the NVIDIA adapter
//
// For purposes where NVIDIA hardware acceleration is used (e.g. FFmpeg)
bool spoutDirectX::FindNVIDIA(int& nAdapter)
{
	IDXGIFactory1* _dxgi_factory1 = nullptr;
	IDXGIAdapter* adapter1_ptr = nullptr;
	DXGI_ADAPTER_DESC desc={};
	UINT32 i = 0;
	bool bFound = false;

	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory1), (void**)&_dxgi_factory1)))
		return false;
	if (!_dxgi_factory1) return false;

	for (i = 0; _dxgi_factory1->EnumAdapters(i, &adapter1_ptr) != DXGI_ERROR_NOT_FOUND; i++) {
		if (!adapter1_ptr) break;
		adapter1_ptr->GetDesc(&desc);
		DXGI_OUTPUT_DESC desc_out={};
		IDXGIOutput* p_output = nullptr;
		// TODO 
		// if(adapter1_ptr->EnumOutputs(0, &p_output ) == DXGI_ERROR_NOT_FOUND) {
		//		if(!p_output) continue;
		//		if(p_output == 0) {
		//			printf("  No outputs\n");
		//			continue;
		//		}
		//	}
		for (int j = 0; adapter1_ptr->EnumOutputs(j, &p_output) != DXGI_ERROR_NOT_FOUND; j++) {
			if (p_output) {
				p_output->GetDesc(&desc_out);
				// printf( "  Output : %d\n", j );
				// printf( "    Name %S\n", desc_out.DeviceName );
				// printf( "    Attached to desktop : (%d) %s\n", desc_out.AttachedToDesktop, desc_out.AttachedToDesktop ? "yes" : "no" );
				// printf( "    Rotation : %d\n", desc_out.Rotation );
				// printf( "    Left     : %d\n", desc_out.DesktopCoordinates.left );
				// printf( "    Top      : %d\n", desc_out.DesktopCoordinates.top );
				// printf( "    Right    : %d\n", desc_out.DesktopCoordinates.right );
				// printf( "    Bottom   : %d\n", desc_out.DesktopCoordinates.bottom );
				p_output->Release();
			}
		}
		adapter1_ptr->Release();

		if (wcsstr(desc.Description, L"NVIDIA")) {
			// printf("spoutDirectX::FindNVIDIA - Found NVIDIA adapter %d (%S)\n", i, desc.Description);
			bFound = true;
			break;
		}

	}

	_dxgi_factory1->Release();

	if (bFound) {
		// printf// ("spoutDirectX::FindNVIDIA - Found NVIDIA adapter %d (%S)\n", i, desc.Description);
		nAdapter = i;
		//	0x10DE	NVIDIA
		//	0x163C	intel
		//	0x8086  Intel
		//	0x8087  Intel
		// printf("Vendor    = %d [0x%.7X]\n", desc.VendorId, desc.VendorId);
		// printf("Revision  = %d [0x%.7X]\n", desc.Revision, desc.Revision);
		// printf("Device ID = %d [0x%.7X]\n", desc.DeviceId, desc.DeviceId);
		// printf("SubSys ID = %d [0x%.7X]\n", desc.SubSysId, desc.SubSysId);
		return true;
	}

	return false;

}

//
// Group: Graphics performance
//
// Windows Graphics performance preferences.
//
// Performance preference requires the system to have multiple graphics
// processors which provide a choice between "Power saving" and "High perfformance". 
// Typically this will be laptop systems with integrated and discrete graphics.
//
// Desktop systems with multiple discrete graphics cards do not provide that choice 
// even though Windows still allows applications to be set for desired preference.
// The adapter reported for all preferences is the same. The one with monitor output.
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
int spoutDirectX::GetPerformancePreference(const char* path)
{
	if (!IsPreferenceAvailable()) {
		SpoutLogWarning("spoutDirectX::GetPerformancePreference - Preferences not available");
		return -1;
	}

	char exepath[MAX_PATH]={};
	// No path specified - get the current application path
	if (!path) {
		if (GetModuleFileNameA(NULL, exepath, MAX_PATH) <= 0) {
			SpoutLogWarning("spoutDirectX::GetPerformancePreference - Could not get application path");
			return -1;
		}
	}
	else {
		strcpy_s(exepath, MAX_PATH, path);
	}

	// A valid sender application path will have a drive letter and terminate with ".exe"
	if(IsApplicationPath(exepath)) {
		char prefs[256]={};
		int preference = 0;
		if (ReadPathFromRegistry(HKEY_CURRENT_USER, "SOFTWARE\\Microsoft\\DirectX\\UserGpuPreferences", exepath, prefs, 256)) {
			std::string pr = prefs;
			pr = pr.substr(pr.find("=")+1, 1);
			preference = atoi(pr.c_str());
			// SpoutLogNotice("    Current preference = %d", preference);
			return preference;
		}
	}
	else {
		SpoutLogWarning("spoutDirectX::GetPerformancePreference - Application path not valid");
	}
	return -1;
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
bool spoutDirectX::SetPerformancePreference(int preference, const char* path)
{
	if (!IsPreferenceAvailable()) {
		SpoutLogWarning("spoutDirectX::SetPerformancePreference : Preferences not available");
		return false;
	}

	char exepath[MAX_PATH]={};
	// No path specified - get the current application path
	if (!path) {
		if (GetModuleFileNameA(NULL, exepath, MAX_PATH) <= 0) {
			SpoutLogWarning("    Could not get application path");
			return false;
		}
	}
	else {
		strcpy_s(exepath, MAX_PATH, path);
	}

	// A valid application path will have a drive letter and terminate with ".exe"
	if (IsApplicationPath(exepath)) {

		if (preference == DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE)
			SpoutLogNotice("spoutDirectX::SetPerformancePreference - high performance");
		else if (preference == DXGI_GPU_PREFERENCE_MINIMUM_POWER)
			SpoutLogNotice("spoutDirectX::SetPerformancePreference - minimum power");
		else if (preference == DXGI_GPU_PREFERENCE_UNSPECIFIED)
			SpoutLogNotice("spoutDirectX::SetPerformancePreference - unspecified");
		else
			SpoutLogNotice("spoutDirectX::SetPerformancePreference - none");
		SpoutLogNotice("    [%s]", path);

		char prefs[256]={};
		if (preference == -1) {
			// Remove preference
			if (RemovePathFromRegistry(HKEY_CURRENT_USER, "SOFTWARE\\Microsoft\\DirectX\\UserGpuPreferences", exepath)) {
				SpoutLog("    Removed preference");
				return true;
			}
		}
		else {
			// GpuPreference=0; // Default
			// GpuPreference=1; // Power saving
			// GpuPreference=2; // High performance
			sprintf_s(prefs, 256, "GpuPreference=%d;", preference);
			if (WritePathToRegistry(HKEY_CURRENT_USER, "SOFTWARE\\Microsoft\\DirectX\\UserGpuPreferences", exepath, prefs)) {
				SpoutLog("    Set [%s]", prefs);
				return true;
			}
		}
	}
	else {
		SpoutLogNotice("    Application path not valid");
	}
	return false;
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
// This applies for laptops with multiple graphics that provide a choice
// between power saving and performance. For desktop systems, the adapter 
// reported for all preferences is the same. The one with monitor output.
//
bool spoutDirectX::GetPreferredAdapterName(int preference, char* adaptername, int maxchars)
{
	if (!adaptername)
		return false;

	// preference : -1, 0, 1, 2
	// Unregistered (-1) use GetAdapterName 
	if (preference < 0) return false;

	if (!IsPreferenceAvailable()) {
		SpoutLogWarning("spoutDirectX::GetPreferredAdapterName : Preferences not available");
		return false;
	}

	if (preference == DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE)
		SpoutLogNotice("spoutDirectX::GetPreferredAdapterName - high performance");
	else if (preference == DXGI_GPU_PREFERENCE_MINIMUM_POWER)
		SpoutLogNotice("spoutDirectX::GetPreferredAdapterName - minimum power");
	else
		SpoutLogNotice("spoutDirectX::GetPreferredAdapterName - unspecified");


	IDXGIFactory2* pFactory = nullptr;
	if (FAILED(CreateDXGIFactory1(__uuidof(IDXGIFactory2), (void**)&pFactory))) {
		SpoutLogError("spoutDirectX::GetPreferredAdapterName - Could not create CreateDXGIFactory1");
		return false;
	}
	if(!pFactory) return false;

	bool bRet = false;
	IDXGIFactory6* pFactory6 = nullptr;
	IDXGIAdapter1* pAdapter1 = nullptr;
	if (SUCCEEDED(pFactory->QueryInterface(IID_PPV_ARGS(&pFactory6)))) {
		if (!pFactory6) return false;
		size_t charsConverted = 0;
		const size_t maxBytes = static_cast<size_t>(maxchars);

		// Get the first adapter for this preference
		if (pFactory6->EnumAdapterByGpuPreference(0, static_cast<DXGI_GPU_PREFERENCE>(preference), IID_PPV_ARGS(&pAdapter1)) != DXGI_ERROR_NOT_FOUND) {
			if (!pAdapter1) {
				pFactory6->Release();
				return false;
			}
			DXGI_ADAPTER_DESC1 desc;
			pAdapter1->GetDesc1(&desc);
			wcstombs_s(&charsConverted, adaptername, maxBytes, desc.Description, maxBytes-1);
			pAdapter1->Release();
			bRet = true;
		}
		pFactory6->Release();
	}

	pFactory->Release();
	return bRet;

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
bool spoutDirectX::SetPreferredAdapter(int preference)
{
	if (!IsPreferenceAvailable()) {
		SpoutLogWarning("spoutDirectX::SetPreferredAdapter : Preferences not available");
		return false;
	}

	if(preference == DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE)
		SpoutLogNotice("spoutDirectX::SetPreferredAdapter - high performance");
	else if (preference == DXGI_GPU_PREFERENCE_MINIMUM_POWER)
		SpoutLogNotice("spoutDirectX::SetPreferredAdapter - minimum power");
	else
		SpoutLogNotice("spoutDirectX::SetPreferredAdapter - unspecified");

	// Get the name of the preferred adapter
	char adaptername[256]={};
	if (GetPreferredAdapterName(preference, adaptername, 256)) {
		// Find it's index in the list of adapters
		const int index = GetAdapterIndex(adaptername);
		if (index >= 0) {
			// Set that index for CreateDX11device to use
			return SetAdapter(index);
		}
	}
	return false;
}

//---------------------------------------------------------
// Function: IsPreferenceAvailable()
// Availability of Windows graphics preference settings.
//
// Settings are available from Windows 10 April 2018 update 
// (Version 1803, build 17134) and later.
bool spoutDirectX::IsPreferenceAvailable()
{
	char build[128]={};
	if (ReadPathFromRegistry(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion", "CurrentBuildNumber", build, 128)) {
		if (atoi(build) >= 17134)
			return true;
	}
	return false;
}

//---------------------------------------------------------
// Function: IsApplicationPath
//
// Is the path a valid application
//
// A valid application path will have a drive letter and terminate with ".exe"
bool spoutDirectX::IsApplicationPath(const char* path)
{
	// Search for the drive letter colon ":" and terminating ".exe"
	std::string spath = path;
	std::string str = spath.substr(1, 1);
	if (spath.substr(1, 1) == ":" && spath.substr(spath.length()-4, 4) == ".exe") {
		return true;
	}
	return false;
}
#endif



//
// Protected
//


void spoutDirectX::DebugLog(const ID3D11Device* pd3dDevice, const char* format, ...)
{

	if (!pd3dDevice)
		return;

// Suppress warning 26826 to use vsprintf_s
#pragma warning(disable:26485)

	//
	// Output for debug build
	//
	// *** Manually remove the comment block below if you have D3D11_1SDKLayers.dll installed. ***
	//
	// See comments in : ID3D11Device* spoutDirectX::CreateDX11device()
	//

	// Construct the log here in any case to avoid UNREFERENCED_PARAMETER warning
	char dlog[128]={};
	va_list args;
	va_start(args, format);
	// An explicit cast to the decayed pointer type prevents the warning
	vsprintf_s(dlog, 128, format, args);
	va_end(args);

#pragma warning(default:26485)
	
/*
#ifdef _DEBUG
	
	// New line
	OutputDebugStringA("\n");
	OutputDebugStringA(dlog);

	ID3D11Debug* DebugDevice = nullptr;
	if (pd3dDevice->QueryInterface(__uuidof(ID3D11Debug), (void**)&DebugDevice) == S_OK) {
		ID3D11InfoQueue *d3dInfoQueue = nullptr;
		if (SUCCEEDED(DebugDevice->QueryInterface(__uuidof(ID3D11InfoQueue), (void**)&d3dInfoQueue))) {
			d3dInfoQueue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_CORRUPTION, true);
			d3dInfoQueue->SetBreakOnSeverity(D3D11_MESSAGE_SEVERITY_ERROR, true);
			D3D11_MESSAGE_ID hide[] =
			{
				D3D11_MESSAGE_ID_SETPRIVATEDATA_CHANGINGPARAMS,
				// Add more message IDs here as needed
			};

			D3D11_INFO_QUEUE_FILTER filter;
			memset(&filter, 0, sizeof(filter));
			filter.DenyList.NumIDs = _countof(hide);
			filter.DenyList.pIDList = hide;
			d3dInfoQueue->AddStorageFilterEntries(&filter);
		}

		// Print live objects to the debug Output window
		DebugDevice->ReportLiveDeviceObjects(D3D11_RLDO_DETAIL);

		DebugDevice->Release();
	}
#endif
*/



}
