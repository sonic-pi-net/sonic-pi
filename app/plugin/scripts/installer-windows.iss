; SPDX-License-Identifier: AGPL-3.0-or-later
; Copyright (c) 2026 Sam Aaron
;
; The Windows installer for "Sonic Pi.vst3": an Inno Setup script, compiled by scripts\build-windows.ps1 -Installer
; after the bundle is built (and signed). It puts the whole bundle, both architectures and the shared Resources, in
; the one folder every DAW scans, C:\Program Files\Common Files\VST3, replacing any earlier version whole, and
; uninstalls from Windows' Installed apps. One installer for x64 and Arm64 Windows: the DAW loads the binary for its
; own architecture from the bundle.
;
; Defined by build-windows.ps1 on ISCC's command line:
;   Version  the plugin's version, from Cargo.toml
;   Bundle   the built bundle, dist\Sonic Pi.vst3
;   OutDir   where the installer goes
;   Sign     defined when the installer and its uninstaller are to be signed, through ISCC's /Ssigntool=

#ifndef Version
  #error Version is not defined: build with scripts\build-windows.ps1 -Installer
#endif

[Setup]
; Fixed for good: Windows knows an installed version, and so an upgrade, by this id.
AppId={{6F0C2B7E-3A54-4D8E-9B1C-5E2A7D9F4C31}
AppName=Sonic Pi VST3
AppVersion={#Version}
AppVerName=Sonic Pi VST3 {#Version}
AppPublisher=Sam Aaron
AppPublisherURL=https://sonic-pi.net
AppSupportURL=https://sonic-pi.net
VersionInfoVersion={#Version}
VersionInfoDescription=Sonic Pi VST3 installer
; Common Files\VST3 is the place, and is not a choice: a DAW looks nowhere else for VST3s.
DefaultDirName={commoncf64}\VST3\Sonic Pi.vst3
DisableDirPage=yes
DisableProgramGroupPage=yes
DisableReadyPage=no
Uninstallable=yes
UninstallDisplayName=Sonic Pi VST3
SetupIconFile={#SourcePath}..\..\gui\images\app.ico
PrivilegesRequired=admin
; x64 Windows, and Arm64 Windows (10 and 11): in 64-bit mode, so {commoncf64} is the native Common Files on both.
ArchitecturesAllowed=x64compatible or arm64
ArchitecturesInstallIn64BitMode=x64compatible or arm64
MinVersion=10.0
; A DAW that has the plugin loaded holds its DLL: Setup offers to close it, as it would hold up the replacement.
CloseApplications=yes
RestartApplications=no
OutputDir={#OutDir}
OutputBaseFilename=Sonic-Pi-VST3-{#Version}-windows-setup
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
#ifdef Sign
SignTool=signtool
SignedUninstaller=yes
#endif

[InstallDelete]
; The earlier version whole, so nothing it had and this one has not is left in the bundle.
Type: filesandordirs; Name: "{app}"

[Files]
Source: "{#Bundle}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Messages]
FinishedLabel=Sonic Pi's VST3 plugin is installed in [name] in Common Files\VST3.%n%nRescan your plugins in your DAW to find it.
FinishedLabelNoIcons=Sonic Pi's VST3 plugin is installed in Common Files\VST3.%n%nRescan your plugins in your DAW to find it.

[Code]
// The plugin's window is a web page shown by Microsoft Edge WebView2. Windows 11 has it, as has nearly every
// Windows 10 through Edge's updates; where it is missing, the plugin loads but shows no window, so say so.
const
  WebView2Key = 'SOFTWARE\Microsoft\EdgeUpdate\Clients\{F3017226-FE2A-4295-8BDF-00C3A9A7E4C5}';

function HasWebView2: Boolean;
var
  pv: String;
begin
  Result :=
    (RegQueryStringValue(HKLM32, WebView2Key, 'pv', pv) and (pv <> '') and (pv <> '0.0.0.0')) or
    (RegQueryStringValue(HKCU, WebView2Key, 'pv', pv) and (pv <> '') and (pv <> '0.0.0.0'));
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep = ssDone) and not HasWebView2 and not WizardSilent then
    MsgBox('Sonic Pi''s plugin window needs Microsoft Edge WebView2, which this computer does not seem to have.' + #13#10#13#10 +
           'Install the "Evergreen" WebView2 Runtime from https://developer.microsoft.com/microsoft-edge/webview2/',
           mbInformation, MB_OK);
end;
