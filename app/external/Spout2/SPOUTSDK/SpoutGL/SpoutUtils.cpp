/*

			SpoutUtils

			Utility functions

	CREDIT - logging based on Openframeworks ofLog
	https://github.com/openframeworks/openFrameworks/tree/master/libs/openFrameworks/utils

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	Copyright (c) 2017-2023, Lynn Jarvis. All rights reserved.

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
	========================

		31.05.15 - started
		01.01.18 - added check for subkey
		17.03.18 - Document SetLogLevel function in header
		16.10.18 - Add SpoutLogNotice, Warning, Error, Fatal
		28.10.18 - Checks for lastlog and fatal messagebox moved from SpoutLog to _doLog
		15.11.18 - Removed delay after SpoutPanel open
		10.12.18 - Add Timeout option to SpoutMessageBox
		12.12.18 - Add SpoutLogFile
		13.12.18 - Add GetSpoutLog and ShowSpoutLogs
		14.12.18 - Clean up
		02.10.19 - Change registry functions including hKey
				   to allow HKLM and changed argument order
				 - Add RemoveSubKey and FindSubKey
		26.01.19 - Corrected Verbose log to show verbose and not notice
		07.04.19 - Add SpoutLog for logging without specifying level
		28.04.19 - Change OpenSpoutConsole() to check for existing console
		19.05.19 - Cleanup
		16.06.19 - Include calling process file name in SpoutMessageBox
		13.10.19 - Corrected EnableSpoutLogFile for a filename without an extension
				   Changed default extension from "txt" to "log"
		27.11.19 - Prevent multiple logs for warnings and errors
		22.12.19 - add pragma in header for registry function lbraries
		22.12.19 - Remove calling process name from SpoutMessageBox
		18.02.20 - Remove messagebox for Fatal errors
		19.05.20 - Add missing LPCSTR cast in SpoutMessageBox ShellExecute
		12.06.20 - Add timing functions for testing
		01.09.20 - Add seconds to log file header
		03.09.20 - Add DisableSpoutLogFile() DisableLogs() and EnableLogs() 
				   for more control over logging
		09.09.20 - move _doLog outside anonymous namespace
		23.09.20 - _doLog : always prevent multiple logs by comparing with the last
				   instead of reserving for > warnings
		16.10.20 - Add bool WriteBinaryToRegistry
		04.03.21 - Add std::string GetSDKversion()
		09.03.21 - Fix code if USE_CHRONO not defined
		17.04.21 - Disable close button on console
				   Bring the main window to the top again
		07.05.21 - Remove noisy warning from ReadPathFromRegistry
		09.06.21 - Update Version to "2.007.002"
		26.07.21 - Update Version to "2.007.003"
		16.09.21 - Update Version to "2.007.004"
		04.10.21 - Remove shlobj.h include due to redifinition conflict with ShObjIdl.h
				   Replace code using environment variable "APPDATA"
		24.10.21 - Update Version to "2.007.005"
		08.11.21 - Change to high_resolution_clock for timer
		15.12.21 - Change back to steady clock
				   Use .clear() instead of "" to clear strings
		20.12.21 - Change from string to to char array for last log
				   Update Version to "2.007.006"
		29.01.21 - Change return logic of RemovePathFromRegistry
		24.02.22 - Update Version to "2.007.007"
		25.02.22 - OpenSpoutConsole - check AllocConsole error for existing console
				   Fix for Processing.
		14.04.22 - Add option in SpoutCommon.h to disable warning 26812 (unscoped enums)
		23.06.22 - Add ElapsedMicroseconds (usec since epoch)
		30.10.22 - Code cleanup and documentation
		01.11.22 - Add IsLaptop(char* computername)
		30.11.22 - Update Version to "2.007.009"
		05.12.22 - Change ordering of _logtofile function to avoid ambiguous warning.
				   GetSpoutLog - optional log file argument. Remove redundant file open.
				   See SpoutSettongs "Log" option.
		07.12.22 - EnableSpoutLogFile allow null file name argument.
				   If a file name was not specified, use the executable name.
				   Use "GetCurrentModule" instead of NULL for GetModuleFileNameA
				   in case the process is a dll (https://gist.github.com/davidruhmann/8008844).
		08.12.22 - _dolog - clean up file log code.
				 - Corrected ExecuteProcess for EndTiming milliseconds return.
		11.12.22 - Initialize all char arrays and structures with {}
				   https://en.cppreference.com/w/cpp/language/zero_initialization
		14.12.22 - Add RemoveSpoutLogFile
		18.12.22 - Add buffer size argument to ReadPathFromRegistry
				   Correct code review warnings where possible
				   Add more documentation to Group: Logs
		19.12.22 - Add GetCurrentModule / LogsEnabled / LogFileEnabled / GetSpoutLogPath
		20.12.22 - Add SPOUT_DLLEXP to all header function declarations for dll export.
				 - Code cleanup
		22.12.22 - Compiler compatibility check
				   Change all {} initializations to "={}"
		31.12.22 - increase log char buffer from 512 to 1024
		01.12.22 - Registry functions
				     check for empty subkey and valuename strings
					 include valuename in warnings
		14.01.23 - OpenSpoutConsole - add MessageBox warning if using a dll
				   EnableSpoutLog - open console rather than call OpenSpoutConsole
		15.01.23 - Use SpoutMessageBox so it doesn't freeze the application GUI
		16.01.23 - Add SpoutMessageBox caption
		17.01.23 - Add SpoutMessageBox with variable arguments
				   Add ConPrint for SpoutUtils console (printf replacement)
				   Remove dll build warning MessageBox.
				   Change "ConPrint" to "_conprint" and use Writefile instead of cout.
		18.01.23 - _conprint - cast WriteFile size argument to DWORD
		19.03.23 - Update SDKversion to 2.007.010

*/

#include "SpoutUtils.h"

//
// Namespace: spoututils
//
// Namespace for utility functions.
//
// - Version
// - Console
// - Logs
// - MessageBox dialog
// - Registry utilities
// - Computer information
// - Timing utilities
//
// Refer to source code for documentation.
//

namespace spoututils {

	// Local variables
	bool bEnableLog = false;
	bool bEnableLogFile = false;
	bool bDoLogs = true;
	SpoutLogLevel CurrentLogLevel = SPOUT_LOG_NOTICE;
	FILE* pCout = NULL; // for log to console
	std::ofstream logFile; // for log to file
	std::string logPath; // folder path for the logfile
	char logChars[1024]={}; // The current log string
	bool bConsole = false;
#ifdef USE_CHRONO
	std::chrono::steady_clock::time_point start;
	std::chrono::steady_clock::time_point end;
#else
	// PC timer
	double PCFreq = 0.0;
	__int64 CounterStart = 0;
	double start = 0.0;
	double end = 0.0;
	double m_FrameStart = 0.0;
#endif

	// Spout SDK version number string
	// Major, minor, release
	std::string SDKversion = "2.007.010";

	//
	// Group: Information
	//

	// ---------------------------------------------------------
	// Function: GetSDKversion
	//
	// Get SDK version number string. 
	std::string GetSDKversion()
	{
		return SDKversion;
	}

	// ---------------------------------------------------------
	// Function: IsLaptop
	// Return whether the system is a laptop.
	//
	// Queries power status. Most likely a laptop if battery power is available. 
	bool IsLaptop ()
	{
		SYSTEM_POWER_STATUS status;
		if (GetSystemPowerStatus(&status)) {
			// SpoutLog("    ACLineStatus         %d", status.ACLineStatus);
			// SpoutLog("    BatteryFlag          %d", status.BatteryFlag);
			// SpoutLog("    BatteryLifePercent   %d", status.BatteryLifePercent);
			// SpoutLog("    SystemStatusFlag     %d", status.SystemStatusFlag);
			// SpoutLog("    BatteryLifeTime      %d", status.BatteryLifeTime);
			// SpoutLog("    BatteryFullLifeTime  %d", status.BatteryFullLifeTime);
			// BatteryFlag, battery charge status (128 - No system battery)
			// BatteryLifePercent,  % of full battery charge remaining (255 - Status unknown)
			if (status.BatteryFlag != 128 && status.BatteryLifePercent != 255) {
				return true;
			}
		}
		return false;
	}


	// ---------------------------------------------------------
	// Function: GetCurrentModule()
	// Get the module handle of a process.
	//
	// This method is necessary if the process is a dll
	//
	// https://gist.github.com/davidruhmann/8008844
	HMODULE GetCurrentModule()
	{
		const DWORD flags = GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT;
		HMODULE hModule = NULL;
		// hModule is NULL if GetModuleHandleEx fails.
		GetModuleHandleExA(flags, (LPCSTR)GetCurrentModule, &hModule);
		return hModule;
	}

	//
	// Group: Console management
	//

	// ---------------------------------------------------------
	// Function: OpenSpoutConsole
	// Open console window.
	//
	// A console window opens without logs.
	// Useful for debugging with console output.
	//
	void OpenSpoutConsole()
	{
		if (!GetConsoleWindow()) {
	
			//
			// Application console window mot found
			//

			// Get calling process window
			HWND hwndFgnd = GetForegroundWindow();
			if (AllocConsole()) {
				const errno_t err = freopen_s(&pCout, "CONOUT$", "w", stdout);
				if (err == 0) {
					SetConsoleTitleA("Spout Log");
					bConsole = true;
					// Disable close button
					// HMENU hmenu = GetSystemMenu(GetConsoleWindow(), FALSE);
					// EnableMenuItem(hmenu, SC_CLOSE, MF_GRAYED);
					// Bring the main window to the top again
					SetWindowPos(hwndFgnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
				}
				else {
					pCout = NULL;
					bConsole = false;
				}
			}
			else {
				// If the calling process is already attached to a console,
				// the error code returned is ERROR_ACCESS_DENIED(5).
				if (GetLastError() == 5) {
					bConsole = true;
				}
			}
		}
	}
	
	// ---------------------------------------------------------
	// Function: CloseSpoutConsole
	// Close console window.
	//
	// The optional warning displays a MessageBox if user notification is required.
	void CloseSpoutConsole(bool bWarning)
	{
		if(bWarning) {
			if(MessageBoxA(NULL, "Console close - are you sure?", "CloseSpoutConsole", MB_YESNO) == IDNO)
				return;
		}
		if (pCout) {
			fclose(pCout);
			FreeConsole();
			pCout = NULL;
			bConsole = false;
		}
	}

			
	//
	// Group: Logs
	//
	//
	// Spout logs are used thoughout the SDK and are printed to a console
	// with EnableLogs or saved to a file with EnableLogFIle.
	//
	// You can set the level above which the logs are shown
	// SPOUT_LOG_SILENT  : SPOUT_LOG_VERBOSE : SPOUT_LOG_NOTICE (default)
	// SPOUT_LOG_WARNING : SPOUT_LOG_ERROR   : SPOUT_LOG_FATAL
	// For example, to show only warnings and errors (you shouldn't see any)
	// or leave set to default Notice to see more information.
	//
	//    SetSpoutLogLevel(SPOUT_LOG_WARNING);
	//
	// You can instead, or additionally, output to a text log file
	// with the name and extension of your choice.
	//    EnableSpoutLogFile("OF Spout Graphics sender.log");
	//
	// The log file is re-created every time the application starts
	// unless you specify to append to the existing one :
	//    EnableSpoutLogFile("OF Spout Graphics sender.log", true);
	//
	// The file is saved in the %AppData% folder unless you specify the full path.
	//    C:>Users>username>AppData>Roaming>Spout
	//
	// If there is no file specified, the executable or dll name is used.
	// 
	// After the application has run you can find and examine the log file
	//
	// This folder can also be shown in Windows Explorer directly from the application.
	//    ShowSpoutLogs();
	//
	// Or the entire log file can be returned as a string
	//    std::string logstring = GetSpoutLog();
	//
	// You can also create your own logs
	// For example :
	//    SpoutLog("SpoutLog test");
	//
	// Or specify the logging level :
	// For example :
	//    SpoutLogNotice("Important notice");
	// or :
	//    SpoutLogFatal("This should not happen");
	// or :
	//    SetSpoutLogLevel(SPOUT_LOG_VERBOSE);
	//    SpoutLogVerbose("Message");
	//
	// See SpoutUtils.h for mre information
	//

	// ---------------------------------------------------------
	// Enum: Log level definitions
	// The level above which the logs are shown.
	// 
	//   SPOUT_LOG_SILENT - Disable all messages.
	//   SPOUT_LOG_VERBOSE - Show all messages.
	//   SPOUT_LOG_NOTICE - Show information messages (default).
	//   SPOUT_LOG_WARNING - Show warning, errors and fatal.
	//   SPOUT_LOG_ERROR - Show errors and fatal.
	//   SPOUT_LOG_FATAL - Show only fatal errors.
	//   SPOUT_LOG_NONE - Ignore log levels.
	// 
	// For example, to show only warnings and errors (you shouldn't see any):
	// 
	// 	SetSpoutLogLevel(SPOUT_LOG_WARNING);
	//

	// ---------------------------------------------------------
	// Function: EnableSpoutLog
	// Enable logging to the console.
	//
	// Logs are displayed in a console window.  
	// Useful for program development.
	//
	void EnableSpoutLog()
	{
		bEnableLog = true;

		// Console output
		if (GetConsoleWindow()) {
			SetConsoleTitleA("Spout Log");
			bConsole = true;
		}
		else if(!bConsole)
			OpenSpoutConsole();

		// Initialize current log string
		logChars[0] = 0;

	}

	// ---------------------------------------------------------
	// Function: EnableSpoutLogFile
	// Enable logging to a file with optional append.
	//
	// As well as a console window, you can output logs to a text file. 
	// Default extension is ".log" unless the full path is used.
	// For no file name or path the executable name is used.
	//
	//     Example : EnableSpoutLogFile("Sender.log");
	//
	// The log file is re-created every time the application starts
	// unless you specify to append to the existing one.  
	//
	//    Example : EnableSpoutLogFile("Sender.log", true);
	//
	// The file is saved in the %AppData% folder unless you specify the full path : 
	//
	//    C:>Users>username>AppData>Roaming>Spout   
	//
	// You can find and examine the log file after the application has run.
	void EnableSpoutLogFile(const char* filename, bool bAppend)
	{
		bEnableLogFile = true;
		if (!logPath.empty()) {
			if (logFile.is_open())
				logFile.close();
			logPath.clear();
		}
		logChars[0] = 0;

		// Create the log file path given the filename passed in
		logPath = _getLogFilePath(filename);
		_logtofile(bAppend);
	}

	// ---------------------------------------------------------
	// Function: DisableSpoutLogFile
	// Disable logging to file
	void DisableSpoutLogFile() {
		if (!logPath.empty()) {
			if (logFile.is_open())
				logFile.close();
			logPath.clear();
		}
	}

	// ---------------------------------------------------------
	// Function: RemoveSpoutLogFile
	// Remove a log file
	void RemoveSpoutLogFile(const char* filename)
	{
		// The path derived from the name or path passed in
		// could be different to the current log file
		std::string path = _getLogFilePath(filename);

		if (!path.empty()) {
			// If it's the same as the current log file
			if (!logPath.empty() && path == logPath) {
				// Stop file logging and clear the log file path
				DisableSpoutLogFile();
			}
			// Remove the file if it exists
			if (_access(path.c_str(), 0) != -1)
				remove(path.c_str());
		}
	}

	// ---------------------------------------------------------
	// Function: DisableSpoutLog
	// Disable logging to console and file
	void DisableSpoutLog()
	{
		CloseSpoutConsole();
		if (!logPath.empty()) {
			if (logFile.is_open())
				logFile.close();
			logPath.clear();
		}
		bEnableLog = false;
		bEnableLogFile = false;
	}

	// ---------------------------------------------------------
	// Function: DisableLogs
	// Disable logs temporarily
	void DisableLogs() {
		bDoLogs = false;
	}

	// ---------------------------------------------------------
	// Function: EnableLogs
	// Enable logging again
	void EnableLogs() {
		bDoLogs = true;
	}

	// ---------------------------------------------------------
	// Function: LogsEnabled
	// Are console logs enabled
	bool LogsEnabled()
	{
		return bEnableLog;
	}

	// ---------------------------------------------------------
	// Function: LogFileEnabled
	// Is file logging enabled
	bool LogFileEnabled()
	{
		return bEnableLogFile;
	}

	// ---------------------------------------------------------
	// Function: GetSpoutLogPath
	// Return the full log file path
	std::string GetSpoutLogPath()
	{
		return logPath;
	}

	// ---------------------------------------------------------
	// Function: GetSpoutLog
	// Return the Spout log file as a string
	// If a file path is not specified, return the current log file
	std::string GetSpoutLog(const char* filepath)
	{
		std::string logstr = "";
		std::string path;

		if (!filepath) return "";

		// Check for specified log file path
		if (*filepath != 0)
			path = filepath;
		else
			path = logPath;

		if (!path.empty()) {
			if (_access(path.c_str(), 0) != -1) { // does the file exist
				// Open the log file
				std::ifstream logstream(path);
				// Source file loaded OK ?
				if (logstream.is_open()) {
					// Get the file text as a single string
					logstr.assign((std::istreambuf_iterator< char >(logstream)), std::istreambuf_iterator< char >());
					logstr += ""; // ensure a NULL terminator
					logstream.close();
				}
			}
		}
		return logstr;
	}

	// ---------------------------------------------------------
	// Function: ShowSpoutLogs
	// Show the Spout log file folder in Windows Explorer
	void ShowSpoutLogs()
	{
		char directory[MAX_PATH]={};

		if (logPath.empty() || _access(logPath.c_str(), 0) == -1) {
			std::string logfilefolder = _getLogPath();
			strcpy_s(directory, MAX_PATH, logfilefolder.c_str());
		}
		else {
			strcpy_s(directory, MAX_PATH, logPath.c_str());
			PathRemoveFileSpecA(directory); // Current log file path
		}

		SHELLEXECUTEINFOA ShExecInfo;
		memset(&ShExecInfo, 0, sizeof(ShExecInfo));
		ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
		ShExecInfo.lpFile = (LPCSTR)directory;
		ShExecInfo.nShow = SW_SHOW;
		ShellExecuteExA(&ShExecInfo);
	}
	
	// ---------------------------------------------------------
	// Function: SetSpoutLogLevel
	// Set the current log level
	void SetSpoutLogLevel(SpoutLogLevel level)
	{
		CurrentLogLevel = level;
	}


	// ---------------------------------------------------------
	// Function: SpoutLog
	// General purpose log - ignore log levels and no log level shown
	void SpoutLog(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_NONE, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: SpoutLogVerbose
	// Verbose - show log for SPOUT_LOG_VERBOSE or above
	void SpoutLogVerbose(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_VERBOSE, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: SpoutLogNotice
	// Notice - show log for SPOUT_LOG_NOTICE or above
	void SpoutLogNotice(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_NOTICE, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: SpoutLogWarning
	// Warning - show log for SPOUT_LOG_WARNING or above
	void SpoutLogWarning(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_WARNING, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: SpoutLogError
	// Error - show log for SPOUT_LOG_ERROR or above
	void SpoutLogError(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_ERROR, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: SpoutLogFatal
	// Fatal - always show log
	void SpoutLogFatal(const char* format, ...)
	{
		va_list args;
		va_start(args, format);
		_doLog(SPOUT_LOG_FATAL, format, args);
		va_end(args);
	}

	// ---------------------------------------------------------
	// Function: _doLog
	// Perform the log
	//
	// Used internally to perform logging. Can also be used externally.
	// The function code can be changed to produce logs as required
	void _doLog(SpoutLogLevel level, const char* format, va_list args)
	{
		if (!format)
			return;

		char currentLog[1024]={}; // allow more than the name length

		// Construct the current log
		vsprintf_s(currentLog, 1024, format, args);

		// Return if logging is paused
		if (!bDoLogs)
			return;

		if (level != SPOUT_LOG_SILENT
			&& CurrentLogLevel != SPOUT_LOG_SILENT
			&& level >= CurrentLogLevel
			&& format != nullptr) {

			// Prevent multiple logs by comparing with the last
			if (strcmp(currentLog, logChars) == 0) {
				// Save the current log as the last
				strcpy_s(logChars, 1024, currentLog);
				return;
			}

			// Save the current log as the last
			strcpy_s(logChars, 1024, currentLog);

			// Console logging
			if (bEnableLog && bConsole) {
				FILE* out = stdout; // Console output
				if (level != SPOUT_LOG_NONE) {
					// Show log level
					fprintf(out, "[%s] ", _levelName(level).c_str());
				}
				// The log
				vfprintf(out, format, args);
				// Newline
				fprintf(out, "\n");

			} // end console log

			// File logging
			if (bEnableLogFile && !logPath.empty()) {
				// Log file output
				// Append to the the current log file so it remains closed
				// No verbose logs for log to file
				if (level != SPOUT_LOG_VERBOSE) {
					logFile.open(logPath, logFile.app);
					if (logFile.is_open()) {
						if (level != SPOUT_LOG_NONE) {
							// Show log level
							logFile << "[" << _levelName(level).c_str()  << "] ";
						}
						// The log and newline
						logFile << currentLog << std::endl;
					}
					logFile.close();
				}
			} // end file log

		}
	}

	// ---------------------------------------------------------
	// Function: _conprint
	// Print to console - (printf replacement).  
	//
	int _conprint(const char* format, ...)
	{

		// Construct the message
		va_list args;
		va_start(args, format);
		vsprintf_s(logChars, 1024, format, args);
		va_end(args);

		//
		// Write to the console without line feed
		//
		// cout and printf do not write if another console is opened by the application.
		// WriteFile writes to either of them.
		//
		DWORD nBytesWritten = 0;
		WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), logChars, (DWORD)strlen(logChars), &nBytesWritten, NULL);
		
		logChars[0]=0;
		return (int)nBytesWritten;
	}


	
	//
	// Group: MessageBox
	//

	// ---------------------------------------------------------
	// Function: SpoutMessageBox
	// MessageBox dialog with optional timeout.
	//
	// Used where a Windows MessageBox would interfere with the application GUI.
	//
	// The dialog closes itself if a timeout is specified.
	int SpoutMessageBox(const char * message, DWORD dwMilliseconds)
	{
		if (!message)
			return 0;
		return SpoutMessageBox(NULL, message, "Message", MB_OK, dwMilliseconds);
	}

	// MessageBox with variable arguments
	int SPOUT_DLLEXP SpoutMessageBox(const char* caption, const char* format, ...)
	{
		std::string strmessage;
		std::string strcaption;

		// Construct the message
		va_list args;
		va_start(args, format);
		vsprintf_s(logChars, 1024, format, args);
		strmessage = logChars;
		va_end(args);

		if(caption && *caption)
			strcaption = caption;
		else
			strcaption = "Message";

		return SpoutMessageBox(NULL, strmessage.c_str(), strcaption.c_str(), MB_OK);

	}



	// ---------------------------------------------------------
	// Function: SpoutMessageBox
	// Messagebox with standard arguments and optional timeout
	//
	// Replaces an existing MessageBox call.
	int SpoutMessageBox(HWND hwnd, LPCSTR message, LPCSTR caption, UINT uType, DWORD dwMilliseconds)
	{
		int iRet = 0;
		char path[MAX_PATH]={};

		std::string spoutmessage = message;

		// Find if there has been a Spout installation with an install path for SpoutPanel.exe
		if (ReadPathFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\SpoutPanel", "InstallPath", path)) {
			// Does SpoutPanel exist ?
			if (_access(path, 0) != -1) {

				//
				// Add optional arguments
				//

				// Text dialog caption
				if (caption && *caption) {
					spoutmessage += " /CAPTION ";
					spoutmessage += caption;
				}

				// If a timeout has been specified, add the timeout option and value
				// SpoutPanel handles the timeout delay
				if (dwMilliseconds > 0) {
					spoutmessage += " /TIMEOUT ";
					spoutmessage += std::to_string((unsigned long long)dwMilliseconds);
				}

				SHELLEXECUTEINFOA ShExecInfo;
				ZeroMemory(&ShExecInfo, sizeof(ShExecInfo));
				ShExecInfo.cbSize = sizeof(SHELLEXECUTEINFO);
				ShExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
				ShExecInfo.hwnd = NULL;
				ShExecInfo.lpVerb = NULL;
				ShExecInfo.lpFile = (LPCSTR)path;
				ShExecInfo.lpParameters = (LPCSTR)spoutmessage.c_str();
				ShExecInfo.lpDirectory = NULL;
				ShExecInfo.nShow = SW_SHOW;
				ShExecInfo.hInstApp = NULL;
				ShellExecuteExA(&ShExecInfo);
				// Returns straight away here but multiple instances of SpoutPanel
				// are prevented in it's WinMain procedure by the mutex.
			}
			else {
				// Registry path OK but no SpoutPanel.exe
				// Use a standard untimed topmost messagebox
				iRet = MessageBoxA(hwnd, spoutmessage.c_str(), caption, (uType | MB_TOPMOST));
			}
		}
		else {
			// No SpoutPanel path registered
			// Use a standard untimed topmost messagebox
			iRet = MessageBoxA(hwnd, spoutmessage.c_str(), caption, (uType | MB_TOPMOST));
		}

		return iRet;
	}

	//
	// Group: Registry utilities
	//

	// Registry functions new for 2.007 including hKey and changed argument order

	// ---------------------------------------------------------
	// Function: ReadDwordFromRegistry
	// Read subkey DWORD value
	bool ReadDwordFromRegistry(HKEY hKey, const char *subkey, const char *valuename, DWORD *pValue)
	{
		if (!subkey || !*subkey || !valuename || !*valuename || !pValue)
			return false;
		

		DWORD dwKey = 0;
		DWORD dwSize = sizeof(DWORD);
		const LONG regres = RegGetValueA(hKey, subkey, valuename, RRF_RT_REG_DWORD, &dwKey, pValue, &dwSize);
		
		if (regres != ERROR_SUCCESS) {
			SpoutLogWarning("ReadDwordFromRegistry - could not read [%s] from registry", valuename);
			return false;
		}

		return true;

	}

	// ---------------------------------------------------------
	// Function: WriteDwordToRegistry
	// Write subkey DWORD value
	bool WriteDwordToRegistry(HKEY hKey, const char *subkey, const char *valuename, DWORD dwValue)
	{
		if (!subkey || !*subkey || !valuename || !*valuename)
			return false;

		HKEY hRegKey = NULL;
		// Does the key already exist ?
		LONG regres = RegOpenKeyExA(hKey, subkey, NULL, KEY_ALL_ACCESS, &hRegKey);
		if (regres != ERROR_SUCCESS) {
			// Create a new key
			regres = RegCreateKeyExA(hKey, subkey, NULL, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hRegKey, NULL);
		}

		if (regres == ERROR_SUCCESS && hRegKey != NULL) {
			// Write the DWORD value
			regres = RegSetValueExA(hRegKey, valuename, 0, REG_DWORD, (BYTE *)&dwValue, 4);
			// For immediate read after write - necessary here because the app might set the values 
			// and read the registry straight away and it might not be available yet.
			// The key must have been opened with the KEY_QUERY_VALUE access right 
			// (included in KEY_ALL_ACCESS)
			RegFlushKey(hRegKey); // needs an open key
			RegCloseKey(hRegKey); // Done with the key
		}

		if (regres != ERROR_SUCCESS) {
			SpoutLogWarning("WriteDwordToRegistry - could not write [%s] to registry", valuename);
			return false;
		}

		return true;
	}

	// ---------------------------------------------------------
	// Function: ReadPathFromRegistry
	// Read subkey character string
	bool ReadPathFromRegistry(HKEY hKey, const char *subkey, const char *valuename, char *filepath, DWORD dwSize)
	{
		if (!subkey || !*subkey || !valuename || !*valuename || !filepath)
			return false;

		HKEY  hRegKey = NULL;
		LONG  regres = 0;
		DWORD dwKey = 0;
		DWORD dwSizePath = dwSize;

		// Does the key exist
		regres = RegOpenKeyExA(hKey, subkey, NULL, KEY_READ, &hRegKey);
		if (regres == ERROR_SUCCESS) {
			// Read the key Filepath value
			regres = RegQueryValueExA(hRegKey, valuename, NULL, &dwKey, (BYTE*)filepath, &dwSizePath);
			RegCloseKey(hRegKey);
			if (regres == ERROR_SUCCESS) {
				return true;
			}
			if (regres == ERROR_MORE_DATA) {
				SpoutLogWarning("ReadPathFromRegistry -  buffer size (%d) not large enough (%d)", dwSize, dwSizePath);
			}
			else {
				SpoutLogWarning("ReadPathFromRegistry - could not read [%s] from registry", valuename);
			}
		}

		// Quit if the key does not exist
		return false;

	}
	
	// ---------------------------------------------------------
	// Function: WritePathToRegistry
	// Write subkey character string
	bool WritePathToRegistry(HKEY hKey, const char *subkey, const char *valuename, const char *filepath)
	{
		if (!subkey || !*subkey || !valuename || !*valuename || !filepath)
			return false;

		HKEY  hRegKey = NULL;
		const DWORD dwSize = (DWORD)strlen(filepath)+1;

		// Does the key already exist ?
		LONG regres = RegOpenKeyExA(hKey, subkey, NULL, KEY_ALL_ACCESS, &hRegKey);
		if (regres != ERROR_SUCCESS) {
			// Create a new key
			regres = RegCreateKeyExA(hKey, subkey, NULL, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hRegKey, NULL);
		}

		if (regres == ERROR_SUCCESS && hRegKey != NULL) {
			// Write the path
			regres = RegSetValueExA(hRegKey, valuename, 0, REG_SZ, (BYTE *)filepath, dwSize);
			RegCloseKey(hRegKey);
		}

		if (regres != ERROR_SUCCESS) {
			SpoutLogWarning("WritePathToRegistry - could not write [%s] to registry", valuename);
			return false;
		}

		return true;

	}

	// ---------------------------------------------------------
	// Function: WriteBinaryToRegistry
	// Write subkey binary hex data string
	bool WriteBinaryToRegistry(HKEY hKey, const char *subkey, const char *valuename, const unsigned char *hexdata, DWORD nChars)
	{
		if (!subkey || !*subkey || !valuename || !*valuename || !hexdata)
			return false;

		HKEY  hRegKey = NULL;
		// Does the key already exist ?
		LONG regres = RegOpenKeyExA(hKey, subkey, NULL, KEY_ALL_ACCESS, &hRegKey);
		if (regres != ERROR_SUCCESS) {
			// Create a new key
			regres = RegCreateKeyExA(hKey, subkey, NULL, NULL, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, NULL, &hRegKey, NULL);
		}

		if (regres == ERROR_SUCCESS && hRegKey != NULL) {
			regres = RegSetValueExA(hRegKey, valuename, 0, REG_BINARY, hexdata, nChars);
			RegCloseKey(hRegKey);
		}

		if (regres != ERROR_SUCCESS) {
			SpoutLogWarning("WriteBinaryToRegistry - could not write to registry");
			return false;
		}

		return true;

	}


	// ---------------------------------------------------------
	// Function: RemovePathFromRegistry
	// Remove subkey value name
	bool RemovePathFromRegistry(HKEY hKey, const char *subkey, const char *valuename)
	{
		if (!subkey || !*subkey || !valuename) {
			SpoutLogWarning("RemovePathFromRegistry - no subkey specified");
			return false;
		}

		HKEY hRegKey = NULL;
		LONG regres = RegOpenKeyExA(hKey, subkey, NULL, KEY_ALL_ACCESS, &hRegKey);
		if (regres == ERROR_SUCCESS) {
			regres = RegDeleteValueA(hRegKey, valuename);
			RegCloseKey(hRegKey);
		}

		if (regres == ERROR_SUCCESS)
			return true;

		// Quit if the key does not exist
		SpoutLogWarning("RemovePathFromRegistry - could not open key [%s]", subkey);
		return false;
	}

	// ---------------------------------------------------------
	// Function: RemoveSubKey
	// Delete a subkey and its values.
	//
	// The "subkey" argument must be a subkey of the key that hKey identifies,
	// but it cannot have subkeys.
	//
	// Note that key names are not case sensitive.
	//
	bool RemoveSubKey(HKEY hKey, const char *subkey)
	{
		if (!subkey || !*subkey)
			return false;

		const LONG lStatus = RegDeleteKeyA(hKey, subkey);
		if (lStatus == ERROR_SUCCESS)
			return true;

		SpoutLogWarning("RemoveSubkey - error #%ld", lStatus);
		return false;
	}

	// ---------------------------------------------------------
	// Function: FindSubKey
	// Find subkey
	bool FindSubKey(HKEY hKey, const char *subkey)
	{
		if (!subkey || !*subkey)
			return false;

		HKEY hRegKey = NULL;
		const LONG lStatus = RegOpenKeyExA(hKey, subkey, NULL, KEY_READ, &hRegKey);
		if(lStatus == ERROR_SUCCESS) {
			RegCloseKey(hRegKey);
			return true;
		}

		SpoutLogWarning("FindSubkey - error #%ld", lStatus);
		return false;

	}


	//
	// Group: Timing
	//
	// Compiler dependent
	//

	// ---------------------------------------------------------
	// Function: 
	// Start timing period
	// void StartTiming() {

	// ---------------------------------------------------------
	// Function: 
	// Stop timing and return microseconds elapsed.
	//
	// Code console output can be enabled for quick timing tests.
	// double EndTiming() {

	// ---------------------------------------------------------
	// Function: ElapsedMicroseconds
	// Microseconds elapsed since epoch
	// Requires std::chrono
	// double ElapsedMicroseconds()

	// ---------------------------------------------------------
	// Function: GetRefreshRate
	// Get system refresh rate
	double GetRefreshRate()
	{
		double frequency = 60.0; // default
		DEVMODE DevMode = {};
		BOOL bResult = true;
		DWORD dwCurrentSettings = 0;
		DevMode.dmSize = sizeof(DEVMODE);
		// Test all the graphics modes
		// https://docs.microsoft.com/en-us/windows/desktop/api/winuser/nf-winuser-enumdisplaysettingsa
		while (bResult) {
			bResult = EnumDisplaySettings(NULL, dwCurrentSettings, &DevMode);
			if (bResult)
				frequency = static_cast<double>(DevMode.dmDisplayFrequency);
			dwCurrentSettings++;
		}
		return frequency;
	}

#ifdef USE_CHRONO

	// Start timing period
	void StartTiming() {
		start = std::chrono::steady_clock::now();
	}

	// Stop timing and return milliseconds elapsed.
	// Code console output can be enabled for quick timing tests.
	double EndTiming() {
		end = std::chrono::steady_clock::now();
		double elapsed = 0;
		elapsed = static_cast<double>(std::chrono::duration_cast<std::chrono::microseconds>(end - start).count()/1000.0);
		return elapsed;
	}

	// Microseconds elapsed since epoch
	double ElapsedMicroseconds()
	{
		const std::chrono::system_clock::time_point timenow = std::chrono::system_clock::now();
		const std::chrono::system_clock::duration duration = timenow.time_since_epoch();
		double timestamp = 0;
		timestamp = static_cast<double>(duration.count()); // nsec/100 - duration period is 100 nanoseconds
		return timestamp / 10.0; // microseconds
	}
#else
	// Start timing period
	void StartTiming() {
		start = GetCounter();
	}

	// Stop timing and return microseconds elapsed.
	// Console output can be enabled for quick timing tests.
	double EndTiming() {
		end = GetCounter();
		return (end-start);
	}

	// -----------------------------------------------
	// Set counter start
	// Used instead of std::chrono for Visual Studio before VS2015
	//
	// Information on using QueryPerformanceFrequency for timing
	// https://docs.microsoft.com/en-us/windows/desktop/SysInfo/acquiring-high-resolution-time-stamps
	//
	void StartCounter()
	{
		LARGE_INTEGER li;
		if (QueryPerformanceFrequency(&li)) {
			// Find the PC frequency if not done yet
			if (PCFreq < 0.0001)
				PCFreq = static_cast<double>(li.QuadPart) / 1000.0;
			// Get the counter start
			QueryPerformanceCounter(&li);
			CounterStart = li.QuadPart;
		}
	}

	// -----------------------------------------------
	// Return msec elapsed since counter start
	double GetCounter()
	{
		LARGE_INTEGER li;
		if (QueryPerformanceCounter(&li)) {
			return static_cast<double>(li.QuadPart - CounterStart) / PCFreq;
		}
		else {
			return 0.0;
		}
	}

#endif


	//
	// Private functions
	//
	namespace
	{
		// Log to file with optional append 
		void _logtofile(bool append)
		{
			bool bNewFile = true;

			// Set default log file if not specified
			// C:\Users\username\AppData\Roaming\Spout\SpoutLog.log
			if (logPath.empty()) {
				logPath = _getLogPath();
				logPath += "\\SpoutLog.log";
			}

			// Check for existence before file open
			if (_access(logPath.c_str(), 0) != -1) {
				bNewFile = false; // File exists already
			}

			// File is created if it does not exist
			if (append) {
				logFile.open(logPath, logFile.app);
			}
			else {
				logFile.open(logPath);
			}

			if (logFile.is_open()) {
				// Date and time to identify the log
				char tmp[128]={};
				time_t datime;
				struct tm tmbuff;
				time(&datime);
				localtime_s(&tmbuff, &datime);
				sprintf_s(tmp, 128, "%4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
					tmbuff.tm_year+1900, tmbuff.tm_mon+1, tmbuff.tm_mday,
					tmbuff.tm_hour, tmbuff.tm_min, tmbuff.tm_sec);

				if (append && !bNewFile) {
					logFile << "   " << tmp << std::endl;
				}
				else {
					logFile << "========================" << std::endl;
					logFile << "    Spout log file" << std::endl;
					logFile << "========================" << std::endl;
					logFile << " " << tmp << std::endl;
				}
				logFile.close();
			}
			else {
				// disable file writes and use a console instead
				logPath.clear();
			}
		}

		// Get the default log file path
		std::string _getLogPath()
		{
			char logpath[MAX_PATH]={};
			logpath[0] = 0;

			// Retrieve user %appdata% environment variable
			char *appdatapath = nullptr;
			size_t len = 0;
			bool bSuccess = true;
			errno_t err = 0;
			err = _dupenv_s(&appdatapath, &len, "APPDATA");
			if (err == 0 && appdatapath) {
				strcpy_s(logpath, MAX_PATH, appdatapath);
				strcat_s(logpath, MAX_PATH, "\\Spout");
				if (_access(logpath, 0) == -1) {
					if (!CreateDirectoryA(logpath, NULL)) {
						bSuccess = false;
					}
				}
			}
			else {
				bSuccess = false;
			}

			if (!bSuccess) {
				// _dupenv_s or CreateDirectory failed
				// Find the path of the executable
				GetModuleFileNameA(NULL, (LPSTR)logpath, sizeof(logpath));
				PathRemoveFileSpecA((LPSTR)logpath);
			}

			return logpath;
		}

		// Create a full log file path given the name passed in
		std::string _getLogFilePath(const char* filename)
		{
			std::string logFilePath; // full path of the log file

			std::string path;
			if (!filename) {
				// Use the executable name if no filename was passed in
				char name[MAX_PATH]={};
				if (GetModuleFileNameA(GetCurrentModule(), name, MAX_PATH) > 0) {
					PathStripPathA(name);
					PathRemoveExtensionA(name);
					strcat_s(name, MAX_PATH, ".log"); // Executable name
					path = _getLogPath(); // C:\Users\username\AppData\Roaming\Spout\application.log
					path += "\\";
					path += name; // Full path
				}
			}
			else {
				path = filename;
			}

			if (!path.empty()) {

				char fname[MAX_PATH]={};
				strcpy_s(fname, MAX_PATH, path.c_str());
				PathRemoveBackslashA(fname);

				// Path without a filename
				if (PathIsDirectoryA(fname)) {
					logFilePath = fname;
					logFilePath += "\\SpoutLog.log";
				}

				// Filename without a path
				else if (PathIsFileSpecA(fname)) {
					// Add an extension if none supplied
					if (PathFindExtensionA(fname) == 0)
						strcat_s(fname, MAX_PATH, ".log");
					logFilePath = _getLogPath();
					logFilePath += "\\";
					logFilePath += fname;
				}

				// Full path with a filename
				else if (PathFindFileNameA(fname)) {
					// Add an extension if none supplied
					if (!PathFindExtensionA(fname))
						strcat_s(fname, MAX_PATH, ".log");
					logFilePath = fname;
				}
			}
			else {
				// If all options fail, logPath is empty and "SpoutLog.log" is used
				logFilePath = "SpoutLog.log";
			}

			return logFilePath;
		}

		// Get the name for the current log level
		std::string _levelName(SpoutLogLevel level) {

			std::string name = "";

			switch (level) {
				case SPOUT_LOG_SILENT:
					name = "silent";
					break;
				case SPOUT_LOG_VERBOSE:
					name = "verbose";
					break;
				case SPOUT_LOG_NOTICE:
					name = "notice";
					break;
				case SPOUT_LOG_WARNING:
					name = "warning";
					break;
				case SPOUT_LOG_ERROR:
					name = "error";
					break;
				case SPOUT_LOG_FATAL:
					name = "fatal";
					break;
				default:
					break;
			}

			return name;
		}


		//
		// Used internally for NVIDIA profile functions
		//

		//
		// Get or Set the current mode directly from or to the NVIDIA base profile.
		// They can be read from the Spout registry keys "NvidiaGPUmode" and "NvidiaThreaded"
		// after being set by SpoutSettings, but may have been changed independently.
		// This avoids the need for including the NVAPI library in the Spout library.
		//
		// Start SpoutSettings.exe with a command line which writes the mode value 
		// to the registry and reads back the registry value for the required mode.
		//
		// Currently only two keys are supported.
		//
		// NvidiaGPUmode - preferred GPU setting.
		// Fails for unsupported hardware and returns -1
		// 0 - high performance : 1 - integrated : 2 - auto select : -1 - fail
		// "Software\\Leading Edge\\Spout", "NvidiaGPUmode"
		//
		// NvidiaThreaded - threaded optimization.
		// 0 - auto : 1 - on : 2 - off
		// "Software\\Leading Edge\\Spout", "NvidiaThreaded"
		//
		bool GetNVIDIAmode(const char *command, int * mode)
		{
			if (!mode || !command)
				return false;

			char exePath[MAX_PATH]={};
			if (!ReadPathFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "SpoutSettings", exePath)) {
				SpoutLogError("spoututils::GetNVIDIAmode - SpoutSettings path not found");
				return false;
			}

			if (!PathFileExistsA(exePath)) {
				SpoutLogError("spoututils::GetNVIDIAmode - SpoutSettings.exe not found");
				return false;
			}

			// SpoutSettings -getCommand
			// Returns mode in registry
			char path[MAX_PATH]={};
			sprintf_s(path, MAX_PATH, "%s -get%s", exePath, command);
			if (ExecuteProcess(path)) {
				DWORD dwMode = 0xffff;
				if (ReadDwordFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", command, &dwMode)) {
					*mode = (int)dwMode;
					return true;
				}
				else {
					SpoutLogError("spoututils::GetNVIDIAmode -  could not read setting from registry");
				}
			}
			else {
				SpoutLogError("spoututils::GetNVIDIAmode -  could not start SpoutSettings");
			}
			return false;
		}

		// Set the current mode to the NVIDIA base profile
		// Starts SpoutSettings.exe with a command line
		// which writes the mode value to the Spout registry
		bool SetNVIDIAmode(const char *command, int mode)
		{
			if (!command)
				return false;

			// Find SpoutSettings path
			char exePath[MAX_PATH]={};
			if (!ReadPathFromRegistry(HKEY_CURRENT_USER, "Software\\Leading Edge\\Spout", "SpoutSettings", exePath)) {
				SpoutLogError("spoututils::SetNVIDIAmode - SpoutSettings path not found");
				return false;
			}

			if (!PathFileExistsA(exePath)) {
				SpoutLogError("spoututils::SetNVIDIAmode - SpoutSettings.exe not found");
				return false;
			}

			// SpoutSettings -setCommand mode
			// Sets the required mode and writes it to the registry
			char path[MAX_PATH]={};
			sprintf_s(path, MAX_PATH, "%s -set%s %d", exePath, command, mode);
			if (ExecuteProcess(path))
				return true;

			return false;
		}

		// Open process and wait for completion
		bool ExecuteProcess(const char *path)
		{
			if (!path)
				return false;

			DWORD dwExitCode = 0; // Exit code when process terminates
			STARTUPINFOA si = { sizeof(STARTUPINFO) };
			bool bRet = false;

			ZeroMemory((void *)&si, sizeof(STARTUPINFO));
			si.cb = sizeof(STARTUPINFO);
			si.dwFlags = STARTF_USESHOWWINDOW;
			si.wShowWindow = SW_HIDE;
			PROCESS_INFORMATION pi;
			SetCursor(LoadCursor(NULL, IDC_WAIT));
			if (CreateProcessA(NULL, (LPSTR)path, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {
				// Wait for CreateProcess to finish
				double elapsed = 0.0;
				if (pi.hProcess) {
					StartTiming(); // for 1 second timeout
					do {
						if (!GetExitCodeProcess(pi.hProcess, &dwExitCode)) {
							bRet = false;
							break;
						}
						elapsed = EndTiming(); // msec
					} while (dwExitCode == STILL_ACTIVE && elapsed < 1000.0);
					bRet = true;
				}
				if (pi.hProcess) CloseHandle(pi.hProcess);
				if (pi.hThread) CloseHandle(pi.hThread);
			}
			else {
				SpoutLogError("spoututils::ExecuteProcess - CreateProcess failed\n    %s", path);
				bRet = false;
			}
			SetCursor(LoadCursor(NULL, IDC_ARROW));

			return bRet;
		}
		
	} // end private namespace

} // end namespace spoututils
