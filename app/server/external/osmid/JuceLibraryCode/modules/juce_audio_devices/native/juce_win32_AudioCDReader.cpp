/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2015 - ROLI Ltd.

   Permission is granted to use this software under the terms of either:
   a) the GPL v2 (or any later version)
   b) the Affero GPL v3

   Details of these licenses can be found at: www.gnu.org/licenses

   JUCE is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
   A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

   ------------------------------------------------------------------------------

   To release a closed-source product which uses JUCE, commercial licenses are
   available: visit www.juce.com for more information.

  ==============================================================================
*/

namespace CDReaderHelpers
{

#define FILE_ANY_ACCESS 0
#ifndef FILE_READ_ACCESS
 #define FILE_READ_ACCESS 1
#endif
#ifndef FILE_WRITE_ACCESS
 #define FILE_WRITE_ACCESS 2
#endif

#define METHOD_BUFFERED 0
#define IOCTL_SCSI_BASE 4
#define SCSI_IOCTL_DATA_OUT          0
#define SCSI_IOCTL_DATA_IN           1
#define SCSI_IOCTL_DATA_UNSPECIFIED  2

#define CTL_CODE2(DevType, Function, Method, Access) (((DevType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method))
#define IOCTL_SCSI_PASS_THROUGH_DIRECT  CTL_CODE2( IOCTL_SCSI_BASE, 0x0405, METHOD_BUFFERED, FILE_READ_ACCESS | FILE_WRITE_ACCESS )
#define IOCTL_SCSI_GET_ADDRESS          CTL_CODE2( IOCTL_SCSI_BASE, 0x0406, METHOD_BUFFERED, FILE_ANY_ACCESS )

#define SENSE_LEN         14
#define SRB_ENABLE_RESIDUAL_COUNT 0x04
#define SRB_DIR_IN        0x08
#define SRB_DIR_OUT       0x10
#define SRB_EVENT_NOTIFY  0x40
#define SC_HA_INQUIRY     0x00
#define SC_GET_DEV_TYPE   0x01
#define SC_EXEC_SCSI_CMD  0x02
#define SS_PENDING        0x00
#define SS_COMP           0x01
#define SS_ERR            0x04

enum
{
    READTYPE_ANY = 0,
    READTYPE_ATAPI1 = 1,
    READTYPE_ATAPI2 = 2,
    READTYPE_READ6 = 3,
    READTYPE_READ10 = 4,
    READTYPE_READ_D8 = 5,
    READTYPE_READ_D4 = 6,
    READTYPE_READ_D4_1 = 7,
    READTYPE_READ10_2 = 8
};

struct SCSI_PASS_THROUGH
{
    USHORT Length;
    UCHAR ScsiStatus;
    UCHAR PathId;
    UCHAR TargetId;
    UCHAR Lun;
    UCHAR CdbLength;
    UCHAR SenseInfoLength;
    UCHAR DataIn;
    ULONG DataTransferLength;
    ULONG TimeOutValue;
    ULONG DataBufferOffset;
    ULONG SenseInfoOffset;
    UCHAR Cdb[16];
};

struct SCSI_PASS_THROUGH_DIRECT
{
    USHORT Length;
    UCHAR ScsiStatus;
    UCHAR PathId;
    UCHAR TargetId;
    UCHAR Lun;
    UCHAR CdbLength;
    UCHAR SenseInfoLength;
    UCHAR DataIn;
    ULONG DataTransferLength;
    ULONG TimeOutValue;
    PVOID DataBuffer;
    ULONG SenseInfoOffset;
    UCHAR Cdb[16];
};

struct SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER
{
    SCSI_PASS_THROUGH_DIRECT spt;
    ULONG Filler;
    UCHAR ucSenseBuf[32];
};

struct SCSI_ADDRESS
{
    ULONG Length;
    UCHAR PortNumber;
    UCHAR PathId;
    UCHAR TargetId;
    UCHAR Lun;
};

#pragma pack(1)

struct SRB_GDEVBlock
{
    BYTE SRB_Cmd;
    BYTE SRB_Status;
    BYTE SRB_HaID;
    BYTE SRB_Flags;
    DWORD SRB_Hdr_Rsvd;
    BYTE SRB_Target;
    BYTE SRB_Lun;
    BYTE SRB_DeviceType;
    BYTE SRB_Rsvd1;
    BYTE pad[68];
};


struct SRB_ExecSCSICmd
{
    BYTE SRB_Cmd;
    BYTE SRB_Status;
    BYTE SRB_HaID;
    BYTE SRB_Flags;
    DWORD SRB_Hdr_Rsvd;
    BYTE SRB_Target;
    BYTE SRB_Lun;
    WORD SRB_Rsvd1;
    DWORD SRB_BufLen;
    BYTE *SRB_BufPointer;
    BYTE SRB_SenseLen;
    BYTE SRB_CDBLen;
    BYTE SRB_HaStat;
    BYTE SRB_TargStat;
    VOID *SRB_PostProc;
    BYTE SRB_Rsvd2[20];
    BYTE CDBByte[16];
    BYTE SenseArea[SENSE_LEN + 2];
};

struct SRB
{
    BYTE SRB_Cmd;
    BYTE SRB_Status;
    BYTE SRB_HaId;
    BYTE SRB_Flags;
    DWORD SRB_Hdr_Rsvd;
};

struct TOCTRACK
{
    BYTE rsvd;
    BYTE ADR;
    BYTE trackNumber;
    BYTE rsvd2;
    BYTE addr[4];
};

struct TOC
{
    WORD tocLen;
    BYTE firstTrack;
    BYTE lastTrack;
    TOCTRACK tracks[100];
};

#pragma pack()

//==============================================================================
struct CDDeviceDescription
{
    CDDeviceDescription()  : ha (0), tgt (0), lun (0), scsiDriveLetter (0)
    {
    }

    void createDescription (const char* data)
    {
        description << String (data + 8, 8).trim() // vendor
                    << ' ' << String (data + 16, 16).trim() // product id
                    << ' ' << String (data + 32, 4).trim(); // rev
    }

    String description;
    BYTE ha, tgt, lun;
    char scsiDriveLetter; // will be 0 if not using scsi
};

//==============================================================================
class CDReadBuffer
{
public:
    CDReadBuffer (const int numberOfFrames)
        : startFrame (0), numFrames (0), dataStartOffset (0),
          dataLength (0), bufferSize (2352 * numberOfFrames), index (0),
          buffer (bufferSize), wantsIndex (false)
    {
    }

    bool isZero() const noexcept
    {
        for (int i = 0; i < dataLength; ++i)
            if (buffer [dataStartOffset + i] != 0)
                return false;

        return true;
    }

    int startFrame, numFrames, dataStartOffset;
    int dataLength, bufferSize, index;
    HeapBlock<BYTE> buffer;
    bool wantsIndex;
};

class CDDeviceHandle;

//==============================================================================
class CDController
{
public:
    CDController() : initialised (false) {}
    virtual ~CDController() {}

    virtual bool read (CDReadBuffer&) = 0;
    virtual void shutDown() {}

    bool readAudio (CDReadBuffer& rb, CDReadBuffer* overlapBuffer = 0);
    int getLastIndex();

public:
    CDDeviceHandle* deviceInfo;
    int framesToCheck, framesOverlap;
    bool initialised;

    void prepare (SRB_ExecSCSICmd& s);
    void perform (SRB_ExecSCSICmd& s);
    void setPaused (bool paused);
};


//==============================================================================
class CDDeviceHandle
{
public:
    CDDeviceHandle (const CDDeviceDescription& device, HANDLE scsiHandle_)
        : info (device), scsiHandle (scsiHandle_), readType (READTYPE_ANY)
    {
    }

    ~CDDeviceHandle()
    {
        if (controller != nullptr)
        {
            controller->shutDown();
            controller = 0;
        }

        if (scsiHandle != 0)
            CloseHandle (scsiHandle);
    }

    bool readTOC (TOC* lpToc);
    bool readAudio (CDReadBuffer& buffer, CDReadBuffer* overlapBuffer = 0);
    void openDrawer (bool shouldBeOpen);
    void performScsiCommand (HANDLE event, SRB_ExecSCSICmd& s);

    CDDeviceDescription info;
    HANDLE scsiHandle;
    BYTE readType;

private:
    ScopedPointer<CDController> controller;

    bool testController (int readType, CDController* newController, CDReadBuffer& bufferToUse);
};

//==============================================================================
HANDLE createSCSIDeviceHandle (const char driveLetter)
{
    TCHAR devicePath[] = { '\\', '\\', '.', '\\', driveLetter, ':', 0, 0 };
    DWORD flags = GENERIC_READ | GENERIC_WRITE;
    HANDLE h = CreateFile (devicePath, flags, FILE_SHARE_WRITE | FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if (h == INVALID_HANDLE_VALUE)
    {
        flags ^= GENERIC_WRITE;
        h = CreateFile (devicePath, flags, FILE_SHARE_WRITE | FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    }

    return h;
}

void findCDDevices (Array<CDDeviceDescription>& list)
{
    for (char driveLetter = 'b'; driveLetter <= 'z'; ++driveLetter)
    {
        TCHAR drivePath[] = { driveLetter, ':', '\\', 0, 0 };

        if (GetDriveType (drivePath) == DRIVE_CDROM)
        {
            HANDLE h = createSCSIDeviceHandle (driveLetter);

            if (h != INVALID_HANDLE_VALUE)
            {
                char buffer[100] = { 0 };

                SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER p = { 0 };
                p.spt.Length             = sizeof (SCSI_PASS_THROUGH);
                p.spt.CdbLength          = 6;
                p.spt.SenseInfoLength    = 24;
                p.spt.DataIn             = SCSI_IOCTL_DATA_IN;
                p.spt.DataTransferLength = sizeof (buffer);
                p.spt.TimeOutValue       = 2;
                p.spt.DataBuffer         = buffer;
                p.spt.SenseInfoOffset    = offsetof (SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER, ucSenseBuf);
                p.spt.Cdb[0]             = 0x12;
                p.spt.Cdb[4]             = 100;

                DWORD bytesReturned = 0;

                if (DeviceIoControl (h, IOCTL_SCSI_PASS_THROUGH_DIRECT,
                                     &p, sizeof (p), &p, sizeof (p),
                                     &bytesReturned, 0) != 0)
                {
                    CDDeviceDescription dev;
                    dev.scsiDriveLetter = driveLetter;
                    dev.createDescription (buffer);

                    SCSI_ADDRESS scsiAddr = { 0 };
                    scsiAddr.Length = sizeof (scsiAddr);

                    if (DeviceIoControl (h, IOCTL_SCSI_GET_ADDRESS,
                                         0, 0, &scsiAddr, sizeof (scsiAddr),
                                         &bytesReturned, 0) != 0)
                    {
                        dev.ha = scsiAddr.PortNumber;
                        dev.tgt = scsiAddr.TargetId;
                        dev.lun = scsiAddr.Lun;
                        list.add (dev);
                    }
                }

                CloseHandle (h);
            }
        }
    }
}

DWORD performScsiPassThroughCommand (SRB_ExecSCSICmd* const srb, const char driveLetter,
                                     HANDLE& deviceHandle, const bool retryOnFailure)
{
    SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER s = { 0 };
    s.spt.Length = sizeof (SCSI_PASS_THROUGH);
    s.spt.CdbLength = srb->SRB_CDBLen;

    s.spt.DataIn = (BYTE) ((srb->SRB_Flags & SRB_DIR_IN)
                            ? SCSI_IOCTL_DATA_IN
                            : ((srb->SRB_Flags & SRB_DIR_OUT)
                                ? SCSI_IOCTL_DATA_OUT
                                : SCSI_IOCTL_DATA_UNSPECIFIED));

    s.spt.DataTransferLength = srb->SRB_BufLen;
    s.spt.TimeOutValue = 5;
    s.spt.DataBuffer = srb->SRB_BufPointer;
    s.spt.SenseInfoOffset = offsetof (SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER, ucSenseBuf);

    memcpy (s.spt.Cdb, srb->CDBByte, srb->SRB_CDBLen);

    srb->SRB_Status = SS_ERR;
    srb->SRB_TargStat = 0x0004;

    DWORD bytesReturned = 0;

    if (DeviceIoControl (deviceHandle, IOCTL_SCSI_PASS_THROUGH_DIRECT,
                         &s, sizeof (s), &s, sizeof (s), &bytesReturned, 0) != 0)
    {
        srb->SRB_Status = SS_COMP;
    }
    else if (retryOnFailure)
    {
        const DWORD error = GetLastError();

        if ((error == ERROR_MEDIA_CHANGED) || (error == ERROR_INVALID_HANDLE))
        {
            if (error != ERROR_INVALID_HANDLE)
                CloseHandle (deviceHandle);

            deviceHandle = createSCSIDeviceHandle (driveLetter);

            return performScsiPassThroughCommand (srb, driveLetter, deviceHandle, false);
        }
    }

    return srb->SRB_Status;
}


//==============================================================================
// Controller types..

class ControllerType1  : public CDController
{
public:
    ControllerType1() {}

    bool read (CDReadBuffer& rb)
    {
        if (rb.numFrames * 2352 > rb.bufferSize)
            return false;

        SRB_ExecSCSICmd s;
        prepare (s);
        s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
        s.SRB_BufLen = rb.bufferSize;
        s.SRB_BufPointer = rb.buffer;
        s.SRB_CDBLen = 12;
        s.CDBByte[0] = 0xBE;
        s.CDBByte[3] = (BYTE) ((rb.startFrame >> 16) & 0xFF);
        s.CDBByte[4] = (BYTE) ((rb.startFrame >> 8) & 0xFF);
        s.CDBByte[5] = (BYTE) (rb.startFrame & 0xFF);
        s.CDBByte[8] = (BYTE) (rb.numFrames & 0xFF);
        s.CDBByte[9] = (BYTE) (deviceInfo->readType == READTYPE_ATAPI1 ? 0x10 : 0xF0);
        perform (s);

        if (s.SRB_Status != SS_COMP)
            return false;

        rb.dataLength = rb.numFrames * 2352;
        rb.dataStartOffset = 0;
        return true;
    }
};

//==============================================================================
class ControllerType2  : public CDController
{
public:
    ControllerType2() {}

    void shutDown()
    {
        if (initialised)
        {
            BYTE bufPointer[] = { 0, 0, 0, 8, 83, 0, 0, 0, 0, 0, 8, 0 };

            SRB_ExecSCSICmd s;
            prepare (s);
            s.SRB_Flags = SRB_EVENT_NOTIFY | SRB_ENABLE_RESIDUAL_COUNT;
            s.SRB_BufLen = 0x0C;
            s.SRB_BufPointer = bufPointer;
            s.SRB_CDBLen = 6;
            s.CDBByte[0] = 0x15;
            s.CDBByte[4] = 0x0C;
            perform (s);
        }
    }

    bool init()
    {
        SRB_ExecSCSICmd s;
        s.SRB_Status = SS_ERR;

        if (deviceInfo->readType == READTYPE_READ10_2)
        {
            BYTE bufPointer1[] = { 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 9, 48, 35, 6, 0, 0, 0, 0, 0, 128 };
            BYTE bufPointer2[] = { 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 9, 48, 1, 6, 32, 7, 0, 0, 0, 0 };

            for (int i = 0; i < 2; ++i)
            {
                prepare (s);
                s.SRB_Flags = SRB_EVENT_NOTIFY;
                s.SRB_BufLen = 0x14;
                s.SRB_BufPointer = (i == 0) ? bufPointer1 : bufPointer2;
                s.SRB_CDBLen = 6;
                s.CDBByte[0] = 0x15;
                s.CDBByte[1] = 0x10;
                s.CDBByte[4] = 0x14;
                perform (s);

                if (s.SRB_Status != SS_COMP)
                    return false;
            }
        }
        else
        {
            BYTE bufPointer[] = { 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 9, 48 };

            prepare (s);
            s.SRB_Flags = SRB_EVENT_NOTIFY;
            s.SRB_BufLen = 0x0C;
            s.SRB_BufPointer = bufPointer;
            s.SRB_CDBLen = 6;
            s.CDBByte[0] = 0x15;
            s.CDBByte[4] = 0x0C;
            perform (s);
        }

        return s.SRB_Status == SS_COMP;
    }

    bool read (CDReadBuffer& rb)
    {
        if (rb.numFrames * 2352 > rb.bufferSize)
            return false;

        if (! initialised)
        {
            initialised = init();

            if (! initialised)
                return false;
        }

        SRB_ExecSCSICmd s;
        prepare (s);
        s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
        s.SRB_BufLen = rb.bufferSize;
        s.SRB_BufPointer = rb.buffer;
        s.SRB_CDBLen = 10;
        s.CDBByte[0] = 0x28;
        s.CDBByte[1] = (BYTE) (deviceInfo->info.lun << 5);
        s.CDBByte[3] = (BYTE) ((rb.startFrame >> 16) & 0xFF);
        s.CDBByte[4] = (BYTE) ((rb.startFrame >> 8) & 0xFF);
        s.CDBByte[5] = (BYTE) (rb.startFrame & 0xFF);
        s.CDBByte[8] = (BYTE) (rb.numFrames & 0xFF);
        perform (s);

        if (s.SRB_Status != SS_COMP)
            return false;

        rb.dataLength = rb.numFrames * 2352;
        rb.dataStartOffset = 0;
        return true;
    }
};

//==============================================================================
class ControllerType3  : public CDController
{
public:
    ControllerType3() {}

    bool read (CDReadBuffer& rb)
    {
        if (rb.numFrames * 2352 > rb.bufferSize)
            return false;

        if (! initialised)
        {
            setPaused (false);
            initialised = true;
        }

        SRB_ExecSCSICmd s;
        prepare (s);
        s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
        s.SRB_BufLen = rb.numFrames * 2352;
        s.SRB_BufPointer = rb.buffer;
        s.SRB_CDBLen = 12;
        s.CDBByte[0] = 0xD8;
        s.CDBByte[3] = (BYTE) ((rb.startFrame >> 16) & 0xFF);
        s.CDBByte[4] = (BYTE) ((rb.startFrame >> 8) & 0xFF);
        s.CDBByte[5] = (BYTE) (rb.startFrame & 0xFF);
        s.CDBByte[9] = (BYTE) (rb.numFrames & 0xFF);
        perform (s);

        if (s.SRB_Status != SS_COMP)
            return false;

        rb.dataLength = rb.numFrames * 2352;
        rb.dataStartOffset = 0;
        return true;
    }
};

//==============================================================================
class ControllerType4  : public CDController
{
public:
    ControllerType4() {}

    bool selectD4Mode()
    {
        BYTE bufPointer[12] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 48 };

        SRB_ExecSCSICmd s;
        prepare (s);
        s.SRB_Flags = SRB_EVENT_NOTIFY;
        s.SRB_CDBLen = 6;
        s.SRB_BufLen = 12;
        s.SRB_BufPointer = bufPointer;
        s.CDBByte[0] = 0x15;
        s.CDBByte[1] = 0x10;
        s.CDBByte[4] = 0x08;
        perform (s);

        return s.SRB_Status == SS_COMP;
    }

    bool read (CDReadBuffer& rb)
    {
        if (rb.numFrames * 2352 > rb.bufferSize)
            return false;

        if (! initialised)
        {
            setPaused (true);

            if (deviceInfo->readType == READTYPE_READ_D4_1)
                selectD4Mode();

            initialised = true;
        }

        SRB_ExecSCSICmd s;
        prepare (s);
        s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
        s.SRB_BufLen = rb.bufferSize;
        s.SRB_BufPointer = rb.buffer;
        s.SRB_CDBLen = 10;
        s.CDBByte[0] = 0xD4;
        s.CDBByte[3] = (BYTE) ((rb.startFrame >> 16) & 0xFF);
        s.CDBByte[4] = (BYTE) ((rb.startFrame >> 8) & 0xFF);
        s.CDBByte[5] = (BYTE) (rb.startFrame & 0xFF);
        s.CDBByte[8] = (BYTE) (rb.numFrames & 0xFF);
        perform (s);

        if (s.SRB_Status != SS_COMP)
            return false;

        rb.dataLength = rb.numFrames * 2352;
        rb.dataStartOffset = 0;
        return true;
    }
};


//==============================================================================
void CDController::prepare (SRB_ExecSCSICmd& s)
{
    zerostruct (s);
    s.SRB_Cmd = SC_EXEC_SCSI_CMD;
    s.SRB_HaID = deviceInfo->info.ha;
    s.SRB_Target = deviceInfo->info.tgt;
    s.SRB_Lun = deviceInfo->info.lun;
    s.SRB_SenseLen = SENSE_LEN;
}

void CDController::perform (SRB_ExecSCSICmd& s)
{
    s.SRB_PostProc = CreateEvent (0, TRUE, FALSE, 0);

    deviceInfo->performScsiCommand (s.SRB_PostProc, s);
}

void CDController::setPaused (bool paused)
{
    SRB_ExecSCSICmd s;
    prepare (s);
    s.SRB_Flags = SRB_EVENT_NOTIFY;
    s.SRB_CDBLen = 10;
    s.CDBByte[0] = 0x4B;
    s.CDBByte[8] = (BYTE) (paused ? 0 : 1);
    perform (s);
}

bool CDController::readAudio (CDReadBuffer& rb, CDReadBuffer* overlapBuffer)
{
    if (overlapBuffer != nullptr)
    {
        const bool canDoJitter = (overlapBuffer->bufferSize >= 2352 * framesToCheck);
        const bool doJitter = canDoJitter && ! overlapBuffer->isZero();

        if (doJitter
             && overlapBuffer->startFrame > 0
             && overlapBuffer->numFrames > 0
             && overlapBuffer->dataLength > 0)
        {
            const int numFrames = rb.numFrames;

            if (overlapBuffer->startFrame == (rb.startFrame - framesToCheck))
            {
                rb.startFrame -= framesOverlap;

                if (framesToCheck < framesOverlap
                     && numFrames + framesOverlap <= rb.bufferSize / 2352)
                    rb.numFrames += framesOverlap;
            }
            else
            {
                overlapBuffer->dataLength = 0;
                overlapBuffer->startFrame = 0;
                overlapBuffer->numFrames = 0;
            }
        }

        if (! read (rb))
            return false;

        if (doJitter)
        {
            const int checkLen = framesToCheck * 2352;
            const int maxToCheck = rb.dataLength - checkLen;

            if (overlapBuffer->dataLength == 0 || overlapBuffer->isZero())
                return true;

            BYTE* const p = overlapBuffer->buffer + overlapBuffer->dataStartOffset;
            bool found = false;

            for (int i = 0; i < maxToCheck; ++i)
            {
                if (memcmp (p, rb.buffer + i, checkLen) == 0)
                {
                    i += checkLen;
                    rb.dataStartOffset = i;
                    rb.dataLength -= i;
                    rb.startFrame = overlapBuffer->startFrame + framesToCheck;
                    found = true;
                    break;
                }
            }

            rb.numFrames = rb.dataLength / 2352;
            rb.dataLength = 2352 * rb.numFrames;

            if (! found)
                return false;
        }

        if (canDoJitter)
        {
            memcpy (overlapBuffer->buffer,
                    rb.buffer + rb.dataStartOffset + 2352 * (rb.numFrames - framesToCheck),
                    2352 * framesToCheck);

            overlapBuffer->startFrame = rb.startFrame + rb.numFrames - framesToCheck;
            overlapBuffer->numFrames = framesToCheck;
            overlapBuffer->dataLength = 2352 * framesToCheck;
            overlapBuffer->dataStartOffset = 0;
        }
        else
        {
            overlapBuffer->startFrame = 0;
            overlapBuffer->numFrames = 0;
            overlapBuffer->dataLength = 0;
        }

        return true;
    }

    return read (rb);
}

int CDController::getLastIndex()
{
    char qdata[100];

    SRB_ExecSCSICmd s;
    prepare (s);
    s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
    s.SRB_BufLen = sizeof (qdata);
    s.SRB_BufPointer = (BYTE*) qdata;
    s.SRB_CDBLen = 12;
    s.CDBByte[0] = 0x42;
    s.CDBByte[1] = (BYTE) (deviceInfo->info.lun << 5);
    s.CDBByte[2] = 64;
    s.CDBByte[3] = 1; // get current position
    s.CDBByte[7] = 0;
    s.CDBByte[8] = (BYTE) sizeof (qdata);
    perform (s);

    return s.SRB_Status == SS_COMP ? qdata[7] : 0;
}

//==============================================================================
bool CDDeviceHandle::readTOC (TOC* lpToc)
{
    SRB_ExecSCSICmd s = { 0 };
    s.SRB_Cmd = SC_EXEC_SCSI_CMD;
    s.SRB_HaID = info.ha;
    s.SRB_Target = info.tgt;
    s.SRB_Lun = info.lun;
    s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
    s.SRB_BufLen = 0x324;
    s.SRB_BufPointer = (BYTE*) lpToc;
    s.SRB_SenseLen = 0x0E;
    s.SRB_CDBLen = 0x0A;
    s.SRB_PostProc = CreateEvent (0, TRUE, FALSE, 0);
    s.CDBByte[0] = 0x43;
    s.CDBByte[1] = 0x00;
    s.CDBByte[7] = 0x03;
    s.CDBByte[8] = 0x24;

    performScsiCommand (s.SRB_PostProc, s);
    return (s.SRB_Status == SS_COMP);
}

void CDDeviceHandle::performScsiCommand (HANDLE event, SRB_ExecSCSICmd& s)
{
    ResetEvent (event);
    DWORD status = performScsiPassThroughCommand ((SRB_ExecSCSICmd*) &s, info.scsiDriveLetter, scsiHandle, true);

    if (status == SS_PENDING)
        WaitForSingleObject (event, 4000);

    CloseHandle (event);
}

bool CDDeviceHandle::readAudio (CDReadBuffer& buffer, CDReadBuffer* overlapBuffer)
{
    if (controller == 0)
    {
           testController (READTYPE_ATAPI2,    new ControllerType1(), buffer)
        || testController (READTYPE_ATAPI1,    new ControllerType1(), buffer)
        || testController (READTYPE_READ10_2,  new ControllerType2(), buffer)
        || testController (READTYPE_READ10,    new ControllerType2(), buffer)
        || testController (READTYPE_READ_D8,   new ControllerType3(), buffer)
        || testController (READTYPE_READ_D4,   new ControllerType4(), buffer)
        || testController (READTYPE_READ_D4_1, new ControllerType4(), buffer);
    }

    buffer.index = 0;

    if (controller != nullptr && controller->readAudio (buffer, overlapBuffer))
    {
        if (buffer.wantsIndex)
            buffer.index = controller->getLastIndex();

        return true;
    }

    return false;
}

void CDDeviceHandle::openDrawer (bool shouldBeOpen)
{
    if (shouldBeOpen)
    {
        if (controller != nullptr)
        {
            controller->shutDown();
            controller = nullptr;
        }

        if (scsiHandle != 0)
        {
            CloseHandle (scsiHandle);
            scsiHandle = 0;
        }
    }

    SRB_ExecSCSICmd s = { 0 };
    s.SRB_Cmd = SC_EXEC_SCSI_CMD;
    s.SRB_HaID = info.ha;
    s.SRB_Target = info.tgt;
    s.SRB_Lun = info.lun;
    s.SRB_SenseLen = SENSE_LEN;
    s.SRB_Flags = SRB_DIR_IN | SRB_EVENT_NOTIFY;
    s.SRB_BufLen = 0;
    s.SRB_BufPointer = 0;
    s.SRB_CDBLen = 12;
    s.CDBByte[0] = 0x1b;
    s.CDBByte[1] = (BYTE) (info.lun << 5);
    s.CDBByte[4] = (BYTE) (shouldBeOpen ? 2 : 3);
    s.SRB_PostProc = CreateEvent (0, TRUE, FALSE, 0);

    performScsiCommand (s.SRB_PostProc, s);
}

bool CDDeviceHandle::testController (const int type, CDController* const newController, CDReadBuffer& rb)
{
    controller = newController;
    readType = (BYTE) type;

    controller->deviceInfo = this;
    controller->framesToCheck = 1;
    controller->framesOverlap = 3;

    bool passed = false;
    memset (rb.buffer, 0xcd, rb.bufferSize);

    if (controller->read (rb))
    {
        passed = true;
        int* p = (int*) (rb.buffer + rb.dataStartOffset);
        int wrong = 0;

        for (int i = rb.dataLength / 4; --i >= 0;)
        {
            if (*p++ == (int) 0xcdcdcdcd)
            {
                if (++wrong == 4)
                {
                    passed = false;
                    break;
                }
            }
            else
            {
                wrong = 0;
            }
        }
    }

    if (! passed)
    {
        controller->shutDown();
        controller = nullptr;
    }

    return passed;
}


//==============================================================================
struct CDDeviceWrapper
{
    CDDeviceWrapper (const CDDeviceDescription& device, HANDLE scsiHandle)
        : deviceHandle (device, scsiHandle), overlapBuffer (3), jitter (false)
    {
        // xxx jitter never seemed to actually be enabled (??)
    }

    CDDeviceHandle deviceHandle;
    CDReadBuffer overlapBuffer;
    bool jitter;
};

//==============================================================================
int getAddressOfTrack (const TOCTRACK& t) noexcept
{
    return (((DWORD) t.addr[0]) << 24) + (((DWORD) t.addr[1]) << 16)
            + (((DWORD) t.addr[2]) << 8) + ((DWORD) t.addr[3]);
}

const int samplesPerFrame = 44100 / 75;
const int bytesPerFrame = samplesPerFrame * 4;
const int framesPerIndexRead = 4;

}

//==============================================================================
StringArray AudioCDReader::getAvailableCDNames()
{
    using namespace CDReaderHelpers;
    StringArray results;

    Array<CDDeviceDescription> list;
    findCDDevices (list);

    for (int i = 0; i < list.size(); ++i)
    {
        String s;
        if (list[i].scsiDriveLetter > 0)
            s << String::charToString (list[i].scsiDriveLetter).toUpperCase() << ": ";

        s << list[i].description;
        results.add (s);
    }

    return results;
}

AudioCDReader* AudioCDReader::createReaderForCD (const int deviceIndex)
{
    using namespace CDReaderHelpers;

    Array<CDDeviceDescription> list;
    findCDDevices (list);

    if (isPositiveAndBelow (deviceIndex, list.size()))
    {
        HANDLE h = createSCSIDeviceHandle (list [deviceIndex].scsiDriveLetter);

        if (h != INVALID_HANDLE_VALUE)
        {
            ScopedPointer<AudioCDReader> cd (new AudioCDReader (new CDDeviceWrapper (list [deviceIndex], h)));

            if (cd->lengthInSamples > 0)
                return cd.release();
        }
    }

    return nullptr;
}

AudioCDReader::AudioCDReader (void* handle_)
    : AudioFormatReader (0, "CD Audio"),
      handle (handle_),
      indexingEnabled (false),
      lastIndex (0),
      firstFrameInBuffer (0),
      samplesInBuffer (0)
{
    using namespace CDReaderHelpers;
    jassert (handle_ != nullptr);

    refreshTrackLengths();

    sampleRate = 44100.0;
    bitsPerSample = 16;
    numChannels = 2;
    usesFloatingPointData = false;

    buffer.setSize (4 * bytesPerFrame, true);
}

AudioCDReader::~AudioCDReader()
{
    using namespace CDReaderHelpers;
    CDDeviceWrapper* const device = static_cast<CDDeviceWrapper*> (handle);
    delete device;
}

bool AudioCDReader::readSamples (int** destSamples, int numDestChannels, int startOffsetInDestBuffer,
                                 int64 startSampleInFile, int numSamples)
{
    using namespace CDReaderHelpers;
    CDDeviceWrapper* const device = static_cast<CDDeviceWrapper*> (handle);

    bool ok = true;

    while (numSamples > 0)
    {
        const int bufferStartSample = firstFrameInBuffer * samplesPerFrame;
        const int bufferEndSample = bufferStartSample + samplesInBuffer;

        if (startSampleInFile >= bufferStartSample
             && startSampleInFile < bufferEndSample)
        {
            const int toDo = (int) jmin ((int64) numSamples, bufferEndSample - startSampleInFile);

            int* const l = destSamples[0] + startOffsetInDestBuffer;
            int* const r = numDestChannels > 1 ? (destSamples[1] + startOffsetInDestBuffer) : nullptr;
            const short* src = (const short*) buffer.getData();
            src += 2 * (startSampleInFile - bufferStartSample);

            for (int i = 0; i < toDo; ++i)
            {
                l[i] = src [i << 1] << 16;

                if (r != nullptr)
                    r[i] = src [(i << 1) + 1] << 16;
            }

            startOffsetInDestBuffer += toDo;
            startSampleInFile += toDo;
            numSamples -= toDo;
        }
        else
        {
            const int framesInBuffer = (int) (buffer.getSize() / bytesPerFrame);
            const int frameNeeded = (int) (startSampleInFile / samplesPerFrame);

            if (firstFrameInBuffer + framesInBuffer != frameNeeded)
            {
                device->overlapBuffer.dataLength = 0;
                device->overlapBuffer.startFrame = 0;
                device->overlapBuffer.numFrames = 0;
                device->jitter = false;
            }

            firstFrameInBuffer = frameNeeded;
            lastIndex = 0;

            CDReadBuffer readBuffer (framesInBuffer + 4);
            readBuffer.wantsIndex = indexingEnabled;

            int i;
            for (i = 5; --i >= 0;)
            {
                readBuffer.startFrame = frameNeeded;
                readBuffer.numFrames = framesInBuffer;

                if (device->deviceHandle.readAudio (readBuffer, device->jitter ? &device->overlapBuffer : 0))
                    break;
                else
                    device->overlapBuffer.dataLength = 0;
            }

            if (i >= 0)
            {
                buffer.copyFrom (readBuffer.buffer + readBuffer.dataStartOffset, 0, readBuffer.dataLength);
                samplesInBuffer = readBuffer.dataLength >> 2;
                lastIndex = readBuffer.index;
            }
            else
            {
                int* l = destSamples[0] + startOffsetInDestBuffer;
                int* r = numDestChannels > 1 ? (destSamples[1] + startOffsetInDestBuffer) : nullptr;

                while (--numSamples >= 0)
                {
                    *l++ = 0;

                    if (r != nullptr)
                        *r++ = 0;
                }

                // sometimes the read fails for just the very last couple of blocks, so
                // we'll ignore and errors in the last half-second of the disk..
                ok = startSampleInFile > (trackStartSamples [getNumTracks()] - 20000);
                break;
            }
        }
    }

    return ok;
}

bool AudioCDReader::isCDStillPresent() const
{
    using namespace CDReaderHelpers;
    TOC toc = { 0 };
    return static_cast<CDDeviceWrapper*> (handle)->deviceHandle.readTOC (&toc);
}

void AudioCDReader::refreshTrackLengths()
{
    using namespace CDReaderHelpers;
    trackStartSamples.clear();
    zeromem (audioTracks, sizeof (audioTracks));

    TOC toc = { 0 };

    if (static_cast<CDDeviceWrapper*> (handle)->deviceHandle.readTOC (&toc))
    {
        int numTracks = 1 + toc.lastTrack - toc.firstTrack;

        for (int i = 0; i <= numTracks; ++i)
        {
            trackStartSamples.add (samplesPerFrame * getAddressOfTrack (toc.tracks [i]));
            audioTracks [i] = ((toc.tracks[i].ADR & 4) == 0);
        }
    }

    lengthInSamples = getPositionOfTrackStart (getNumTracks());
}

bool AudioCDReader::isTrackAudio (int trackNum) const
{
    return trackNum >= 0 && trackNum < getNumTracks() && audioTracks [trackNum];
}

void AudioCDReader::enableIndexScanning (bool b)
{
    indexingEnabled = b;
}

int AudioCDReader::getLastIndex() const
{
    return lastIndex;
}

int AudioCDReader::getIndexAt (int samplePos)
{
    using namespace CDReaderHelpers;
    CDDeviceWrapper* const device = static_cast<CDDeviceWrapper*> (handle);

    const int frameNeeded = samplePos / samplesPerFrame;

    device->overlapBuffer.dataLength = 0;
    device->overlapBuffer.startFrame = 0;
    device->overlapBuffer.numFrames = 0;
    device->jitter = false;

    firstFrameInBuffer = 0;
    lastIndex = 0;

    CDReadBuffer readBuffer (4 + framesPerIndexRead);
    readBuffer.wantsIndex = true;

    int i;
    for (i = 5; --i >= 0;)
    {
        readBuffer.startFrame = frameNeeded;
        readBuffer.numFrames = framesPerIndexRead;

        if (device->deviceHandle.readAudio (readBuffer))
            break;
    }

    if (i >= 0)
        return readBuffer.index;

    return -1;
}

Array<int> AudioCDReader::findIndexesInTrack (const int trackNumber)
{
    using namespace CDReaderHelpers;
    Array <int> indexes;

    const int trackStart = getPositionOfTrackStart (trackNumber);
    const int trackEnd = getPositionOfTrackStart (trackNumber + 1);

    bool needToScan = true;

    if (trackEnd - trackStart > 20 * 44100)
    {
        // check the end of the track for indexes before scanning the whole thing
        needToScan = false;
        int pos = jmax (trackStart, trackEnd - 44100 * 5);
        bool seenAnIndex = false;

        while (pos <= trackEnd - samplesPerFrame)
        {
            const int index = getIndexAt (pos);

            if (index == 0)
            {
                // lead-out, so skip back a bit if we've not found any indexes yet..
                if (seenAnIndex)
                    break;

                pos -= 44100 * 5;

                if (pos < trackStart)
                    break;
            }
            else
            {
                if (index > 0)
                    seenAnIndex = true;

                if (index > 1)
                {
                    needToScan = true;
                    break;
                }

                pos += samplesPerFrame * framesPerIndexRead;
            }
        }
    }

    if (needToScan)
    {
        CDDeviceWrapper* const device = static_cast<CDDeviceWrapper*> (handle);

        int pos = trackStart;
        int last = -1;

        while (pos < trackEnd - samplesPerFrame * 10)
        {
            const int frameNeeded = pos / samplesPerFrame;

            device->overlapBuffer.dataLength = 0;
            device->overlapBuffer.startFrame = 0;
            device->overlapBuffer.numFrames = 0;
            device->jitter = false;

            firstFrameInBuffer = 0;

            CDReadBuffer readBuffer (4);
            readBuffer.wantsIndex = true;

            int i;
            for (i = 5; --i >= 0;)
            {
                readBuffer.startFrame = frameNeeded;
                readBuffer.numFrames = framesPerIndexRead;

                if (device->deviceHandle.readAudio (readBuffer))
                    break;
            }

            if (i < 0)
                break;

            if (readBuffer.index > last && readBuffer.index > 1)
            {
                last = readBuffer.index;
                indexes.add (pos);
            }

            pos += samplesPerFrame * framesPerIndexRead;
        }

        indexes.removeFirstMatchingValue (trackStart);
    }

    return indexes;
}

void AudioCDReader::ejectDisk()
{
    using namespace CDReaderHelpers;
    static_cast<CDDeviceWrapper*> (handle)->deviceHandle.openDrawer (true);
}
