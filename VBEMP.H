/*
 * ReactOS VBE miniport video driver
 *
 * Copyright (C) 2004 Filip Navara
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#ifndef VBEMP_H
#define VBEMP_H

/* INCLUDES *******************************************************************/

#include "winerror.h"
#include "devioctl.h"
#include "miniport.h"
#include "ntddvdeo.h"
#include "video.h"

#define PAGE_EXECUTE_READWRITE 0x40     

#define MEM_COMMIT           0x1000     

typedef enum _SECTION_INHERIT {
    ViewShare = 1,
    ViewUnmap = 2
} SECTION_INHERIT;

#define SECTION_QUERY       0x0001
#define SECTION_MAP_WRITE   0x0002
#define SECTION_MAP_READ    0x0004
#define SECTION_MAP_EXECUTE 0x0008
#define SECTION_EXTEND_SIZE 0x0010
#define STANDARD_RIGHTS_REQUIRED         (0x000F0000L)
#define SECTION_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED|SECTION_QUERY|\
                            SECTION_MAP_WRITE |      \
                            SECTION_MAP_READ |       \
                            SECTION_MAP_EXECUTE |    \
                            SECTION_EXTEND_SIZE)

typedef struct _UNICODE_STRING { 
    USHORT Length; 
    USHORT MaximumLength; 
    PWSTR  Buffer; 
} UNICODE_STRING; 

typedef UNICODE_STRING *PUNICODE_STRING;
typedef LONG NTSTATUS;
typedef NTSTATUS *PNTSTATUS;
#define NT_SUCCESS(Status) ((NTSTATUS)(Status) >= 0)

#define STATUS_SUCCESS                          ((NTSTATUS)0x00000000L)

typedef struct _OBJECT_ATTRIBUTES {
    ULONG Length;
    HANDLE RootDirectory;
    PUNICODE_STRING ObjectName;
    ULONG Attributes;
    PVOID SecurityDescriptor;        // Points to type SECURITY_DESCRIPTOR
    PVOID SecurityQualityOfService;  // Points to type SECURITY_QUALITY_OF_SERVI
} OBJECT_ATTRIBUTES;
typedef OBJECT_ATTRIBUTES *POBJECT_ATTRIBUTES;

#define InitializeObjectAttributes( p, n, a, r, s ) { \
    (p)->Length = sizeof( OBJECT_ATTRIBUTES );          \
    (p)->RootDirectory = r;                             \
    (p)->Attributes = a;                                \
    (p)->ObjectName = n;                                \
    (p)->SecurityDescriptor = s;                        \
    (p)->SecurityQualityOfService = NULL;               \
    }

typedef UCHAR BYTE;
typedef USHORT WORD;
typedef ULONG DWORD;
#define LOWORD(l)           ((WORD)((ULONG)(l) & 0xffff))
#define HIWORD(l)           ((WORD)((ULONG)(l) >> 16))
typedef DWORD ACCESS_MASK;

VOID
RtlInitUnicodeString(
    PUNICODE_STRING DestinationString,
    PCWSTR SourceString
    );

typedef struct _KV86M_REGISTERS
{
  /*
   * General purpose registers
   */

  ULONG Ebp;
  ULONG Edi;
  ULONG Esi;
  ULONG Edx;
  ULONG Ecx;
  ULONG Ebx;
  ULONG Eax;
  ULONG Ds;
  ULONG Es;
  ULONG Fs;
  ULONG Gs;

  /*
   * Control registers 
   */
  ULONG Eip;
  ULONG Cs;
  ULONG Eflags;
  ULONG Esp;
  ULONG Ss;

  /*
   * Control structures
   */
  ULONG RecoveryAddress;
  UCHAR RecoveryInstruction[4];
  ULONG Vif;                  
  ULONG Flags;
  PNTSTATUS PStatus;
} KV86M_REGISTERS, *PKV86M_REGISTERS;

NTSTATUS
Ke386CallBios(UCHAR Int, PKV86M_REGISTERS Regs);

NTSTATUS NtVdmControl( IN ULONG ControlCode, IN PVOID ControlData );

NTSTATUS
NtMapViewOfSection(
  IN HANDLE               SectionHandle,
  IN HANDLE               ProcessHandle,
  IN OUT PVOID            *BaseAddress OPTIONAL,
  IN ULONG                ZeroBits OPTIONAL,
  IN ULONG                CommitSize,
  IN OUT PLARGE_INTEGER   SectionOffset OPTIONAL,
  IN OUT PULONG           ViewSize,
  IN SECTION_INHERIT      InheritDisposition,
  IN ULONG                AllocationType OPTIONAL,
  IN ULONG                Protect );

NTSTATUS
ZwMapViewOfSection(
    IN HANDLE SectionHandle,
    IN HANDLE ProcessHandle,
    IN OUT PVOID *BaseAddress,
    IN ULONG ZeroBits,
    IN ULONG CommitSize,
    IN OUT PLARGE_INTEGER SectionOffset OPTIONAL,
    IN OUT PULONG ViewSize,
    IN SECTION_INHERIT InheritDisposition,
    IN ULONG AllocationType,
    IN ULONG Protect
    );

NTSTATUS
ZwAllocateVirtualMemory(
IN HANDLE ProcessHandle,
IN OUT PVOID *BaseAddress,
IN ULONG ZeroBits,
IN OUT PULONG AllocationSize,
IN ULONG AllocationType,
IN ULONG Protect
);

typedef enum _POOL_TYPE {
    NonPagedPool,
    PagedPool,
    NonPagedPoolMustSucceed,
    DontUseThisType,
    NonPagedPoolCacheAligned,
    PagedPoolCacheAligned,
    NonPagedPoolCacheAlignedMustS,
    MaxPoolType

    // end_wdm
    ,
    //
    // Note these per session types are carefully chosen so that the appropriate
    // masking still applies as well as MaxPoolType above.
    //

    NonPagedPoolSession = 32,
    PagedPoolSession = NonPagedPoolSession + 1,
    NonPagedPoolMustSucceedSession = PagedPoolSession + 1,
    DontUseThisTypeSession = NonPagedPoolMustSucceedSession + 1,
    NonPagedPoolCacheAlignedSession = DontUseThisTypeSession + 1,
    PagedPoolCacheAlignedSession = NonPagedPoolCacheAlignedSession + 1,
    NonPagedPoolCacheAlignedMustSSession = PagedPoolCacheAlignedSession + 1,

    // begin_wdm

    } POOL_TYPE;

typedef unsigned __int64 UINT_PTR, *PUINT_PTR;
//typedef unsigned __int64 ULONG_PTR, *PULONG_PTR;

PVOID
ExAllocatePool(
    IN POOL_TYPE PoolType,
    IN ULONG NumberOfBytes
    );

typedef struct _INT10_BIOS_ARGUMENTS {
  ULONG  Eax;
  ULONG  Ebx;
  ULONG  Ecx;
  ULONG  Edx;
  ULONG  Esi;
  ULONG  Edi;
  ULONG  Ebp;
  USHORT  SegDs;
  USHORT  SegEs;
} INT10_BIOS_ARGUMENTS, *PINT10_BIOS_ARGUMENTS;


#define STATUS_UNSUCCESSFUL 0xC0000001L
#define STATUS_NOT_IMPLEMENTED 0xC0000002L

NTSTATUS
ZwOpenSection(
    OUT PHANDLE SectionHandle,
    IN ACCESS_MASK DesiredAccess,
    IN POBJECT_ATTRIBUTES ObjectAttributes
    );

NTSTATUS
NtClose(
    IN HANDLE Handle
    );

VOID
_cdecl
sprintf(
    PCHAR DebugMessage,
    ...
    );

ULONG
_cdecl
DbgPrint(
    PCH Format,
    ...
    );


VOID
HalDisplayString (
    PUCHAR String
    );

#define DPRINT(a) HalDisplayString a; 
//NtDisplayString a;

/* FIXME: Missing define in w32api! */
#ifndef NtCurrentProcess
#define NtCurrentProcess() ( (HANDLE) -1 )
#endif

#include <pshpack1.h>

/*
 * VBE specification defined structure for general adapter info
 * returned by function 0x4F00.
 */
typedef struct
{
   CHAR Signature[4];
   WORD Version;
   DWORD OemStringPtr;
   LONG Capabilities;
   DWORD VideoModePtr;
   WORD TotalMemory;
   WORD OemSoftwareRevision;
   DWORD OemVendorNamePtr;
   DWORD OemProductNamePtr;
   DWORD OemProductRevPtr;
   CHAR Reserved[222];
   CHAR OemData[256];
} VBE_INFO, *PVBE_INFO;

/*
 * VBE specification defined structure for specific video mode
 * info returned by function 0x4F01.
 */
typedef struct {
   /* Mandatory information for all VBE revisions */
   WORD ModeAttributes;
   BYTE WinAAttributes;
   BYTE WinBAttributes;
   WORD WinGranularity;
   WORD WinSize;
   WORD WinASegment;
   WORD WinBSegment;
   DWORD WinFuncPtr;
   WORD BytesPerScanLine;

   /* Mandatory information for VBE 1.2 and above */
   WORD XResolution;
   WORD YResolution;
   BYTE XCharSize;
   BYTE YCharSize;
   BYTE NumberOfPlanes;
   BYTE BitsPerPixel;
   BYTE NumberOfBanks;
   BYTE MemoryModel;
   BYTE BankSize;
   BYTE NumberOfImagePages;
   BYTE Reserved1;

   /* Direct Color fields (required for Direct/6 and YUV/7 memory models) */
   BYTE RedMaskSize;
   BYTE RedFieldPosition;
   BYTE GreenMaskSize;
   BYTE GreenFieldPosition;
   BYTE BlueMaskSize;
   BYTE BlueFieldPosition;
   BYTE ReservedMaskSize;
   BYTE ReservedFieldPosition;
   BYTE DirectColorModeInfo;

   /* Mandatory information for VBE 2.0 and above */
   DWORD PhysBasePtr;
   DWORD Reserved2;
   WORD Reserved3;

   /* Mandatory information for VBE 3.0 and above */
   WORD LinBytesPerScanLine;
   BYTE BnkNumberOfImagePages;
   BYTE LinNumberOfImagePages;
   BYTE LinRedMaskSize;
   BYTE LinRedFieldPosition;
   BYTE LinGreenMaskSize;
   BYTE LinGreenFieldPosition;
   BYTE LinBlueMaskSize;
   BYTE LinBlueFieldPosition;
   BYTE LinReservedMaskSize;
   BYTE LinReservedFieldPosition;
   DWORD MaxPixelClock;

   CHAR Reserved4[189];
} VBE_MODEINFO, *PVBE_MODEINFO;

#define VBE_MODEATTR_LINEAR 0x80

#include <poppack.h>

typedef struct {
   /* Trampoline memory for communication with VBE real-mode interface. */
   PHYSICAL_ADDRESS PhysicalAddress;
   PVOID TrampolineMemory;

   /* Pointer to mapped frame buffer memory */
   PVOID FrameBufferMemory;

   /* General controller/BIOS information */
   BOOLEAN VGACompatible;
   WORD VBEVersion;

   /* Saved information about video modes */
   ULONG ModeCount;
   WORD *ModeNumbers;
   PVBE_MODEINFO ModeInfo;
   WORD CurrentMode;
} VBE_DEVICE_EXTENSION, *PVBE_DEVICE_EXTENSION;

VP_STATUS
VBEFindAdapter(
   IN PVOID HwDeviceExtension,
   IN PVOID HwContext,
   IN PWSTR ArgumentString,
   IN OUT PVIDEO_PORT_CONFIG_INFO ConfigInfo,
   OUT PUCHAR Again);

BOOLEAN
VBEInitialize(PVOID HwDeviceExtension);

BOOLEAN 
VBEStartIO(
   PVOID HwDeviceExtension,
   PVIDEO_REQUEST_PACKET RequestPacket);

VP_STATUS
VBEGetPowerState(
   PVOID HwDeviceExtension,
   ULONG HwId,
   PVIDEO_POWER_MANAGEMENT VideoPowerControl);

VP_STATUS 
VBESetPowerState(
   PVOID HwDeviceExtension,
   ULONG HwId,
   PVIDEO_POWER_MANAGEMENT VideoPowerControl);

BOOLEAN
VBESetCurrentMode(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_MODE RequestedMode,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBEResetDevice(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBEMapVideoMemory(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_MEMORY RequestedAddress,
   PVIDEO_MEMORY_INFORMATION MapInformation,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBEUnmapVideoMemory(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBEQueryNumAvailModes(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_NUM_MODES Modes,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBEQueryAvailModes(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_MODE_INFORMATION ReturnedModes,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN  
VBEQueryCurrentMode(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_MODE_INFORMATION VideoModeInfo,
   PSTATUS_BLOCK StatusBlock);

BOOLEAN
VBESetColorRegisters(
   PVBE_DEVICE_EXTENSION DeviceExtension,
   PVIDEO_CLUT ColorLookUpTable,
   PSTATUS_BLOCK StatusBlock);

#endif /* VBEMP_H */
