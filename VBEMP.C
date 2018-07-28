/*
* ReactOS VBE miniport video driver
* Copyright (C) 2004 Filip Navara
*
* Power Management and VBE 1.2 support
* Copyright (C) 2004 Magnus Olsen
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
*
* TODO:
* - Check input parameters everywhere.
* - Call VideoPortVerifyAccessRanges to reserve the memory we're about
*   to map.
*/

#define VBE12_SUPPORT

#if defined(ALLOC_PRAGMA)
#pragma alloc_text(PAGE,DriverEntry)
#pragma alloc_text(PAGE,VBEFindAdapter)
#pragma alloc_text(PAGE,VBESortModesCallback)
#pragma alloc_text(PAGE,VBESortModes)
#pragma alloc_text(PAGE,VBEInitialize)
#pragma alloc_text(PAGE,VBEStartIO)
#pragma alloc_text(PAGE,VBEGetPowerState)
#pragma alloc_text(PAGE,VBESetPowerState)
#pragma alloc_text(PAGE,VBEGetChildDescriptor)
#endif

/* INCLUDES *******************************************************************/

#include "vbemp.h"

/* PUBLIC AND PRIVATE FUNCTIONS ***********************************************/

#if (_WIN32_WINNT >= 0x0500)
#define DDC_MONITOR        (0xDDC00001)
#define NONDDC_MONITOR     (0xDDC00000)
#endif

ULONG VBELoaded = 0;
ULONG TM = 0;
ULONG DDCPassed = 0;
ULONG
DriverEntry(PVOID Context1, PVOID Context2)
{
  VIDEO_HW_INITIALIZATION_DATA InitData;
  ULONG initializationStatus;
  INTERFACE_TYPE aBusType[] = {PCIBus, Isa, Eisa, MicroChannel,
    InterfaceTypeUndefined};
  INTERFACE_TYPE *pBusType = aBusType;
  
  VideoPortZeroMemory(&InitData, sizeof(VIDEO_HW_INITIALIZATION_DATA));
  InitData.HwInitDataSize = sizeof(VIDEO_HW_INITIALIZATION_DATA);
  InitData.HwFindAdapter = VBEFindAdapter;
  InitData.HwInitialize = VBEInitialize;
  InitData.HwStartIO = VBEStartIO;
  InitData.HwResetHw = VBEResetHw;
#if (_WIN32_WINNT >= 0x0500)
  InitData.HwGetPowerState = VBEGetPowerState;
  InitData.HwSetPowerState = VBESetPowerState;
//InitData.HwGetVideoChildDescriptor = VBEGetChildDescriptor;
#endif
  InitData.HwDeviceExtensionSize = sizeof(VBE_DEVICE_EXTENSION);
  DPRINT((".DriverEntry() called\n"));
  
  while (*pBusType != InterfaceTypeUndefined)
  {
    DPRINT((".DriverEntry().VideoPortInitialize().BusType(0x%02lX) init...\n",*pBusType));
    InitData.AdapterInterfaceType = *pBusType;
    initializationStatus = VideoPortInitialize(Context1,
      Context2,
      &InitData,
      NULL);
    if (initializationStatus == NO_ERROR)
    {
      DPRINT((".DriverEntry().VideoPortInitialize().BusType(0x%02lX): OK\n",*pBusType));
      return initializationStatus;
    }
    else
      DPRINT((".DriverEntry().VideoPortInitialize().BusType(0x%02lX): FAIL\n",*pBusType));
    pBusType++;
  }
  DPRINT((".DriverEntry(): initialization loop end\n"));
  return initializationStatus;
}

/*
* VBEFindAdapter
*
* Should detect a VBE compatible display adapter, but it's not possible
* to use video port Int 10 services at this time during initialization,
* so we always return NO_ERROR and do the real work in VBEInitialize.
*/

VP_STATUS 
VBEFindAdapter(
               IN PVOID HwDeviceExtension,
               IN PVOID HwContext,
               IN PWSTR ArgumentString,
               IN OUT PVIDEO_PORT_CONFIG_INFO ConfigInfo,
               OUT PUCHAR Again)
{
  ULONG Slot,i;
  PPCI_COMMON_CONFIG PciData;
  UCHAR buffer[sizeof(PCI_COMMON_CONFIG)];
  PciData = (PPCI_COMMON_CONFIG)buffer;
  DPRINT((".VBEFindAdapter(Bustype: 0x%02X, BusNo: 0x%02X) called\n",
  ConfigInfo->AdapterInterfaceType,ConfigInfo->SystemIoBusNumber));
if (ConfigInfo->AdapterInterfaceType == 1) // ISA
   VBELoaded = 1;
if (ConfigInfo->AdapterInterfaceType == 5) // PCI/AGP
{
   DPRINT((" Analysing Bus...\n"));
        for (Slot=0; Slot<63; Slot++) 
        {
    if (VideoPortGetBusData(HwDeviceExtension,PCIConfiguration,
      Slot,PciData,0,PCI_COMMON_HDR_LENGTH+4))
    {           
    if (!(PciData->VendorID == 0xffff))
        {
        DPRINT((" Device %04X:%04X Bus 0x%02X Slot 0x%02X",
    PciData->VendorID,PciData->DeviceID, ConfigInfo->SystemIoBusNumber, Slot));
      if (PciData->BaseClass == 3)  
      {
        DPRINT((" -> VGA Adapter\n"));
        DPRINT(("DumpPCIConfigSpace: ------------------------\n"));
        DPRINT(("  BaseClass: 0x%04X\n",      PciData->BaseClass  ));
        DPRINT(("  Vendor Id: 0x%04X\n",      PciData->VendorID  ));
        DPRINT(("  Device Id: 0x%04X\n",      PciData->DeviceID  ));
        DPRINT(("  Rev Id: 0x%04X\n",         PciData->RevisionID  ));
        DPRINT(("  SubClass: 0x%04X\n",       PciData->SubClass  ));
#if (_WIN32_WINNT >= 0x0400)
        DPRINT(("  SubVendor Id: 0x%04X\n",   PciData->u.type0.SubVendorID  ));
        DPRINT(("  SubSystem Id: 0x%04X\n",   PciData->u.type0.SubSystemID  ));
#endif
        DPRINT(("  Command: 0x%X\n",        PciData->Command  ));
        DPRINT(("  Status: 0x%X\n",         PciData->Status  ));
        DPRINT(("  ProgIf: 0x%X\n",         PciData->ProgIf  ));
        DPRINT(("  CacheLine: 0x%X\n",      PciData->CacheLineSize  ));
        DPRINT(("  Latency: 0x%X\n",        PciData->LatencyTimer  ));
        DPRINT(("  Header Type: 0x%X\n",    PciData->HeaderType  ));
        DPRINT(("  BIST: 0x%04X\n",           PciData->BIST  ));
        for  (i=0;i<6;i++)
        {
        if (PciData->u.type0.BaseAddresses[i])
        DPRINT(("  Base Reg[%d]: 0x%08X\n", i, PciData->u.type0.BaseAddresses[i] & 0xFFFFFFF0 ));
        }
        DPRINT(("  Rom Base: 0x%X\n",       PciData->u.type0.ROMBaseAddress  ));
        DPRINT(("  Interrupt Line: IRQ%d\n", PciData->u.type0.InterruptLine  ));
        DPRINT(("  Interrupt Pin: %d\n",  PciData->u.type0.InterruptPin  ));
        DPRINT(("  Min Grant: 0x%04X\n",      PciData->u.type0.MinimumGrant  ));
        DPRINT(("  Max Latency: 0x%04X\n",    PciData->u.type0.MaximumLatency ));
        DPRINT(("  AGP Capability: 0x%04X\n", buffer[0x40]));
        DPRINT(("  AGP Next Cap:   0x%04X\n", buffer[0x41]));
        DPRINT(("  AGP Revision:   0x%04X\n", buffer[0x42]));
        DPRINT(("  AGP Status:     0x%04X\n", buffer[0x43]));
        DPRINT(("End. ---------------------------------------\n")); 
        VBELoaded = 1;
      } else DPRINT(("\n"));
       }
  }
  }
}
#if (_WIN32_WINNT >= 0x0500)
//VBELoaded = 1;
#else
  Again = FALSE;
#endif
if (VBELoaded) {
  DPRINT((".VBEFindAdapter(): VDM init\n"));
  ConfigInfo->VdmPhysicalVideoMemoryAddress.LowPart  = 0x000A0000;
  ConfigInfo->VdmPhysicalVideoMemoryAddress.HighPart = 0x00000000;
  ConfigInfo->VdmPhysicalVideoMemoryLength           = 0x00020000;
  ConfigInfo->NumEmulatorAccessEntries = 0;
  ConfigInfo->EmulatorAccessEntries = NULL;
  ConfigInfo->EmulatorAccessEntriesContext = 0;
  ConfigInfo->HardwareStateSize = 0;
  VBELoaded = 0;
  DPRINT((".VBEFindAdapter(): return NO_ERROR\n"));
  return NO_ERROR;
  }
  DPRINT((".VBEFindAdapter(): return ERROR_DEV_NOT_EXIST\n"));
  return ERROR_DEV_NOT_EXIST;
}

/*
* VBESortModesCallback
*
* Helper function for sorting video mode list.
*/

static int
VBESortModesCallback(PVBE_MODEINFO VbeModeInfoA, PVBE_MODEINFO VbeModeInfoB)
{
  DPRINT(("VBESortModesCallback: %dx%dx%d / %dx%dx%d\n",
    VbeModeInfoA->XResolution, VbeModeInfoA->YResolution,
    VbeModeInfoA->BitsPerPixel,
    VbeModeInfoB->XResolution, VbeModeInfoB->YResolution,
    VbeModeInfoB->BitsPerPixel));
  
    /*
    * FIXME: Until some reasonable method for changing video modes will
    * be available we favor more bits per pixel. It should be changed
    * later.
  */
  if (VbeModeInfoA->BitsPerPixel < VbeModeInfoB->BitsPerPixel) return -1;
  if (VbeModeInfoA->BitsPerPixel > VbeModeInfoB->BitsPerPixel) return 1;
  if (VbeModeInfoA->XResolution < VbeModeInfoB->XResolution) return -1;
  if (VbeModeInfoA->XResolution > VbeModeInfoB->XResolution) return 1;
  if (VbeModeInfoA->YResolution < VbeModeInfoB->YResolution) return -1;
  if (VbeModeInfoA->YResolution > VbeModeInfoB->YResolution) return 1;
  return 0;
}

/*
* VBESortModes
*
* Simple function for sorting the video mode list. Uses bubble sort.
*/

VOID 
VBESortModes(PVBE_DEVICE_EXTENSION DeviceExtension)
{
  BOOLEAN Finished = FALSE;
  ULONG Pos;
  int Result;
  VBE_MODEINFO TempModeInfo;
  WORD TempModeNumber;
  
  while (!Finished)
  {
    Finished = TRUE;
    for (Pos = 0; Pos < DeviceExtension->ModeCount - 1; Pos++)
    {
      Result = VBESortModesCallback(
        DeviceExtension->ModeInfo + Pos,
        DeviceExtension->ModeInfo + Pos + 1);
      if (Result > 0)
      {
        Finished = FALSE;
        
        VideoPortMoveMemory(
          &TempModeInfo,
          DeviceExtension->ModeInfo + Pos,
          sizeof(VBE_MODEINFO));
        TempModeNumber = DeviceExtension->ModeNumbers[Pos];
        
        VideoPortMoveMemory(
          DeviceExtension->ModeInfo + Pos,
          DeviceExtension->ModeInfo + Pos + 1,
          sizeof(VBE_MODEINFO));
        DeviceExtension->ModeNumbers[Pos] =
          DeviceExtension->ModeNumbers[Pos + 1];
        
        VideoPortMoveMemory(
          DeviceExtension->ModeInfo + Pos + 1,
          &TempModeInfo,
          sizeof(VBE_MODEINFO));
        DeviceExtension->ModeNumbers[Pos + 1] = TempModeNumber;
      }
    }
  }
}

/*
* VBEInitialize
*
* Performs the first initialization of the adapter, after the HAL has given
* up control of the video hardware to the video port driver.
*
* This function performs these steps:
* - Gets global VBE information and finds if VBE BIOS is present.
* - Builds the internal mode list using the list of modes provided by
*   the VBE.
*/

BOOLEAN 
VBEInitialize(PVOID HwDeviceExtension)
{
  VP_STATUS Status;
  PVBE_DEVICE_EXTENSION VBEDeviceExtension = 
    (PVBE_DEVICE_EXTENSION)HwDeviceExtension;
  ULONG ModeCount;
  ULONG SuitableModeCount;
  USHORT ModeTemp;
  ULONG CurrentMode;
  PVBE_MODEINFO VbeModeInfo;
  PVBE_INFO VbeInfo;

  CONTEXT BiosRegisters;
//PHYSICAL_ADDRESS PhysicalAddress;
  PVOID TrampolineMemory;
  PVOID TrampolineMemory2;
  ULONG ViewSize;
  
  DPRINT((".VBEInitialize() Start...\n"));

  TrampolineMemory = (PVOID)0x20000;
  TrampolineMemory2 = (PVOID)0x20200;
  ViewSize = 0x400;
  Status = ZwAllocateVirtualMemory(NtCurrentProcess(),
    &TrampolineMemory, 0, &ViewSize, MEM_COMMIT,
    PAGE_READWRITE); // try PAGE_EXECUTE_READWRITE
  if (!NT_SUCCESS(Status))
  {
    DPRINT(("Failed to allocate virtual memory (Status %x)\n", Status));
    return FALSE;
  }
  else
    DPRINT(("OK to allocate virtual memory\n"));
  if (TrampolineMemory > (PVOID)(0x100000 - ViewSize))
  {
    DPRINT(("Failed to allocate virtual memory at right address "
      "(was %x)\n", TrampolineMemory));
    return FALSE;
  }
  else
    DPRINT(("OK to allocate virtual memory at right address "
    "(was %x)\n", TrampolineMemory));
  
  VbeInfo = (PVBE_INFO)TrampolineMemory;
  VideoPortMoveMemory(VbeInfo->Signature, "VBE2", 4);
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_GET_CONTROLLER_INFORMATION;
  BiosRegisters.SegEs = (ULONG)TrampolineMemory >> 4;
  BiosRegisters.Edi =(ULONG)TrampolineMemory & 0xF;
  Status = Ke386CallBios(0x10, &BiosRegisters);

   if (BiosRegisters.Eax == VBE_SUCCESS)
   {
        VideoPortMoveMemory(&VBEDeviceExtension->VbeInfo,VbeInfo,sizeof(VBE_INFO));
        DPRINT(("VESA/VBE BIOS Present (%d.%02d, %ld Kb)\n",
        VBEDeviceExtension->VbeInfo.Version >> 8, VBEDeviceExtension->VbeInfo.Version & 0xFF,
        VBEDeviceExtension->VbeInfo.TotalMemory << 6));
  DPRINT(("OEM String: %s\n",(ULONG)((VBEDeviceExtension->VbeInfo.OemStringPtr >> 12) + LOWORD(VBEDeviceExtension->VbeInfo.OemStringPtr))));
  DPRINT(("OEM VendorName: %s\n",(ULONG)((VBEDeviceExtension->VbeInfo.OemVendorNamePtr >> 12) + LOWORD(VBEDeviceExtension->VbeInfo.OemVendorNamePtr))));
  DPRINT(("OEM ProductName: %s\n",(ULONG)((VBEDeviceExtension->VbeInfo.OemProductNamePtr >> 12) + LOWORD(VBEDeviceExtension->VbeInfo.OemProductNamePtr))));
  DPRINT(("OEM OemProductRev: %s\n",(ULONG)((VBEDeviceExtension->VbeInfo.OemProductRevPtr >> 12) + LOWORD(VBEDeviceExtension->VbeInfo.OemProductRevPtr))));

#ifdef VBE12_SUPPORT
      if (VBEDeviceExtension->VbeInfo.Version < 0x102)
#else
      if (VBEDeviceExtension->VbeInfo.Version < 0x200)
#endif
      {
         DPRINT(("VBE BIOS present, but incompatible version.\n"));
   Status =   ZwFreeVirtualMemory(NtCurrentProcess(),&TrampolineMemory, 
        &ViewSize, MEM_RELEASE);

   if (Status != NO_ERROR)
   {
      DPRINT(("Failed to free virtual memory (Status %x)\n", Status));
   }
   else
   DPRINT(("OK to free virtual memory\n"));
         return FALSE;
      }
   }
   else
   {
   DPRINT(("No VBE BIOS found.\n"));
    DPRINT(("ERR: Ke386CallBios (eax 0x%X es 0x%X edi 0x%X status 0x%X)\n", 
      BiosRegisters.Eax, BiosRegisters.SegEs, BiosRegisters.Edi, Status));
   Status =   ZwFreeVirtualMemory(NtCurrentProcess(),&TrampolineMemory, 
        &ViewSize, MEM_RELEASE);

   if (Status != NO_ERROR)
   {
      DPRINT(("Failed to free virtual memory (Status %x)\n", Status));
   }
   else
   DPRINT(("OK to free virtual memory\n"));
      return FALSE;
   }
  
  /*
  * Build a mode list here that can be later used by
  * IOCTL_VIDEO_QUERY_NUM_AVAIL_MODES and IOCTL_VIDEO_QUERY_AVAIL_MODES
  * calls.
  */

  /*
  * Get the number of supported video modes.
  *
  * No need to be map the memory. It's either in the video BIOS memory or
  * in our trampoline memory. In either case the memory is already mapped.
  */

   for (ModeCount = 0; ; ModeCount++)
   {
      /* Read the VBE mode number. */
        VideoPortMoveMemory(&ModeTemp,
         (PVOID)((VBEDeviceExtension->VbeInfo.VideoModePtr >> 12) + 
         LOWORD(VBEDeviceExtension->VbeInfo.VideoModePtr) +
         + (ModeCount << 1)),
         sizeof(ModeTemp));

      /* End of list? */
      if (ModeTemp == 0xFFFF || ModeTemp == 0)
         break;
    }
   DPRINT(("Found %u mode(s)\n",ModeCount));

 /*
  * Allocate space for video modes information.
  */
  
  VBEDeviceExtension->ModeInfo =
    ExAllocatePool(PagedPool, ModeCount * sizeof(VBE_MODEINFO));
  VBEDeviceExtension->ModeNumbers =
    ExAllocatePool(PagedPool, ModeCount * sizeof(WORD));

    /*
    * Get the actual mode infos.
  */
  
  VbeModeInfo = (PVBE_MODEINFO)TrampolineMemory2;
  for (CurrentMode = 0, SuitableModeCount = 0;
  CurrentMode < ModeCount;
  CurrentMode++)
  {
     /* Read the VBE mode number. */
        VideoPortMoveMemory(&ModeTemp,
         (PVOID)((VBEDeviceExtension->VbeInfo.VideoModePtr >> 12) + 
         LOWORD(VBEDeviceExtension->VbeInfo.VideoModePtr) +
         + (CurrentMode << 1)),
         sizeof(ModeTemp));
     /* Call VBE BIOS to read the mode info. */
      VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
      BiosRegisters.Eax = VBE_GET_MODE_INFORMATION;
      BiosRegisters.Ecx = ModeTemp;
      BiosRegisters.SegEs = (ULONG)TrampolineMemory2 >> 4;
      BiosRegisters.Edi =(ULONG)TrampolineMemory2 & 0xF;
      Ke386CallBios(0x10,&BiosRegisters);
    
      VideoPortMoveMemory(&VBEDeviceExtension->ModeInfo[SuitableModeCount],
        VbeModeInfo, sizeof(VBE_MODEINFO));
  
    /* Is this mode acceptable? */
      if (BiosRegisters.Eax == VBE_SUCCESS &&
      VbeModeInfo->XResolution >= 320 &&
      VbeModeInfo->YResolution >= 200 &&
      VbeModeInfo->BitsPerPixel == 8 && //hack
      (VbeModeInfo->MemoryModel == VBE_MEMORYMODEL_PACKEDPIXEL ||
      VbeModeInfo->MemoryModel == VBE_MEMORYMODEL_DIRECTCOLOR))
    {
      if (VbeModeInfo->ModeAttributes & VBE_MODEATTR_LINEAR)
      {
        DPRINT(("0x%04lX; %dx%dx%d, LFB:0x%08lX\n",
          ModeTemp,
          VbeModeInfo->XResolution,
          VbeModeInfo->YResolution,
          VbeModeInfo->BitsPerPixel,
          VbeModeInfo->PhysBasePtr));                                                            
        
        VBEDeviceExtension->ModeNumbers[SuitableModeCount] = ModeTemp | 0x4000;
        SuitableModeCount++;
      }
#ifdef VBE12_SUPPORT
      else
      {
        DPRINT(("0x%04lX; %dx%dx%d, noLFB\n",
          ModeTemp,
          VbeModeInfo->XResolution,
          VbeModeInfo->YResolution,
          VbeModeInfo->BitsPerPixel))
        
        VBEDeviceExtension->ModeNumbers[SuitableModeCount] = ModeTemp;
        SuitableModeCount++;
      }
#endif
    } 
    else
    {
        DPRINT(("0x%04lX; %dx%dx%d, LFB:0x%08lX - skipped\n",
          ModeTemp,
          VbeModeInfo->XResolution,
          VbeModeInfo->YResolution,
          VbeModeInfo->BitsPerPixel,
          VbeModeInfo->PhysBasePtr));                                                            
    }
  }
  
  if (SuitableModeCount == 0)
  {
    DPRINT(("VBEMP: No video modes supported\n"));
    return FALSE;
  }
  
   VBEDeviceExtension->ModeCount = SuitableModeCount;

   DPRINT(("VBEMP: %u video mode(s) supported\n",SuitableModeCount));
  
  /*
  * Sort the video mode list according to resolution and bits per pixel.
  */
  
  //   VBESortModes(VBEDeviceExtension);
  
  /*
  * Print the supported video modes when DBG is set.
  */
#ifdef xDBG
  for (CurrentMode = 0;
  CurrentMode < SuitableModeCount;
  CurrentMode++)
  {
      DPRINT(("0x%04lX; %dx%dx%d, LFB:0x%04lX\n",
      VBEDeviceExtension->ModeNumbers[CurrentMode] & 0x1FF,
      VBEDeviceExtension->ModeInfo[CurrentMode].XResolution,
      VBEDeviceExtension->ModeInfo[CurrentMode].YResolution,
      VBEDeviceExtension->ModeInfo[CurrentMode].BitsPerPixel,
      VBEDeviceExtension->ModeInfo[CurrentMode].PhysBasePtr));                                                            
  }
#endif
  VBEDeviceExtension->CurrentMode = 0; //ADDED BY KTP!!
#if (_WIN32_WINNT < 0x0500)
   VBEDeviceExtension->DDCData =
      ExAllocatePool(PagedPool, 256);

   VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
   BiosRegisters.Eax = 0x4F15;
   BiosRegisters.Ebx = 1;
   BiosRegisters.Ecx = 0;
   BiosRegisters.Edx = 0;
   BiosRegisters.SegEs = (ULONG)TrampolineMemory2 >> 4;
   BiosRegisters.Edi =(ULONG)TrampolineMemory2 & 0xF;
   DPRINT(("INFO: Ke386CallBios (eax 0x%X es 0x%X edi 0x%X)\n", 
   BiosRegisters.Eax, BiosRegisters.SegEs, BiosRegisters.Edi));
   Status = Ke386CallBios(0x10, &BiosRegisters);

   if (Status != NO_ERROR)
   {
     DPRINT(("DDC: Failed to Ke386CallBios (Status %x, eax %x es %x edi %x)\n", 
     Status, BiosRegisters.Eax, BiosRegisters.SegEs, BiosRegisters.Edi));
   }
   else
   DPRINT(("DDC: OK to Ke386CallBios\n"));
   if (BiosRegisters.Eax == VBE_SUCCESS)
         {   
      VideoPortMoveMemory(&VBEDeviceExtension->DDCData,
        VbeModeInfo, 256);
       DPRINT(("DDCData: OK\n"));
          DDCPassed = 1;
        };
#endif  
  VideoPortSetRegistryParameters(VBEDeviceExtension,
    L"HardwareInformation.ChipType",
    L"VESA/SVGA",
    sizeof(L"VESA/SVGA"));
  
  VideoPortSetRegistryParameters(VBEDeviceExtension,
    L"HardwareInformation.DacType",
    L"Internal TrueColor DAC",
    sizeof(L"Internal TrueColor DAC"));
  
  TM  = VBEDeviceExtension->VbeInfo.TotalMemory << 16;
  VideoPortSetRegistryParameters(VBEDeviceExtension,
    L"HardwareInformation.MemorySize",
    &TM,
    sizeof(ULONG));
  
  VideoPortSetRegistryParameters(VBEDeviceExtension,
    L"HardwareInformation.AdapterString",
    L"VESA-Compatible Display Adapter",
    sizeof(L"VESA-Compatible Display Adapter"));
  
  VideoPortSetRegistryParameters(VBEDeviceExtension,
    L"HardwareInformation.BiosString",
    L"VBE BIOS",
    sizeof(L"VBE BIOS"));
  
  DPRINT((".VBEInitialize() End...\n"));
  
  return TRUE;
}

/*
* VBEStartIO
*
* Processes the specified Video Request Packet.
*/

BOOLEAN 
VBEStartIO(
           PVOID HwDeviceExtension,
           PVIDEO_REQUEST_PACKET RequestPacket)
{
  BOOLEAN Result;

    VP_STATUS status;
    ULONG inIoSpace;
    PVIDEO_SHARE_MEMORY pShareMemory;
    PVIDEO_SHARE_MEMORY_INFORMATION pShareMemoryInformation;
    PHYSICAL_ADDRESS shareAddress;
    PVOID virtualAddress;
    ULONG sharedViewSize, ChID;
    ULONG ulBankSize;
    VOID (*pfnBank)(LONG,LONG,PVOID);
  
  RequestPacket->StatusBlock->Status = STATUS_UNSUCCESSFUL;
  //DPRINT(("VBEStartIO: 0x%04lX\n",(RequestPacket->IoControlCode & 0xFFF) >> 2));
  
  switch (RequestPacket->IoControlCode)
  {
  case IOCTL_VIDEO_SET_CURRENT_MODE:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_SET_CURRENT_MODE\n"));
    if (RequestPacket->InputBufferLength < sizeof(VIDEO_MODE)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBESetCurrentMode(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_MODE)RequestPacket->InputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_RESET_DEVICE:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_RESET_DEVICE\n"));
    Result = VBEResetDevice(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_MAP_VIDEO_MEMORY:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_MAP_VIDEO_MEMORY\n"));
    if (RequestPacket->OutputBufferLength < sizeof(VIDEO_MEMORY_INFORMATION) ||
      RequestPacket->InputBufferLength < sizeof(VIDEO_MEMORY)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBEMapVideoMemory(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_MEMORY)RequestPacket->InputBuffer,
      (PVIDEO_MEMORY_INFORMATION)RequestPacket->OutputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_UNMAP_VIDEO_MEMORY:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_UNMAP_VIDEO_MEMORY\n"));
    if (RequestPacket->InputBufferLength < sizeof(VIDEO_MEMORY)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBEUnmapVideoMemory(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_MEMORY)RequestPacket->InputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_QUERY_NUM_AVAIL_MODES:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_QUERY_NUM_AVAIL_MODES\n"));
    if (RequestPacket->OutputBufferLength < sizeof(VIDEO_NUM_MODES)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBEQueryNumAvailModes(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_NUM_MODES)RequestPacket->OutputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_QUERY_AVAIL_MODES:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_QUERY_AVAIL_MODES\n"));
    if (RequestPacket->OutputBufferLength <
      ((PVBE_DEVICE_EXTENSION)HwDeviceExtension)->ModeCount * sizeof(VIDEO_MODE_INFORMATION)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBEQueryAvailModes(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_MODE_INFORMATION)RequestPacket->OutputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_SET_COLOR_REGISTERS:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_SET_COLOR_REGISTERS\n"));
    if (RequestPacket->InputBufferLength < sizeof(VIDEO_CLUT) ||
      RequestPacket->InputBufferLength <
      (((PVIDEO_CLUT)RequestPacket->InputBuffer)->NumEntries * sizeof(ULONG)) +
      sizeof(VIDEO_CLUT))
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBESetColorRegisters(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_CLUT)RequestPacket->InputBuffer,
      RequestPacket->StatusBlock);
    break;
    
  case IOCTL_VIDEO_QUERY_CURRENT_MODE:
    DPRINT(("VBEStartIO: IOCTL_VIDEO_QUERY_CURRENT_MODE\n"));
    if (RequestPacket->OutputBufferLength < sizeof(VIDEO_MODE_INFORMATION)) 
    {
      RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
      return TRUE;
    }
    Result = VBEQueryCurrentMode(
      (PVBE_DEVICE_EXTENSION)HwDeviceExtension,
      (PVIDEO_MODE_INFORMATION)RequestPacket->OutputBuffer,
      RequestPacket->StatusBlock);
    break;
    
#if (_WIN32_WINNT >= 0x0500)
      case IOCTL_VIDEO_GET_CHILD_STATE:
         DPRINT(("VBEStartIO: IOCTL_VIDEO_GET_CHILD_STATE\n"));
         if (RequestPacket->OutputBufferLength < sizeof(ULONG)) 
         {
            RequestPacket->StatusBlock->Status = ERROR_INSUFFICIENT_BUFFER;
            return TRUE;
         }
         VideoPortMoveMemory(&ChID,RequestPacket->InputBuffer,sizeof(ULONG));
         DPRINT((" - Child 0x%08X State Request\n",(ULONG)ChID));
         return FALSE;
         break;
#endif
    case IOCTL_VIDEO_SHARE_VIDEO_MEMORY:

        DPRINT(("VBEStartIO: IOCTL_VIDEO_SHARE_VIDEO_MEMORY\n"));

        if ( (RequestPacket->OutputBufferLength < sizeof(VIDEO_SHARE_MEMORY_INFORMATION)) ||
             (RequestPacket->InputBufferLength < sizeof(VIDEO_MEMORY)) ) {

            status = ERROR_INSUFFICIENT_BUFFER;
            DPRINT(("IOCTL_VIDEO_SHARE_VIDEO_MEMORY - ERROR_INSUFFICIENT_BUFFER\n"));
            break;

        }

        pShareMemory = RequestPacket->InputBuffer;

        if ( (pShareMemory->ViewOffset > TM) ||
             ((pShareMemory->ViewOffset + pShareMemory->ViewSize) >
                  TM) ) {

            status = ERROR_INVALID_PARAMETER;
            DPRINT(("IOCTL_VIDEO_SHARE_VIDEO_MEMORY - ERROR_INVALID_PARAMETER\n"));
            break;

        }

        RequestPacket->StatusBlock->Information =
                                    sizeof(VIDEO_SHARE_MEMORY_INFORMATION);

        virtualAddress = pShareMemory->ProcessHandle;
        sharedViewSize = pShareMemory->ViewSize;

  DPRINT((" ->TotalMemory 0x%08lX\n",TM));
  DPRINT((" ->pShareMemory->ViewOffset 0x%08lX comp w TM\n",pShareMemory->ViewOffset));
  DPRINT((" ->pShareMemory->ViewSize %ld (0x%08lX) comp w TM\n",pShareMemory->ViewSize,pShareMemory->ViewSize));
  DPRINT((" ->sharedViewSize %ld (0x%08lX) iniitial\n",sharedViewSize,sharedViewSize));

        inIoSpace = 0;

        shareAddress.QuadPart = 0xA0000;

        pfnBank = vBankMap;
        ulBankSize = 0x10000; // 64K banks

  DPRINT((" ->shareAddress.QuadPart 0x%08lX\n",shareAddress.QuadPart));
  DPRINT((" .VideoPortMapBankedMemory() called\n"));
        status = VideoPortMapBankedMemory(HwDeviceExtension,
                                          shareAddress,
                                          &sharedViewSize,
                                          &inIoSpace,
                                          &virtualAddress,
                                          ulBankSize,   // bank size                  
                                          TRUE,        // we have separate read/write
                                          (PVOID)pfnBank,
                                          (PVOID)HwDeviceExtension);

        pShareMemoryInformation = RequestPacket->OutputBuffer;

        pShareMemoryInformation->SharedViewOffset = pShareMemory->ViewOffset;
        pShareMemoryInformation->VirtualAddress = virtualAddress;
        pShareMemoryInformation->SharedViewSize = sharedViewSize;

  DPRINT((" ->pShareMemory->ViewSize %ld (0x%08lX)\n",pShareMemory->ViewSize,pShareMemory->ViewSize));
  DPRINT((" ->pShareMemory->ViewOffset 0x%08lX\n",pShareMemory->ViewOffset));
  DPRINT((" ->virtualAddress 0x%08lX from call\n",virtualAddress));
  DPRINT((" ->sharedViewSize %ld (0x%08lX) aligned\n",sharedViewSize,sharedViewSize));

        break;

    case IOCTL_VIDEO_UNSHARE_VIDEO_MEMORY:

        DPRINT(("VBEStartIO: IOCTL_VIDEO_UNSHARE_VIDEO_MEMORY\n"));

        if (RequestPacket->InputBufferLength < sizeof(VIDEO_SHARE_MEMORY)) {

            status = ERROR_INSUFFICIENT_BUFFER;
            break;

        }

        pShareMemory = RequestPacket->InputBuffer;

        status = VideoPortUnmapMemory(HwDeviceExtension,
                                      pShareMemory->RequestedVirtualAddress,
                                      pShareMemory->ProcessHandle);

        break;
/*
    case IOCTL_VIDEO_GET_BANK_SELECT_CODE:

        DPRINT(("VBEStartIO: IOCTL_VIDEO_GET_BANK_SELECT_CODE\n"));

        status = VBEGetBankSelectCode(HwDeviceExtension,
                                        (PVIDEO_BANK_SELECT) RequestPacket->OutputBuffer,
                                        RequestPacket->OutputBufferLength,
                                        &RequestPacket->StatusBlock->Information);

        break; */
  default:
    DPRINT(("VBEStartIO: 0x%04lX (Unknown IOCTL)\n",(RequestPacket->IoControlCode & 0xFFF) >> 2));
    RequestPacket->StatusBlock->Status = STATUS_NOT_IMPLEMENTED;
    return FALSE;
   }
   
   if (Result)
     RequestPacket->StatusBlock->Status = STATUS_SUCCESS;
   
   return TRUE;
}

/*
* VBEResetHw
*
* This function is called to reset the hardware to a known state.
*/

BOOLEAN 
VBEResetHw(
           PVOID DeviceExtension,
           ULONG Columns,
           ULONG Rows)
{
  VIDEO_X86_BIOS_ARGUMENTS BiosRegisters;
  PVBE_DEVICE_EXTENSION VBEDeviceExtension = 
    (PVBE_DEVICE_EXTENSION)DeviceExtension;
  
   DPRINT((".VBEResetHw() called, cols = %d, rows = %d\n", Columns, Rows));

  if (!VBEResetDevice(DeviceExtension, NULL))
    return FALSE;
  
  /* Change number of columns/rows */
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  
  if (Columns == 80 && Rows == 25)
  {
    /* Default text size, don't change anything. */
    return TRUE;
  }
  else if (Columns == 80 && Rows == 28)
  {
    /* Use 9x14 font (80x28) */
    BiosRegisters.Eax = 0x1111;
  }
  else if (Columns == 80 && Rows == 43)
  {
    /* Use 8x8 font in 350 scans mode (80x43) */
    BiosRegisters.Eax = 0x1201;
    BiosRegisters.Ebx = 0x30;
    VideoPortInt10(DeviceExtension, &BiosRegisters);
    
    BiosRegisters.Eax = 0x3;
    BiosRegisters.Ebx = 0;
    VideoPortInt10(DeviceExtension, &BiosRegisters);
    
    BiosRegisters.Eax = 0x1112;
  }
  else if (Columns == 80 && Rows == 50)
  {
    /* Use 8x8 font (80x50) */
    BiosRegisters.Eax = 0x1112;
  }
  else
    return FALSE;
  
  VideoPortInt10(DeviceExtension, &BiosRegisters);
  
  return TRUE;
}


/*
* VBESetCurrentMode
*
* Sets the adapter to the specified operating mode.
*/

BOOLEAN
VBESetCurrentMode(
                  PVBE_DEVICE_EXTENSION DeviceExtension,
                  PVIDEO_MODE RequestedMode,
                  PSTATUS_BLOCK StatusBlock)
{
  VIDEO_X86_BIOS_ARGUMENTS BiosRegisters;
  /*
  ULONG Freq;

  for (Freq = 18000000;Freq < 200000000;Freq = Freq + 500000) // 18 mhz
  {
    VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
    BiosRegisters.Eax = 0x4F0B;
    BiosRegisters.Ebx = 0;
    BiosRegisters.Edx = DeviceExtension->ModeNumbers[RequestedMode->RequestedMode];
    BiosRegisters.Ecx = Freq;
    DPRINT(("VBEGetPixelClock (eax 0x%X edx 0x%X ecx %ld), Result:", 
      BiosRegisters.Eax, BiosRegisters.Edx, BiosRegisters.Ecx));
    VideoPortInt10(DeviceExtension, &BiosRegisters);
    
    if (BiosRegisters.Eax == VBE_SUCCESS)
      DPRINT(("OK, nearest freq %ld)\n", BiosRegisters.Ecx))
      else
    {
      DPRINT(("ERR, eax 0x%X ebx 0x%X ecx %ld edx 0x%X\n", 
        BiosRegisters.Eax, BiosRegisters.Ebx, BiosRegisters.Ecx, BiosRegisters.Edx));
      DPRINT(("VBEMP: VBEGetPixelClock failed (0x%X)\n", BiosRegisters.Eax));
      break;
    }
  }
  */
  /*********************************/
  DPRINT((".VBESetCurrentMode()...\n"));
  
  if (RequestedMode->RequestedMode >= DeviceExtension->ModeCount)
  {
    return ERROR_INVALID_PARAMETER;
  }
  
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_SET_VBE_MODE;
  BiosRegisters.Ebx = DeviceExtension->ModeNumbers[RequestedMode->RequestedMode];
  DPRINT(("INFO: VideoPortInt10 (eax 0x%X ebx 0x%X)\n", 
    BiosRegisters.Eax, BiosRegisters.Ebx));
  VideoPortInt10(DeviceExtension, &BiosRegisters);
  
  if (BiosRegisters.Eax == VBE_SUCCESS)
  {
    DeviceExtension->CurrentMode = (USHORT) RequestedMode->RequestedMode;
  }
  else
  {
    DPRINT(("ERR: VideoPortInt10 (eax 0x%X ebx 0x%X)\n", 
      BiosRegisters.Eax, BiosRegisters.Ebx));
    DPRINT(("VBEMP: VBESetCurrentMode failed (0x%X)\n", BiosRegisters.Eax));
    return FALSE;
  }
  return TRUE;
}

/*
* VBEResetDevice
*
* Resets the video hardware to the default mode, to which it was initialized
* at system boot. 
*/

BOOLEAN
VBEResetDevice(
               PVBE_DEVICE_EXTENSION DeviceExtension,
               PSTATUS_BLOCK StatusBlock)
{
  VIDEO_X86_BIOS_ARGUMENTS BiosRegisters;
  
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_SET_VBE_MODE;
  BiosRegisters.Ebx = 0x3;
  VideoPortInt10(DeviceExtension, &BiosRegisters);
  
  return BiosRegisters.Eax == VBE_SUCCESS;
}

/*
* VBEMapVideoMemory
*
* Maps the video hardware frame buffer and video RAM into the virtual address
* space of the requestor. 
*/

BOOLEAN
VBEMapVideoMemory(
                  PVBE_DEVICE_EXTENSION DeviceExtension,
                  PVIDEO_MEMORY RequestedAddress,
                  PVIDEO_MEMORY_INFORMATION MapInformation,
                  PSTATUS_BLOCK StatusBlock)
{
  PHYSICAL_ADDRESS FrameBuffer;
  ULONG inIoSpace = 0; // VIDEO_MEMORY_SPACE_MEMORY;
  
  StatusBlock->Information = sizeof(VIDEO_MEMORY_INFORMATION);
  
  if (DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].ModeAttributes &
    VBE_MODEATTR_LINEAR)
  {    
    FrameBuffer.QuadPart =
      DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].PhysBasePtr;
    MapInformation->VideoRamBase = RequestedAddress->RequestedVirtualAddress;
    if (DeviceExtension->VbeInfo.Version < 0x300)
    {
      MapInformation->VideoRamLength = 
        DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].BytesPerScanLine *
        DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].YResolution;
        DPRINT((" VideoRamLength via BytesPerScanLine(VBE < 3.00)\n"));
    }
    else
    {
      MapInformation->VideoRamLength = 
        DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].LinBytesPerScanLine *
        DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].YResolution;
        DPRINT((" VideoRamLength via LinBytesPerScanLine(VBE >= 3.00)\n"));
    }
//if (MapInformation->VideoRamLength & 0xFFFF) 
//MapInformation->VideoRamLength = (MapInformation->VideoRamLength & 0xFFFF0000) + 0x10000; 
  MapInformation->VideoRamLength = TM; //hack
  if ((MapInformation->VideoRamLength & (4*1024*1024 - 1)) == 0)
  DPRINT(("VBEMP: Memory 4Mb aligned. OK.\n"))
  else
  DPRINT(("VBEMP: Warning! Memory NOT 4Mb aligned!!!!\n"));

#if (_WIN32_WINNT >= 0x0400)
  DPRINT(("VBEMP: Setting VIDEO_MEMORY_SPACE_P6CACHE (4 Mb aligned)\n"));
  inIoSpace |= VIDEO_MEMORY_SPACE_P6CACHE;
#endif
  }
#ifdef VBE12_SUPPORT
  else
  {
    FrameBuffer.QuadPart = 0xA0000;
    MapInformation->VideoRamBase = RequestedAddress->RequestedVirtualAddress;
      MapInformation->VideoRamLength = // 0x20000;
       DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].BytesPerScanLine *
        DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].YResolution;
      if (MapInformation->VideoRamLength & 0xFFFF) 
  MapInformation->VideoRamLength = (MapInformation->VideoRamLength & 0xFFFF0000) + 0x10000; 
  }
#endif
  
  DPRINT((" FrameBuffer 0x%08lX bytes\n",FrameBuffer.QuadPart));
  DPRINT((" ->VideoRamBase 0x%08lX\n",MapInformation->VideoRamBase));
  DPRINT((" ->VideoRamLength %ld (0x%08lX) bytes\n",MapInformation->VideoRamLength,MapInformation->VideoRamLength));
  DPRINT((" ->OffscreenRamLength %ld (0x%08lX) bytes\n",DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].Reserved3,
  DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].Reserved3));
  if (DeviceExtension->VbeInfo.Version < 0x200)
  {  
  DPRINT((" .VideoPortMapBankedMemory() called\n"));
  VideoPortMapBankedMemory(DeviceExtension,
                                          FrameBuffer, 
                                          &MapInformation->VideoRamLength,
                                          &inIoSpace,
                                          &MapInformation->VideoRamBase,
                                          0x10000,   // bank size                  
                                          TRUE,        // we have separate read/write
                                          (PVOID)vBankMap,
                                          (PVOID)DeviceExtension);
    }
    else   
   {
   DPRINT((" .VideoPortMapMemory() called\n"));
   VideoPortMapMemory(DeviceExtension, FrameBuffer,
    &MapInformation->VideoRamLength, &inIoSpace,
    &MapInformation->VideoRamBase);
   }
  MapInformation->FrameBufferBase = MapInformation->VideoRamBase;
  MapInformation->FrameBufferLength = MapInformation->VideoRamLength;

  DPRINT((" FrameBuffer 0x%08lX bytes\n",FrameBuffer.QuadPart));
  DPRINT((" ->VideoRamBase 0x%08lX\n",MapInformation->VideoRamBase));
  DPRINT((" ->VideoRamLength %ld (0x%08lX) bytes\n",MapInformation->VideoRamLength,MapInformation->VideoRamLength));
  DPRINT((" ->FrameBufferBase 0x%08lX\n",MapInformation->FrameBufferBase));
  DPRINT((" ->FrameBufferLength %ld (0x%08lX) bytes\n",MapInformation->FrameBufferLength,MapInformation->FrameBufferLength));
  DPRINT((" ->OffscreenRamLength %ld (0x%08lX) bytes\n",DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].Reserved3,
  DeviceExtension->ModeInfo[DeviceExtension->CurrentMode].Reserved3));

  return TRUE;
}

/*
* VBEUnmapVideoMemory
*
* Releases a mapping between the virtual address space and the adapter's
* frame buffer and video RAM.
*/

BOOLEAN
VBEUnmapVideoMemory(
                    PVBE_DEVICE_EXTENSION DeviceExtension,
                    PVIDEO_MEMORY VideoMemory,
                    PSTATUS_BLOCK StatusBlock)
{
  VideoPortUnmapMemory(DeviceExtension, VideoMemory->RequestedVirtualAddress,
    NULL);
  return TRUE;
}   

/*
* VBEQueryNumAvailModes
*
* Returns the number of video modes supported by the adapter and the size
* in bytes of the video mode information, which can be used to allocate a
* buffer for an IOCTL_VIDEO_QUERY_AVAIL_MODES request.
*/

BOOLEAN
VBEQueryNumAvailModes(
                      PVBE_DEVICE_EXTENSION DeviceExtension,
                      PVIDEO_NUM_MODES Modes,
                      PSTATUS_BLOCK StatusBlock)
{
  Modes->NumModes = DeviceExtension->ModeCount;
  Modes->ModeInformationLength = sizeof(VIDEO_MODE_INFORMATION);
  StatusBlock->Information = sizeof(VIDEO_NUM_MODES);
  return TRUE;
}

/*
* VBEQueryMode
*
* Returns information about one particular video mode.
*/

VOID   
VBEQueryMode(
             PVBE_DEVICE_EXTENSION DeviceExtension,
             PVIDEO_MODE_INFORMATION VideoMode,
             ULONG VideoModeId)
{
  PVBE_MODEINFO VBEMode = &DeviceExtension->ModeInfo[VideoModeId];
  
  VideoMode->Length = sizeof(VIDEO_MODE_INFORMATION);
  VideoMode->ModeIndex = VideoModeId;
  VideoMode->VisScreenWidth = VBEMode->XResolution;
  VideoMode->VisScreenHeight = VBEMode->YResolution;
  if (DeviceExtension->VbeInfo.Version < 0x300)
    VideoMode->ScreenStride = VBEMode->BytesPerScanLine;
  else
    VideoMode->ScreenStride = VBEMode->LinBytesPerScanLine;
  VideoMode->NumberOfPlanes = VBEMode->NumberOfPlanes;
  VideoMode->BitsPerPlane = VBEMode->BitsPerPixel / VBEMode->NumberOfPlanes;
  VideoMode->Frequency = 1;
  VideoMode->XMillimeter = 320; /* KTP */
  VideoMode->YMillimeter = 240; /* KTP */
  if (VBEMode->BitsPerPixel > 8)
  {
    if (DeviceExtension->VbeInfo.Version < 0x300)
    {
      VideoMode->NumberRedBits = VBEMode->RedMaskSize;
      VideoMode->NumberGreenBits = VBEMode->GreenMaskSize;
      VideoMode->NumberBlueBits = VBEMode->BlueMaskSize;
      VideoMode->RedMask = ((1 << VBEMode->RedMaskSize) - 1) << VBEMode->RedFieldPosition;
      VideoMode->GreenMask = ((1 << VBEMode->GreenMaskSize) - 1) << VBEMode->GreenFieldPosition;
      VideoMode->BlueMask = ((1 << VBEMode->BlueMaskSize) - 1) << VBEMode->BlueFieldPosition;
    }
    else
    {
      VideoMode->NumberRedBits = VBEMode->LinRedMaskSize;
      VideoMode->NumberGreenBits = VBEMode->LinGreenMaskSize;
      VideoMode->NumberBlueBits = VBEMode->LinBlueMaskSize;
      VideoMode->RedMask = ((1 << VBEMode->LinRedMaskSize) - 1) << VBEMode->LinRedFieldPosition;
      VideoMode->GreenMask = ((1 << VBEMode->LinGreenMaskSize) - 1) << VBEMode->LinGreenFieldPosition;
      VideoMode->BlueMask = ((1 << VBEMode->LinBlueMaskSize) - 1) << VBEMode->LinBlueFieldPosition;
    }
  }
  else
  {
    VideoMode->NumberRedBits = 
      VideoMode->NumberGreenBits = 
      VideoMode->NumberBlueBits = 6;
    VideoMode->RedMask = 
      VideoMode->GreenMask = 
      VideoMode->BlueMask = 0;
  }
  VideoMode->VideoMemoryBitmapWidth = VBEMode->XResolution;
  VideoMode->VideoMemoryBitmapHeight = VBEMode->YResolution;
  VideoMode->AttributeFlags = VIDEO_MODE_GRAPHICS | VIDEO_MODE_COLOR |
    VIDEO_MODE_NO_OFF_SCREEN;
  if (VideoMode->BitsPerPlane <= 8)
    VideoMode->AttributeFlags |= VIDEO_MODE_PALETTE_DRIVEN;
  VideoMode->DriverSpecificAttributeFlags = 0;
}

/*
* VBEQueryAvailModes
*
* Returns information about each video mode supported by the adapter.
*/

BOOLEAN
VBEQueryAvailModes(
                   PVBE_DEVICE_EXTENSION DeviceExtension,
                   PVIDEO_MODE_INFORMATION ReturnedModes,
                   PSTATUS_BLOCK StatusBlock)
{
  ULONG CurrentModeId;
  PVIDEO_MODE_INFORMATION CurrentMode;
  PVBE_MODEINFO CurrentVBEMode;
  
  for (CurrentModeId = 0, CurrentMode = ReturnedModes,
    CurrentVBEMode = DeviceExtension->ModeInfo;
  CurrentModeId < DeviceExtension->ModeCount;
  CurrentModeId++, CurrentMode++, CurrentVBEMode++)
  {
    VBEQueryMode(DeviceExtension, CurrentMode, CurrentModeId);
  }
  
  StatusBlock->Information =
    sizeof(VIDEO_MODE_INFORMATION) * DeviceExtension->ModeCount;
  
  return TRUE;
}

/*
* VBEQueryCurrentMode
*
* Returns information about current video mode.
*/

BOOLEAN  
VBEQueryCurrentMode(
                    PVBE_DEVICE_EXTENSION DeviceExtension,
                    PVIDEO_MODE_INFORMATION VideoModeInfo,
                    PSTATUS_BLOCK StatusBlock)
{
  StatusBlock->Information = sizeof(VIDEO_MODE_INFORMATION);
  
  VBEQueryMode(
    DeviceExtension,
    VideoModeInfo,
    DeviceExtension->CurrentMode);
  
  return TRUE;
}

/*
* VBESetColorRegisters
*
* Sets the adapter's color registers to the specified RGB values. There
* are code paths in this function, one generic and one for VGA compatible
* controllers. The latter is needed for Bochs, where the generic one isn't
* yet implemented.
*/

BOOLEAN
VBESetColorRegisters(
                     PVBE_DEVICE_EXTENSION DeviceExtension,
                     PVIDEO_CLUT ColorLookUpTable,
                     PSTATUS_BLOCK StatusBlock)
{
  USHORT Entry;
  PULONG OutputEntry;
  ULONG OutputBuffer[256];
  PVOID TrampolineMemory;
  CONTEXT BiosRegisters;

  if (ColorLookUpTable->NumEntries + ColorLookUpTable->FirstEntry > 256)
    return FALSE;
  
    /*
    * For VGA compatible adapters program the color registers directly.
  */
  
  if (!(DeviceExtension->VbeInfo.Capabilities & 2))
  {
    for (Entry = 0; Entry < ColorLookUpTable->NumEntries; Entry++)
    {
      VideoPortWritePortUchar((PUCHAR)0x03c8, (UCHAR) Entry);
      VideoPortWritePortUchar((PUCHAR)0x03c9, ColorLookUpTable->LookupTable[Entry].RgbArray.Red);
      VideoPortWritePortUchar((PUCHAR)0x03c9, ColorLookUpTable->LookupTable[Entry].RgbArray.Green);
      VideoPortWritePortUchar((PUCHAR)0x03c9, ColorLookUpTable->LookupTable[Entry].RgbArray.Blue);
    }
    DPRINT((".VBESetColorRegisters(): program the color registers directly: OK\n"));
    return TRUE;
  }
  else
  {
  /*
  * We can't just copy the values, because we need to swap the Red 
  * and Blue values.
    */
      for (Entry = ColorLookUpTable->FirstEntry,
           OutputEntry = OutputBuffer;
           Entry < ColorLookUpTable->NumEntries + ColorLookUpTable->FirstEntry;
           Entry++, OutputEntry++)
      {
         *OutputEntry =
            (ColorLookUpTable->LookupTable[Entry].RgbArray.Red << 16) |
            (ColorLookUpTable->LookupTable[Entry].RgbArray.Green << 8) |
            (ColorLookUpTable->LookupTable[Entry].RgbArray.Blue);
      }

      TrampolineMemory = (PVOID)0x20000;
    
        VideoPortMoveMemory((PVOID)TrampolineMemory,
          &OutputBuffer,
         (OutputEntry - OutputBuffer) * sizeof(ULONG));

      VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
      BiosRegisters.Eax = VBE_SET_GET_PALETTE_DATA;
      BiosRegisters.Ebx = 0;
      BiosRegisters.Ecx = ColorLookUpTable->NumEntries;
      BiosRegisters.Edx = ColorLookUpTable->FirstEntry;
      BiosRegisters.SegEs = (ULONG)TrampolineMemory >> 4;
      BiosRegisters.Edi =(ULONG)TrampolineMemory & 0xF;
      Ke386CallBios(0x10, &BiosRegisters);
      DPRINT((".VBESetColorRegisters(): program the color registers via VBE call: "));
    if ((BiosRegisters.Eax & 0xFFFF) == VBE_SUCCESS) DPRINT(("OK\n")) else DPRINT(("FALSE\n"));
    return (BiosRegisters.Eax & 0xFFFF) == VBE_SUCCESS;
  }
}

VOID 
vBankMap(
    LONG iBankRead,
    LONG iBankWrite,
    PVOID pvContext
    )
{
//VIDEO_X86_BIOS_ARGUMENTS BiosRegs;
//DPRINT(("vBankMap(%d,%d)\n", iBankRead,iBankWrite));
/*
    _asm {
        mov     eax,iBankRead
        mov     edx,iBankWrite
        lea     ebx,BankSwitchStart
        call    ebx
    }
}
*/
  ULONG BiosRegs[7] = {0x4F05,0,0,iBankRead,0,0,0};
//VideoPortZeroMemory(&BiosRegs, sizeof(BiosRegs));
//BiosRegs.Eax = 0x4F05;
//BiosRegs.Edx = iBankRead;
  VideoPortInt10(pvContext,&BiosRegs);
/*
_asm{
mov eax,iBankRead 
mov edx,iBankWrite
        push    dx
        mov     dx, 3CEh                ; read bank
        mov     ah, al
        xor     ah, 2
        in      al, dx
        push    ax
        mov     al, 0Eh
        out     dx, ax
        pop     ax
        out     dx, al
        pop     ax

        mov     dx, 3C4h                ; write bank
        mov     ah, al
        xor     ah, 2
        in      al, dx                  ; save 3C4 index
        push    ax
        mov     al, 0Eh
        out     dx, ax
        pop     ax
        out     dx, al
*/
}

/*

VP_STATUS
VBEGetBankSelectCode(
    PVBE_DEVICE_EXTENSION HwDeviceExtension,
    PVIDEO_BANK_SELECT BankSelect,
    ULONG BankSelectSize,
    PULONG OutputSize
    )
{
    ULONG codeSize;
    PUCHAR pCodeDest;
    PUCHAR pCodeBank;

    ULONG pMode = HwDeviceExtension->CurrentMode;

    if (BankSelectSize < sizeof(VIDEO_BANK_SELECT)) {

        DPRINT((".VBEGetBankSelectCode(): return ERROR_INSUFFICIENT_BUFFER\n"));
            return ERROR_INSUFFICIENT_BUFFER;

    }

    BankSelect->BankingFlags = 0;
    codeSize = 0;
    pCodeBank = NULL;

        BankSelect->BankingType = VideoBanked1R1W;
        BankSelect->Granularity = 0x10000;    // 64K bank start adjustment
        pCodeBank = &BankSwitchStart;
        codeSize = ((ULONG)&BankSwitchEnd) - ((ULONG)&BankSwitchStart);

    BankSelect->Size = sizeof(VIDEO_BANK_SELECT) + codeSize;
    BankSelect->Length = sizeof(VIDEO_BANK_SELECT);

    if (BankSelectSize < BankSelect->Size ) {
        *OutputSize = sizeof(VIDEO_BANK_SELECT);
        DPRINT((".VBEGetBankSelectCode(0x%02lX < 0x%02lX): return ERROR_MORE_DATA\n",BankSelectSize,BankSelect->Size));
        return ERROR_MORE_DATA;
    }
    else
    DPRINT((".VBEGetBankSelectCode(0x%02lX < 0x%02lX)\n",BankSelectSize,BankSelect->Size));

    BankSelect->BitmapWidthInBytes = HwDeviceExtension->ModeInfo[pMode].BytesPerScanLine;
    BankSelect->BitmapSize = HwDeviceExtension->ModeInfo[pMode].BytesPerScanLine *
    HwDeviceExtension->ModeInfo[pMode].YResolution;
    if (BankSelect->BitmapSize & 0xFFFF) 
    BankSelect->BitmapSize = (BankSelect->BitmapSize & 0xFFFF0000) + 0x10000;

    pCodeDest = (PUCHAR)BankSelect + sizeof(VIDEO_BANK_SELECT);

    if (pCodeBank != NULL) {

        BankSelect->CodeOffset = pCodeDest - (PUCHAR)BankSelect;
        VideoPortMoveMemory(pCodeDest, pCodeBank, codeSize);
        pCodeDest += codeSize;
    }

    *OutputSize = BankSelect->Size;

    DPRINT((".VBEGetBankSelectCode(): return NO_ERROR\n"));
    return NO_ERROR;

} // end VBEGetBankSelectCode()
*/
#if (_WIN32_WINNT >= 0x0500)

/*
* VBEGetPowerState
*
* Queries whether the device can support the requested power state.
*/

VP_STATUS 
VBEGetPowerState(
                 PVOID HwDeviceExtension,
                 ULONG HwId,
                 PVIDEO_POWER_MANAGEMENT VideoPowerControl)
{
  VIDEO_X86_BIOS_ARGUMENTS BiosRegisters;
  PVBE_DEVICE_EXTENSION VBEDeviceExtension = 
    (PVBE_DEVICE_EXTENSION)HwDeviceExtension;
  
  DPRINT((".VBEGetPowerState() called, HwId = 0x%08lX\n", HwId));
  
  if (HwId != DISPLAY_ADAPTER_HW_ID ||
    VideoPowerControl->Length < sizeof(VIDEO_POWER_MANAGEMENT))
    return ERROR_INVALID_FUNCTION;
  
    /*
    * Get general power support information.
  */
  
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_POWER_MANAGEMENT_EXTENSIONS;
  BiosRegisters.Ebx = 0;  
  BiosRegisters.Edi = 0;
  VideoPortInt10(HwDeviceExtension, &BiosRegisters);
  
  if (BiosRegisters.Eax == VBE_NOT_SUPPORTED)
    return ERROR_NOT_SUPPORTED;   
  if (BiosRegisters.Eax != VBE_SUCCESS)
    return ERROR_INVALID_FUNCTION;   
  
    /*
    * Get current power state.
  */
  
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_POWER_MANAGEMENT_EXTENSIONS;
  BiosRegisters.Ebx = 0x2;
  BiosRegisters.Edi = 0;
  VideoPortInt10(HwDeviceExtension, &BiosRegisters);
  
  if (BiosRegisters.Eax == VBE_SUCCESS)
  {
    VideoPowerControl->DPMSVersion = BiosRegisters.Ebx & 0xFF;
    switch (BiosRegisters.Ebx >> 8)
    {
    case 0: VideoPowerControl->PowerState = VideoPowerOn; break;
    case 1: VideoPowerControl->PowerState = VideoPowerStandBy; break;
    case 2: VideoPowerControl->PowerState = VideoPowerSuspend; break;
    case 4: VideoPowerControl->PowerState = VideoPowerOff; break;
    case 5: VideoPowerControl->PowerState = VideoPowerOn; break;
    default: VideoPowerControl->PowerState = VideoPowerUnspecified;
    }
    
    return NO_ERROR;
  }
  
  return ERROR_NOT_SUPPORTED;
}

/*
* VBESetPowerState
*
* Sets the power state of the specified device
*/

VP_STATUS 
VBESetPowerState(
                 PVOID HwDeviceExtension,
                 ULONG HwId,
                 PVIDEO_POWER_MANAGEMENT VideoPowerControl)
{
  VIDEO_X86_BIOS_ARGUMENTS BiosRegisters;
  PVBE_DEVICE_EXTENSION VBEDeviceExtension = 
    (PVBE_DEVICE_EXTENSION)HwDeviceExtension;
  
  DPRINT((".VBESetPowerState() called, HwId = 0x%08lX\n", HwId));
  
  if (HwId != DISPLAY_ADAPTER_HW_ID ||
    VideoPowerControl->Length < sizeof(VIDEO_POWER_MANAGEMENT) ||
    VideoPowerControl->PowerState < VideoPowerOn ||
    VideoPowerControl->PowerState > VideoPowerHibernate)
    return ERROR_INVALID_FUNCTION;
  
  if (VideoPowerControl->PowerState == VideoPowerHibernate)
    return NO_ERROR;
  
    /*
    * Set current power state.
  */
  
  VideoPortZeroMemory(&BiosRegisters, sizeof(BiosRegisters));
  BiosRegisters.Eax = VBE_POWER_MANAGEMENT_EXTENSIONS;
  BiosRegisters.Ebx = 1;
  BiosRegisters.Edi = 0;
  switch (VideoPowerControl->PowerState)
  {
  case VideoPowerStandBy: BiosRegisters.Ebx |= 0x100; break;
  case VideoPowerSuspend: BiosRegisters.Ebx |= 0x200; break;
  case VideoPowerOff: BiosRegisters.Ebx |= 0x400; break;
  }
  
  VideoPortInt10(HwDeviceExtension, &BiosRegisters);
  
  if (BiosRegisters.Eax == VBE_NOT_SUPPORTED)
    return ERROR_NOT_SUPPORTED;   
  if (BiosRegisters.Eax != VBE_SUCCESS)
    return ERROR_INVALID_FUNCTION;   
  
  return VBE_SUCCESS;
}

ULONG
VBEGetChildDescriptor( 
    PVOID HwDeviceExtension,
    PVIDEO_CHILD_ENUM_INFO ChildEnumInfo,
    PVIDEO_CHILD_TYPE pChildType,  
    PVOID pChildDescriptor, 
    PULONG pUId, 
    PULONG pUnused )
{
   VP_STATUS Status;
   PVBE_DEVICE_EXTENSION VBEDeviceExtension = 
     (PVBE_DEVICE_EXTENSION)HwDeviceExtension;
    DPRINT((".VBEGetChildDescriptor() called, ChildIndex = 0x%08lX, Size = 0x%08lX\n", 
    ChildEnumInfo->ChildIndex,ChildEnumInfo->ChildDescriptorSize));

    switch (ChildEnumInfo->ChildIndex) 
    {
        case DISPLAY_ADAPTER_HW_ID:
            *pChildType = VideoChip;
             DPRINT((".VBEGetChildDescriptor(): found a video chip\n"));
            return ERROR_NO_MORE_DEVICES;
        case 0:
            //
            // Case 0 is used to enumerate devices found by the ACPI firmware.
            // We do not currently support ACPI devices
            //
            return ERROR_NO_MORE_DEVICES;
        case 1:
            //
            // Treat index 1 as the monitor
            //
            *pChildType = Monitor;

   if (DDCPassed == 1)
         {   
       VideoPortMoveMemory(pChildDescriptor,
       &VBEDeviceExtension->DDCData,
         ChildEnumInfo->ChildDescriptorSize);

        DPRINT((".VBEGetChildDescriptor(): found a DDC monitor\n"));
       *pUId = DDC_MONITOR;
           }
          else
            {
                DPRINT((".VBEGetChildDescriptor(): found a non-DDC monitor\n"));
                *pUId = NONDDC_MONITOR;
            }
            return ERROR_MORE_DATA;
        default:
            return ERROR_NO_MORE_DEVICES;
    }
}
#endif
