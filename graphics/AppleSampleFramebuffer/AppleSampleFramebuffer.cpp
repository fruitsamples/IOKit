
/*
 * Simple framebuffer sample driver.
 */

#define CLASS			AppleSampleFramebuffer

#define NUM_BUILTIN_MODES	0

#include "AppleSampleFramebuffer.h"

#include <IOKit/assert.h>
#include <IOKit/IOBufferMemoryDescriptor.h>
#include <IOKit/graphics/IOGraphicsInterfaceTypes.h>
#include <IOKit/i2c/IOI2CInterface.h>
#include <libkern/OSByteOrder.h>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* 
 * Define the metaclass information that is used for runtime
 * typechecking of IOKit objects. We're a subclass of IOFramebuffer.
 */

#define super IOFramebuffer

OSDefineMetaClassAndStructors( AppleSampleFramebuffer, IOFramebuffer );

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#if 1
#define DEBG(fmt, args...)
#else
#define DEBG(fmt, args...)		\
do { 					\
    IOLog(fmt, ## args);		\
} while( false )
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

bool CLASS::start( IOService * provider )
{
    IODeviceMemory *	mem;
    IOMemoryMap *	map;

    DEBG(__FILE__ "::start\n");

    if( !super::start( provider ))
        return( false );

    /*
     * Our provider class is specified in the driver property table
     * as IOPCIDevice, so the provider must be of that class.
     * The assert is just to make absolutely sure for debugging.
     */

    assert( OSDynamicCast( IOPCIDevice, provider ));
    fPCIDevice = (IOPCIDevice *) provider;

    /*
     * Enable memory response from the card
     */
    fPCIDevice->setMemoryEnable( true );

    /*
     * Log some info about the device
     */

    /* print all the device's memory ranges */
    for( UInt32 index = 0;
         index < fPCIDevice->getDeviceMemoryCount();
         index++ ) {

        mem = fPCIDevice->getDeviceMemoryWithIndex( index );
        assert( mem );
        DEBG("Range[%ld] %08lx:%08lx\n", index,
              mem->getPhysicalAddress(), mem->getLength());
    }


    /* map a range based on its config space base address register */
    map = fPCIDevice->mapDeviceMemoryWithRegister(
                                  kIOPCIConfigBaseAddress1 );
    if (!map)
	return (false);
    {
        DEBG("Range@0x%x (%08lx) mapped to kernel virtual address %08x\n",
                kIOPCIConfigBaseAddress1,
                map->getPhysicalAddress(),
                map->getVirtualAddress());
	fRegisterMap = map;
	fRegisters = (volatile UInt32 *) map->getVirtualAddress();
    }

    /* look up a range based on its config space base address register */
    mem = fPCIDevice->getDeviceMemoryWithRegister(
                                  kIOPCIConfigBaseAddress0 );
    if( mem )
        DEBG("Range@0x%x %08lx:%08lx\n", kIOPCIConfigBaseAddress0,
                mem->getPhysicalAddress(), mem->getLength());

    fVRAMBytes = kVRAMSize;
    // On this HW assume vram is found in aperture0 of length kVRAMSize,
    // make a memory descriptor that contains all the addressable vram.
    fVRAMMemory = IODeviceMemory::withSubRange( mem, 0, fVRAMBytes );

    if( !fVRAMMemory )
    {
        IOLog("!fVRAMMemory\n");
        return( false );
    }

    // make a memory descriptor that addresses cursor vram.
    fCursorMemory = IODeviceMemory::withSubRange( mem, fVRAMBytes, kCursorBytes );
    if( !fCursorMemory )
    {
        IOLog("!fCursorMemory\n");
        return( false );
    }

    /* map cursor memory */
    fCursorMap = fCursorMemory->map();
    if (!fCursorMap)
    {
        IOLog("!fCursorMap\n");
        return( false );
    }
    fCursorBits = (volatile UInt32 *) fCursorMap->getVirtualAddress();
    DEBG("curs vram (%08lx) mapped to kernel virtual address %p\n",
	    fCursorMap->getPhysicalAddress(),
	    fCursorBits);

    fCursorBuffer = IONew(UInt32, kCursorSize * kCursorSize);
    if (!fCursorBuffer)
    {
        IOLog("!fCursorBuffer\n");
        return( false );
    }

    hwGetCRTC(0);

    // publish range limits for programmable timings

    IODisplayTimingRange range;
    bzero(&range, sizeof(range));

    range.minPixelClock =  25000000ULL;
    range.maxPixelClock = 350000000ULL;

    range.supportedSyncFlags = 0
				| kIORangeSupportsSeparateSyncs
				| kIORangeSupportsSyncOnGreen
				| kIORangeSupportsCompositeSync
				| kIORangeSupportsVSyncSerration;
    range.supportedSignalLevels = 0
				| kIORangeSupportsSignal_0700_0300
				| kIORangeSupportsSignal_0714_0286
				| kIORangeSupportsSignal_1000_0400
				| kIORangeSupportsSignal_0700_0000;
    
    range.maxFrameRate  		= 0xffffffff;
    range.maxLineRate   		= 0xffffffff;

    range.maxHorizontalTotal		= 4095;
    range.maxVerticalTotal		= 4095;

    range.maxHorizontalActiveClocks	= 4095;
    range.maxHorizontalBlankingClocks	= 4095;
    range.maxHorizontalSyncOffsetClocks	= 4095;
    range.maxHorizontalPulseWidthClocks	= 4095;

    range.maxVerticalActiveClocks	= 4095;
    range.maxVerticalBlankingClocks	= 4095;
    range.maxVerticalSyncOffsetClocks	= 4095;
    range.maxVerticalPulseWidthClocks	= 4095;

    range.maxHorizontalBorderLeft	= 4095;
    range.maxHorizontalBorderRight	= 4095;
    range.maxVerticalBorderTop		= 4095;
    range.maxVerticalBorderBottom	= 4095;

    range.charSizeHorizontalActive	= 1;
    range.charSizeHorizontalBlanking	= 1;
    range.charSizeHorizontalSyncOffset	= 1;
    range.charSizeHorizontalSyncPulse	= 1;
    range.charSizeVerticalActive	= 1;
    range.charSizeVerticalBlanking	= 1;
    range.charSizeVerticalSyncOffset	= 1;
    range.charSizeVerticalSyncPulse	= 1;
    range.charSizeHorizontalBorderLeft	= 1;
    range.charSizeHorizontalBorderRight = 1;
    range.charSizeVerticalBorderTop	= 1;
    range.charSizeVerticalBorderBottom	= 1;
    range.charSizeHorizontalTotal	= 1;
    range.charSizeVerticalTotal		= 1;

    setProperty(kIOFBTimingRangeKey, &range, sizeof(range));

    // make i2c bus 0 public
    OSArray *		array = 0;
    OSDictionary *	dict;
    dict = OSDictionary::withCapacity(1);
    if (dict)
    {
	setNumber(dict, kIOI2CInterfaceIDKey, 0);
	array = OSArray::withObjects((const OSObject **) &dict, 1, 1);
	dict->release();
    }
    if (array)
    {
        setProperty(kIOFBI2CInterfaceInfoKey, array);
	array->release();
    }

    // read a config space register
    DEBG("Config register@0x%x = %08lx\n", kIOPCIConfigCommand,
          fPCIDevice->configRead32(kIOPCIConfigCommand) );

    if( !fVRAMMemory ) {
        IOLog("!fVRAMMemory\n");
        return( false );
    }

    return( true );
}

/*
 * We'll come here when the device goes away, or the driver is unloaded.
 */
 
void CLASS::stop( IOService * provider )
{
    if (fCursorBuffer)
    {
	IODelete(fCursorBuffer, UInt32, kCursorSize * kCursorSize);
	fCursorBuffer = 0;
    }

    if (fCursorMap) {
        fCursorMap->release();
        fCursorMap = 0;
    }
    if (fCursorMemory) {
        fCursorMemory->release();
        fCursorMemory = 0;
    }
    if (fVRAMMemory) {
        fVRAMMemory->release();
        fVRAMMemory = 0;
    }
    if (fRegisterMap) {
        fRegisterMap->release();
        fCursorMap = 0;
    }

    PMstop();

    super::stop( provider );
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

IOReturn CLASS::validateDetailedTiming(
                    void * description, IOByteCount descripSize )
{
    return( kIOReturnSuccess );
}

IOReturn CLASS::setDetailedTimings( OSArray * array )
{
    if (array)
        setProperty( kIOFBDetailedTimingsKey, array );	// retains
    else
        removeProperty( kIOFBDetailedTimingsKey );

    fDetailedTimings = array;
    fDetailedTimingsSeed++;
    return (kIOReturnSuccess);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static const CLASS::ModeDescription modeTable[] = {
#if NUM_BUILTIN_MODES
  { CLASS::kDisplayMode1, 60<<16,
	kDisplayModeValidFlag | kDisplayModeSafeFlag | kDisplayModeDefaultFlag,
	kIOTimingIDAppleNTSC_FF },
  { CLASS::kDisplayMode2, 50<<16,
	kDisplayModeValidFlag | kDisplayModeSafeFlag,
	kIOTimingIDApplePAL_FF },
#endif
};

static const CLASS::ModeDescription programmedMode =
  { 0, 0, 0, 0 };

static const IODetailedTimingInformation timingTable[] = {
#if NUM_BUILTIN_MODES
  { { 0 }, 0, 0, 0,
	0, 0,
	31500, 31500, 31500,
	640, 128, 32, 32,
	480, 10, 3, 1,
	0, 0, 0, 0,
	0, 0, 0, 0,
	{ 0 } },
  { { 0 }, 0, 0, 0,
	0, 0,
	31500, 31500, 31500,
	768, 128, 32, 32,
	576, 10, 3, 1,
	0, 0, 0, 0,
	0, 0, 0, 0,
	{ 0 } }
#endif
};

IOReturn CLASS::validateDisplayMode( IODisplayModeID mode,
		const ModeDescription ** desc, const IODetailedTimingInformation ** timing )
{
    IOReturn	err = kIOReturnBadArgument;
    OSData  *	data;

    if (fDetailedTimings && (mode & 0xffff0000))
    {
	if (desc)
	    *desc = &programmedMode;
	data = OSDynamicCast(OSData, fDetailedTimings->getObject(mode & 0x7fff));
	if (data)
	{
	    if (timing)
		*timing = (IODetailedTimingInformation *) data->getBytesNoCopy();

	    err = kIOReturnSuccess;
	}
    }
    else
    {
	for(UInt32 index = 0; index < (sizeof(modeTable) / sizeof(modeTable[0])); index++)
	{
	    if(modeTable[index].modeID == mode)
	    {
		if (desc)
		    *desc 	= modeTable + index;
		if (timing)
		    *timing = timingTable + index;
    
		err = kIOReturnSuccess;
		break;
	    }
	}
    }

    return( err );
}

IOReturn CLASS::getTimingInfoForDisplayMode(
		IODisplayModeID displayMode, IOTimingInformation * info )
{
    IOReturn				err;
    const ModeDescription *		desc;
    const IODetailedTimingInformation *	timing;

    if( (err = validateDisplayMode( displayMode, &desc, &timing )))
        return (err);

    info->appleTimingID = desc->appleTimingID;

    if( info->flags & kIODetailedTimingValid)
    {
	info->detailedInfo.v2 = *timing;
    }

    return( kIOReturnSuccess);
}

IOReturn CLASS::connectFlags( IOIndex connectIndex,
                    IODisplayModeID displayMode, IOOptionBits * flags )
{
    IOReturn				err;
    const ModeDescription *		desc;
    const IODetailedTimingInformation *	timing;

    if( (err = validateDisplayMode( displayMode, &desc, &timing )))
        return (err);

    *flags = desc->flags;

    return( kIOReturnSuccess );
}

IOReturn CLASS::enableController( void )
{
    IOReturn		err;

    do {

        // native gamma format - 256 x 8-bit per channel

	setProperty(kIOFBGammaWidthKey,   8, 32);
	setProperty(kIOFBGammaCountKey, 256, 32);

        // setup the hardware to display the startup mode here.
	// or return kIODisplayModeIDBootProgrammable if display state is unknown
	fCurrentMode = kIODisplayModeIDBootProgrammable;
//	fCurrentMode = kDisplayMode1;

	hwEnableVBLInterrupts();

	err = fPCIDevice->registerInterrupt(0, this, &deviceInterrupt, 0 /*ref*/);
	err = fPCIDevice->enableInterrupt(0);

        // initialize power management of the device
        initForPM();

	fEnabled = true;
	err = kIOReturnSuccess;

    } while( false);

    return( err);
}

void CLASS::deviceInterrupt(OSObject * target, void * refCon,
					     IOService * nub, int source)
{
    CLASS *		self = (CLASS *) target;
    IOFBInterruptProc	proc;

    if (!self->hwClearVBLInterrupt())
	return;

    self->fVBLInterruptCount++;

    if ((proc = self->fInterrupts[0].handler))
	(*proc) (self->fInterrupts[0].target, self->fInterrupts[0].ref);
}

IOReturn CLASS::registerForInterruptType( IOSelect interruptType,
        IOFBInterruptProc proc, OSObject * target, void * ref,
        void ** interruptRef )
{
    IOReturn err = kIOReturnSuccess;

    if (interruptType != kIOFBVBLInterruptType)
        return (kIOReturnUnsupported);

    fInterrupts[0].target  = target;
    fInterrupts[0].ref     = ref;
    fInterrupts[0].handler = proc;

    *interruptRef = &fInterrupts[0];

    return (err);
}

IOReturn CLASS::unregisterInterrupt( void * interruptRef )
{
    InterruptService * service = (InterruptService *) interruptRef;

    service->handler = 0;

    return (kIOReturnSuccess);
}

IOReturn CLASS::setInterruptState( void * interruptRef, UInt32 state )
{
    return (kIOReturnUnsupported);
}


const char * CLASS::getPixelFormats( void )
{
    static const char *	formats = IO16BitDirectPixels "\0"
                                    IO32BitDirectPixels;

    return( formats );
}

IOItemCount CLASS::getDisplayModeCount( void )
{
    return( kNumDisplayModes );
}

IOReturn CLASS::getDisplayModes(
			IODisplayModeID * allDisplayModes )
{
#if NUM_BUILTIN_MODES
    allDisplayModes[0] = kDisplayMode1;
    allDisplayModes[1] = kDisplayMode2;
#endif

    return( kIOReturnSuccess);
}

IOReturn CLASS::getInformationForDisplayMode(
		IODisplayModeID displayMode,
		IODisplayModeInformation * info )
{
    IOReturn				err;
    const ModeDescription *		desc;
    const IODetailedTimingInformation *	timing;

    if( (err = validateDisplayMode( displayMode, &desc, &timing )))
        return (err);

    bzero( info, sizeof( *info));

    info->maxDepthIndex	= 1;
    info->nominalWidth	= timing->horizontalActive;
    info->nominalHeight	= timing->verticalActive;
    info->refreshRate	= desc->refreshRate;

    return(kIOReturnSuccess);
}

UInt64 CLASS::getPixelFormatsForDisplayMode( 
		IODisplayModeID displayMode, IOIndex depth )
{
    return( 0 );
}

IOReturn CLASS::getPixelInformation(
	IODisplayModeID displayMode, IOIndex depth,
	IOPixelAperture aperture, IOPixelInformation * info )
{
    IOReturn				err;
    const ModeDescription *		desc;
    const IODetailedTimingInformation *	timing;

    if (aperture)
        return (kIOReturnUnsupportedMode);

    if( (err = validateDisplayMode( displayMode, &desc, &timing )))
        return (err);

    bzero( info, sizeof( *info));

    info->activeWidth		= timing->horizontalActive;
    info->activeHeight		= timing->verticalActive;
    info->bytesPerPlane		= 0;

    switch( depth ) {
      case 0:
        strcpy(info->pixelFormat, IO16BitDirectPixels );
        info->pixelType 	= kIORGBDirectPixels;
        info->componentMasks[0] = 0x7c00;
        info->componentMasks[1] = 0x03e0;
        info->componentMasks[2] = 0x001f;
        info->bitsPerPixel 	= 16;
        info->componentCount 	= 3;
        info->bitsPerComponent	= 5;
        info->bytesPerRow       = timing->horizontalActive * 2;
	break;
      case 1:
        strcpy(info->pixelFormat, IO32BitDirectPixels );
        info->pixelType 	= kIORGBDirectPixels;
        info->componentMasks[0] = 0x00ff0000;
        info->componentMasks[1] = 0x0000ff00;
        info->componentMasks[2] = 0x000000ff;
        info->bitsPerPixel 	= 32;
        info->componentCount 	= 3;
        info->bitsPerComponent	= 8;
        info->bytesPerRow       = timing->horizontalActive * 4;
	break;
      default:
        return( kIOReturnUnsupportedMode);
    }

    return( kIOReturnSuccess);
}

IOReturn CLASS::getCurrentDisplayMode( 
		IODisplayModeID * displayMode, IOIndex * depth )
{
    if( displayMode)
	*displayMode = fCurrentMode;
    if( depth)
	*depth = fCurrentDepth;

    return( kIOReturnSuccess);
}

IOReturn CLASS::setDisplayMode( 
		IODisplayModeID displayMode, IOIndex depth )
{
    IOReturn				err;
    const ModeDescription *		desc;
    const IODetailedTimingInformation *	timing;

    if( (err = validateDisplayMode( displayMode, &desc, &timing )))
        return (err);

    hwSetCRTC( 0, timing, depth );

    fCurrentMode  = displayMode;
    fCurrentDepth = depth;

    return( kIOReturnSuccess );
}

IODeviceMemory * CLASS::getApertureRange( IOPixelAperture aper )
{
    if( kIOFBSystemAperture != aper)
        return( 0 );

    if (fVRAMMemory)
	fVRAMMemory->retain();

    // return a reference to the caller
    return( fVRAMMemory );
}

IODeviceMemory * CLASS::getVRAMRange( void )
{
    if (!fEnabled)
	return (0);

    if (fVRAMMemory)
        fVRAMMemory->retain();

    return (fVRAMMemory);
}

bool CLASS::isConsoleDevice( void )
{
    return( true );
}

IOReturn CLASS::setGammaTable( UInt32 channelCount,
                            UInt32 dataCount, UInt32 dataWidth, void * data )
{
    UInt8 * redData;
    UInt8 * greenData;
    UInt8 * blueData;

    if ((dataCount != 256) || (dataWidth != 8))
	return (kIOReturnUnsupported);

    redData = (UInt8 *) data;
    if (channelCount < 3)
	greenData = blueData = redData;
    else
    {
	greenData = redData + dataCount;
	blueData  = redData + dataCount;
    }

    hwSetGamma( dataCount, redData, greenData, blueData );

    return( kIOReturnSuccess );
}

IOReturn CLASS::setCLUTWithEntries(
                    IOColorEntry * colors, UInt32 index, UInt32 numEntries,
                    IOOptionBits options )
{
    return( kIOReturnSuccess );
}

IOReturn CLASS::setAttribute( IOSelect attribute, UInt32 value )
{
    IOReturn err;

    switch( attribute )
    {
        case kIOPowerAttribute:
            err = doSetPowerState( value );
	    break;

	default:
	    err = super::setAttribute( attribute, value );
    }

    return( err );
}

IOReturn CLASS::getAttribute( IOSelect attribute, UInt32 * value )
{
    IOReturn err;

    switch( attribute )
    {
	// support HW cursors
        case kIOHardwareCursorAttribute:
	// would like CLUT updates VBL synced
	case kIODeferCLUTSetAttribute:
	    *value = true;
	    err = kIOReturnSuccess;
	    break;

	default:
	    err = super::getAttribute( attribute, value );
    }

    return( err );
}

IOReturn CLASS::getAttributeForConnection( IOIndex connectIndex,
				    IOSelect attribute, UInt32  * value )
{
    IOReturn err;

    switch( attribute )
    {
        case kConnectionSupportsLLDDCSense:
            err = kIOReturnSuccess;
	    break;

	default:
	    err = super::getAttributeForConnection( connectIndex, attribute, value );
    }

    return( err );
}

IOReturn CLASS::setCursorImage( void * cursorImage )
{
    bool			ok;
    IOReturn			ret;
    IOHardwareCursorDescriptor	hwDesc;
    IOHardwareCursorInfo	hwCursorInfo;

    hwDesc.majorVersion			= kHardwareCursorDescriptorMajorVersion;
    hwDesc.minorVersion			= kHardwareCursorDescriptorMinorVersion;
    hwDesc.height			= kCursorSize;
    hwDesc.width			= kCursorSize;
    hwDesc.bitDepth			= kIO32ARGBPixelFormat;
    hwDesc.maskBitDepth			= 0;
    hwDesc.numColors			= 0;
    hwDesc.colorEncodings		= 0;
    hwDesc.flags			= 0;
    hwDesc.supportedSpecialEncodings	= 0;
    hwDesc.colorEncodings		= 0;

    hwCursorInfo.majorVersion		= kHardwareCursorInfoMajorVersion;
    hwCursorInfo.minorVersion		= kHardwareCursorInfoMinorVersion;
    hwCursorInfo.colorMap		= 0;
    hwCursorInfo.hardwareCursorData	= (UInt8 *) fCursorBuffer;

    ok = convertCursorImage( cursorImage, &hwDesc, &hwCursorInfo );

    if (!ok)
	return (kIOReturnUnsupported);

    ret = hwSetCursorImage(&hwCursorInfo);

    return (ret);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void CLASS::initForPM( void )
{
    IOPMPowerState powerStates[ kPowerStateCount ] = {
      // version,
      // capabilityFlags, outputPowerCharacter, inputPowerRequirement,
      { 1, 0,		     0,	          0,	       0, 0, 0, 0, 0, 0, 0, 0 },
      { 1, 0,                0,           IOPMPowerOn, 0, 0, 0, 0, 0, 0, 0, 0 },
      { 1, IOPMDeviceUsable, IOPMPowerOn, IOPMPowerOn, 0, 0, 0, 0, 0, 0, 0, 0 }
      // staticPower, unbudgetedPower, powerToAttain, timeToAttain, settleUpTime, 
      // timeToLower, settleDownTime, powerDomainBudget
    };
    bool dozeOnly;

    dozeOnly = false;

    if( dozeOnly) {
        powerStates[kSleepState].capabilityFlags |= kIOPMPreventSystemSleep;
        powerStates[kDozeState].capabilityFlags  |= kIOPMPreventSystemSleep;
        powerStates[kWakeState].capabilityFlags  |= kIOPMPreventSystemSleep;
    }

    fCurrentPowerState = kWakeState;

    // register ourselves with superclass policy-maker
    registerPowerDriver( this, powerStates, kPowerStateCount );
    // no sleep until children
    temporaryPowerClampOn();
    // not below doze until system sleep
    changePowerStateTo( kDozeState );
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
// maxCapabilityForDomainState
//
// This simple device needs only power.  If the power domain is supplying
// power, the frame buffer can be on.  If there is no power it can only be off.
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

unsigned long CLASS::maxCapabilityForDomainState(
					IOPMPowerFlags domainState )
{
   if( domainState & IOPMPowerOn)
       return( kPowerStateMax );
   else
       return( kSleepState );
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
// initialPowerStateForDomainState
//
// The power domain may be changing state.  If power is on in the new
// state, that will not affect our state at all.  If domain power is off,
// we can attain only our lowest state, which is off.
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

unsigned long CLASS::initialPowerStateForDomainState(
					 IOPMPowerFlags domainState )
{
   if( domainState & IOPMPowerOn)
       return( kPowerStateMax );
   else
       return( kSleepState );
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
// powerStateForDomainState
//
// The power domain may be changing state.  If power is on in the new
// state, that will not affect our state at all.  If domain power is off,
// we can attain only our lowest state, which is off.
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

unsigned long CLASS::powerStateForDomainState( IOPMPowerFlags domainState )
{
   if( domainState & IOPMPowerOn)
       return( pm_vars->myCurrentState );
   else
       return( kSleepState );
}

IOReturn CLASS::doSetPowerState( UInt32 newState )
{
    UInt32	oldState;
    IOIndex	crtcIndex = 0;
    IOIndex	postEvent = 0;

    if( newState == fCurrentPowerState)
        return( kIOReturnSuccess );

    if( newState > kPowerStateMax)
        newState = kPowerStateMax;

    oldState = fCurrentPowerState;

    // tell superclass what we're doing

    if( kWakeState == oldState) {
        super::handleEvent( kIOFBNotifyWillPowerOff );
        postEvent = kIOFBNotifyDidPowerOff;
	hwSetDisplayPowerState( crtcIndex, kSleepState );
    } else if( kWakeState == newState) {
        super::handleEvent( kIOFBNotifyWillPowerOn );
        postEvent = kIOFBNotifyDidPowerOn;
    }

    // when going to sleep state, protect vram against access from user mappings

    if( kSleepState == newState) {
        IOMemoryDescriptor * vram;
        if( (vram = getVRAMRange())) {
            vram->redirect( kernel_task, true );
            vram->release();
        }
    }

    // change state

    hwSetPowerState( newState );

    fCurrentPowerState = newState;

    // when coming from sleep state, undo the vram protection
    
    if( kSleepState == oldState) {
        IOMemoryDescriptor * vram;
        if( (vram = getVRAMRange())) {
            vram->redirect( kernel_task, false );
            vram->release();
        }
    }

    // tell superclass what we've done

    if( postEvent)
    {
        super::handleEvent( postEvent );
        if (kIOFBNotifyDidPowerOn == postEvent)
        {
            hwSetDisplayPowerState( crtcIndex, kWakeState );
        }
    }

    return( kIOReturnSuccess );
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


bool CLASS::hwGetClockParams( UInt64 freq, ClockParams * result )
{
    // calculate clock parameters for given frequency
    return (true);
}

IOReturn CLASS::hwGetCRTC( IOIndex crtcIndex )
{
    // program CRTC for detailed timing description and depth

    return (kIOReturnSuccess);
}

IOReturn CLASS::hwSetCRTC( IOIndex crtcIndex, const IODetailedTimingInformation * timing, IOIndex depth )
{
    // program CRTC for detailed timing description and depth

    return (kIOReturnSuccess);
}

IOReturn CLASS::hwSetDisplayPowerState( IOIndex crtcIndex, UInt32 state )
{
    // set sync generation and any other registers for display sleep

    return (kIOReturnSuccess);
}

IOReturn CLASS::hwSetPowerState( UInt32 state )
{
    // set HW to power state

    return (kIOReturnSuccess);
}

void CLASS::setDDCClock( IOIndex bus, UInt32 value )
{
    // set the i2c clock line on a bus to kIODDCLow, kIODDCHigh, or kIODDCTristate
}

void CLASS::setDDCData( IOIndex bus, UInt32 value )
{
    // set the i2c data line on a bus to kIODDCLow, kIODDCHigh, or kIODDCTristate
}

bool CLASS::readDDCClock( IOIndex bus )
{
    // read the i2c clock line on a bus
    return (false);
}

bool CLASS::readDDCData( IOIndex bus )
{
    // read the i2c data line on a bus
    return (false);
}

IOReturn CLASS::hwEnableVBLInterrupts( void )
{
    // make the HW generate vertical blank interrupts

    return (kIOReturnSuccess);
}

bool CLASS::hwClearVBLInterrupt( void )
{
    // clear the HW generation of vertical blank interrupt
    // return true if the HW was generating an interrupt

    bool didInterrupt = true;

    return (didInterrupt);
}

IOReturn CLASS::hwSetGamma( UInt32 dataCount, UInt8 * redData, UInt8 * greenData, UInt8 * blueData )

{
    // set gamma table with dataCount entries of red, green, blue 8 bit data

    return( kIOReturnSuccess );
}

IOReturn CLASS::hwSetCursorImage( IOHardwareCursorInfo * hwCursorInfo )
{
    // set hardware cursor to the image data found in fCursorBuffer,
    // of size/format in hwCursorInfo

    return( kIOReturnUnsupported );
}

IOReturn CLASS::setCursorState( SInt32 x, SInt32 y, bool visible )
{
    // set hardware cursor position (relative to top left of framebuffer) 
    // and visibility.
    // if x or y is negative, the cursor is partially off the top or left of the screen

    return (kIOReturnSuccess);
}

