
#include <IOKit/pci/IOPCIDevice.h>
#include <IOKit/graphics/IOFramebuffer.h>

class CLASS : public IOFramebuffer
{
    /*
     * Declare the metaclass information that is used for runtime
     * typechecking of IOKit objects.
     */

    OSDeclareDefaultStructors( AppleSampleFramebuffer );

public:
    enum
    {
#if NUM_BUILTIN_MODES
	kDisplayMode1 = 10,
	kDisplayMode2 = 20,
#endif
	kNumDisplayModes = NUM_BUILTIN_MODES
    };
    
    enum
    {
	kCursorSize	= 64,
	kCursorBytes	= kCursorSize * kCursorSize * sizeof(UInt32),
	kReservedBytes  = 132 * 1024,
	kVRAMSize	= 8 * 1024 * 1024 - kReservedBytes - kCursorBytes
    };
    
    enum
    {
	// this framebuffer has three power states: sleep, doze, wake.
	kSleepState		= 0,
	kDozeState		= 1,
	kWakeState		= 2,
	kPowerStateCount	= 3,
	kPowerStateMax		= kPowerStateCount - 1
    };

    struct ModeDescription
    {
	IODisplayModeID	modeID;
	IOFixed		refreshRate;
	UInt32		flags;
	IOAppleTimingID	appleTimingID;
    };

private:
    struct ClockParams
    {
	// hardware specific parameters for clock PLL
    };
    struct CRTCParams
    {
	// hardware specific parameters for CRTC
    };
    struct InterruptService
    {
	IOFBInterruptProc	handler;
	OSObject *		target;
	void *			ref;
    };

private:
    IOPCIDevice *	fPCIDevice;
    IOMemoryMap *	fRegisterMap;
    volatile UInt32 *	fRegisters;
    IODeviceMemory *	fVRAMMemory;
    IOByteCount		fVRAMBytes;
    IODeviceMemory *	fCursorMemory;
    IOMemoryMap    *    fCursorMap;
    volatile UInt32 *	fCursorBits;
    UInt32 *		fCursorBuffer;

    CRTCParams		fCRTCParams[3];

    IODisplayModeID	fCurrentMode;
    IOIndex 		fCurrentDepth;

    OSArray *		fDetailedTimings;
    UInt32		fDetailedTimingsSeed;

    InterruptService	fInterrupts[2];

    UInt32		fCurrentPowerState;
    UInt32		fVBLInterruptCount;
    UInt8		fEnabled;
    

public:
    /* IOService overrides */

    virtual bool start( IOService * provider );
    virtual void stop( IOService * provider );
    virtual unsigned long maxCapabilityForDomainState( IOPMPowerFlags domainState );
    virtual unsigned long initialPowerStateForDomainState( IOPMPowerFlags domainState );
    virtual unsigned long powerStateForDomainState( IOPMPowerFlags domainState );

    /* IOFramebuffer overrides */

    virtual IOReturn enableController( void );

    virtual const char * getPixelFormats( void );

    virtual IOItemCount getDisplayModeCount( void );

    virtual IOReturn getDisplayModes( IODisplayModeID * allDisplayModes );

    virtual IOReturn getInformationForDisplayMode( IODisplayModeID displayMode,
                    IODisplayModeInformation * info );

    virtual IOReturn getTimingInfoForDisplayMode(
		IODisplayModeID displayMode, IOTimingInformation * info );

    virtual IOReturn connectFlags( IOIndex connectIndex,
                    IODisplayModeID displayMode, IOOptionBits * flags );

    virtual UInt64  getPixelFormatsForDisplayMode( IODisplayModeID displayMode,
                    IOIndex depth );

    virtual IOReturn getPixelInformation(
	IODisplayModeID displayMode, IOIndex depth,
	IOPixelAperture aperture, IOPixelInformation * pixelInfo );

    virtual IOReturn getCurrentDisplayMode( IODisplayModeID * displayMode,
                            IOIndex * depth );

    virtual IOReturn setDisplayMode( IODisplayModeID displayMode,
                            IOIndex depth );

    virtual IODeviceMemory * getVRAMRange( void );
    virtual IODeviceMemory * getApertureRange( IOPixelAperture aperture );

    virtual bool isConsoleDevice( void );

    virtual IOReturn setCLUTWithEntries( IOColorEntry * colors, UInt32 index,
                UInt32 numEntries, IOOptionBits options );

    virtual IOReturn setGammaTable( UInt32 channelCount, UInt32 dataCount,
                    UInt32 dataWidth, void * data );

    virtual IOReturn setAttribute( IOSelect attribute, UInt32 value );
    virtual IOReturn getAttribute( IOSelect attribute, UInt32 * value );
    virtual IOReturn getAttributeForConnection( IOIndex connectIndex,
                    IOSelect attribute, UInt32  * value );

    virtual IOReturn validateDetailedTiming(
                    void * description, IOByteCount descripSize );
    virtual IOReturn setDetailedTimings( OSArray * array );

    virtual IOReturn setCursorImage( void * cursorImage );
    virtual IOReturn setCursorState( SInt32 x, SInt32 y, bool visible );

    virtual IOReturn registerForInterruptType( IOSelect interruptType,
	    IOFBInterruptProc proc, OSObject * target, void * ref,
	    void ** interruptRef );
    virtual IOReturn unregisterInterrupt( void * interruptRef );
    virtual IOReturn setInterruptState( void * interruptRef, UInt32 state );

    /* i2c access */

    virtual void setDDCClock( IOIndex connectIndex, UInt32 value );
    virtual void setDDCData( IOIndex connectIndex, UInt32 value );
    virtual bool readDDCClock( IOIndex connectIndex );
    virtual bool readDDCData( IOIndex connectIndex );

    /* internal methods */

private:
    void initForPM( void );
    
    static void deviceInterrupt(OSObject * target, void * refCon,
				IOService * nub, int source);

    IOReturn doSetPowerState( UInt32 newState );

    IOReturn validateDisplayMode( IODisplayModeID mode,
		const ModeDescription ** info,
		const IODetailedTimingInformationV2 ** timing );

    /* hw methods */

private:
    IOReturn hwEnableVBLInterrupts( void );
    bool hwClearVBLInterrupt( void );
    IOReturn hwSetGamma( UInt32 dataCount, UInt8 * redData, UInt8 * greenData, UInt8 * blueData );
    IOReturn hwSetCursorImage( IOHardwareCursorInfo * hwCursorInfo );

    bool hwGetClockParams( UInt64 freq, ClockParams * result );
    IOReturn hwGetCRTC( IOIndex crtcIndex );
    IOReturn hwSetCRTC( IOIndex crtcIndex, const IODetailedTimingInformation * timing, IOIndex depth );
    IOReturn hwSetDisplayPowerState( IOIndex crtcIndex, UInt32 state );
    IOReturn hwSetPowerState( UInt32 state );

};


