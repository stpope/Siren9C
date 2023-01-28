﻿'From Squeak6.1alpha of 23 January 2023 [latest update: #22394] on 24 January 2023 at 2:54:23 pm'!

Instance Variables

	Interface
	port

Calls

!
	"
	self initialize
	"
	Smalltalk addToStartUpList: self after: (Smalltalk classNamed: #CStandardLibrary)! !
	"Answer the module name"
	
	^ 'libportmidi.2.0.3.dylib'! !
	"Try playing a note."
	"PortMIDILibrary listDeviceTable"

	"PortMIDILibrary new pm_count_devices "
	"PortMIDILibrary new pm_default_input_device "
	"PortMIDILibrary new pm_default_output_device "
	"PortMIDILibrary new pm_test"

	PortMIDILibrary new pm_test! !