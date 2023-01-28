DeviceModel subclass: #MIDIDevice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIDevice commentStamp: '<historical>' prior: 0!MIDIDevice is a subclass of Model whose instances are used to model MIDI input/output hardware devices.
The abstract class MIDIDevice implements the generic MIDI note on/off type events.
Subclasses of MIDIDevice exist for specific models of devices and  implement the device-specific (system exclusive) commands.
MIDIDevice are passed Events and channel numbers by their voices and generate commands as ByteArrays that they pass to MIDIPorts.
Several devices may share one port (if there are several MIDI-capable devices on one cable), and one voice may point to several device/channel pairs on one or more devices.

Instance variables:
	port <MidiPort> the MIDIPort I use
	stream <WriteStream> logging stream
	debug <Boolean> verbosity flag for Transcript dumping

Class variable:
	MStream	<WriteStream> the stream I dump by bytes on if my port is nil (good for debugging new subclasses)

Standard MIDI Commands:
		Note On = 9n kk vv
		Note Off = 8n kk vv
		Key Pressure = An kk vv
		Pitch Wheel = En ll ll
		After Touch Channel Pressure = Dn vv
		Control Change = Bn cc vv
		Program change = Cn pp

Relevant Constants for description of MIDI commands:
		n = 4-bit channel number
		kk = 7-bit key number
		vv = 7-bit key velocity
		ll = 7-bit low-order value
		hh = 7-bit high-order value
		cc = 7-bit control number
		pp = 7-bit program number!!MIDIDevice methodsFor: 'accessing'!port
	"Answer the receiver's I/O port."

	^port! !!MIDIDevice methodsFor: 'accessing'!port: aPort
	"Set the receiver's I/O port."

	port := aPort! !!MIDIDevice methodsFor: 'accessing'!stream
	"Answer the receiver's MIDI stream ID."

	^stream! !!MIDIDevice methodsFor: 'accessing'!stream: aNumber
	"Set the receiver's MIDI stream ID."

	stream := aNumber! !!MIDIDevice methodsFor: 'initialization'!checkPort
	"Ensure that the receiver's output port is initialized."

	port == nil
		ifTrue: [port := MIDIPort default].! !!MIDIDevice methodsFor: 'initialization'!close
	"Release the receiver's hold on the port."

"	Transcript show: 'MidiDevice reset: ', port printString; cr."
	port close.
	port := nil! !!MIDIDevice methodsFor: 'initialization'!initialize
	"Set the receiver up on the default output port."
 
"	Transcript show: 'MidiDevice init...'; cr."
"	port == nil
		ifTrue: [port := MIDIPort default].
	stream := 1."! !!MIDIDevice methodsFor: 'initialization'!initialize: thePort
	"Set the receiver up on the given output port."

"	Transcript show: 'MidiDevice init...'; cr."
	port == nil
		ifTrue: [port := thePort].! !!MIDIDevice methodsFor: 'note events' stamp: 'stp 1/5/2023 07:25'!play: anEvent at: aTime 	"Play the given event on my port as a default MIDI	noteOn/noteOff pair."	| p d a v now ch |	now := Time microsecondClock.	port		ifNil: [self halt.			port := MIDIPort concreteClass default].	(Delay		forMicroseconds: (aTime - now max: 1000)) wait.	self checkPort.	v := self mapVoice: anEvent voice.	anEvent pitch isNil		ifTrue: [a := anEvent value rounded.			"if it's a control change"			ch := anEvent controller.			ch				ifNotNil: [port						controlChange: index						controller: ch						to: a]]		ifFalse: [p := self mapPitch: anEvent pitch.			d := self mapDuration: anEvent dur.			a := self mapAmplitude: anEvent ampl.			port				play: index				pitch: p value				dur: d				amp: a				voice: v]! !!MIDIDevice methodsFor: 'note events' stamp: 'stp 1/17/2023 18:51'!play: anEvent at: aTime chan: chan 	"Play the given event on my port as a default MIDI	noteOn/noteOff pair."	| p d a v now ch |	now := Time microsecondClock.	port ifNil:		 [port := MIDIPort concreteClass default].	(port status == #open) ifFalse:			[port openOutput: index].	(Delay forMicroseconds: (aTime - now max: 1000)) wait.	self checkPort.	v := self mapVoice: anEvent voice.	anEvent pitch isNil		ifTrue: [a := anEvent value rounded.			"if it's a control change"			ch := anEvent controller.			ch ifNotNil: [port						controlChange: v						controller: ch						to: a]]		ifFalse: [p := self mapPitch: anEvent pitch.			d := self mapDuration: anEvent dur.			a := self mapAmplitude: anEvent ampl.			port play: index				pitch: p value				dur: d				amp: a				voice: chan]! !!MIDIDevice methodsFor: 'note events' stamp: 'stp 1/5/2023 07:26'!play: anEvent at: aTime on: chan 	"Play the given event on my port as a default MIDI	noteOn/noteOff pair."	| p d a v now ch |	now := Time microsecondClock.	port		ifNil: [self halt.			port := MIDIPort concreteClass default].	(Delay		forMicroseconds: (aTime - now max: 1000)) wait.	self checkPort.	v := self mapVoice: anEvent voice.	anEvent pitch isNil		ifTrue: [a := anEvent value rounded.			"if it's a control change"			ch := anEvent controller.			ch				ifNotNil: [port						controlChange: v						controller: ch						to: a]]		ifFalse: [p := self mapPitch: anEvent pitch.			d := self mapDuration: anEvent dur.			a := self mapAmplitude: anEvent ampl.			port				play: index				pitch: p value				dur: d				amp: a				voice: v]! !!MIDIDevice methodsFor: 'parameter mapping'!mapAmplitude: aVal
	"Map a numerical or symbolic amplitude to a MIDI-compatible 
	 volume number (key velocity)"


	^(aVal species == Amplitude 
		ifTrue: [aVal asMIDI value]
		ifFalse: [aVal class == SmallInteger 
			ifTrue: [^aVal]		"assume a volume number"
									"assume a dynamic name"
			ifFalse: [(aVal isNil or: [aVal value isNil])
				ifTrue: [64]
				ifFalse: [Amplitude value: aVal asMIDI]]])! !!MIDIDevice methodsFor: 'parameter mapping' stamp: 'stp 12/30/2022 17:29'!mapDuration: t1 	^ t1 species == MDuration		ifTrue: [t1 asMS value]		ifFalse: [t1 class == SmallInteger				ifTrue: [t1]				ifFalse: [t1 isNil						ifFalse: [Duration value: t1 asMidi]]]! !!MIDIDevice methodsFor: 'parameter mapping'!mapPitch: aVal
	"Map a numerical or symbolic pitch to a MIDI-compatible note number."

	aVal ifNil: [^nil].
	^(aVal species == Pitch
		ifTrue: [aVal asMIDI value]
		ifFalse: [aVal class == SmallInteger
			ifTrue: [aVal]			"assume a note number"
										"assume a note name or freq"
			ifFalse: [Pitch value: aVal asMidi]])! !!MIDIDevice methodsFor: 'parameter mapping' stamp: 'stp 1/4/2023 16:07'!mapVoice: aVal 	"Map a numerical or symbolic pitch to a MIDI-compatible note	number. "	^ aVal isInteger		ifTrue: [(aVal truncated min: 15)				max: 0]		ifFalse: [aVal species == Voice				ifTrue: [aVal channel]				ifFalse: [0]]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIDevice class	instanceVariableNames: ''!!MIDIDevice class methodsFor: 'examples'!playOnDevice
	"Play a note out MIDI from the device level."
	"self halt. MIDIDevice playOnDevice"
	"MIDIPort allNotesOff"
	"MIDIDevice initialize"
	"MStream inspect"

	| device port |
	port := MIDIPort new openOutput.
	device := MIDIDevice on: port.
	device play: (60 key, 500 msec, 96 velocity, (#voice -> 0)) at: 0.
	(Delay forSeconds: 1) wait.
	port allNotesOff.
	device close.
	Transcript show: ' done.'; cr.! !!MIDIDevice class methodsFor: 'examples'!scheduleExample
	"Play MIDI using the event scheduler."
	"MIDIDevice scheduleExample"
	"EventScheduler interrupt; flush"

	 | device port voice scale start |
	port := MIDIPort default. 
	device := MIDIDevice on: port.
	voice := MIDIVoice on: device.
	scale := EventList scaleFrom: 36 to: 60 in: 2.0.
	scale scale: #duration by: 2.
	start := Time microsecondClock + 500000.
	scale events do: [ :ass |
		ass event voice: voice.
		ass key: (ass key value + start) usec.
		EventScheduler addClient: ass].
	"self halt."
	EventScheduler run! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 07:30'!inOn: port 	"Open a generic MidiDevice on the given Port"	| me |	me := self new.	me port: port.	me index: SirenUtility defaultMIDIIn.	me name: 'A MIDI Device'.	me direction: #in.		^ me! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 07:29'!inOn: ind port: po 	"Open a generic MidiDevice on the given Port"	| me |	me := self new.	me port: po.	me index: ind.	me name: 'A MIDI Device'.	me direction: #in.	^me! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/2/2023 12:15'!new	"Open a generic MidiDevice on the default Port"	"MIDIDevice new"	"| me |	me := super new initialize.	me port: MIDIPort default open.	^ m	"	^ super new initialize.! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/2/2023 12:09'!on: port 	"Open a generic MidiDevice on the given Port for output"	^ self outOn: port! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 07:29'!outOn: port 	"Open a generic MidiDevice on the given Port"	| me |	me := self new.	me port: port.	me index: SirenUtility defaultMIDIOut.	me name: 'A MIDI Device'.	me direction: #out.	^ me! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 15:11'!outOn: ind port: po 	"Open a generic MidiDevice on the given device index on the given	Port. "	"MIDIVoice on: 	(MIDIDevice 	outOn: (PortMIDIPort indexOfOutNamed: '*Kontakt*') 	port: MIDIPort concreteClass default) 	channel: 1"	| me |	me := self new.	me index: ind.	me port: po.	(po class streams includesKey: ind)		ifFalse: [po openOutput: ind].	me name: 'A MIDI Device'.	me direction: #out.	^ me! !!MIDIDevice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 07:36'!outOnDeviceNamed: devNam 	"Open a generic MidiDevice on the given Port"	"MIDIDevice outOnDeviceNamed: 'Kontakt'"	| ind me |	ind := me := self new.	me port: MIDIPort concreteClass default.	me		index: (PortMIDIPort indexOfOutNamed: '*' , devNam , '*').	me name: 'A MIDI Device'.	me direction: #out.	^ me! !!MIDIDevice class methodsFor: 'class initialization' stamp: 'stp 1/3/2023 13:34'!initialize	"Set up the one class variable"	"MIDIDevice initialize"	"MStream := Transcript.
	MStream := WriteStream on: (String	new: 4096).
"	! !Model subclass: #MIDIDump	instanceVariableNames: 'eventList live notesOn startedAt'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIDump commentStamp: '<historical>' prior: 0!An instance of MIDIDump is used as a dependent of the MIDIPort to demonstrate the use of dependency for MIDI input. See the class example.

Instance Variables:
	verbose <Boolean> should I log events to the Transcript?
	live <Boolean> should I match noteOn/Off commands on the fly?
	eventList <EventList> the event list I'm capturing input to
	notesOn <OrderedCollection of MusciEvents> the current playing notes
	startedAt <Integer> the clock time I was started at
!!MIDIDump methodsFor: 'updating'!update: aspect with: aParameter from: aSender
	"The model (port) changed; add the event to our event list."
	"MIDIDump exampleList"

	| packet evt cmd found time |
	packet := aSender input removeFirst.
	SirenUtility log: packet printString.
	eventList ifNil: [^self].
	cmd := packet command.
	live
		ifTrue: [(cmd = (MIDICommands at: #noteOn) and: [packet ampl ~= 0])
			ifTrue: [^notesOn add: (MusicEvent dur: packet time pitch: packet pitch 
								ampl: packet ampl voice: packet voice)].
			(cmd = (MIDICommands at: #noteOff) or: [packet ampl = 0])
			ifTrue: [found := notesOn removeAllSuchThat: 
					[ :ev | ((ev pitch value = packet pitch) and: [ev voice =  packet voice])].
				found isEmpty
					ifTrue: [^Transcript show: 'No note-on found for ', packet printString; cr].
				evt := found first.
				time := evt dur value.
				evt dur: (packet time - time).			"fix the event's duration"
				^eventList add: evt at: time - startedAt]]
		ifFalse: [^eventList add: ((MusicEvent pitch: packet pitch 
							ampl: packet ampl voice: packet voice) 
						command: cmd) at: (packet time - startedAt)].
	SirenUtility log: 'Unrecognized packet: ', packet printString! !!MIDIDump methodsFor: 'initialize release'!initialize
	"Setup the receiver's instance variables."

	live := false.
	notesOn := OrderedCollection new.
	startedAt := Time microsecondClock! !!MIDIDump methodsFor: 'accessing'!eventList

	^eventList! !!MIDIDump methodsFor: 'accessing'!eventList: aValue

	eventList := aValue! !!MIDIDump methodsFor: 'accessing'!live: aValue

	live := aValue! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIDump class	instanceVariableNames: ''!!MIDIDump class methodsFor: 'instance creation'!new
	^super new initialize! !!MIDIDump class methodsFor: 'examples'!example
	"Set up a MIDI dump object as a dependent of the input port. Dump for 10 seconds,
	then turn off. The default update: method just dumps the MIDI packet into the transcript; 
	customize this by writing your own update: method."
	"MIDIDump example"

	| inst port |
"Get a MIDIDump and the input port."
	inst := MIDIDump new.
	port := MIDIPort new openInput.
"Add the dump as a dependent of the port."
	port addDependent: inst.
"Turn on the port for input."
	port stopMIDIEcho.
	port stopControllerCaching.
	port startMIDIInput.
"Wait"
	Cursor wait showWhile: [(Delay forSeconds: 5) wait].
"Turn the port off"
	port stopMIDIInput.
	port close.
"Don't forget to remove the dependent."
	port removeDependent: inst.! !!MIDIDump class methodsFor: 'examples'!exampleEditor
	"Set up a MIDI dump object as a dependent of the input port. Capture it to an event list and update the view in real time BROKEN."
	"MIDIDump exampleEditor"

	| inst port list |
"Get a MIDIDump and the input port."
	inst := MIDIDump new.
	inst live: true.
	list := EventList newAnonymous.
	list edit.
	inst eventList: list.
	port := MIDIPort new openInput.
"Add the dump as a dependent of the port."
	port addDependent: inst.
"Turn on the port for input."
	port stopMIDIEcho.
	port stopControllerCaching.
	port startMIDIInput.
"Wait"
	Cursor wait showWhile: [(Delay forSeconds: 5) wait].
"Turn the port off"
	port stopMIDIInput.
	port close.
"Don't forget to remove the dependent."
	port removeDependent: inst.! !!MIDIDump class methodsFor: 'examples'!exampleList
	"Set up a MIDI dump object as a dependent of the input port. Capture it to an event list."
	"MIDIDump exampleList"
	"PortMidiInterface unloadLibraries"

	| inst port list |
"Get a MIDIDump and the input port."
	inst := MIDIDump new.
	list := EventList newAnonymous.
	list code: #noteOnOff.
	inst eventList: list.
	port := MIDIPort new openInput.
"Add the dump as a dependent of the port."
	port addDependent: inst.
"Turn on the port for input."
	port stopMIDIEcho.
	port stopControllerCaching.
	port startMIDIInput.
"Wait"
	Cursor wait showWhile: [(Delay forSeconds: 5) wait].
"Turn the port off"
	port stopMIDIInput.
	port close.
"Don't forget to remove the dependent."
	port removeDependent: inst.
	list setDurations.
	list "inspect" edit! !MIDIDevice subclass: #MIDIFB01	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIFB01 commentStamp: '<historical>' prior: 0!MidiFB01 is a type of MidiDevice for the system exclusive commands of the Yamaha FB-01 synthesiser.
It implements the special commands available here (like note commands with fractional pitch and given duration).

The standard FB01 note command is like:
		2n k f v d1 d2 = cmd:channel, noteNum, frac, vel, durLow, durHigh
!!MIDIFB01 methodsFor: 'note events' stamp: 'stp 1/3/2023 13:34'!play: anEvent on: aChannel 	"send FB01-specific code for note with fraction and duration
		2n k	f v d1 d2 = cmd:channel, noteNum, frac, vel, durLow, durHigh"	| bytes realDur |	bytes := ByteArray new: 6.	bytes at: 1 put: 32 + aChannel.	bytes		at: 2		put: (self mapPitch: anEvent pitch).	bytes at: 3 put: 0.	bytes		at: 4		put: (self mapAmplitude: anEvent ampl).	realDur := self mapDuration: anEvent duration.	bytes		at: 5		put: (realDur bitAnd: 127).	bytes		at: 6		put: (realDur bitShift: -7).	port == nil		ifFalse: [port put: bytes length: 6]! !MIDIDevice subclass: #MIDIPF70	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIPF70 commentStamp: '<historical>' prior: 0!MidiPF70 is a type of MidiDevice for the system exclusive commands of the Yamaha PF-70 electric piano.
It implements the special commands available here (like note commands with fractional pitch and given duration).

The standard PF70 note command is like:
		2n k f v d1 d2 = cmd:channel, noteNum, frac, vel, durLow, durHigh
!Object subclass: #MIDIPacket	instanceVariableNames: 'length time flags duration data'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIPacket commentStamp: '<historical>' prior: 0!A MIDIPacket represents a simple MIDI datagram with a time-stamp and a data array.

Instance Variables
	data <ByteArray> The MIDI data bytes (may include a MIDI command and running status messages)
	length <Integer> The number of meaningful bytes in the data
	time <Integer> The msec timestamp when the packet was created or received.
	flags <Integer> Any 'flags' received from the MIDI driver.
	duration <Integer> my event's length

Pool dictionaries:
	MIDICommands <Dictionary of (command symbol -> octet)> e.g., (#noteOn -> 16r90)
		(It is initialized by class MIDIPort.)

See the class comment in MIDIPort for details.
!!MIDIPacket methodsFor: 'accessing'!ampl
	"Answer the receiver's MIDI velocity."

	^data at: 3! !!MIDIPacket methodsFor: 'accessing'!ampl: a
	"Set the receiver's MIDI velocity."

	data at: 3 put: a! !!MIDIPacket methodsFor: 'accessing'!channel
	"Answer the receiver's MIDI channel."

	^(data at: 1) bitAnd: 16r0F! !!MIDIPacket methodsFor: 'accessing'!command
	"Answer the receiver's MIDI command nibble."

	^(data at: 1) bitAnd: 16rF0! !!MIDIPacket methodsFor: 'accessing'!data
	"Answer the receiver's 'data' byte array."

	^data! !!MIDIPacket methodsFor: 'accessing'!data: anObject
	"Set the receiver's instance variable 'data' to be anObject."

	data := anObject! !!MIDIPacket methodsFor: 'accessing'!duration
	"Answer the receiver's duration."

	^duration! !!MIDIPacket methodsFor: 'accessing'!duration: d
	"Set the receiver's duration."

	duration := d! !!MIDIPacket methodsFor: 'accessing'!key
	"Answer the receiver's MIDI key number."

	^data at: 2! !!MIDIPacket methodsFor: 'accessing'!key: k
	"Set the receiver's MIDI key number."

	^data at: 2 put: k! !!MIDIPacket methodsFor: 'accessing'!length
	"Answer the receiver's 'length'."

	^length! !!MIDIPacket methodsFor: 'accessing'!length: anObject
	"Set the receiver's instance variable 'length' to be anObject."

	length := anObject! !!MIDIPacket methodsFor: 'accessing'!pitch
	"Answer the receiver's MIDI key number."

	^data at: 2! !!MIDIPacket methodsFor: 'accessing'!pitch: aValue
	"Set the receiver's MIDI key number."

	data at: 2 put: aValue! !!MIDIPacket methodsFor: 'accessing'!second: aValue
	"Set the receiver's MIDI key number."

	data at: 2 put: aValue! !!MIDIPacket methodsFor: 'accessing'!setPitch: p dur: d loudness: vol
	"Answer an instance of the class with the given parameters."

	self pitch: p; velocity: vol; duration: d! !!MIDIPacket methodsFor: 'accessing'!status: s
	"Set the receiver's MIDI command."

	data at: 1 put: s! !!MIDIPacket methodsFor: 'accessing'!third: aValue
	"Set the receiver's MIDI key velocity."

	data at: 3 put: aValue! !!MIDIPacket methodsFor: 'accessing'!time
	"Answer the receiver's 'time'."

	^time! !!MIDIPacket methodsFor: 'accessing'!time: anObject
	"Set the receiver's instance variable 'time' to be anObject."

	time := anObject! !!MIDIPacket methodsFor: 'accessing'!vel
	"Answer the receiver's MIDI velocity."

	^data at: 3! !!MIDIPacket methodsFor: 'accessing'!vel: v
	"Set the receiver's MIDI velocity."

	^data at: 3 put: v! !!MIDIPacket methodsFor: 'accessing'!voice
	"Answer the receiver's MIDI channel."

	^(data at: 1) bitAnd: 16r0F! !!MIDIPacket methodsFor: 'accessing'!voice: v
	"Set the receiver's MIDI channel."

	(data at: 1) put: (((data at: 1) bitAnd: 16rF0) bitOr: v)! !!MIDIPacket methodsFor: 'printing'!print: x on: aStream
	"Store a two-digit hexadecimal version of the 8-bit argument x on the stream."

	| digs |
	digs := '01234567890ABCDEF'.
	aStream nextPut: (digs at: ((x bitAnd: 16r00F0) bitShift: -4) + 1).
	aStream nextPut: (digs at: (x bitAnd: 16r000F) + 1)! !!MIDIPacket methodsFor: 'printing'!printOn: aStream
	"Store a readable version of the receiver on the argument."

	length ifNil: [length := data size].
	aStream nextPutAll: (MIDICommands keyAtValue: (data first bitAnd: 16rF0) ifAbsent: ['unknown status']).
	aStream nextPutAll: "(time isNil
			ifFalse: [' t: ', time printString]
			ifTrue: ['']), "
		((flags isNil and: [flags ~= 0])
			ifFalse: [' f: ', flags printString]
			ifTrue: ['']), 
		' d: ['.
	1 to: length do:
		[ :ind | 
		self print: (data at: ind) on: aStream.
		aStream space].
	aStream skip: -1; nextPut: $]! !!MIDIPacket methodsFor: 'initialize release'!initialize
	"Set the receiver's time stamp"

	time := Time microsecondClock! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIPacket class	instanceVariableNames: ''!!MIDIPacket class methodsFor: 'instance creation'!bytesFor14BitValue: val
	"Answer a two-element array with the 14-bit values that correspond to the given value."
	"val should be +- 8192"

	| int |
	val abs > 8191
		ifTrue: [self error: '14-bit value out of range.'].
	int := val + 8192.
	^Array with: (int bitAnd: 16r7F) with: ((int bitShift: -7) bitAnd: 16r7F)! !!MIDIPacket class methodsFor: 'instance creation'!fromBytes: aByteArray
	"Answer a new instance of a MIDI packet instantiated from the given byte array."

	^super new data: aByteArray! !!MIDIPacket class methodsFor: 'instance creation'!fromInt: anInt at: aTime
	"Answer a new instance of a MIDI packet instantiated from the given 3-byte long value."

	| bytes |
	bytes := ByteArray new: 3.
	bytes at: 3 put: ((anInt bitShift: -16) bitAnd: 16rFF).
	bytes at: 2 put: ((anInt bitShift: -8) bitAnd: 16rFF).
	bytes at: 1 put: (anInt bitAnd: 16rFF).
	^(self new data: bytes) time: aTime! !!MIDIPacket class methodsFor: 'instance creation'!new
	"Answer a new instance."

	^super new initialize! !!MIDIPacket class methodsFor: 'instance creation'!new: size
	"Answer a new instance with a data array of the given size."

	^self fromBytes: (ByteArray new: size)! !!MIDIPacket class methodsFor: 'instance creation'!setPitch: p dur: d loudness: vol
	"Answer an instance of the class with the given parameters."

	^self new pitch: p; velocity: vol; duration: d! !PortModel subclass: #MIDIPort	instanceVariableNames: 'inputData readProcess'	classVariableNames: 'GeneralMIDIFamilies GeneralMIDIMap MIDICommands MIDIDrums Streams'	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIPort commentStamp: '<historical>' prior: 0!An instance of a subclass of MIDIPort is used for the interface betweeen Siren and external MIDI drivers and devices. It implements both note-oriented (e.g., play: pitch at: aDelay dur: aDur amp: anAmp voice: voice), and data-oriented (e.g., put: data at: delay length: size) behaviors for MIDI I/O. There is an extensive test suite and demo in the class examples method and in the Siren outline view.

There is typically only one instance of MIDIPort. The messages new, default, and instance all answer the sole instance. MIDIPorts use dependency to signal input data, objects wishing to receive input should register themselves as dependents of a port. In the default Siren implementation, the scheduler is all in Smalltalk, and only the simplest MIDI driver is assumed.

Instance Variables:
	readProcess <Process> The loop process to read input data.
	inputData <OrderedCollection of (Integer -> ByteArray)> The available data.
	status <Symbol> #open or #closed
	device <Integer> my IO device's index

Class Variables:
	Instance <MIDIPort|nil> The sole instance, or nil.
	Debug <Boolean> Debug mode prints all I/O to the Transcript.
	UseSingleton <Boolean> whether to use a singleton instance (not necessary)
	DefaultInputDevice <Integer> the index in the driver of the default input device
	DefaultOutputDevice see above

MIDI Commands Supported:
	0x9x pp vv -- note-on (x=channel, pp=pitch, vv=velocity)
	0x8x pp vv -- note-off (x=channel, pp=pitch, vv=velocity)
	0xCx cc -- program-change (x=channel, pp=pitch)
	0xEx ll hh -- program-change (x=channel, ll=low 7 bits, hh=high 7 bits)
	0xBx cc dd -- control change (x=channel, cc=controller, dd=data)
!!MIDIPort methodsFor: 'accessing'!input
	"Answer the receiver's Q of input data."
	"MIDIPort default input"

	^inputData! !!MIDIPort methodsFor: 'accessing'!isActive
	"Answer whether the receiver is active."

	self subclassResponsibility! !!MIDIPort methodsFor: 'accessing'!reader
	"Answer the receiver's read process"

	^readProcess! !!MIDIPort methodsFor: 'accessing'!resetInput
	"Reset the receiver's Q of input data."
	"MIDIPort instance resetInput"

	inputData := OrderedCollection new: 16! !!MIDIPort methodsFor: 'control commands'!allNotesOff
	"Turn all MIDI notes off using the channel message 123."
	"MIDIPort default allNotesOff"

	self isActive ifFalse: [self open].
	0 to: 15 do:
		[ :chan |
		self controlChange: chan controller: 123 to: 0]! !!MIDIPort methodsFor: 'control commands'!allNotesOffVerbose
	"Turn all MIDI notes off (the verbose way)."
	"MIDIPort instance allNotesOff"

	| bytes answer |
	bytes := ByteArray new: 3.
	bytes at: 3 put: 0.
	0 to: 15 do:
		[ :chan |
		0 to: 127 do:
			[ :key |
			bytes at: 1 put: (16r80 bitOr: chan).
			bytes at: 2 put: key.
			answer := self put: bytes length: 3.
			answer = 3
				ifFalse: [self error: 'Error sending note-off.']]]! !!MIDIPort methodsFor: 'control commands'!controlChange: chan controller: controller to: value
	"Send out a control-change command now."
	"No range-checking on arguments."

	| data |
	data := ByteArray new: 3.
	data at: 1 put: ((MIDICommands at: #ctrlChange) bitOr: chan);
		at: 2 put: controller;
		at: 3 put: value.
	^self put: data length: 3! !!MIDIPort methodsFor: 'control commands'!pitchBend: chan to: value
	"Send out a pitch-bend command at the given time."

	| data pitchBytes |
	data := ByteArray new: 3.
"Get the two-byte 14-bit value for the argument."
	pitchBytes := MIDIPacket bytesFor14BitValue: value.
	data at: 1 put: ((MIDICommands at: #pitchWheel) bitOr: chan);
		at: 2 put: pitchBytes first;
		at: 3 put: (pitchBytes at: 2).
	self put: data length: 3! !!MIDIPort methodsFor: 'control commands'!programChange: chan to: value
	"Send out a program-change command at the given time."
	"No range-checking on arguments."

	| data prog answer |
	data := ByteArray new: 2.
	value isInteger
		ifTrue: [prog := value]
		ifFalse: [prog := GeneralMIDIMap at: value asSymbol
						ifAbsent: [self error: 'Instrument not found']].
	data at: 1 put: ((MIDICommands at: #progChange) bitOr: chan);
		at: 2 put: prog - 1.
	answer := self put: data length: 2.
	(Delay forMilliseconds: 50) wait.		"Give the synth some time..."
	^answer! !!MIDIPort methodsFor: 'control commands'!sysex: command
	"Send out a MIDI system exclusive data packet at the given time."

	| data size |
	size := command size + 2.
	data := ByteArray new: size.
	data at: 1 put: (MIDICommands at: #sysEx).
	1 to: command size do:
		[ :i | data at: (i + 1) put: (command at: i)].
	data at: size put: 16rF7.
	self put: data length: size! !!MIDIPort methodsFor: 'open/close'!close
	"Close MIDI."
	"MIDIPort instance close"

	self subclassResponsibility! !!MIDIPort methodsFor: 'open/close'!open
	"Open the MIDI driver -- start the lower-level driver up."
	"MIDIPort default"

	self openOutput: self class out! !!MIDIPort methodsFor: 'open/close' stamp: 'stp 12/31/2022 14:33'!openInput	"Open the MIDI driver -- start the lower-level driver up."	"MIDIPort default"	self openInput: SirenUtility defaultMIDIIn! !!MIDIPort methodsFor: 'open/close'!openInput: which
	"Open the MIDI driver -- start the lower-level driver up."
	"MIDIPort default"

	self subclassResponsibility! !!MIDIPort methodsFor: 'open/close' stamp: 'stp 12/31/2022 14:33'!openOutput	"Open the MIDI driver -- start the lower-level driver up."	"MIDIPort default"	self openOutput: SirenUtility defaultMIDIOut! !!MIDIPort methodsFor: 'open/close'!openOutput: which
	"Open the MIDI driver -- start the lower-level driver up."
	"MIDIPort default"

	self subclassResponsibility! !!MIDIPort methodsFor: 'open/close'!reset
	"Reset the port."

	self allNotesOff! !!MIDIPort methodsFor: 'ioctl'!eventsAvailable
	"Answer the number of events in the input Q."
	"MIDIPort instance eventsAvailable"

	^inputData size! !!MIDIPort methodsFor: 'ioctl'!hasBuffer
	"Answer whether the MIDI driver has a time-stamped output buffer."

	^false! !!MIDIPort methodsFor: 'ioctl'!hasClock
	"Answer whether the MIDI driver has its own clock."

	^false! !!MIDIPort methodsFor: 'ioctl'!hasControllerCache
	"Answer whether the MIDI driver supports a controller data buffer."

	^false! !!MIDIPort methodsFor: 'ioctl'!hasDurs
	"Answer whether the MIDI driver supports a 1-call note-on/off command."

	^false! !!MIDIPort methodsFor: 'ioctl'!readLoop
	"polling or waiting loop"

	self subclassResponsibility! !!MIDIPort methodsFor: 'ioctl'!startControllerCaching
	"Start caching MIDI controller in the driver."

	self subclassResponsibility! !!MIDIPort methodsFor: 'ioctl'!startMIDIEcho
	"Start echoing MIDI input from the driver."

	^self			"ignored"! !!MIDIPort methodsFor: 'ioctl'!startMIDIInput
	"Start the polling loop (or semaphore waiter) MIDI input."
	"Create and start the reader process."

	self resetInput.
	readProcess := [self readLoop] newProcess.
	readProcess priority: (Processor userBackgroundPriority - 1).
	readProcess resume.! !!MIDIPort methodsFor: 'ioctl'!stopControllerCaching
	"Stop caching MIDI controller in the driver."

	self subclassResponsibility! !!MIDIPort methodsFor: 'ioctl'!stopMIDIEcho
	"Stop echoing MIDI input from the driver."

	^self			"ignored"! !!MIDIPort methodsFor: 'ioctl'!stopMIDIInput
	"Stop signalling the read semaphore on MIDI input."

	readProcess ifNotNil: 
			[readProcess terminate.
			readProcess release].
	readProcess := nil.
	self breakDependents.! !!MIDIPort methodsFor: 'initialize release' stamp: 'stp 1/5/2023 07:31'!initialize	"Setup the receiver's instance variables."	status := #Init.	"self openOutput: SirenUtility defaultMIDIOut.	device := SirenUtility defaultMIDIOut	"! !!MIDIPort methodsFor: 'initialize release'!release
	"Release--break and dependencies on the error value."

	super release.! !!MIDIPort methodsFor: 'read/write'!get: packet
	"Read the data from the receiver into the argument (a MIDIPacket). Answer the number of data bytes read."

	self subclassResponsibility! !!MIDIPort methodsFor: 'read/write'!nextEventInto: anEventAssociation
	"Record via the receiver into the argument."
	"Answer a MusicEvent; if there's no note-off yet, put nil in the duration."

	| pa1 |
	pa1 := inputData first.
	pa1 length = 3
		ifFalse: [self error: ('Unknown MIDI packet (len = ', pa1 length printString, ')')].
	pa1 command = 16r90		"if note-on command..."
		ifTrue: [inputData do:	"look for note off."
			[ :pa2 | 
			pa2 pitch = pa1 pitch
				ifTrue: [(pa2 command = 16r80 or: [pa2 ampl = 0])
					ifTrue: [inputData removeFirst.
						inputData remove: pa2.
						^MusicEvent dur: (pa2 time - pa1 time) 
							pitch: pa1 pitch ampl: pa1 ampl voice: pa1 voice]]]]
		ifFalse: [self error: ('Unknown MIDI packet (cmd = ', 
						pa1 command printString, ')')].
	^MusicEvent dur: nil pitch: pa1 pitch ampl: pa1 ampl voice: pa1 voice! !!MIDIPort methodsFor: 'read/write'!nextMessage
	"Answer the first (length -> bytes) association from the input data collection."
	"MIDIPort instance nextMessage"

	^inputData isEmpty
		ifTrue: [nil]
		ifFalse: [inputData removeFirst]! !!MIDIPort methodsFor: 'read/write'!play: aPitch dur: aDur amp: anAmp voice: aVoice
	"Play a note (on/off message pair) with the given parameters on the receiver."
	"This is the default method for drivers with no 1-call noteOn/Off command."
	"Dur is assumed to be in msec."

	self playOn: aPitch amp: anAmp voice: aVoice.
"Schedule note-off in a forked wait block."
	aDur isNil
		ifFalse: [[(Delay forMilliseconds: aDur) wait.
				self playOff: aPitch amp: anAmp voice: aVoice] fork]! !!MIDIPort methodsFor: 'read/write' stamp: 'stp 1/11/2023 16:11'!play: streamID pitch: aPitch dur: aDur amp: anAmp voice: aVoice 	"Play a note (on/off message pair) with the given parameters on the receiver."	"This is the default method for drivers with no 1-call noteOn/Off command."		#open == status		ifFalse: [Transcript show: 'Lazy MIDI port open'; cr.			self error: 'port not open'			"self openOutput: device"].			self playOn: streamID				"play note-on command"		pitch: aPitch		amp: anAmp		voice: aVoice.			aDur isNil							"Schedule note-off after a delay in a forked block."		ifFalse: 			[[(Delay forMilliseconds: aDur) wait.			self playOff: streamID				pitch: aPitch				amp: anAmp				voice: aVoice] fork]! !!MIDIPort methodsFor: 'read/write'!playOff: streamID pitch: aPitch amp: anAmp voice: aVoice
	"Play a note-on command with the given parameters on the receiver."

	self subclassResponsibility! !!MIDIPort methodsFor: 'read/write'!playOn: streamID pitch: aPitch amp: anAmp voice: aVoice
	"Play a note-on command with the given parameters on the receiver."

	self subclassResponsibility! !!MIDIPort methodsFor: 'read/write'!put: data length: size
	"Send the argument data to the receiver now."

	self subclassResponsibility! !!MIDIPort methodsFor: 'read/write'!readController: index
	"Read the given controller value."

	| array |
	array := WordArray new: 1.
	array gcCopyToHeap.
	self readControllersFrom: index to: index into: array.
	^array first! !!MIDIPort methodsFor: 'read/write'!readControllersFrom: lo to: hi into: array
	"Read a range of controllers"

	self subclassResponsibility! !!MIDIPort methodsFor: 'printing' stamp: 'stp 1/2/2023 11:26'!printOn: aStream 	"Append to the argument aStream a sequence of characters 	that	identifies the collection."	super printOn: aStream.	device		ifNotNil: [aStream nextPutAll: ' on device ' , device printString]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIPort class	instanceVariableNames: ''!!MIDIPort class methodsFor: 'message tests' stamp: 'stp 1/20/2023 07:39'!functionExample
	"Demonstrate control commands by playing a note and making a crescendo with the volume pedal."
	"MIDIPort functionExample"

	| event |											"Set the sound to organ."
	MIDIPort default openOutput; programChange: 0 to: 'Church Organ'.
														"Play a note."
	(5 sec, 60 key, 40 velocity, (#voice -> 0)) play.
														"Fade it."
	event := FunctionEvent new function: (LinearFunction from: #((0 40) (4 120))).
	event interval: 0.2 sec.							"5 updates per sec"
	event controller: 4.								"tell it what to do, i.e., its MIDI controller"
	event voice: 0.
	event play! !!MIDIPort class methodsFor: 'message tests'!testBend
	"Demonstrate pitch-bend by playing two notes and bending them."
	"MIDIPort testBend"

	| port |
	port := self new.
	port openOutput.
"Set the recorder instrument."
	port programChange: 0 to: 'Recorder'.
	port programChange: 1 to: 'Recorder'.
"Play two notes."
	port play: 76 dur: 5000 amp: 60 voice: 0.
	port play: 80 dur: 5000 amp: 60 voice: 1.
"Bend them--one up, one down."
	0 to: 250 do:
		[ :i |
		port pitchBend: 0 to: 4096 + (i * 16).
		port pitchBend: 1 to: 4096 - (i * 16).
		(Delay forMilliseconds: 5) wait].
	2 wait.
	port close.! !!MIDIPort class methodsFor: 'message tests'!testControlContinuous
	"Demonstrate control commands by playing a note and making a crescendo with the volume pedal."
	"MIDIPort testControlContinuous"

	| port |
	port := self new.
	port openOutput.
"Set the sound to organ."
	port programChange: 0 to: 'Church Organ'.
"Play a note."
	port play: 60 dur: 5000 amp: 30 voice: 0.
"Fade it."
	0 to: 120 do:
		[ :i |
		port controlChange: 0 controller: 4 to: i.
		(Delay forMilliseconds: 30) wait].
	port close.! !!MIDIPort class methodsFor: 'message tests'!testProgramChange
	"Demonstrate program change by setting up an organ instrument to play on."
	"MIDIPort testProgramChange"

	| port |
	port := self default.
	port open.
"Set the flute instrument."
	port programChange: 0 to: 'Church Organ'.
"Play a note."
	port play: 72 dur: 1000 amp: 70 voice: 0.
	port play: 76 dur: 900 amp: 70 voice: 0.! !!MIDIPort class methodsFor: 'message tests'!testSysex
	"Demonstrate system exclusive commands by loading the Santur scale
	and playing a scale."
	"Note that this is a Roland GS sysEx, but it should work on QuickTime Musical Instruments."
	"MIDIPort testSysex"

	| port command |
	port := self default.
	port open.
"Set the flute instrument."
	port programChange: 0 to: 'Sitar'.
"Down-load an alternate tuning scale."
	command := #(16r41 16r10 16r42 16r12 16r40 16r11 "address"
			16r40 16r40 16r72 16r40 16r40 16r0E 16r40 16r40 16r40 16r0E 16r40 16r40 "scale"
			16r0E 16r53 "checksum").
	port sysex: command.
"Play a scale."
	MIDIPort testAScale! !!MIDIPort class methodsFor: 'driver performance tests'!testRandomPlayHighLevel: num dur: dur
	"Play 'num' random pitches (molto legato) spaced 'dur' msec apart."
	"This version uses the 1-call note command and driver-side scheduler."
	"MIDIPort testRandomPlayHighLevel: 64 dur: 80"

	| inst rand time |
	rand := Random new.
	inst := MIDIPort default.
	time := Time microsecondClock + 100000.
	num timesRepeat:
		[inst play: (rand next * 36 + 24) truncated "random pitches"
				at: time 
				dur: (dur * 4) truncated 		"molto legato"
				amp: 96 voice: 0.
		time := time + dur].
	(Delay forMicroseconds: ((num * dur) + 1000000)) wait! !!MIDIPort class methodsFor: 'driver performance tests'!testRandomPlayLowLevel: num dur: dur
	"Play 'num' random pitches spaced 'dur' msec apart."
	"This test creates the messages and does the scheduling right here."
	"MIDIPort testRandomPlayLowLevel: 64 dur: 80"

	| inst data rand |
	rand := Random new.
	inst := MIDIPort new.
	inst open.
	data := ByteArray new: 3.
	data at: 3 put: 96.				"velocity"
"	data at: 2 put: 60."
	num timesRepeat:
		[data at: 1 put: 16r90.		"note-on"
		data at: 2 put: (rand next * 36 + 24) truncated.	"pitch"
		inst put: data length: 3.
		(Delay forMilliseconds: dur) wait.
		Processor yield.
		data at: 1 put: 16r80.			"note-off"
		inst put: data length: 3]! !!MIDIPort class methodsFor: 'driver performance tests'!testRollLowLevel: num dur: dur
	"Play a roll of 'num' notes spaced 'dur' msec apart."
	"This test creates the messages and does the scheduling right here."
	"MIDIPort testRollLowLevel: 64 dur: 80"

	| inst data |
	inst := MIDIPort new.
	inst open.
	data := ByteArray new: 3.
	data at: 3 put: 96.				"velocity"
	data at: 2 put: 60.				"pitch"
	num timesRepeat:
		[data at: 1 put: 16r90.		"note-on"
		inst put: data length: 3.
		(Delay forMilliseconds: dur) wait.
		data at: 1 put: 16r80.			"note-off"
		inst put: data length: 3]! !!MIDIPort class methodsFor: 'output tests' stamp: 'stp 1/19/2023 06:58'!testACloud
	"Open MIDI, play a dense cloud."
	"MIDIPort testACloud"
	"MIDIPort cleanUp"

	| lis |						"play merged non-overlapping clouds"	lis := EventList newAnonymous.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {50 to: 64. 78 to: 80}			ampl: {80 to: 90. 20 to: 50}			density: 8) eventList.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {22 to: 40. 48 to: 50}			ampl: {70 to: 80. 20 to: 60}			density: 9) eventList.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {72 to: 72. 88 to: 90}			ampl: {40 to: 50. 90 to: 90}			density: 10) eventList.	lis playOn: (MIDIVoice defaultOut: 1)! !!MIDIPort class methodsFor: 'output tests' stamp: 'stp 1/17/2023 18:08'!testANote	"Open MIDI, play a note - low level."	"MIDIPort release"	"MIDIPort testANote"	| inst |	inst := MIDIPort new.	inst openOutput: SirenUtility defaultMIDIOut.	inst		play: 60		dur: 500		amp: 100		voice: 1.	1 wait.	inst close! !!MIDIPort class methodsFor: 'output tests'!testARandomNote
	"Open MIDI, play a note."
	"MIDIPort testARandomNote"

	| inst err data rand |
	rand := Random new.
	inst := MIDIPort new.
	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	data := ByteArray new: 3.
	data at: 1 put: 16r90.			"note-on"
	data at: 2 put: (rand next * 36 + 24) truncated.		"pitch"
	data at: 3 put: 96.		"velocity"
	err := inst put: data length: 3.
	Transcript show: 'note-on --> ', err printString; cr.
	(Delay forMilliseconds: 1000) wait.
	data at: 1 put: 16r80.		"silence = note-off"
	err := inst put: data length: 3.
	Transcript show: 'note-off --> ', err printString; cr.! !!MIDIPort class methodsFor: 'output tests' stamp: 'stp 1/19/2023 06:58'!testAScale
	"Play a fast scale using the low-level MIDI port."
	"Scale example answers (start dur pitch amp)."
	"MIDIPort testAScale"
	"MIDIPort cleanUp"

	(EventList			scaleFrom: 48			to: 72			in: 1.5)			playOn: (MIDIVoice defaultOut: 1)! !!MIDIPort class methodsFor: 'output tests' stamp: 'stp 1/19/2023 06:58'!testAScaleLowLevel
	"Play a fast scale using the low-level MIDI port."
	"Scale example answers (start dur pitch amp)."
	"MIDIPort testAScale"
	"MIDIPort cleanUp"

	| port |
	port := (MIDIVoice defaultOut: 1) instrument port.
	(MIDIPort scaleFrom: 60 to: 84 in: 2000) do:
		[ :not |
		port play: (not at: 3) dur: (not at: 2) amp: (not at: 4) voice: 1.
		(Delay forMilliseconds: (not at: 2)) wait]	"	^ (EventList				scaleFrom: 48				to: 72				in: 1.5)				playOn: (MIDIVoice defaultOut: 0)"! !!MIDIPort class methodsFor: 'output tests'!testAllNotesOff
	"Try to open and close the MIDI port."
	"MIDIPort testAllNotesOff"

	| inst err data |
	inst := MIDIPort new.
	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	data := ByteArray new: 3.
	data at: 1 put: 16r90.		"note-on"
	data at: 2 put: 60.			"pitch"
	data at: 3 put: 127.		"velocity"
	err := inst put: data length: 3.
	err := inst allNotesOff.
	Transcript show: 'ANO --> ', err printString; cr.
	err := inst close.
	Transcript show: 'close --> ', err printString; cr.! !!MIDIPort class methodsFor: 'output tests'!testInspect
	"Inspect a MIDI port."
	"MIDIPort testInspect"

	| inst err |
	inst := MIDIPort new.
	inst inspect.
"	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	err := inst test.
	Transcript show: 'test --> ', err printString; cr.
	err := inst close.
	Transcript show: 'close --> ', err printString; cr.
"! !!MIDIPort class methodsFor: 'output tests' stamp: 'stp 12/31/2022 10:17'!testMouseMIDI	"Open MIDI, play notes based on the mouse position."	"x --> voice; y --> pitch."	"MIDIPort testMouseMIDI"	| inst moPt ht |	inst := MIDIPort default.	inst open.	MIDIPort setupTunedPercussion.	(Delay forMilliseconds: 200) wait.	ht := DisplayScreen actualScreenSize y.	[Sensor anyButtonPressed]		whileFalse: [moPt := Sensor cursorPoint.			inst				play: ht - moPt y // 20 + 24				dur: 250				amp: 96				voice: moPt x // 50.			(Delay forMilliseconds: 100) wait]! !!MIDIPort class methodsFor: 'output tests'!testNoteOnOff
	"Open MIDI, play a note, and close it."
	"MIDIPort testNoteOnOff"

	| inst err data |
	inst := MIDIPort new.
	data := ByteArray new: 3.
	data at: 1 put: 16r90.
	data at: 2 put: 64.
	data at: 3 put: 96.
	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	err := inst put: data length: 3.
	Transcript show: 'play --> ', err printString; cr.
	data at: 3 put: 0.
	err := inst put: data length: 3.
	Transcript show: 'play --> ', err printString; cr.
	err := inst close.
	Transcript show: 'close --> ', err printString; cr.! !!MIDIPort class methodsFor: 'output tests'!testOpenClose
	"Try to open and close the MIDI port."
	"MIDIPort testOpenClose"
	"MIDIPort default close"
	"MIDIPort cleanUp"

	| inst err |
	inst := MIDIPort new.
	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	err := inst close.
	instance := nil.
	Transcript show: 'close --> ', err printString; cr.! !!MIDIPort class methodsFor: 'output tests'!testOutput
	"Open MIDI, play some random notes, and close it."
	"MIDIPort testOutput"

	| inst err |
	inst := MIDIPort new.
	err := inst open.
	Transcript cr; show: 'open --> ', err printString; cr.
	err := inst test.
	Transcript show: 'test --> ', err printString; cr.
	err := inst close.
	Transcript show: 'close --> ', err printString; cr.! !!MIDIPort class methodsFor: 'input tests'!dumpExample
	"Set up a MIDI dump object as a dependent of the input port. Dump for 10 seconds,
	then turn off. The default update: method just dumps the MIDI packet into the transcript; 
	customize this by writing your own update: method."
	"MIDIPort dumpExample"
	"MIDIPort testInputStop"

	| inst port |
"Get a MIDIDump and the input port."
	inst := MIDIDump new.
	port := MIDIPort new openInput.
"Add the dump as a dependent of the port."
	port addDependent: inst.
"Turn on the port for input."
	port startMIDIInput.
"Wait"
	Cursor wait showWhile: [(Delay forSeconds: 5) wait].
"Turn the port off"
	port stopMIDIInput.
	port close.
"Don't forget to remove the dependent."
	port removeDependent: inst.! !!MIDIPort class methodsFor: 'input tests'!testInput
	"Open MIDI, try to read something--dump it to the transcript."
	"MIDIPort testInput"
	"MIDIPort testInputStop"
	"MIDIPort concreteClass someInstance eventsAvailable"

	| port |
	port := self new.
	SirenUtility verbosity: 2.
	port openInput.
	port stopMIDIEcho.
	port stopControllerCaching.
	port startMIDIInput.

"Execute this to end the test"
"	| port |
	port := MIDIPort concreteClass someInstance.
	port stopMIDIEcho.
	port stopMIDIInput
"! !!MIDIPort class methodsFor: 'input tests' stamp: 'stp 1/17/2023 18:21'!testInput: ignored
	"Open MIDI, try to read something--dump it to the transcript."
	"MIDIPort testInput"
	"MIDIPort testInputStop"
	"MIDIPort concreteClass someInstance eventsAvailable"

	| port |
	port := self new.
	SirenUtility verbosity: 2.
	port openInput.
	port stopMIDIEcho.
	port stopControllerCaching.
	port startMIDIInput.

"Execute this to end the test"
"	| port |
	port := MIDIPort concreteClass someInstance.
	port stopMIDIEcho.
	port stopMIDIInput
"! !!MIDIPort class methodsFor: 'input tests'!testInputStop
	"Execute this to end the input test"
	"MIDIPort testInputStop"

	| port |
	port := self concreteClass someInstance.
	SirenUtility verbosity: 1.
	port stopMIDIEcho.
	port stopMIDIInput.
	port close.! !!MIDIPort class methodsFor: 'input tests' stamp: 'stp 1/17/2023 18:21'!testInputStop: ignored
	"Execute this to end the input test"
	"MIDIPort testInputStop"

	| port |
	port := self concreteClass someInstance.
	SirenUtility verbosity: 1.
	port stopMIDIEcho.
	port stopMIDIInput.
	port close.! !!MIDIPort class methodsFor: 'controller tests' stamp: 'stp 12/31/2022 11:35'!testControllerCaching	"Set up uncached controller reading--make a loop that reads and	prints controller 48 
		twice a second (until you press the shift button)."	"MIDIPort testControllerCaching"	"MIDIPort testInputStop"	| inst |	inst := self new.	inst openInput: self concreteClass in.	"Edit your controller device # here"	inst startControllerCaching.	inst startMIDIInput.	[Sensor shiftPressed]		whileFalse: [Transcript show: 'Ctrl 48: ' , (inst readController: 48) printString;				 cr.			(Delay forMilliseconds: 500) wait].	MIDIPort testInputStop! !!MIDIPort class methodsFor: 'controller tests' stamp: 'stp 12/31/2022 11:35'!testControllerCaching2	"Set up uncached controller reading--read controllers 48-52 as an	array and print it; stop on mouse press."	"MIDIPort testControllerCaching2"	"MIDIPort testInputStop"	| array inst |	inst := self new.	inst openInput: self concreteClass in.	"Edit your controller device # here"	inst startControllerCaching.	inst startMIDIInput.	array := Array new: 5.	[Sensor shiftPressed]		whileFalse: [inst				readControllersFrom: 48				to: 52				into: array.			Transcript tab; show: array printString; cr.			(Delay forMilliseconds: 500) wait].	MIDIPort testInputStop! !!MIDIPort class methodsFor: 'controller tests' stamp: 'stp 12/31/2022 11:35'!testControllerCachingFrom: lo to: hi 	"Set up uncached controller reading--read controllers from lo to hi	(inclusive) as an array and print it; 
		stop on <SHIFT> press."	"MIDIPort testControllerCachingFrom: 48 to: 55"	"MIDIPort testInputStop"	| inst array count |	inst := self new.	inst openInput: 3.	"Edit your controller device # here"	inst startControllerCaching.	inst startMIDIInput.	array := WordArray new: hi - lo + 1.	array gcCopyToHeap.	"this is so that you can pass this ptr to C"	count := 0.	[Sensor shiftPressed]		whileFalse: [inst				readControllersFrom: lo				to: hi				into: array.			Transcript tab; show: count printString , '	' , array printString; cr.			(Delay forMilliseconds: 500) wait.			count := count + 1].	Transcript show: 'Done';		 cr.	MIDIPort testInputStop! !!MIDIPort class methodsFor: 'controller tests'!testControllerRecording
	"Set up uncached controller reading--make a loop that reads and prints controller 48 
		40 times a second for 5 seconds."
	"MIDIPort testControllerRecording"
	"MIDIPort testInputStop"

	| inst fcn |
	inst := self new.
	inst openInput: 3.			"Edit your controller device # here"
	inst startControllerCaching.
	fcn := Function ofSize: 200.
	inst startMIDIInput.
	200 timesRepeat:
		[fcn add: (inst readController: 48) asFloat / 127.0.
		(Delay forMilliseconds: 25) wait].
	MIDIPort testInputStop.
	fcn edit! !!MIDIPort class methodsFor: 'utilities'!allNotesOff
	"MIDIPort allNotesOff"

	| port |
	port := self concreteClass someInstance.
	port ifNil: [port := self new].
	port allNotesOff! !!MIDIPort class methodsFor: 'utilities'!cleanUp
	"Close down and clean up all MIDI, sound IO, event lists, etc."
	"MIDIPort cleanUp"
	"self concreteClass instanceCount"
	"self concreteClass allInstances"

	instance := nil.
	self concreteClass instanceCount = 0
		ifFalse: [self concreteClass allInstances do: 
			[ :p | 
			p isActive ifTrue: [p stopMIDIInput]. 
			p isActive ifTrue: [p close]. 
			p release]]. 
	EventScheduler instance flush.
	MIDIVoice flushDefault! !!MIDIPort class methodsFor: 'utilities'!showInput
	"Open MIDI, wait to read something, then dump it to the transcript."
	"This toggles on and off if called repeatedly."
	"MIDIPort showInput"
	"instance input"

	self default open.
	(instance reader == nil)
		ifTrue: [Transcript cr; show: 'Stopping MIDI Dump'; cr; cr.
			instance stopMIDIEcho.
			instance stopMIDIInput]
		ifFalse: [Transcript cr; cr; show: 'Starting MIDI Dump'; cr.
			instance startMIDIEcho.
			instance stopControllerCaching.
			instance startMIDIInput]! !!MIDIPort class methodsFor: 'general MIDI patches'!setAllInstrumentsTo: iname
	"Set instruments 0-15 to the General MIDI name iname"
	"MIDIPort setAllInstrumentsTo: 'Tenor Sax'. MIDIPort testAScale"
	"MIDIPort setAllInstrumentsTo: 'Music Box'. MIDIPort testAScale"

	| port |
	port := self default.
	0 to: 15 do:
		[ :ind |
		port programChange: ind to: (GeneralMIDIMap at: iname asSymbol)]! !!MIDIPort class methodsFor: 'general MIDI patches'!setEnsemble: orch
	"Down-load a general MIDI patch for the given ensemble (a collection of 
	[chan -> name] associations)."
	"MIDIPort setEnsemble: 
			(Array with: (0 -> 'Reed Organ') with: (1 -> 'Rock Organ')
					with: (2 -> 'Church Organ') with: (3 -> 'Drawbar Organ')
					with: (4 -> 'Percussive Organ') with: (5 -> 'Accordion'))"

	| port |
	port := self default.
	port open.
	orch do:
		[ :item |
		port programChange: item key to: (GeneralMIDIMap at: item value asSymbol)]! !!MIDIPort class methodsFor: 'general MIDI patches'!setEnsembleInOrder: orch
	"Down-load a general MIDI patch for the given ensemble (a collection of symbolic keys 
	into the General MIDI voice map) mapping the first element to MIDI channel 1, etc."

	| port |
	port := self default.
	port open.
	1 to: orch size do:
		[ :ind |
		port programChange: (ind - 1) to: (GeneralMIDIMap at: (orch at: ind) asSymbol)]! !!MIDIPort class methodsFor: 'general MIDI patches'!setupDefaultGeneralMIDI
	"Down-load a general MIDI patch for a 16-voice percussion ensemble."
	"MIDIPort setupDefaultGeneralMIDI. MIDIPort testAScale"

	MIDIPort setEnsembleInOrder: 
			#('Acoustic Grand Piano' 'Bright Acoustic Piano' 'Electric Grand Piano' 'Honky-tonk Piano'
				'Electric Piano 1' 'Electric Piano 2' 'Harpsichord' 'Clavinet' 'Celesta' 'Glockenspiel' 
				'Music Box' 'Vibraphone' 'Marimba' 'Xylophone' 'Tubular Bells' 'Dulcimer').! !!MIDIPort class methodsFor: 'general MIDI patches'!setupOrgan
	"Down-load a general MIDI patch for a 4-voice organ."
	"MIDIPort setupOrgan. MIDIPort testAScale"

	MIDIPort setEnsemble: 
			(OrderedCollection with: (0 -> 'Reed Organ') with: (1 -> 'Rock Organ')
					with: (2 -> 'Church Organ') with: (3 -> 'Drawbar Organ')
					with: (4 -> 'Percussive Organ') with: (5 -> 'Accordion'))! !!MIDIPort class methodsFor: 'general MIDI patches'!setupTunedPercussion
	"Down-load a general MIDI patch for a 16-voice percussion ensemble."
	"MIDIPort setupTunedPercussion. MIDIPort testAScale"

	MIDIPort setEnsembleInOrder: 
			#(Agogo 'Tinkle Bell' Timpani Xylophone 
				Applause 'Taiko Drum' Glockenspiel 'Synth Drum'
				Gunshot 'Steel Drums' Helicopter Vibraphone
				Woodblock 'Telephone Ring' Kalimba 'Blown Bottle')! !!MIDIPort class methodsFor: 'general MIDI patches'!setupWindOrchestra
	"Down-load a general MIDI patch for a 16-voice wind ensemble."
	"This could be done more tersely with the setEnsembleInOrder: method."
	"MIDIPort setupWindOrchestra. MIDIPort default test"
	"GeneralMIDIMap keys asSortedCollection"

	| port orch |
	port := self default.
	port open.
	orch := #(Flute 'English Horn' Bassoon Clarinet Piccolo 'Pan Flute' Recorder 'Tenor Sax'
			  Flute 'English Horn' Bassoon Clarinet Piccolo 'Pan Flute' Recorder 'Tenor Sax').
	1 to: orch size do:
		[ :ind |
		port programChange: (ind - 1) to: (GeneralMIDIMap at: (orch at: ind) asString)]! !!MIDIPort class methodsFor: 'examples'!examples
	"Select and execute the following for usage examples."

"Try to open and close the MIDI port (report to transcript)."
	"MIDIPort testOpenClose"
"Open MIDI, play a 1-sec. note."
	"MIDIPort testANote"
"Open MIDI, play a fast scale."
	"MIDIPort testAScale"
"Open MIDI, play notes based on the mouse position (x --> voice; y --> pitch) until mouse down."
	"MIDIPort testMouseMIDI"

"Open MIDI, try to read something--dump it to the transcript."
	"MIDIPort testInput"
"Execute this to end the input test"
	"MIDIPort testInputStop"
"Get the port's pending input."
	"MIDIPort default eventsAvailable"
	"MIDIPort default readAll"
	"MIDIPort default input"
	"MIDIPort default resetInput"
"Set up a MIDI dump object as a dependent of the input port. Dump for 30 seconds,
then turn off. The default update: method just dumps the MIDI packet into the transcript; 
customize this by writing your own update: method."
	"MIDIPort dumpExample"
"Set up uncached controller reading and dump input to the transcript."
	"MIDIPort testControllerInput"
	"MIDIPort testInputStop"
"Set up uncached controller reading--read controllers from lo to hi as an array and print it; 
stop on mouse press."
	"MIDIPort testControllerCachingFrom: 48 to: 52"

"Play 'num' random pitches spaced 'dur' msec apart."
"This test creates the messages and does the scheduling right here."
	"MIDIPort testRandomPlayLowLevel: 64 dur: 80"
"Play a roll of 'num' notes spaced 'dur' msec apart."
"This version uses the 1-call note command and driver-side scheduler."
	"MIDIPort testRollHighLevel: 64 dur: 50"
"Play a roll of 'num' notes spaced 'dur' msec apart."
"This test creates the messages and does the scheduling right here."
	"MIDIPort testRollLowLevel: 64 dur: 50"

"Demonstrate control commands by playing a note and making a crescendo with the volume pedal."
	"MIDIPort testControlContinuous"
"Demonstrate pitch-bend by playing two notes and bending them."
	"MIDIPort testBend"

"Demonstrate program change by setting up an organ instrument to play on."
	"MIDIPort testProgramChange"
"Down-load a general MIDI patch for a 4-voice organ."
	"MIDIPort setupOrgan. MIDIPort default test"
"Down-load a general MIDI patch for a 16-voice percussion ensemble."
	"MIDIPort setupTunedPercussion. MIDIPort testAScale"

"ANO"
	"MIDIPort allNotesOff"
"Close down and clean up."
	"MIDIPort cleanUp"! !!MIDIPort class methodsFor: 'examples'!scaleFrom: lo to: hi in: dur
	"Answer array of (start dur pitch amp)"
	"MIDIPort scaleFrom: 48 to: 72 in: 2000"

	| arr size ndur |
	size := hi - lo + 1.
	ndur := (dur / size) truncated.
	arr := Array new: size.
	1 to: size do:
		[ :ind |
		arr at: ind put: (Array with: (ind - 1 * ndur) with: ndur with: (ind + lo - 1) with: 96)].
	^arr! !!MIDIPort class methodsFor: 'instance creation'!concreteClass
	"Answer the class to use for MIDI"
	"MIDIPort concreteClass instanceCount"


	^PortMIDIPort! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/18/2023 12:23'!initialize	"Set up the dictionaries of commands, ioctl primitive selectors and	arguments, and general MIDI maps."	"MIDIPort initialize"	self initializeData! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/18/2023 12:24'!initializeData	"Set up the dictionaries of commands, ioctl primitive selectors and	arguments, and general MIDI maps."	"PortModel initializeData"	Streams := IdentityDictionary new.	self initializeMIDITables.	useSingleton := false.	instance := nil.	self concreteClass initializeData! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/13/2023 22:44'!initializeGeneralMIDI1	GeneralMIDIMap := Dictionary new.	GeneralMIDIMap		add: 'Acoustic Grand Piano' asSymbol -> 1.	GeneralMIDIMap		add: 'Bright Acoustic Piano' asSymbol -> 2.	GeneralMIDIMap		add: 'Electric Grand Piano' asSymbol -> 3.	GeneralMIDIMap		add: 'Honky-tonk Piano' asSymbol -> 4.	GeneralMIDIMap		add: 'Electric Piano 1' asSymbol -> 5.	GeneralMIDIMap		add: 'Electric Piano 2' asSymbol -> 6.	GeneralMIDIMap		add: 'Harpsichord' asSymbol -> 7.	GeneralMIDIMap		add: 'Clavinet' asSymbol -> 8.	GeneralMIDIMap		add: 'Celesta' asSymbol -> 9.	GeneralMIDIMap		add: 'Glockenspiel' asSymbol -> 10.	GeneralMIDIMap		add: 'Music Box' asSymbol -> 11.	GeneralMIDIMap		add: 'Vibraphone' asSymbol -> 12.	GeneralMIDIMap		add: 'Marimba' asSymbol -> 13.	GeneralMIDIMap		add: 'Xylophone' asSymbol -> 14.	GeneralMIDIMap		add: 'Tubular Bells' asSymbol -> 15.	GeneralMIDIMap		add: 'Dulcimer' asSymbol -> 16.	GeneralMIDIMap		add: 'Drawbar Organ' asSymbol -> 17.	GeneralMIDIMap		add: 'Percussive Organ' asSymbol -> 18.	GeneralMIDIMap		add: 'Rock Organ' asSymbol -> 19.	GeneralMIDIMap		add: 'Church Organ' asSymbol -> 20.	GeneralMIDIMap		add: 'Reed Organ' asSymbol -> 21.	GeneralMIDIMap		add: 'Accordion' asSymbol -> 22.	GeneralMIDIMap		add: 'Harmonica' asSymbol -> 23.	GeneralMIDIMap		add: 'Tango Accordion' asSymbol -> 24.	GeneralMIDIMap		add: 'Acoustic Guitar (nylon)' asSymbol -> 25.	GeneralMIDIMap		add: 'Acoustic Guitar (steel)' asSymbol -> 26.	GeneralMIDIMap		add: 'Electric Guitar (jazz)' asSymbol -> 27.	GeneralMIDIMap		add: 'Electric Guitar (clean)' asSymbol -> 28.	GeneralMIDIMap		add: 'Electric Guitar (muted)' asSymbol -> 29.	GeneralMIDIMap		add: 'Overdriven Guitar' asSymbol -> 30.	GeneralMIDIMap		add: 'Distortion Guitar' asSymbol -> 31.	GeneralMIDIMap		add: 'Guitar harmonics' asSymbol -> 32.	GeneralMIDIMap		add: 'Acoustic Bass' asSymbol -> 33.	GeneralMIDIMap		add: 'Electric Bass (finger)' asSymbol -> 34.	GeneralMIDIMap		add: 'Electric Bass (pick)' asSymbol -> 35.	GeneralMIDIMap		add: 'Fretless Bass' asSymbol -> 36.	GeneralMIDIMap		add: 'Slap Bass 1' asSymbol -> 37.	GeneralMIDIMap		add: 'Slap Bass 2' asSymbol -> 38.	GeneralMIDIMap		add: 'Synth Bass 1' asSymbol -> 39.	GeneralMIDIMap		add: 'Synth Bass 2' asSymbol -> 40.	GeneralMIDIMap		add: 'Violin' asSymbol -> 41.	GeneralMIDIMap		add: 'Viola' asSymbol -> 42.	GeneralMIDIMap		add: 'Cello' asSymbol -> 43.	GeneralMIDIMap		add: 'Contrabass' asSymbol -> 44.	GeneralMIDIMap		add: 'Tremolo Strings' asSymbol -> 45.	GeneralMIDIMap		add: 'Pizzicato Strings' asSymbol -> 46.	GeneralMIDIMap		add: 'Orchestral Harp' asSymbol -> 47.	GeneralMIDIMap		add: 'Timpani' asSymbol -> 48.	GeneralMIDIMap		add: 'String Ensemble 1' asSymbol -> 49.	GeneralMIDIMap		add: 'String Ensemble 2' asSymbol -> 50.	GeneralMIDIMap		add: 'SynthStrings 1' asSymbol -> 51.	GeneralMIDIMap		add: 'SynthStrings 2' asSymbol -> 52.	GeneralMIDIMap		add: 'Choir Aahs' asSymbol -> 53.	GeneralMIDIMap		add: 'Voice Oohs' asSymbol -> 54.	GeneralMIDIMap		add: 'Synth Voice' asSymbol -> 55.	GeneralMIDIMap		add: 'Orchestra Hit' asSymbol -> 56.	GeneralMIDIMap		add: 'Trumpet' asSymbol -> 57.	GeneralMIDIMap		add: 'Trombone' asSymbol -> 58.	GeneralMIDIMap		add: 'Tuba' asSymbol -> 59.	GeneralMIDIMap		add: 'Muted Trumpet' asSymbol -> 60.	GeneralMIDIMap		add: 'French Horn' asSymbol -> 61.	GeneralMIDIMap		add: 'Brass Section' asSymbol -> 62.	GeneralMIDIMap		add: 'SynthBrass 1' asSymbol -> 63.	GeneralMIDIMap		add: 'SynthBrass 2' asSymbol -> 64! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/13/2023 22:44'!initializeGeneralMIDI2	GeneralMIDIMap		add: 'Soprano Sax' asSymbol -> 65.	GeneralMIDIMap		add: 'Alto Sax' asSymbol -> 66.	GeneralMIDIMap		add: 'Tenor Sax' asSymbol -> 67.	GeneralMIDIMap		add: 'Baritone Sax' asSymbol -> 68.	GeneralMIDIMap		add: 'Oboe' asSymbol -> 69.	GeneralMIDIMap		add: 'English Horn' asSymbol -> 70.	GeneralMIDIMap		add: 'Bassoon' asSymbol -> 71.	GeneralMIDIMap		add: 'Clarinet' asSymbol -> 72.	GeneralMIDIMap		add: 'Piccolo' asSymbol -> 73.	GeneralMIDIMap		add: 'Flute' asSymbol -> 74.	GeneralMIDIMap		add: 'Recorder' asSymbol -> 75.	GeneralMIDIMap		add: 'Pan Flute' asSymbol -> 76.	GeneralMIDIMap		add: 'Blown Bottle' asSymbol -> 77.	GeneralMIDIMap		add: 'Shakuhachi' asSymbol -> 78.	GeneralMIDIMap		add: 'Whistle' asSymbol -> 79.	GeneralMIDIMap		add: 'Ocarina' asSymbol -> 80.	GeneralMIDIMap		add: 'Lead 1 (square)' asSymbol -> 81.	GeneralMIDIMap		add: 'Lead 2 (sawtooth)' asSymbol -> 82.	GeneralMIDIMap		add: 'Lead 3 (calliope)' asSymbol -> 83.	GeneralMIDIMap		add: 'Lead 4 (chiff)' asSymbol -> 84.	GeneralMIDIMap		add: 'Lead 5 (charang)' asSymbol -> 85.	GeneralMIDIMap		add: 'Lead 6 (voice)' asSymbol -> 86.	GeneralMIDIMap		add: 'Lead 7 (fifths)' asSymbol -> 87.	GeneralMIDIMap		add: 'Lead 8 (bass + lead)' asSymbol -> 88.	GeneralMIDIMap		add: 'Pad 1 (new age)' asSymbol -> 89.	GeneralMIDIMap		add: 'Pad 2 (warm)' asSymbol -> 90.	GeneralMIDIMap		add: 'Pad 3 (polysynth)' asSymbol -> 91.	GeneralMIDIMap		add: 'Pad 4 (choir)' asSymbol -> 92.	GeneralMIDIMap		add: 'Pad 5 (bowed)' asSymbol -> 93.	GeneralMIDIMap		add: 'Pad 6 (metallic)' asSymbol -> 94.	GeneralMIDIMap		add: 'Pad 7 (halo)' asSymbol -> 95.	GeneralMIDIMap		add: 'Pad 8 (sweep)' asSymbol -> 96.	GeneralMIDIMap		add: 'FX 1 (rain)' asSymbol -> 97.	GeneralMIDIMap		add: 'FX 2 (soundtrack)' asSymbol -> 98.	GeneralMIDIMap		add: 'FX 3 (crystal)' asSymbol -> 99.	GeneralMIDIMap		add: 'FX 4 (atmosphere)' asSymbol -> 100.	GeneralMIDIMap		add: 'FX 5 (brightness)' asSymbol -> 101.	GeneralMIDIMap		add: 'FX 6 (goblins)' asSymbol -> 102.	GeneralMIDIMap		add: 'FX 7 (echoes)' asSymbol -> 103.	GeneralMIDIMap		add: 'FX 8 (sci-fi)' asSymbol -> 104.	GeneralMIDIMap		add: 'Sitar' asSymbol -> 105.	GeneralMIDIMap		add: 'Banjo' asSymbol -> 106.	GeneralMIDIMap		add: 'Shamisen' asSymbol -> 107.	GeneralMIDIMap		add: 'Koto' asSymbol -> 108.	GeneralMIDIMap		add: 'Kalimba' asSymbol -> 109.	GeneralMIDIMap		add: 'Bag pipe' asSymbol -> 110.	GeneralMIDIMap		add: 'Fiddle' asSymbol -> 111.	GeneralMIDIMap		add: 'Shanai' asSymbol -> 112.	GeneralMIDIMap		add: 'Tinkle Bell' asSymbol -> 113.	GeneralMIDIMap		add: 'Agogo' asSymbol -> 114.	GeneralMIDIMap		add: 'Steel Drums' asSymbol -> 115.	GeneralMIDIMap		add: 'Woodblock' asSymbol -> 116.	GeneralMIDIMap		add: 'Taiko Drum' asSymbol -> 117.	GeneralMIDIMap		add: 'Melodic Tom' asSymbol -> 118.	GeneralMIDIMap		add: 'Synth Drum' asSymbol -> 119.	GeneralMIDIMap		add: 'Reverse Cymbal' asSymbol -> 120.	GeneralMIDIMap		add: 'Guitar Fret Noise' asSymbol -> 121.	GeneralMIDIMap		add: 'Breath Noise' asSymbol -> 122.	GeneralMIDIMap		add: 'Seashore' asSymbol -> 123.	GeneralMIDIMap		add: 'Bird Tweet' asSymbol -> 124.	GeneralMIDIMap		add: 'Telephone Ring' asSymbol -> 125.	GeneralMIDIMap		add: 'Helicopter' asSymbol -> 126.	GeneralMIDIMap		add: 'Applause' asSymbol -> 127.	GeneralMIDIMap		add: 'Gunshot' asSymbol -> 128! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/13/2023 22:41'!initializeMIDIDrums	MIDIDrums := Dictionary new.	MIDIDrums add: 'Open Cuica' asSymbol -> 79.	MIDIDrums add: 'Mute Triangle' asSymbol -> 80.	MIDIDrums add: 'Open Triangle' asSymbol -> 81.	MIDIDrums add: 'Acoustic Bass Drum' asSymbol -> 35.	MIDIDrums add: 'Bass Drum 1' asSymbol -> 36.	MIDIDrums add: 'Side Stick' asSymbol -> 37.	MIDIDrums add: 'Acoustic Snare' asSymbol -> 38.	MIDIDrums add: 'Hand Clap' asSymbol -> 39.	MIDIDrums add: 'Electric Snare' asSymbol -> 40.	MIDIDrums add: 'Low Floor Tom' asSymbol -> 41.	MIDIDrums add: 'Closed Hi Hat' asSymbol -> 42.	MIDIDrums add: 'High Floor Tom' asSymbol -> 43.	MIDIDrums add: 'Pedal Hi-Hat' asSymbol -> 44.	MIDIDrums add: 'Low Tom' asSymbol -> 45.	MIDIDrums add: 'Open Hi-Hat' asSymbol -> 46.	MIDIDrums add: 'Low-Mid Tom' asSymbol -> 47.	MIDIDrums add: 'Hi Mid Tom' asSymbol -> 48.	MIDIDrums add: 'Crash Cymbal 1' asSymbol -> 49.	MIDIDrums add: 'High Tom' asSymbol -> 50.	MIDIDrums add: 'Ride Cymbal 1' asSymbol -> 51.	MIDIDrums add: 'Chinese Cymbal' asSymbol -> 52.	MIDIDrums add: 'Ride Bell' asSymbol -> 53.	MIDIDrums add: 'Tambourine' asSymbol -> 54.	MIDIDrums add: 'Splash Cymbal' asSymbol -> 55.	MIDIDrums add: 'Cowbell' asSymbol -> 56.	MIDIDrums add: 'Crash Cymbal 2' asSymbol -> 57.	MIDIDrums add: 'Vibraslap' asSymbol -> 58.	MIDIDrums add: 'Ride Cymbal 2' asSymbol -> 59.	MIDIDrums add: 'Hi Bongo' asSymbol -> 60.	MIDIDrums add: 'Low Bongo' asSymbol -> 61.	MIDIDrums add: 'Mute Hi Conga' asSymbol -> 62.	MIDIDrums add: 'Open Hi Conga' asSymbol -> 63.	MIDIDrums add: 'Low Conga' asSymbol -> 64.	MIDIDrums add: 'High Timbale' asSymbol -> 65.	MIDIDrums add: 'Low Timbale' asSymbol -> 66.	MIDIDrums add: 'High Agogo' asSymbol -> 67.	MIDIDrums add: 'Low Agogo' asSymbol -> 68.	MIDIDrums add: 'Cabasa' asSymbol -> 69.	MIDIDrums add: 'Maracas' asSymbol -> 70.	MIDIDrums add: 'Short Whistle' asSymbol -> 71.	MIDIDrums add: 'Long Whistle' asSymbol -> 72.	MIDIDrums add: 'Short Guiro' asSymbol -> 73.	MIDIDrums add: 'Long Guiro' asSymbol -> 74.	MIDIDrums add: 'Claves' asSymbol -> 75.	MIDIDrums add: 'Hi Wood Block' asSymbol -> 76.	MIDIDrums add: 'Low Wood Block' asSymbol -> 77.	MIDIDrums add: 'Mute Cuica' asSymbol -> 78.	GeneralMIDIFamilies := Dictionary new.	GeneralMIDIFamilies add: 'Pianos' asSymbol -> (1 to: 8).	GeneralMIDIFamilies add: 'Tuned Pecussion' asSymbol -> (9 to: 16).	GeneralMIDIFamilies add: 'Organs' asSymbol -> (17 to: 24).	GeneralMIDIFamilies add: 'Guitars' asSymbol -> (25 to: 32).	GeneralMIDIFamilies add: 'Bass' asSymbol -> (33 to: 40).	GeneralMIDIFamilies add: 'Solo Strings' asSymbol -> (41 to: 48).	GeneralMIDIFamilies add: 'Ensembles' asSymbol -> (49 to: 56).	GeneralMIDIFamilies add: 'Brass' asSymbol -> (57 to: 64).	GeneralMIDIFamilies add: 'Reeds' asSymbol -> (65 to: 72).	GeneralMIDIFamilies add: 'Pipes' asSymbol -> (73 to: 80).	GeneralMIDIFamilies add: 'Synth Lead' asSymbol -> (81 to: 88).	GeneralMIDIFamilies add: 'Synth Pad' asSymbol -> (89 to: 96).	GeneralMIDIFamilies add: 'Synth Effects' asSymbol -> (97 to: 104).	GeneralMIDIFamilies add: 'Ethnic' asSymbol -> (105 to: 112).	GeneralMIDIFamilies add: 'Percussion' asSymbol -> (113 to: 120).	GeneralMIDIFamilies add: 'Sound Effects' asSymbol -> (121 to: 128)! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/13/2023 22:45'!initializeMIDITables	"MIDIPort initializeMIDITables"		MIDICommands := Dictionary new.	MIDICommands		at: #noteOn		put: 144.	MIDICommands		at: #noteOff		put: 128.	MIDICommands		at: #polyTouch		put: 160.	MIDICommands		at: #chanTouch		put: 208.	MIDICommands		at: #ctrlChange		put: 176.	MIDICommands		at: #progChange		put: 192.	MIDICommands		at: #pitchWheel		put: 224.	MIDICommands		at: #sysEx		put: 240.			self initializeGeneralMIDI1.	self initializeGeneralMIDI2.		self initializeMIDIDrums! !!MIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/5/2023 13:34'!streams^Streams! !Voice subclass: #MIDIVoice	instanceVariableNames: 'currentTime channel'	classVariableNames: 'Instance UseSingleton'	poolDictionaries: ''	category: 'MusicIO-MIDI'!!MIDIVoice commentStamp: '<historical>' prior: 0!A MidiVoice is used to play note events onto one or more output devices.
I store a MidiDevice (with a MIDIPort) in my instrument variable and play events onto it when asked to.

Instance variables:
	instrument	<MIDIDevice> my instrument, a MidiDevice
!!MIDIVoice methodsFor: 'initialize-release'!close
	"Close the stream, device, or whatever."

	instrument close! !!MIDIVoice methodsFor: 'initialize-release' stamp: 'stp 1/2/2023 10:26'!initialize	"Set up a MIDIVoice's time counter."	super initialize.	currentTime := nil.	name := 'A MIDI Voice'! !!MIDIVoice methodsFor: 'initialize-release'!reset
	"Reset my time."
	
	currentTime := nil! !!MIDIVoice methodsFor: 'events'!play: anEventList
	"Expand the list and play it now."

	anEventList voice: self.
	^anEventList play! !!MIDIVoice methodsFor: 'events' stamp: 'stp 1/19/2023 05:54'!playEvent: anEvent at: aTime
	"Send the given Event to the instrument  on my channel."

	^ aTime isNil
		ifTrue: [instrument play: anEvent at: Time microsecondClock chan: channel]
		ifFalse: [instrument play: anEvent at: aTime usec value chan: channel]! !!MIDIVoice methodsFor: 'accessing' stamp: 'stp 1/19/2023 06:53'!channel	"Answer my MIDI channel"		^ channel! !!MIDIVoice methodsFor: 'accessing' stamp: 'stp 1/19/2023 06:54'!channel: anInt	"Set my MIDI channel - this is where we switch from 1-based to 0-based"		channel := anInt - 1! !!MIDIVoice methodsFor: 'accessing'!number
	"Answer my channel or icon number"

	| n |
	n := Number readFrom: (ReadStream on: (name)).
	n = 0 ifTrue: [^self channel].
	^n! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIVoice class	instanceVariableNames: ''!!MIDIVoice class methodsFor: 'examples'!randomExample
	"Play some random notes on a voice on a device on a port."
	"MIDIVoice randomExample"

	| events voice |
	events := (EventList randomExample: 80 
		from: ((#duration: -> (20 to: 80)), (#pitch: -> (48 to: 80)),
				(#ampl: -> (84 to: 124)))).
	voice := MIDIVoice default.
	events playOn: voice! !!MIDIVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:32'!scaleExample	"Play a scale on a voice on a device on a port to the MIDI Mgr."	"MIDIVoice scaleExample"	"MIDIPort allNotesOff"		| voice device port scale |	port := MIDIPort default.	device := MIDIDevice on: port.	voice := MIDIVoice on: device.	scale := EventList scaleFrom: 48 to: 72 in: 3.0.	scale eventsDo: [ :e | e duration: e duration * 2].	voice play: scale! !!MIDIVoice class methodsFor: 'examples'!voiceInspect
	"MIDIVoice voiceInspect"

	(MIDIVoice on: (MIDIDevice on: (MIDIPort default))) inspect! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 07:59'!default	"Answer the default MIDIVoice."	"MIDIVoice default"		^ self defaultOut! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 07:59'!defaultIn		"Answer the default MIDIVoice."	"MIDIVoice default"		UseSingleton		ifTrue: [Instance				ifNil: [Instance := MIDIVoice								on: (MIDIDevice on: MIDIPort default)].			^ Instance]		ifFalse: [^ MIDIVoice				on: (MIDIDevice on: MIDIPort default)]! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/17/2023 17:41'!defaultOut	"Answer the default MIDIVoice for channel 1."	"MIDIVoice default"	"MIDIVoice instanceCount"		^ self defaultOut: 1! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/17/2023 17:42'!defaultOut: chan	"Answer the default MIDIVoice."	"MIDIVoice default"	"MIDIVoice instanceCount"		| mv |	mv := MIDIVoice		on: (MIDIDevice outOn: MIDIPort default).	mv channel: chan.	^ mv! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 13:27'!named: aName onDevice: aMidiDevice channel: aChannel 	"set up a MidiVoice on the given device and channel"	"MIDIVoice named: 'xxx' onDevice: (MIDIDevice on: 1) channel: 1"	| me | self halt.	me := self new initializeNamed: aName.	me instrument add: aChannel -> aMidiDevice.	^ me! !!MIDIVoice class methodsFor: 'instance creation'!new
	"Cache the latest instance."

	^self default! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 06:57'!on: aMidiDevice 	"Answer up a MidiVoice on the given device."	"MIDIVoice on: (MIDIDevice on: (MIDIPort default openOutput))"	| me |	me := self basicNew initialize.	me instrument: aMidiDevice.	me channel: 1.	^ me! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/17/2023 17:44'!on: aMidiDevice channel: aChannel 	"Set up a MidiVoice on the given device and channel"	"MIDIVoice on: 			(MIDIDevice 				outOn: (PortMIDIPort indexOfOutNamed: '*Kontakt*') 				port: MIDIPort concreteClass default) 		channel: 1"		| me |	me := self new initialize.	me instrument: aMidiDevice.	me channel: aChannel.	^ me! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 05:59'!onDevice: aMidiDevice channel: aChannel 	"Set up a MidiVoice on the given device and channel"	"MIDIVoice		on:  (MIDIDevice 				outOn: (PortMIDIPort indexOfOutNamed: '*Kontakt*') 				port: MIDIPort concreteClass default) 		channel: 1"		| me |	me := self new initialize.	me instrument: aMidiDevice.	me channel: aChannel.	^ me! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 06:52'!onDeviceNamed: aName channel: aChannel 	"Set up a MidiVoice on the given device and channel"	"MIDIVoice onDeviceNamed: 'Kontakt' channel: 1"		| me |	me := self new initialize.	me		instrument: (MIDIDevice				outOn: (PortMIDIPort indexOfOutNamed: '*' , aName , '*')				port: MIDIPort concreteClass default).	me channel: aChannel.	^ me! !!MIDIVoice class methodsFor: 'instance creation' stamp: 'stp 1/19/2023 06:00'!outOn: dev chan: chan 	"Answer the selected MIDIVoice."	"MIDIVoice default"	"MIDIVoice instanceCount"		| mv |	mv := MIDIVoice				on: (MIDIDevice outOn: dev).	mv channel: chan.	^ mv! !!MIDIVoice class methodsFor: 'class initialization' stamp: 'stp 12/31/2022 13:59'!defaultOutInterface	"Answer the class's default intertface # (use external device list to get	the right value)."	"MIDIVoice defaultOutInterface"	^ 6! !!MIDIVoice class methodsFor: 'class initialization'!flushDefault
	"Reset the class's default instance."
	"MIDIVoice flushDefault"

	Instance := nil! !!MIDIVoice class methodsFor: 'class initialization'!initialize
	"Reset the class's default instance."
	"MIDIVoice initialize"

	Instance := nil.
	UseSingleton := false.! !ExternalLibrary subclass: #PortMIDILibrary	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!PortMIDILibrary commentStamp: 'stp 12/9/2022 12:00' prior: 0!A PortMIDILibrary is an external library to interface with the PortMIDI library -- see https://portmedia.sourceforge.net

Instance Variables

	Interface
	port

Calls

!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PortMIDILibrary class	instanceVariableNames: ''!!PortMIDILibrary class methodsFor: 'instance creation' stamp: 'stp 1/17/2023 16:53'!new	"Make certain to initialize instances, or just use the class."	"	| lib |	lib := PortMIDILibrary someInstance.	lib ifNil: 		[lib := PortMIDILibrary basicNew].	^ lib"		self error: 'Do not make instances of PortMIDILibrary'! !!PortMIDILibrary class methodsFor: 'system startup' stamp: 'stp 12/27/2022 11:45:15'!initialize
	"
	self initialize
	"
	Smalltalk addToStartUpList: self after: (Smalltalk classNamed: #CStandardLibrary)! !!PortMIDILibrary class methodsFor: 'accessing' stamp: 'stp 1/17/2023 16:54'!moduleName
	"Answer the module name"
	
	^ 'libportmidi.2.0.3.dylib'! !!PortMIDILibrary class methodsFor: 'testing' stamp: 'stp 1/18/2023 12:42'!resetMIDIDriver	"Close and reopen the low-level driver"	"PortMIDILibrary resetMIDIDriver"	"PortMIDILibrary initialize"		| num |	PortModel initialize.	Smalltalk garbageCollect.	PortMIDILibrary pm_terminate.	PortMIDILibrary pm_initialize.	num := PortMIDILibrary pm_count_devices.	Transcript cr; show: 'MIDI reset; found ' , num printString , ' MIDI devices.'; cr.	PortMIDIPort listDeviceTable! !!PortMIDILibrary class methodsFor: 'testing' stamp: 'stp 12/29/2022 04:04'!testANote
	"Try playing a note."
	"PortMIDILibrary listDeviceTable"

	"PortMIDILibrary new pm_count_devices "
	"PortMIDILibrary new pm_default_input_device "
	"PortMIDILibrary new pm_default_output_device "
	"PortMIDILibrary new pm_test"

	PortMIDILibrary new pm_test! !!PortMIDILibrary class methodsFor: 'prims - initialization' stamp: 'stp 1/23/2023 17:50'!pm_close: which	"Close the selected MIDI output device/port."	<cdecl: int32_t pm_close(uint32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_close' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - initialization' stamp: 'stp 1/23/2023 17:50'!pm_initialize	"Start the MIDI back-end."	"PortMIDILibrary pm_initialize"		<cdecl: int32_t pm_initialize(void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_initialize' module: 'PortMIDIPrims'>"	^self externalCallFailed	! !!PortMIDILibrary class methodsFor: 'prims - initialization' stamp: 'stp 1/23/2023 17:51'!pm_open: which with: direction	"Open the selected MIDI output device/port."		<cdecl: int32_t pm_open(int32_t, uint32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_open' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - initialization' stamp: 'stp 1/23/2023 17:51'!pm_terminate	"Stop the MIDI back-end."	"PortMIDILibrary new pm_terminate"		<cdecl: int32_t pm_terminate(void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_terminate' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - I/O' stamp: 'stp 1/23/2023 17:51'!pm_poll: which	"The MIDI driver input process poll."	<cdecl: uint32_t pm_poll(uint32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_poll' module: 'PortMIDIPrims'>"		^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - I/O' stamp: 'stp 1/23/2023 17:51'!pm_read: which	"The MIDI driver input read prim."	<cdecl: uint64_t pm_read(uint32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_read' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - I/O' stamp: 'stp 1/23/2023 17:51'!pm_read_controllers: which with: lo with: hi with: array	"Read a range of controllers"	<cdecl: int32_t pm_read_controllers(uint32_t, uint32_t, uint32_t, short *) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_read_controllers:' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - I/O' stamp: 'stp 1/23/2023 17:52'!pm_write_data2: which with: d1 with: d2	"Write a 2-byte MIDI Command."	<cdecl: int32_t pm_write_data2(uint32_t, char, char) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_write_data2' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - I/O' stamp: 'stp 1/23/2023 17:53'!pm_write_data3: which with: d1 with: d2 with: d3	"Write a standard 3-byte MIDI Command."	<cdecl: int32_t pm_write_data3(uint32_t, char, char, char) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_write_data3:' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:50'!pm_count_devices	"Answer the number of MIDI ports"	"PortMIDILibrary pm_count_devices"		<cdecl: int32_t pm_count_devices (void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_count_devices' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:50'!pm_default_input_device	"Answer the default MIDI input device/port."	"PortMIDILibrary new pm_default_input_device"	<cdecl: int32_t pm_default_input_device (void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_default_input_device' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:50'!pm_default_output_device	"Answer the default MIDI output device/port."	"PortMIDILibrary new pm_default_output_device"	<cdecl: int32_t pm_default_output_device (void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_default_output_device' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:50'!pm_dev_dir: which	"Answer the i/o direction of the selected MIDI output device/port."	<cdecl: int32_t pm_dev_dir(int32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_dev_dir' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:50'!pm_get_name: which	"Answer the name of the selected MIDI output device/port."	<cdecl: const char * pm_get_name(uint32_t) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_get_name' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:51'!pm_start_controller_cacheing	"Start MIDI controller cacheing."	<cdecl: int32_t pm_start_controller_cacheing(void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_start_controller_cacheing' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:51'!pm_stop_controller_cacheing	"Stop MIDI controller cacheing."	<cdecl: int32_t pm_stop_controller_cacheing(void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_stop_controller_cacheing' module: 'PortMIDIPrims'>"	^self externalCallFailed! !!PortMIDILibrary class methodsFor: 'prims - operations' stamp: 'stp 1/23/2023 17:52'!pm_test	"Test MIDI by playing a note.."	"PortMIDILibrary pm_test"		<cdecl: void pm_test (void) module: 'libportmidi.2.0.3.dylib' >"	<primitive: 'pm_test' module: 'PortMIDIPrims'>"	^self externalCallFailed! !MIDIPort subclass: #PortMIDIPort	instanceVariableNames: 'driver'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-MIDI'!!PortMIDIPort commentStamp: 'stp 1/18/2023 06:29' prior: 0!An instance of PortMIDIPort is the interface to the external driver that talks to the PortMIDI interface.

Instance Variables:
	driver	<PortMidiInterface class>  my I/O interface driver

Shared Variables:
	Devices	<Dictionary>  unised
	Streams	<IdentityDictionary key: Integer value: Integer>  the map between Siren devices and interface ports
!!PortMIDIPort methodsFor: 'initialize release' stamp: 'stp 1/14/2023 10:16'!initialize	"Setup the receiver's instance variables."	"MIDIPort testANote"	"PortMIDIPort devices"		| return |	super initialize.	driver := PortMIDILibrary.			"use the class rather than an instance now"	return := driver pm_initialize.	return = 0		ifFalse: [self error: 'MIDI initialize failed'].	status := #init! !!PortMIDIPort methodsFor: 'initialize release'!release
	"Release--break and dependencies on the error value."

	super release.
	[driver ifNotNil: [self critical: [driver pm_terminate]]]
		on: Error
		do: [ :ex | "ignore" ]! !!PortMIDIPort methodsFor: 'accessing'!isActive
	"Answer whether the receiver is active."

	^(status == #open)! !!PortMIDIPort methodsFor: 'open/close'!close
	"Close MIDI."
	"MIDIPort instance close"

	self close: device! !!PortMIDIPort methodsFor: 'open/close'!close: stream
	"Close MIDI."
	"MIDIPort instance close"

	| which |
	readProcess ifNotNil: 
			[readProcess terminate.
			readProcess release].
	readProcess := nil.
	self breakDependents.
	which := Streams at: stream ifAbsent: [nil].
	self critical: [which ifNotNil:
		[[driver pm_close: which]
			on: Error
			do: [ :ex | Transcript show: 'MIDI close error'; cr]]].
	status := #closed.
	^#OK! !!PortMIDIPort methodsFor: 'open/close' stamp: 'stp 1/2/2023 14:54'!open
	"Open the MIDI driver -- start the lower-level driver up."
	"MIDIPort default"

	self isActive
		ifTrue: [^self].
	^self openOutput: SirenUtility defaultMIDIOut! !!PortMIDIPort methodsFor: 'open/close' stamp: 'stp 1/17/2023 17:29'!openInput: dev 	"Open the MIDI driver -- start the lower-level driver up."	"MIDIPort default"	| which |	status == #open		ifTrue: [^ self].	[which := driver pm_open: dev with: 1]		on: Error		do: [:ex | 			Transcript show: 'MIDI openInput error';				 cr.			which := -1].	which < 0		ifTrue: [self error: 'MIDI open failed'].	device := dev.	Streams at: dev put: which.	status := #open.	^ which! !!PortMIDIPort methodsFor: 'open/close' stamp: 'stp 1/18/2023 12:58'!openOutput: dev 	"Open the MIDI driver -- start the lower-level driver up."	"MIDIPort default"		| which dPtr |	dev isString		ifTrue: [dPtr := (self class devices detect: [ :d | d name = dev]) port]		ifFalse: [dPtr := dev].	((status == #open) and: [Streams includesKey: dPtr]) ifTrue: [ ^ self].	self critical: 		[[which := driver pm_open: dPtr with: 0]				on: Error				do: [ :ex | 					which := -1.					Transcript show: 'MIDI open failed'; cr]].	which < 0		ifTrue: [self error: 'MIDI open failed'].	Streams at: dPtr put: which.	status := #open.	device := dev! !!PortMIDIPort methodsFor: 'open/close'!terminate
	"Close MIDI."
	"MIDIPort instance close"

	driver pm_terminate.
	status := #init! !!PortMIDIPort methodsFor: 'ioctl'!readLoop
	"The MIDI driver input process loop."

	| int packet delay dev |
	delay := Delay forMilliseconds: 5.					"default loop delay"
	dev := Streams at: device ifAbsent: [Streams at: self class in].
	self resetInput.
"LOOP"
	[true] whileTrue: 
		[((driver pm_poll: dev) = 0)				"poll"
			ifTrue: [delay wait]
			ifFalse: [self critical: [int := driver pm_read: dev].	"read"
				(int = 0) ifFalse: 
					[packet := MIDIPacket fromInt: int 		"parse"
								at: Time microsecondClock.
					SirenUtility log: '		', packet printString level:2.
					inputData addLast: packet.			"queue"
					self changed: #read]]]					"signal"! !!PortMIDIPort methodsFor: 'ioctl'!startControllerCaching
	"Start caching MIDI controller in the driver."

	self critical: [driver pm_start_controller_cacheing]! !!PortMIDIPort methodsFor: 'ioctl'!stopControllerCaching
	"Stop caching MIDI controller in the driver."

	self critical: [driver pm_stop_controller_cacheing]! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/18/2023 15:57'!playOff: aPitch amp: anAmp voice: aVoice 	"Play the argument on the receiver--no duration-->no note-off."	SirenUtility		ifVerbose: [device ifNotNil: 			[Transcript show: 'MIDI playOff (' , (Streams at: device) printString , ') ' , 					aVoice printString , ' - ' , aPitch printString , ' - ' , anAmp printString; cr]].	[self critical: 		[driver				pm_write_data3: (Streams at: device)				with: ((MIDICommands at: #noteOff) bitOr: aVoice)				with: aPitch				with: 0]]		on: Error		do: [:ex | Transcript show: 'MIDI write error'; cr]! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/18/2023 15:58'!playOff: streamID pitch: pitch amp: amp voice: voice 	"Play the argument on the receiver."	SirenUtility		ifVerbose: [Transcript show: 'MIDI playOffS (' , streamID printString , ') ' , voice printString , ' - ' , 					pitch printString , ' - ' , amp printString; cr].	self critical: 		[driver pm_write_data3: (Streams at: streamID ifAbsent: [0])				with: ((MIDICommands at: #noteOn) bitOr: voice)				with: pitch				with: 0]! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/18/2023 15:52'!playOn: aPitch amp: anAmp voice: aVoice 	"Play the argument on the receiver--no duration-->no note-off."	SirenUtility		ifVerbose: [device ifNotNil: 			[Transcript show: 'MIDI playOn (' ,  (Streams at: device) printString , ') ' , aVoice printString , ' - ' , 						aPitch printString , ' - ' , anAmp printString; cr]].	[self		critical: [driver				pm_write_data3: (Streams at: device)				with: ((MIDICommands at: #noteOn)						bitOr: aVoice)				with: aPitch				with: anAmp]]		on: Error		do: [:ex | Transcript show: 'MIDI write error.';				 cr] "Period or right bracket expected ->"! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/18/2023 15:58'!playOn: streamID pitch: pitch amp: amp voice: voice 	SirenUtility		ifVerbose: [device ifNotNil: 			[Transcript show: 'MIDI playOn (' ,  (Streams at: device) printString , ') ' , voice printString , ' - ' , 						pitch printString , ' - ' , amp printString; cr]].	self critical: 		[driver pm_write_data3: (Streams at: device)				with: ((MIDICommands at: #noteOn) bitOr: voice)				with: pitch				with: amp]! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/2/2023 15:49'!put: streamID data: data length: size 	"Send the argument (a ByteArray for historical reasons) to the	receiver now."	[size < 2		ifTrue: [^ self error: 'Unknown MIDI command format -- illegal length'].	SirenUtility		ifVerbose: [Transcript show: 'MIDI putS (' , streamID printString , ') ' , data printString;				 cr].	size = 2		ifTrue: [^ self				critical: [driver						pm_write_data2: streamID						with: (data at: 1)						with: (data at: 2)]].	size = 3		ifTrue: [^ self				critical: [driver						pm_write_data3: streamID						with: (data at: 1)						with: (data at: 2)						with: (data at: 3)]].	size > 3		ifTrue: [self halt			"not supported yet"]]		on: Error		do: [:ex | Transcript show: 'MIDI write error';				 cr].	^ nil! !!PortMIDIPort methodsFor: 'read/write' stamp: 'stp 1/3/2023 18:31'!put: data length: size 	"Send the argument (a ByteArray for historical reasons) to the	receiver now."	SirenUtility		ifVerbose: [Transcript show: 'MIDI put (' , device printString , ') ' , data printString;				 cr]. 	size = 2		ifTrue: [^ driver				pm_write_data2: device				with: (data at: 1)				with: (data at: 2)].	size = 3		ifTrue: [^ driver				pm_write_data3: device				with: (data at: 1)				with: (data at: 2)				with: (data at: 3)].	self error: 'Invalid MIDI message'! !!PortMIDIPort methodsFor: 'read/write'!readControllersFrom: lo to: hi into: array
	"Read a range of controllers"

	self critical: [driver pm_read_controllers: (Streams at: device) with: lo with: hi with: array].! !!PortMIDIPort methodsFor: 'printing' stamp: 'stp 1/2/2023 11:27'!printOn: aStream 	"Append to the argument aStream a sequence of characters 	that	identifies the collection."	super printOn: aStream.	aStream nextPutAll: ' of driver ' , driver printString! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PortMIDIPort class	instanceVariableNames: ''!!PortMIDIPort class methodsFor: 'examples' stamp: 'stp 1/19/2023 06:58'!testANote	"Open MIDI, play a note."	"PortMIDIPort testANote"	| inst |	inst := MIDIPort  defaultOut: 1.	"this answers an instance of me."	inst open.	inst		playOn: 48		amp: 100		voice: 0.	1 wait.	inst		playOff: 48		amp: 100		voice: 0.	inst close.	inst terminate.	MIDIPort cleanUp! !!PortMIDIPort class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 13:29'!indexOfInNamed: nam 	"PortMIDIPort indexOfOutNamed: '*Nord*'"	| which |	devices		ifEmpty: [PortMIDILibrary resetMIDIDriver.			PortMIDIPort new].	1		to: devices size		do: [:ind | 			| aD |			aD := devices at: ind.			((nam match: aD name)					and: [aD direction == #in])				ifTrue: [which := ind]].	^ which - 1! !!PortMIDIPort class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 13:29'!indexOfOutNamed: nam 	"PortMIDIPort outDeviceNamed: '*Nord*'"	| which |	devices		ifEmpty: [PortMIDILibrary resetMIDIDriver.			PortMIDIPort new].	1		to: devices size		do: [:ind | 			| aD |			aD := devices at: ind.			((nam match: aD name)					and: [aD direction == #out])				ifTrue: [which := ind]].	^ which - 1! !!PortMIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/18/2023 12:39'!initializeData	"Set up the class defaults and load the device table."	"PortModel initialize"	"PortMIDIPort initializeData"	"PortMIDIPort instanceCount"	"PortMIDIPort someInstance"	| driver |	instance := nil.	useSingleton := false.	mutex := Semaphore forMutualExclusion.	self resetDevices.	driver := PortMIDILibrary "new".	self mutex		critical: [0				to: driver pm_count_devices - 1				do: [ :cnt | 					| nam dir dev |					nam := driver pm_get_name: cnt.					nam						ifNil: [nam := 'Anonymous'].					dir := driver pm_dev_dir: cnt.					dev := DeviceModel new name: nam asSymbol;								 port: cnt.					dir == 0						ifTrue: [dev out: 1]						ifFalse: [dev in: 1].					devices add: dev]]."	self listDeviceTable"! !!PortMIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/17/2023 16:56'!listDeviceTable	"Print a list of devices to the transcript."	"PortModel initialize"	"PortMIDIPort listDeviceTable"	"MIDI reset; found 9 MIDI devices	MIDI Device Table (0 - 4)	# 0 - MIDIPLUS TBOX 2x2 Midi In 1 - out -- DEFAULT 	# 1 - Faders - out	# 2 - IAC Driver Bus 1 - out	# 3 - Nord Stage 3 MIDI Output - out	# 4 - Nord Stage 3 - in -- DEFAULT 	# 5 - MIDIPLUS TBOX 2x2 Midi Out 2 - in	# 6 - IAC Driver Bus 1 - in	# 7 - Nord Stage 3 MIDI Input - in	# 8 - Kontakt 7 Virtual Input - in"	| lib num din dout |	lib := PortMIDILibrary "new".	num := lib pm_count_devices.	din := lib pm_default_input_device.	dout := lib pm_default_output_device.	SirenUtility defaultMIDIIn: din.	SirenUtility defaultMIDIOut: dout.	Transcript show: 'MIDI Device Table';		 cr.	1		to: devices size		do: [:ind | 			| cnt dev |			cnt := ind - 1.			dev := devices at: ind.			Transcript tab; show: '# ' , cnt printString , ' - ' , dev name , ' - '.			dev in				ifTrue: [Transcript show: 'out'].			dev out				ifTrue: [Transcript show: 'in'].			(cnt == din					or: [cnt == dout])				ifTrue: [Transcript show: ' -- DEFAULT '].			Transcript cr].	Transcript cr! !!PortMIDIPort class methodsFor: 'class initialization' stamp: 'stp 1/5/2023 10:45'!resetDevices	"Flush the device list"	devices := OrderedCollection new! !MIDIDevice initialize!MIDIPort initialize!MIDIVoice initialize!PortMIDILibrary initialize!