Object subclass: #AbstractOSCPacket	instanceVariableNames: 'oscBytes'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-OSC'!!AbstractOSCPacket commentStamp: '<historical>' prior: 0!AbstractOSCPacket is the parent of the concrete OSC packet classes.

Subclasses must implement the following messages:
	osc>>toOSCBytes:

Instance Variables:
	oscBytes	<ByteArray>  my packet contents
!!AbstractOSCPacket methodsFor: 'osc'!toOSCBytes: converter 

	self subclassResponsibility.
	^self! !!AbstractOSCPacket methodsFor: 'accessing'!oscBytes

	^oscBytes! !!AbstractOSCPacket methodsFor: 'accessing'!oscSize

	^oscBytes size! !!AbstractOSCPacket methodsFor: 'initialize-release'!init

	| convertor |
	convertor := OSCByteConvertor new.
	self toOSCBytes: convertor.
	oscBytes := convertor contents.
	^self! !AbstractOSCPacket subclass: #OSCBundle	instanceVariableNames: 'messages time'	classVariableNames: 'MillisecScale MillisecToNTP SecsInAYear'	poolDictionaries: ''	category: 'MusicIO-OSC'!!OSCBundle commentStamp: '<historical>' prior: 0!OSCBundle represents a bundle of OSC messages

Instance Variables:
	messages	<OrderedCollection of OSCMessage>  my contents
	time	<Integer>  my action time

Shared Variables:
	MillisecScale	<Integer>  msec/year (?)
	MillisecToNTP	<Fraction>  msec scale for NTP
	SecsInAYear	<Integer>  sec/year
!!OSCBundle methodsFor: 'osc'!timeTagToOSCBytes: aConvertor 
	"Store a time thingy into the given convertor"

	time isNil 
		ifTrue: 
			[8 timesRepeat: [aConvertor nextPut: 0].
			^self].
	(self class asSecondsSince1900: time) toOSCBytes: aConvertor.
	(self class asNTPMilliseconds: time) toOSCBytes: aConvertor! !!OSCBundle methodsFor: 'osc'!toOSCBytes: aConvertor 
	"^self"

	'#bundle' toOSCBytes: aConvertor.
	self timeTagToOSCBytes: aConvertor.
	messages
		do: 
			[:aMessage | 
			aMessage oscSize toOSCBytes: aConvertor.
			aConvertor nextPutAll: aMessage oscBytes]! !!OSCBundle methodsFor: 'initialize-release' stamp: 'stp 12/31/2022 10:18'!init: aCollection 	self init: aCollection time: Time now! !!OSCBundle methodsFor: 'initialize-release'!init: aCollection time: aTime 
	"Initialize the receiver."

	messages := aCollection.
	time := aTime.
	self init! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OSCBundle class	instanceVariableNames: ''!!OSCBundle class methodsFor: 'instance creation' stamp: 'stp 12/31/2022 10:18'!with: aCollection 	^ self with: aCollection at: Time now! !!OSCBundle class methodsFor: 'instance creation'!with: aCollection at: aTimestamp
	^self new init: aCollection time: aTimestamp! !!OSCBundle class methodsFor: 'support'!asNTPMilliseconds: aTimestamp 
	"Convert aTimestamp to the OSC representation of the same"

	| msec |
	msec := aTimestamp millisecond * MillisecToNTP. "((2 raisedTo: 32) / 1000)"
	^msec asInteger! !!OSCBundle class methodsFor: 'support'!asSecondsSince1900: aTimestamp 
	"Convert aTimestamp to seconds since 1900"

	| sec |
	"Gets us the number of seconds since Jan 1. 1901"
	sec := aTimestamp asSeconds.
	"sec := sec + SecsInAYear."
	"Convert that to be GMT-based"
	sec := TimeZone default convertLocalSecondsToGMT: sec.
	"Add one year worth of seconds"
	^sec + SecsInAYear. "(60 * 60 * 24 * 365)"! !!OSCBundle class methodsFor: 'support'!timestampToOSC: aTimestamp 
	"Convert aTimestamp to the OSC representation of the same"

	| sec msec |
	sec := self asSecondsSince1900: aTimestamp.
	msec := self asNTPMilliseconds: aTimestamp.
	^(sec bitShift: 32) bitOr: msec! !!OSCBundle class methodsFor: 'class initialization'!initialize
	"Initialize the values of the shared variables."
	"OSCBundle initialize"

	MillisecScale = 4295000.
	MillisecToNTP:= ((2 raisedTo: 32) / 1000).
	SecsInAYear:= 60 * 60 * 24 * 365! !!OSCBundle class methodsFor: 'examples'!example1
	"Simple example"

	^self 
		with: (Array with: OSCMessage scRun with: OSCMessage scIncreaseVolume)! !!OSCBundle class methodsFor: 'examples'!example2

	^self with: (Array with: self example1 with: OSCMessage scDecreaseVolume)! !Object subclass: #OSCByteConvertor	instanceVariableNames: 'packetData'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-OSC'!!OSCByteConvertor commentStamp: '<historical>' prior: 0!Instances of OSCByteConvertor translate between different OSC formats.

Instance Variables:
	packetData	<PositionableStream>  my contents
!!OSCByteConvertor methodsFor: 'stream' stamp: 'stp 2/4/2023 16:56'!contents
	^packetData contents! !!OSCByteConvertor methodsFor: 'stream' stamp: 'stp 2/4/2023 16:56'!next: t1 put: t2 
	^packetData next: t1 put: t2! !!OSCByteConvertor methodsFor: 'stream' stamp: 'stp 2/4/2023 16:57'!nextPut: t1 
	^packetData nextPut: t1! !!OSCByteConvertor methodsFor: 'stream'!nextPutAll: t1 

	^packetData nextPutAll: t1! !!OSCByteConvertor methodsFor: 'stream'!nextPutType: char
"	Transcript show: (String with: char); space."
	^self nextPut: char asInteger! !!OSCByteConvertor methodsFor: 'stream'!position
	^packetData position! !!OSCByteConvertor methodsFor: 'initialize-release' stamp: 'stp 2/4/2023 16:56'!init
	packetData := ByteArray new writeStream.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OSCByteConvertor class	instanceVariableNames: ''!!OSCByteConvertor class methodsFor: 'instance creation'!new

	^super new init! !AbstractOSCPacket subclass: #OSCMessage	instanceVariableNames: 'address arguments'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-OSC'!!OSCMessage commentStamp: '<historical>' prior: 0!OSCMessages are concrete old-format (untyped) OSC messages

Instance Variables:
	address	<String>  my OSC cmomand address
	arguments	<Collection>  the arguments
!!OSCMessage methodsFor: 'osc'!addressToOSCBytes: convertor 
	"Convert the address to the OSC format"
	address toOSCBytes: convertor.
	^self! !!OSCMessage methodsFor: 'osc'!argumentsToOSCBytes: convertor 
	"Add the type tags, then the arguments"

	arguments do: [:each | each toOSCBytes: convertor].
	^self! !!OSCMessage methodsFor: 'osc'!toOSCBytes: converter 
	"Convert myself to OSC format"

	self addressToOSCBytes: converter.
	arguments notEmpty ifTrue: [self argumentsToOSCBytes: converter].
	^self! !!OSCMessage methodsFor: 'osc'!typesToOSC: converter

	| index numPad |
	arguments isEmpty ifTrue: [^self].
	arguments do: [:each | each toOSCTypeOn: converter].
	index := 1.
	numPad := 4 - (converter position \\ 4).
	"This isn't very ST -- should be numPad do:[converter nextPut: 0]"
	[index <= numPad]
		whileTrue: 
			[converter nextPut: 0.
			index := index + 1].
	^self! !!OSCMessage methodsFor: 'printing' stamp: 'stp 1/18/2023 07:06'!printOn: aStream

	aStream nextPutAll: 'OSC: ', address, '  [ '.		arguments do: [ :ar |			ar isFloat ifTrue: [aStream nextPutAll: (ar printString: 4), ' ']			ifFalse: [aStream nextPutAll: ar printString, ' ']].	aStream nextPutAll: ']'! !!OSCMessage methodsFor: 'initialize-release'!initAddress: t1 arguments: t2 

	address := t1.
	arguments := t2.
	self init.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OSCMessage class	instanceVariableNames: ''!!OSCMessage class methodsFor: 'instance-creation'!for: addr

	^self new initAddress: addr arguments: #()! !!OSCMessage class methodsFor: 'instance-creation'!for: addr with: args 
	"Answer a TOM with the given address and arguments"

	^self new initAddress: addr arguments: args! !!OSCMessage class methodsFor: 'examples'!example1
	"OSCMessage example1"

	^self for: '/example1' with: #(100)! !!OSCMessage class methodsFor: 'examples'!example2
	"OSCMessage example2"

	^self for: '/example2' with: #('tester' 10.4)! !!OSCMessage class methodsFor: 'examples'!example3
	"OSCMessage example3"

	^self for: '/example3' with: #(20 30 'blah')! !PortModel subclass: #OSCPort	instanceVariableNames: 'socket address'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-OSC'!!OSCPort commentStamp: '<historical>' prior: 0!An OSCPort represents a connection to an OSC client.

Instance Variables:
	socket <SocketAccessor>  how am I connected?
	address <IPSocketAddress>  where do I go?

Shared Variables:
	DefaultIP <ByteArray>  where's my default server?
	DefaultPort	 <Integer>  where's my default server?
!!OSCPort methodsFor: 'accessing'!portNumber: newPortNum 
	"Reset the receiver's UDP socket port number"

	address port: newPortNum! !!OSCPort methodsFor: 'actions' stamp: 'stp 2/4/2023 16:52'!send: aMessage 
	"Send something - lowest level"

"	SirenUtility log: aMessage printString level: 1."	self critical: [socket sendData: aMessage oscBytes "toHost: #[127 0 0 1] port: 54321" ]! !!OSCPort methodsFor: 'initialize-release' stamp: 'stp 12/10/2022 14:59'!init: sock 
	"Set up"

	socket := sock.
	address := sock address.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OSCPort class	instanceVariableNames: ''!!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/11/2022 15:25'!cslHost
	"Answer the default port for CSL"

	| cslServ |
	cslServ := Socket newUDP.
	cslServ setPeer: self defaultIP port: self defaultPort.

	^self to: cslServ
! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/10/2022 14:57'!default
	"Answer the default instance"

	^self cslHost! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/31/2022 11:18'!localhost	"Answer an OSC port on the local host"	| addr sock |	addr := SocketAddress				hostAddress: (ByteArray						with: 127						with: 0						with: 0						with: 1).	sock := Socket new.	sock connectTo: addr port: self defaultPort.	^ sock! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/31/2022 11:19'!scHost	"Answer the default OSC server reference"	| addr sock |	addr := SocketAddress				hostAddress: (ByteArray						with: 192						with: 168						with: 61						with: 1).	sock := Socket new.	sock connectTo: addr port: self defaultSCOSCPort.	^ sock! !!OSCPort class methodsFor: 'instance-creation'!to: t1 
	"Open a port on the given device"

	^self new init: t1! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/11/2022 15:26'!to: ip port: port
	"^an OSCPort with the given attributes"
	"OSCPort to: #[127 0 0 1] port: 54321"

	| serv |
	serv := Socket newUDP.
	serv setPeer: ip port:port.
	^self to: serv
! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/31/2022 11:20'!toHostName: host 	"Answer an instance on the given host"		| addr sock |	addr := NetNameResolver addressesForName: host.	sock := Socket new.	sock connectTo: addr port: self defaultPort.	^ sock! !!OSCPort class methodsFor: 'instance-creation' stamp: 'stp 12/31/2022 11:21'!toHostName: aHostName portNumber: aPort 	"^an OSCPort 
	Convenience method"	| addr sock |	addr := NetNameResolver addressesForName: aHostName.	sock := Socket new.	sock connectTo: addr port: aPort.	^ sock! !!OSCPort class methodsFor: 'instance-creation'!toLocalhostPortNumber: aPort 
	"^an OSCPort 
	Convenience method"

	^self toHostName: 'localhost' port: aPort! !!OSCPort class methodsFor: 'defaults'!defaultIP
	"Answer the class var default"

	^SirenUtility defaultOSCHost! !!OSCPort class methodsFor: 'defaults'!defaultIP: value
	"Set the class var default"

	SirenUtility defaultOSCHost: value! !!OSCPort class methodsFor: 'defaults' stamp: 'stp 12/15/2022 14:14'!defaultPort
	"Answer the class var default"

	^SirenUtility defaultOSCPort! !!OSCPort class methodsFor: 'defaults'!defaultPort: value
	"Set the class var default"

	SirenUtility defaultOSCPort: value! !!OSCPort class methodsFor: 'defaults' stamp: 'stp 1/18/2023 07:27'!defaultSCOSCPort
	"Answer the default port for OSC to SuperCollider"

	^ 57123! !!OSCPort class methodsFor: 'examples'!demo
	"OSCPort demo"

	self sendStart.
	self sendFreq.
	self sendStop.
	^self! !!OSCPort class methodsFor: 'examples'!demo2
	"OSCPort demo2"

	self scHost send: TypedOSCMessage start.
	self scHost send: TypedOSCMessage changeFreq.
	self scHost send: TypedOSCMessage scStop.
	^self! !!OSCPort class methodsFor: 'examples'!sendFreq
	"PSCPort sendFreq"

	self localhost send: TypedOSCMessage changeFreq.
	^self! !!OSCPort class methodsFor: 'examples'!sendStart
	"OSCPort sendstop"

	self localhost send: TypedOSCMessage start.
	^self! !!OSCPort class methodsFor: 'examples'!sendStop
	"Send a stop message"

	self localhost send: TypedOSCMessage scStop.
	^self! !NotelistVoice subclass: #OSCVoice	instanceVariableNames: 'port range'	classVariableNames: 'Verbose'	poolDictionaries: ''	category: 'MusicIO-OSC'!!OSCVoice commentStamp: '<historical>' prior: 0!An OSCVoice can play events out to an OSC server using a predefined parameter mapping block to generate the OSC message.

Instance Variables:
	port	<OSCPort>  my output port
!!OSCVoice methodsFor: 'events'!oscMessageFrom: event 
	"Answer a message for the given event by passing it to my parameter-mapper block"

	^parameterMap value: event! !!OSCVoice methodsFor: 'events'!play: anEL 
	"Expand the list and play it now."

	self play: anEL at: Time microsecondClock! !!OSCVoice methodsFor: 'events' stamp: 'stp 12/15/2022 14:12'!playAssociation: ass in: list 
	"Play the association..."

	| realTime msg |
	realTime := ass key asMsec value.
	self waitTill: realTime.
	msg := parameterMap value: ass value.
	port send: msg.
	^self
"
	stream := OrderedCollection new.
	parameterMap do: [ :item | self
			mapProperty: item
			of: ass
			within: list].
	stream size = 0 ifTrue: [^self].
	stream size = 1 ifTrue: [^port send: (stream at: 1)].
"! !!OSCVoice methodsFor: 'events'!playEvent: event at: start 
	"Expand the list and play it at the given time."

	start ifNotNil:
		[self waitTill: start].
	event hasItems ifTrue: [^self play: event at: start].
	port send: (self oscMessageFrom: event)! !!OSCVoice methodsFor: 'events'!waitTill: time

	| now |
	time ifNotNil: 
			[now := Time microsecondClock.
			time > now
				ifTrue: [(Delay forMicroseconds: time - now) wait]
				ifFalse: [nil]].
	^self! !!OSCVoice methodsFor: 'utilities' stamp: 'stp 2/4/2023 16:13'!send: command args: args
	"Send an OSC command with the given arguments"
	"OSCVoice new send: '/i68/fi' args: '/Users/stp/Code/Audio/Synths/SoundFonts/male.sf2' "	
	port send: (TypedOSCMessage for: command with: args)! !!OSCVoice methodsFor: 'accessing'!port
	"Return the OSCPort"

	^port! !!OSCVoice methodsFor: 'accessing'!port: aPort 
	"Set the OSCPort"

	port := aPort.
	^self! !!OSCVoice methodsFor: 'accessing' stamp: 'stp 12/15/2022 14:04'!range: anInterval
	"Set the voices instrument # range."

	range := anInterval
! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OSCVoice class	instanceVariableNames: ''!!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:15'!bellExample1
	"Play notes on CSL FM bell instruments"
	"OSCVoice bellExample1"
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL FM bell has args: dur, ampl, pitch, gliss, rat, ind, pos"
	
	| evList voice |						"create the event list"
	evList := (EventList randomExample: 20
		from: ((#duration: -> 1.0),
				(#ampl: -> (0.25 to: 0.5)), 
				(#pitch: -> (40.1 to: 300.1)),
				(#gliss: -> (0.2 to: 2.0)),
				(#ratio: -> (0.8 to: 1.5)),
				(#ind: -> (1.0 to: 4.0)),
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn').
		ev duration: ev duration * 4].
	voice := self parameterMap: (self pMapForCSLFMBell: (21 to: 30))."plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"														"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 15:03'!bellExample2
	"Play notes on CSL FM bell instruments"
	"OSCVoice bellExample2"
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL FM bell has args: dur, ampl, pitch, gliss, rat, ind, pos"
	
	| evList voice |						"create the event list"
	evList := (EventList randomExample: 20
		from: ((#duration: -> 6.0),
				(#ampl: -> (0.15 to: 0.3)), 
				(#pitch: -> (40.1 to: 100.1)),
				(#gliss: -> (0.2 to: 2.0)),
				(#ratio: -> (0.8 to: 1.5)),
				(#ind: -> (1.0 to: 8.0)),
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn').
		ev duration: ev duration * 4].
	voice := self parameterMap: (self pMapForCSLFMBell: (21 to: 30))."plug in the parameter map"
	evList voice: voice.
	evList play.
! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:15'!fmExample1
	"Play a few random notes on the CSL FM instrument"
	"OSCVoice fmExample1"
	"EventScheduler interrupt; flush"
	"Startup the CSL OSC demo (or dumpOSC) and run these"
	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos, att, dec, sus, rel, i_att, i_dec, i_sus, i_rel"

	| evList voice  |
	evList := (EventList randomExample: 15 			"create the event list"
		from: ((#duration: -> (0.15 to: 0.3)), 
			(#pitch: -> (110.1 to: 220.1)), 
			(#ratio: -> (0.8 to: 6.0)),
			(#ampl: -> (0.1 to: 0.4)), 
			(#modIndex: -> (0.0 to: 4.0)), 
			(#pos: -> (-0.7 to: 0.7)),
			(#att: -> (0.03 to: 0.05)),
			(#dec: -> (0.03 to: 0.05)),
			(#sus: -> (0.5 to: 0.7)),
			(#rel: -> (0.05 to: 0.06)),
			(#iatt: -> (0.05 to: 0.1)),
			(#idec: -> 0.005),
			(#isus: -> (0.3 to: 0.9)),
			(#irel: -> 0.1),
			(#inst: -> '/i1/pn'))).	1 to: evList events size do: 		[ :ind |											"plug in instruments"		((evList events at: ind) event) inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
	voice := self parameterMap: (self pMapForCSLFM: (11 to: 20)).	"plug in the parameter map"
	evList voice: voice.
	evList play.! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 14:11'!fmExample2
	"Play molto legato notes on 4 CSL FM instruments and loop until interrupted "
	"OSCVoice fmExample2"		"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos"

	| evList voice |						"create the event list"
	evList := (EventList randomExample: 10
		from: ((#duration: -> 1.0),
				(#pitch: -> (40.1 to: 100.1)),
				(#ratio: -> (0.8 to: 1.5)),
				(#ampl: -> (0.25 to: 0.5)), 
				(#modIndex: -> (1.0 to: 4.0)),
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |			"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 8 + 1) printString, '/pn').
		ev duration: ev duration * 4].
	voice := self parameterMap: (self pMapForCSLSimpleFM: (11 to: 20)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:15'!fmExample3		"UNTESTED"
	"Play a long CSL FM note and apply some real-time control functions to it"
	"OSCVoice fmExample3"
	"Startup the CSL OSC demo and run these"
	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos"

	| evList voice voice2 gliss map2 pan map3 voice3 |
	evList := ((EventList newNamed: #LongNote) add:
		(10.0 sec, 66.0 Hz, -3 dB, (#pos -> -1.0), (#ratio -> 1.17), 
				(#modIndex -> 2.64), (#inst -> '/i1/pn'))).
	voice := self default.
	voice parameterMap: (self pMapForCSLSimpleFM: (11 to: 20)).	"plug in the parameter map"
	evList voice: voice.
	evList play.										"start the note"

	2 wait.			"pause 2 sec"

"Now make a glissando down"
	gliss := FunctionEvent new function: (LinearFunction from: #((0 66.0) (8 30.0))).
	gliss interval: 0.1 sec.						"10 per sec"
	map2 :=  [ :e | TypedOSCMessage for: '/i1/cf' with: (Array with: e value)].
	voice2 := self default.
	voice2 parameterMap: map2.					"plug in the parameter map"
	gliss voice: voice2.
	gliss play.

"and pan it"
	pan := FunctionEvent new function: (LinearFunction from: #((0 -1.0) (8 1.0))).
	pan interval: 0.1 sec.						"10 per sec"
	map3 :=  [ :e | TypedOSCMessage for: '/i1/po' with: (Array with: e value)].
	voice3 := self default.
	voice3 parameterMap: map3.					"plug in the parameter map"
	pan voice: voice3.
	pan play.! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:15'!fmExample3a
	"Play a long CSL FM note and apply some real-time control functions to it"
	"OSCVoice fmExample3a"
	"Startup the CSL OSC demo and run these"
	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos"

	| evList voice |
	evList := ((EventList newNamed: #LongNote) add:
			(10.0 sec, 66.0 Hz, -3 dB, 			(#pos -> 0.0),		 	(#ratio -> 1.17), 
			(#modIndex -> 2.0), 			(#inst -> '/i1/pn'))).														"plug in the parameter map"
	voice := self parameterMap: (self pMapForCSLSimpleFM: (11 to: 20)).
	evList voice: voice.	
	evList play.										"start the note"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/8/2023 10:32'!fmExample4
	"Play an FM bell using CSL; fork a block that plays a note and waits a bit; 
		stop the scheduler to stop the block."
	"OSCVoice fmExample4"
	"EventScheduler interrupt; flush"
	

	| evList voice rand ind |
	rand := Random new.
	EventScheduler run.
	ind := 1.
	[[EventScheduler isRunning] whileTrue:		"repeat block to play a bell sound"
		[evList := ((EventList new) add:
		(3.0 sec, (rand nextFrom: 1000.0 to: 1200.0) Hz , -24 dB, 
				(#pos -> (rand nextFrom: -1.0 to: 1.0)), 
				(#ratio -> 1.414), 
				(#modIndex -> 2.0), 
				(#inst -> ('/i', ind printString, '/pn')))).
		voice := OSCVoice onPort: (OSCPort to: #[127 0 0 1] port: 54321).
		voice parameterMap: (self pMapForCSLSimpleFM: (11 to: 20)).	"plug in p-map"
		evList voice: voice.
		evList play.
		ind := ind + 1.		ind > 9 ifTrue: [ind := 1].
		(rand nextFrom: 3 to: 6) wait]] forkAt: (Processor userBackgroundPriority - 1)! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:27'!functionExample "BROKEN"
	"Play a function out to OSC"
	"OSCVoice functionExample"

	| event voice map |							"create the function and event"
	event := FunctionEvent new function: (LinearFunction default scaleBy: 5@1).
	event interval: 0.25 sec.						"4 per sec"
	map :=  [ :e | TypedOSCMessage for: '/osc/1/ampl' with: (Array with: e value)].
	voice := self default.
	voice parameterMap: map.					"plug in the parameter map"
	event voice: voice.
	event play! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 13:57'!granulatorExample1
	"Play on the CSL granulator1 instrument - short bursts of garbled speak'n'spell "
	"OSCVoice granulatorExample1"		"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice |	
	voice := self parameterMap: (self pMapForCSLGranulator: (62 to: 63)).	"plug in the parameter map"
	evList := (20 sec, -66 dB, (#voice -> voice)) asEventList.						"create the event list with 1 note"	evList play.
! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:16'!granulatorExample2
	"Play on the CSL granulator2 instrument until interrupted - long low drones"
	"OSCVoice granulatorExample2"		"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice |	
	evList := (30 sec, -12 dB) asEventList.								"create the event list with 1 note"	
	voice := self onPort: OSCPort default.								"edit this to taste."
	voice parameterMap: (self pMapForCSLGranulator: (64 to: 67)).	"plug in the parameter map"
	evList voice: voice.	
	evList play.
"	evList loop"															"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 11:54'!granulatorExample2: index
	"Play on the CSL granulator2 instrument"
	"OSCVoice granulatorExample2: 1"	"OSCVoice granulatorExample2: 2"	"OSCVoice granulatorExample2: 3"
	"Startup the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice osc |		osc := '/i', index printString, '/pn'.
	evList := (30 sec, -12 dB, (#inst -> osc)) asEventList.					"create the event list with 1 note"	
	voice := self onPort: OSCPort default.								"edit this to taste."
	voice parameterMap: (self pMapForCSLGranulator: (64 to: 67)).	"plug in the parameter map"
	evList voice: voice.	
	evList play.
! !!OSCVoice class methodsFor: 'examples'!midiScaleExample
	"OSCVoice midiScaleExample"

	 | list voice |
	list := EventList scaleFrom: 48 to: 60 in: 2.5.
	voice := self default.
	list voice: voice.
	list play.! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/8/2023 10:36'!pluckExample1
	"Play notes on 10 CSL plucked string instruments - high fast notes"
	"OSCVoice pluckExample1"
	"EventScheduler interrupt; flush"
	"Startup the CSL OSC demo and run these"
	"CSL plucked string has args: ampl, freq, pos"

	| evList voice |												"create the event list"
	evList := (EventList randomExample: 120
		from: ((#duration: -> 0.07),
				(#ampl: -> (0.25 to: 0.5)), 
				(#pitch: -> (80.0 to: 600.0)), 
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |											"plug in instruments"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"												"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 11:38'!pluckExample1: amplInterval
	"Play notes on 10 CSL plucked string instruments - high fast notes"
	"OSCVoice pluckExample1: (0.25 to: 0.4)"
	"EventScheduler interrupt; flush"
	"Startup the CSL OSC demo and run these"
	"CSL plucked string has args: ampl, freq, pos"

	| evList voice |												"create the event list"
	evList := (EventList randomExample: 120
		from: ((#duration: -> 0.07),
				(#ampl: -> amplInterval), 
				(#pitch: -> (80.0 to: 600.0)), 
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |											"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.
	evList play.	^ evList duration.! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:16'!pluckExample2
	"Play notes on 10 CSL plucked string instruments - slow low notes"
	"OSCVoice pluckExample2"
	"EventScheduler interrupt; flush"
	"Startup the CSL OSC demo and run these"
	"CSL plucked string has args: ampl, freq, pos"

	| evList voice |												"create the event list"
	evList := (EventList randomExample: 12
		from: ((#duration: -> 3.0),
				(#ampl: -> (0.25 to: 0.5)), 
				(#pitch: -> (40.0 to: 100.0)), 
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |												"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
																	"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"														"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/8/2023 11:22'!samplerExample1
	"Play notes on 4 different CSL SoundFont sampler instruments"
	"OSCVoice samplerExample1"
	"Startup the CSL OSC demo and run these"
	"CSL sampler has args: dur, chan, key, ampl, pos"
	
	| evList voice |						"create the event list"
	evList := (EventList randomExample: 24
		from: ((#duration: -> 0.33),				(#pitch: -> (80 to: 800.1)),
				(#ampl: -> (0.7 to: 0.9)),				(#pos: -> (-1.0 to: 1.0)))).
	voice := self onPort: OSCPort default.								"edit this to taste."	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
	evList voice: voice.	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'vibra.sf2').	voice send: '/i69/fi' args: (Array with: SirenUtility cslDir, 'organ.sf2').	voice send: '/i70/fi' args: (Array with: SirenUtility cslDir, 'Tympani.sf2').	voice send: '/i71/fi' args: (Array with: SirenUtility cslDir, 'male.sf2').	1 to: evList events size do: 		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"		ev := (evList events at: ind) event.		ev inst: ('/i', (ind - 1 \\ 4 + 1) printString, '/pn').		ev duration: ev duration * 2].	evList play.	
	(Delay forSeconds: evList duration asSeconds value + 2) wait.	voice send: '/i68/ano' args: (Array with: 1).	voice send: '/i69/ano' args: (Array with: 1).	voice send: '/i70/ano' args: (Array with: 1).! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 11:06'!samplerExample2
	"Play notes on a CSL SoundFont GM sampler instrument"
	"OSCVoice samplerExample2"
	"Startup the CSL OSC demo and run these"
	"CSL sampler has args: dur, chan, key, ampl, pos"
	
	| evList voice |						"create the event list"
	evList := (EventList randomExample: 24
		from: ((#duration: -> 0.3),				(#channel: -> (1 to: 120)),				(#pitch: -> (80 to: 800.1)),
				(#ampl: -> (0.6 to: 0.9)),				(#pos: -> (-1.0 to: 1.0)))).
	voice := self onPort: OSCPort default.								"edit this to taste."	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
	evList voice: voice.	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList eventsDo: [ :ev |		ev inst: ('/i1/pn').		ev duration: ev duration * 2].	evList play.	
	(Delay forSeconds: evList duration asSeconds value + 2) wait.	voice send: '/i68/ano' args: (Array with: 1).	! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/8/2023 11:27'!samplerExample3
	"Play a Bach chorale on a CSL SoundFont GM string instrument"
	"OSCVoice samplerExample3"
	"Startup the CSL OSC demo and run these"
	"CSL sampler has args: dur, chan, key, ampl, pos"
	
	| evList voice |	
	evList := Score11 chorale1.	evList scaleTimesBy: 6.0.
	voice := self onPort: OSCPort default.								"edit this to taste."	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
	evList voice: voice.	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList eventsDo: [ :ev |		ev inst: ('/i1/pn').		ev channel: 17].	evList play.	
	(Delay forSeconds: evList duration asSeconds value + 2) wait.	voice send: '/i68/ano' args: (Array with: 1).! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 11:06'!samplerExample3Menu
	"Play a Bach chorale on a CSL SoundFont GM string instrument"
	"OSCVoice samplerExample3Menu"
	"Startup the CSL OSC demo and run these"
	"CSL sampler has args: dur, chan, key, ampl, pos"
	
	| evList voice inst |
	inst := MIDIPort generalMIDIMenu.	evList := Score11 chorale1.	evList scaleTimesBy: 4.0.
	voice := self onPort: OSCPort default.								"edit this to taste."	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
	evList voice: voice.	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList eventsDo: [ :ev |		ev inst: ('/i1/pn').		ev channel: inst].	evList play.	
	(Delay forSeconds: evList duration asSeconds value + 2) wait.	voice send: '/i68/ano' args: (Array with: 1).! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/6/2023 11:07'!samplerExample4
	"Play a Bach chorale on a CSL SoundFont GM string instrument"
	"OSCVoice samplerExample4"
	"Startup the CSL OSC demo and run these"
	"CSL sampler has args: dur, chan, key, ampl, pos"
	
	| evList voice |	
	evList := Score11 chorale1.
	evList scaleTimesBy: 4.0.	voice := self onPort: OSCPort default.								"edit this to taste."	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
	evList voice: voice."	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'all-strings-SEC-tremolo-panned.sfz')."	voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'all-woodwinds-SEC-sustain-panned.sfz').	evList eventsDo: [ :ev |		ev inst: ('/i1/pn').		ev channel: 1].	evList play.	
	(Delay forSeconds: evList duration asSeconds value + 1) wait.	voice send: '/i68/ano' args: (Array with: 1).! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:45'!scaleExample	"Play a scale on the CSL plucked string voice."
	"OSCVoice scaleExample"

	 | num list voice |
	list := EventList scaleFrom: 48 to: 60 in: 2.0.	num := list events size.	1 to: num do: 		[ :ind | | ev |											"plug in missing parameters"		ev := (list events at: ind) event.		ev ampl: 0.5.											"amplitude"		ev pos:  (ind / num * 2 - 1.0).							"stereo pan L to R"		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].	"cycle through the 10 plucked string instruments"	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"	list voice: voice.
	list play													"...and play"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sharcExample1
	"Play short notes on the 16 SHARC instruments (version w vibrato)"
	"OSCVoice sharcExample1"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL SHARC instrument has args: dur, amp, pitch, pos, att, dec, sus, rel, chiff"

	| evList voice |						"create the event list"
	evList := (EventList randomExample: 20
		from: ((#duration: -> 0.25),
				(#pitch: -> (110.1 to: 220.1)),
				(#ampl: -> (0.15 to: 0.2)),
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 16 + 1) printString, '/pn').
		ev duration: ev duration * 4].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLSHARC1: (35 to: 50)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"								"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sharcExample2
	"Play long low notes on the 16 SHARC instruments"
	"OSCVoice sharcExample2"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL SHARC instrument has args: dur, amp, pitch, pos, att, dec, sus, rel, chiff"

	| evList voice |						"create the event list"
	evList := (EventList randomExample: 8
		from: ((#duration: -> 2.5),
				(#pitch: -> (40.1 to: 88.1)),
				(#ampl: -> (0.1 to: 0.2)),
				(#pos: -> (-1.0 to: 1.0)), 
				(#att: -> (0.3 to: 0.5)),
				(#dec: -> (0.3 to: 0.5)),
				(#sus: -> (0.7 to: 1.0)),
				(#rel: -> (0.5 to: 0.6)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 16 + 1) printString, '/pn').
		ev duration: ev duration * 4].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.

	voice := self parameterMap: (self pMapForCSLSHARC2: (31 to: 46)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"								"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sharcExample3
	"Play long notes on the 16 SHARC instruments"
	"OSCVoice sharcExample3"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	

	| evt voice |						"create the event"
	evt := (4.0 sec, 50.1 Hz, -8 dB, (#pos -> 0.0), (#inst -> '/i1/pn')).
	voice := self parameterMap: (self pMapForCSLSHARC1: (35 to: 50)).	"plug in the parameter map"
	evt voice: voice.
	evt play.
! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sharcExample4
	"Play long low notes on the 16 SHARC instruments"
	"OSCVoice sharcExample2"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL SHARC instrument has args: dur, amp, pitch, pos, att, dec, sus, rel, chiff"

	| evList voice |						"create the event list"
	evList := (EventList randomExample: 8
		from: ((#duration: -> 2.5),
				(#pitch: -> (40.1 to: 88.1)),
				(#ampl: -> (0.1 to: 0.2)),
				(#pos: -> (-1.0 to: 1.0)), 
				(#att: -> (0.3 to: 0.5)),
				(#dec: -> (0.3 to: 0.5)),
				(#sus: -> (0.7 to: 1.0)),
				(#rel: -> (0.5 to: 0.6)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 16 + 1) printString, '/pn').
		ev duration: ev duration * 4].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.

	voice := self parameterMap: (self pMapForCSLSHARC2: (56 to: 61)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"								"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sndExample1
	"Play random notes on the CSL sound file instruments"
	"OSCVoice sndExample1"
	"Startup the CSL OSC demo and run these"
	"CSL SndFileInstrument has args: ampl, pos, start, stop"

	| events voice rand |
	rand := Random new.
	events := EventList new. "named: EventList nextName."
	25 timesRepeat:
		[ | in |
		in := rand nextIntFrom: 1 to: 4.
		events add: (MusicEvent new duration: (rand nextFrom: 0.1 to: 0.2); 
				inst: ('/i', in printString, '/pn');
				ampl: (rand nextFrom: 0.05 to: 0.5); 
				pos: (rand nextFrom: -1.0 to: 1.0) ; 
				start: (rand nextFrom: 0.0 to: 0.5); 
				stop: (rand nextFrom: 0.5 to: 1.0))].
	voice := self parameterMap: (self pMapForCSLSnd: (31 to: 34)).	"plug in the parameter map"
													"load 4 sound files"
"	voice send: '/i5/fi' args: (Array with: (SirenUtility findFile: 'moon.snd')).
	voice send: '/i6/fi' args: (Array with: (SirenUtility findFile: 'wet.snd')).
	voice send: '/i7/fi' args: (Array with: (SirenUtility findFile: 'round.snd')).
	voice send: '/i8/fi' args: (Array with: (SirenUtility findFile: 'shine.snd')).
"	events voice: voice.
	events play! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sndExample2
	"Play random notes on the CSL sound file instruments; loop until interrupted"
	"OSCVoice sndExample2"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL SndFileInstrument has args: ampl, pos, start, stop"
													"load voice files into instruments 5-8"
"	voice send: '/i5/fi' args: (Array with: (SirenUtility findFile: 'moon.snd')).
	voice send: '/i6/fi' args: (Array with: (SirenUtility findFile: 'wet.snd')).
	voice send: '/i7/fi' args: (Array with: (SirenUtility findFile: 'round.snd')).
	voice send: '/i8/fi' args: (Array with: (SirenUtility findFile: 'shine.snd')).
"
	| events voice rand |
	rand := Random new.
	events := EventList new. "named: EventList nextName."
	50 timesRepeat:
		[ | in |
		in := rand nextIntFrom: 1 to: 4.
		events add: (MusicEvent new duration: (rand nextFrom: 0.05 to: 0.15); 
				inst: ('/i', in printString, '/pn');
				ampl: (rand nextFrom: 0.1 to: 0.5); 
				pos: (rand nextFrom: -1.0 to: 1.0)).
		(rand next > 0.75) 							"add 25% rests"
			ifTrue: [events add: (DurationEvent new duration: rand next sec)]].

	voice := self  parameterMap: (self pMapForCSLSnd: (31 to: 34)).	"plug in the parameter map"
	events voice: voice.							"set-up and loop list"
	events play.
"	events loop"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!sosExample1		"UNTESTED"
	"Play random notes on the CSL sum-of-sines instruments; loop until interrupted"
	"OSCVoice sosExample1"	
	"EventScheduler flush"
	"Startup the CSL/OSC additive and run these"
	"CSL AdditiveInstrument has args: dur, ampl, freq, pos, att, dec, sus, rel"

	| events voice rand instr notes pitches freq0 key spect |
	rand := Random new.
	instr := nil. "SHARCInstrument fromDir: 'tuba'."			"load a tuba sample"
	notes := Dictionary new.
	instr samples associationsDo:						"make a dict of sample freq -> SOS-array"
		[ :ass |
		notes at: (ass key pitch asHz value) put: (ass value asSumOfSines)].
	pitches := notes keys asSortedCollection. "get the pitches in Hz"
	freq0 := pitches removeFirst.
	pitches := (pitches collect: [ :pt | | int | 				"make a list of freq-interval -> base-freq"
			int := freq0 to: pt. 
			freq0 := pt. 
			int]) asSortedCollection: [ :x :y | x first <= y first ].
	events := EventList EventList newNamed.		"temp event list"
	1 to: 25 do:										"add 25 notes + some rests"
		[ :counter | | pch |
		pch := rand nextFrom: 80.0 to: 440.0.
			key := pitches detect: [ :pt | pt includes: pch] ifNone: [nil].
			key ifNil: [pch < pitches first first
				ifTrue: [spect := notes at: pitches first first]
				ifFalse: [pch > pitches last last
					 ifTrue: [spect := notes at: pitches last last]]]
			ifNotNil: [spect := notes at: key first].
		events add: (MusicEvent new 
				duration: (rand nextFrom: 0.1 to: 0.2); 
				inst: ('/i', (counter \\ 16 + 1) printString);
				ampl: (rand nextFrom: 0.01 to: 0.3); 
				pitch: pch; 
				pos: (rand nextFrom: -1.0 to: 1.0);
				spectrum: spect).
		(rand next > 0.8) 							"add 20% rests"
			ifTrue: [events add: (DurationEvent new duration: (rand next / 4) sec)]].

	voice := self default.
	voice parameterMap: self pMapForCSLAdditive.	"plug in the parameter map"
													"load voice files into instruments 5-8"
	events voice: voice.							"set-up and loop list"
	events play! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!vSOSExample1
	"Play random notes on the CSL simple vector SHARC instruments"
	"OSCVoice vSOSExample1"	
	"EventScheduler flush"
	"Startup the CSL/OSC additive and run these"
	"CSL AdditiveInstrument has args: dur, ampl, freq, pos, [att, dec, sus, rel]"

	| evList voice |						"create the event list"
	evList := EventList randomExample: 8
		from: ((#duration: -> 2.5),
				(#pitch: -> (44.1 to: 88.1)),
				(#ampl: -> (0.2 to: 0.3)),
				(#pos: -> (-1.0 to: 1.0))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 5 + 1) printString, '/pn').
		ev duration: ev duration * 4].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLVectorSHARC: (51 to: 55)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!vSOSExample2
	"Play long low notes on the fancy vector SOS instruments"
	"OSCVoice vSOSExample2"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"
	"CSL  instrument has args: dur, amp, pitch, pos, (att, dec, sus, rel)"

	| evList voice |						"create the event list"
	evList := (EventList randomExample: 10
		from: ((#duration: -> 2.5),
				(#pitch: -> (40.1 to: 88.1)),
				(#ampl: -> (0.1 to: 0.2)),
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |						"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 5 + 1) printString, '/pn').
		ev duration: ev duration * 4].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.

	voice := self parameterMap: (self pMapForCSLVectorSHARC: (56 to: 61)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"								"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 2/5/2023 22:17'!vSOSExample3
	"Play a long note on the SHARC VSOS instrument"
	"OSCVoice vSOSExample3"		
	"EventScheduler flush"
	"Startup the CSL OSC demo and run these"

	| evt voice |						"create the event"
	evt := (4.0 sec, 50.1 Hz, -8 dB, (#pos -> 0.0), (#inst -> '/i3/pn')).
	voice := self parameterMap: (self pMapForCSLSHARC1: (56 to: 61)).	"plug in the parameter map"
	evt voice: voice.
	evt play.
! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 12/12/2022 07:23'!cslHost
	"Answer the default voice for CSL over OSC"

	^self onPort: OSCPort cslHost! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 12/12/2022 07:35'!cslHost: mapSelector
	"Answer the default voice for CSL over OSC using the given map selector."
	"| list voice |
	list := EventList scaleFrom: 48 to: 60 in: 2.5.
	voice := self cslHost: pMapForCSLSimpleFM.
	list voice: voice.
	list play."
	
	| inst |
	inst := self onPort: OSCPort cslHost.
	inst parameterMap: (self perform: mapSelector).
	^inst! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 12/20/2022 07:18'!default
	"Answer an OSC voice with the default OSC host and port."

	^self onPort: OSCPort default! !!OSCVoice class methodsFor: 'instance creation'!localhost
	"Answer an instance on the local host"

	^self onPort: OSCPort localhost! !!OSCVoice class methodsFor: 'instance creation'!map: mapSelector
	"Answer a default instance that uses the given parameter map name"
	"OSCVoice map: #pMapForCSLSimpleFM"

	| inst |
	inst := self default.
	inst parameterMap: (self perform: mapSelector).
	^inst! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 18:58'!map: mapSelector range: rngIntrvl
	"Answer a default instance that uses the given parameter map name"
	"OSCVoice map: #pMapForCSLSimpleFM: range:  (1 to: 9)"

	| inst |
	inst := self default.
	inst parameterMap: (self perform: mapSelector with: rngIntrvl).
	^inst! !!OSCVoice class methodsFor: 'instance creation'!onPort: pt
	"Answer an instance on the given port"

	| me |
	(me := self new initialize) port: pt.
	^me! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 2/8/2023 11:09'!pMap: map range: range	"Answer an OSCVoice using the gicven pmap and range"	"OSCVoice pMap: #pMapForCSLPluck: range: (1 to: 10)"
	^ self parameterMap: (self perform: map with: range)! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 12/22/2022 13:42'!parameterMap: pmap
	"Answer an OSC voice for use with the default OSC settings and the given parameter map."
	"OSCVoice parameterMap: (OSCVoice pMapForCSLPluck: (1 to: 10))"
	
	| inst |
	inst := self default.
	inst parameterMap: pmap.
	^inst! !!OSCVoice class methodsFor: 'instance creation'!scHost
	"Answer the default voice for SC over OSC"

	^self onPort: OSCPort scHost! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 1/2/2023 12:43'!verbose	^ SirenUtility verbosity > 0! !!OSCVoice class methodsFor: 'instance creation' stamp: 'stp 1/22/2023 18:13'!verbose: aBool 	aBool	 	ifTrue: [SirenUtility verbosity: 1]		ifFalse: [SirenUtility verbosity: 0]! !!OSCVoice class methodsFor: 'class initialization' stamp: 'stp 12/30/2022 16:39'!initialize"Set up the class vars""OSCVoice initialize"Verbose := false! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/10/2023 10:35'!play4vSampler: evList
	"Play the given event list on 4 different CSL SoundFont sampler instruments"
	"OSCVoice play4vSampler: ((Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList) startingAt: 68"
	
	^ self play4vSampler: evList startingAt: 68! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/9/2023 12:38'!play4vSampler: evList startingAt: s1
	"Play the given event list on 4 different CSL SoundFont sampler instruments"
	"OSCVoice play4vSampler: ((Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList) startingAt: 68"
	
	|  voice |
	voice := self onPort: OSCPort default.		voice parameterMap: (self pMapForCSLSFSampler: (s1 to: s1 + 8)).	"plug in the parameter map"
		voice send: ('/i', s1 printString, '/fi') args: (Array with: SirenUtility cslDir, 'vibra.sf2').	voice send: ('/i', (s1 + 1) printString, '/fi') args: (Array with: SirenUtility cslDir, 'organ.sf2').	voice send: ('/i', (s1 + 2) printString, '/fi') args: (Array with: SirenUtility cslDir, 'male.sf2').	voice send: ('/i', (s1 + 3) printString, '/fi') args: (Array with: SirenUtility cslDir, 'Tympani.sf2').	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: ('/i', s1 printString, '/ano') args: (Array with: 1).		voice send: ('/i', (s1 + 1) printString, '/ano') args: (Array with: 1).		voice send:  ('/i', (s1 + 2) printString, '/ano') args: (Array with: 1).		voice send:  ('/i', (s1 + 3) printString, '/ano') args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/10/2023 09:11'!playCSLPlucks: evList
	"Play the given event list on the CSL plucked string instrument"
	"OSCVoice playCSLPlucks: (Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList"
	
	|  voice |
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.	evList play! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/6/2023 11:06'!playGMSampler: evList
	"Play the given event list on the CSL SoundFont GeneralMidi sampler"
	"OSCVoice play4vSampler: (Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList"
	
	|  voice |
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: '/i68/ano' args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/6/2023 11:06'!playGMSampler: evList chan: s1
	"Play the given event list on the CSL SoundFont GeneralMidi sampler"
	"OSCVoice playGMSampler: ((Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList) chan: 68"
	
	|  voice |
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: ('/i', s1 printString, '/fi') args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: '/i68/ano' args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/9/2023 12:39'!playGMSampler: evList startingAt: s1
	"Play the given event list on the CSL SoundFont GeneralMidi sampler"
	"OSCVoice playGMSampler: (Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList startingAt: 1"
	
	|  voice |
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: ('/i', s1 printString, '/fi') args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: '/i68/ano' args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/6/2023 11:06'!playGMSamplerInst: evList chan: s1
	"Play the given event list on the CSL SoundFont GeneralMidi sampler; present a menu of samples"
	"OSCVoice playGMSamplerInst: (DynamicSelectionCloud example2) chan: 68"
	
	|  inst voice |	inst := MIDIPort generalMIDIMenu.
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: ('/i', s1 printString, '/fi') args: (Array with: SirenUtility cslDir, 'Arachno SoundFont.sf2').	evList voice: voice.	evList eventsDo: [ :ev |		ev inst: ('/i1/pn').		ev channel: inst].	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: '/i68/ano' args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/9/2023 12:41'!playSFSampler: evList using: libName
	"Play the given event list on the CSL SoundFont sampler"
	"OSCVoice playSFSampler: ((Roll length: 6 sec rhythm: 60 note: 72) ampl: 100) eventList) using: libName"
	
	|  voice |		
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: '/i68/fi' args: (Array with: SirenUtility cslDir, libName).	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: '/i68/ano' args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'playing' stamp: 'stp 2/9/2023 20:41'!playSFSampler: evList using: libName chan: s1
	"Play the given event list on the CSL SoundFont sampler"
	"OSCVoice playSFSampler: ((DynamicSelectionCloud example2) 						scaleStartTimesBy: 4; scaleDurationsBy: 8; setValue: nil toProp: #voice)				using: 'all-woodwinds-SEC-sustain-panned.sfz' 				chan: 68"
	
	|  voice |		
	voice := self onPort: OSCPort default.	voice parameterMap: (self pMapForCSLSFSampler: (68 to: 75)).	"plug in the parameter map"
		voice send: ('/i', s1 printString, '/fi') args: (Array with: SirenUtility cslDir, libName).	evList voice: voice.	evList play.		[(Delay forSeconds: evList duration asSeconds value + 1) wait.		voice send: ('/i', s1 printString, '/ano') args: (Array with: 1)] fork! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 12/30/2022 16:24'!defaultPMap	"Answer the default parameterMap for use with OSC."	^ self defaultPMap: 11 args: 4! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 12/30/2022 17:07'!defaultPMap: instr args: nArgs 	"Answer the default parameterMap for use with OSC."	^ [:e | 	| arr msg |	nArgs = 3		ifTrue: [arr := Array						with: e duration asSec value						with: e ampl asRatio value						with: e pitch asHz value]		ifFalse: [nArgs = 4				ifTrue: [arr := Array								with: e duration asSec value								with: e ampl asRatio value								with: e pitch asHz value								with: 0.0]				ifFalse: [self error: 'No default OSC patameter map available']].	msg := TypedOSCMessage for: '/i' , instr printString , '/pn' with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 10:39'!pMapForCSLFM: range 	"Answer the default parameterMap for use with the CSL FM example	instrument. "	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos, att, dec,	sus, rel, i_att, i_dec, i_sus, i_rel"	^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 14.	"set up the parameter map"	arr at: 1 put: e duration asSec value.	arr at: 2 put: e ampl value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e pitch asHz value * e ratio.	arr at: 5 put: e modIndex.	arr at: 6 put: e pos.	arr at: 7 put: e att.	arr at: 8 put: e dec.	arr at: 9 put: e sus.	arr at: 10 put: e rel.	arr at: 11 put: e iatt.	arr at: 12 put: e idec.	arr at: 13 put: e isus.	arr at: 14 put: e irel.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/10/2023 11:55'!pMapForCSLFMBell: range 	"Answer the default parameterMap for use with the CSL FM bell instrument. "	"CSL FM bell has args: dur, ampl, pitch, gliss,rat, ind, pos"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.		"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 7.					"set up the parameter map"	arr at: 1 put: e duration asSec value.	arr at: 2 put: e ampl value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e gliss.	e ratio isNil			ifTrue: [arr at: 5 put: 1.414]			ifFalse: [arr at: 5 put: e ratio].	e ind isNil			ifTrue: [arr at: 6 put: 3.0]			ifFalse: [arr at: 6 put: e ind].	e pos isNil			ifTrue: [arr at: 7 put: 0.0]			ifFalse: [arr at: 7 put: e pos].	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 1/23/2023 17:17'!pMapForCSLGranulator: range 	"Answer the default parameterMap for use with the CSL granulator instrument. "	"Instrument has args: dur, ampl"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.							"fix instr num"	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.					"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 2.								"set up the parameter map"	arr at: 1 put: e duration asSec value asFloat.	arr at: 2 put: e ampl asRatio value asFloat.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString; cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 11:03'!pMapForCSLPluck: range 	"Answer the default parameterMap for use with the CSL plucked string example instrument."	"Evt is: ampl, pitch, pos"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 3.			"set up the parameter map"	arr at: 1 put: e ampl asRatio value.	arr at: 2 put: e pitch asHz value.	e pos ifNil: [arr at: 3 put: 0.0]		ifNotNil: [arr at: 3 put: e pos].	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose ifTrue: [Transcript show: msg printString; cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/5/2023 15:14'!pMapForCSLSFSampler: range 	"Answer the default parameterMap for use with the CSL SoundFont sampler instrument. "	"Instrument has args: dur, chan, key, ampl, pos - types fiiff"		^ [ :e | | arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.							"fix instr num"	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.					"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 5.								"set up the parameter map"	arr at: 1 put: e duration asSec value asFloat.	e channel ifNotNil: [arr at: 2 put: e channel]			ifNil: [e voice isInteger 				ifTrue: [arr at: 2 put: e voice]				ifFalse: [arr at: 2 put: 1]].	arr at: 3 put: e pitch asMIDI value.	arr at: 4 put: e ampl asRatio value asFloat.	e pos ifNotNil: [arr at: 5 put: e pos asFloat.]			ifNil: [arr at: 5 put: 0.0].	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString; cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/13/2023 00:43'!pMapForCSLSHARC1: range 	"Answer the default parameterMap for use with the CSL sum-of-sines	SHARC instrument.
		CSL SHARCInstrument (short version) args: dur,	amp, pitch, pos"	"OSCVoice sosExample1"	"EventScheduler flush"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 4.	arr at: 1 put: e duration asSec value.	"required fields"	arr at: 2 put: e ampl asRatio value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e pos.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 10:48'!pMapForCSLSHARC2: range 	"Answer the default parameterMap for use with the CSL sum-of-sines	SHARC instrument.
		CSL SHARCInstrument args: dur, amp, pitch, pos,	att, dec, sus, rel"	"OSCVoice sosExample2"	"EventScheduler flush"	^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 8.	arr at: 1 put: e duration asSec value.	"required fields"	arr at: 2 put: e ampl value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e pos.	arr at: 5 put: e att.	arr at: 6 put: e dec.	arr at: 7 put: e sus.	arr at: 8 put: e rel.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 10:48'!pMapForCSLSimpleFM: range 	"Answer the default parameterMap for use with the CSL FM example	instrument. "	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos"	^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 6.	"set up the parameter map"	arr at: 1 put: e duration asSec value.	arr at: 2 put: e ampl asRatio value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e pitch asHz value * e ratio.	arr at: 5 put: e modIndex.	arr at: 6 put: e pos.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 10:48'!pMapForCSLSnd: range 	"Answer the default parameterMap for use with the CSL FM example	instrument. "	"CSL SndFileInstrument has args: ampl, pos, start, stop"	^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 2.	"set up the parameter map"	arr at: 1 put: e ampl value.	arr at: 2 put: e pos.	"arr at: 3 put: e start.
	arr at: 4 put: e stop.
"	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString;				 cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 2/8/2023 10:49'!pMapForCSLVectorSHARC: range 	"Answer the default parameterMap for use with the CSL vector SHARC	instrument -- dur, ampl, pitch, pos"	"OSCVoice vSOSExample1"	"EventScheduler flush"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.	"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 4.	arr at: 1 put: e duration asSec value.		"required fields"	arr at: 2 put: e ampl value.	arr at: 3 put: e pitch asHz value.	arr at: 4 put: e pos.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString; cr].	msg]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 1/18/2023 07:25'!pMapForMIDI	"Answer the default parameterMap for use with OSC-to-MIDI. 
	In this	class, we create and return a OSCMessage with data taken from the	given event."	^ [ :e | 	| arr timestamp |	arr := Array				with: (e voice isInteger						ifTrue: [e voice]						ifFalse: [1])				with: e pitch asMIDI value				with: e ampl asMIDI value.	timestamp := Time now.	OSCBundle		with: (Array				with: (TypedOSCMessage for: '/osc/midi/out/noteOn' with: arr)				with: (OSCBundle						with: (Array								with: (TypedOSCMessage for: '/osc/midi/out/noteOff' with: arr))						at: (timestamp addMilliseconds: e duration asMsec value)))		at: timestamp]! !!OSCVoice class methodsFor: 'parameter maps'!pMapForMIDItoFM
	"Answer the default parameterMap for use with the CSL FM example instrument."
	"CSL FMInstrument has args: dur, ampl, pch, index, pos"

	^[ :e | | arr |
	arr := Array new: 6.				"set up the parameter map"
	arr at: 1 put: e duration asSec value.
	arr at: 2 put: e ampl asRatio value.
	arr at: 3 put: e pitch asHz value.
	arr at: 5 put: e modIndex.
	arr at: 6 put: e pos.
"	Transcript show: arr printString; cr."
	TypedOSCMessage for: e inst with: arr]! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 1/18/2023 07:26'!pMapWithBundle	"Answer the default parameterMap for use with OSC-to-MIDI. In this	class, we create and return a TypedOSCMessage with data taken from	the given event. This allows us to have other versions that create OSC	bundles. "	^ [ :e | 	| arr |	arr := Array				with: 1				with: e pitch asMIDI value				with: e ampl asMIDI value.	OSCBundle		with: (Array				with: (TypedOSCMessage for: '/osc/midi/out/noteOn' with: arr)				with: (OSCBundle						with: (TypedOSCMessage for: '/osc/midi/out/noteOff' with: arr)						at: (Time now addMilliseconds: e duration asMsec value)))]! !OSCMessage subclass: #TypedOSCMessage	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-OSC'!!TypedOSCMessage commentStamp: '<historical>' prior: 0!A TypedOSCMessage represents the new form of (typed) OSC messages!!TypedOSCMessage methodsFor: 'osc'!argumentsToOSCBytes: convertor 
	"Convert the args and type string to OSC format"

	convertor nextPut: $, asInteger.
	self typesToOSC: convertor.
	super argumentsToOSCBytes: convertor.
	^self! !!TypedOSCMessage methodsFor: 'osc' stamp: 'stp 2/4/2023 16:57'!typesToOSC: converter 
	"Convert the arguments to an OSC type string."

	| t2 t3 |
	arguments isEmpty ifTrue: [ ^ self].
	arguments do: [ :t4 | t4 toOSCTypeOn: converter].
	t2 := 1.
	t3 := 4 - (converter position \\ 4).
	[t2 <= t3] whileTrue: 
			[converter nextPut: 0.
			t2 := t2 + 1].
	^ self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!TypedOSCMessage class	instanceVariableNames: ''!!TypedOSCMessage class methodsFor: 'examples'!changeFreq
	"TypedOSCMessage changeFreq"

	^self for: '/freq' with: #(440)! !!TypedOSCMessage class methodsFor: 'examples'!scDecreaseVolume
	"TypedOSCMessage scDecreaseVolume"

	^self for: '/sc/mixer/volume' with: #(1 0.2)! !!TypedOSCMessage class methodsFor: 'examples'!scIncreaseVolume
	"TypedOSCMessage scIncreaseVolume"

	^self for: '/sc/mixer/volume' with: #(1 1.0)! !!TypedOSCMessage class methodsFor: 'examples'!scRun
	"TypedOSCMessage scRun"

	^self for: '/sc/run'! !!TypedOSCMessage class methodsFor: 'examples'!scStop
	"TypedOSCMessage scStop"

	^self for: '/sc/stop'! !!TypedOSCMessage class methodsFor: 'examples'!start
	"TypedOSCMessage start"

	^self for: '/start'! !OSCBundle initialize!OSCVoice initialize!