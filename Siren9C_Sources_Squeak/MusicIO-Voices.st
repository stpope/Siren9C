Model subclass: #Voice	instanceVariableNames: 'name instrument stream'	classVariableNames: 'DefaultVoiceClass'	poolDictionaries: ''	category: 'MusicIO-Voices'!!Voice commentStamp: '<historical>' prior: 0!Subclasses of Voice implement the mappings between symbolic event parameters (like timbre descriptions and pitch names) and concrete output event data.
They can be used for generating sound compiler notelists or (real-time output or file dumps of) MIDI data.

The instance variables are:
	instrument <Symbol, association or device> my instrument
	name 		<String or Symbol> my handle or name
	stream		<Stream> my (optional) I/O stream

The global object Voices is a dictionary that note events can refer to with integer or symbolic keys.
!!Voice methodsFor: 'accessing' stamp: 'stp 1/10/2023 11:10'!instrument	"return my instrument."		^ instrument! !!Voice methodsFor: 'accessing'!instrument: newValue
	"set my instrument."

	instrument := newValue! !!Voice methodsFor: 'accessing'!stream
	"answer the stream of the receiver voice"

	^stream! !!Voice methodsFor: 'accessing'!stream: someStream
	"plug in a stream to the receiver voice"

	stream := someStream! !!Voice methodsFor: 'events'!eventList
	"Answer an event list read from the receiver."

	| e |
	e := EventList newAnonymous.
	self readOnto: e.
	^e! !!Voice methodsFor: 'events'!play: anEventList
	"Expand the list and play it now."

	^self play: anEventList at: Time microsecondClock! !!Voice methodsFor: 'events'!play: anEventOrList at: start
	"Expand the list and play it at the given time."

	| t e |
	anEventOrList hasItems
		ifFalse: [^self playEvent: anEventOrList at: start].
	anEventOrList events do:
		[ :ass |
		t := ass key asMsec.
		e := ass value.
		e hasItems 
			ifTrue: [self play: e at: t + start]
			ifFalse: [self playEvent: e at: t + start]]! !!Voice methodsFor: 'events'!playEvent: anEvent
	"Play the argument now."

	self playEvent: anEvent at: 0! !!Voice methodsFor: 'events'!playEvent: anEvent at: aTime
	"make sure my subclasses implement this"

	self subclassResponsibility! !!Voice methodsFor: 'events'!readOnto: eventList
	"Read events from the receiver into the argument."

	self subclassResponsibility! !!Voice methodsFor: 'printing'!printOn: aStream
	"Print the receivcer on the argument."

	aStream nextPutAll: 'a ', self class name! !!Voice methodsFor: 'testing'!isVoice
	"Answer true"

	^true! !!Voice methodsFor: 'initialize-release'!close
	"Close the stream, devise, or whatever."

	self subclassResponsibility! !!Voice methodsFor: 'initialize-release'!initialize
	"Set up a Voice."

	^self! !!Voice methodsFor: 'initialize-release'!initializeNamed: aName 
	"Set up a named Voice."

	self initialize.
	(aName isKindOf: Number)
		ifTrue: [name := aName printString]
		ifFalse: [name := aName].
	SirenSession voices ~~ nil
		ifTrue: [SirenSession voices at: name asSymbol put: self]! !!Voice methodsFor: 'initialize-release' stamp: 'stp 1/28/2023 13:49'!species	^Voice! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Voice class	instanceVariableNames: ''!!Voice class methodsFor: 'class initialization' stamp: 'stp 1/19/2023 07:56'!initialize
	"set up the shared dictionary for caching voices"
	"Voice initialize"
	"Voice allSubclassesDo: [ :each | each initialize]"

	SirenSession initializeVoices.
	DefaultVoiceClass := #MIDIVoice.
	self allSubclassesDo: [ :each | each initialize]! !!Voice class methodsFor: 'class initialization'!reset
	"Reset all sub-instances of Voice."
	"Voice reset"

	self allSubInstancesDo: [ :aV | aV reset ]! !!Voice class methodsFor: 'class initialization'!setDefault: className
	"Set the default class to use for new voices"
	"Voice initialize."

	DefaultVoiceClass := className! !!Voice class methodsFor: 'instance creation'!default
	"Answer a voice."

	DefaultVoiceClass == #MIDIVoice ifTrue: [^MIDIVoice default].
	DefaultVoiceClass == #OSCVoice ifTrue: [^OSCVoice default].
	DefaultVoiceClass == #SoundVoice ifTrue: [^SoundVoice default].
	DefaultVoiceClass == #SuperColliderVoice ifTrue: [^SuperColliderVoice default].
	(SirenSession voices == nil or: [SirenSession voices isEmpty])
		ifFalse: [^SirenSession voices associations asOrderedCollection first value]
		ifTrue: [self error: 'Unknown default voice class'].! !!Voice class methodsFor: 'instance creation'!named: aName
	"Answer the shared dictionary's voice item at the given name (or the name itself if it's a voice)"

	(aName isKindOf: Voice)
		ifTrue: [^aName].
	(aName class == SmallInteger)
		ifTrue: [^MIDIVoice default].
	(SirenSession voices ~~ nil and: 
			[SirenSession voices includesKey: aName printString asSymbol])
		ifTrue: [^SirenSession voices at: aName printString asSymbol]
			ifFalse: [Voice default].! !!Voice class methodsFor: 'instance creation'!on: someStream
	"Answer a new voice instance on the given stream."

	| me |
	me := self new initialize.
	me stream: someStream.
	^me! !Voice subclass: #MIDIFileVoice	instanceVariableNames: 'fileType tracks ppq tempo'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!MIDIFileVoice commentStamp: '<historical>' prior: 0!A MIDIFileVoice can read version 2 MIDI files.!!MIDIFileVoice methodsFor: 'accessing'!defaultEventClass

	^MusicEvent! !!MIDIFileVoice methodsFor: 'accessing'!tempo

	^tempo! !!MIDIFileVoice methodsFor: 'accessing'!tempo: tem

	tempo := tem! !!MIDIFileVoice methodsFor: 'read/write'!eventList
	"load a MIDI file into anEventList."
	"For file types 1 and 2, extra EventLists are created within anEventList, one per track."
	"(MIDIFileVoice newOn: 'fuga1.midi') eventList inspect"
	"(MIDIFileVoice newOn: 'fuga1.midi') eventList edit"

	| anEventList subEventList |
	anEventList := EventList newAnonymous.
	self readMThd.
	[stream atEnd] whileFalse:
		[subEventList := EventList newAnonymous.
		self readTrackOnto: subEventList.
		anEventList add: subEventList at: 0].
"	anEventList at: #tempo put: ((60000000/tempo) asInteger)."		"tempo in bpm"
	Transcript show: ((anEventList events) size) printString, ' track EventLists'; cr.
	^anEventList! !!MIDIFileVoice methodsFor: 'read/write'!readMThd
	"Read the header chunk from MIDI binary file."

	| size word |
	size := stream size.
	Transcript show: 'size: ', size printString; cr.
	word := stream nextWord.	" 'MT' "
	(word == 16r4D54) ifFalse:
		[Transcript show: 'wierd value in MIDI header', 
				word printString; cr].
	word := stream nextWord.	" 'hd' "
	word := stream nextWord.	"null padding"
	word := stream nextWord.	"length -- always 6"
	fileType := stream nextWord.	"MIDI file type (0, 1, 2)"
	Transcript show: 'MIDI file format is ', fileType printString; cr.
	tracks := stream nextWord.	"no. tracks"
	Transcript show: 'number of tracks = ', tracks printString; cr.
	ppq := stream nextWord.	"division (ticks per quarter beat)"
	Transcript show: 'ticks per quarter beat = ', ppq printString; cr.! !!MIDIFileVoice methodsFor: 'read/write'!readMTrk
	"Read a track chunk from MIDI binary file. Answer its length."

	| length word  |
	word := stream nextWord.	" 'MT' "
	(word == 16r4D54) ifFalse: [^0 "self error: 'MT expected in track chunk'"].
	word := stream nextWord.	" 'rk' "
	word := stream nextWord.	"null padding"
	length := stream nextWord.	"length"
	^length! !!MIDIFileVoice methodsFor: 'read/write'!readOnto: eventList 
	"Read a MIDI file"

	| track |
	self readMThd.
	[stream atEnd]
		whileFalse: 
			[track := EventList newAnonymous.
			self readTrackOnto: track.
			eventList add: track at: 0].
	Transcript show: eventList events size printString , ' tracks.'; cr.
	^self! !!MIDIFileVoice methodsFor: 'read/write'!readTrackOnto: anEventList 
	"Read a track of MIDI events from my stream into anEventList"

	| fileTime trackTime deltaTime byte type vel  length startPos |
	length := self readMTrk.	"read track chunk header"
	startPos := stream position.
	fileTime := 0.
	trackTime := 0.
	[stream position < (startPos + length)]
		whileTrue: [			"Get the delta time"
			fileTime := fileTime + self getVarLen.	
							"convert pulses to ms"
			deltaTime := (fileTime / ppq * 600000 / 1000 asFloat - trackTime) asInteger.
			trackTime := trackTime + deltaTime.	"Get the event itself"
			byte := stream next.
			(byte bitAnd: 128) = 0
				ifFalse: ["if not sysex" byte == 255
					ifTrue: ["a meta-event?"
						type := stream next]
					ifFalse: [type := byte.
						type == 240 ifFalse: [byte := stream next]]].
			(type < 128 or: [type == 240])
				ifTrue: ["Process a meta- or sysex event"
					type == 81
						ifTrue: [tempo := 0.
							self getVarLen timesRepeat: 
								[tempo := tempo * 256 + stream next]]
				"		ifFalse: [Transcript show: 'meta or sysex, ' , 
								(type printStringRadix: 16) , ' -- '.
							self getVarLen timesRepeat:
								 [Transcript show: (stream next printStringRadix: 16), ' '].
							Transcript cr]"]
				ifFalse: [			"Process normal MIDI event"
					(type bitAnd: 224)= 192
						ifTrue: [vel := nil]
						ifFalse: ["program change or channel pressure"
							vel := stream next].
					anEventList 
						add: ((self defaultEventClass pitch: byte ampl: vel 
								voice: (type bitAnd: 15)) command: (byte bitAnd: 16rF0))
						at: trackTime]].
	anEventList code: #noteOnOff.
	anEventList setDurations.
"	Transcript show: anEventList events size printString , ' events'; cr"! !!MIDIFileVoice methodsFor: 'private' stamp: 'stp 1/20/2023 11:44'!getVarLen
	"Combine 1 or more input bytes from my stream into a variable length item"

	| answer byte |
	answer := 0.
	byte := 16r80.
	[(byte bitAnd: 16r80) == 0] whileFalse: [
		byte := stream next asInteger. 
		answer := (answer bitShift: 7) + (byte bitAnd: 16r7F)].
	^answer! !!MIDIFileVoice methodsFor: 'initialize-release'!close
	"Close the stream, device, or whatever."

	stream close! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MIDIFileVoice class	instanceVariableNames: ''!!MIDIFileVoice class methodsFor: 'instance creation' stamp: 'stp 1/20/2023 11:45'!newOn: fnam 	| inst realName |	realName := SirenUtility findFile: fnam.	(inst := self new) stream: (FileStream named: realName) binary.	^ inst! !!MIDIFileVoice class methodsFor: 'examples'!midiFileExample
	"MIDIFileVoice midiFileExample"

	(MIDIFileVoice newOn: 'K194.MID')
		readOnto: (EventList newNamed: #K194).
	(EventList named: #K194) edit.
	^self! !Voice subclass: #NotelistVoice	instanceVariableNames: 'parameterMap lastTime header'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!NotelistVoice commentStamp: '<historical>' prior: 0!Instances of the subclasses of NotelistVoice can read or write note list file streams in various formats for software sound synthesis packages such as cmusic, csound, or cmix.

Note that we are output-only at present.

Instance Variables:
	parameterMap		<OrderedCollection of Symbols, Blocks, or Associations> 
			the object's P:=map, used to print out or parse note commands.

!!NotelistVoice methodsFor: 'accessing'!addMap: anItem
	"Add the argument (a symbol or block), to the parameterMap of the receiver"

	parameterMap addLast: anItem! !!NotelistVoice methodsFor: 'accessing'!header: aString
	"Set the file header string of the receiver."

	header := aString! !!NotelistVoice methodsFor: 'accessing'!parameterMap
	"Answer the parameterMap of the receiver."

	^parameterMap! !!NotelistVoice methodsFor: 'accessing'!parameterMap: anOrderedCollection
	"Set the parameterMap of the receiver."

	parameterMap := anOrderedCollection! !!NotelistVoice methodsFor: 'events'!dump: aStringOrValue 
	"The case I handle is strings. Subclasses may override."

	stream nextPutAll: aStringOrValue! !!NotelistVoice methodsFor: 'events'!endLine
	"Put the proper command terminator on the receiver's stream."

	stream cr! !!NotelistVoice methodsFor: 'events' stamp: 'stp 12/28/2022 16:52'!mapProperty: item of: ass within: list 
	"Write the given event association to the receiver's stream in the appropriate format."

	| event time |
	event := ass value.
	time := ass key asSec value.
	item isSymbol
		ifTrue: [item == #start
				ifTrue: [self dump: time printString]
				ifFalse: [item == #delay
						ifTrue: [self dump: (list nextTimeFor: ass) asSec value printString]
						ifFalse: [self dump: ((event perform: item) asFloat printString: 4)]].
			^self].
	item isString
		ifTrue: [self dump: item.
			^self].
	(item isKindOf: BlockClosure)
		ifTrue: [item numArgs = 1
				ifTrue: ["1-arg blocks = send event"
					self dump: (item value: event)
					"2-arg blocks = send ass and list"]
				ifFalse: [self dump: (item value: ass value: list)]]
		ifFalse: [self error: 'unknown voice map item type.']! !!NotelistVoice methodsFor: 'events'!play: anEL
	"Perform the argument."

	self writeHeader.
	self play: anEL at: 0.
	self writeFooter! !!NotelistVoice methodsFor: 'events'!play: anEL at: start
	"Perform the argument."

	| time event |
	anEL do: 
		[ :ass |
		time := ass key asSec value.
		event := ass value.
		event isEventList
			ifTrue: [self play: event at: time + start]
			ifFalse: [self playAssociation: ass in: anEL]].! !!NotelistVoice methodsFor: 'events'!playAssociation: ass in: list
	"Write the given event association to the receiver's stream in the appropriate format."

	stream == nil ifTrue: [^self].
	parameterMap do:			"print out the fields in my p-map"
		[ :item |
		self mapProperty: item of: ass within: list ]
		separatedBy: [self space].
	self endLine! !!NotelistVoice methodsFor: 'events'!space
	"Put the proper field separator on the receiver's stream."

	stream space; tab! !!NotelistVoice methodsFor: 'events'!writeFooter
	"Write a notelist file footer on the receiver's stream."

	^self! !!NotelistVoice methodsFor: 'events'!writeHeader
	"Write a notelist file header on the receiver's stream."

	^self! !!NotelistVoice methodsFor: 'initialize-release'!close
	"Close the receiver's output stream (if it's a file)."

	stream flush; close! !!NotelistVoice methodsFor: 'initialize-release'!initialize
	"Initialize the receiver."

	self parameterMap: self class defaultPMap! !!NotelistVoice methodsFor: 'initialize-release'!initializeNamed: aName onStream: theStream
	"Answer a new named NotelistVoice on the given stream."

	self initializeNamed: aName.
	stream := theStream! !!NotelistVoice methodsFor: 'initialize-release'!initializeOnStream: theStream
	"Answer a new named NotelistVoice on the given stream."

	self initialize.
	stream := theStream! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!NotelistVoice class	instanceVariableNames: ''!!NotelistVoice class methodsFor: 'instance creation'!defaultPMap
	"Answer the default parameterMap of the receiver class's instances."

	self subclassResponsibility! !!NotelistVoice class methodsFor: 'instance creation' stamp: 'stp 12/31/2022 07:18'!newNamed: aName onFile: fName 	"Set up a NotelistVoice on the given output file"	"CmusicVoice newNamed: 'violin' onFile: 't.out.cmusic'"	^ self new initializeNamed: aName onStream: (FileStream named: fName)! !!NotelistVoice class methodsFor: 'instance creation'!newNamed: aName onStream: aStream
	"Set up a NotelistVoice on the given output stream"
	"CmusicVoice newNamed: 'violin' onStream: Transcript"

	^self new initializeNamed: aName onStream: aStream! !!NotelistVoice class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 16:51'!onFileNamed: fName
	"Set up a NotelistVoice on the given output file"
	"CmusicVoice onFile: 't.out.cmusic'"

	^self new initializeOnStream: (FileStream fileNamed: fName)! !NotelistVoice subclass: #CmixVoice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!CmixVoice commentStamp: '<historical>' prior: 0!A CmixVoice can write an event list to a MINC format file for use by cmix.!!CmixVoice methodsFor: 'events'!endLine

	stream nextPutAll: ');'; cr.
	^self! !!CmixVoice methodsFor: 'events'!playEvent: evt at: start 
	"Play the given event"

	| param newEv siz |
	stream == nil ifTrue: [^self].
	stream nextPutAll: parameterMap first.
	newEv := 2.
	siz := parameterMap size.
	[newEv <= siz]
		whileTrue: 
			[(param := parameterMap at: newEv) isSymbol
				ifTrue: [param == #start
						ifTrue: [stream nextPutAll: start printString]
						ifFalse: [stream nextPutAll: (evt perform: param) printString]]
				ifFalse: [param isString
						ifTrue: [stream nextPutAll: param]
						ifFalse: [(param isMemberOf: BlockContext)
								ifTrue: [stream nextPutAll: (param value: evt)]
								ifFalse: [self error: 'unknown voice map item type.']]].
			self space.
			newEv := newEv + 1].
	self endLine.
	^self! !!CmixVoice methodsFor: 'events'!space

	stream nextPut: $,; space; tab.
	^self! !!CmixVoice methodsFor: 'events'!writeHeader
	"Write out a CMix score file header."

	stream nextPutAll: '/* cmix MINC data file created ' , Date today printString , ' */'; cr.
	stream cr; nextPutAll: 'system("sfcreate -r 44100 -c 1 -i out.snd");'; cr.
	stream nextPutAll: 'output("out.snd");			/* output sound file */'; cr.
	stream nextPutAll: 'makegen(1, 10, 1024, 1)		/* f1 = sine wave */'; cr; cr.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!CmixVoice class	instanceVariableNames: ''!!CmixVoice class methodsFor: 'examples'!randomExampleToFileAndEdit
	"CmixVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.m'.
	^self! !!CmixVoice class methodsFor: 'examples' stamp: 'stp 12/31/2022 07:16'!randomExampleToFileNamed: fnam 	"Create a random event list and store it on a file."	| evt strm list rand vox |	strm := FileStream named: fnam.	rand := Random new.	"Create the event list and add a few additional properties"	(list := EventList randomExample: 64)		do: [:ass | 			(evt := ass event) at: #pos put: rand next.			evt				at: #prop1				put: (rand nextFrom: 320 to: 480).			evt				at: #prop2				put: (rand nextIntFrom: 12000 to: 23000)].	"Create a cmix voice and customize its parameter map."	(vox := CmixVoice newNamed: 'filetwo.cmx' onStream: strm) addMap: #pos.	vox		addMap: [:item | ((item at: #prop1)				/ 1000) printString].	vox addMap: #prop2.	"Play the list on the voice."	vox play: list.	strm close.	(FileStream named: fnam) edit! !!CmixVoice class methodsFor: 'instance creation'!default

	CmixVoice newNamed: 'fm' onStream: 'tmp.m' asFilename writeStream.
	^self! !!CmixVoice class methodsFor: 'instance creation'!defaultPMap
	"CmixVoice randomExampleToFileAndEdit"

	| map |
	map := OrderedCollection new: 8.
	map add: [ :ass :list | 'ins(', (ass key asSec value asFloat printString: 4)].
	map add: [ :t2 | t2 duration asSec value asFloat printString: 4 ].
	map add: [ :t3 | t3 loudness asDB positiveDB truncated printString].
	^map! !NotelistVoice subclass: #CmusicVoice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!CmusicVoice commentStamp: '<historical>' prior: 0!A CmusicVoice can write an event list to a sc format file for use by Cmusic.!!CmusicVoice methodsFor: 'events'!endLine

	stream nextPut: $;; cr.
	^self! !!CmusicVoice methodsFor: 'events'!writeFooter

	stream cr; nextPutAll: 'ter;'; cr; cr.
	^self! !!CmusicVoice methodsFor: 'events'!writeHeader
	"Write the cmusic score file header."

	stream nextPutAll: '/* cmusic score file created ', Date today printString , ' */'; cr.
	stream cr; nextPutAll: '#include <carl/cmusic.h>'; cr.
	stream nextPutAll: '#include "instruments.sc"'; cr.
	stream cr; nextPutAll: 'SINE(f1);'; cr; cr.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!CmusicVoice class	instanceVariableNames: ''!!CmusicVoice class methodsFor: 'examples'!randomExampleToFileAndEdit
	"CmusicVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sc'.
	^self! !!CmusicVoice class methodsFor: 'examples' stamp: 'stp 12/31/2022 07:17'!randomExampleToFileNamed: fnam 	"Store the given score to a file"	| ev strm list rand vox |	strm := FileStream named: fnam.	rand := Random new.	"Create the event list and add a few additional properties"	(list := EventList randomExample: 64)		do: [:ass | 			(ev := ass event) at: #pos put: rand next.			ev				at: #prop1				put: (rand nextFrom: 320 to: 480).			ev				at: #prop2				put: (rand nextIntFrom: 12000 to: 23000)].	"Create a cmusic voice and customize its parameter map."	(vox := CmusicVoice newNamed: 'violin' onStream: strm) parameterMap		at: 3		put: [:t8 | #(#in1 #in2 #in3 #in4) at: t8 voice \\ 4 + 1].	vox		addMap: [:t9 | (t9 pitch asHz value printString: 5)				, 'Hz'].	vox addMap: #pos.	vox		addMap: [:it | ((it at: #prop1)				/ 10) truncated printString].	vox addMap: #prop2.	"Play the list on the voice."	vox play: list.	strm close.	(FileStream named: fnam) edit! !!CmusicVoice class methodsFor: 'examples'!randomExampleToTranscript
	"CmusicVoice randomExampleToTranscript"

	(CmusicVoice newNamed: 'violin' onStream: Transcript)
		play: (EventList randomExample: 8).
	^self! !!CmusicVoice class methodsFor: 'instance creation'!defaultPMap
	"Answer the default cmusic p-map"

	| map |
	map := OrderedCollection new: 8.
	map add: 'note '.
	map add: #start.
	map add: 'ins'.
	map add: [ :t2 | t2 duration asSec value printString ].
	map add: [ :t3 | (t3 loudness asDB value printString ), 'dB'].
	^map! !NotelistVoice subclass: #CsoundVoice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!CsoundVoice commentStamp: '<historical>' prior: 0!A CsoundVoice can write an event list to a sco format file for use by Csound.!!CsoundVoice methodsFor: 'events'!writeFooter

	stream cr; nextPutAll: 'end'; cr; cr.
	^self! !!CsoundVoice methodsFor: 'events'!writeHeader

	stream nextPutAll: ';; csound score file created ', Date today printString; cr.
	stream cr; nextPutAll: 'f1 0 1024 10 1'; cr.
	stream cr.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!CsoundVoice class	instanceVariableNames: ''!!CsoundVoice class methodsFor: 'examples'!randomExampleToFileAndEdit
	"CsoundVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sco'.
	^self! !!CsoundVoice class methodsFor: 'examples' stamp: 'stp 12/31/2022 07:15'!randomExampleToFileNamed: fnam 	"Create a random event list and store it on a file."	"CsoundVoice randomExampleToFileAndEdit"	| evt strm list rand vox |	strm := FileStream named: fnam.	rand := Random new.	"Create the event list and add a few additional properties"	(list := EventList randomExample: 64)		do: [:ass | 			(evt := ass event) at: #pos put: rand next.			evt				at: #prop1				put: (rand nextFrom: 320 to: 480).			evt				at: #prop2				put: (rand nextIntFrom: 12000 to: 23000)].	"Create a csounc voice and customize its parameter map."	(vox := CsoundVoice newNamed: '1' onStream: strm) addMap: #pos.	vox		addMap: [:item | (item at: #prop1)				/ 1000 printString: 4].	vox addMap: #prop2.	"Play the list on the voice."	vox play: list.	strm close.	(FileStream named: fnam) edit! !!CsoundVoice class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 16:56'!defaultPMap	| map |	(map := OrderedCollection new: 8)		add: [:t2 | 'i' , (t2 voice == nil					ifTrue: [1]					ifFalse: [t2 voice]) printString].	map add: #start.	map		add: [:t3 | t3 duration asSec value asFloat printString: 4].	map		add: [:t4 | t4 loudness asDB positiveDB printString: 4].	^ map! !Voice subclass: #SoundVoice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!SoundVoice commentStamp: '<historical>' prior: 0!SoundVoice is unfinished; it is intended for putting sound objects in scores.!!SoundVoice methodsFor: 'events'!playEvent: t1 at: t2 

	stream play: t1.
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!SoundVoice class	instanceVariableNames: ''!!SoundVoice class methodsFor: 'constant access' stamp: 'stp 1/29/2023 10:00'!default

"	^self on: SoundPort default"! !NotelistVoice subclass: #SuperColliderVoice	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!SuperColliderVoice commentStamp: '<historical>' prior: 0!A SuperColliderVoice can write an event list to a sc format file for use by SuperCollider.!!SuperColliderVoice methodsFor: 'events'!endLine
	"Put the proper command terminator on the receiver's stream."

	stream nextPutAll: ' ],'; cr! !!SuperColliderVoice methodsFor: 'events'!space
	"Put the proper field separator on the receiver's stream."

	stream space; nextPut: $,; space; tab! !!SuperColliderVoice methodsFor: 'events'!writeFooter
	"Write a notelist file footer on the receiver's stream."

"	stream skip: -2.
"	stream nextPutAll: '];'; cr! !!SuperColliderVoice methodsFor: 'events'!writeHeader
	"Write a SuperCollider notelist file header on the receiver's stream."

	header ifNotNil: 
		[^stream nextPutAll: header].

	stream nextPutAll: 
'// SC2 Score for rotating bell clusters; generated on ',
		Date today printString, ' at ',  Time now printString, '

// Instrument format [ delT, \panner, \fname, dur, att, dec, ampl, angVel ]

score = #[
'! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!SuperColliderVoice class	instanceVariableNames: ''!!SuperColliderVoice class methodsFor: 'examples'!randomExampleToFileAndEdit
	"SuperColliderVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sc'.
	^self! !!SuperColliderVoice class methodsFor: 'examples' stamp: 'stp 12/31/2022 07:59'!randomExampleToFileNamed: fnam 	"Create a random event list and store it on a file."	"SuperColliderVoice randomExampleToFileAndEdit"	| evt strm list rand vox |	strm :=  FileStream named: fnam..	rand := Random new.	"Create the event list and add a few additional properties"	(list := EventList randomExample: 64)		do: [:ass | 			evt := ass event.			evt				at: #attack				put: (rand nextFrom: 0.001 to: 0.03).			evt				at: #decay				put: (rand nextFrom: 0.2 to: 0.3).			evt				at: #index				put: (rand nextFrom: 0.2 to: 2.3).			evt				at: #decay				put: (rand nextFrom: 0.2 to: 0.3)].	"Create a SuperCollider voice and customize its parameter map."	vox := SuperColliderVoice newNamed: '1' onStream: strm.	"Play the list on the voice."	vox play: list.	strm close.	( FileStream named: fnam) edit! !!SuperColliderVoice class methodsFor: 'instance creation'!defaultPMap
	"Answer up the parameter map for the default instance."
	"Instrument format
		[ delT, 'fm', fname, dur, att, dec, ampl, index ] "

	| map |
	map := OrderedCollection new: 8.
	map add: [ :ass :li | '	[ ', ((li nextTimeFor: ass) asSec value asFloat printString: 6)].
	map add: '\fm'.
	map add: [ :e | e duration asSec value asFloat printString: 4].
	map add: [ :e | e attack value printString: 4].
	map add: [ :e | e decay value printString: 4].
	map add: [ :e | e ampl value asFloat printString: 4].
	map add: [ :e | e index printString: 4].
	^map! !!SuperColliderVoice class methodsFor: 'instance creation'!pMapForPanner
	"Answer up the parameter map for the default instance."
	"Instrument format
		[ delT, 'panner', fname, dur, att, dec, ampl, angVel ] "

	| map |
	map := OrderedCollection new: 8.
	map add: [ :ass :li | '	[ ', (li nextTimeFor: ass) asSec value 
										printString: 5].
	map add: '\panner'.
	map add: [ :e | '\', e partial].
	map add: [ :e | e duration asSec value printString].
	map add: [ :e | e attack value printString].
	map add: [ :e | e decay value printString].
	map add: [ :e | e ampl value printString truncateTo: 6].
	map add: [ :e | e angularVelocity printString truncateTo: 6].
	^map! !Model subclass: #VoiceDialog	instanceVariableNames: 'name family properties'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicIO-Voices'!!VoiceDialog commentStamp: '<historical>' prior: 0!The VoiceDialog is a simple dialog box for configuring Siren voices.  It supports the creation of orchestras of named MIDI, OSC, Sound or File voices in the dictionary (SirenSession voices).

Structure:
	name - string - the voices name
	family - symbol - the output medium from the list: #(MIDI OSC Sound File)
	properties - dictionary - the configuration parameters
!!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 12:40:23'!createVoice
	"Use the settings to create a voice in 1 of the 4 families."
	
	Transcript show: 'Creating ', family asString, ' voice'; cr.
	((#MIDI -> 
		[ | dev vx |
		dev := MIDIDevice 
			outOn: (PortMIDIPort indexOfOutNamed: (properties at: #device) value) 
			port: MIDIPort concreteClass default.
		vx := MIDIVoice on: dev channel: (properties at: #channel) asInteger.
		SirenSession voice: name put: vx]), 
	(#OSC -> 
		[ | ip po vx | 
		ip := (((properties at: #oscHost) splitOn: $. asCharacter) collect: [ :it | it asInteger]) asByteArray.
		po := OSCPort to: ip port: (properties at: #oscPort) asInteger.
		vx := OSCVoice onPort: po.
		vx parameterMap: (properties at: #oscMap).
		SirenSession voice: name put: vx]), 
	(#Sound -> 
		[SirenSession voice: name put: (SoundVoice default instrument: (properties at: #soundName))]), 
	(#File -> 
		[ | cls |
		cls := ((#Csound -> CsoundVoice), 
			(#SuperCollider -> SuperColliderVoice), 
			(#cmusic -> CmusicVoice), 
			(#CMix -> CmixVoice)) case: (properties at: #fileFormat).
		cls newNamed: name onFile: (properties at: #fileName)])) case: family! !!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 07:30:00'!mediumNames
	"Answer the list of medium families"
	
	^ #(MIDI OSC Sound File)! !!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 10:20:46'!mediumSelectionIndex
	^ #(MIDI OSC Sound File) indexOf: family! !!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 13:13:04'!mediumSelectionIndex: index
	"Respond to a selection in the medium/family list."
	
	family := #(MIDI OSC Sound File) at: index.
	name := nil.
	self changed: #name! !!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 13:15:10'!name
	"Answer the list of medium families"
	
	| ind fold |
	name ifNil: 
		[ind := 1.
		fold := 'Orch1'.
		[(SirenSession voices includesKey: fold asSymbol) and: [(SirenSession voices at: fold asSymbol) includesKey: (family string, 'Voice', ind printString)]]
			whileTrue: [ind := ind + 1].
		name := fold, '/', family string, 'Voice', ind printString].
	^name! !!VoiceDialog methodsFor: 'accessing' stamp: 'stp 1/9/2023 06:44:36'!name: string
	"Answer the list of medium families"
	
	name := string! !!VoiceDialog methodsFor: 'midi' stamp: 'stp 1/9/2023 07:22:45'!midiProperties

	^ (properties at: #device ifAbsent: [0 -> 'No device']) value, ' - channel ', (properties at: #channel value ifAbsent: ['X'])
! !!VoiceDialog methodsFor: 'midi' stamp: 'stp 1/9/2023 10:48:03'!selectMIDIChannel

	| names sel |
	names := (1 to: 16) collect: [ :int | int printString].
	sel := (PopUpMenu labelArray: names) startUp.
  	properties at: #channel put: sel printString.
	self changed: #midiProperties! !!VoiceDialog methodsFor: 'midi' stamp: 'stp 1/17/2023 17:21'!selectMIDIDevice

	| oDevs sel |
	oDevs := (MIDIPort concreteClass devices
						select: [:item | item direction == #out]) collect: [:item | item name asString].
	oDevs ifEmpty: [^self inform: 'No MIDI devices found'].	sel := (PopUpMenu labelArray: oDevs) startUp.
  	(sel > 0) ifTrue: [properties at: #device put: (sel -> (oDevs at: sel)).
			self changed: #midiProperties]! !!VoiceDialog methodsFor: 'osc' stamp: 'stp 1/9/2023 11:56:54'!selectOSCHost
	"Let the user edit the desired OSC host/port string."  " '127.0.0.1:54321' "

	| str ind len ip port |
	str := (FillInTheBlankMorph request: 'What''s the voice''s OSC host/port?'
		initialAnswer: SirenSession instance oscAddress).
	str ifNil: [^self].
	str := str asByteString.
	(str includes: $:) ifFalse: [^self].
	ind := str indexOf: $: ifAbsent: [nil].
	ind ifNil: [^self].
	ip := str copyFrom: 1 to: ind - 1.
	len := str size.
	port := str copyFrom: ind + 1 to: len.
"	Transcript show: 'Set OSC host/port to: ', ip, ' : ', port; cr.
"	properties at: #oscHost put: ip asByteString.
	properties at: #oscPort put: port asByteString! !!VoiceDialog methodsFor: 'osc' stamp: 'stp 1/9/2023 10:43:34'!selectOSCInstrument
	"Let the user edit the desired OSC host/port string."  " '127.0.0.1:54321' "

	| in1 in2 |
	in1 := (FillInTheBlankMorph request: 'Starting instrument number'
		initialAnswer: '1').
	in1 ifNil: [^ self].
	in2 := (FillInTheBlankMorph request: 'Ending instrument number'
		initialAnswer: '1').
"	Transcript show: 'Set OSC host/port to: ', ip, ' : ', port; cr.
"	properties at: #oscInst0 put: in1.
	properties at: #oscInstN put: in2! !!VoiceDialog methodsFor: 'osc' stamp: 'stp 1/9/2023 10:57:44'!selectOSCMap
	"Let the user select from the available OSC parameter maps"

	| mList sel |
	mList := (OSCVoice class organization listAtCategoryNamed: 'parameter maps') collect: [ :meth | meth asString].
	sel := (PopUpMenu labelArray: mList) startUp.
	(sel == 0) ifTrue: [^self].
	properties at: #oscMap put: (mList at: sel)! !!VoiceDialog methodsFor: 'osc' stamp: 'stp 1/9/2023 10:40:02'!selectOSCServer
	| arr sel |
	arr := #('CSL' 'SuperCollider' 'Other' ).
	sel := (PopUpMenu labelArray: arr) startUp.
	sel == 0
		ifTrue: [^ self].
	properties at: #oscServer put: (arr at: sel).
! !!VoiceDialog methodsFor: 'files' stamp: 'stp 1/9/2023 11:00:12'!selectFileFormat

	| mList sel |
	mList := #(Csound SuperCollider cmusic CMix).
	sel := (PopUpMenu labelArray: mList) startUp.
	(sel == 0) ifTrue: [^self].
	properties at: #fileFormat put: (mList at: sel)! !!VoiceDialog methodsFor: 'files' stamp: 'stp 1/17/2023 17:33'!selectFileName

	| ext str |
	ext := properties at: #fileFormat ifAbsent: [#Text].
		ext := ((#Text -> 'txt'),			(#Csound -> 'sco'),			(#SuperCollider -> 'sc'),
		(#cmusic -> 'sco'),
		(#CMix -> 'cm')) at: ext.
	str := (FillInTheBlankMorph request: 'What''s the voice''s score file name?'
		initialAnswer: 'score1.', ext).
	str ifNil: [^self].
	properties at: #fileName put: str asByteString! !!VoiceDialog methodsFor: 'files' stamp: 'stp 1/9/2023 11:10:51'!selectSndInstrument

	| mList sel |
	mList := (FMSound class organization listAtCategoryNamed: 'instruments') collect: [ :meth | meth asString].
	sel := (PopUpMenu labelArray: mList) startUp.
	(sel == 0) ifTrue: [^self].
	properties at: #soundName put: (mList at: sel)! !!VoiceDialog methodsFor: 'initialization' stamp: 'stp 1/9/2023 07:08:52'!initialize

	properties := Dictionary new.
	family := #MIDI! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!VoiceDialog class	instanceVariableNames: ''!!VoiceDialog class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 06:53'!borderOn: morph	"Answer a BorderedMorph around the given morph."		^morph	"BorderedMorph new		color: Color transparent;		borderWidth: 1;		cellGap: ToolBuilder default panelSpacing;		addMorph: morph"! !!VoiceDialog class methodsFor: 'instance creation' stamp: 'stp 1/9/2023 07:02:08'!new
	^ super new initialize! !!VoiceDialog class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 07:03'!open
	"VoiceDialog open"

	| inst topLevel select midi snd  osc window font color h w |
	color := Color veryLightGray.	w := 700.	h := 250.
	topLevel := AlignmentMorph newRow.
	select := AlignmentMorph newColumn borderWidth: 4; color: color; borderColor: color.
	midi := AlignmentMorph newColumn borderWidth: 4; color: color; borderColor: color.
	osc := AlignmentMorph newColumn borderWidth: 4; color: color; borderColor: color.
	snd := AlignmentMorph newColumn borderWidth: 4; color: color; borderColor: color.
	inst := self new.
	font := (TextStyle named: 'ParcPlaceLegacy') fontOfSize: 16.
	
	select addMorph: ((StringMorph contents: 'Medium' font: font emphasis: 2) extent: (w / 5) @ (h / 8)) ;
		addMorphBack: (self borderOn: ((PluggableListMorph
					on: inst 
					list: #mediumNames
					selected: #mediumSelectionIndex
					changeSelected: #mediumSelectionIndex:) extent: (w / 5) @ (h / 3.5)));
		addMorphBack: (self borderOn: (((PluggableTextMorph on: inst
				text: #name accept: #name:) acceptOnCR: true) wrapFlag: true; extent: (w / 5) @ (h / 5)));
		addMorphBack: ((SirenSession createButton: 'Create voice' action: #createVoice on: inst) 
				 extent: (w / 4) @(h / 6)).

	midi addMorph: ((StringMorph contents: 'MIDI' font: font emphasis: 2) extent: (w / 5) @(h / 8));
		addMorphBack: (SirenSession createButton: 'Device' action: #selectMIDIDevice on: inst);
		addMorphBack: (SirenSession createButton: 'Channel' action: #selectMIDIChannel on: inst);
		addMorphBack: (VariableSpacer new);
		addMorphBack: ((PluggableTextMorph on: inst
				text: #midiProperties accept: nil) wrapFlag: true; extent: (w / 5) @ (h / 4)).

	osc addMorph: ((StringMorph contents: 'OSC' font: font emphasis: 2) extent: (w / 5) @ (h / 8));
		addMorphBack: (SirenSession createButton: 'Host/port' action: #selectOSCHost on: inst) ;
		addMorphBack: (SirenSession createButton: 'Server type' action: #selectOSCServer on: inst) ;
		addMorphBack: (SirenSession createButton: 'Instrument range' action: #selectOSCInstrument on: inst) ;
		addMorphBack: (SirenSession createButton: 'Parameter map' action: #selectOSCMap on: inst) .

	snd addMorph: ((StringMorph contents: 'Sound' font: font emphasis: 2) extent: (w / 5) @ (h / 8));
		addMorphBack: (SirenSession createButton: 'Instrument' action: #selectSndInstrument on: inst);
		addMorphBack: (VariableSpacer new);		addMorphBack: ((StringMorph contents: 'File' font: font emphasis: 2) extent: (w / 5) @ (h / 8));
		addMorphBack: (SirenSession createButton: 'Score format' action: #selectFileFormat on: inst);
		addMorphBack: (SirenSession createButton: 'File name' action: #selectFileName on: inst).

	topLevel addMorph: select ;
		addMorphBack: midi;
		addMorphBack: osc;
		addMorphBack: snd.

	self new.				"initialize singleton"
	window := SystemWindow new model: self new.
	window setLabel: 'Siren Voice Editor'.
	window		addMorph: topLevel		frame: (0 @ 0 corner: 1 @ 1).
	window openInWorldExtent: w@h
! !Voice initialize!