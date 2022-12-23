Model subclass: #Voice
They can be used for generating sound compiler notelists or (real-time output or file dumps of) MIDI data.

The instance variables are:
	instrument <Symbol, association or device> my instrument
	name 		<String or Symbol> my handle or name
	stream		<Stream> my (optional) I/O stream

The global object Voices is a dictionary that note events can refer to with integer or symbolic keys.
!
	"return my instrument."

	^instrument! !
	"set my instrument."

	instrument := newValue! !
	"answer the stream of the receiver voice"

	^stream! !
	"plug in a stream to the receiver voice"

	stream := someStream! !
	"Answer an event list read from the receiver."

	| e |
	e := EventList newAnonymous.
	self readOnto: e.
	^e! !
	"Expand the list and play it now."

	^self play: anEventList at: Time microsecondClock! !
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
			ifFalse: [self playEvent: e at: t + start]]! !
	"Play the argument now."

	self playEvent: anEvent at: 0! !
	"make sure my subclasses implement this"

	self subclassResponsibility! !
	"Read events from the receiver into the argument."

	self subclassResponsibility! !
	"Print the receivcer on the argument."

	aStream nextPutAll: 'a ', self class name! !
	"Answer true"

	^true! !
	"Close the stream, devise, or whatever."

	self subclassResponsibility! !
	"Set up a Voice."

	^self! !
	"Set up a named Voice."

	self initialize.
	(aName isKindOf: Number)
		ifTrue: [name := aName printString]
		ifFalse: [name := aName].
	SirenSession voices ~~ nil
		ifTrue: [SirenSession voices at: name asSymbol put: self]! !
	"set up the shared dictionary for caching voices"
	"Voice initialize"
	"Voice allSubclassesDo: [ :each | each initialize]"

	SirenSession initializeVoices.
	DefaultVoiceClass := #MIDIVoice.
! !
	"Reset all sub-instances of Voice."
	"Voice reset"

	self allSubInstancesDo: [ :aV | aV reset ]! !
	"Set the default class to use for new voices"
	"Voice initialize."

	DefaultVoiceClass := className! !
	"Answer a voice."

	DefaultVoiceClass == #MIDIVoice ifTrue: [^MIDIVoice default].
	DefaultVoiceClass == #OSCVoice ifTrue: [^OSCVoice default].
	DefaultVoiceClass == #SoundVoice ifTrue: [^SoundVoice default].
	DefaultVoiceClass == #SuperColliderVoice ifTrue: [^SuperColliderVoice default].
	(SirenSession voices == nil or: [SirenSession voices isEmpty])
		ifFalse: [^SirenSession voices associations asOrderedCollection first value]
		ifTrue: [self error: 'Unknown default voice class'].! !
	"Answer the shared dictionary's voice item at the given name (or the name itself if it's a voice)"

	(aName isKindOf: Voice)
		ifTrue: [^aName].
	(aName class == SmallInteger)
		ifTrue: [^MIDIVoice default].
	(SirenSession voices ~~ nil and: 
			[SirenSession voices includesKey: aName printString asSymbol])
		ifTrue: [^SirenSession voices at: aName printString asSymbol]
			ifFalse: [Voice default].! !
	"Answer a new voice instance on the given stream."

	| me |
	me := self new initialize.
	me stream: someStream.
	^me! !

	^MusicEvent! !

	^tempo! !

	tempo := tem! !
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
	^anEventList! !
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
	Transcript show: 'ticks per quarter beat = ', ppq printString; cr.! !
	"Read a track chunk from MIDI binary file. Answer its length."

	| length word  |
	word := stream nextWord.	" 'MT' "
	(word == 16r4D54) ifFalse: [^0 "self error: 'MT expected in track chunk'"].
	word := stream nextWord.	" 'rk' "
	word := stream nextWord.	"null padding"
	length := stream nextWord.	"length"
	^length! !
	"Read a MIDI file"

	| track |
	self readMThd.
	[stream atEnd]
		whileFalse: 
			[track := EventList newAnonymous.
			self readTrackOnto: track.
			eventList add: track at: 0].
	Transcript show: eventList events size printString , ' tracks.'; cr.
	^self! !
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
"	Transcript show: anEventList events size printString , ' events'; cr"! !
	"Combine 1 or more input bytes from my stream into a variable length item"

	| answer byte |
	answer := 0.
	byte := 16r80.
	[(byte bitAnd: 16r80) == 0] whileFalse: [
		byte := stream next. 
		answer := (answer bitShift: 7) + (byte bitAnd: 16r7F)].
	^answer! !
	"Close the stream, device, or whatever."

	stream close! !

	| inst realName |
	realName := SirenUtility findFile: fnam.
	(inst := self new) stream: (Filename named: realName) readStream binary.
	^inst! !
	"MIDIFileVoice midiFileExample"

	(MIDIFileVoice newOn: 'K194.MID')
		readOnto: (EventList newNamed: #K194).
	(EventList named: #K194) edit.
	^self! !

Note that we are output-only at present.

Instance Variables:
	parameterMap		<OrderedCollection of Symbols, Blocks, or Associations> 
			the object's P:=map, used to print out or parse note commands.

!
	"Add the argument (a symbol or block), to the parameterMap of the receiver"

	parameterMap addLast: anItem! !
	"Set the file header string of the receiver."

	header := aString! !
	"Answer the parameterMap of the receiver."

	^parameterMap! !
	"Set the parameterMap of the receiver."

	parameterMap := anOrderedCollection! !
	"The case I handle is strings. Subclasses may override."

	stream nextPutAll: aStringOrValue! !
	"Put the proper command terminator on the receiver's stream."

	stream cr! !
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
	(item isMemberOf: BlockClosure)
		ifTrue: [item numArgs = 1
				ifTrue: ["1-arg blocks = send event"
					self dump: (item value: event)
					"2-arg blocks = send ass and list"]
				ifFalse: [self dump: (item value: ass value: list)]]
		ifFalse: [self error: 'unknown voice map item type.']! !
	"Perform the argument."

	self writeHeader.
	self play: anEL at: 0.
	self writeFooter! !
	"Perform the argument."

	| time event |
	anEL do: 
		[ :ass |
		time := ass key asSec value.
		event := ass value.
		event isEventList
			ifTrue: [self play: event at: time + start]
			ifFalse: [self playAssociation: ass in: anEL]].! !
	"Write the given event association to the receiver's stream in the appropriate format."

	stream == nil ifTrue: [^self].
	parameterMap do:			"print out the fields in my p-map"
		[ :item |
		self mapProperty: item of: ass within: list ]
		separatedBy: [self space].
	self endLine! !
	"Put the proper field separator on the receiver's stream."

	stream space; tab! !
	"Write a notelist file footer on the receiver's stream."

	^self! !
	"Write a notelist file header on the receiver's stream."

	^self! !
	"Close the receiver's output stream (if it's a file)."

	stream flush; close! !
	"Initialize the receiver."

	self parameterMap: self class defaultPMap! !
	"Answer a new named NotelistVoice on the given stream."

	self initializeNamed: aName.
	stream := theStream! !
	"Answer a new named NotelistVoice on the given stream."

	self initialize.
	stream := theStream! !
	"Answer the default parameterMap of the receiver class's instances."

	self subclassResponsibility! !
	"Set up a NotelistVoice on the given output file"
	"CmusicVoice newNamed: 'violin' onFile: 't.out.cmusic'"

	^self new initializeNamed: aName onStream: (Filename named: fName) readStream! !
	"Set up a NotelistVoice on the given output stream"
	"CmusicVoice newNamed: 'violin' onStream: Transcript"

	^self new initializeNamed: aName onStream: aStream! !
	"Set up a NotelistVoice on the given output file"
	"CmusicVoice onFile: 't.out.cmusic'"

	^self new initializeOnStream: fName asFilename writeStream! !

	stream nextPutAll: ');'; cr.
	^self! !
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
	^self! !

	stream nextPut: $,; space; tab.
	^self! !
	"Write out a CMix score file header."

	stream nextPutAll: '/* cmix MINC data file created ' , Date today printString , ' */'; cr.
	stream cr; nextPutAll: 'system("sfcreate -r 44100 -c 1 -i out.snd");'; cr.
	stream nextPutAll: 'output("out.snd");			/* output sound file */'; cr.
	stream nextPutAll: 'makegen(1, 10, 1024, 1)		/* f1 = sine wave */'; cr; cr.
	^self! !
	"CmixVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.m'.
	^self! !
	"Create a random event list and store it on a file."

	| evt strm list rand vox |
	strm := (Filename named: fnam) writeStream.
	rand := Random new.
"Create the event list and add a few additional properties"
	(list := EventList randomExample: 64)
		do: 
			[ :ass | 
			(evt := ass event) at: #pos put: rand next.
			evt at: #prop1 put: (rand nextFrom: 320 to: 480).
			evt at: #prop2 put: (rand nextIntFrom: 12000 to: 23000)].
"Create a cmix voice and customize its parameter map."
	(vox := CmixVoice newNamed: 'file:=two' onStream: strm) addMap: #pos.
	vox addMap: [ :item | ((item at: #prop1) / 1000) printString].
	vox addMap: #prop2.
"Play the list on the voice."
	vox play: list.
	strm close.
	(Filename named: fnam) edit.
	^self! !

	CmixVoice newNamed: 'fm' onStream: 'tmp.m' asFilename writeStream.
	^self! !
	"CmixVoice randomExampleToFileAndEdit"

	| map |
	map := OrderedCollection new: 8.
	map add: [ :ass :list | 'ins(', (ass key asSec value asFloat printString: 4)].
	map add: [ :t2 | t2 duration asSec value asFloat printString: 4 ].
	map add: [ :t3 | t3 loudness asDB positiveDB truncated printString].
	^map! !

	stream nextPut: $;; cr.
	^self! !

	stream cr; nextPutAll: 'ter;'; cr; cr.
	^self! !
	"Write the cmusic score file header."

	stream nextPutAll: '/* cmusic score file created ', Date today printString , ' */'; cr.
	stream cr; nextPutAll: '#include <carl/cmusic.h>'; cr.
	stream nextPutAll: '#include "instruments.sc"'; cr.
	stream cr; nextPutAll: 'SINE(f1);'; cr; cr.
	^self! !
	"CmusicVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sc'.
	^self! !
	"Store the given score to a file"

	| ev strm list rand vox |
	strm := (Filename named: fnam) writeStream.
	rand := Random new.
"Create the event list and add a few additional properties"
	(list := EventList randomExample: 64)
		do: 
			[ :ass | 
			(ev := ass event) at: #pos put: rand next.
			ev at: #prop1 put: (rand nextFrom: 320 to: 480).
			ev at: #prop2 put: (rand nextIntFrom: 12000 to: 23000)].
"Create a cmusic voice and customize its parameter map."
	(vox := CmusicVoice newNamed: 'violin' onStream: strm) parameterMap at: 3 
			put: [ :t8 | #(#in1 #in2 #in3 #in4) at: t8 voice \\ 4 + 1].
	vox addMap: [ :t9 | (t9 pitch asHz value printString: 5), 'Hz'].
	vox addMap: #pos.
	vox addMap: [ :it | ((it at: #prop1) / 10) truncated printString].
	vox addMap: #prop2.
"Play the list on the voice."
	vox play: list.
	strm close.
	(Filename named: fnam) edit.
	^self! !
	"CmusicVoice randomExampleToTranscript"

	(CmusicVoice newNamed: 'violin' onStream: Transcript)
		play: (EventList randomExample: 8).
	^self! !
	"Answer the default cmusic p-map"

	| map |
	map := OrderedCollection new: 8.
	map add: 'note '.
	map add: #start.
	map add: 'ins'.
	map add: [ :t2 | t2 duration asSec value printString ].
	map add: [ :t3 | (t3 loudness asDB value printString ), 'dB'].
	^map! !

	stream cr; nextPutAll: 'end'; cr; cr.
	^self! !

	stream nextPutAll: ';; csound score file created ', Date today printString; cr.
	stream cr; nextPutAll: 'f1 0 1024 10 1'; cr.
	stream cr.
	^self! !
	"CsoundVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sco'.
	^self! !
	"Create a random event list and store it on a file."
	"CsoundVoice randomExampleToFileAndEdit"

	| evt strm list rand vox |
	strm := (Filename named: fnam) writeStream.
	rand := Random new.
"Create the event list and add a few additional properties"
	(list := EventList randomExample: 64)
		do: [ :ass | 
			(evt := ass event) at: #pos put: rand next.
			evt at: #prop1 put: (rand nextFrom: 320 to: 480).
			evt at: #prop2 put: (rand nextIntFrom: 12000 to: 23000)].
"Create a csounc voice and customize its parameter map."
	(vox := CsoundVoice newNamed: '1' onStream: strm) addMap: #pos.
	vox addMap: [ :item | ((item at: #prop1) / 1000) printString: 4].
	vox addMap: #prop2.
"Play the list on the voice."
	vox play: list.
	strm close.
	(Filename named: fnam) edit.
	^self! !

	| map |
	(map := OrderedCollection new: 8) add: [:t2 | 'i' , (t2 voice == nil
				ifTrue: [1]
				ifFalse: [t2 voice]) printString].
	map add: #start.
	map add: [ :t3 | t3 duration asSec value asFloat printString: 4].
	map add: [ :t4 | t4 loudness asDB positiveDB printString: 4].
	^map! !

	stream play: t1.
	^self! !

	^self on: SoundPort default! !
	"Put the proper command terminator on the receiver's stream."

	stream nextPutAll: ' ],'; cr! !
	"Put the proper field separator on the receiver's stream."

	stream space; nextPut: $,; space; tab! !
	"Write a notelist file footer on the receiver's stream."

"	stream skip: -2.
"	stream nextPutAll: '];'; cr! !
	"Write a SuperCollider notelist file header on the receiver's stream."

	header ifNotNil: 
		[^stream nextPutAll: header].

	stream nextPutAll: 
'// SC2 Score for rotating bell clusters; generated on ',
		Date today printString, ' at ',  Time now printString, '

// Instrument format [ delT, \panner, \fname, dur, att, dec, ampl, angVel ]

score = #[
'! !
	"SuperColliderVoice randomExampleToFileAndEdit"

	self randomExampleToFileNamed: 'rand.sc'.
	^self! !
	"Create a random event list and store it on a file."
	"SuperColliderVoice randomExampleToFileAndEdit"

	| evt strm list rand vox |
	strm := (Filename named: fnam) writeStream.
	rand := Random new.
"Create the event list and add a few additional properties"
	(list := EventList randomExample: 64)
		do: [ :ass | 
			evt := ass event.
			evt at: #attack put: (rand nextFrom: 0.001 to: 0.03).
			evt at: #decay put: (rand nextFrom: 0.2 to: 0.3).
			evt at: #index put: (rand nextFrom: 0.2 to: 2.3).
			evt at: #decay put: (rand nextFrom: 0.2 to: 0.3)].
"Create a SuperCollider voice and customize its parameter map."
	vox := SuperColliderVoice newNamed: '1' onStream: strm.
"Play the list on the voice."
	vox play: list.
	strm close.
	(Filename named: fnam) edit.
	^self! !
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
	^map! !
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
	^map! !