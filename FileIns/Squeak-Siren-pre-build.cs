'From Squeak6.1alpha of 16 January 2023 [latest update: #22362] on 17 January 2023 at 2:08:18 pm'!!Object methodsFor: 'converting' stamp: 'stp 1/4/2023 08:33'!asText	"Answer a text for the receiver"	^ self asStringOrText! !!Object methodsFor: 'associating'!=> anObject
	"Answer an EventAssociation between the receiver and the argument."
	"1.234 => (DurationEvent dur: 250 msec voice: #flute)"

	^EventAssociation new key: self value: anObject! !!Object methodsFor: 'converting'!Hertz
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self! !!Object methodsFor: 'converting'!Hz
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self! !!Object methodsFor: 'converting'!ampl
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self! !!Object methodsFor: 'converting'!beat
	"Answer a ratio duration whose value is the receiver."

	^RatioDuration value: self! !!Object methodsFor: 'converting'!dB
	"Answer a deciBel loudness whose value is the receiver."

	^DBLoudness value: self! !!Object methodsFor: 'converting'!dur
	"Answer a duration whose value is the receiver."

	^Duration value: self! !!Object methodsFor: 'converting'!duration
	"Answer a duration whose value is the receiver."

	^Duration value: self! !!Object methodsFor: 'converting'!loudness
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self! !!Object methodsFor: 'converting'!measures
	"Answer a duration whose value is the receiver."

	^MeasureDuration value: self! !!Object methodsFor: 'converting'!mostGeneral
	^self! !!Object methodsFor: 'converting'!msec
	"Answer a duration whose value is the receiver." 

	^MSecondDuration value: self! !!Object methodsFor: 'converting'!note
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self! !!Object methodsFor: 'converting'!pitch
	"Answer a pitch whose value is the receiver."

	^Pitch value: self! !!Object methodsFor: 'converting'!sec
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self! !!Object methodsFor: 'converting'!seconds
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self! !!Object methodsFor: 'converting'!usec
	"Answer a duration whose value is the receiver." 

	^USecondDuration value: self! !!Object methodsFor: 'converting'!value
	"Answer the 'value' of the receiver (i.e., itself)."

	^self! !!Object methodsFor: 'converting'!velocity
	"Answer a MIDI loudness whose value is the receiver."

	^MIDIVelocity value: self! !!Object methodsFor: 'testing'!hasItems
	"Answer whether or not the receiver has items or components."

	^false! !!Object methodsFor: 'testing'!isAssociation
	"Return true if the receiver is an association.
	Note: Do not override in any class except association."

	^false! !!Object methodsFor: 'testing'!isCPointer
	"Coerces CPointers to true and everything else to false."

	^false! !!Object methodsFor: 'testing'!isDisplayItem
	"Answer whether the receiver is a kind of DisplayItem"

	^false! !!Object methodsFor: 'testing'!isEvent
	"Coerces Events to true and everything else to false.  Event
	overrides with ^true"

	^false! !!Object methodsFor: 'testing'!isEventList
	"Coerces EventLists to true and everything else to false.  EventList
	overrides with ^true"

	^false! !!Object methodsFor: 'testing'!isMusicMagnitude
	"Answer false for anything but a real MusicMagnitude."

	^false! !!Object methodsFor: 'testing'!isPoint
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^false! !!Object methodsFor: 'testing'!isTreeNode
	"Answer whether the receiver is a tree node."

	^false! !!Object methodsFor: 'testing'!isUnitGenerator
	"Answer Whether the receiver is a kind of CSL UnitGenerator"

	^false! !!Object methodsFor: 'testing'!isVoice
	"Answer false"

	^false! !!Association methodsFor: 'testing'!isAssociation
	"Return true if the receiver is an association."

	^true! !!Association methodsFor: 'copying'!, anAssociation

 	"Answer a Dictionary with of the receiver and the argument as its elements."
	"(#a -> 1), (#b -> 2)"
	| dict |	dict := Dictionary new.
	dict add: self.
	dict add: anAssociation.
	^dict! !!Behavior methodsFor: 'accessing class hierarchy'!sortedSubclasses
	"Answer the receiver's subclasses in a sorted collection by name with abstract classes first."

	 | coll coll2 coll3 allClasses |
	subclasses == nil
		ifTrue: [^Set new].
	coll := subclasses collect: [ :e | e name ].
	coll := coll asSortedCollection.
	allClasses := OrderedCollection new: 1024.
	SystemUtils allClassesDo: [ :cls | allClasses add: cls].
	coll := coll collect: [ :e | allClasses detect: [ :cls | cls name == e]].
	coll2 := coll select: [ :e | e subclasses isEmpty].
	coll3 := coll select: [ :e | e subclasses isEmpty not].
	coll2 isEmpty ifTrue: [^coll3].
	coll3 isEmpty ifTrue: [^coll2].
	coll := OrderedCollection withAll: coll2.
	coll addAll: coll3.
	^coll! !!Boolean methodsFor: 'printing'!toOSCBytes: aConvertor
	^self! !!Browser methodsFor: 'initialize-release' stamp: 'stp 1/13/2023 07:21'!switchesFrame: bottomFraction fromLeft: leftFraction width: rightFraction	^LayoutFrame new		leftFraction: leftFraction offset: 0;		topFraction: bottomFraction offset: self buttonHeight negated;		rightFraction: (leftFraction + rightFraction) offset: 0;		bottomFraction: bottomFraction offset: 0! !!Character methodsFor: 'converting'!asString
	"Answer the receiver converted into a string."

	^String with: self! !!Collection methodsFor: 'testing' stamp: 'stp 1/12/2023 06:49'!hasItems	"Answer whether or not the receiver has items or components."	^true! !!Color class methodsFor: 'instance creation' stamp: 'stp 12/31/2022 11:59'!random: aRand	"Return a random color that isn't too dark or under-saturated."	^ self basicNew		setHue: 360.0 * aRand next		saturation: 0.3 + (aRand next * 0.7)		brightness: 0.4 + (aRand next * 0.6)! !!Controller methodsFor: 'initialize-release' stamp: 'stp 1/14/2023 08:00'!initialize	"Initialize the state of the receiver. Subclasses should include 'super 	initialize' when redefining this message to insure proper initialization."! !!Controller methodsFor: 'model access' stamp: 'stp 1/14/2023 12:19'!model: aModel 	"Controller|model: and Controller|view: are sent by View|controller: in 	order to coordinate the links between the model, view, and controller. In 	ordinary usage, the receiver is created and passed as the parameter to 	View|controller: so that the receiver's model and view links can be set 	up by the view."	model := aModel.! !!Debugger methodsFor: 'initialize' stamp: 'cmm 6/22/2018 23:04'!initialExtent	"Initial extent for the full debugger. For the notifier's extent see #initialExtentForNotifier."		^ SavedExtent ifNil: [ 600@700]! !!Delay class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 17:25'!forMicroseconds: anInteger 	"Return a new Delay for the given number of milliseconds. Sending	'wait' to this Delay will cause the sender's process to be suspended for	approximately that length of time."	anInteger < 0		ifTrue: [self error: 'delay times cannot be negative'].	^ self new setDelay: (anInteger asInteger / 1000) forSemaphore: Semaphore new! !!Dictionary methodsFor: 'enumerating'!case: aKey
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))	case: #b"

	^self case: aKey otherwise: []! !!Dictionary methodsFor: 'enumerating'!case: aKey otherwise: otherwiseBlock
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))
	case: #b otherwise: [Transcript cr; show: 'other'; cr]"

	^(self at: aKey ifAbsent: otherwiseBlock) value! !!Dictionary methodsFor: 'copying'!, anAssociation 	"Add the argument to the receiver."
	"(#a -> 1), (#b -> 2), (#c -> 3)"

	self add: anAssociation.	^self! !!False methodsFor: 'printing'!printOSCTypeOn: aStream
	aStream nextPut: $F! !!False methodsFor: 'printing'!toOSCTypeOn: aConvertor
	aConvertor nextPutType: $F! !!FileDirectory class methodsFor: 'platform specific' stamp: 'stp 12/29/2022 11:06'!separatorString	^ self pathNameDelimiter asString! !!FileList methodsFor: 'toolbuilder' stamp: 'stp 1/13/2023 10:31'!buildWith: builder	"FileList open"	| windowSpec window |	windowSpec := self buildWindowWith: builder specs: {		(self topConstantHeightFrame: self pathAndPatternHeight			fromLeft: 0			width: 1) -> [self buildPatternInputWith: builder].		(self frameOffsetFromTop: self pathAndPatternHeight			fromLeft: 0.25			width: 0.75			bottomFraction: 0.3) -> [self buildFileListWith: builder].		(self frameOffsetFromTop: self pathAndPatternHeight			fromLeft: 0			width: 0.25			bottomFraction: 1) -> [self buildDirectoryTreeWith: builder].		(0.25@0.3 corner: 1@1) -> [self buildContentPaneWith: builder].	}.	window := builder build: windowSpec.	self changed: #selectedPath.	^window! !!FileStream class methodsFor: 'instance creation' stamp: 'stp 12/31/2022 07:09'!named: fileName 	^ self concreteStream		fileNamed: (self fullName: fileName)! !!Form methodsFor: 'display box access' stamp: 'stp 12/31/2022 10:43'!bounds	^ Rectangle origin: 0 @ 0 corner: width @ height! !!Form class methodsFor: 'instance creation' stamp: 'stp 12/2/2022 11:48:00'!extent: extentPoint depth: bitsPerPixel fromBytes: aByteArray	"Answer an instance of me with bitmap of the given dimensions and depth from the given 32-bit PADDED-ROWS data."	^self extent: extentPoint depth: bitsPerPixel bits: aByteArray swapWords swapBytes! !!Inspector methodsFor: 'initialization' stamp: 'stp 12/7/2022 11:45'!initialExtent	"Answer the desired extent for the receiver when it is first opened on the screen.  "	^ 450 @ 450! !!LoopedSampledSound class methodsFor: 'instance creation' stamp: 'stp 12/29/2022 10:56'!fromAIFFFileNamed: fileName mergeIfStereo: mergeFlag 	"Initialize this sound from the data in the given AIFF file. If mergeFlag	is true and the file is stereo, its left and right channels are mixed	together to produce a mono sampled sound."	| aiffFileReader |	aiffFileReader := AIFFFileReader new.	aiffFileReader		readFromFile: fileName		mergeIfStereo: mergeFlag		skipDataChunk: false.	^self new fromAIFFFileReader: aiffFileReader mergeIfStereo: mergeFlag! !!Number methodsFor: 'converting' stamp: 'stp 12/30/2022 14:33'!asSec	^ SecondDuration value: self asFloat! !!Number methodsFor: 'converting'!wait
	"Delay for the receiver in seconds"

	(Delay forSeconds: self) wait.! !!Float methodsFor: 'converting' stamp: 'stp 12/30/2022 16:01'!toOSCBytes: aConvertor 	"Store a float in the proper byte-endian order"	"(1.5 asIEEE32BitWord bitShift: -16) bitAnd: 16rFF"		| int |	int := self asIEEE32BitWord.	Smalltalk isBigEndian		ifTrue: [aConvertor nextPut: (int bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF)]		ifFalse: [aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: (int bitAnd: 16rFF)]! !!Float methodsFor: 'converting'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $f! !!Float methodsFor: 'printing' stamp: 'stp 1/7/2023 10:17:03'!printString: digits
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream fractionDigits: digits.
	^aStream contents! !!Integer methodsFor: 'converting'!key
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self! !!Integer methodsFor: 'printing'!toOSCBytes: aConvertor 
	"pad beginning with 0's"

	aConvertor next: 4 - self digitLength put: 0.
	self digitLength
		to: 1
		by: -1
		do: [:i | aConvertor nextPut: (self digitAt: i)]! !!ObjectExplorer methodsFor: 'user interface' stamp: 'stp 12/7/2022 11:46'!initialExtent	^450@450! !!PackagePaneBrowser methodsFor: 'class list' stamp: 'stp 1/12/2023 11:49'!classList	"Answer an array of the class names of the selected category. Answer an 	empty array if no selection exists."	^ self hasSystemCategorySelected 		ifFalse: [self packageClasses]		ifTrue: [super classList]! !!PackagePaneBrowser methodsFor: 'toolbuilder' stamp: 'stp 1/13/2023 07:13'!buildDefaultBrowserWith: builder	"assemble the spec for a full 5-pane browser - package, category, class, protocol & message lists, build it and return the built but not opened morph.	the build-but-don't-open phase is factored out to support the prototypicalToolWindow facility"	"PackagePaneBrowser fullOnClass: Browser."	| max windowSpec | 	max := self wantsOptionalButtons ifTrue:[0.27] ifFalse:[0.35].	windowSpec := self buildWindowWith: builder specs: {		(0@0 corner: 0.15@max) -> [self buildPackageListWith: builder].		(0.15@0 corner: 0.35@max) -> [self buildSystemCategoryListWith: builder].		(self classListFrame: max fromLeft: 0.35 width: 0.25) -> [self buildClassListWith: builder].		(self switchesFrame: max fromLeft: 0.35 width: 0.25) -> [self buildSwitchesWith: builder].		(0.6@0 corner: 0.75@max) -> [self buildMessageCategoryListWith: builder].		(0.75@0 corner: 1@max) -> [self buildMessageListWith: builder].		(0@max corner: 1@1) -> [self buildCodePaneWith: builder].	}.	self setMultiWindowFor:windowSpec.	windowSpec defaultFocus: #packageList.	^builder build: windowSpec! !!Point methodsFor: 'testing'!isPoint
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^true! !!Point methodsFor: 'truncation and round off'!float
	"Answer a new Point that is the receiver's x and y as floating-point numbers."

	^x asFloat @ y asFloat! !!Point methodsFor: 'coercing'!@ aZValue
	"Answer a ZPoint with the receiver and the argument as coordinates."

	^ZPoint x: x y: y z: aZValue! !!PopUpMenu methodsFor: 'basic control sequence' stamp: 'stp 1/7/2023 10:46:55'!startUp
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil! !!Random methodsFor: 'accessing'!nextFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high - low) + low)! !!Random methodsFor: 'accessing'!nextIntFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high + 1 - low) + low) truncated! !!RealEstateAgent class methodsFor: 'settings' stamp: 'stp 1/13/2023 07:10'!windowColumnsDesired	"Answer how many separate vertical columns of windows are wanted.  5/22/96 sw"		^ Preferences reverseWindowStagger		ifTrue: [1]		ifFalse: [(self maximumUsableArea width > 640)				ifTrue: [3]				ifFalse: [1]]! !!SelectorBrowser class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 09:21'!open	"Create and schedule a selector fragment window."	self new open! !!SequenceableCollection methodsFor: 'printing'!toOSCBytes: aConvertor 
	self do: [:each | each toOSCBytes: aConvertor]! !!SequenceableCollection methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPut: $[.
	self do: [:each | each toOSCTypeOn: aConvertor].
	aConvertor nextPut: $]! !!ByteArray methodsFor: 'converting' stamp: 'stp 1/12/2023 11:44'!swapBytes	"Answer a copy of the receiver with the even/odd bytes swapped."	"#[2 0 3 0 3 0 3 128] swapBytes"		| new1 |	new1 := self class newFrom: self.	1 to: new1 size by: 2 do: 		[ :ind | | tmp |		tmp := new1 at: ind.		new1 at: ind put: (new1 at: ind + 1).		new1 at: ind + 1 put: tmp.].	^new1! !!ByteArray methodsFor: 'converting' stamp: 'stp 1/12/2023 11:44'!swapWords	"Answer a copy of the receiver with the even/odd WORDS swapped."	"#[2 0 3 0 3 0 3 128] swapWords "		| new1 |	new1 := self class newFrom: self.	1 to: new1 size by: 4 do: 		[ :ind | | t1 |		t1 := new1 at: ind.		new1 at: ind put: (new1 at: ind + 2).		new1 at: ind + 2 put: t1.		t1 := new1 at: ind + 1.		new1 at: ind + 1 put: (new1 at: ind + 3).		new1 at: ind + 3 put: t1.].	^new1! !!ByteArray methodsFor: 'printing' stamp: 'stp 12/29/2022 17:32:37'!printIP
	| printSize stream |
	stream := WriteStream on: (String new: 32).
	printSize := self size.
"	printSize > self maxPrintElements
		ifTrue: [printSize := self maxPrintElements].
"	(1 to: printSize)
		do: [ :index | (self at: index) printOn: stream]
		separatedBy: [stream nextPut: $.].
	^stream contents! !!SmallInteger methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $i! !!String methodsFor: 'printing'!toOSCBytes: aConvertor 
	self isEmpty ifTrue: [^self].
	aConvertor nextPutAll: self asByteArray.
				"Align to 4 byte boundry"
	4 - (self size \\ 4) timesRepeat: [aConvertor nextPut: 0]! !!String methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s! !!String class methodsFor: 'instance creation'!cr
	"Answer a string with a carriage return character"

	^self with: Character cr! !!Symbol methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s! !!Time class methodsFor: 'general inquiries' stamp: 'stp 12/30/2022 16:05'!microsecondClock	"Answer the value of the millisecond clock. Unlike older	implementations, this is a clock; it will never roll-over."	^ self utcMicrosecondClock! !!True methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $T! !!UndefinedObject methodsFor: 'printing'!toOSCBytes: aConvertor
	^self! !!UndefinedObject methodsFor: 'printing'!toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $N! !