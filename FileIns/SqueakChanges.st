
!Object methodsFor: 'associating'!

=> anObject
	"Answer an EventAssociation between the receiver and the argument."
	"1.234 => (DurationEvent dur: 250 msec voice: #flute)"

	^EventAssociation new key: self value: anObject! !

!Object methodsFor: 'testing'!

isMusicMagnitude
	"Answer false for anything but a real MusicMagnitude."

	^false!

isAssociation
	"Return true if the receiver is an association.
	Note: Do not override in any class except association."

	^false!

isTreeNode
	"Answer whether the receiver is a tree node."

	^false!

isVoice
	"Answer false"

	^false!

hasItems
	"Answer whether or not the receiver has items or components."

	^false!

isCPointer
	"Coerces CPointers to true and everything else to false."

	^false!

isUnitGenerator
	"Answer Whether the receiver is a kind of CSL UnitGenerator"

	^false!

isPoint
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^false!

isEvent
	"Coerces Events to true and everything else to false.  Event
	overrides with ^true"

	^false!

isEventList
	"Coerces EventLists to true and everything else to false.  EventList
	overrides with ^true"

	^false!

isDisplayItem
	"Answer whether the receiver is a kind of DisplayItem"

	^false! !

!Object methodsFor: 'converting'!

Hz
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self!

usec
	"Answer a duration whose value is the receiver." 

	^USecondDuration value: self!

beat
	"Answer a ratio duration whose value is the receiver."

	^RatioDuration value: self!

value
	"Answer the 'value' of the receiver (i.e., itself)."

	^self!

dur
	"Answer a duration whose value is the receiver."

	^Duration value: self!

velocity
	"Answer a MIDI loudness whose value is the receiver."

	^MIDIVelocity value: self!

ampl
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self!

note
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self!

pitch
	"Answer a pitch whose value is the receiver."

	^Pitch value: self!

msec
	"Answer a duration whose value is the receiver." 

	^MSecondDuration value: self!

loudness
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self!

Hertz
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self!

seconds
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self!

dB
	"Answer a deciBel loudness whose value is the receiver."

	^DBLoudness value: self!

measures
	"Answer a duration whose value is the receiver."

	^MeasureDuration value: self!

mostGeneral
	^self!

duration
	"Answer a duration whose value is the receiver."

	^Duration value: self!

sec
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self! !


!Number methodsFor: 'converting'!

wait
	"Delay for the receiver in seconds"

	(Delay forSeconds: self) wait.! !


!Association methodsFor: 'testing'!

isAssociation
	"Return true if the receiver is an association."

	^true! !

!Association methodsFor: 'copying'!

, anAssociation

 	"Answer a Dictionary with of the receiver and the argument as its elements."
	"(#a -> 1), (#b -> 2)"
	| dict |	dict := Dictionary new.
	dict add: self.
	dict add: anAssociation.
	^dict! !

!UndefinedObject methodsFor: 'printing'!

toOSCBytes: aConvertor
	^self!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $N! !


!Integer methodsFor: 'printing'!

toOSCBytes: aConvertor 
	"pad beginning with 0's"

	aConvertor next: 4 - self digitLength put: 0.
	self digitLength
		to: 1
		by: -1
		do: [:i | aConvertor nextPut: (self digitAt: i)]! !


!Boolean methodsFor: 'printing'!

toOSCBytes: aConvertor
	^self! !



!Integer methodsFor: 'converting'!

key
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self! !


!Random methodsFor: 'accessing'!

nextFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high - low) + low)!

nextIntFrom: low to: high
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high + 1 - low) + low) truncated! !



!Behavior methodsFor: 'accessing class hierarchy'!

sortedSubclasses
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
	^coll! !


!SequenceableCollection methodsFor: 'printing'!

toOSCBytes: aConvertor 
	self do: [:each | each toOSCBytes: aConvertor]!

toOSCTypeOn: aConvertor 
	aConvertor nextPut: $[.
	self do: [:each | each toOSCTypeOn: aConvertor].
	aConvertor nextPut: $]! !


!Float methodsFor: 'converting'!

toOSCBytes: aConvertor 
	"Store a float in the proper byte-endian order"

	| flt |
	flt := self asFloat.
	UninterpretedBytes isBigEndian
		ifTrue: [aConvertor nextPut: (flt basicAt: 1).
			aConvertor nextPut: (flt basicAt: 2).
			aConvertor nextPut: (flt basicAt: 3).
			aConvertor nextPut: (flt basicAt: 4)]
		ifFalse: [aConvertor nextPut: (flt basicAt: 4).
			aConvertor nextPut: (flt basicAt: 3).
			aConvertor nextPut: (flt basicAt: 2).
			aConvertor nextPut: (flt basicAt: 1)]!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $f! !



!True methodsFor: 'printing'!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $T! !


!String class methodsFor: 'instance creation'!

cr
	"Answer a string with a carriage return character"

	^self with: Character cr! !


!String methodsFor: 'printing'!

toOSCBytes: aConvertor 
	self isEmpty ifTrue: [^self].
	aConvertor nextPutAll: self asByteArray.
				"Align to 4 byte boundry"
	4 - (self size \\ 4) timesRepeat: [aConvertor nextPut: 0]!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s! !



!Symbol methodsFor: 'printing'!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s! !


!Dictionary methodsFor: 'enumerating'!

case: aKey otherwise: otherwiseBlock
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))
	case: #b otherwise: [Transcript cr; show: 'other'; cr]"

	^(self at: aKey ifAbsent: otherwiseBlock) value!

case: aKey
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))	case: #b"

	^self case: aKey otherwise: []! !

!Dictionary methodsFor: 'copying'!

, anAssociation 	"Add the argument to the receiver."
	"(#a -> 1), (#b -> 2), (#c -> 3)"

	self add: anAssociation.	^self! !


!Character methodsFor: 'converting'!

asString
	"Answer the receiver converted into a string."

	^String with: self! !


!Point methodsFor: 'testing'!

isPoint
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^true! !

!Point methodsFor: 'truncation and round off'!

float
	"Answer a new Point that is the receiver's x and y as floating-point numbers."

	^x asFloat @ y asFloat! !

!Point methodsFor: 'coercing'!

@ aZValue
	"Answer a ZPoint with the receiver and the argument as coordinates."

	^ZPoint x: x y: y z: aZValue! !

!SmallInteger methodsFor: 'printing'!

toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $i! !


!False methodsFor: 'printing'!

printOSCTypeOn: aStream
	aStream nextPut: $F!

toOSCTypeOn: aConvertor
	aConvertor nextPutType: $F! !

!Debugger methodsFor: 'initialize' stamp: 'cmm 6/22/2018 23:04'!initialExtent	"Initial extent for the full debugger. For the notifier's extent see #initialExtentForNotifier."		^ SavedExtent ifNil: [ 600@700]! !!Inspector methodsFor: 'initialization' stamp: 'stp 12/7/2022 11:45'!initialExtent	"Answer the desired extent for the receiver when it is first opened on the screen.  "	^ 450 @ 450! !!ObjectExplorer methodsFor: 'user interface' stamp: 'stp 12/7/2022 11:46'!initialExtent	^450@450! !