!SystemDictionary methodsFor: 'VW-compatibility' stamp: 'STP 4/22/2022 11:36:12'!

defineClass: clName superclass: superClass indexedType: ind private: priv instanceVariableNames: iVars classInstanceVariableNames: cIVars imports: imps category: cat
	"Visualworks-compatible class creation method"
	
	^superClass subclass: clName
	instanceVariableNames: iVars
	classVariableNames: ''
	poolDictionaries: ''
	category: cat! !

'From Cuis 6.0 [latest update: #5569] on 2 December 2022 at 11:03:21 am'!

!ByteArray methodsFor: 'converting' stamp: 'stp 12/2/2022 10:54:00'!
swapWords
	"Answer a copy of the receiver with the even/odd WORDS swapped."
	"#[2 0 3 0 3 0 3 128] swapWords"
	
	| new1 |
	new1 _ self class newFrom: self.
	1 to: new1 size by: 4 do: 
		[ :ind | | t1 |
		t1 _ new1 at: ind.
		new1 at: ind put: (new1 at: ind + 2).
		new1 at: ind + 2 put: t1.
		t1 _ new1 at: ind + 1.
		new1 at: ind + 1 put: (new1 at: ind + 3).
		new1 at: ind + 3 put: t1.].
	^new1! !

!ByteArray methodsFor: 'converting' stamp: 'stp 12/2/2022 10:47:34'!
swapBytes
	"Answer a copy of the receiver with the even/odd bytes swapped."
	"#[2 0 3 0 3 0 3 128] swapBytes"
	
	| new1 |
	new1 _ self class newFrom: self.
	1 to: new1 size by: 2 do: 
		[ :ind | | tmp |
		tmp _ new1 at: ind.
		new1 at: ind put: (new1 at: ind + 1).
		new1 at: ind + 1 put: tmp.].
	^new1! !

!ByteArray methodsFor: 'converting' stamp: 'stp 12/2/2022 11:01:27'!
reverseWords
	"Answer a copy of the receiver with the even/odd WORDS swapped."
	"#[2 0 3 0 3 0 3 128] reverseWords"
	
	| new1 |
	new1 _ self class newFrom: self.
	1 to: new1 size by: 4 do: 
		[ :ind | | t1 t2 t3 t4 |
		t1 _ new1 at: ind.
		t2 _ new1 at: ind + 1.
		t3 _ new1 at: ind + 2.
		t4 _ new1 at: ind + 3.
		new1 at: ind + 3 put: t1.
		new1 at: ind + 2 put: t2.
		new1 at: ind + 1 put: t3.
		new1 at: ind put: t4].
	^new1! !


'From Cuis 6.0 [latest update: #5569] on 2 December 2022 at 11:48:56 am'!

!Form class methodsFor: 'instance creation' stamp: 'stp 12/2/2022 11:48:00'!
extent: extentPoint depth: bitsPerPixel fromBytes: aByteArray
	"Answer an instance of me with bitmap of the given dimensions and depth from the given 32-bit PADDED-ROWS data."

	^self extent: extentPoint depth: bitsPerPixel bits: aByteArray swapWords swapBytes	! !

!CharacterSequence methodsFor: 'converting' stamp: 'stp 12/1/2022 23:41:31'!
clipTo: smallSize
	"return myself or a copy shortened by clipping to smallSize"

	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	^self copyFrom: 1 to: smallSize! !

'From Cuis 6.0 [latest update: #5569] on 2 December 2022 at 4:20:29 am'!

!Form methodsFor: 'display box access' stamp: 'stp 12/2/2022 04:20:24'!
bounds
	^ Rectangle
		origin: `0 @ 0`
		corner: width @ height! !

'From Cuis 6.0 [latest update: #5569] on 1 December 2022 at 11:58:13 pm'!

!Rectangle methodsFor: 'rectangle functions' stamp: 'stp 12/1/2022 23:58:05'!
expandedBy: delta 
	"Answer a Rectangle that is outset from the receiver by delta. delta is a 
	Rectangle, Point, or scalar."

	^(delta is: #Rectangle)
		ifTrue: [
			Rectangle
				origin: origin - delta origin 
				corner: corner + delta corner ]
		ifFalse: [
			Rectangle
				origin: origin - delta 
				corner: corner + delta ]! !

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


!Boolean methodsFor: 'printing'!

toOSCBytes: aConvertor
	^self! !


!Collection class methodsFor: 'instance creation'!

with: firstObject with: secondObject with: thirdObject with: fourthObject  with: fifth with: sixth 
	"Answer a new instance of a Collection containing the	
	four arguments as the elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	newCollection add: thirdObject.
	newCollection add: fourthObject.
	newCollection add: fifth.
	newCollection add: sixth.
	^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject  with: fifth
	"Answer a new instance of a Collection containing the	
	four arguments as the elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	newCollection add: thirdObject.
	newCollection add: fourthObject.
	newCollection add: fifth.
	^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject  with: fifth with: sixth with: seventh with: eighth
	"Answer a new instance of a Collection containing the	
	four arguments as the elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	newCollection add: thirdObject.
	newCollection add: fourthObject.
	newCollection add: fifth.
	newCollection add: sixth.
	newCollection add: seventh.
	newCollection add: eighth.
	^newCollection!

with: firstObject with: secondObject with: thirdObject with: fourthObject  with: fifth with: sixth with: seventh
	"Answer a new instance of a Collection containing the	
	four arguments as the elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	newCollection add: thirdObject.
	newCollection add: fourthObject.
	newCollection add: fifth.
	newCollection add: sixth.
	newCollection add: seventh.
	^newCollection! !


!Collection methodsFor: 'testing'!

hasItems
	"Answer whether or not the receiver has items or components (true)."

	^true! !

!Collection methodsFor: 'processing'!

scrambled
	"Answer a collection of the elements of the receiver in random order."

	^self collect: [ :item | 0 - item]!

negated
	"Answer a collection of the negations of the receiver."

	^self collect: [ :item | 0 - item]!

reciprocal
	"Answer a collection of the reciprocals of the receiver."

	^self collect: [ :item | 1 / item]! !


!SequenceableCollection methodsFor: 'copying'!

copyAfter: anObject
	"Answer a copy of the receiver after the last occurrence of anObject, non-inclusive."

	| index |
	index := self lastIndexOf: anObject ifAbsent: [^self copy].
	^self copyFrom: index + 1 to: self size! !

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

!Float methodsFor: 'printing'!

printString: digits
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream digits: digits.
	^aStream contents! !

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


!Interval methodsFor: 'accessing'!

stop
	"Answer the last element of the arithmetic progression."

	^stop! !

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


!ByteArray methodsFor: 'printing'!

printIP
	| printSize stream |
	stream := WriteStream on: (String new: 32).
	printSize := self size.
	printSize > self maxPrintElements
		ifTrue: [printSize := self maxPrintElements].
	(1 to: printSize)
		do: [ :index | (self at: index) printOn: stream]
		separatedBy: [stream nextPut: $.].
	^stream contents! !


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


'From Squeak6.0 of 21 October 2022 [latest update: #22114] on 25 December 2022 at 4:25:13 pm'!!Time class methodsFor: 'clock' stamp: 'stp 12/25/2022 16:25'!microsecondClock	"Answer the UTC microseconds since the Smalltalk epoch (January 1st 1901, the start of the 20th century).	 The value is derived from the Posix epoch with a constant offset corresponding to elapsed microseconds	 between the two epochs according to RFC 868."	<primitive: 240>	^0! !!Collection methodsFor: 'testing' stamp: 'stp 12/25/2022 16:21'!hasItems	"Answer whether or not the receiver has items or components."	^true! !!Object methodsFor: 'testing'!hasItems
	"Answer whether or not the receiver has items or components."

	^false! !!Float methodsFor: 'converting' stamp: 'stp 12/11/2022 00:27'!toOSCBytes: aConvertor 	"Store a float in the proper byte-endian order"	"(1.5 asIEEE32BitWord bitShift: -16) bitAnd: 16rFF"		| int |	int := self asIEEE32BitWord.	Smalltalk isBigEndian		ifTrue: [aConvertor nextPut: (int bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF)]		ifFalse: [aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: (int bitAnd: 16rFF)]! !
!Float methodsFor: 'converting' stamp: 'stp 12/11/2022 00:27'!toOSCTypeOn: aConvertor  
	aConvertor nextPutType: $f! !
!#ByteArray methodsFor: 'printing' stamp: 'stp 12/29/2022 17:32:37'!
printIP
	| printSize stream |
	stream := WriteStream on: (String new: 32).
	printSize := self size.
"	printSize > self maxPrintElements
		ifTrue: [printSize := self maxPrintElements].
"	(1 to: printSize)
		do: [ :index | (self at: index) printOn: stream]
		separatedBy: [stream nextPut: $.].
	^stream contents! !

