'From Squeak6.1alpha of 16 January 2023 [latest update: #22362] on 17 January 2023 at 2:08:18 pm'!
	"Answer an EventAssociation between the receiver and the argument."
	"1.234 => (DurationEvent dur: 250 msec voice: #flute)"

	^EventAssociation new key: self value: anObject! !
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self! !
	"Answer a HertzPitch whose value is the receiver."

	^HertzPitch value: self! !
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self! !
	"Answer a ratio duration whose value is the receiver."

	^RatioDuration value: self! !
	"Answer a deciBel loudness whose value is the receiver."

	^DBLoudness value: self! !
	"Answer a duration whose value is the receiver."

	^Duration value: self! !
	"Answer a duration whose value is the receiver."

	^Duration value: self! !
	"Answer a loudness whose value is the receiver."

	^Amplitude value: self! !
	"Answer a duration whose value is the receiver."

	^MeasureDuration value: self! !
	^self! !
	"Answer a duration whose value is the receiver." 

	^MSecondDuration value: self! !
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self! !
	"Answer a pitch whose value is the receiver."

	^Pitch value: self! !
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self! !
	"Answer a duration whose value is the receiver."

	^SecondDuration value: self! !
	"Answer a duration whose value is the receiver." 

	^USecondDuration value: self! !
	"Answer the 'value' of the receiver (i.e., itself)."

	^self! !
	"Answer a MIDI loudness whose value is the receiver."

	^MIDIVelocity value: self! !
	"Answer whether or not the receiver has items or components."

	^false! !
	"Return true if the receiver is an association.
	Note: Do not override in any class except association."

	^false! !
	"Coerces CPointers to true and everything else to false."

	^false! !
	"Answer whether the receiver is a kind of DisplayItem"

	^false! !
	"Coerces Events to true and everything else to false.  Event
	overrides with ^true"

	^false! !
	"Coerces EventLists to true and everything else to false.  EventList
	overrides with ^true"

	^false! !
	"Answer false for anything but a real MusicMagnitude."

	^false! !
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^false! !
	"Answer whether the receiver is a tree node."

	^false! !
	"Answer Whether the receiver is a kind of CSL UnitGenerator"

	^false! !
	"Answer false"

	^false! !
	"Return true if the receiver is an association."

	^true! !

 	"Answer a Dictionary with of the receiver and the argument as its elements."
	"(#a -> 1), (#b -> 2)"
	| dict |	dict := Dictionary new.
	dict add: self.
	dict add: anAssociation.
	^dict! !
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
	^self! !
	"Answer the receiver converted into a string."

	^String with: self! !
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))	case: #b"

	^self case: aKey otherwise: []! !
	"Execute one of the values of the receiver or the otherwise block."
	"((#a -> [Transcript cr; show: 'a'; cr]),
 		(#b -> [Transcript cr; show: 'b'; cr]),
 		(#c -> [Transcript cr; show: 'c'; cr]))
	case: #b otherwise: [Transcript cr; show: 'other'; cr]"

	^(self at: aKey ifAbsent: otherwiseBlock) value! !
	"(#a -> 1), (#b -> 2), (#c -> 3)"

	self add: anAssociation.	^self! !
	aStream nextPut: $F! !
	aConvertor nextPutType: $F! !
	"Delay for the receiver in seconds"

	(Delay forSeconds: self) wait.! !
	aConvertor nextPutType: $f! !
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream fractionDigits: digits.
	^aStream contents! !
	"Answer a MIDI pitch whose value is the receiver."

	^MIDIPitch value: self! !
	"pad beginning with 0's"

	aConvertor next: 4 - self digitLength put: 0.
	self digitLength
		to: 1
		by: -1
		do: [:i | aConvertor nextPut: (self digitAt: i)]! !
	"Coerces Points to true and everything else to false. Point overrides with ^true"

	^true! !
	"Answer a new Point that is the receiver's x and y as floating-point numbers."

	^x asFloat @ y asFloat! !
	"Answer a ZPoint with the receiver and the argument as coordinates."

	^ZPoint x: x y: y z: aZValue! !
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil! !
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high - low) + low)! !
	"Answer the next random number as a float in the given range."

	| value |
	value := self next.
	^(value * (high + 1 - low) + low) truncated! !
	self do: [:each | each toOSCBytes: aConvertor]! !
	aConvertor nextPut: $[.
	self do: [:each | each toOSCTypeOn: aConvertor].
	aConvertor nextPut: $]! !
	| printSize stream |
	stream := WriteStream on: (String new: 32).
	printSize := self size.
"	printSize > self maxPrintElements
		ifTrue: [printSize := self maxPrintElements].
"	(1 to: printSize)
		do: [ :index | (self at: index) printOn: stream]
		separatedBy: [stream nextPut: $.].
	^stream contents! !
	aConvertor nextPutType: $i! !
	self isEmpty ifTrue: [^self].
	aConvertor nextPutAll: self asByteArray.
				"Align to 4 byte boundry"
	4 - (self size \\ 4) timesRepeat: [aConvertor nextPut: 0]! !
	aConvertor nextPutType: $s! !
	"Answer a string with a carriage return character"

	^self with: Character cr! !
	aConvertor nextPutType: $s! !
	aConvertor nextPutType: $T! !
	^self! !
	aConvertor nextPutType: $N! !