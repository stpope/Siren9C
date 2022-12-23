!classDefinition: #MusicMagnitude category: #'Music-Models-Representation'!
Magnitude subclass: #MusicMagnitude
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!MusicMagnitude commentStamp: '<historical>' prior: 0!
Instances of the subclasses of MusicMagnitude are used to represent scalar magnitudes in musical objects. The class MusicMagnitude is a place for the music magnitudes to share their state and basic accessing behavior.

MusicMagnitudes generally implement precise mixed-mode arithmetic and comparison methods using a per-species generality table and auto-coercion within a species.

The "type abstraction" classes (Duration, Pitch, Amplitude, etc.) represent "what"; they are never instantiated; their class creation methods answer instances of their species, but they manage coercion within the classes of their species. The "representational abstraction" classes (NominalMagnitude, OrdinalMagnitude, NumericalMagnitude, RatioMagnitude, etc.) represent "how"; they share value model semantics and coercion techniques. Note that the concrete implementation classes therefore answer their species by property model rather than by implementation subclass (i.e., HertzPitch species --> Pitch rather than NumericalMagnitude).

MusicMagnitudes can have dependents (observers), and send themselves the changed: message when their values are changed.

There are many examples in the implementation classes and the Siren  outline.

Instance variables:
	value		<Number or Symbol> the magnitude's value
!


!MusicMagnitude methodsFor: 'accessing'!
, anArgument
	"Answer en Event with the given magnitude as its first property."
	"440 Hz, 250 msec"

	| event |
	event := MusicEvent new.
	event perform: self species propertyName with: self.
	anArgument isMusicMagnitude ifTrue:
		[event perform: anArgument species propertyName with: anArgument].
	^event! !

!MusicMagnitude methodsFor: 'accessing'!
value
	"Answer the instance variable 'value'."

	^value! !

!MusicMagnitude methodsFor: 'accessing'!
value: theValue
	"Accept the argument, 'theValue', as the new instance variable 'value' 
		and signal any dependents."

	value := theValue.
	self changed: #value with: value! !


!MusicMagnitude methodsFor: 'printing'!
printOn: aStream
	"Print the receiver on the argument as a MM declaration."

	^self printOn: aStream parens: true! !

!MusicMagnitude methodsFor: 'printing'!
printOn: aStream parens: useParens
	"Print the receiver on the argument as a MM declaration."

	useParens
		ifTrue: [self isImmediate
			ifFalse: [aStream nextPutAll: '(', self species name, ': ', 
				self printValue, ')']
			ifTrue: [aStream nextPutAll: '(', self printValue, self units, ')']]
		ifFalse: [self isImmediate
			ifFalse: [aStream nextPutAll: self species name, ': ', self printValue]
			ifTrue: [aStream nextPutAll: self printValue, self units]]! !

!MusicMagnitude methodsFor: 'printing'!
printString
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream  parens: true.
	^aStream contents! !

!MusicMagnitude methodsFor: 'printing'!
printStringParens: bool
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream  parens: bool.
	^aStream contents! !

!MusicMagnitude methodsFor: 'printing'!
printValue

	value isString ifTrue: [^value].
"	value isFloat ifTrue: [^value printPlaces: 4].
"	^value printString! !

!MusicMagnitude methodsFor: 'printing'!
storeOn: aStream
	"Store the receiver on the argument as a MM declaration."

	aStream nextPutAll: '(', self class name, ' value: ', value printString, ')'! !

!MusicMagnitude methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^nil! !


!MusicMagnitude methodsFor: 'private'!
species
	"Answer MusicMagnitude."

	^MusicMagnitude! !


!MusicMagnitude methodsFor: 'converting'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

	^self class value: (rcvr perform: selector with: self value)! !

!MusicMagnitude methodsFor: 'converting'!
coerceTo: type
	"Coerce the receiver to the given class and answer a new object."

	^self perform: type ddMsgName! !

!MusicMagnitude methodsFor: 'converting'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^self class ddMsgName! !

!MusicMagnitude methodsFor: 'converting'!
msec
	"Answer a MSecondDuration."

	^self asMsec! !

!MusicMagnitude methodsFor: 'converting'!
usec
	"Answer a MSecondDuration."

	^self asUsec! !


!MusicMagnitude methodsFor: 'testing'!
isImmediate
	"Answer whether the receiver has an immediate representation."

	^self units ~~ nil! !

!MusicMagnitude methodsFor: 'testing'!
isMusicMagnitude
	"Answer true for the receiver and all subsclasses."

	^true! !

!MusicMagnitude methodsFor: 'testing' stamp: 'stp 11/28/2022 21:38:08'!
isZero
	"Answer falst at all times."

	^false! !


!MusicMagnitude methodsFor: 'arithmetic'!
* aValue
	"Answer the product of the receiver and the argument"

	| arr more less |
	^(aValue class == self class)
		ifTrue: [self class new value: (self value * aValue value)]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				less := arr last.
				more class new value: (more value * (less coerceTo: more class) value)]
			ifFalse: [self class new value: (value * aValue)]]! !

!MusicMagnitude methodsFor: 'arithmetic'!
+ aValue
	"Answer the sum of the receiver and the argument, doing generality-based
	class coercion within a species using auto-generated coercion messages."
	"If the same class, add values directly."
	"If the same species, add values coercing generality."
	"1 sec + 10 msec"

	| arr more less |
	^(aValue class == self class)
		ifTrue: [self class new value: (self value + aValue value)]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				less := arr last.
				more class new value: (more value + (less coerceTo: more class) value)]
			ifFalse: [self class new value: (value + aValue)]]! !

!MusicMagnitude methodsFor: 'arithmetic'!
- aValue
	"Answer the difference of the receiver and the argument"

	| arr more less |
	^(aValue class == self class)
		ifTrue: [self class new value: (self value - aValue value)]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				less := (arr last coerceTo: more class) value.
				more == self
					ifTrue: [more class new value: (more value - less)]
					ifFalse: [more class new value: (less - more value)]]
			ifFalse: [self class new value: (value - aValue)]]! !

!MusicMagnitude methodsFor: 'arithmetic'!
/ aValue
	"Answer the quotient of the receiver and the argument"

	| arr more less |
	^(aValue class == self class)
		ifTrue: [self class new value: (self value / aValue value)]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				less := (arr last coerceTo: more class) value.
				more == self
					ifTrue: [more class new value: (more value / less)]
					ifFalse: [more class new value: (less / more value)]]
			ifFalse: [self class new value: (value / aValue)]]! !


!MusicMagnitude methodsFor: 'comparing'!
< aValue
	"Answer whether the receiver is less than the argument"
	"100 msec < 0.2 sec"

	| arr more |
	^(aValue class == self class)
		ifTrue: [self value < aValue value]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				more == self 		"if I'm the more general"
					ifTrue: [aValue value < ((self coerceTo: aValue class) value)]
					ifFalse: [self value < ((aValue coerceTo: self class) value)]]
			ifFalse: [value < aValue]]! !

!MusicMagnitude methodsFor: 'comparing'!
= aValue
	"Answer whether the receiver and the argument are equal."

	| arr more less |
	^(aValue class == self class)
		ifTrue: [self value = aValue value]
		ifFalse: [(aValue species == self species)
			ifTrue: [arr := self species generalize: self and: aValue.
				more := arr first.
				less := arr last.
				(more value = (less coerceTo: more class) value)]
			ifFalse: [value = aValue]]! !

!MusicMagnitude methodsFor: 'comparing'!
hash
	"Answer a SmallInteger unique to the receiver."

	^value hash! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MusicMagnitude class' category: #'Music-Models-Representation'!
MusicMagnitude class
	instanceVariableNames: ''!

!MusicMagnitude class methodsFor: 'instance creation'!
value: aValue
	"Answer and instance with the argument as its value."

	^self new value: aValue! !


!MusicMagnitude class methodsFor: 'class constants'!
color
	"Answer the default color to display instances with (hack for making pretty graphs)."

	| spec |
	spec := self new species.
	^(spec == MusicMagnitude)
		ifTrue: [ColorValue gray]
		ifFalse: [spec color]! !

!MusicMagnitude class methodsFor: 'class constants'!
propertyName
	"Answer the receiver class's default property name."

	^self subclassResponsibility! !

!MusicMagnitude class methodsFor: 'class constants'!
relativeMember
	"Answer the receiver class's species member used as a relative magnitude."

	^self subclassResponsibility! !


!classDefinition: #MusicModel category: #'Music-Models-Representation'!
MusicMagnitude subclass: #MusicModel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!MusicModel commentStamp: '<historical>' prior: 0!
MusicModel adds the class instance variable generalityTable that is used for "abstract" music magnitude models such as Pitch and Amplitude.

See the class methods for generality.

Class inst Vars:
	generalityTable <Dictionary of (Class ->Integer)> The species generality table
!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MusicModel class' category: #'Music-Models-Representation'!
MusicModel class
	instanceVariableNames: ''!

!MusicModel class methodsFor: 'generality'!
generality
	"Answer the class inst var for the generality table."

	^generalityTable! !

!MusicModel class methodsFor: 'generality' stamp: 'stp 11/20/2022 11:04:46'!
generalize: aMag and: otherMag
	"Answer an array of the two arguments with the most general first."

	^(self generalityTable at: aMag class) > (self generalityTable at: otherMag class)
		ifTrue: [Array with: aMag with: otherMag]
		ifFalse: [Array with: otherMag with: aMag]! !


!MusicModel class methodsFor: 'examples'!
examples

	"Pitch generalize: (#c pitch) and: (21 key)"
	"Pitch generalize: (#c pitch) and: (220 Hz)"
	"Pitch generalize: (220 Hz) and: (#c pitch)"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"
	"Duration generalize: (100 msec) and: ((1/4) beat)"
	"Duration generalize: (100 msec) and: (0.3 sec)"

	 (#ff ampl) - (32 velocity)! !


!classDefinition: #Chroma category: #'Music-Models-Representation'!
MusicModel subclass: #Chroma
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Chroma commentStamp: '<historical>' prior: 0!
Class Chroma is the abstract representational class for the pitch and mode-element species.
Instances of its subclass species are used to model pitches, gamut members, and frequencies.!



!classDefinition: #ModeMember category: #'Music-Models-Representation'!
Chroma subclass: #ModeMember
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!ModeMember commentStamp: '<historical>' prior: 0!
Instances of the ModeMember species model pitches as elements of a mode (e.g., minor) or gamut (e.g., pentatonic on F).
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number or pitch, and the mode or gamut could be shared.!



!classDefinition: #Pitch category: #'Music-Models-Representation'!
Chroma subclass: #Pitch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Pitch commentStamp: '<historical>' prior: 0!
Instances of classes whose species is Pitch are used for the pitch or frequency parameters of musical events.
Pitches come in several flavors, as in the classes HertzPitch, RatioPitch, MIDIPitch and SymbolicPitch.
The class Pitch is abstract, its class creation method answers an instance of a concrete Pitch class.

Examples:
	Pitch value: 440.0 	"create an instance with units of Hertz"
	Pitch value: 77		"create an instance with units of MIDI key numbers"
	Pitch value: #e4		"create a symbolic instance"
	Pitch value: 'e4'		"same as using a symbol"
	Pitch value: 4/3		"create a ratio instance"

Note that new pitch representations such as music11-like pch (4.11 = 11th note in oct 4) or oct (4.1100 = oct4 + 1100 cts) notations can be added by overriding the float-to-Hz or float-to-midi conversions.

See also the class example.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Pitch class' category: #'Music-Models-Representation'!
Pitch class
	instanceVariableNames: 'generalityTable'!

!Pitch class methodsFor: 'instance creation' stamp: 'stp 11/28/2022 21:41:59'!
value: aValue
	"Answer a new instance of a member of my species."

	aValue species == Pitch ifTrue: [^aValue].
	(aValue isMemberOf: SmallInteger)
			ifTrue: [^MIDIPitch new value: aValue].
	(aValue isKindOf: Float)
			ifTrue: [^HertzPitch new value: aValue].
	(aValue isMemberOf: Fraction)
			ifTrue: [^RatioPitch new value: aValue].
	(aValue isKindOf: CharacterSequence)
			ifTrue: [^SymbolicPitch new value: aValue asSymbol].
	^self new value: aValue! !


!Pitch class methodsFor: 'class constants'!
color
	"Answer the default color to display instances with."

	^ColorValue green! !

!Pitch class methodsFor: 'class constants' stamp: 'stp 11/30/2022 11:53:29'!
generalityTable
	"Answer the class inst var"

	^generalityTable! !

!Pitch class methodsFor: 'class constants'!
initialize
	"Set up the class inst var, a generality table."
	"Pitch initialize"
	"Pitch generalize: (#c pitch) and: (21 key)"
	"Pitch generalize: (#c pitch) and: (220 Hz)"
	"Pitch generalize: (220 Hz) and: (#c pitch)"

	generalityTable :=
		((HertzPitch -> 80),
		(RatioPitch -> 60),
		(MIDIPitch -> 40),
		(SymbolicPitch -> 20))! !

!Pitch class methodsFor: 'class constants'!
initializeGenerality
	"Set up the class inst var, a generality table."
	"Pitch initializeGenerality"
	"Pitch generalize: (#c pitch) and: (21 key)"
	"Pitch generalize: (#c pitch) and: (220 Hz)"
	"Pitch generalize: (220 Hz) and: (#c pitch)"

	generalityTable :=
		((HertzPitch -> 80),
		(RatioPitch -> 60),
		(MIDIPitch -> 40),
		(SymbolicPitch -> 20))! !

!Pitch class methodsFor: 'class constants'!
mostGeneral
	"Answer the most general-purpose duration--Hertz"

	^#asHz! !

!Pitch class methodsFor: 'class constants'!
propertyName
	"Answer the receiver class's default property name."

	^#pitch:! !

!Pitch class methodsFor: 'class constants'!
relativeMember
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioPitch! !

!Pitch class methodsFor: 'class constants'!
species
	"Answer Pitch."

	^Pitch! !


!Pitch class methodsFor: 'examples'!
example
	"Print a simple message to the transcript demonstrating the various types."
	"SymbolicPitch nameMap inspect"
	"Pitch example"

	| me |
	me := Pitch value: 'c3'.
	Transcript show: me printString; cr;
		show: me asHertz printString; cr;
		show: me asMIDI printString; cr.
	me inspect! !

!Pitch class methodsFor: 'examples'!
exampleAdC
"Pitch exampleAdC"
	
	"Some examples of where current Pitch implementation is problematic:"

	"440.0 Hz asSymbol"	"shows up as a5, not the standard MIDIname a3."
	"445 Hz asMIDI"		"is quantized down OK, but should be a3/note 69 also."	
	"439.9 Hz asSymbol"			"gets quantized down to g#!!"
	"60 key asHertz asMIDI"		"does not survive conversion as MIDInote 60."

	"My suggestions:"
	"438 Hz asSymbol"  	"rounds to nearest chromatic note, a3."
	"443.5 Hz asMIDI"	"detto."
	"265 Hz asFracMIDI"	"converts to float chromatics; can be rounded, used 						for MIDI pitch bend or for precise synthesis in Hz."
	"61.26 key asHertz"	"float chromatics can also be used directly; for 					microtonal scales this is clearer than Hz (to me at least)."

	"260.0 Hz asFracSymbol inspect" 	"is rounded, but keeps track of offsets in  						an inst var (fracPitch); survives conversions etc."

	"Note that asMIDI and asSymbol can now be used to round pitches to 	chromatics, while asFracMIDI and asFracSymbol keep microtonal 	precision."! !


!classDefinition: #Chronos category: #'Music-Models-Representation'!
MusicModel subclass: #Chronos
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Chronos commentStamp: '<historical>' prior: 0!
Class Chronos is the abstract representational class for the duration and meter species.
Instances of its subclass species are used to model times, durations and metronomes.!



!classDefinition: #Duration category: #'Music-Models-Representation'!
Chronos subclass: #Duration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Duration commentStamp: '<historical>' prior: 0!
Instances of classes whose species is Duration are used for the duration parameters of musical events.
Durations come in several flavors, as in the classes RatioDuration, MSecondDuration and ConditionalDuration.
The class Duration is abstract, its class creation method answers an instance of a concrete Duration class.

Examples:
	Duration value: 0.77		"create an instance with seconds as the unit"
	Duration value: 770		"create an instance with milliseconds as the unit"
	Duration value: 1/4		"create an instance with beats as the unit"
	Duration value: [ :x | x > 4]	"create an instance for: 'until x > 4'"

See also the class example.!


!Duration methodsFor: 'public protocol' stamp: 'jmv 5/24/2016 17:11'!
totalMilliseconds

	^ (seconds * 1000) + (nanos // 1e6)! !

!Duration methodsFor: 'public protocol' stamp: 'jmv 6/30/2011 09:01'!
totalMinutes
	"Answer an integer number of minutes"
	^ seconds // 60! !

!Duration methodsFor: 'public protocol' stamp: 'jmv 4/17/2013 00:32'!
totalNanoSeconds

	^ (seconds * Time nanosInSecond) + nanos! !

!Duration methodsFor: 'public protocol' stamp: 'jmv 4/9/2010 13:38'!
totalSeconds

	^ seconds! !


!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/9/2010 14:07'!
* operand
	"operand is a Number" 	^ self class nanoSeconds: ( (self totalNanoSeconds * operand) asInteger).
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/9/2010 14:07'!
+ operand

	"operand is a Duration" 	^ self class nanoSeconds: (self totalNanoSeconds + operand totalNanoSeconds)
! !

!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 07:59'!
- operand
	"operand is a Duration" 	^ self + operand negated
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/9/2010 14:08'!
/ operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds / operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds / operand totalNanoSeconds ]
.
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/9/2010 14:07'!
< comparand

	^ self totalNanoSeconds < comparand totalNanoSeconds
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2014 11:22'!
= comparand 
	"Answer whether the argument is a <Duration> representing the same 
	period of time as the receiver."

	self == comparand
		ifTrue: [ ^ true ].

	self species == comparand species 
		ifFalse: [ ^ false ].

	^ self totalNanoSeconds = comparand totalNanoSeconds! !

!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:01'!
abs

	^ self class seconds: seconds abs nanoSeconds: nanos abs
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
days
	"Answer the number of days the receiver represents."

	^ seconds quo: Time secondsInDay! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 11/4/2019 11:10:43'!
hash
	^seconds hash bitXor: nanos hash! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
hours
	"Answer the number of hours the receiver represents."


	^ (seconds rem: Time secondsInDay) quo: Time secondsInHour! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:34'!
minutes
	"Answer the number of minutes the receiver represents."

	^ (seconds rem: Time secondsInHour) quo: Time secondsInMinute! !

!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!
negated

	^ self class seconds: seconds negated nanoSeconds: nanos negated
! !

!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!
negative


	^ self positive not
! !

!Duration methodsFor: 'ansi protocol' stamp: 'brp 5/13/2003 08:02'!
positive


	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]
! !

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:34'!
seconds
	"Answer the number of seconds the receiver represents."

	^seconds rem: Time secondsInMinute! !


!Duration methodsFor: 'squeak protocol' stamp: 'jmv 4/9/2010 14:08'!
// operand

	"operand is a Duration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds // operand) asInteger ]
		ifFalse: [
			self assert: operand class == Duration.
			self totalNanoSeconds // operand totalNanoSeconds ]
! !

!Duration methodsFor: 'squeak protocol' stamp: 'jmv 4/9/2010 14:07'!
\\ operand

	"modulo. Remainder defined in terms of //. Answer a Duration with the 
	same sign as aDuration. operand is a Duration or a Number."

	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds \\ operand) ]
		ifFalse: [ self - (operand * (self // operand)) ]
! !

!Duration methodsFor: 'squeak protocol' stamp: 'brp 9/25/2003 13:42'!
asDelay

	^ Delay forDuration: self! !

!Duration methodsFor: 'squeak protocol' stamp: 'brp 4/13/2006 10:20'!
isZero

	^ seconds = 0 and: [ nanos = 0 ]
! !

!Duration methodsFor: 'squeak protocol' stamp: 'brp 5/13/2003 08:03'!
nanoSeconds


	^ nanos
! !

!Duration methodsFor: 'squeak protocol' stamp: 'jmv 3/13/2020 08:18:50'!
printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
	"
	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	m printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	s printOn: aStream length: 2 zeroPadded: true.
	n = 0 ifFalse: [
		| z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c digitValue > 0 ].
		z _ #(3 6 9) detect: [ :ez | ez >= z ].	"print either milliseconds, microseconds or nanoseconds"
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ]! !

!Duration methodsFor: 'squeak protocol' stamp: 'jmv 4/9/2010 14:07'!
roundTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 6 minutes."

	^ self class nanoSeconds: (self totalNanoSeconds roundTo: aDuration totalNanoSeconds)

! !

!Duration methodsFor: 'squeak protocol' stamp: 'jmv 4/9/2010 14:08'!
truncateTo: aDuration
	"e.g. if the receiver is 5 minutes, 37 seconds, and aDuration is 2 minutes, answer 4 minutes."

	^ self class
		nanoSeconds: (self totalNanoSeconds truncateTo: aDuration totalNanoSeconds)

! !


!Duration methodsFor: 'private' stamp: 'jmv 4/17/2013 00:32'!
seconds: secondCount nanoSeconds: nanoCount 
	"Private - only used by Duration class"

	seconds := secondCount.
	nanos := nanoCount rounded.
	"normalize if signs do not match"
	[ nanos < 0 and: [ seconds > 0 ] ]
		whileTrue: [ seconds := seconds - 1.
			nanos := nanos + Time nanosInSecond ].
	[ seconds < 0 and: [ nanos > 0 ] ]
		whileTrue: [ seconds := seconds + 1.
			nanos := nanos - Time nanosInSecond ]

! !

!Duration methodsFor: 'private' stamp: 'brp 9/25/2003 14:42'!
storeOn: aStream

	aStream
		nextPut: $(;
		nextPutAll: self className;
		nextPutAll: ' seconds: ';
		print: seconds;
		nextPutAll: ' nanoSeconds: ';
		print: nanos;
		nextPut: $).
! !


!Duration methodsFor: 'initialization' stamp: 'nk 3/30/2004 10:01'!
initialize
	self seconds: 0 nanoSeconds: 0.
! !


!Duration methodsFor: 'double dispatching' stamp: 'pb 9/13/2017 06:21:11'!
subtractFrom: aDateAndTimeOrDate

	^aDateAndTimeOrDate subtractDuration: self! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Duration class' category: #'Music-Models-Representation'!
Duration class
	instanceVariableNames: 'generalityTable'!

!Duration class methodsFor: 'ansi protocol' stamp: 'gk 8/31/2006 01:09'!
days: days hours: hours minutes: minutes seconds: seconds

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: 0! !

!Duration class methodsFor: 'ansi protocol' stamp: 'jmv 4/17/2013 00:35'!
days: days seconds: seconds

	^ self basicNew seconds: days * Time secondsInDay + seconds nanoSeconds: 0! !

!Duration class methodsFor: 'ansi protocol' stamp: 'gk 8/31/2006 01:34'!
seconds: seconds

	^ self seconds: seconds nanoSeconds: 0
! !

!Duration class methodsFor: 'ansi protocol' stamp: 'gk 8/31/2006 00:09'!
zero

	^ self basicNew seconds: 0 nanoSeconds: 0
! !


!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
days: aNumber

	^ self seconds: aNumber * Time secondsInDay nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: nanos	

 	^ self seconds: ((days * Time secondsInDay) 
						+ (hours * Time secondsInHour)
							+ (minutes * Time secondsInMinute) 
								+ seconds)
		nanoSeconds: nanos! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 6/24/2021 10:19:10'!
fromString: aString
	"
	Extended to accept non-ANSI formats, such as:
		Duration fromString: '1:50:00'
		Duration fromString: '5:30.5'
		Duration fromString: '1:6:50:00'
		Duration fromString: '3.5h'
		Duration fromString: '2.5min'
		Duration fromString: '1.5s'
		Duration fromString: '200s'
		Duration fromString: '200ms'
	"
	| colons s |

	"If aString includes at least one $:, complete ANSI format"
	colons _ aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		s _ aString.
		[colons < 3] whileTrue: [
			s _ '00:', s.
			colons _ colons + 1 ].
		^ self readFrom: (ReadStream on: s) ].

	"'3.5h' means 3.5 hours"
	(aString endsWith: 'h') ifTrue: [
		^self hours: aString asNumber ].

	"'3.5min' means 3.5 minutes"
	(aString endsWith: 'min') ifTrue: [
		^self minutes: aString asNumber ].

	"'3ms' means 3 milliseconds"
	(aString endsWith: 'ms') ifTrue: [
		^self milliSeconds: aString asNumber ].

	"'3.5s' means 3.5 seconds"
	(aString endsWith: 's') ifTrue: [
		^self seconds: aString asNumber ].
	
	^nil! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:35'!
hours: aNumber

	^ self seconds: aNumber * Time secondsInHour nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:31'!
milliSeconds: milliCount

	^ self nanoSeconds: milliCount * 1000000! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:34'!
minutes: aNumber

	^ self seconds: aNumber * Time secondsInMinute nanoSeconds: 0! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:32'!
nanoSeconds: nanos
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo _ nanos quo: Time nanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * Time nanosInSecond)! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 6/24/2021 10:17:17'!
readFrom: aStream
	"Formatted as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]"

	| sign days hours minutes seconds nanos nanosBuffer |
	sign := (aStream peekFor: $-) ifTrue: [-1] ifFalse: [1].
	days := (aStream upTo: $:) findPositiveInteger * sign.
	hours := (aStream upTo: $:) findPositiveInteger * sign.
	minutes := (aStream upTo: $:) findPositiveInteger * sign.
	seconds := (aStream upTo: $.) findPositiveInteger * sign.
	nanosBuffer := '000000000' copy.
	nanos := WriteStream on: nanosBuffer.
	[aStream atEnd not and: [aStream peek isDigit]]
		whileTrue: [nanos nextPut: aStream next].
		
	^ self 
		days: days 
		hours: hours 
		minutes: minutes 
		seconds: seconds 
		nanoSeconds: (nanosBuffer findPositiveInteger * sign)

	"
		Duration readFrom: '0:00:00:00' readStream
		Duration readFrom: '0:00:00:00.000000001' readStream
		Duration readFrom: '0:00:00:00.9' readStream
		Duration readFrom: '0:00:00:00.99' readStream
		Duration readFrom: '0:00:00:00.99999999' readStream
		Duration readFrom: '0:00:00:00.999999999' readStream
		Duration readFrom: '0:00:00:00.100000000' readStream
		Duration readFrom: '0:00:00:00.001 ' readStream
		Duration readFrom: '0:00:00:00.1' readStream
		Duration readFrom: '0:00:00:01 ' readStream
		Duration readFrom: '0:12:45:45' readStream
		Duration readFrom: '1:00:00:00' readStream
		Duration readFrom: '365:00:00:00' readStream
		Duration readFrom: '-7:09:12:06.10' readStream
		Duration readFrom: '+0:01:02:3' readStream
 	"! !

!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 4/17/2013 00:32'!
seconds: seconds nanoSeconds: nanos

	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * Time nanosInSecond + nanos! !

!Duration class methodsFor: 'squeak protocol' stamp: 'gk 8/30/2006 23:20'!
weeks: aNumber

	^ self days: (aNumber * 7) seconds: 0
! !


!Duration class methodsFor: 'instance creation'!
value: aValue
	"Answer a new instance of a member of my species."

	| number |
	aValue species == Duration ifTrue: [^aValue].
	(aValue isMemberOf: SmallInteger)
		ifTrue: [aValue < 10
			ifTrue: [^SecondDuration new value: aValue]
			ifFalse: [^MSecondDuration new value: aValue]].
	(aValue isKindOf: LimitedPrecisionReal)
		ifTrue: [^SecondDuration new value: aValue].
	(aValue isMemberOf: Fraction)
		ifTrue: [^RatioDuration new value: aValue].
	(aValue isKindOf: String)
		ifTrue: [number := Number readFrom: (ReadStream on: aValue).
			number = 0 ifTrue: [^self error: 'Unknown duration value.'].
			^Duration value: number].
	(aValue isMemberOf: BlockContext)
		ifTrue: [^ConditionalDuration new value: aValue].
"	^self error: 'Unknown duration type.'"
	^aValue! !


!Duration class methodsFor: 'class constants'!
color
	"Answer the default color to display instances with."

	^ColorValue red! !

!Duration class methodsFor: 'class constants' stamp: 'stp 11/20/2022 11:05:29'!
generalityTable
	"Answer the class inst var"

	^generalityTable! !

!Duration class methodsFor: 'class constants'!
initialize
	"Set up the class inst var, a generality table."
	"Duration initialize"
	"Duration generalize: (100 msec) and: ((1/4) beat)"
	"Duration generalize: (100 msec) and: (0.3 sec)"

	generalityTable :=
		((SecondDuration -> 80),
		(RatioDuration -> 60),
		(MSecondDuration -> 40),
		(ConditionalDuration -> 0))! !

!Duration class methodsFor: 'class constants'!
initializeGeneralities
	"Set up the class inst var, a generality table."
	"Duration initializeGeneralities"
	"Duration generalize: (100 msec) and: ((1/4) beat)"
	"Duration generalize: (100 msec) and: (0.3 sec)"

	generalityTable :=
		((SecondDuration -> 80),
		(RatioDuration -> 60),
		(MSecondDuration -> 40),
		(ConditionalDuration -> 0))! !

!Duration class methodsFor: 'class constants'!
mostGeneral
	"Answer the most general-purpose duration--seconds"

	^#asSec! !

!Duration class methodsFor: 'class constants'!
propertyName
	"Answer the receiver class's default property name."

	^#duration:! !

!Duration class methodsFor: 'class constants'!
relativeMember
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioDuration! !

!Duration class methodsFor: 'class constants'!
species
	"Answer Duration."

	^Duration! !


!Duration class methodsFor: 'examples'!
example
	"Print a simple message to the transcript demonstrating the various types."
	"Duration example"

	| me |
	me := Duration value: 1/4.
	Transcript show: me printString; cr;
		show: me asMS printString; cr.
	me inspect! !


!classDefinition: #Meter category: #'Music-Models-Representation'!
Chronos subclass: #Meter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Meter commentStamp: '<historical>' prior: 0!
Instances of the Meter species model the tempo or metronome used to map durations.
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number, process or block.!



!classDefinition: #Ergon category: #'Music-Models-Representation'!
MusicModel subclass: #Ergon
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Ergon commentStamp: '<historical>' prior: 0!
Class Ergon is the abstract representational class for the amplitude/loudness/dynamic species.
Instances of its subclass species are used to model loudness values.!



!classDefinition: #Amplitude category: #'Music-Models-Representation'!
Ergon subclass: #Amplitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Amplitude commentStamp: '<historical>' prior: 0!
Instances of classes whose species is Amplitude are used for the loudness parameters of musical events.
Amplitudes come in several flavors, as in the classes RatioMagnitude, MIDIVelocity and SymbolicLoudness.
The class Amplitude is abstract, its class creation method answers an instance of a concrete Amplitude/Loudness class.

Examples:
	Amplitude value: 0.77		"create a ratio instance - range 0.0 to 1.0 (cmusic)"
	Amplitude value: 77		"create an MIDI instance - range 0 to 127"
	Amplitude value: #mp		"create a symbolic instance - range #ppp to #fff"

See also the class example.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Amplitude class' category: #'Music-Models-Representation'!
Amplitude class
	instanceVariableNames: 'generalityTable'!

!Amplitude class methodsFor: 'instance creation' stamp: 'stp 11/28/2022 21:42:54'!
value: aValue
	"Answer a new instance of a member of my species."

	aValue species == Amplitude ifTrue: [^aValue].
	(aValue isMemberOf: SmallInteger)
			ifTrue: [^MIDIVelocity new value: aValue].
	(aValue isKindOf: Float)
			ifTrue: [^RatioLoudness new value: aValue].
	(aValue isMemberOf: Fraction)
			ifTrue: [^RatioLoudness new value: aValue].
	(aValue isKindOf: CharacterSequence)
			ifTrue: [^SymbolicLoudness new value: aValue asSymbol].
	^self new value: aValue! !


!Amplitude class methodsFor: 'class constants'!
color
	"Answer the default color to display instances with."

	^ColorValue blue! !

!Amplitude class methodsFor: 'class constants' stamp: 'stp 11/30/2022 11:54:30'!
generalityTable
	"Answer the class inst var"

	^generalityTable! !

!Amplitude class methodsFor: 'class constants'!
initialize
	"Set up the class inst var, a generality table."
	"Amplitude initialize"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"

	generalityTable :=
		((DBLoudness -> 80),
		(RatioLoudness -> 80),
		(MIDIVelocity -> 40),
		(SymbolicLoudness -> 20))! !

!Amplitude class methodsFor: 'class constants'!
initializeGenerality
	"Set up the class inst var, a generality table."
	"Amplitude initialize"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"

	generalityTable :=
		((DBLoudness -> 80),
		(RatioLoudness -> 80),
		(MIDIVelocity -> 40),
		(SymbolicLoudness -> 20))! !

!Amplitude class methodsFor: 'class constants'!
mostGeneral
	"Answer the most general-purpose duration--relative"

	^#asRatio! !

!Amplitude class methodsFor: 'class constants'!
propertyName
	"Answer the receiver class's default property name."

	^#loudness:! !

!Amplitude class methodsFor: 'class constants'!
relativeMember
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioLoudness! !

!Amplitude class methodsFor: 'class constants'!
species
	"Answer Amplitude."

	^Amplitude! !


!Amplitude class methodsFor: 'examples'!
example
	"Print a simple message to the transcript demonstrating the various types."
	"SymbolicLoudness nameMap inspect"
	"Amplitude example"

	| me |
	me := Amplitude value: #mf.
	Transcript show: me printString; cr;
		show: me asRatio printString; cr;
		show: me asMIDI printString; cr.
	me inspect! !


!classDefinition: #Positus category: #'Music-Models-Representation'!
MusicModel subclass: #Positus
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Positus commentStamp: '<historical>' prior: 0!
Class Positus is the abstract representational class for the position, space, and direction species.
Instances of its subclass species are used to model spatial and positional values.!



!classDefinition: #Directionality category: #'Music-Models-Representation'!
Positus subclass: #Directionality
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Directionality commentStamp: '<historical>' prior: 0!
Instances of the Directionality species model the radiation characteristics of sound sources in room simulations.
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable would hold a 1- or 2-dimensional position as a number or point.!



!classDefinition: #Position category: #'Music-Models-Representation'!
Positus subclass: #Position
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Position commentStamp: '<historical>' prior: 0!
Instances of the Position species model the position of sound sources in room simulations or scores.
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number or point.!



!classDefinition: #Spatialization category: #'Music-Models-Representation'!
Positus subclass: #Spatialization
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!Spatialization commentStamp: '<historical>' prior: 0!
Instances of the Spatialization species model the characteristics and configuration of room simulations in scores.
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable would hold a record with the geometry or the room, the positions of default sources, and the listener's position and features.!



!classDefinition: #PField category: #'Music-Models-Representation'!
MusicMagnitude subclass: #PField
	instanceVariableNames: 'name field'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!PField commentStamp: '<historical>' prior: 0!
Instances of PField magnitudes are used for arbitrary parameters of musical events oriented towards formats with positional parameter declarations, e.g., Music-V-style notelist formats (see uses).

Instance variables:
	field <Integer> field in the score
	name <String or Symbol> the parameter's name (optional)

Examples:
	PField field: 5 value: 0.7 name: #position
	PField field: 9 value: 'legato' name: #expression
!


!PField methodsFor: 'accessing'!
field
	"Answer the instance variable 'field'."

	^field! !

!PField methodsFor: 'accessing'!
field: theNumber
	"Accept the argument, 'theNumber', as the new instance nariable 'field'."

	field := theNumber! !

!PField methodsFor: 'accessing'!
name
	"Answer the instance variable 'name'."

	^name! !

!PField methodsFor: 'accessing'!
name: theName
	"Accept the argument, 'theName', as the new instance nariable 'name'."

	name := theName! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'PField class' category: #'Music-Models-Representation'!
PField class
	instanceVariableNames: ''!

!PField class methodsFor: 'instance creation'!
field: aNumber value: aValue
	"Answer an initialized instance."

	^(self new) field: aNumber; value: aValue! !

!PField class methodsFor: 'instance creation'!
field: aNumber value: aValue name: aName
	"Answer an initialized instance."

	^(self new) field: aNumber; value: aValue; name: aName! !

Pitch initialize!
Duration initialize!
Amplitude initialize!