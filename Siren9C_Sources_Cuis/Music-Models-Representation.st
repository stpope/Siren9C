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

!MusicMagnitude methodsFor: 'printing' stamp: 'stp 1/7/2023 21:55:27'!
printOn: aStream parens: useParens 
	"Print the receiver on the argument as a MM declaration."
	| digits |
	Sensor shiftPressed
		ifTrue: [digits := 10]
		ifFalse: [digits := 4].
	useParens
		ifTrue: [aStream nextPutAll: '('].
	aStream nextPutAll: self species name , ': '.
	value isFloat
		ifTrue: [value printOn: aStream fractionDigits: digits]
		ifFalse: [value printOn: aStream].
	useParens
		ifTrue: [aStream nextPutAll: ')']! !

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
storeOn: aStream
	"Store the receiver on the argument as a MM declaration."

	aStream nextPutAll: '(', self class name, ' value: ', value printString, ')'! !

!MusicMagnitude methodsFor: 'printing' stamp: 'stp 12/30/2022 12:49'!
units
	"Answer the units string of the receiver."
	^ ''! !


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


!MusicMagnitude class methodsFor: 'class constants' stamp: 'stp 12/31/2022 11:02'!
color
	"Answer the default color to display instances with (hack for making
	pretty graphs)."
	| spec |
	spec := self new species.
	^ spec == MusicMagnitude
		ifTrue: [Color gray]
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
	instanceVariableNames: 'generalityTable'!

!MusicModel class methodsFor: 'generality' stamp: 'stp 12/28/2022 13:42:58'!
generalityTable
	"Answer the class inst var"

	^generalityTable! !

!MusicModel class methodsFor: 'generality' stamp: 'stp 12/28/2022 13:38:37'!
generalize: aMag and: otherMag
	"Answer an array of the two arguments with the most general first."

	^(generalityTable at: aMag class) > (generalityTable at: otherMag class)
		ifTrue: [Array with: aMag with: otherMag]
		ifFalse: [Array with: otherMag with: aMag]! !


!MusicModel class methodsFor: 'examples'!
examples

	"Pitch generalize: (#c pitch) and: (21 key)"
	"Pitch generalize: (#c pitch) and: (220 Hz)"
	"Pitch generalize: (220 Hz) and: (#c pitch)"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"
	"MDuration generalize: (100 msec) and: ((1/4) beat)"
	"MDuration generalize: (100 msec) and: (0.3 sec)"

	 (#ff ampl) - (32 velocity)! !


!MusicModel class methodsFor: 'class initialization' stamp: 'stp 12/28/2022 13:45:33'!
initialize
	"Set up the class inst var, a generality table."
	"MusicModel initialize"

	^self! !

!MusicModel class methodsFor: 'class initialization' stamp: 'stp 12/28/2022 13:45:50'!
initializeGenerality
	"Set up the class inst var, a generality table."
	"MusicModel initializeGenerality"

	MusicModel allSubclassesDo: [ :cl | cl initialize]! !


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
	instanceVariableNames: ''!

!Pitch class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 16:38'!
value: aValue 
	"Answer a new instance of a member of my species."
	aValue species == Pitch
		ifTrue: [^ aValue].
	(aValue isMemberOf: SmallInteger)
		ifTrue: [^ MIDIPitch new value: aValue].
	(aValue isKindOf: Float)
		ifTrue: [^ HertzPitch new value: aValue].
	(aValue isMemberOf: Fraction)
		ifTrue: [^ RatioPitch new value: aValue].
	(aValue isKindOf: String)
		ifTrue: [^ SymbolicPitch new value: aValue asSymbol].
	^ self new value: aValue! !


!Pitch class methodsFor: 'class constants' stamp: 'stp 12/31/2022 11:02'!
color
	"Answer the default color to display instances with."
	^ Color green! !

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



!classDefinition: #MDuration category: #'Music-Models-Representation'!
Chronos subclass: #MDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Representation'!
!MDuration commentStamp: '<historical>' prior: 0!
Instances of classes whose species is MDuration are used for the duration parameters of musical events.
Durations come in several flavors, as in the classes RatioDuration, MSecondDuration and ConditionalDuration.
The class MDuration is abstract, its class creation method answers an instance of a concrete Duration class.

Examples:
	MDuration value: 0.77		"create an instance with seconds as the unit"
	Duration value: 770		"create an instance with milliseconds as the unit"
	MDuration value: 1/4		"create an instance with beats as the unit"
	MDuration value: [ :x | x > 4]	"create an instance for: 'until x > 4'"

See also the class example.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MDuration class' category: #'Music-Models-Representation'!
MDuration class
	instanceVariableNames: ''!

!MDuration class methodsFor: 'class constants' stamp: 'stp 12/31/2022 11:01'!
color
	"Answer the default color to display instances with."
	^ Color red! !

!MDuration class methodsFor: 'class constants' stamp: 'stp 12/28/2022 13:58:18'!
initialize
	"Set up the class inst var, a generality table."
	"MDuration initialize"
	"MDuration generalize: (100 msec) and: ((1/4) beat)"
	"MDuration generalize: (100 msec) and: (0.3 sec)"

	generalityTable :=
		((SecondDuration -> 40),
		(RatioDuration -> 60),
		(USecondDuration -> 50),
		(MSecondDuration -> 50),
		(ConditionalDuration -> 0))! !

!MDuration class methodsFor: 'class constants'!
mostGeneral
	"Answer the most general-purpose duration--seconds"

	^#asSec! !

!MDuration class methodsFor: 'class constants'!
propertyName
	"Answer the receiver class's default property name."

	^#duration:! !

!MDuration class methodsFor: 'class constants' stamp: 'stp 12/31/2022 07:06'!
relativeMember
	"Answer the receiver class's species member used as a relative
	magnitude. "
	^ RatioDuration! !

!MDuration class methodsFor: 'class constants'!
species
	"Answer MDuration."

	^MDuration! !


!MDuration class methodsFor: 'examples'!
example
	"Print a simple message to the transcript demonstrating the various types."
	"MDuration example"

	| me |
	me := MDuration value: 1/4.
	Transcript show: me printString; cr;
		show: me asMS printString; cr.
	me inspect! !


!MDuration class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 13:35:11'!
value: aValue
	"Answer a new instance of a member of my species."

	| number |
	aValue species == MDuration ifTrue: [^aValue].
	(aValue isMemberOf: SmallInteger)
		ifTrue: ["aValue < 100
			ifTrue: [^SecondMDuration new value: aValue]
			ifFalse: ["^MSecondDuration new value: aValue"]"].
	(aValue isKindOf: Float)
		ifTrue: [^SecondDuration new value: aValue].
	(aValue isMemberOf: Fraction)
		ifTrue: [^RatioDuration new value: aValue].
	(aValue isKindOf: String)
		ifTrue: [number := Number readFrom: (ReadStream on: aValue).
			number = 0 ifTrue: [^self error: 'Unknown duration value.'].
			^MDuration value: number].
	(aValue isMemberOf: BlockClosure)
		ifTrue: [^ConditionalDuration new value: aValue].
"	^self error: 'Unknown duration type.'"
	^aValue! !


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
	instanceVariableNames: ''!

!Amplitude class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 16:39'!
value: aValue 
	"Answer a new instance of a member of my species."
	aValue species == Amplitude
		ifTrue: [^ aValue].
	(aValue isMemberOf: SmallInteger)
		ifTrue: [^ MIDIVelocity new value: aValue].
	(aValue isKindOf: Float)
		ifTrue: [^ RatioLoudness new value: aValue].
	(aValue isMemberOf: Fraction)
		ifTrue: [^ RatioLoudness new value: aValue].
	(aValue isKindOf: String)
		ifTrue: [^ SymbolicLoudness new value: aValue asSymbol].
	^ self new value: aValue! !


!Amplitude class methodsFor: 'class constants' stamp: 'stp 12/31/2022 11:01'!
color
	"Answer the default color to display instances with."
	^ Color blue! !

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

MusicModel initialize!
Pitch initialize!
MDuration initialize!
Amplitude initialize!