Magnitude subclass: #MusicMagnitude

MusicMagnitudes generally implement precise mixed-mode arithmetic and comparison methods using a per-species generality table and auto-coercion within a species.

The "type abstraction" classes (MDuration, Pitch, Amplitude, etc.) represent "what"; they are never instantiated; their class creation methods answer instances of their species, but they manage coercion within the classes of their species. The "representational abstraction" classes (NominalMagnitude, OrdinalMagnitude, NumericalMagnitude, RatioMagnitude, etc.) represent "how"; they share value model semantics and coercion techniques. Note that the concrete implementation classes therefore answer their species by property model rather than by implementation subclass (i.e., HertzPitch species --> Pitch rather than NumericalMagnitude).

MusicMagnitudes can have dependents (observers), and send themselves the changed: message when their values are changed.

There are many examples in the implementation classes and the Siren  outline.

Instance variables:
	value		<Number or Symbol> the magnitude's value
!
	"Answer en Event with the given magnitude as its first property."
	"440 Hz, 250 msec"

	| event |
	event := MusicEvent new.
	event perform: self species propertyName with: self.
	anArgument isMusicMagnitude ifTrue:
		[event perform: anArgument species propertyName with: anArgument].
	^event! !
	"Answer the instance variable 'value'."

	^value! !
	"Accept the argument, 'theValue', as the new instance variable 'value' 
		and signal any dependents."

	value := theValue.
	self changed: #value with: value! !
	"Print the receiver on the argument as a MM declaration."

	^self printOn: aStream parens: true! !
	"Print the receiver on the argument as a MM declaration."

	useParens
		ifTrue: [self isImmediate
			ifFalse: [aStream nextPutAll: '(', self species name, ': ', 
				self printValue, ')']
			ifTrue: [aStream nextPutAll: '(', self printValue, self units, ')']]
		ifFalse: [self isImmediate
			ifFalse: [aStream nextPutAll: self species name, ': ', self printValue]
			ifTrue: [aStream nextPutAll: self printValue, self units]]! !
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream  parens: true.
	^aStream contents! !
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream  parens: bool.
	^aStream contents! !

	value isString ifTrue: [^value].
"	value isFloat ifTrue: [^value printPlaces: 4].
"	^value printString! !
	"Store the receiver on the argument as a MM declaration."

	aStream nextPutAll: '(', self class name, ' value: ', value printString, ')'! !
	"Answer the units string of the receiver."

	^nil! !
	"Answer MusicMagnitude."

	^MusicMagnitude! !
	"If I am involved in arithmetic with a number, convert me to a number."

	^self class value: (rcvr perform: selector with: self value)! !
	"Coerce the receiver to the given class and answer a new object."

	^self perform: type ddMsgName! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^self class ddMsgName! !
	"Answer a MSecondMDuration."

	^self asMsec! !
	"Answer a MSecondMDuration."

	^self asUsec! !
	"Answer whether the receiver has an immediate representation."

	^self units ~~ nil! !
	"Answer true for the receiver and all subsclasses."

	^true! !
	"Answer falst at all times."

	^false! !
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
 aValue
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
	"Answer a SmallInteger unique to the receiver."

	^value hash! !
	"Answer and instance with the argument as its value."

	^self new value: aValue! !
	"Answer the default color to display instances with (hack for making pretty graphs)."

	| spec |
	spec := self new species.
	^(spec == MusicMagnitude)
		ifTrue: [ColorValue gray]
		ifFalse: [spec color]! !
	"Answer the receiver class's default property name."

	^self subclassResponsibility! !
	"Answer the receiver class's species member used as a relative magnitude."

	^self subclassResponsibility! !

See the class methods for generality.

Class inst Vars:
	generalityTable <Dictionary of (Class ->Integer)> The species generality table
!
	"Answer the class inst var for the generality table."

	^generalityTable! !
	"Answer an array of the two arguments with the most general first."

	^(self generalityTable at: aMag class) > (self generalityTable at: otherMag class)
		ifTrue: [Array with: aMag with: otherMag]
		ifFalse: [Array with: otherMag with: aMag]! !

	"Pitch generalize: (#c pitch) and: (21 key)"
	"Pitch generalize: (#c pitch) and: (220 Hz)"
	"Pitch generalize: (220 Hz) and: (#c pitch)"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"
	"MDuration generalize: (100 msec) and: ((1/4) beat)"
	"MDuration generalize: (100 msec) and: (0.3 sec)"

	 (#ff ampl) - (32 velocity)! !
Instances of its subclass species are used to model pitches, gamut members, and frequencies.!
Instances of its subclass species are used to model times, MDurations and metronomes.!
Instances of its subclass species are used to model loudness values.!
Amplitudes come in several flavors, as in the classes RatioMagnitude, MIDIVelocity and SymbolicLoudness.
The class Amplitude is abstract, its class creation method answers an instance of a concrete Amplitude/Loudness class.

Examples:
	Amplitude value: 0.77		"create a ratio instance - range 0.0 to 1.0 (cmusic)"
	Amplitude value: 77		"create an MIDI instance - range 0 to 127"
	Amplitude value: #mp		"create a symbolic instance - range #ppp to #fff"

See also the class example.!
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
	"Answer the default color to display instances with."

	^ColorValue blue! !
	"Answer the class inst var"

	^generalityTable! !
	"Set up the class inst var, a generality table."
	"Amplitude initialize"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"

	generalityTable :=
		((DBLoudness -> 80),
		(RatioLoudness -> 80),
		(MIDIVelocity -> 40),
		(SymbolicLoudness -> 20))! !
	"Set up the class inst var, a generality table."
	"Amplitude initialize"
	"Amplitude generalize: (21 dB) and: (88 velocity)"
	"Amplitude generalize: (#ff ampl) and: (127 velocity)"

	generalityTable :=
		((DBLoudness -> 80),
		(RatioLoudness -> 80),
		(MIDIVelocity -> 40),
		(SymbolicLoudness -> 20))! !
	"Answer the most general-purpose MDuration--relative"

	^#asRatio! !
	"Answer the receiver class's default property name."

	^#loudness:! !
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioLoudness! !
	"Answer Amplitude."

	^Amplitude! !
	"Print a simple message to the transcript demonstrating the various types."
	"SymbolicLoudness nameMap inspect"
	"Amplitude example"

	| me |
	me := Amplitude value: #mf.
	Transcript show: me printString; cr;
		show: me asRatio printString; cr;
		show: me asMIDI printString; cr.
	me inspect! !
MDurations come in several flavors, as in the classes RatioMDuration, MSecondMDuration and ConditionalMDuration.
The class MDuration is abstract, its class creation method answers an instance of a concrete MDuration class.

Examples:
	MDuration value: 0.77		"create an instance with seconds as the unit"
	MDuration value: 770		"create an instance with milliseconds as the unit"
	MDuration value: 1/4		"create an instance with beats as the unit"
	MDuration value: [ :x | x > 4]	"create an instance for: 'until x > 4'"

See also the class example.!

	^ (seconds * 1000) + (nanos // 1e6)! !
	"Answer an integer number of minutes"
	^ seconds // 60! !

	^ (seconds * Time nanosInSecond) + nanos! !

	^ seconds! !
	"operand is a Number" 	^ self class nanoSeconds: ( (self totalNanoSeconds * operand) asInteger).
! !
 operand

	"operand is a MDuration" 	^ self class nanoSeconds: (self totalNanoSeconds + operand totalNanoSeconds)
! !
	"operand is a MDuration" 	^ self + operand negated
! !

	"operand is a MDuration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds / operand) asInteger ]
		ifFalse: [
			self assert: operand class == MDuration.
			self totalNanoSeconds / operand totalNanoSeconds ]
.
! !

	^ self totalNanoSeconds < comparand totalNanoSeconds
! !
	"Answer whether the argument is a <MDuration> representing the same 
	period of time as the receiver."

	self == comparand
		ifTrue: [ ^ true ].

	self species == comparand species 
		ifFalse: [ ^ false ].

	^ self totalNanoSeconds = comparand totalNanoSeconds! !

	^ self class seconds: seconds abs nanoSeconds: nanos abs
! !
	"Answer the number of days the receiver represents."

	^ seconds quo: Time secondsInDay! !
	^seconds hash bitXor: nanos hash! !
	"Answer the number of hours the receiver represents."


	^ (seconds rem: Time secondsInDay) quo: Time secondsInHour! !
	"Answer the number of minutes the receiver represents."

	^ (seconds rem: Time secondsInHour) quo: Time secondsInMinute! !

	^ self class seconds: seconds negated nanoSeconds: nanos negated
! !


	^ self positive not
! !


	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]
! !
	"Answer the number of seconds the receiver represents."

	^seconds rem: Time secondsInMinute! !

	"operand is a MDuration or a Number"


	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds // operand) asInteger ]
		ifFalse: [
			self assert: operand class == MDuration.
			self totalNanoSeconds // operand totalNanoSeconds ]
! !

	"modulo. Remainder defined in terms of //. Answer a MDuration with the 
	same sign as aMDuration. operand is a MDuration or a Number."

	^ operand isNumber
		ifTrue: [ self class nanoSeconds: (self totalNanoSeconds \\ operand) ]
		ifFalse: [ self - (operand * (self // operand)) ]
! !

	^ Delay forMDuration: self! !

	^ seconds = 0 and: [ nanos = 0 ]
! !


	^ nanos
! !
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(MDuration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
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
	"e.g. if the receiver is 5 minutes, 37 seconds, and aMDuration is 2 minutes, answer 6 minutes."

	^ self class nanoSeconds: (self totalNanoSeconds roundTo: aMDuration totalNanoSeconds)

! !
	"e.g. if the receiver is 5 minutes, 37 seconds, and aMDuration is 2 minutes, answer 4 minutes."

	^ self class
		nanoSeconds: (self totalNanoSeconds truncateTo: aMDuration totalNanoSeconds)

! !
	"Private - only used by MDuration class"

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

	aStream
		nextPut: $(;
		nextPutAll: self className;
		nextPutAll: ' seconds: ';
		print: seconds;
		nextPutAll: ' nanoSeconds: ';
		print: nanos;
		nextPut: $).
! !
	self seconds: 0 nanoSeconds: 0.
! !

	^aDateAndTimeOrDate subtractMDuration: self! !

	^ self days: days hours: hours minutes: minutes seconds: seconds nanoSeconds: 0! !

	^ self basicNew seconds: days * Time secondsInDay + seconds nanoSeconds: 0! !

	^ self seconds: seconds nanoSeconds: 0
! !

	^ self basicNew seconds: 0 nanoSeconds: 0
! !

	^ self seconds: aNumber * Time secondsInDay nanoSeconds: 0! !

 	^ self seconds: ((days * Time secondsInDay) 
						+ (hours * Time secondsInHour)
							+ (minutes * Time secondsInMinute) 
								+ seconds)
		nanoSeconds: nanos! !
	"
	Extended to accept non-ANSI formats, such as:
		MDuration fromString: '1:50:00'
		MDuration fromString: '5:30.5'
		MDuration fromString: '1:6:50:00'
		MDuration fromString: '3.5h'
		MDuration fromString: '2.5min'
		MDuration fromString: '1.5s'
		MDuration fromString: '200s'
		MDuration fromString: '200ms'
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

	^ self seconds: aNumber * Time secondsInHour nanoSeconds: 0! !

	^ self nanoSeconds: milliCount * 1000000! !

	^ self seconds: aNumber * Time secondsInMinute nanoSeconds: 0! !
	"This method is slow. If you have nanos less than 10^6 you should use #seconds:nanoSeconds: instead."

	| quo |
	quo _ nanos quo: Time nanosInSecond.
	^ self basicNew
		seconds: quo
		nanoSeconds: nanos - (quo * Time nanosInSecond)! !
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
		MDuration readFrom: '0:00:00:00' readStream
		MDuration readFrom: '0:00:00:00.000000001' readStream
		MDuration readFrom: '0:00:00:00.9' readStream
		MDuration readFrom: '0:00:00:00.99' readStream
		MDuration readFrom: '0:00:00:00.99999999' readStream
		MDuration readFrom: '0:00:00:00.999999999' readStream
		MDuration readFrom: '0:00:00:00.100000000' readStream
		MDuration readFrom: '0:00:00:00.001 ' readStream
		MDuration readFrom: '0:00:00:00.1' readStream
		MDuration readFrom: '0:00:00:01 ' readStream
		MDuration readFrom: '0:12:45:45' readStream
		MDuration readFrom: '1:00:00:00' readStream
		MDuration readFrom: '365:00:00:00' readStream
		MDuration readFrom: '-7:09:12:06.10' readStream
		MDuration readFrom: '+0:01:02:3' readStream
 	"! !

	^ self basicNew
		seconds: seconds truncated
		nanoSeconds: seconds fractionPart * Time nanosInSecond + nanos! !

	^ self days: (aNumber * 7) seconds: 0
! !
	"Answer a new instance of a member of my species."

	| number |
	aValue species == MDuration ifTrue: [^aValue].
	(aValue isMemberOf: SmallInteger)
		ifTrue: [aValue < 10
			ifTrue: [^SecondDuration new value: aValue]
			ifFalse: [^MSecondDuration new value: aValue]].
	(aValue isKindOf: Float)
		ifTrue: [^SecondDuration new value: aValue].
	(aValue isMemberOf: Fraction)
		ifTrue: [^RatioDuration new value: aValue].
	(aValue isKindOf: String)
		ifTrue: [number := Number readFrom: (ReadStream on: aValue).
			number = 0 ifTrue: [^self error: 'Unknown Duration value.'].
			^MDuration value: number].
	(aValue isMemberOf: BlockContext)
		ifTrue: [^ConditionalDuration new value: aValue].
"	^self error: 'Unknown MDuration type.'"
	^aValue! !
	"Answer the default color to display instances with."

	^ColorValue red! !
	"Answer the class inst var"

	^generalityTable! !
	"Set up the class inst var, a generality table."
	"MDuration initialize"
	"MDuration generalize: (100 msec) and: ((1/4) beat)"
	"MDuration generalize: (100 msec) and: (0.3 sec)"

	generalityTable :=
		((SecondDuration -> 80),
		(RatioDuration -> 60),
		(MSecondDuration -> 40),
		(ConditionalDuration -> 0))! !
	"Set up the class inst var, a generality table."
	"MDuration initializeGeneralities"
	"MDuration generalize: (100 msec) and: ((1/4) beat)"
	"MDuration generalize: (100 msec) and: (0.3 sec)"

	generalityTable :=
		((SecondMDuration -> 80),
		(RatioMDuration -> 60),
		(MSecondMDuration -> 40),
		(ConditionalMDuration -> 0))! !
	"Answer the most general-purpose MDuration--seconds"

	^#asSec! !
	"Answer the receiver class's default property name."

	^#duration:! !
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioMDuration! !
	"Answer MDuration."

	^MDuration! !
	"Print a simple message to the transcript demonstrating the various types."
	"MDuration example"

	| me |
	me := MDuration value: 1/4.
	Transcript show: me printString; cr;
		show: me asMS printString; cr.
	me inspect! !
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number, process or block.!
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number or pitch, and the mode or gamut could be shared.!

Instance variables:
	field <Integer> field in the score
	name <String or Symbol> the parameter's name (optional)

Examples:
	PField field: 5 value: 0.7 name: #position
	PField field: 9 value: 'legato' name: #expression
!
	"Answer the instance variable 'field'."

	^field! !
	"Accept the argument, 'theNumber', as the new instance nariable 'field'."

	field := theNumber! !
	"Answer the instance variable 'name'."

	^name! !
	"Accept the argument, 'theName', as the new instance nariable 'name'."

	name := theName! !
	"Answer an initialized instance."

	^(self new) field: aNumber; value: aValue! !
	"Answer an initialized instance."

	^(self new) field: aNumber; value: aValue; name: aName! !
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
	"Answer a new instance of a member of my species."

	aValue species == Pitch ifTrue: [^aValue].
	(aValue isInteger)
			ifTrue: [^MIDIPitch new value: aValue].
	(aValue isFloat)
			ifTrue: [^HertzPitch new value: aValue].
	(aValue isFraction)
			ifTrue: [^RatioPitch new value: aValue].
	(aValue isString)
			ifTrue: [^SymbolicPitch new value: aValue asSymbol].
	^self new value: aValue! !
	"Answer the default color to display instances with."

	^ColorValue green! !
	"Answer the class inst var"

	^generalityTable! !
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
	"Answer the most general-purpose MDuration--Hertz"

	^#asHz! !
	"Answer the receiver class's default property name."

	^#pitch:! !
	"Answer the receiver class's species member used as a relative magnitude."

	^RatioPitch! !
	"Answer Pitch."

	^Pitch! !
	"Print a simple message to the transcript demonstrating the various types."
	"SymbolicPitch nameMap inspect"
	"Pitch example"

	| me |
	me := Pitch value: 'c3'.
	Transcript show: me printString; cr;
		show: me asHertz printString; cr;
		show: me asMIDI printString; cr.
	me inspect! !
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
Instances of its subclass species are used to model spatial and positional values.!
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable would hold a 1- or 2-dimensional position as a number or point.!
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable could hold a number or point.!
This class can be used as a concrete one (adding a few methods to fill it out), or like the other representational classes (making concrete classes of this species).
In the later case, the value instance variable would hold a record with the geometry or the room, the positions of default sources, and the listener's position and features.!