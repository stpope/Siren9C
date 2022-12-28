MusicMagnitude subclass: #ConditionalDuration
The accessing protocol allows them to be spawned as co-processes in schedulers.
The valueAt: and waitUntil: methods allow flexible conditional scheduling.
ConditionalDurations can be made dependents of other objects using the until: instance creation message.

See the class examples.
!
	"Respond to a change in the receiver's model"

	anAspect == #value
		ifTrue: []! !
	"Answer the result of passing the argument to the receiver's block."

	^value value: anArg! !
	"Cycle the receiver until the argument fulfills the receiver's block."

	^self waitUntil: []! !
	"Cycle the receiver until the argument fulfills the receiver's block."

	[value value: anArg value] 
		whileFalse: ["Processor yield" (Delay forMilliseconds: 10) wait]! !
	"Answer Duration."

	^MDuration! !
	"It is an error to try this here--we implement what's ok for CDs"

	^self error: 'Arithmetic is not possible with conditional durations.'! !
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !
 aDuration
	"Answer the sum of the receiver and the argument--the composition of two blocks"

	| block |
	^aDuration class == self class 
		ifTrue: [block _ aDuration value.
			self class new value:
					[ :a | (value value: a) and: [block value: a]]]
		ifFalse: [block := aDuration value.
			self class new value:
					[ :a | (value value: a) and: [a > block]]]! !
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !
	"Answer a new conditional duration whose value is the given block"
	"ConditionalDuration until: [ :x | x > 5]"

	| me |
	me := ConditionalDuration value: boolBlock.
	aModel addDependent: me! !
	"Answer a new conditional duration whose value is between lo and hi (given in seconds)"

	| t val |
	val := ((Random new nextFrom: lo to: hi) * 1000000) truncated.
	t := Time microsecondClock.	
	^ConditionalDuration value: [ :x | Time microsecondClock > (t + val)].! !
	"Answer a new conditional duration whose value is the given block"
	"ConditionalDuration until: [ :x | x > 5]"

	^ConditionalDuration value: boolBlock! !
	"Print a simple message to the transcript demonstrating the various types."
	"ConditionalDuration example"

	| t0 t me |
	t0 := Time microsecondClock.		"get the time"
					"make a duration 'until the argument is > 3000000'"
	me := ConditionalDuration value: [ :x | x > 3000000].
	Transcript cr; show: t0 printString; cr.
					"use it to wait 3000000 usec."
	me waitUntil: [Time microsecondClock - t0].
	t := Time microsecondClock.	
	Transcript show: (t - t0) printString; cr.
					"make a duration 'until 3 seconds from now'"
	me := ConditionalDuration value: [ :x | Time microsecondClock > (t + 3000000)].
					"wait it out."
	me waitUntil: [].
	t := Time microsecondClock.	
	Transcript show: (t - t0) printString; cr.
	me inspect! !
	"Demonstrate the random duration"
	"ConditionalDuration exampleWithRands"

	| t0 t me |
	5 timesRepeat:
		[me := ConditionalDuration randomBetween: 0.25 and: 0.75.
		t0 := Time microsecondClock.		"get the time"
		me wait.
		t := Time microsecondClock.	
		Transcript show: ((t - t0) / 1000) truncated printString; cr.]! !
Examples are pitch (#d4) or dynamic (#mp) names.

Class Instance Variable:
	NameMap	<Dictionary or Array of (Symbol -> Number or Interval)> the class' look-up table!
	^nameMap! !
When the values are floating-point numbers, the range is often 0.0 to 1.0.
Integer ranges such as 0 to 127 can also be used in subclasses.

One may want to add range-checking for these cases.

Note that the class NumericalMagnitude is vacuous at present, and exists solely for representational modeling.!
	^self class value: val! !
	^self value! !
This is the most general Pitch representation.
The value is assumed to be a floating-point number.!
	"Print the receiver as a pitch string in Hertz."

	aStream nextPutAll: value printString, ' Hz.'! !
	"Answer the units string of the receiver."

	^' Hz'! !
	"Answer Pitch."

	^Pitch! !
	"Assuming value is a frequency, Answer a fractional key number"
	"(Pitch value: 440.0) asFracMIDI"		"440.0 is MIDI key 69.0"
	"(Pitch value: 445.0) asFracMIDI" 	"445.0 is MIDI key 69.195..."

	"(8.17579891564371 Hz is the frequency of C0.)"

	^MIDIPitch new value: (((value / 8.17579891564371) log: 2) * 12)! !
	"Assuming value is a frequency, answer a symbolic note name"
	"(Pitch value: 880.0) asSymbol"			"880.0 is a4"
	"(438.0 Hz) asFracSymbol inspect"			"262.0 is a3 - an offset"
	"(asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromFracMIDI: (self asFracMIDI)! !
	"Answer a HertzPitch."

	^self! !
	"Answer a HertzPitch."

	^self! !
	"Assuming value is a frequency, Answer a key number"
	"(Pitch value: 440.0) asMIDI"		"440.0 is MIDI key 69"
	"(Pitch value: 261.3) asMIDI"		"261.3 is MIDI key 60"
		
	^MIDIPitch new value: (self asFracMIDI value rounded).! !
	"Assuming value is a frequency, answer a symbolic note name"
	"(Pitch value: 880.0) asSymbol"			"880.0 is a4"
	"(438.0 Hz) asSymbol"			"438.0 is a3 - an offset."
	"(asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromMIDI: (self asMIDI)! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asHz! !
	"Accept theValue, as the new instance variable 'value' -- perform  range-checking"

"	| range |
	range := self class range.
	range == nil
		ifFalse: [((theValue < range first) or: [theValue > range last])
			ifTrue: [self error: 'value out of range.']].
"	value := theValue! !
	"Answer the class' range."

	^range! !
	"Set the class' range."

	range := anInterval! !

The translation key <--> Hz. is done with the logarithm or 27.5 Hz. (very low A = 440/16).

Note that AdC's additions make fractional MIDI pitches possible, whereby the first 2 digits to the right of the decimal point signify pitch cents. There are coercion methods such as asFracMIDI for handling microtonal MIDI pitches.
!
	"Answer the units string of the receiver."

	^' key'! !
	"Answer Pitch."

	^Pitch! !
	"Answer a fractionalMIDIPitch (or integer if value is no fraction)."
	"60 key asFracMIDI "
	"60.4 key asFracMIDI "

	^self! !
	"Assuming value is a key number, answer a symbolic pitch name"
	"(Pitch value: 37) asSymbol "
	"(Pitch value: 41.6 key) asSymbol "
	"(443 Hz) asFracSymbol inspect"
	"( asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromFracMIDI: value.! !
	"Assuming value is a key number, answer a frequency"
	"(Pitch value: 69) asHertz"			"440.0 is Midi key 69"
	"(Pitch value: 60) asHertz"			"261.62... is Midi key 60"
	"(Pitch value: 60.51 key) asHertz"	"269.44... is Midi key 60 + 51 cents."
			"NOTE: the key message coerces a float to become a MIDIPitch;
			by default a float becomes a PitchHertz. 
			(see class Pitch, instance creation)"
	"(8.17579891564371 = MIDInote 0 in Hz.)"

	^HertzPitch new value: (8.17579891564371 * (2.0 raisedTo:
									((value asFloat) / 12.0)))! !
	"Assuming value is a key number, answer a frequency"
	"(Pitch value: 69) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: 60) asHertz"		"261.0 is Midi key 60"
	"(Pitch value: 60.51 key) asHertz"	"269.44... is Midi key 60 + 51 cents."

	^self asHertz! !
	"Answer a MIDIPitch."
	"60 key asMIDI "
	"60.6 key asMIDI "

	(value isMemberOf: SmallInteger)
			ifTrue: [^self].
	(value isMemberOf: Float)
			ifTrue: [^self value rounded key].! !
	"Assuming value is a key number, answer a symbolic pitch name"
	"(Pitch value: 37) asSymbol"

	^SymbolicPitch fromMIDI: value! !
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !
	"Initialize the class instance variable."
	"MIDIPitch initialize."

	range := 1 to: 127! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asKey! !

The SymbolicLoudness class maps symbolic dynamic names onto this range on an approximately logarithmic scale.!
	"Truncate"

	super value: theValue.
	value := value truncated.! !
	"Print the receiver as a MIDI velocity."

	aStream nextPutAll: "'v:'," value printString! !
	"Answer the units string of the receiver."

	^' vel'! !
	"Answer Amplitude."

	^Amplitude! !
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(MIDIVelocity value: 64) asDB"

	^DBLoudness new value: (20 * ((value / 127) asFloat log))! !
	"Answer a MIDIVelocity."

	^self! !
	"Answer a RatioLoudness."

	^RatioLoudness new value: ((value / 127.0) asFloat)! !
	"Answer a symbolic loudness."
	"(Amplitude value: 107) asSymbol"	"107 is fff"

	^SymbolicLoudness fromMIDI: value! !
	"Answer the most numerically meaningful version of the receiver."

	^self asRatio! !
	"Initialize the class instance variable."
	"MIDIVelocity initialize."

	range := 0 to: 127! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asVelocity! !

It is partially implemented and unused.

Instance Variables:
	timeSignature	<Fraction>  my time signature
!
	"Answer the units string of the receiver."

	^' measures'! !
	"Answer Duration."

	^ MDuration! !
They use the instance or class instance tables for holding comparative relationships among instances (e.g., mag1 might know that it's > mag2).
The relation-setting (i.e., order assignment) messages are: ==, >>, <<, =<, and =>.
The query messages are: =?, > < <=, and >=.

Each subclass may decide whether instances or the class will hold the table of relationships.
The decision should be made on the basis of the expected number of magnitude instances and the sparseness of their relationships.

See the subclass' class examples.

Instance Variable:
	table <Dictionary of OrdinalMagnitude -> Symbol> instance rel. table

Class Instance Variable:
	Table <Dictionary of OrdinalMagnitude -> Symbol> class rel. table of all instances
!
	"Answer a SmallInteger unique to the receiver.  Essential.  See  
	Object documentation whatIsAPrimitive."

	<primitive: 75>
	^self! !
	"Answer the receiver's loop-up table--its or the class'."

	^table == nil
		ifFalse: [table]
		ifTrue: [self class table == nil
			ifFalse: [self class table at: self]
			ifTrue: [table := Dictionary new.
					table]]! !
	"Signal an error."

	^self error: 'OrdinalMagnitudes don''t have values!!'! !
	"Signal an error."

	^super value: theValue
"
	^self error: 'OrdinalMagnitudes don''t have values!!'
"! !
	"Release the receiver's table."

	| relatives |
	table == nil
		ifFalse: [relatives := table keys.
			table := nil.
			relatives do: [ :m | m release]]! !
	"Print the receiver as an ordinal magnitude."

	value == nil
		ifTrue: [aStream nextPutAll:
						(self class name "copyFrom: 1 to: 3")]
		ifFalse: [aStream nextPutAll: value printString].! !
	"Answer that it is an error to attempt coercion with ordinal magnitudes."

	^self error: 'Coercion is undefined for this receiver.'! !
	"Answer whether the receiver is less than the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [((self table at: anotherOMag) == #<)]
		ifFalse: [((self table associations detect:
					[ :z |
					(z value == #<) and: [z key < anotherOMag]]
			ifNone: [nil]) ~~ nil)]! !
	"Specify that the receiver is less than the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #<
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #<.
				anotherOMag >> self]! !
	"Answer whether the receiver is less than or equal to the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [(((self table at: anotherOMag) == #<)
				or: [(self table at: anotherOMag) == #<=])
				or: [(self table at: anotherOMag) == #=]]
		ifFalse: [(self table associations detect:
			[ :r |
			(((r value == #<) or: [r value == #=])
							or: [r value == #<=])
					and: [r key <= anotherOMag]]
			ifNone: [nil]) ~~ nil]! !
	"Specify that the receiver is less than or equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #<=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #<=.
				anotherOMag => self]! !
	"Specify that the receiver is equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #=.
				anotherOMag == self]! !
	"Specify that the receiver is greater than or equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #>=.
				anotherOMag =< self]! !
	"Answer whether the receiver is equal to the argument."

	^self == anotherOMag
		ifTrue: [true]
		ifFalse: [(self table includesKey: anotherOMag)
			ifTrue: [((self table at: anotherOMag) == #=)]
			ifFalse: [((self table associations detect:
					[ :z |
					(z value == #=) and: [z key =? anotherOMag]]
				ifNone: [nil]) ~~ nil)]]! !
	"Answer whether the receiver is greater than the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>]
		ifFalse: [(self table associations detect:
					[ :z |
					(z value == #>) and: [z key > anotherOMag]]
			ifNone: [nil]) ~~ nil]! !
	"Answer whether the receiver is greater than or equal to the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [(((self table at: anotherOMag) == #>)
				or: [(self table at: anotherOMag) == #>=])
				or: [(self table at: anotherOMag) == #=]]
		ifFalse: [(self table associations detect:
			[ :r |
			(((r value == #>) or: [r value == #=])
							or: [r value == #>=])
					and: [r key >= anotherOMag]]
			ifNone: [nil]) ~~ nil]! !
	"Specify that the receiver is greater than the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #>.
				anotherOMag << self]! !
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !
 aValue
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !
	"Answer whether the receiver and the argument are equivalent."

	^self == aValue! !
	"Answer the class' instance look-up table."

	^Table! !
	"Answer the sorted values."
	
	^Table == nil
		ifTrue: [self allInstances asSortedCollection]
		ifFalse: [Table keys asSortedCollection]! !
	"Answer a new instance and, if it's in use, add it to the table."

	| me |
	me := super new.
	Table == nil ifFalse: [Table at: me put: Dictionary new].
	^me! !
	"Answer an instance"

	^self new value: aValue! !
	"Release the shared class table and all instances."
	"OrdinalMagnitude allSubclassesDo: [ :t | t flush]"

	Table == nil
		ifFalse: [Table keysDo: [ :d | d release].
				Table := nil]
		ifTrue: [self allInstancesDo: [ :d | d release]]! !
	"Set up a shared class table for all instances."

	Table := Dictionary new.! !
The instance variable tables are used for the name -> relation symbol map.

See the class examples.!
	"Answer Duration."

	^ MDuration! !
	"Demonstrate the use of an OrdinalMagnitude with a scale of length."
	"Length example"

	| a b c d e f |		"define six values with names."
	Length useTable.
	a := Length value: #sip.
	b := Length value: #gasp.
	c := Length value: #focus.
	d := Length value: #inhale.
	e := Length value: #gesture.
	f := Length value: #breath.
					"put them in some order."
	a << b.	b << c.	c << d.	d << e.	e << f.
					"now ask some queries and let them extrapolate."
	Transcript show: '(f > e) ', (f > e) printString; cr.
	Transcript show: '(f > b) ', (f > b) printString; cr.
	Transcript show: '(f =? f) ', (f =? f) printString; cr.
	Transcript show: '(f =? a) ', (f =? a) printString; cr.
	Transcript show: '(f >= a) ', (f >= a) printString; cr.
	Transcript show: '(d >= b) ', (d >= b) printString; cr.
	Transcript show: '(b >= d) ', (b >= d) printString; cr.
	Transcript show: '(f < a) ', (f < a) printString; cr.
	Transcript show: '(a < f) ', (a < f) printString; cr.
					"inspect them if you hold down <LEFT-SHIFT>"
	Length allInstances asSortedCollection inspect! !

Instance Variable:
	relative		<MusicMagnitude> the reference value!
	"Answer the receiver's value mapped to the receiver."

	^value! !
	"Answer the receiver's value mapped to the receiver."

	^relative == nil
		ifTrue: [value]
		ifFalse: [self class new value: (relative value * value)]! !
	"Answer self."

	^self! !
	"Apply the receivers reference."

	relative == nil
		ifFalse: [value := value * relative value.
				relative := nil]! !
	"Set the receivers reference."

	relative := aRelative! !
	"Answer and instance with the argument as its value."

	^(self new value: aValue) relativeTo: aMMagnitude! !
They can be expanded into msec. relative to some given event (a whole note), or use the default tempo of 1 sec.!
	"Print the receiver as a fractional duration."

	aStream nextPutAll: "'d:'," value printString! !
	"Answer the units string of the receiver."

	^' beat'! !
	"Answer Duration."

	^ MDuration! !
	"Answer a MM whose value is the argument over the receiver's value."
	"1/4 beat"

	^self species relativeMember value: (numerator / value)! !
	"Answer a MSecondDuration."

	^relative == nil
		ifTrue: [MSecondDuration new value: ((value * 1000) truncated)]
		ifFalse: [MSecondDuration new value:
					((value * 1000 * (relative asRatio value)) truncated)]! !
	"Answer a MSecondDuration."

	^relative == nil
		ifTrue: [MSecondDuration new value: ((value * 1000) truncated)]
		ifFalse: [MSecondDuration new value:
					((value * 1000 * (relative asRatio value)) truncated)]! !
	"Answer a SecondDuration."

	^relative == nil
		ifTrue: [SecondDuration new value: value asFloat]
		ifFalse: [SecondDuration new value:
					(value asFloat * (relative asRatio value))]! !
	"Answer a USecondDuration."

	^self asUseconds! !
	"Answer a USecondDuration."

	^relative == nil
		ifTrue: [USecondDuration new value: ((value * 1000000) truncated)]
		ifFalse: [USecondDuration new value:
					((value * 1000000 * (relative asRatio value)) truncated)]! !
	"Answer the most numerically meaningful version of the receiver."

	^self asSec! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asBeat! !
!
	"Answer Amplitude."

	^Amplitude! !
	"Print the receiver as a ratio."

	aStream nextPutAll: "'a:'," value printString! !
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(RatioLoudness value: 0.25) asDB"

	^DBLoudness new value: (20 * (value log))! !
	"Answer a MIDI key velocity (0 to 127)"
	
	^MIDIVelocity new value: ((value * 127.0) asInteger)! !
	"Answer a symbolic dynamic."
	
	^(self asMIDI) asSymbol! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asAmpl! !
!
	"Answer the units string of the receiver."

	^' dB'! !
	"Subtract the argument from the receiver after turning both into db."

	^DBLoudness value: (self value - aDyn asDB value)! !
	"Subtract the argument from the receiver after turning both into db."

	^DBLoudness value: (self value - aDyn asDB value)! !
	"Multiply the argument by the receiver after turning both into db."

	^DBLoudness value: (self value * aDyn asDB value)! !
	"Multiply the argument by the receiver after turning both into db."

	^DBLoudness value: (self value * aDyn asDB value)! !
	"Divide the argument by the receiver after turning both into db."

	^DBLoudness value: (self value / aDyn asDB value)! !
	"Divide the argument by the receiver after turning both into db."

	^DBLoudness value: (self value / aDyn asDB value)! !
	"Add the argument to the receiver after turning both into db."

	^DBLoudness value: (self value + aDyn asDB value)! !
	"Answer Amplitude."

	^Amplitude! !
	"Answer a MIDI key velocity (0 to 127)"
	
	^self asRatio asMIDI! !
	"Answer the receiver as a ratio between 0 and 1."
	"-12 dB asRatio"

	^RatioLoudness new value: (10 raisedTo: (value / 20))! !
	"Answer the zero-based positive dB value of the receiver."
	"90 dB = 32000"
	"0 = 90		-6 = 80		-12 = 70"

	^90 + (value / 0.6)! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asDB! !
They can be transformed into other values (e.g., note names or key numbers), but often at a loss of accuracy if they are not well-tempered.
!
	"Answer Pitch."

	^Pitch! !
	"Print the receiver as a ratio."

	aStream nextPutAll: "'p:'," value printString! !
	"return a RatioPitch as a fractional MIDINote."

	^self asHertz asFracMIDI.! !
	"Assuming value is a ratio, answer a symbolic note name"

	^SymbolicPitch fromFracMIDI: (self asFracMIDI)! !
	"Answer a HertzPitch."

	^relative == nil
		ifTrue: [HertzPitch new value: (value * 261.623)]
		ifFalse: [HertzPitch new value:
					(value * (relative asHertz value))]! !
	"return a RatioPitch as a MIDINote (rounded)."

	^self asHertz asMIDI.! !
	"Assuming value is a ratio, answer a symbolic note name"

	^SymbolicPitch fromMIDI: (self asMIDI)! !
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asStep! !
This is among the most general duration time representations.!
	"Print the receiver on the argument as a MM declaration."

	| valstr |
	valstr := value printString.
	valstr size > 6
		ifTrue: [valstr := valstr copyFrom: 1 to: 5].
	aStream nextPutAll: '(', valstr, self units, ')'! !
	"Answer the units string of the receiver."

	^' sec'! !
	"Delay for a time corresponding to the receiver"
	"1.5 seconds wait"

	(Delay forMicroseconds: self asUseconds value) wait! !
	"Answer the receiver in seconds."

	^self asSec! !
	"Answer Duration."

	^ MDuration! !
	"Answer a float of seconds."

	^self asSeconds value! !
	"Answer a RatioDuration."

	^RatioDuration new value: (self asSec value)! !
	"Answer a MSecondDuration."

	^self asMsec! !
	"Answer a MSecondDuration."

	^self asMseconds! !
	"Answer a MSecondDuration."

	^MSecondDuration new value: (value * 1000) truncated! !
	"Answer a RatioDuration."

	^RatioDuration new value: (self asSec value asFloat)! !
	"Answer a float of seconds."

	^self asSeconds! !
	"Answer a float of seconds."

	^self! !
	"Answer a USecondDuration."

	^self asUseconds! !
	"Answer an int of micro seconds."

	^USecondDuration new value: (value * 1000000) truncated! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asSec! !
!
	"Answer the units string of the receiver."

	^' msec'! !
	"Answer a MSecondDuration."

	^self! !
	"Answer a float of seconds."

	^SecondDuration new value: (value / 1000) asFloat! !
	"Answer a float of micro seconds."

	^USecondDuration new value: (value * 1000) truncated! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asMsec! !
The instance variable tables are used for the name -> relation symbol map.

See the class examples.!
	"Answer Pitch."

	^Pitch! !
	"Demonstrate the use of an OrdinalMagnitude with a scale of sharpness."
	"Sharpness example"

	| a b c d e f |		"define six values with names."
	a := Sharpness value: #soft.
	b := Sharpness value: #nasal.
	c := Sharpness value: #round.
	d := Sharpness value: #high.
	e := Sharpness value: #loud.
	f := Sharpness value: #shrill.
					"put them in some order."
	a << b.	b << c.	c << d.	d << e.	e << f.
					"now ask some queries and let them extrapolate."
	Transcript show: '(f > e) ', (f > e) printString; cr.
	Transcript show: '(f > b) ', (f > b) printString; cr.
	Transcript show: '(f =? f) ', (f =? f) printString; cr.
	Transcript show: '(f =? a) ', (f =? a) printString; cr.
	Transcript show: '(f >= a) ', (f >= a) printString; cr.
	Transcript show: '(d >= b) ', (d >= b) printString; cr.
	Transcript show: '(b >= d) ', (b >= d) printString; cr.
	Transcript show: '(f < a) ', (f < a) printString; cr.
	Transcript show: '(a < f) ', (a < f) printString; cr.
	Sharpness values inspect! !

The class instance variable NameMap is used for the name <--> MIDI velocity range (0 to 127) mapping dictionary.
!
	"Print the receiver as a symbolic amplitude."

	aStream nextPutAll: value! !
	"Answer the units string of the receiver."

	^' ampl'! !
	"Answer Amplitude."

	^Amplitude! !
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(#ff ampl) asDB"

	^self asRatio asDB! !
	"Answer a MIDIVelocity."
	"(#pp ampl) asMIDI"		"pianissimo is MIDI velocity 24"

	^MIDIVelocity new value:
			((self class nameMap keyAtValue: value) last)! !
	"Answer a RatioLoudness."
	"(#pp ampl) asRatio"		"pianissimo is 0.2"

	^RatioLoudness new value:
			(((self class nameMap keyAtValue: value) last / 120) asFloat)! !
	"Answer a SymbolicLoudness."

	^self! !
	"Answer the most numerically meaningful version of the receiver."

	^self asRatio! !
	"Set up the class dynamic mapping dictionary"
	"SymbolicLoudness initialize"
	
	nameMap := Dictionary new.
	nameMap at: (1 to: 12) put: #ppp.
	nameMap at: (12 to: 24) put: #pp.
	nameMap at: (24 to: 38) put: #p.
	nameMap at: (38 to: 50) put: #mp.
	nameMap at: (50 to: 70) put: #mf.
	nameMap at: (70 to: 85) put: #f.
	nameMap at: (85 to: 100) put: #ff.
	nameMap at: (100 to: 127) put: #fff.! !
	"Assuming value is a key velocity, answer a symbolic loudness name"
	"(Amplitude value: 39) asSymbol"

	| int |
	int := nameMap keys detect: [ :range | range includes: aValue]
				ifNone: [^self error: 'invalid Loudness range.'].
	^(self new) value: (nameMap at: int)! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asDynamic! !
The range is c0 to g#9 and the values are symbols.
Note the confusion between the sharp sign (always placed after the note name) and Smalltalk's symbol key #.
One often writes ('c#3' asSymbol) to be safe.

Instance Variables:
	fracPitch <Float> the remainder for microtonal tunings

The class instance variable NameMap is used for the name <--> MIDI key number mapping array.
!
	"Print the receiver as a symbolic pitch."

	value printOn: aStream! !
	"Answer the units string of the receiver."

	^' pitch'! !
 aValue
	"Answer the sum of the receiver and the argument-handle adding Integers as a special case."

	^(aValue isMemberOf: self class)
		ifTrue: [self class new value: (value + aValue value)]
		ifFalse: [(aValue isMemberOf: SmallInteger)
			ifTrue: [SymbolicPitch fromMIDI: (self asMIDI value + aValue)]
			ifFalse: [super + aValue]]! !
	"Answer the sum of the receiver and the argument-handle adding Integers as a special case."

	^(aValue isMemberOf: self class)
		ifTrue: [self class new value: (value - aValue value)]
		ifFalse: [(aValue isMemberOf: SmallInteger)
			ifTrue: [SymbolicPitch fromMIDI: (self asMIDI value - aValue)]
			ifFalse: [super - aValue ]]! !
	"transpose a SymbolicPitch by aValue in fractional halfsteps"

	^((self asFracMIDI transposeBy: aValue) asFracSymbol).! !
	"Answer Pitch."

	^Pitch! !
	"Assuming value is a symbolic note name, answer a key number."
	"MIDI is 0=c-based, NameMap is 0=a-based"
	"(Pitch value: #c3) asFracMIDI"
	"(Pitch value: 'c#3' asSymbol) asFracMIDI"
	"( 'c#3' pitch fracPitch: -0.4) asFracMIDI"

	(fracPitch isNil)
	ifTrue: [^self asMIDI]
	ifFalse: [^(self asMIDI value + fracPitch) key].! !
	"assuming value is a symbolic note name, return a frequency "
	"(Pitch value: #a6) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: #c5) asHertz"		"261.0 is Midi key 60 or middle C"

	^(self asFracMIDI) asHertz! !
	"assuming value is a symbolic note name, return a frequency "
	"(Pitch value: #a6) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: #c5) asHertz"		"261.0 is Midi key 60 or middle C"

	^self asHertz! !
	"Assuming value is a symbolic note name, answer a key number."
	"MIDI is 0=c-based, NameMap is 0=a-based"
	"(Pitch value: #c3) asMIDI"
	"(Pitch value: 'c#3' asSymbol) asMIDI"

	| noteName octave |
	octave := '3'.	"if no octave is given, assume c3 - b3."
	noteName := self value.
	noteName last isDigit
		ifTrue: [octave := noteName last.
			noteName := noteName copyFrom: 1 to: noteName size - 1].
	(noteName last == $-)	"if octave has minus sign...."
		ifTrue: [octave := octave digitValue negated.
			noteName := noteName copyFrom: 1 to: noteName size - 1].
	^MIDIPitch new value: (self class nameMap detect: [:item | item key == noteName asSymbol]) value - 3 + (octave asString asNumber + 2 * 12)! !
	"Answer a SymbolicPitch."

	^self.! !
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !
	"Answer the receiver's accidental, if any."

	| c |
	(value size > 1)
		ifFalse: [^nil]
		ifTrue: [c := value at: 2].
	^((c == $#) or: [c == $b])
		ifTrue: [c]
		ifFalse: [nil]! !
	"return microtonal offset as fractions of a halfstep"

	^fracPitch! !
	"set microtonal offset as fractions of a halfstep"

	fracPitch := aValue.! !
	"Set up the class name mapping array."
	"SymbolicPitch initialize"

	nameMap := OrderedCollection new: 32.
	nameMap add: ('a' asSymbol -> 0);
			add: ('a#' asSymbol -> 1);
			add: ('as' asSymbol -> 1);
			add: ('a-sharp' asSymbol -> 1);
			add: ('bb' asSymbol -> 1);
			add: ('b-flat' asSymbol -> 1);
			add: ('b' asSymbol -> 2);
			add: ('cb' asSymbol -> 2);
			add: ('c-flat' asSymbol -> 2);
			add: ('c' asSymbol -> 3);
			add: ('c#' asSymbol -> 4);
			add: ('cs' asSymbol -> 4);
			add: ('c-sharp' asSymbol -> 4);
			add: ('d-flat' asSymbol -> 4);
			add: ('db' asSymbol -> 4);
			add: ('d' asSymbol -> 5);
			add: ('d#' asSymbol -> 6);
			add: ('ds' asSymbol -> 6);
			add: ('d-sharp' asSymbol -> 6);
			add: ('e-flat' asSymbol -> 6);
			add: ('eb' asSymbol -> 6);
			add: ('e' asSymbol -> 7);
			add: ('f-flat' asSymbol -> 7);
			add: ('fb' asSymbol -> 7);
			add: ('f' asSymbol -> 8);
			add: ('f#' asSymbol -> 9);
			add: ('fs' asSymbol -> 9);
			add: ('f-sharp' asSymbol -> 9);
			add: ('g-flat' asSymbol -> 9);
			add: ('gb' asSymbol -> 9);
			add: ('g' asSymbol -> 10);
			add: ('g#' asSymbol -> 11);
			add: ('gs' asSymbol -> 11);
			add: ('g-sharp' asSymbol -> 11);
			add: ('a-flat' asSymbol -> 11);
			add: ('ab' asSymbol -> 11).! !
	"Assuming value is a key number, answer a symbolic pitch name"
	"SymbolicPitch fromMIDI: (Pitch value: 69)"
	"SymbolicPitch fromMIDI: (Pitch value: 68.9 key)"
	"(fromMIDI throws away microtonal offset, fromFracMIDI keeps it.)"	

	| tempSym tempKey |
	tempKey := aValue value.
	tempSym := self value: ((nameMap detect: 
					[ :ass | 
					ass value = (tempKey rounded + 3 \\ 12)]) key, 
		(aValue value // 12 - 2) printString).
	tempSym fracPitch: (tempKey - tempKey rounded).
	^tempSym.! !
	"Assuming value is a key number, answer a symbolic pitch name"
	"SymbolicPitch fromMIDI: (Pitch value: 69)"
	"SymbolicPitch fromMIDI: (Pitch value: 68.9 key)"
	"(fromMIDI throws away microtonal offset, fromFracMIDI keeps it.)"	

	^self value: ((nameMap detect: 
					[ :ass | 
					ass value = (aValue value rounded + 3 \\ 12)]) key, 
		(aValue value // 12 - 2) printString).! !
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asNote! !
This is the default time representation (most general duration) and is usually used for keys in event lists.!
	"Answer the units string of the receiver."

	^' usec'! !
	"Answer a MSecondDuration."

	^MSecondDuration new value: (value / 1000) truncated! !
	"Answer a float of seconds."

	^SecondDuration new value: (value / 1000000) asFloat! !
	"Answer a float of micro seconds."

	^self! !