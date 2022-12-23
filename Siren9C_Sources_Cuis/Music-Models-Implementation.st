!classDefinition: #ConditionalDuration category: #'Music-Models-Implementation'!
MusicMagnitude subclass: #ConditionalDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!ConditionalDuration commentStamp: '<historical>' prior: 0!
Instances of ConditionalDuration are duration times where the value is a block.
The accessing protocol allows them to be spawned as co-processes in schedulers.
The valueAt: and waitUntil: methods allow flexible conditional scheduling.
ConditionalDurations can be made dependents of other objects using the until: instance creation message.

See the class examples.
!


!ConditionalDuration methodsFor: 'updating'!
update: anAspect with: aValue
	"Respond to a change in the receiver's model"

	anAspect == #value
		ifTrue: []! !


!ConditionalDuration methodsFor: 'accessing'!
valueAt: anArg
	"Answer the result of passing the argument to the receiver's block."

	^value value: anArg! !

!ConditionalDuration methodsFor: 'accessing'!
wait
	"Cycle the receiver until the argument fulfills the receiver's block."

	^self waitUntil: []! !

!ConditionalDuration methodsFor: 'accessing'!
waitUntil: anArg
	"Cycle the receiver until the argument fulfills the receiver's block."

	[value value: anArg value] 
		whileFalse: ["Processor yield" (Delay forMilliseconds: 10) wait]! !


!ConditionalDuration methodsFor: 'private'!
species
	"Answer Duration."

	^Duration! !


!ConditionalDuration methodsFor: 'converting'!
mostGeneral
	"It is an error to try this here--we implement what's ok for CDs"

	^self error: 'Arithmetic is not possible with conditional durations.'! !


!ConditionalDuration methodsFor: 'arithmetic'!
* aValue
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!ConditionalDuration methodsFor: 'arithmetic'!
+ aDuration
	"Answer the sum of the receiver and the argument--the composition of two blocks"

	| block |
	^aDuration class == self class 
		ifTrue: [block _ aDuration value.
			self class new value:
					[ :a | (value value: a) and: [block value: a]]]
		ifFalse: [block := aDuration value.
			self class new value:
					[ :a | (value value: a) and: [a > block]]]! !

!ConditionalDuration methodsFor: 'arithmetic'!
- aValue
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!ConditionalDuration methodsFor: 'arithmetic'!
/ aValue
	"Answer that it is an error to attempt arithmetic with this magnitude."

	^self error: 'Arithmetic is undefined for this receiver.'! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ConditionalDuration class' category: #'Music-Models-Implementation'!
ConditionalDuration class
	instanceVariableNames: ''!

!ConditionalDuration class methodsFor: 'instance creation'!
on: aModel until: boolBlock
	"Answer a new conditional duration whose value is the given block"
	"ConditionalDuration until: [ :x | x > 5]"

	| me |
	me := ConditionalDuration value: boolBlock.
	aModel addDependent: me! !

!ConditionalDuration class methodsFor: 'instance creation'!
randomBetween: lo and: hi
	"Answer a new conditional duration whose value is between lo and hi (given in seconds)"

	| t val |
	val := ((Random new nextFrom: lo to: hi) * 1000000) truncated.
	t := Time microsecondClock.	
	^ConditionalDuration value: [ :x | Time microsecondClock > (t + val)].! !

!ConditionalDuration class methodsFor: 'instance creation'!
until: boolBlock
	"Answer a new conditional duration whose value is the given block"
	"ConditionalDuration until: [ :x | x > 5]"

	^ConditionalDuration value: boolBlock! !


!ConditionalDuration class methodsFor: 'examples'!
example
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

!ConditionalDuration class methodsFor: 'examples'!
exampleWithRands
	"Demonstrate the random duration"
	"ConditionalDuration exampleWithRands"

	| t0 t me |
	5 timesRepeat:
		[me := ConditionalDuration randomBetween: 0.25 and: 0.75.
		t0 := Time microsecondClock.		"get the time"
		me wait.
		t := Time microsecondClock.	
		Transcript show: ((t - t0) / 1000) truncated printString; cr.]! !


!classDefinition: #NominalMagnitude category: #'Music-Models-Implementation'!
MusicMagnitude subclass: #NominalMagnitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!NominalMagnitude commentStamp: '<historical>' prior: 0!
Instances of the NominalMagnitude classes are named (symbolic) properties where a symbol <--> value map is well-established for a given range and domain, e.g., 0.0 to 1.0 or 0 to 127.
Examples are pitch (#d4) or dynamic (#mp) names.

Class Instance Variable:
	NameMap	<Dictionary or Array of (Symbol -> Number or Interval)> the class' look-up table!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'NominalMagnitude class' category: #'Music-Models-Implementation'!
NominalMagnitude class
	instanceVariableNames: 'nameMap'!

!NominalMagnitude class methodsFor: 'class instance variables' stamp: 'stp 5/2/2022 14:40:58'!
nameMap
	^nameMap! !


!classDefinition: #SymbolicLoudness category: #'Music-Models-Implementation'!
NominalMagnitude subclass: #SymbolicLoudness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!SymbolicLoudness commentStamp: '<historical>' prior: 0!
Instances of SymbolicLoudness are dynamic names in the range #ppp to #fff.

The class instance variable NameMap is used for the name <--> MIDI velocity range (0 to 127) mapping dictionary.
!


!SymbolicLoudness methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a symbolic amplitude."

	aStream nextPutAll: value! !

!SymbolicLoudness methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' ampl'! !


!SymbolicLoudness methodsFor: 'private'!
species
	"Answer Amplitude."

	^Amplitude! !


!SymbolicLoudness methodsFor: 'converting'!
asDB
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(#ff ampl) asDB"

	^self asRatio asDB! !

!SymbolicLoudness methodsFor: 'converting'!
asMIDI
	"Answer a MIDIVelocity."
	"(#pp ampl) asMIDI"		"pianissimo is MIDI velocity 24"

	^MIDIVelocity new value:
			((self class nameMap keyAtValue: value) last)! !

!SymbolicLoudness methodsFor: 'converting'!
asRatio
	"Answer a RatioLoudness."
	"(#pp ampl) asRatio"		"pianissimo is 0.2"

	^RatioLoudness new value:
			(((self class nameMap keyAtValue: value) last / 120) asFloat)! !

!SymbolicLoudness methodsFor: 'converting'!
asSymbol
	"Answer a SymbolicLoudness."

	^self! !

!SymbolicLoudness methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asRatio! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SymbolicLoudness class' category: #'Music-Models-Implementation'!
SymbolicLoudness class
	instanceVariableNames: ''!

!SymbolicLoudness class methodsFor: 'class initialization' stamp: 'STP 5/2/2022 14:34:18'!
initialize
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


!SymbolicLoudness class methodsFor: 'instance creation' stamp: 'stp 5/2/2022 14:41:34'!
fromMIDI: aValue
	"Assuming value is a key velocity, answer a symbolic loudness name"
	"(Amplitude value: 39) asSymbol"

	| int |
	int := nameMap keys detect: [ :range | range includes: aValue]
				ifNone: [^self error: 'invalid Loudness range.'].
	^(self new) value: (nameMap at: int)! !


!SymbolicLoudness class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asDynamic! !


!classDefinition: #SymbolicPitch category: #'Music-Models-Implementation'!
NominalMagnitude subclass: #SymbolicPitch
	instanceVariableNames: 'fracPitch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!SymbolicPitch commentStamp: '<historical>' prior: 0!
Instances of SymbolicPitch represent well-tempered note names relative to a4=440Hz.
The range is c0 to g#9 and the values are symbols.
Note the confusion between the sharp sign (always placed after the note name) and Smalltalk's symbol key #.
One often writes ('c#3' asSymbol) to be safe.

Instance Variables:
	fracPitch <Float> the remainder for microtonal tunings

The class instance variable NameMap is used for the name <--> MIDI key number mapping array.
!


!SymbolicPitch methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a symbolic pitch."

	value printOn: aStream! !

!SymbolicPitch methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' pitch'! !


!SymbolicPitch methodsFor: 'arithmetic'!
+ aValue
	"Answer the sum of the receiver and the argument-handle adding Integers as a special case."

	^(aValue isMemberOf: self class)
		ifTrue: [self class new value: (value + aValue value)]
		ifFalse: [(aValue isMemberOf: SmallInteger)
			ifTrue: [SymbolicPitch fromMIDI: (self asMIDI value + aValue)]
			ifFalse: [super + aValue]]! !

!SymbolicPitch methodsFor: 'arithmetic'!
- aValue
	"Answer the sum of the receiver and the argument-handle adding Integers as a special case."

	^(aValue isMemberOf: self class)
		ifTrue: [self class new value: (value - aValue value)]
		ifFalse: [(aValue isMemberOf: SmallInteger)
			ifTrue: [SymbolicPitch fromMIDI: (self asMIDI value - aValue)]
			ifFalse: [super - aValue ]]! !

!SymbolicPitch methodsFor: 'arithmetic'!
transposeBy: aValue
	"transpose a SymbolicPitch by aValue in fractional halfsteps"

	^((self asFracMIDI transposeBy: aValue) asFracSymbol).! !


!SymbolicPitch methodsFor: 'private'!
species
	"Answer Pitch."

	^Pitch! !


!SymbolicPitch methodsFor: 'converting'!
asFracMIDI
	"Assuming value is a symbolic note name, answer a key number."
	"MIDI is 0=c-based, NameMap is 0=a-based"
	"(Pitch value: #c3) asFracMIDI"
	"(Pitch value: 'c#3' asSymbol) asFracMIDI"
	"( 'c#3' pitch fracPitch: -0.4) asFracMIDI"

	(fracPitch isNil)
	ifTrue: [^self asMIDI]
	ifFalse: [^(self asMIDI value + fracPitch) key].! !

!SymbolicPitch methodsFor: 'converting'!
asHertz
	"assuming value is a symbolic note name, return a frequency "
	"(Pitch value: #a6) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: #c5) asHertz"		"261.0 is Midi key 60 or middle C"

	^(self asFracMIDI) asHertz! !

!SymbolicPitch methodsFor: 'converting'!
asHz
	"assuming value is a symbolic note name, return a frequency "
	"(Pitch value: #a6) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: #c5) asHertz"		"261.0 is Midi key 60 or middle C"

	^self asHertz! !

!SymbolicPitch methodsFor: 'converting'!
asMIDI
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

!SymbolicPitch methodsFor: 'converting'!
asSymbol
	"Answer a SymbolicPitch."

	^self.! !

!SymbolicPitch methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !


!SymbolicPitch methodsFor: 'accessing'!
accidental
	"Answer the receiver's accidental, if any."

	| c |
	(value size > 1)
		ifFalse: [^nil]
		ifTrue: [c := value at: 2].
	^((c == $#) or: [c == $b])
		ifTrue: [c]
		ifFalse: [nil]! !

!SymbolicPitch methodsFor: 'accessing'!
fracPitch
	"return microtonal offset as fractions of a halfstep"

	^fracPitch! !

!SymbolicPitch methodsFor: 'accessing'!
fracPitch: aValue
	"set microtonal offset as fractions of a halfstep"

	fracPitch := aValue.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SymbolicPitch class' category: #'Music-Models-Implementation'!
SymbolicPitch class
	instanceVariableNames: ''!

!SymbolicPitch class methodsFor: 'class initialization' stamp: 'stp 5/2/2022 14:42:06'!
initialize
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


!SymbolicPitch class methodsFor: 'instance creation' stamp: 'stp 5/2/2022 14:41:42'!
fromFracMIDI: aValue
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

!SymbolicPitch class methodsFor: 'instance creation' stamp: 'stp 5/2/2022 14:41:48'!
fromMIDI: aValue
	"Assuming value is a key number, answer a symbolic pitch name"
	"SymbolicPitch fromMIDI: (Pitch value: 69)"
	"SymbolicPitch fromMIDI: (Pitch value: 68.9 key)"
	"(fromMIDI throws away microtonal offset, fromFracMIDI keeps it.)"	

	^self value: ((nameMap detect: 
					[ :ass | 
					ass value = (aValue value rounded + 3 \\ 12)]) key, 
		(aValue value // 12 - 2) printString).! !


!SymbolicPitch class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asNote! !


!classDefinition: #NumericalMagnitude category: #'Music-Models-Implementation'!
MusicMagnitude subclass: #NumericalMagnitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!NumericalMagnitude commentStamp: '<historical>' prior: 0!
Instances of the subclasses of NumericalMagnitude are music magnitudes with numerical values.
When the values are floating-point numbers, the range is often 0.0 to 1.0.
Integer ranges such as 0 to 127 can also be used in subclasses.

One may want to add range-checking for these cases.

Note that the class NumericalMagnitude is vacuous at present, and exists solely for representational modeling.!


!NumericalMagnitude methodsFor: 'double dispatching'!
adaptInteger: val
	^self class value: val! !

!NumericalMagnitude methodsFor: 'double dispatching'!
adaptToInteger
	^self value! !


!classDefinition: #HertzPitch category: #'Music-Models-Implementation'!
NumericalMagnitude subclass: #HertzPitch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!HertzPitch commentStamp: '<historical>' prior: 0!
Instances of HertzPitch are frequency values in Hertz.
This is the most general Pitch representation.
The value is assumed to be a floating-point number.!


!HertzPitch methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a pitch string in Hertz."

	aStream nextPutAll: value printString, ' Hz.'! !

!HertzPitch methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' Hz'! !


!HertzPitch methodsFor: 'private'!
species
	"Answer Pitch."

	^Pitch! !


!HertzPitch methodsFor: 'converting'!
asFracMIDI
	"Assuming value is a frequency, Answer a fractional key number"
	"(Pitch value: 440.0) asFracMIDI"		"440.0 is MIDI key 69.0"
	"(Pitch value: 445.0) asFracMIDI" 	"445.0 is MIDI key 69.195..."

	"(8.17579891564371 Hz is the frequency of C0.)"

	^MIDIPitch new value: (((value / 8.17579891564371) log: 2) * 12)! !

!HertzPitch methodsFor: 'converting'!
asFracSymbol
	"Assuming value is a frequency, answer a symbolic note name"
	"(Pitch value: 880.0) asSymbol"			"880.0 is a4"
	"(438.0 Hz) asFracSymbol inspect"			"262.0 is a3 - an offset"
	"(asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromFracMIDI: (self asFracMIDI)! !

!HertzPitch methodsFor: 'converting'!
asHertz
	"Answer a HertzPitch."

	^self! !

!HertzPitch methodsFor: 'converting'!
asHz
	"Answer a HertzPitch."

	^self! !

!HertzPitch methodsFor: 'converting'!
asMIDI
	"Assuming value is a frequency, Answer a key number"
	"(Pitch value: 440.0) asMIDI"		"440.0 is MIDI key 69"
	"(Pitch value: 261.3) asMIDI"		"261.3 is MIDI key 60"
		
	^MIDIPitch new value: (self asFracMIDI value rounded).! !

!HertzPitch methodsFor: 'converting'!
asSymbol
	"Assuming value is a frequency, answer a symbolic note name"
	"(Pitch value: 880.0) asSymbol"			"880.0 is a4"
	"(438.0 Hz) asSymbol"			"438.0 is a3 - an offset."
	"(asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromMIDI: (self asMIDI)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'HertzPitch class' category: #'Music-Models-Implementation'!
HertzPitch class
	instanceVariableNames: ''!

!HertzPitch class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asHz! !


!classDefinition: #IntervalMagnitude category: #'Music-Models-Implementation'!
NumericalMagnitude subclass: #IntervalMagnitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!IntervalMagnitude commentStamp: '<historical>' prior: 0!
Instances of subclasses of the abstract class IntervalMagnitude are NumericalMagnitudes where a specific interval is defined within which values are possible. Examples are MIDI values in the range of 0 .. 127. the actual range is a class inst var.!


!IntervalMagnitude methodsFor: 'accessing'!
value: theValue
	"Accept theValue, as the new instance variable 'value' -- perform  range-checking"

"	| range |
	range := self class range.
	range == nil
		ifFalse: [((theValue < range first) or: [theValue > range last])
			ifTrue: [self error: 'value out of range.']].
"	value := theValue! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'IntervalMagnitude class' category: #'Music-Models-Implementation'!
IntervalMagnitude class
	instanceVariableNames: ''!

!IntervalMagnitude class methodsFor: 'class inst var access'!
range
	"Answer the class' range."

	^range! !

!IntervalMagnitude class methodsFor: 'class inst var access'!
range: anInterval
	"Set the class' range."

	range := anInterval! !


!classDefinition: #MIDIPitch category: #'Music-Models-Implementation'!
IntervalMagnitude subclass: #MIDIPitch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!MIDIPitch commentStamp: '<historical>' prior: 0!
Instances of MIDIPitch are well-tempered piano key numbers where 60 is middle-c (c3) according to the MIDI standard.

The translation key <--> Hz. is done with the logarithm or 27.5 Hz. (very low A = 440/16).

Note that AdC's additions make fractional MIDI pitches possible, whereby the first 2 digits to the right of the decimal point signify pitch cents. There are coercion methods such as asFracMIDI for handling microtonal MIDI pitches.
!


!MIDIPitch methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' key'! !


!MIDIPitch methodsFor: 'private'!
species
	"Answer Pitch."

	^Pitch! !


!MIDIPitch methodsFor: 'converting'!
asFracMIDI
	"Answer a fractionalMIDIPitch (or integer if value is no fraction)."
	"60 key asFracMIDI "
	"60.4 key asFracMIDI "

	^self! !

!MIDIPitch methodsFor: 'converting'!
asFracSymbol
	"Assuming value is a key number, answer a symbolic pitch name"
	"(Pitch value: 37) asSymbol "
	"(Pitch value: 41.6 key) asSymbol "
	"(443 Hz) asFracSymbol inspect"
	"( asSymbol throws away microtonal offset, asFracSymbol keeps it.)"	

	^SymbolicPitch fromFracMIDI: value.! !

!MIDIPitch methodsFor: 'converting'!
asHertz
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

!MIDIPitch methodsFor: 'converting'!
asHz
	"Assuming value is a key number, answer a frequency"
	"(Pitch value: 69) asHertz"		"440.0 is Midi key 69"
	"(Pitch value: 60) asHertz"		"261.0 is Midi key 60"
	"(Pitch value: 60.51 key) asHertz"	"269.44... is Midi key 60 + 51 cents."

	^self asHertz! !

!MIDIPitch methodsFor: 'converting'!
asMIDI
	"Answer a MIDIPitch."
	"60 key asMIDI "
	"60.6 key asMIDI "

	(value isMemberOf: SmallInteger)
			ifTrue: [^self].
	(value isMemberOf: Float)
			ifTrue: [^self value rounded key].! !

!MIDIPitch methodsFor: 'converting'!
asSymbol
	"Assuming value is a key number, answer a symbolic pitch name"
	"(Pitch value: 37) asSymbol"

	^SymbolicPitch fromMIDI: value! !

!MIDIPitch methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MIDIPitch class' category: #'Music-Models-Implementation'!
MIDIPitch class
	instanceVariableNames: ''!

!MIDIPitch class methodsFor: 'class initialization'!
initialize
	"Initialize the class instance variable."
	"MIDIPitch initialize."

	range := 1 to: 127! !


!MIDIPitch class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asKey! !


!classDefinition: #MIDIVelocity category: #'Music-Models-Implementation'!
IntervalMagnitude subclass: #MIDIVelocity
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!MIDIVelocity commentStamp: '<historical>' prior: 0!
Instances of MIDIVelocity are key velocities (approximately proportional to loudness) in the range 0 to 127.

The SymbolicLoudness class maps symbolic dynamic names onto this range on an approximately logarithmic scale.!


!MIDIVelocity methodsFor: 'accessing'!
value: theValue
	"Truncate"

	super value: theValue.
	value := value truncated.! !


!MIDIVelocity methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a MIDI velocity."

	aStream nextPutAll: "'v:'," value printString! !

!MIDIVelocity methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' vel'! !


!MIDIVelocity methodsFor: 'private'!
species
	"Answer Amplitude."

	^Amplitude! !


!MIDIVelocity methodsFor: 'converting'!
asDB
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(MIDIVelocity value: 64) asDB"

	^DBLoudness new value: (20 * ((value / 127) asFloat log))! !

!MIDIVelocity methodsFor: 'converting'!
asMIDI
	"Answer a MIDIVelocity."

	^self! !

!MIDIVelocity methodsFor: 'converting'!
asRatio
	"Answer a RatioLoudness."

	^RatioLoudness new value: ((value / 127.0) asFloat)! !

!MIDIVelocity methodsFor: 'converting'!
asSymbol
	"Answer a symbolic loudness."
	"(Amplitude value: 107) asSymbol"	"107 is fff"

	^SymbolicLoudness fromMIDI: value! !

!MIDIVelocity methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asRatio! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MIDIVelocity class' category: #'Music-Models-Implementation'!
MIDIVelocity class
	instanceVariableNames: ''!

!MIDIVelocity class methodsFor: 'class initialization'!
initialize
	"Initialize the class instance variable."
	"MIDIVelocity initialize."

	range := 0 to: 127! !


!MIDIVelocity class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asVelocity! !


!classDefinition: #MeasureDuration category: #'Music-Models-Implementation'!
NumericalMagnitude subclass: #MeasureDuration
	instanceVariableNames: 'timeSignature'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!MeasureDuration commentStamp: '<historical>' prior: 0!
A MeasureDuration reprsents time as beats and measures.

It is partially implemented and unused.

Instance Variables:
	timeSignature	<Fraction>  my time signature
!


!MeasureDuration methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' measures'! !


!MeasureDuration methodsFor: 'private'!
species
	"Answer Duration."

	^Duration! !


!classDefinition: #RatioMagnitude category: #'Music-Models-Implementation'!
NumericalMagnitude subclass: #RatioMagnitude
	instanceVariableNames: 'relative'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!RatioMagnitude commentStamp: '<historical>' prior: 0!
Instances of the RatioMagnitude subclasses are fractional values relative to some context-defined reference value--their relative. The relative may be provided, or it may be assumed if uninitialized (e.g., for a default whole note's duration or default reference pitch).

Instance Variable:
	relative		<MusicMagnitude> the reference value!


!RatioMagnitude methodsFor: 'accessing'!
realValue
	"Answer the receiver's value mapped to the receiver."

	^value! !

!RatioMagnitude methodsFor: 'accessing'!
value
	"Answer the receiver's value mapped to the receiver."

	^relative == nil
		ifTrue: [value]
		ifFalse: [self class new value: (relative value * value)]! !


!RatioMagnitude methodsFor: 'converting'!
asRatio
	"Answer self."

	^self! !

!RatioMagnitude methodsFor: 'converting'!
map
	"Apply the receivers reference."

	relative == nil
		ifFalse: [value := value * relative value.
				relative := nil]! !

!RatioMagnitude methodsFor: 'converting'!
relativeTo: aRelative
	"Set the receivers reference."

	relative := aRelative! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RatioMagnitude class' category: #'Music-Models-Implementation'!
RatioMagnitude class
	instanceVariableNames: ''!

!RatioMagnitude class methodsFor: 'instance creation'!
value: aValue relative: aMMagnitude
	"Answer and instance with the argument as its value."

	^(self new value: aValue) relativeTo: aMMagnitude! !


!classDefinition: #RatioDuration category: #'Music-Models-Implementation'!
RatioMagnitude subclass: #RatioDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!RatioDuration commentStamp: '<historical>' prior: 0!
Instances of RatioDuration are 'beat' fractions.
They can be expanded into msec. relative to some given event (a whole note), or use the default tempo of 1 sec.!


!RatioDuration methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a fractional duration."

	aStream nextPutAll: "'d:'," value printString! !

!RatioDuration methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' beat'! !


!RatioDuration methodsFor: 'private'!
species
	"Answer Duration."

	^Duration! !


!RatioDuration methodsFor: 'double dispatching'!
quotientFromInteger: numerator
	"Answer a MM whose value is the argument over the receiver's value."
	"1/4 beat"

	^self species relativeMember value: (numerator / value)! !


!RatioDuration methodsFor: 'converting'!
asMS
	"Answer a MSecondDuration."

	^relative == nil
		ifTrue: [MSecondDuration new value: ((value * 1000) truncated)]
		ifFalse: [MSecondDuration new value:
					((value * 1000 * (relative asRatio value)) truncated)]! !

!RatioDuration methodsFor: 'converting'!
asMsec
	"Answer a MSecondDuration."

	^relative == nil
		ifTrue: [MSecondDuration new value: ((value * 1000) truncated)]
		ifFalse: [MSecondDuration new value:
					((value * 1000 * (relative asRatio value)) truncated)]! !

!RatioDuration methodsFor: 'converting'!
asSec
	"Answer a SecondDuration."

	^relative == nil
		ifTrue: [SecondDuration new value: value asFloat]
		ifFalse: [SecondDuration new value:
					(value asFloat * (relative asRatio value))]! !

!RatioDuration methodsFor: 'converting'!
asUsec
	"Answer a USecondDuration."

	^self asUseconds! !

!RatioDuration methodsFor: 'converting'!
asUseconds
	"Answer a USecondDuration."

	^relative == nil
		ifTrue: [USecondDuration new value: ((value * 1000000) truncated)]
		ifFalse: [USecondDuration new value:
					((value * 1000000 * (relative asRatio value)) truncated)]! !

!RatioDuration methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asSec! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RatioDuration class' category: #'Music-Models-Implementation'!
RatioDuration class
	instanceVariableNames: ''!

!RatioDuration class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asBeat! !


!classDefinition: #RatioLoudness category: #'Music-Models-Implementation'!
RatioMagnitude subclass: #RatioLoudness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!RatioLoudness commentStamp: '<historical>' prior: 0!
Instances of RatioLoudness are fractional amplitude values normally relative to the range 0 to 1.
!


!RatioLoudness methodsFor: 'private'!
species
	"Answer Amplitude."

	^Amplitude! !


!RatioLoudness methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a ratio."

	aStream nextPutAll: "'a:'," value printString! !


!RatioLoudness methodsFor: 'converting'!
asDB
	"Answer a dB loudness; ratio 1 = 0dB, ratio 0.5 = -6dB, etc."
	"(RatioLoudness value: 0.25) asDB"

	^DBLoudness new value: (20 * (value log))! !

!RatioLoudness methodsFor: 'converting'!
asMIDI
	"Answer a MIDI key velocity (0 to 127)"
	
	^MIDIVelocity new value: ((value * 127.0) asInteger)! !

!RatioLoudness methodsFor: 'converting'!
asSymbol
	"Answer a symbolic dynamic."
	
	^(self asMIDI) asSymbol! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RatioLoudness class' category: #'Music-Models-Implementation'!
RatioLoudness class
	instanceVariableNames: ''!

!RatioLoudness class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asAmpl! !


!classDefinition: #DBLoudness category: #'Music-Models-Implementation'!
RatioLoudness subclass: #DBLoudness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!DBLoudness commentStamp: '<historical>' prior: 0!
Instances of DBLoudness represent deciBel loudness values.
!


!DBLoudness methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' dB'! !


!DBLoudness methodsFor: 'double dispatch'!
differenceFromDynamic: aDyn
	"Subtract the argument from the receiver after turning both into db."

	^DBLoudness value: (self value - aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
differenceFromVelocity: aDyn
	"Subtract the argument from the receiver after turning both into db."

	^DBLoudness value: (self value - aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
productFromDynamic: aDyn
	"Multiply the argument by the receiver after turning both into db."

	^DBLoudness value: (self value * aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
productFromVelocity: aDyn
	"Multiply the argument by the receiver after turning both into db."

	^DBLoudness value: (self value * aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
quotientFromDynamic: aDyn
	"Divide the argument by the receiver after turning both into db."

	^DBLoudness value: (self value / aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
quotientFromVelocity: aDyn
	"Divide the argument by the receiver after turning both into db."

	^DBLoudness value: (self value / aDyn asDB value)! !

!DBLoudness methodsFor: 'double dispatch'!
sumFromDynamic: aDyn
	"Add the argument to the receiver after turning both into db."

	^DBLoudness value: (self value + aDyn asDB value)! !


!DBLoudness methodsFor: 'private'!
species
	"Answer Amplitude."

	^Amplitude! !


!DBLoudness methodsFor: 'converting'!
asMIDI
	"Answer a MIDI key velocity (0 to 127)"
	
	^self asRatio asMIDI! !

!DBLoudness methodsFor: 'converting'!
asRatio
	"Answer the receiver as a ratio between 0 and 1."
	"-12 dB asRatio"

	^RatioLoudness new value: (10 raisedTo: (value / 20))! !

!DBLoudness methodsFor: 'converting'!
positiveDB
	"Answer the zero-based positive dB value of the receiver."
	"90 dB = 32000"
	"0 = 90		-6 = 80		-12 = 70"

	^90 + (value / 0.6)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DBLoudness class' category: #'Music-Models-Implementation'!
DBLoudness class
	instanceVariableNames: ''!

!DBLoudness class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asDB! !


!classDefinition: #RatioPitch category: #'Music-Models-Implementation'!
RatioMagnitude subclass: #RatioPitch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!RatioPitch commentStamp: '<historical>' prior: 0!
Instances of RatioPitch are fractional pitch values normally relative to c=261.623Hz.
They can be transformed into other values (e.g., note names or key numbers), but often at a loss of accuracy if they are not well-tempered.
!


!RatioPitch methodsFor: 'private'!
species
	"Answer Pitch."

	^Pitch! !


!RatioPitch methodsFor: 'printing'!
printOn0: aStream
	"Print the receiver as a ratio."

	aStream nextPutAll: "'p:'," value printString! !


!RatioPitch methodsFor: 'converting'!
asFracMIDI
	"return a RatioPitch as a fractional MIDINote."

	^self asHertz asFracMIDI.! !

!RatioPitch methodsFor: 'converting'!
asFracSymbol
	"Assuming value is a ratio, answer a symbolic note name"

	^SymbolicPitch fromFracMIDI: (self asFracMIDI)! !

!RatioPitch methodsFor: 'converting'!
asHertz
	"Answer a HertzPitch."

	^relative == nil
		ifTrue: [HertzPitch new value: (value * 261.623)]
		ifFalse: [HertzPitch new value:
					(value * (relative asHertz value))]! !

!RatioPitch methodsFor: 'converting'!
asMIDI
	"return a RatioPitch as a MIDINote (rounded)."

	^self asHertz asMIDI.! !

!RatioPitch methodsFor: 'converting'!
asSymbol
	"Assuming value is a ratio, answer a symbolic note name"

	^SymbolicPitch fromMIDI: (self asMIDI)! !

!RatioPitch methodsFor: 'converting'!
mostGeneral
	"Answer the most numerically meaningful version of the receiver."

	^self asHertz! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'RatioPitch class' category: #'Music-Models-Implementation'!
RatioPitch class
	instanceVariableNames: ''!

!RatioPitch class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asStep! !


!classDefinition: #SecondDuration category: #'Music-Models-Implementation'!
NumericalMagnitude subclass: #SecondDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!SecondDuration commentStamp: '<historical>' prior: 0!
Instances of SecondDuration are duration times in floating-point seconds.
This is among the most general duration time representations.!


!SecondDuration methodsFor: 'printing'!
printOn: aStream
	"Print the receiver on the argument as a MM declaration."

	| valstr |
	valstr := value printString.
	valstr size > 6
		ifTrue: [valstr := valstr copyFrom: 1 to: 5].
	aStream nextPutAll: '(', valstr, self units, ')'! !

!SecondDuration methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' sec'! !


!SecondDuration methodsFor: 'process delay'!
wait
	"Delay for a time corresponding to the receiver"
	"1.5 seconds wait"

	(Delay forMicroseconds: self asUseconds value) wait! !


!SecondDuration methodsFor: 'private'!
mostGeneral
	"Answer the receiver in seconds."

	^self asSec! !

!SecondDuration methodsFor: 'private'!
species
	"Answer Duration."

	^Duration! !


!SecondDuration methodsFor: 'converting'!
adaptToFloat
	"Answer a float of seconds."

	^self asSeconds value! !

!SecondDuration methodsFor: 'converting'!
asBeat
	"Answer a RatioDuration."

	^RatioDuration new value: (self asSec value)! !

!SecondDuration methodsFor: 'converting'!
asMS
	"Answer a MSecondDuration."

	^self asMsec! !

!SecondDuration methodsFor: 'converting'!
asMsec
	"Answer a MSecondDuration."

	^self asMseconds! !

!SecondDuration methodsFor: 'converting'!
asMseconds
	"Answer a MSecondDuration."

	^MSecondDuration new value: (value * 1000) truncated! !

!SecondDuration methodsFor: 'converting'!
asRatio
	"Answer a RatioDuration."

	^RatioDuration new value: (self asSec value asFloat)! !

!SecondDuration methodsFor: 'converting'!
asSec
	"Answer a float of seconds."

	^self asSeconds! !

!SecondDuration methodsFor: 'converting'!
asSeconds
	"Answer a float of seconds."

	^self! !

!SecondDuration methodsFor: 'converting'!
asUsec
	"Answer a USecondDuration."

	^self asUseconds! !

!SecondDuration methodsFor: 'converting'!
asUseconds
	"Answer an int of micro seconds."

	^USecondDuration new value: (value * 1000000) truncated! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SecondDuration class' category: #'Music-Models-Implementation'!
SecondDuration class
	instanceVariableNames: ''!

!SecondDuration class methodsFor: '-- all --'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asSec! !


!classDefinition: #MSecondDuration category: #'Music-Models-Implementation'!
SecondDuration subclass: #MSecondDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!MSecondDuration commentStamp: '<historical>' prior: 0!
Instances of MSecondDuration are duration times in milli-seconds.
!


!MSecondDuration methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' msec'! !


!MSecondDuration methodsFor: 'converting'!
asMseconds
	"Answer a MSecondDuration."

	^self! !

!MSecondDuration methodsFor: 'converting'!
asSeconds
	"Answer a float of seconds."

	^SecondDuration new value: (value / 1000) asFloat! !

!MSecondDuration methodsFor: 'converting'!
asUseconds
	"Answer a float of micro seconds."

	^USecondDuration new value: (value * 1000) truncated! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'MSecondDuration class' category: #'Music-Models-Implementation'!
MSecondDuration class
	instanceVariableNames: ''!

!MSecondDuration class methodsFor: 'coercion'!
ddMsgName
	"Answer the selector to be used in coercing double-dispatching messages."

	^#asMsec! !


!classDefinition: #USecondDuration category: #'Music-Models-Implementation'!
SecondDuration subclass: #USecondDuration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!USecondDuration commentStamp: '<historical>' prior: 0!
Instances of USecondDuration are duration times in micro-seconds.
This is the default time representation (most general duration) and is usually used for keys in event lists.!


!USecondDuration methodsFor: 'printing'!
units
	"Answer the units string of the receiver."

	^' usec'! !


!USecondDuration methodsFor: 'converting'!
asMseconds
	"Answer a MSecondDuration."

	^MSecondDuration new value: (value / 1000) truncated! !

!USecondDuration methodsFor: 'converting'!
asSeconds
	"Answer a float of seconds."

	^SecondDuration new value: (value / 1000000) asFloat! !

!USecondDuration methodsFor: 'converting'!
asUseconds
	"Answer a float of micro seconds."

	^self! !


!classDefinition: #OrdinalMagnitude category: #'Music-Models-Implementation'!
MusicMagnitude subclass: #OrdinalMagnitude
	instanceVariableNames: 'table'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!OrdinalMagnitude commentStamp: '<historical>' prior: 0!
Instances of the OrdinalMagnitude classes are order-only magnitudes.
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


!OrdinalMagnitude methodsFor: 'accessing'!
hash
	"Answer a SmallInteger unique to the receiver.  Essential.  See  
	Object documentation whatIsAPrimitive."

	<primitive: 75>
	^self! !

!OrdinalMagnitude methodsFor: 'accessing'!
table
	"Answer the receiver's loop-up table--its or the class'."

	^table == nil
		ifFalse: [table]
		ifTrue: [self class table == nil
			ifFalse: [self class table at: self]
			ifTrue: [table := Dictionary new.
					table]]! !

!OrdinalMagnitude methodsFor: 'accessing'!
value
	"Signal an error."

	^self error: 'OrdinalMagnitudes don''t have values!!'! !

!OrdinalMagnitude methodsFor: 'accessing'!
value: theValue
	"Signal an error."

	^super value: theValue
"
	^self error: 'OrdinalMagnitudes don''t have values!!'
"! !


!OrdinalMagnitude methodsFor: 'initialize-release'!
release
	"Release the receiver's table."

	| relatives |
	table == nil
		ifFalse: [relatives := table keys.
			table := nil.
			relatives do: [ :m | m release]]! !


!OrdinalMagnitude methodsFor: 'printing'!
printOn: aStream
	"Print the receiver as an ordinal magnitude."

	value == nil
		ifTrue: [aStream nextPutAll:
						(self class name "copyFrom: 1 to: 3")]
		ifFalse: [aStream nextPutAll: value printString].! !


!OrdinalMagnitude methodsFor: 'converting'!
mostGeneral
	"Answer that it is an error to attempt coercion with ordinal magnitudes."

	^self error: 'Coercion is undefined for this receiver.'! !


!OrdinalMagnitude methodsFor: 'ordering'!
< anotherOMag
	"Answer whether the receiver is less than the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [((self table at: anotherOMag) == #<)]
		ifFalse: [((self table associations detect:
					[ :z |
					(z value == #<) and: [z key < anotherOMag]]
			ifNone: [nil]) ~~ nil)]! !

!OrdinalMagnitude methodsFor: 'ordering'!
<< anotherOMag
	"Specify that the receiver is less than the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #<
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #<.
				anotherOMag >> self]! !

!OrdinalMagnitude methodsFor: 'ordering'!
<= anotherOMag
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

!OrdinalMagnitude methodsFor: 'ordering'!
=< anotherOMag
	"Specify that the receiver is less than or equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #<=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #<=.
				anotherOMag => self]! !

!OrdinalMagnitude methodsFor: 'ordering'!
== anotherOMag
	"Specify that the receiver is equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #=.
				anotherOMag == self]! !

!OrdinalMagnitude methodsFor: 'ordering'!
=> anotherOMag
	"Specify that the receiver is greater than or equal to the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>=
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #>=.
				anotherOMag =< self]! !

!OrdinalMagnitude methodsFor: 'ordering'!
=? anotherOMag
	"Answer whether the receiver is equal to the argument."

	^self == anotherOMag
		ifTrue: [true]
		ifFalse: [(self table includesKey: anotherOMag)
			ifTrue: [((self table at: anotherOMag) == #=)]
			ifFalse: [((self table associations detect:
					[ :z |
					(z value == #=) and: [z key =? anotherOMag]]
				ifNone: [nil]) ~~ nil)]]! !

!OrdinalMagnitude methodsFor: 'ordering'!
> anotherOMag
	"Answer whether the receiver is greater than the argument."

	^(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>]
		ifFalse: [(self table associations detect:
					[ :z |
					(z value == #>) and: [z key > anotherOMag]]
			ifNone: [nil]) ~~ nil]! !

!OrdinalMagnitude methodsFor: 'ordering'!
>= anotherOMag
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

!OrdinalMagnitude methodsFor: 'ordering'!
>> anotherOMag
	"Specify that the receiver is greater than the argument."

	(self table includesKey: anotherOMag)
		ifTrue: [(self table at: anotherOMag) == #>
			ifFalse: [^self error: 'relationship ',
							(self table at: anotherOMag),
							' already specified.']]
		ifFalse: [self table at: anotherOMag put: #>.
				anotherOMag << self]! !


!OrdinalMagnitude methodsFor: 'arithmetic'!
* aValue
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!OrdinalMagnitude methodsFor: 'arithmetic'!
+ aValue
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!OrdinalMagnitude methodsFor: 'arithmetic'!
- aValue
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!OrdinalMagnitude methodsFor: 'arithmetic'!
/ aValue
	"Answer that it is an error to attempt arithmetic with ordinal magnitudes."

	^self error: 'Arithmetic is undefined for this receiver.'! !

!OrdinalMagnitude methodsFor: 'arithmetic'!
= aValue
	"Answer whether the receiver and the argument are equivalent."

	^self == aValue! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OrdinalMagnitude class' category: #'Music-Models-Implementation'!
OrdinalMagnitude class
	instanceVariableNames: ''!

!OrdinalMagnitude class methodsFor: 'table access'!
table
	"Answer the class' instance look-up table."

	^Table! !

!OrdinalMagnitude class methodsFor: 'table access'!
values
	"Answer the sorted values."
	
	^Table == nil
		ifTrue: [self allInstances asSortedCollection]
		ifFalse: [Table keys asSortedCollection]! !


!OrdinalMagnitude class methodsFor: 'instance creation'!
new
	"Answer a new instance and, if it's in use, add it to the table."

	| me |
	me := super new.
	Table == nil ifFalse: [Table at: me put: Dictionary new].
	^me! !

!OrdinalMagnitude class methodsFor: 'instance creation'!
value: aValue
	"Answer an instance"

	^self new value: aValue! !


!OrdinalMagnitude class methodsFor: 'class initialization'!
flush
	"Release the shared class table and all instances."
	"OrdinalMagnitude allSubclassesDo: [ :t | t flush]"

	Table == nil
		ifFalse: [Table keysDo: [ :d | d release].
				Table := nil]
		ifTrue: [self allInstancesDo: [ :d | d release]]! !

!OrdinalMagnitude class methodsFor: 'class initialization'!
useTable
	"Set up a shared class table for all instances."

	Table := Dictionary new.! !


!classDefinition: #Length category: #'Music-Models-Implementation'!
OrdinalMagnitude subclass: #Length
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!Length commentStamp: '<historical>' prior: 0!
Instances of Length represent subjective length (~ duration * loudness) values.
The instance variable tables are used for the name -> relation symbol map.

See the class examples.!


!Length methodsFor: 'private'!
species
	"Answer Duration."

	^Duration! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Length class' category: #'Music-Models-Implementation'!
Length class
	instanceVariableNames: ''!

!Length class methodsFor: 'examples'!
example
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


!classDefinition: #Sharpness category: #'Music-Models-Implementation'!
OrdinalMagnitude subclass: #Sharpness
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Models-Implementation'!
!Sharpness commentStamp: '<historical>' prior: 0!
Instances of Sharpness represent subjective sharpness (~ pitch * loudness) values.
The instance variable tables are used for the name -> relation symbol map.

See the class examples.!


!Sharpness methodsFor: 'private'!
species
	"Answer Pitch."

	^Pitch! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Sharpness class' category: #'Music-Models-Implementation'!
Sharpness class
	instanceVariableNames: ''!

!Sharpness class methodsFor: 'examples'!
example
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

SymbolicLoudness initialize!
SymbolicPitch initialize!
MIDIPitch initialize!
MIDIVelocity initialize!