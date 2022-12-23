DurationEvent subclass: #Function
Class Function is concrete and represents functions that are described by a array of data points assumed to lie equally-spaced in the unit interval.
Functions are normally created from an array of values over the unit interval; x varies from 0.0 to 1.0 and y is free over that range.
One can address them within the unit interval with atX: or one can address them with integer indeces up to the data set's size with atIndex: (can be dangerous).

Examples are a ramp from 0 to 1 such as:
	[(LinearFunction from: #((0 0) (1 2))) at: 0.33]
or a spline that traces a sine-like path centered around 1:
	[(SplineFunction from: #((0 1) (0.33 2) (0.67 0) (1 1))) at: 0.35]

InstanceVariables:
	data <Array of Numbers, OrderedCollection of (Z)Points, or UninterpretedBytes>
				the data values or breakpoints of the function
	domain <Interval> the x-range
	range <Interval> the y-range

!
	"Open a function view on the receiver."
	"(LinearFunction default scaleBy: 256@1) edit"

	FunctionView onFunction: self! !
	"Open a function view on the receiver."
	"(LinearFunction default scaleBy: 256@1) edit"

	extent isPoint
		ifTrue: [FunctionView onFunction: self x: extent x y: extent y]
		ifFalse: [FunctionView onFunction: self x: extent y: 256]! !
	^#ratio! !
	"Answer an averaged version of the receiver of the given size."
	"(Function readFloatsFrom: (MusicMagnitude modeRootName, 'data/y2.2.env')) averagedTo: 32"
 
	| ilen factor fcn sum index |
	ilen := self size.
	fcn := Function ofSize: size.
	size > ilen
		ifTrue: [^self interpolatedTo: size].
	factor := (size / ilen) asFloat.
	1 to: size do:
			[ :i |
			sum := 0.
			0 to: size do:
				[ :j |
				index :=  (i + j * factor) truncated max: 1.
				sum := sum + (data at: index)].
			fcn add: (sum/size)].
	^fcn! !
	"Release the receiver's 'data'."

	data := nil! !
	"Answer a version of the receiver of the given size taking the maximum value of each window."
	"(Function readFloatsFrom: 'snd/yy/ec/syl/y2.env') maxTo: 32 win: 8"
 
	| ilen factor fcn step maxv |
	ilen := self size.
	fcn := Function ofSize: size.
	size > ilen
		ifTrue: [^self interpolatedTo: size].
	factor := (1/size) asFloat.
	step := (factor / wsize) asFloat.
	0 to: size -1 do:
			[ :i |
			maxv := 0.
			0 to: wsize -1 do:
				[ :j |
				maxv := maxv max: (self at: (i*factor)+(j+step)) abs].
			fcn add: maxv].
	^fcn! !
	"Answer a down-sampled version of the receiver of the given size."
	"((Function readFloatsFrom: (MusicMagnitude modeRootName, 'data/y2.2.env')) sampledTo: 64) edit"
 
	| ilen factor fcn |
	ilen := self size.
	fcn := Function ofSize: size.
	size > ilen
		ifTrue: [^self interpolatedTo: size].
	factor := (size / ilen) asFloat.
	1 to: size do:
			[ :i |
			fcn add: (self at: i*factor)].
	^fcn! !
	"Answer a version of the receiver smoothed to about 32 points."
	"| arr snd fcn|
	arr := Array new: 512.
	snd := 'Nomad:Squeak:Siren:sounds:kombination.1.aiff' asSoundFile.
	snd rmsFrom: 1 to: snd size into: arr.
	fcn := (Function from: arr).
	fcn smoothed edit"

	| nu avgs sc bre prev epsilon |
	nu := 64.
	avgs := Array new: nu.
	sc := (data size / nu) truncated.
"get the running averages"
	1 to: nu do:
		[ :in | | sum |
		1 to: sc do:
			[ :sa | | val |
			sum := 0.0.
			val := data at: (in - 1 * sc + sa).
			sum := sum + val].
		avgs at: in put: (sum / sc)].
"try to find constant regions and jumps"
	bre := OrderedCollection new: nu / 2.
	prev := avgs first.
	bre add: 0 @ prev.		"first breakpoint"
	epsilon := 0.33.
	2 to: nu - 1 do:
		[ :in | | this diff dif2 next |
		this := avgs at: in.
		next := avgs at: in + 1.
		diff := this - prev.
		this / (diff abs) > epsilon
			ifTrue: [dif2 := next - this.
				diff sign = dif2 sign ifFalse: [bre add: ((in * sc) @ this)]].
		prev := this].
	bre add: (data size @ avgs last).
	^LinearFunction from: bre! !

	| min max |
	data first isPoint
		ifTrue: [domain := data first x to: data last x]
		ifFalse: [domain := 1 to: data size].
	min := 1.0e30.  max := -1.0e30.
	data do: [ :d |
		d < min ifTrue: [min := d].
		d > max ifTrue: [max := d]].
	self range: (min to: max)! !
	"Add the argument to the receiver's point collection."

	data add: aValue.
	aValue < range first
		ifTrue: [self range: (aValue to: range last)].
	aValue > range stop
		ifTrue: [self range: (range first to: aValue)].! !
	"Answer the value at the given index (between 0 and 1 -or- 1 and data size)."

	| val |
	anIndex isSymbol
		ifTrue: [^super at: anIndex].
	anIndex isInteger
		ifTrue: [val := data at: ((anIndex max: 1) min: data size)]
			ifFalse: [val := data at: ((anIndex max: 1) min: data size) truncated].
	^(val * scale) + offset! !
	"Put the given value at the given index (between 0 and 1)."

	anIndex isSymbol
		ifTrue: [^ super at: anIndex put: aValue].
	^ data at: ((self size * anIndex max: 1)
			min: data size) truncated put: aValue! !
	"Answer the value at the given index (between 0 and 1)."

	^self at: anIndex! !
	"Answer the receiver's 'data'."

	^data! !
	"Set the receiver's instance variable 'data' to be anObject."

	data := anObject! !
	"Answer the class of the elements in the receiver's data collection."

	data ifNil: [^nil].
	data ifEmpty: [^nil].
	^data first class! !
	"Answer the receiver's 'domain'."

	^domain! !
	"Set the receiver's instance variable 'domain' to be anObject."

	domain := anObject! !
	"Answer the domain of the receiver's collection of breakpoints."

	^duration isNil
		ifTrue: [domain last - domain first]
		ifFalse: [duration]! !
	"Answer the next X value after index whose Y value is more than delta from the value at thisIndex"
	"(LinearFunction from: #((0 0.2) (0.6 0.3) (0.62 1.0) (1.0 0.3))) edit"
	"(LinearFunction from: #((0 0.2) (0.6 0.3) (0.62 1.0) (1.0 0.3))) nextXMoreThan: 0.01 from: 0.1"
	"(LinearFunction from: #((0 0.2) (0.6 0.3) (0.62 1.0) (1.0 0.3))) nextXMoreThan: 0.1 from: 0.6"


	| step |
	self duration <= 10
		ifTrue: [step := 0.005]			"default 200 steps"
		ifFalse: [step := 1].
	^self nextXMoreThan: delta from: thisIndex step: step! !
	"Answer the next X value after thisIndex whose Y value is more than delta from the value at thisIndex"
	"(LinearFunction from: #((0@0.2) (0.6@0.3) (0.62@1.0) (1.0 0.3))) nextXMoreThan: 0.05 from: 0.1"
	"(LinearFunction from: #((0@0.2) (0.6@0.3) (0.62@1.0) (1.0 0.3))) nextXMoreThan: 0.1 from: 0.6"


	| val1 val2 newIndex |
	val1 := self at: thisIndex.
	newIndex := thisIndex + step.
	val2 := self at: newIndex.
	[((val2 - val1) abs > delta) or: [newIndex >= self duration]]
		whileFalse: [newIndex := newIndex + step.
				val2 := self at: newIndex].
	^newIndex! !
	"Set the receiver's 'offset'."

	offset := offVal! !
	"Answer the given value in the receiver's breakpoint collection."

	^data at: index! !
	"Assign the given values in the receiver's breakpoint collection."

	data at: index put: value.
	value < range first
		ifTrue: [self range: (value to: range last)].
	value > range last
		ifTrue: [self range: (range first to: value)].! !
	"Answer the receiver's 'data'."

	| i |
	data ifNil: [^nil].
	data first isPoint ifTrue: [^data].
	i := 0.
	^data collect: [ :d | i := i + 1. (i@d) ].! !
	"Answer the receiver's 'range'."

	^range! !
	"Set the receiver's instance variable 'range' to be anObject."

	range := anObject! !
	"Answer the given value in the receiver's breakpoint collection (this is not overridden in ExpSeg)."

	^data at: index! !
	"Answer the value at the given index (between 0 and 1)."

	^self at: anIndex! !
	"Answer the receiver's 'scale'."

	^range extent! !
	"Set the receiver's 'scale'."

	scale := scaleVal! !
	^nil! !
	"Answer the size of the receiver's collection of breakpoints."

	^data size
"
	| si |
	si := domain last - domain first.
	^(si = 0)
		ifTrue: [data size]
		ifFalse: [si]
"! !
	"Format and print the receiver on the argument."

	| pt howMany |
	data size > 4096
		ifTrue: [howMany := 2048]
		ifFalse: [howMany := data size].
	aStream nextPutAll: self class name, ' from: '.
	aStream nextPutAll: '#('.
	data isEmpty
		ifTrue: [^aStream nextPutAll: ' <empty> )'].
	data first isPoint
		ifTrue: [1 to: howMany do: 
			[ :i |
			pt := data at: i.
			aStream nextPut: $(.
			pt x printOn: aStream.
			aStream space.
			pt y printOn: aStream.
			aStream nextPut: $); space].
			aStream nextPut: $)]
		ifFalse: [1 to: howMany do: 
			[ :i |
			pt := data at: i.
			pt printOn: aStream.
			aStream space.].
		howMany < data size
			ifTrue: [aStream nextPutAll: ' ... '].
			aStream nextPut: $)]! !
	"Format and print the receiver on the argument."

	| pt |
	aStream nextPutAll: '(', self class name, ' from: '.
	aStream nextPut: $#; nextPut: $(.
	1 to: data size do: 
			[ :i |
			pt := data at: i.
			aStream nextPut: $(.
			pt x printOn: aStream.
			aStream space.
			pt y printOn: aStream.
			aStream nextPut: $); space].
	aStream nextPutAll: '))'; cr.! !
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value! !
	"Evaluate aBlock with each of the receiver's elements as the argument."

	data do: aBlock! !
	^data isEmpty! !
	"Initialize the receiver for the given size."

	data := OrderedCollection new: size.
"	data := Array new: size withAll: 0."
	range := 0.0 to: 0.0.
	domain := 1 to: size.
	scale := 1.0.
	offset := 0.0! !
	"Answer whether or not the receiver has a function breakpoint near the given x value."

	^(self detect: [ :pt | (pt x - anXValue) abs < 0.01] ifNone: [nil]) ~= nil! !
	"Answer the receiver`s point nearest the given x value."
	"(LinSeg from: #((0@0) (0.3@1) (0.6@0.6) (1@0))) indexOfPointNearestX: 0.7"

	| ind |
	ind := 1.
	2 to: data size do:
		[ :i |
		((data at: ind) x - anXValue) abs < 
			((data at: i) x - anXValue) abs
		ifFalse: [ind := i]
		ifTrue: [^ind]].
	^data size! !
	"Answer a function graph with the argument multiplied by the receiver"

	^FunctionGraph opL: self opR: aFcnOrNum operation: #*! !
 aFcnOrNum
	"Answer a function graph with the argument added to the receiver"

	^FunctionGraph opL: self opR: aFcnOrNum operation: #+! !
	"Answer a function graph with the argument subtracted from the receiver"

	^FunctionGraph opL: self opR: aFcnOrNum operation: #-! !
	"Answer a function graph with the receiver divided by the argument"

	^FunctionGraph opL: self opR: aFcnOrNum operation: #/! !
	"Answer a exponential attack/decay/sustain/release envelope."
	"Function exponentialADSR1"
	"FunctionView onFunction: Function exponentialADSR1"

	^ExponentialFunction from: #((0 0 5) (0.2 1 -3) (0.8 0.5 -2) (1 0))! !
	"Answer a exponential attack/decay/sustain/release envelope."
	"Function exponentialADSR2"
	"FunctionView openOn: Function exponentialADSR2"

	^ExponentialFunction from: #((0 0 5) (0.05 1 -3) (0.8 0.5 -2) (1 0))! !
	"Answer a linear attact/decat/sustain/release envelope."
	"Function linearADSR1"
	"FunctionView openOn: Function linearADSR1"

	^LinearFunction from: #((0 0) (0.1 1) (0.2 0.6) (0.9 0.4) (1 0))! !
	"Answer a linear attact/decat/sustain/release envelope."
	"Function linearADSR2"
	"FunctionView openOn: Function linearADSR2"

	^LinearFunction from: #((0 0) (0.05 1) (0.1 0.5) (0.9 0.4) (1 0))! !
	"Answer a generic spline curve."
	"Function spline"
	"FunctionView openOn: Function spline"

	^SplineFunction from: #((0 0.5) (0.35 0.6) (0.65 0.2) (1 0.6))! !
	"Answer a simple Fourier summation."
	"Function sumOfSines"
	"FunctionView openOn: Function sumOfSines"

	^FourierSummation from: #((1 0.5 0) (3 0.3 0) (5 0.2 0) (7 0.17 0) (9 0.15 0) (11 0.1 0))! !
	"Answer a default instance of the receiver class."

	^Function fileExample! !
	"Answer a function with the given array of collections, points, or data values."
	"(Function from: #( 0 1 0 1 0 1 0 1 0 1 0)) at: 0.4"
	"(LinearFunction from: #((0 0) (0.5 1) (1 0))) at: 0.2"

	| me |
	me := self ofSize: anArray size.
	(anArray first hasItems)
		ifTrue: [anArray do: 
			[ :anCollection | me add: (anCollection first @ anCollection last)]]
		ifFalse: [anArray do: 
				[ :aValue | me add: aValue]].
	me updateRange.
	^me! !
	"Load 1 or more functions from a text file."
	"(Function fromFile: 'G1.2a1.env') first edit"

	| strm envs fullName coll |
	fullName := SirenUtility findFile: fName.
	fullName ifNil: [self error: 'Cannot find function file ', fName].
	strm := fullName asFilename readStream.
	envs := strm fileIn.
	strm close.
	envs ifNil: [self error: 'Cannot load function file ', fName].
	coll := OrderedCollection new.
	envs do: [ :def |
		coll add:  ((Smalltalk at: (def at: 1) asSymbol) 
					perform: (def at: 2) asSymbol 
					with: (def at: 3))].
	^coll size = 1
		ifTrue: [coll first]
		ifFalse: [coll]! !
	"Answer an instance of the receiver class."

	^(super new) initialize! !
	"Answer an instance of the receiver class of the requested size."
	"Function ofSize: 8"

	^(super new) initialize: size! !
	"Answer a function with the given number of data points in the given range."
	"FunctionView onFunction: (Function randomOfSize: 256 from: 0.1 to: 0.9)"

	| rand me |
	me := self ofSize: size.
	rand := Random new.
	1 to: size do: [ :i | me add: (rand nextFrom: low to: high)].
	me domain: (1 to: size).
	^me! !
	"Answer a function with the given number of data points in the given range."
	"FunctionView onFunction: (Function randomWalkSize: 128 from: 0.3 to: 0.7)"

	| rand me val step v t |
	me := self ofSize: size.
	rand := Random new.
	val :=rand nextFrom: low to: high.
	step := high - low / 4.
	1 to: size do: [ :i | 
		v := rand nextFrom: step negated to: step.
		t := (val + v).
		((t  > high or: [t < low])) ifTrue: [v := v negated].
		val := val + v.
		me add: val].
	^me! !
	"Answer a function with the given points."
	"Function readFloatsFrom: 'snd/yy/ec/syl/y2.env'"

	| file stream size me |
	file := filename asFilename.
	file exists ifFalse: [^self error: 'non-extant file ', filename].
	size := file fileSize / 4.		"assume floating-point values"
	me := self ofSize: size.
	stream := file readStream binary.
	1 to: size do: [ :i | me add: stream nextFloat].
	^me! !
	"Function usage example; read a function from a binary file and view it."
	"Function averagedFunctionFileExample"

	FunctionView onFunction:
			 ((Function readFloatsFrom: 
				(SirenUtility findFile: 'y2.2.env')) 
		averagedTo: 32)! !
	"Function usage example; read a function from a binary file and view it."
	"Function fileExample inspect"

	^Function readFloatsFrom: (SirenUtility findFile: 'G1.2a1.env')! !
	"Function usage example; read a function from a binary file and view it."
	"Function functionFileExample"

	FunctionView onFunction: Function default! !
	"Function usage example; make a roll-type eventList and apply a crescendo/decrescendo to it"
	"Function functionPlayExample"

	| temp fcn |
	temp := EventList newNamed: #test3.
	(0 to: 4000 by: 50) do: 	"4 seconds, 20 notes per second"
			[ :index |			"add the same note"
			temp add: (MusicEvent dur: 100 pitch: 36 ampl: 100) 
					at: index].
	fcn := LinearFunction from: #((0 @ 0) (0.5 @ 1) (1 @ 0)).
	temp applyFunction: fcn to: #loudness.
	temp play! !
	"Function usage example; make a z-z function and view it."
	"Function functionViewExample"

	FunctionView onFunction: (Function from: #( 0 1 0 0.5 1.0 0.5 0 1 0 0.3 0.6 0.9 1 0 1 0))! !
	"Function usage example; read a function from a binary file and view it."
	"Function maxedFunctionFileExample"

	FunctionView onFunction: ((Function readFloatsFrom: (SirenUtility findFile: 'y2.2.env')) maxTo: 128 win: 32)! !
	"Function usage example; make a random walk fcn and view it."
	"Function randomViewExample"

	FunctionView onFunction: (Function randomOfSize: 64 from: 0.1 to: 0.9)! !
	"Function usage example; make a random walk fcn and view it."
	"Function randomViewExample2"

	FunctionView onFunction: (Function randomWalkSize: 128 from: 0.3 to: 0.7)! !
	"Answer the default size for the instances' storage array."

	^1024! !

Instance variables:
	myForm		<Form> my function plot--default size = 1024@180
	myArray		<Array of Floats> my value array--default length = 1024
	lazy <Boolean> do I cache my values (eager) or cmopute them on the fly (lazy)?

Example:
	| fcn |
	fcn := FourierSummation from: #((1 1 0) (3 0.3 0) (5 0.2 0) 
							(7 0.15 0) (9 0.11 0) (11 0.09 0)).
	Transcript show: (fcn at: 0.14) printString; cr.
!
	"Initialize the receiver for the given size."

	super initialize: size.
	domain := 0 to: 1.
	range := -1 to: 1.
	lazy := false		"i.e., pre-compute the curve"! !
	"Compute the block by sine summation--set the values in the cached array."

	| width twoPi scale |
	width := self class defaultSize.
	myArray := Array new: width.
	twoPi := 2.0 * Float pi.
	scale := twoPi / width.
	1 to: width do:
		[ :x |
		myArray at: x put: (self computeValueAt: (x * scale))].! !
	"Compute the answer by sine summation."
	"(Fourier from: #((1 1 0))) computeValueAt: (Float pi / 4)"

	| value twoPi |
	twoPi := 2.0 * Float pi.
	value := 0.
	data do:
		[ :aZPoint |		"each point is (number @ amplitude @ phase)"
		value := value + (((anIndex * aZPoint x) asFloat
						+ (aZPoint z asFloat / 360.0 * twoPi)) sin
					* aZPoint y asFloat)].
	^value! !
	"Add the argument to the receiver's point collection."

"	| y |"
	data add: a3DPoint.
"	y := a3DPoint y.
	y < domain first
		ifTrue: [self domain: (y to: domain last)].
	y > domain last
		ifTrue: [self domain: (domain first to: y)]."! !
	"Answer the value from my array--assume an index in the range 0 to 1"

	| val |
	lazy
		ifTrue: [val := self computeValueAt: theIndex]
		ifFalse: [myArray ifNil: [self computeCurve].
				val := myArray at: ((theIndex * myArray size) truncated max: 1)].
	^(val * scale) + offset! !
	"Answer a default instance of the receiver class."

	^FourierSummation from: #((1 0.5 0) (3 0.3 0) (5 0.2 0) 
							(7 0.17 0) (9 0.15 0) (11 0.1 0))! !
	"Answer a sum-of-sines function with the given points."

	| me |
	me := self ofSize: anArrayOfZPoints size.
	anArrayOfZPoints do: 
			[ :anArray | 
			me add: (anArray first @ (anArray at: 2) @ anArray last)].
	^me! !
	"Make a Sine summation that approaches a square wave"
	"FourierSummation fourierExample"

	| fcn |
	fcn := FourierSummation from: #((1 1 0) (3 0.3 0) (5 0.2 0) 
							(7 0.15 0) (9 0.11 0) (11 0.09 0)).
	Transcript show: (fcn at: 0.14) printString; cr.
	fcn inspect! !
	"Make a Sine summation that approaches a square wave and open a view on it."
	"FourierSummation fourierViewExample"

	FunctionView onFunction: (FourierSummation from: #((1 0.5 0) (3 0.3 0) (5 0.2 0) 
							(7 0.17 0) (9 0.15 0) (11 0.1 0)))! !

	^delta! !

	delta := aValue! !
	^function! !
	function := aFcn.
	self duration: aFcn duration.! !

	^interval! !

	interval := aValue! !
	"Answer the receiver's data value"

	| realIndex |
	startedAt ifNil:
		[index := 0.
		startedAt := Time microsecondClock].
	realTime
		ifTrue: [realIndex := (Time microsecondClock - startedAt) / 1000000]
		ifFalse: [realIndex := index].
	^function atX: realIndex! !
	"Play the receiver on the voice then."

	aVoice isVoice
		ifTrue: [aVoice playEvent: self at: aTime].
	(SirenSession voices includesKey: aVoice)
			ifTrue: [^(SirenSession voices at: aVoice) playEvent: self at: aTime].
	aVoice isInteger
			ifTrue: [Voice default playEvent: self at: aTime]! !
	"Answer the next time to reschedule me"

	interval ifNotNil:
		[^now + interval asUsec value].
	delta ifNotNil:
		[^now + (function nextXMoreThan: delta from: index)]! !
	"Play the event list by passing it off to the event scheduler."

	EventScheduler instance addClient: self in: 1 msec.
	EventScheduler instance run! !
	"Expand an eventList for the appointment scheduler."

	| aVoice next |
	aVoice := self voice.
	startedAt ifNil:
		[index := 0.
		startedAt := Time microsecondClock].
	aVoice isVoice
		ifTrue: [aVoice playEvent: self at: aTime]
		ifFalse: [(SirenSession voices includesKey: aVoice)
			ifTrue: [(SirenSession voices at: aVoice) playEvent: self at: aTime]
			ifFalse: [aVoice isInteger
				ifTrue: [Voice default playEvent: self at: aTime]]].
	next := self nextTime: aTime.
	realTime
		ifTrue: [((Time microsecondClock - startedAt) / 1000000) > function duration
			ifTrue: [index := 0. ^nil]]
		ifFalse: [index := index + 1.
			index > function size
				ifTrue: [index := 0. ^nil]].
	^next			   "answer the time of the next appointment"! !
	"Create and answer a new instance of me"

	^super new initialize! !
	"Answer the value at the given index (between 0 and 1 -or- 1 and data size)."

	| op1 op2 |
	anIndex isSymbol
		ifTrue: [^operandL at: anIndex].
	operandL isEvent
		ifFalse: [op1 := operandL]
		ifTrue: [op1 := operandL at: anIndex].
	operandR isEvent
		ifFalse: [op2 := operandR]
		ifTrue: [op2 := operandR at: anIndex].
	^op1 perform: operation with: op2! !

	operandL := aFcn.
	range := aFcn range.
	domain := aFcn domain.
! !

	operandR := aValue! !

	^operation! !

	operation := aValue! !
	"Open the sum of a triangle and a sine"
	"FunctionGraph example"

	| fcn1 fcn2 |
	fcn1 := LinearFunction from: #((0 0) (0.5 0.9) (1 0)).
	fcn2 := FourierSummation from: #((17 0.05 0)).
"	(FunctionGraph opL: fcn1 opR: fcn2 operation: #+) open"
	(fcn1 + fcn2) open.! !
	"Answer a new function graph"

	^self new operandL: opL; operandR: opR; operation: op! !

Example:
	LinearFunction from: #((0 0) (0.1 1) (0.2 0.6) (0.9 0.4) (1 0))
!
	"Normalize the receiver to the range and domain of 0-1, inclusive."

	| xs ys |
	self updateRange.
	xs := domain stop "- domain first".
	ys := range stop "- range first".
	((xs abs > 1) or: [ys abs > 1]) ifTrue: 
		[data := data collect: [ :pt |
			pt setX: (pt x / xs) setY: (pt y / ys)].
		range := 0 to: 1.
		domain := 0 to: 1]! !
	"Scale the receiver's points by the given scale."
	"LinearFunction default scaleBy: 5@1"

	| newD |
	newD := Array new: data size.
	1 to: data size do:
		[ :i | newD at: i put: (data at: i) * scalePt].
	data := newD.
	self updateRange! !

	| minx maxx miny maxy |
	domain := 1 to: data size.
	minx := 1.0e30.  maxx := -1.0e30.
	miny := 1.0e30.  maxy := -1.0e30.
	data do: [ :d |
		d x < minx ifTrue: [minx := d x].
		d x > maxx ifTrue: [maxx := d x].
		d y < miny ifTrue: [miny := d y].
		d y > maxy ifTrue: [maxy := d y]].
	self range: (miny to: maxy).
	self domain: (minx to: maxx)! !
	"Add the argument to the receiver's point collection."

	data add: anItem.
	domain ifNil: [self domain: (anItem x to: anItem x)].
	range ifNil: [self range: (anItem y to: anItem y)].
	anItem x < domain first
		ifTrue: [self domain: (anItem x to: domain last)].
	anItem x > domain stop
		ifTrue: [self domain: (domain first to: anItem x)].
	anItem y < range first
		ifTrue: [self range: (anItem y to: range last)].
	anItem y > range stop
		ifTrue: [self range: (range first to: anItem y)]! !
	"Answer the value at the given index in my range--do linear interpolation."

	| val xVal count x1 x2 y1 y2|
	xVal := data first x.
	count := 1.
	[xVal < anIndex] whileTrue:
		[count := count + 1.
		count > data size
			ifTrue: [val := data last y].
		xVal := (data at: count) x].
	xVal = anIndex 
		ifTrue: [val := (data at: count) y]
		ifFalse: [x1 := (data at: (count - 1)) x.
			x2 := (data at: count) x.
			y1 := (data at: (count - 1)) y.
			y2 := (data at: count) y.
			val := y1 + ((anIndex - x1) / (x2 - x1) * (y2 - y1))].
	^(val * scale) + offset! !
	"Answer the size of the receiver's collection of breakpoints."

	| si |
	si := domain stop - domain first.
	^(si = 0)
		ifTrue: [data size]
		ifFalse: [si]! !
	"Answer an ADSR envelope."
	"FunctionView onFunction: (LinearFunction a: 0.03 d: 0.03 s: 0.4 r: 0.3)"

	| susT arr |
	susT := 1.0 - (att + dec).
	arr := Array new: 5.
	arr at: 1 put: (0 @ 0).
	arr at: 2 put: (att @  1).
	arr at: 3 put: ((att + dec) @ sus).
	arr at: 4 put: (susT @ sus).
	arr at: 5 put: (1 @ 0).
	^self from: arr! !
	"Answer a default instance of the receiver class - an ADSR shape."

	^LinearFunction from: #((0 0) (0.15 1.0) (0.25 0.75) (0.75 0.5) (1 0))! !
	"Create a linear envelope from the given Loris linear envelope."

	| inst step |
	inst := self ofSize: siz.
	step := dur / siz.
	0 to: siz do: [ :ind |
		inst add: ((step * ind) asFloat @ (data valueAt: (step * ind)) asFloat)].
	inst updateRange.
	^inst! !
	"Answer a LinSeg of a typical envelope function."
	"LinearFunction exampleEnvelope"
	"FunctionView onFunction: (LinearFunction exampleEnvelope)"

	| rand pts size x y |
	rand := Random new.
	size := rand nextIntFrom: 5 to: 15.
	pts := Array new: size.
	pts at: 1 put: 0@0.
	pts at: size put: 1@0.
	x := 0.
	y := 0.8.
	2 to: size - 1 do:
		[ :i |
		x := x + (rand nextFrom: 0.05 to: 0.15).
		y := ((y + (rand nextFrom: -0.3 to: 0.3)) min: 1.0) max: 0.0.
		pts at: i put: (x@y)].
	^LinearFunction from: pts! !
	"Make a LinSeg and answer a value."
	"LinearFunction linearExample"

	| fcn |
	fcn := LinearFunction ofSize: 2.
	fcn add: (0@0.5); add: (1@1).
	Transcript show: (fcn at: 0.14) printString; cr.
	fcn inspect! !
	"Make an ADSR-shaped LinSeg and open a view on its form."
	"LinearFunction linearViewExample"

	FunctionView onFunction: 
		(LinearFunction from: #((0 0) (0.1 1) (0.2 0.6) (0.9 0.4) (1 0)))! !
They use ZPoints for x/y/exponent. The exponent determines the speed of the exponential/logarithmic transition between breakpoint values. A value of 0 leads to linear interpolation.

Example:
	ExponentialFunction from: #((0 0 -5) (0.2 1 -3) (0.8 0.5 -2) (1 0))
!
	"Answer the value at the given index in my range--do exponential interpolation such
 	that if f(x, i) is the i-th function value in the transition from breakpoint v[J] to 
	v[J+1], then:
		f(x) = v[J] + (v[J+1] - v[J])*(1 - exp(i*x[J]/(N-1)))/(1 - exp(x[J]))
	for 0 <= i < N, where N is the number of function points between t[J] and the next 
	horizontal value, and x is the exponential weight whereby x = 0 will yield a 
	straight line, x < 0 will yield an exponential transition, and x > 0 will yield a 
	logarithmic transition."

	| weight interp xVal count x1 x2 y1 y2 val |
	xVal := data first x.
	count := 1.
	[xVal < anIndex] whileTrue:
		[count := count + 1.
		count > data size
			ifTrue: [val := data last x].
		xVal := (data at: count) x].
	xVal = anIndex 
		ifTrue: [val := (data at: count) y]
		ifFalse: [x1 := (data at: (count - 1)) x.
			x2 := (data at: count) x.
			y1 := (data at: (count - 1)) y.
			y2 := (data at: count) y.
			weight := (data at: count - 1) z.
			interp := (anIndex - x1) / (x2 - x1).
			weight == 0		"special case for linear interpolation"
				ifTrue: [val := y1 + (interp * (y2 - y1))].
			val := y1 + ((y2 - y1) * ((1 - (interp * weight) exp) / (1 - weight exp)))].
	^(val * scale) + offset! !
	"Answer the given value in the receiver's breakpoint collection as a 2-D point."

	| pt |
	pt := data at: index.
	^pt x @ pt y! !
	"Format and print the receiver on the argument."

	| pt |
	data size > 16
		ifTrue: [^super printOn: aStream].
	aStream nextPutAll: self class name, ' from: '.
	aStream nextPut: $#; nextPut: $(.
	1 to: data size do: 
			[ :i |
			pt := data at: i.
			aStream nextPut: $(.
			pt x printOn: aStream.
			aStream space.
			pt y printOn: aStream.
			aStream space.
			(pt respondsTo: #z)
				ifTrue: [pt z printOn: aStream].
			aStream nextPut: $); space].
			aStream nextPut: $)! !
	"Answer an ADSR envelope."
	"(ExponentialFunction a: 0.06 d: 0.06 s: 0.4 r: 0.4) edit"

	| susT arr |
	susT := 1.0 - (att + dec).
	arr := Array new: 5.
	arr at: 1 put: (0 @ 0 @ 3).
	arr at: 2 put: (att @  1 @ -3).
	arr at: 3 put: ((att + dec) @ sus @ -2).
	arr at: 4 put: (susT @ sus @ -2).
	arr at: 5 put: (1 @ 0).
	^self from: arr! !
	"Answer a default instance of the receiver class."
	"ExponentialFunction default edit"

	^self from: #((0 0 3 ) (0.08 1.0 -3) (0.2 0.5 -1) (1 0 0))! !
	"Answer a function with the given points."
	"ExponentialFunction from: #((0 0 5) (0.5 1 5) (1 0))"

	| me |
	me := self ofSize: anArrayOfPoints size.
	anArrayOfPoints first isPoint
		ifTrue: [anArrayOfPoints do: 
			[ :aZPoint |  me add: aZPoint]]
		ifFalse: [anArrayOfPoints do: 
			[ :anArray |  me add: (anArray first @ (anArray at: 2) @ anArray last)]].
	me updateRange.
	^me! !
	"Make an exp seg and open a view on its form."
	"ExponentialFunction expsegViewExample"

	FunctionView onFunction: 
		(ExponentialFunction from: #((0 0 -5) (0.2 1 -3) (0.8 0.5 -2) (1 0)))! !
	"Make an exp seg and open a view on its form."
	"ExponentialFunction expASRViewExample"

	FunctionView onFunction: 
		(ExponentialFunction from: #((0 0 -3) (0.3 0.9 0) (0.7 0.9 -3) (1 0)))! !
	"Make an exp seg and open a view on its form."
	"ExponentialFunction expsegViewExample"

	FunctionView onFunction: 
		(ExponentialFunction from: #((0 0 -3) (0.2 1 -5) (0.6 0.5 -1) (1 0)))! !

Instance Variables:
	sound <StoredSound> the receiver's sound
	window <Symbol>the window type id
	windowSize <Integer> the window size
	stepSize <Integer> the step between windows
	fft <FFT> the receiver's FFT analyzer
	magReal <Symbol> The spectrum type: mag, real, polar, or complex!
	"Answer the receiver's decimation factor."

	^windowSize / stepSize! !
	"Set the receiver's decimation factor."

	^stepSize := windowSize / factor! !
	"Answer the receiver's 'windowSize'."

	^windowSize! !
	"Set the receiver's instance variable 'windowSize' to be aNum."

	windowSize := aNum.
! !
	"Answer the receiver's 'sound'."

	^sound! !
	"Set the receiver's instance variable 'sound' to be anObject."

	sound := anObject! !
	"Answer the receiver's 'stepSize'."

	^stepSize! !
	"Set the receiver's instance variable 'stepSize' to be anObject."

	stepSize := anObject! !
	"Answer the receiver's 'windowSize'."

	^windowSize! !
	"Set the receiver's instance variable 'windowSize' to be aNum."

	windowSize := aNum.
! !
	"Do the FFT and store the results into the receiver's data."
	"Spectrum sweepExample"
	"Spectrum fileExample"

	| fft dSize min max samples spectrum |
	dSize := (sound size / stepSize) truncated.		"# of windows"
	data := Array new: dSize.						"array of arrays"
	min := 1.0e30.  max := -1.0e30.
	samples := ByteArray new: (windowSize * 4).		"FFT's private data"
	samples gcCopyToHeap.
	spectrum := ByteArray new: (windowSize * 4 + 1).
	spectrum gcCopyToHeap.
	fft := "FFTWInterface" Object new.						"set up the FFT"
	[fft fftw_initialize: windowSize with: samples with: spectrum]
		on: Error
		do: [ :ex | self error: 'FFT not installed'].
	Cursor execute showWhile:
		[1 to: dSize do:
			[ :winCount | | buffer slice |
			buffer := self window: winCount.	"window the data buffer"
			1 to: windowSize do:							"copy it to the FFT's input buffer"
				[ :count | samples floatAt: (count - 1 * 4 + 1) put: (buffer at: count)].
			fft fftw_forward_transform.				"do the FFT"
			slice := Array new: (windowSize / 2).
			magReal == #real				"pack data--real or complex"
				ifFalse: [self error: 'Complex FFT not supported']
				ifTrue: [1 to: windowSize / 2 do:	"copy the magnitude spectrum out"
					[ :ind | | re im mag |
						re := spectrum floatAt: (ind * 2 - 2 * 4 + 1).
						im := spectrum floatAt: (ind * 2 - 1 * 4 + 1).
						mag := ((re * re) + (im * im)) sqrt.
						slice at: ind  put: mag.
						min := mag min: min.
						max := mag max: max].
					data at: winCount put: slice]]].
	self range: (min to: max)! !
	"Do the FFT and store the results into the receiver's data."
	"Display restoreAfter: [Spectrum sweepExample]"
	"Display restoreAfter: [Spectrum fileExample]"

	| fft dSize min max |
	dSize := (sound size / stepSize) truncated.
	data := Array new: dSize.		"array of arrays"
	min := 1.0e30.  max := -1.0e30.
	Cursor execute showWhile:
		[1 to: dSize do:
			[ :ind |
			fft realData: (self window: ind).
			fft transformForward: true.		"do fft"
			magReal == #real				"pack data--real or complex"
				ifTrue: [data at: ind put: fft realData]
				ifFalse: [data at: ind put: 				"store complex spectrum as mag"
					((1 to: windowSize) collect:
						[ :i | ((fft realData at: i) squared 
								+ (fft imagData at: i) squared) sqrt])].
			(data at: ind) do:			"calc min/max"
				[ :d | min := d min: min.
					max := d max: max]]].
	self range: (min to: max)! !
	"Create the receiver's data array for the right number of frames."

	data := Array new: (sound size / stepSize) truncated.
	domain := 0 to: data size! !
	"Iterate over the receier's data getting the RMS data range"

	| min max off |
	min := 1.0e30.  max := -1.0e30.
	off := windowSize / 2.
	magReal == #real ifTrue:
		[data do: 
			[ :fr | fr do: 
				[ :d | min := d min: min.
					max := d max: max]]].

	magReal == #complex ifTrue:
		[data do: 
			[ :fr | 1 to: off do: 
				[ :ind | | val |
				val :=  ((fr at: ind) squared + (fr at: ind + off) squared) sqrt.
					min := val min: min.
					max := val max: max]]].

	self range: (min to: max)! !
	"Answer the real data array for the given window of the receiver's sound."

	| dArray base sHalf  ind|
	dArray := Array new: windowSize.
	base := which - 1 * stepSize.		"which is 1-based"
	window == #rectangular
		ifTrue: [1 to: dArray size do:
				[ :i | dArray at: i put: (sound floatSampleAt: (base + i))]].
	window == #triangular
		ifTrue: [sHalf := dArray size asFloat / 2.0.
			1 to: sHalf do:
				[ :i | dArray at: i put: ((sound floatSampleAt: (base + i)) * (i asFloat / sHalf))].
			1 to: sHalf do:
				[ :i | 
				ind := i + sHalf.
				dArray at: ind put: ((sound floatSampleAt: (base + ind)) * (1 - (i asFloat / sHalf )))]].
	^dArray! !
	"Set the receiver's instance variable 'magReal' to be #complex."

	magReal := #complex! !
	"Set the receiver's instance variable 'magReal' to be #real."

	magReal := #real! !
	"Set the receiver's 'window'."
	"#rectangular, #triangular"

	window := aSymbol! !
	"Answer the receiver's 'window'."

	^window! !
	"Answer a new frame interpolated for the given (floating-point) index."

	| bas fra pre nex int |
	bas := ind truncated max: 1.
	fra := ind fractionPart.
	pre := data at: bas.
	fra < 0.05 ifTrue: [^pre].
	bas = data size ifTrue: [^pre].
	nex := data at: bas + 1.
	int := pre class new: pre size.
	1 to: pre size do:
		[ :x | | v0 v1 |
		v0 := pre at: x.
		v1 := nex at: x.
		int at: x put: (v0 + ((v1 - v0) * fra))].
	^int! !
	"Interpolate/decimate the receiver by the given (float or function) time factor."

	| siz newD |
	fact isNumber ifTrue:
		[siz := (data size * fact) truncated.
		newD := Array new: siz.
		1 to: siz do:
			[ :in |
			newD at: in put: (self interpolateFrame: (in asFloat / fact))].
		data := newD.
		^self]
				"else fact is a function"! !
	"Display the receiver"

	SpectrumView openOn: self! !
	"Format and print the receiver on the argument."

	aStream nextPutAll: self class name, ' on: '.
	sound printOn: aStream.
	aStream nextPutAll: (' window: ', windowSize printString, ' step: ', stepSize printString)! !
	"Set up the receiver."

	super initialize.
	window := " #rectangular " #triangular.
	windowSize := 256.
	stepSize := 1024.
"	fft := FFT new nu: (windowSize log: 2) truncated.
"	magReal := #real.			"mag, real, polar, or complex"! !
	"Answer the value at the given index (between 1 and self size)."

	anIndex isSymbol
		ifTrue: [^super at: anIndex].
	^data at: anIndex! !
	"Answer the value at the given index (between 0 and 1)."

	anIndex isSymbol ifTrue: [^super at: anIndex put: aValue].
	data at: anIndex put: aValue! !
	"Answer the frame at the given index (between 1 and self size)."

	^data at: anIndex! !
	"Iterate over a single frame with the given block."

	magReal == #real
		ifTrue: [(data at: anIndex) do: aBlock]

		ifFalse: [ | base |
			base := anIndex * windowSize.
			base to: base + windowSize do: aBlock]! !
	"Answer the given imag value in the given frame."

	magReal == #complex
		ifTrue: [^(data at: frm) at: frq + windowSize]! !
	"Set the given imag value in the given frame."

	magReal == #complex
		ifTrue: [(data at: frm) at: frq + windowSize put: val]! !
	"Set the value at the given index (between 0 and 1)."

	anIndex isSymbol ifTrue: [^super at: anIndex put: aValue].
	data at: anIndex put: aValue! !
	"Answer the requested real value from the given frame."

	^(data at: frm) at: frq! !
	"Set the given real value in the given frame."

	magReal == #complex
		ifTrue: [(data at: frm) at: frq put: val]! !
	"Read a sound from disk and take its fft."
	"Spectrum fileExample display"

	| me |
	me := self new initialize.
	me sound: (SampledSound fromFile: (SirenUtility findFile: 'a.snd')).
	me windowSize: 1024.
	me stepSize: 512.
	me real.
	me compute.
	^me! !
	"Create a swept sine wave and take its fft."
	"Spectrum sweepExample display"

	| me |
	me := self new initialize.
	me sound: (SampledSound sweepDur: 1.0 rate: 44100 from: 20 to: 5000 chans: 1).
	me windowSize: 1024.
	me stepSize: 1024.
	me real.
	me compute.
	^me! !

Instance Variable:
	linSeg		<LinearFunction> my linear twin!
	"Compute the receiver by cubic interpolation; use the Geometric spline."

	| points spline |
	points := OrderedCollection new: data size.
	data do: [ :pt |
			points add: (pt * 100)].
	spline := Spline controlPoints: points.
	points := spline computeCurve; computePoints.
	1 to: points size do: [ :i |
			points at: i put: ((points at: i) / 100)].
	linSeg := LinearFunction from: points! !
	"Answer the value at the given index in my range--take it out of my computed form."

	| val |
	linSeg == nil
		ifTrue: [self computeCurve].
	val := linSeg at: anIndex.
	^(val * scale) + offset! !
	"Make a SplineSeg."
	"SplineFunction splineExample"

	| fcn |
	fcn := SplineFunction from: #((0 1) (0.35 1.3) (0.65 0.7) (1 1)).
	fcn inspect.
"	^fcn at: 0.14 
"! !
	"Make a SplineSeg and open a view on its form."
	"SplineFunction splineSegViewExample"

	FunctionView onFunction: 
		(SplineFunction from: #((0 0.5) (0.35 0.7) (0.65 0.2) (1 0.5)))! !
	"Answer a default instance of the receiver class."

	^SplineFunction from: #((0 0.5) (0.35 0.7) (0.65 0.35) (1 0.5))! !