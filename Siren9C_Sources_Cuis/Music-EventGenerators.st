!classDefinition: #Peal category: #'Music-EventGenerators'!
SequenceableCollection subclass: #Peal
	instanceVariableNames: 'base directions position finished'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Peal commentStamp: '<historical>' prior: 0!
Peals are repetitive note-generating sequences.  
This implementation was written by Mark Lentczner in 1986.

Instance variables:
	base <Collection of Numbers> the pitches i use
	directions <Array of +-1> the direction of the current sub-sequence
	position <Number> the current index
	finished <Boolean> an i done?!


!Peal methodsFor: 'playing'!
eventList

	| list |
	list := EventList new.
	self playOn: list durations: 125 meter: 125 at: 0.
	^list! !

!Peal methodsFor: 'playing'!
play
	self eventList play! !

!Peal methodsFor: 'playing'!
playOn0: out durations: dur meter: mtr at: start
	"Play the receiver"

	| time siz |
	time := start.
	siz := self size.
	[finished] whileFalse:
		[1 to: siz do: 
			[ :i | out play: (self at: i) at: time dur: dur amp: 100. 
				time := time + mtr ].
		self change.
		1 to: siz - 1 do: 
			[ :i | out play: (self at: i) at: time dur: dur amp: 100. 
				time := time + mtr ].
		out play: (self at: siz ) at: time dur: dur + mtr amp: 100.  
				time := time + mtr + mtr.
		self change].
	1 to: siz do: 
			[ :i | out play: (self at: i) at: time dur: dur amp: 100. 
				time := time + mtr ].! !

!Peal methodsFor: 'playing'!
playOn: vox
	self eventList playOn: vox! !

!Peal methodsFor: 'playing'!
playOn: out durations: dur meter: mtr at: start
	"Play the receiver"

	| time siz |
	time := start.
	siz := self size.
	[finished] whileFalse:
		[1 to: siz do: 
			[ :i | out add: time => ((self at: i) note, dur msec, 100 velocity). 
				time := time + mtr].
		self change.
		1 to: siz - 1 do: 
			[ :i | out add: time => ((self at: i) note, dur msec, 100 velocity). 
				time := time + mtr].
		out add: time => ((self at: siz) note, (dur + mtr) msec, 100 velocity).  
				time := time + mtr + mtr.
		self change].
	1 to: siz do: 
			[ :i | out add: time => ((self at: i) note, dur msec, 100 velocity). 
				time := time + mtr].! !


!Peal methodsFor: 'accessing'!
at: index
	"Answer the element in the base collection currently mapped into the index position."

	^ base at: (self basicAt: index)! !

!Peal methodsFor: 'accessing'!
at: index put: object

	^ self error: 'Can''t modify a peal by at:put:'! !

!Peal methodsFor: 'accessing'!
currentChange
	"Answer the base as mapped by the current change."

	^(1 to: self size) collect: [ :i  | self at: i ]! !

!Peal methodsFor: 'accessing'!
finished
	"Answer is the peal has completed it's last change"

	^ finished! !

!Peal methodsFor: 'accessing'!
setBase: baseSet
	"Initialize the base set and everything else."

	base := baseSet.
	directions := Array new: self size withAll: 1.
	position := 1.
	1 to: self size do: [:i | self basicAt: i put: i].
	finished := false.! !

!Peal methodsFor: 'accessing'!
size
	"Redone here because SequencableCollection overrides it."

	^ self basicSize! !


!Peal methodsFor: 'private'!
changeOrder: n bounds: range
	"Produce the next change in the peal by moving the element n within the range.  If the element wants to move outside the range, alter it's direction and move the next element (recursively call this)."

	| neighbor location |
	location := self indexOfElement: n.
	neighbor := location + (directions at: n).

	(n = self size) ifTrue:
		[(n = 1) ifFalse:
			[self swap: location with: neighbor.
			directions at: n-1 put: (directions at: n-1) negated].
		finished := true.
		^ self].

	(range includes: neighbor) ifTrue:
		[self swap: location with: neighbor]
	ifFalse:
		[directions at: n put: (directions at: n ) negated.
		self changeOrder: n+1 bounds: (range copyWithout: location)]! !

!Peal methodsFor: 'private'!
indexOfElement: n
	"Answer the index the anElement item  (i.e. the n-th item, not the item = to n) within the receiver."

	1 to: self size do:
		[:i | (self basicAt: i) = n ifTrue: [^ i]].
	^ self error: 'An element does not exist in this peal'! !

!Peal methodsFor: 'private'!
swap: index1 with: index2
	"We need to redefine this, since we redefined at: & at:put: to map trhrough the base collection"

	| temp |
	temp := self basicAt: index1.
	self basicAt: index1 put: (self basicAt: index2).
	self basicAt: index2 put: temp! !


!Peal methodsFor: 'changing'!
change
	"Produce the next change in the peal"

	self changeOrder: 1 bounds: (1 to: self size)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Peal class' category: #'Music-EventGenerators'!
Peal class
	instanceVariableNames: ''!

!Peal class methodsFor: 'examples'!
pealExample1
	"Play a simple bell peal"
	"Peal pealExample1"

	(Peal upon: #(60 62 64 65)) eventList edit.! !


!Peal class methodsFor: 'instance creation'!
new: size
	"Create a new peal on the given size with 1..size as the base set."

	| newPeal |
	newPeal := self basicNew: size.
	newPeal setBase: (1 to: size).
	^ newPeal! !

!Peal class methodsFor: 'instance creation'!
upon: baseSet
	"Create a new peal on the base set given"

	| newPeal |
	newPeal := self basicNew: baseSet size.
	newPeal setBase: baseSet.
	^ newPeal! !


!classDefinition: #EventGenerator category: #'Music-EventGenerators'!
EventList subclass: #EventGenerator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!EventGenerator commentStamp: '<historical>' prior: 0!
This class is the parent of most of the simple music-structure classes in this category.
An EventGenerator can create an EventList using its given parameters and a function (block) to use to create Event sequences.  See the subclasses for examples.
!


!EventGenerator methodsFor: 'playing'!
edit
	"edit me"

	self eventList edit! !

!EventGenerator methodsFor: 'playing'!
eventList
	"Answer an eventList for the receiver."

	self subclassResponsibility! !

!EventGenerator methodsFor: 'playing'!
play
	"Play the receiver's event list."

	self eventList play! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'EventGenerator class' category: #'Music-EventGenerators'!
EventGenerator class
	instanceVariableNames: ''!

!EventGenerator class methodsFor: 'accessing instances'!
all
	"Find the and return instances of me or any subclasses of me."

	| coll |
	coll := Set new.
	self allSubInstancesDo: [ :anI | coll add: anI ].
	^coll! !

!EventGenerator class methodsFor: 'accessing instances'!
allPlaying
	"Find all playing instances of me or my subclasses."

	| coll |
	coll := Set new.
	self allSubInstancesDo: [ :anI | anI playing ifTrue: [coll add: anI]].
	^coll! !

!EventGenerator class methodsFor: 'accessing instances'!
named: aName 
	"Find the named instance of me or a subclass of me."

	^SirenSession eventLists at: aName
		ifAbsent: [SirenSession events at: aName ifAbsent: [nil]]! !

!EventGenerator class methodsFor: 'accessing instances'!
stopAll
	"Find all playing instances of me or my subclasses and terminate them."

	self allSubInstancesDo: [ :anI | anI playing ifTrue: [anI stop]].! !


!EventGenerator class methodsFor: 'examples'!
randomExample
	"Play random examples from the set of examples in my subclasses"
	"[Sensor anyButtonPressed] whileFalse: [EventGenerator randomExample]"

	((Smalltalk at: 
			(#(Cloud DynamicCloud SelectionCloud DynamicSelectionCloud "Peal") 
					atRandom)) 
		randomExample) play! !


!classDefinition: #Cloud category: #'Music-EventGenerators'!
EventGenerator subclass: #Cloud
	instanceVariableNames: 'density'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Cloud commentStamp: '<historical>' prior: 0!
Clouds are eventLists that are describe in terms of their contours or selection criteria.
This can be (e.g.,) POD-style specification of ranges for random selection, or selection from a given set of pitches.
See the class examples of my subclasses for description examples.

Instance variables:
	density		<Number> number of notes per second!


!Cloud methodsFor: 'accessing'!
density: theDensity
	"set the density instance variable"

	density := theDensity! !


!Cloud methodsFor: 'playing'!
eventList
	"Make the cloud's events; assume that the receiver's pitch, loudness and 
	durations are actually intervals"

	| pattern number length start |
	pattern := EventList new.
	number := duration asSeconds value asFloat * density.
	length := (1000 / density) rounded.
	start := 0.
	number truncated timesRepeat:
		[pattern add: (MusicEvent 
				dur: (length * 2) 	"legato"
				pitch: self pitch value atRandom
				ampl: self loudness value atRandom
				voice: ((self voice == nil 
						ifTrue: [nil] 
						ifFalse: [self voice atRandom])))
			at: start.
		start := start + length].
	^pattern! !

!Cloud methodsFor: 'playing'!
eventList0
	"Make the cloud's events; assume that the receiver's pitch, loudness and 
	durations are actually intervals"

	| pattern rand number length start |
	pattern := EventList new.
	rand := Random new.
	number := duration asSeconds value asFloat * density.
	length := (1000 / density) rounded.
	start := 0.
	number truncated timesRepeat:
		[pattern add: (MusicEvent 
			dur: (length * 2 "+ (2000 * rand next) rounded")
			pitch: ((self pitch value first) + 
					((rand next * (self pitch value last - self pitch value first)) rounded))
			ampl: ((self loudness value first) + 
					((rand next * (self loudness value last - self loudness value first)) rounded))
			voice: ((self voice == nil 
						ifTrue: [nil] 
						ifFalse: [(self voice first) + 
					((rand next * (self voice last - self voice first)) rounded)])))
			at: start.
		start := start + length].
	^pattern! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Cloud class' category: #'Music-EventGenerators'!
Cloud class
	instanceVariableNames: ''!

!Cloud class methodsFor: 'examples'!
dictionaryExample
	"Answer an initialized Cloud generated from the given property map dictionary."
	"Cloud dictionaryExample"

	(Cloud fromDictionary: ((#duration -> 5000), 
			(#pitch -> (48 to: 70)),
			(#loudness -> (50 to: 100)), 
			(#voice -> (1 to: 4)), 
			(#density -> 15))) eventList edit! !

!Cloud class methodsFor: 'examples'!
example1
	"Create and edit a low 6 second stochastic cloud with 5 events per second"
	"Cloud example1"

	| c |
	c := (Cloud  dur: 6
		pitch: (48 to: 60)  
		ampl: (80 to: 120) 
		voice: (1 to: 4)
		density: 5) eventList.
"	c edit "
	c play
"	c inspect"! !

!Cloud class methodsFor: 'examples'!
randomExample
	"Play a stochastic cloud with random properties."
	"Cloud randomExample"

	| rand pBase score |
	rand := Random new.
	pBase := 20 + (rand next * 60) rounded.
	score := (Cloud  dur: (2 + (rand next * 6) asInteger)
		pitch: (pBase to: (pBase + (rand next * 40)) rounded)  
		ampl: (80 to: 120) 
		voice: (1 to: 8)
		density: (rand next * 20)) eventList.
	score play.
	(Delay forSeconds: 5) wait.
	MIDIPort allNotesOff
"	score inspect
	score edit
"! !

!Cloud class methodsFor: 'examples'!
randomExample2
	"Play a stochastic cloud with random properties."
	"Cloud randomExample2"

	| rand pBase score |
	rand := Random new.
	pBase := 20 + (rand next * 60) rounded.
	score := (Cloud  dur: (3 + (rand next * 6))
		pitch: (pBase to: (pBase + (rand next * 40)) rounded)  
		ampl: (80 to: 120) 
		voice: (1 to: 8)
		density: (rand next * 20)) eventList.
	score play.
	MIDIPort allNotesOff
"	score inspect
	score edit
"! !


!Cloud class methodsFor: 'instance creation'!
dur: aD pitch: aP ampl: anA
	"return an initialized Cloud"

	^self new duration: aD; 
		pitch: aP; 
		loudness: anA! !

!Cloud class methodsFor: 'instance creation'!
dur: aD pitch: aP ampl: anA voice: aVoice
	"return an initialized Cloud"

	^self new duration: aD; 
		pitch: aP; 
		loudness: anA; 
		voice: aVoice! !

!Cloud class methodsFor: 'instance creation'!
dur: aD pitch: aP ampl: anA voice: aVoice density: aDe
	"return an initialized Cloud"

	^self new duration: aD; 
		pitch: aP; 
		loudness: anA; 
		voice: aVoice; 
		density: aDe! !

!Cloud class methodsFor: 'instance creation'!
dur: aD voice: aVoice density: aDe
	"return an initialized Cloud"

	^self new duration: aD; 
		voice: aVoice; 
		density: aDe! !

!Cloud class methodsFor: 'instance creation'!
fromDictionary: aDictionary
	"Answer an initialized Cloud generated from the given property map dictionary."

	"(Cloud fromDictionary: ((#duration -> 5000), 
			(#pitch -> (48 to: 74)),
			(#loudness -> (50 to: 100)), 
			(#voice -> (1 to: 4)), 
			(#density -> 25))) eventList edit"


	| me |
	me := self new.
	aDictionary associationsDo: 
		[ :ass |
		me perform: ((ass key, ':') asSymbol) with: ass value].
	^me! !


!classDefinition: #DynamicCloud category: #'Music-EventGenerators'!
Cloud subclass: #DynamicCloud
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!DynamicCloud commentStamp: '<historical>' prior: 0!
Dynamic clouds are clouds that have starting and ending conditions (ranges or selection sets) and interpolate between them over their duration.!


!DynamicCloud methodsFor: 'playing'!
eventList
	"make the cloud's events--assume that the receiver's pitch, loudness and durations are actually ARRAYS of two intervals (for the start and end ranges)"

	| pattern rand number length start percent pRange p1 p2 p3 p4 
		amplRange a1 a2 a3 a4 v1 v2 v3 v4 vRange |
	pattern := EventList new.
	rand := Random new.
	number := duration asSeconds value asFloat * density.
	length := (1000 / density) rounded.
	start := 0.
	(self pitch value isMemberOf: Array)
		ifTrue: [pRange := self pitch value first.
			p1 := self pitch value first first.
			p2 := self pitch value first last.
			p3 := self pitch value last first - p1.
			p4 := self pitch value last last - p2]
		ifFalse: [pRange := self pitch value].
	(self loudness value isMemberOf: Array)
		ifTrue: [amplRange := self loudness value first.
			a1 := self loudness value first first.
			a2 := self loudness value first last.
			a3 := self loudness value last first - a1.
			a4 := self loudness value last last - a2]
		ifFalse: [amplRange := self loudness value].
	(self voice isMemberOf: Array)
		ifTrue: [vRange := self voice first.
			v1 := self voice first first.
			v2 := self voice first last.
			v3 := self voice last first - v1.
			v4 := self voice last last - v2]
		ifFalse: [vRange := self voice].
	1 to: number truncated do:
		[ :count |
		pattern add: (MusicEvent 
			dur: (length * 2 + (length * rand next) rounded)
			pitch: ((pRange first rounded) + 
					((rand next * (pRange last - pRange first)) rounded))
			ampl: ((amplRange first rounded) + 
					((rand next * (amplRange last - amplRange first)) rounded))
			voice: (self voice == nil 
						ifTrue: [nil] 
						ifFalse: [(vRange first rounded) + 
					((rand next * (vRange last - vRange first)) rounded)]))
			at: start.
		start := start + length.
	percent := count / number.
	(self pitch value isMemberOf: Array)
		ifTrue: [pRange := p1 + (p3 * percent) to: p2 + (p4 * percent)].
	(self loudness value isMemberOf: Array)
		ifTrue: [amplRange := a1 + (a3 * percent) to: a2 + (a4 * percent)].
	(self voice isMemberOf: Array)
		ifTrue: [vRange := v1 + (v3 * percent) to: v2 + (v4 * percent)]].
	^pattern! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DynamicCloud class' category: #'Music-EventGenerators'!
DynamicCloud class
	instanceVariableNames: ''!

!DynamicCloud class methodsFor: 'examples'!
crescendoExample
	"Create and play an 8-second cloud that goes from soft to loud on the same pitch."
	"DynamicCloud crescendoExample"

	(DynamicCloud  dur: 6  
		pitch: (40 to: 40)
		ampl: #((20 to: 30) (80 to: 100))
		voice: (1 to: 1)
		density: 20) eventList 
	"edit" 
	play.  MIDIPort allNotesOffIn: 8
	"inspect"! !

!DynamicCloud class methodsFor: 'examples'!
example1
	"Edit a 6-second cloud that goes from low to high and soft to loud."
	"DynamicCloud example1"

	(DynamicCloud  dur: 6  
		pitch: #((30 to: 44) (50 to: 50))
		ampl: #((20 to: 40) (90 to: 120))
		voice: (1 to: 4)
		density: 20) eventList "edit" play "inspect"! !

!DynamicCloud class methodsFor: 'examples'!
example4
	"Edit a 6 second cloud that focuses on its center"
	"DynamicCloud example4"

	(DynamicCloud  dur: 6  
		pitch: #((30 to: 64) (40 to: 40))
		ampl: #((20 to: 40) (90 to: 120))
		voice: (1 to: 8)
		density: 10) eventList edit! !

!DynamicCloud class methodsFor: 'examples' stamp: 'stp 12/2/2022 12:12:21'!
focusExample
	"Create a 6-second cloud that focuses toa single high note."
	"DynamicCloud focusExample"

	(DynamicCloud  dur: 8  
		pitch: #((60 to: 84) (92 to: 92))
		ampl: #((20 to: 20) (90 to: 100))
		voice: (1 to: 4)
		density: 16) eventList edit "play" "inspect"! !

!DynamicCloud class methodsFor: 'examples'!
randomExample
	"Create a dynamic second cloud with random properties."
	"DynamicCloud randomExample edit"

	| rand |
	rand := Random new.
	^DynamicCloud  dur: (rand nextIntFrom: 3 to: 8)  
		pitch: (Array with: ((rand nextIntFrom: 24 to: 48) 
					to: (rand nextIntFrom: 24 to: 54))
				with: ((rand nextIntFrom: 48 to: 60)
					to: (rand nextIntFrom: 60 to: 72)))
		ampl: (Array with: ((rand nextIntFrom: 24 to: 36)
					to: (rand nextIntFrom: 36 to: 48))
				with: ((rand nextIntFrom: 72 to: 88)
					to: (rand nextIntFrom: 80 to: 112)))
		voice: (1 to: 4)
		density: (rand nextIntFrom: 4 to: 16)! !


!classDefinition: #SelectionCloud category: #'Music-EventGenerators'!
Cloud subclass: #SelectionCloud
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!SelectionCloud commentStamp: '<historical>' prior: 0!
Selection clouds are created with a set of pitch and amplitude values and return eventLists selected from these values.!


!SelectionCloud methodsFor: 'playing'!
eventList
	"Make the cloud's events"
	"assume that the receiver's pitch, loudness and durations are actually intervals"

	| pattern rand number length start |
	pattern := EventList new.
	rand := Random new.
	density isEvent
		ifTrue: [^self eventListWithDensityFunction].
	number := duration asSeconds value asFloat * density.
	length := (1000 / density) rounded.
	start := 0.
	number truncated timesRepeat:
		[ pattern add: (MusicEvent 
			dur: (length * 2 "+ (2000 * rand next) rounded")
			pitch: (self pitch value atRandom: rand)
			ampl: (self loudness value atRandom: rand)
			voice: (self voice atRandom: rand))
			at: start.
		start := start + length].
	^pattern! !

!SelectionCloud methodsFor: 'playing'!
eventListWithDensityFunction
	"make the cloud's events"
	"assume that the receiver's density is a function and that the voices are a selection set."

	| dur pattern rand dens time playing ends snd |
	pattern := EventList new.
	rand := Random new.
	time := 0.0. 		"msec"
	playing := 0.
	dens := density at: 0.
	ends := SortedCollection new: (density range extent + 1) truncated.
"prime the generators"
	playing := 1.
	ends add: 0.0.							"time at first note end"
	time := 0.0.
	dur := duration asSec value.
"Step through the end times."
	[time < dur] whileTrue:
		[time := ends removeFirst.			"step to next end time"
		playing := playing - 1.
		dens := density at: (time / dur).
		[dens > playing] whileTrue: 
			[snd := self voice atRandom: rand.
			pattern add: (MusicEvent dur: snd duration 
								voice: snd 
								ampl: (1.0 / dens)) at: time.
			ends add: (time + snd duration asSec value).
			playing := playing + 1]].
	^pattern! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SelectionCloud class' category: #'Music-EventGenerators'!
SelectionCloud class
	instanceVariableNames: ''!

!SelectionCloud class methodsFor: 'examples'!
example1
	"Create a low 4 second cloud selecting pitch, amp and voice from value arrays."
	"SelectionCloud example1"

	(SelectionCloud  dur: 4  
		pitch: #(32 40 48 50 52 55 57 )
		ampl: #(80 80 120)
		voice: #(1 2 3 4)
		density: 8) eventList "edit" play "inspect"! !

!SelectionCloud class methodsFor: 'examples'!
exampleRand
	"Edit a selection cloud with random properties."
	"SelectionCloud exampleRand"

	| rand pBase pArray aArray score |
	rand := Random new.
	pBase := 30 + (rand next * 40) rounded.
	pArray := Array new: 2 + ((rand next * 4) rounded).
	1 to: pArray size do: [:ind | pArray at: ind put: (pBase+(rand  next * 12) rounded)].
	pBase := 20 + (rand next * 60) rounded.
	aArray := Array new: 2 + ((rand next * 6) rounded).
	1 to: aArray size do: [:ind | aArray at: ind put: (pBase+(rand  next * (120 - pBase)) rounded)].
	score := (SelectionCloud  dur: (3 + (rand next * 4) rounded)
		pitch: pArray
		ampl: aArray
		voice: (1 to: 8)
		density: (3 + (rand next * 15) rounded)) eventList.
	score edit.
"	score play.
	(Delay forMilliseconds: (score duration + 1000)) wait
"! !

!SelectionCloud class methodsFor: 'examples'!
randomExample
	"Create a selection cloud with random properties."
	"SelectionCloud randomExample"

	| rand pBase pArray aArray score |
	rand := Random new.
	pBase := 20 + (rand next * 40) rounded.
	pArray := Array new: 2 + ((rand next * 4) rounded).
	1 to: pArray size do: [:ind | pArray at: ind put: (pBase+(rand  next * 12) rounded)].
	pBase := 20 + (rand next * 60) rounded.
	aArray := Array new: 2 + ((rand next * 6) rounded).
	1 to: aArray size do: [:ind | aArray at: ind put: (pBase+(rand  next * (120 - pBase)) rounded)].
	score := (SelectionCloud  dur: (3 + (rand next * 4) rounded)
		pitch: pArray
		ampl: aArray
		voice: (1 to: 8)
		density: (3 + (rand next * 15) rounded)) eventList.
	score inspect.
"	score play.
	(Delay forMilliseconds: (score duration + 1000)) wait
"! !


!classDefinition: #DynamicSelectionCloud category: #'Music-EventGenerators'!
SelectionCloud subclass: #DynamicSelectionCloud
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!DynamicSelectionCloud commentStamp: '<historical>' prior: 0!
Dynamic selection clouds are described by their beginning and ending pitch and amplitude sets and interpolate between them.!


!DynamicSelectionCloud methodsFor: 'playing'!
eventList
	"make the dynamic selection cloud's events"
	"assume that the receiver's pitch, loudness and durations are actually 2-D arrays"

	| pattern rand number length start |
	pattern := EventList new.
	rand := Random new.
	number := duration asSeconds value asFloat * density.
	length := (1000 / density) rounded.
	start := 0.
	1 to: number do:
		[ :counter |
		pattern add: (MusicEvent 
			dur: (length * 2 "+ (1000 * rand next) rounded")
			pitch: ((rand next > (counter / number)) 
					ifTrue: [ (self pitch value at: 1) atRandom: rand  ]
					ifFalse: [ (self pitch value at: 2) atRandom: rand ])
			ampl: (self loudness value atRandom: rand)
			voice: (self voice atRandom: rand))
			at: start.
		start := start + length].
	^pattern! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DynamicSelectionCloud class' category: #'Music-EventGenerators'!
DynamicSelectionCloud class
	instanceVariableNames: ''!

!DynamicSelectionCloud class methodsFor: 'examples'!
example1
	"Create a selection cloud that focuses onto a trill."
	"DynamicSelectionCloud example1"

	(DynamicSelectionCloud  dur: 4
		pitch: #( #(36 38 40 43 45) #(53 54) )
		ampl: #(80 80 120)
		voice: #(1 3 5 7)
		density: 16) eventList "edit" play "inspect"! !

!DynamicSelectionCloud class methodsFor: 'examples'!
example2
	"Create a selection cloud that makes a transition from one triad to another"
	"DynamicSelectionCloud example2"

	(DynamicSelectionCloud dur: 4
		pitch: #( #(60 62 64) #(72 74 76) )
		ampl: #(80 80 120)
		voice: #(1 3 5 7)
		density: 12) eventList open "inspect"! !

!DynamicSelectionCloud class methodsFor: 'examples'!
randomExample
	"Answer a dynamic selection cloud with random properties."
	"DynamicSelectionCloud randomExample"

	| rand pBase pArray1 pArray2 aArray1 score |
	rand := Random new.
	pBase := rand nextIntFrom: 42 to: 54.
	pArray1 := Array new: (rand nextIntFrom: 2 to: 4).
	1 to: pArray1 size do: 
			[:ind | pArray1 at: ind put: (rand nextIntFrom: pBase to: pBase + 12)].
	pBase := rand nextIntFrom: 60 to: 72.
	pArray2 := Array new: (rand nextIntFrom: 4 to: 8).
	1 to: pArray2 size do: 
			[:ind | pArray2 at: ind put: (rand nextIntFrom: pBase to: pBase + 12)].
	pBase := rand nextIntFrom: 40 to: 60.
	aArray1 := Array new: (rand nextIntFrom: 2 to: 6).
	1 to: aArray1 size do: 
			[:ind | aArray1 at: ind put: (rand nextIntFrom: pBase to: pBase + 50)].
	Transcript cr; show: 'DynSelCloud: ', pArray1 printString; cr; 
			show: pArray2 printString; cr; cr.
	score := (DynamicSelectionCloud  dur: 8
		pitch: (Array with: pArray1 with: pArray2)
		ampl: aArray1
		voice: (1 to: 8)
		density: (rand nextIntFrom: 5 to: 15)) eventList.
"	score inspect.
"
"	score play.
	(Delay forMilliseconds: (score duration + 1000)) wait
"
	score edit! !


!classDefinition: #ExtDynamicSelectionCloud category: #'Music-EventGenerators'!
DynamicSelectionCloud subclass: #ExtDynamicSelectionCloud
	instanceVariableNames: 'list'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!

!ExtDynamicSelectionCloud methodsFor: 'playing'!
eventList
	"make the dynamic selection cloud's events"
	"assume that the receiver's 'pitch' is a list of associations like
		(0 -> pitch-set)
		(1 -> pitch-set) ..."

	| pattern rand length start to from howMany pList |
	pattern := EventList new.
	rand := Random new.
	length := (1 / density) asFloat.
	start := 0.
	pList := self pitch value.
	to := pList at: 1.
	2 to: pList size do:
		[ :which | | set1 set2 |
		from := to.
		to := pList at: which.
		set1 := from value.
		set2 := to value.
		howMany := ((to key - from key) * density).
"		Transcript show: start printString, ' -> ', howMany printString, 
						' from ', set1 printString, ' & ', set2 printString; cr.
"		1 to: howMany do:
			[ :count |
			pattern add: (MusicEvent 
				dur: length
				pitch: ((rand next > (count / howMany)) 
					ifTrue: [set1 atRandom: rand]
					ifFalse: [set2 atRandom: rand])
				ampl: self loudness
				voice: self voice)
				at: start.
			start := start + length]].
	^pattern! !


!ExtDynamicSelectionCloud methodsFor: 'accessing'!
list: aList
	"Set the receiver's list"

	list := aList! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ExtDynamicSelectionCloud class' category: #'Music-EventGenerators'!
ExtDynamicSelectionCloud class
	instanceVariableNames: ''!

!ExtDynamicSelectionCloud class methodsFor: 'examples'!
chordExample
	"Answer a dynamic selection cloud that plays chords from a scale."
	"ExtDynamicSelectionCloud chordExample"
	"Create cloud from a set like
		(0 -> chord) (1 -> chord) ..."
	
	| score chords list |
	chords := ((NeapolitanMinor root: N do) generateChordsPoly: 5 inOctave: 3) scrambled.
	list := OrderedCollection new.
	1 to: 7 do:
		[ :ind |
		ind even ifTrue: [list add: ((ind - 1) * 3 -> ((chords at: ind) collect: [ :no | no + 24]))]
			ifFalse: [list add: ((ind - 1) * 3 -> (chords at: ind))]].
	score := (self dur: 8
		pitch: list
		ampl: 60
		voice: nil
		density: 10) eventList.
	score eventsDo: [ :ev |
		ev inst: '/i1/pn'.
		ev modIndex: 2.0.
		ev ratio: 1.02.
		ev pos: 0.0].
"	score edit"
	score voice: (OSCVoice map: #pMapForCSLSimpleFM).
	score play! !


!classDefinition: #Cluster category: #'Music-EventGenerators'!
EventGenerator subclass: #Cluster
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Cluster commentStamp: '<historical>' prior: 0!
Cluster is an abstraction of a set of simultaneous (or same-pitch) events.
A cluster need only have a set of pitches, or a rhythm.
!


!Cluster methodsFor: 'playing'!
eventList
	"make the cluster's events"

	| pattern |
	pattern := EventList newAnonymous.
	self events do:
		[ :anEvent |
		pattern add: (MusicEvent 
			dur: self duration
			pitch: ((anEvent isAssociation) 
					ifTrue: [anEvent value pitch]
					ifFalse: [anEvent])
			ampl: self loudness
			voice: self voice)
			at: 0 ].
	^pattern! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Cluster class' category: #'Music-EventGenerators'!
Cluster class
	instanceVariableNames: ''!

!Cluster class methodsFor: 'instance creation'!
dur: aD list: anEL ampl: anA
	"return an initialized Cluster with the given list as pitches"

	^self new duration: aD; 
		events: anEL; 
		loudness: anA! !

!Cluster class methodsFor: 'instance creation'!
dur: aD list: anEL ampl: anA voice: aV
	"return an initialized Cluster with the given list as pitches"

	^self new duration: aD; 
		events: anEL; 
		loudness: anA; 
		voice: aV! !

!Cluster class methodsFor: 'instance creation'!
dur: aD pitchSet: aColl ampl: anA voice: aV
	"return an initialized Cluster with the given list as pitches"

	^self new duration: aD; 
		events: aColl; 
		loudness: anA; 
		voice: aV! !


!Cluster class methodsFor: 'examples'!
example1
	"Cluster example1"
	"play a 2 second c-major chord"

	(Cluster  dur: 2.0
		pitchSet: (#(0 4 7  12 16 19  24 28 31  36 40 43  48) collect: [ :val | val + 24])
		ampl: 100
		voice: 1) play! !

!Cluster class methodsFor: 'examples'!
example2
	"Cluster example2"
	"play a low 2 second cluster"

	(Cluster  dur: 2.0
		pitchSet: #(48  50  52  54 56)
		ampl: 100
		voice: 6) play! !


!classDefinition: #Chord category: #'Music-EventGenerators'!
Cluster subclass: #Chord
	instanceVariableNames: 'root inversion type arity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Chord commentStamp: '<historical>' prior: 0!
Instances of Chord are eventLists that can be created by giving them a root and inversion. They can return eventLists.

Instance variables:
	root <Pitch> the root of the chord
	inversion <Integer> the inversion level (unused at present)
	type <Symbol> #major, #minor, etc.
	arity <Integer> number of notes of the chord!


!Chord methodsFor: 'accessing'!
arity: aNumber
	"set the number of notes of the receiver chord"

	arity := aNumber! !

!Chord methodsFor: 'accessing'!
duration: aDuration
	"set the durations of my notes"
	
	| realDuration |
	aDuration class == Duration 
		ifTrue: [ realDuration := aDuration ]
		ifFalse: [ realDuration := Duration value: aDuration ].
	events do: [ :anAssociation |
		anAssociation value duration: realDuration ].
	duration := realDuration asMS! !

!Chord methodsFor: 'accessing'!
eventList
	"Answer the receiver's events"

	type == #major
		ifTrue: [arity = 3
			ifTrue: [^self majorTriad]
			ifFalse: [^self majorTetrad]]
		ifFalse: [arity = 3
			ifTrue: [^self minorTriad]
			ifFalse: [^self minorTetrad]].
	self error: 'Unknown chord type/arity'! !

!Chord methodsFor: 'accessing'!
inversion: number
	"set the inversion of the receiver chord"

	inversion := number! !

!Chord methodsFor: 'accessing'!
root: tonic
	"set the root of the receiver chord"

	root := tonic! !

!Chord methodsFor: 'accessing'!
type: aSymbol
	"set the type symbol of the receiver chord"

	type := aSymbol! !


!Chord methodsFor: 'generating events'!
majorTetrad
	"return a three-note major chord on the given tonic in the given inversion"
	"Chord majorTetradOn: 'c3' inversion: 0"

	| me rootNote |
	me := EventList newAnonymous.
	rootNote := me defaultEventClass dur: duration pitch: root ampl: 100.
	inversion = 0 ifTrue:
		[ me add: rootNote at: 0;
			add: (rootNote copy transposeBy: 4) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0 ].
	inversion = 1 ifTrue:
		[ me add: (rootNote copy transposeBy: 4) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 16) at: 0 ].
	inversion = 2 ifTrue:
		[ me add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 16) at: 0;
			add: (rootNote copy transposeBy: 19) at: 0 ].
	^me! !

!Chord methodsFor: 'generating events'!
majorTriad
	"return a three-note major chord on the given tonic in the given inversion"
	"Chord majorTriadOn: 'c3' inversion: 0"

	| me rootNote |
	me := EventList newAnonymous.
	rootNote := me defaultEventClass dur: duration pitch: root ampl: 100.
	inversion = 0 ifTrue:
		[ me add: rootNote at: 0;
			add: (rootNote copy transposeBy: 4) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0 ].
	inversion = 1 ifTrue:
		[ me add: (rootNote copy transposeBy: 4) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0 ].
	inversion = 2 ifTrue:
		[ me add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 16) at: 0 ].
	^me! !

!Chord methodsFor: 'generating events'!
minorTetrad
	"return a three-note minor chord on the given tonic in the given inversion"
	"Chord minorTetradOn: 'c3' inversion: 0"

	| me rootNote |
	me := EventList newAnonymous.
	rootNote := me defaultEventClass dur: duration pitch: root ampl: 100.
	inversion = 0 ifTrue:
		[ me add: rootNote at: 0;
			add: (rootNote copy transposeBy: 3) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0 ].
	inversion = 1 ifTrue:
		[ me add: (rootNote copy transposeBy: 3) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 15) at: 0 ].
	inversion = 2 ifTrue:
		[ me add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 15) at: 0;
			add: (rootNote copy transposeBy: 19) at: 0 ].
	^me! !

!Chord methodsFor: 'generating events'!
minorTriad
	"return a three-note minor chord on the given tonic in the given inversion"
	"Chord minorTriadOn: 'c3' inversion: 0"

	| me rootNote |
	me := EventList newAnonymous.
	rootNote := me defaultEventClass dur: duration pitch: root ampl: 100.
	inversion = 0 ifTrue:
		[ me add: rootNote at: 0;
			add: (rootNote copy transposeBy: 3) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0 ].
	inversion = 1 ifTrue:
		[ me add: (rootNote copy transposeBy: 3) at: 0;
			add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0 ].
	inversion = 2 ifTrue:
		[ me add: (rootNote copy transposeBy: 7) at: 0;
			add: (rootNote copy transposeBy: 12) at: 0;
			add: (rootNote copy transposeBy: 15) at: 0 ].
	^me! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Chord class' category: #'Music-EventGenerators'!
Chord class
	instanceVariableNames: ''!

!Chord class methodsFor: 'instance creation'!
majorTetradOn: tonic inversion: inversion
	"return a three-note major chord on the given tonic in the given inversion"
	"Chord majorTetradOn: 'c3' inversion: 0"

	| me |
	me := self newAnonymous.
	me root: tonic;
		inversion: inversion;
		type: #major;
		arity: 4.
	^me! !

!Chord class methodsFor: 'instance creation'!
majorTriadOn: tonic inversion: inversion
	"return a three-note major chord on the given tonic in the given inversion"
	"Chord majorTriadOn: 'c3' inversion: 0"

	| me |
	me := self newAnonymous.
	me root: tonic;
		inversion: inversion;
		type: #major;
		arity: 3.
	^me! !

!Chord class methodsFor: 'instance creation'!
minorTetradOn: tonic inversion: inversion
	"return a three-note minor chord on the given tonic in the given inversion"
	"Chord minorTetradOn: 'c3' inversion: 0"

	| me |
	me := self newAnonymous.
	me root: tonic;
		inversion: inversion;
		type: #minor;
		arity: 4.
	^me! !

!Chord class methodsFor: 'instance creation'!
minorTriadOn: tonic inversion: inversion
	"return a three-note minor chord on the given tonic in the given inversion"
	"Chord minorTriadOn: 'c3' inversion: 0"

	| me |
	me := self newAnonymous.
	me root: tonic;
		inversion: inversion;
		type: #minor;
		arity: 3.
	^me! !


!Chord class methodsFor: 'examples'!
example
	"Create and edit some chords."

	"(Chord majorTriadOn: 'c3' inversion: 0) inspect"

	"((Chord majorTetradOn: 'f2' inversion: 2) duration: 1.0) inspect"

	((Chord majorTetradOn: 'g5' inversion: 1) duration: 1.0) edit! !


!classDefinition: #Arpeggio category: #'Music-EventGenerators'!
Chord subclass: #Arpeggio
	instanceVariableNames: 'delay'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Arpeggio commentStamp: '<historical>' prior: 0!
Arpeggii can be created on Chords or other event lists and can step through their events (assumed to be simultaneous at the start) with the given delay time.

Instance variables:
	delay <Duration or Number> the delay betyween the onsets of my events!


!Arpeggio methodsFor: 'playing'!
edit
	"edit me"

"	HauerSteffensView openWith8ButtonsAndHAndVScrollingOn: self"! !

!Arpeggio methodsFor: 'playing'!
eventList
	"return my event list"

	^events! !

!Arpeggio methodsFor: 'playing'!
play
	"don't expand me"

	^self playAt: 0! !


!Arpeggio methodsFor: 'private'!
setDelays
	"set the start times of my notes"

	| counter |
	counter := 0.
	events do:
		[ :anAss |
		anAss key: (MSecondDuration value: (anAss key value + (counter * delay))).
		counter := counter + 1]! !


!Arpeggio methodsFor: 'accessing'!
delay: aValue
	"set the delay between the onset of my notes"

	delay := aValue.
	self setDelays.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Arpeggio class' category: #'Music-EventGenerators'!
Arpeggio class
	instanceVariableNames: ''!

!Arpeggio class methodsFor: 'instance creation'!
on: aChordOrList delay: aDelay
	"create an Arpeggio on the given chord or event list"

	| me |
	me := self newAnonymous.
	me events: aChordOrList events copy.
	me delay: aDelay.
	^me! !


!classDefinition: #Roll category: #'Music-EventGenerators'!
Cluster subclass: #Roll
	instanceVariableNames: 'number delta noteDuration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Roll commentStamp: '<historical>' prior: 0!
Rolls repeat their single events.
It will eventually be possible to apply pitch, amplitude or duration envelopes to them as well.

Instance variables:
	number <Integer> how many notes to play
	delta <Number> delta time between events
	noteDuration <Number> duration of events!


!Roll methodsFor: 'initialize'!
length: aLength rhythm: aDuration note: aNote
	"make a new roll of the specified length..."

	delta := aDuration.
	noteDuration := aDuration.
	number := aLength / aDuration.
	self pitch: (Pitch value: aNote).
	^self! !

!Roll methodsFor: 'initialize'!
number: aNumber rhythm: aDuration note: aNote
	"make a new roll with the specified number of notes..."

	delta := aDuration.
	number := aNumber.
	noteDuration := aDuration.
	self pitch: (Pitch value: aNote).
	^self! !


!Roll methodsFor: 'playing'!
eventList
	"return an eventList for me"

	| newList |
	newList := EventList newAnonymous.
	0 to: number do:
		[ :count |
			newList add: (self defaultEventClass 
					dur: noteDuration pitch: self pitch ampl: self loudness) 
				at: (delta * count) ].
	^newList! !


!Roll methodsFor: 'accessing'!
duration
	"compute and answer the duration"

	^duration == nil 
		ifFalse: [duration]
		ifTrue: [number * noteDuration]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Roll class' category: #'Music-EventGenerators'!
Roll class
	instanceVariableNames: ''!

!Roll class methodsFor: 'instance creation'!
length: aNumber rhythm: aDuration note: aNote
	"return a new roll of the specified length..."

	^self new length: aNumber rhythm: aDuration note: aNote! !

!Roll class methodsFor: 'instance creation'!
number: aNumber rhythm: aDuration note: aNote
	"return a new roll with the specified number of events..."

	^self new number: aNumber rhythm: aDuration note: aNote! !


!Roll class methodsFor: 'examples'!
rollExample
	"Create and edit/play a few rolls."

	"((Roll length: 2 rhythm: 100 note: 48) ampl: 100) eventList"
	"((Roll length: 2 rhythm: 100 note: 48) ampl: 100) eventList edit"
	((Roll length: 2 rhythm: 50 note: 60) ampl: 80) eventList play.
"	MIDIPort allNotesOff."! !


!classDefinition: #Trill category: #'Music-EventGenerators'!
Roll subclass: #Trill
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Trill commentStamp: '<historical>' prior: 0!
A trill is like a roll, except that it can be given an eventList for repetition.!


!Trill methodsFor: 'playing'!
eventList
	"return an eventList for me"

	| newList |
	newList := EventList newAnonymous.
	0 to: number do:
		[ :count |
			newList add: (self defaultEventClass 
					dur: noteDuration 
					pitch: (self pitch value at: ((count \\ self pitch value size) + 1)) 
					ampl: self loudness) 
				at: (delta * count) ].
	^newList! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Trill class' category: #'Music-EventGenerators'!
Trill class
	instanceVariableNames: ''!

!Trill class methodsFor: 'instance creation'!
length: aNumber rhythm: aDuration note: aNote
	"return a new roll of the specified length..."

	^self new length: aNumber rhythm: aDuration note: aNote! !

!Trill class methodsFor: 'instance creation'!
length: aNumber rhythm: aDuration notes: aNote
	"return a new trill of the specified length..."

	^self new length: aNumber rhythm: aDuration note: aNote! !


!classDefinition: #Ostinato category: #'Music-EventGenerators'!
EventGenerator subclass: #Ostinato
	instanceVariableNames: 'list playing process'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventGenerators'!
!Ostinato commentStamp: '<historical>' prior: 0!
Ostinati can repeat their basic eventList without end, if necessary.

Instance variables:
	list <EventList> my riff
	playing <Boolean> am i on?
	process <Process> the process that's playing me
!


!Ostinato methodsFor: 'initializing'!
initializeAnonymous
	"set up a new Ostinato."

	super initializeAnonymous.
	playing := false.! !

!Ostinato methodsFor: 'initializing'!
initializeNamed: aName
	"Set up a new Ostinato."

	super initializeNamed: aName.
	playing := false.! !


!Ostinato methodsFor: 'accessing'!
events
	^list events! !

!Ostinato methodsFor: 'accessing'!
list
	^list! !

!Ostinato methodsFor: 'accessing'!
list: anEL
	list := anEL! !

!Ostinato methodsFor: 'accessing'!
process: anBlockOrProc
	process := anBlockOrProc! !


!Ostinato methodsFor: 'playing'!
play: numberOfTimes
	"return an event list from the given number of times playing"

	| time pattern |
	time := 0 msec.
	pattern := EventList newAnonymous.
	numberOfTimes timesRepeat:
		[pattern add: list at: time.
		time := list duration + time].
	pattern play! !

!Ostinato methodsFor: 'playing'!
playAt: firstTime
	"Play an ostinato until turned off."

	| startTime rTime | 
	startTime := firstTime.
	rTime := Time microsecondClock.
	playing := true.
	voice := list voice == nil 
		ifFalse: [list voice] 
		ifTrue: [Voice default].
	process := [[playing] whileTrue: 
			[voice play: list at: startTime.
			startTime := startTime + list duration.
			(Delay untilMicroseconds: rTime + startTime - 500000) wait.
		Transcript show: 'x '. ]] fork"At: 5"! !

!Ostinato methodsFor: 'playing'!
playOn: aVoice at: firstTime
	"play an ostinato until turned off."

	| startTime | 
	startTime := firstTime.
	playing := true.
	process := [[playing] whileTrue: 
			[list playOn: aVoice at: startTime.
			startTime := startTime + list duration.
			(Delay untilMilliseconds: startTime - 500) wait]] forkAt: 5! !

!Ostinato methodsFor: 'playing'!
stop
	"stop a playing Ostinato."
		
	playing := false.
"	self list stop"! !


!Ostinato methodsFor: 'printing'!
printOn: aStream

	self name isNil ifFalse: [
		aStream nextPutAll: 'Ostinato named: ', self name asString, ' on EventList: (']
	ifTrue: [
		aStream nextPutAll: 'Ostinato on EventList: ('].
	aStream cr.
	list printOn: aStream.
	aStream nextPutAll: ') playing= ', playing printString; cr! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Ostinato class' category: #'Music-EventGenerators'!
Ostinato class
	instanceVariableNames: ''!

!Ostinato class methodsFor: 'instance creation'!
named: aSymbol onList: anEL 
	"create an Ostinato on the given EventList."

	| me |
	me := self new initializeNamed: aSymbol.
	me list: anEL.
	^me! !

!Ostinato class methodsFor: 'instance creation'!
named: aSymbol onList: anEL dur: theDur
	"create an Ostinato on the given EventList."

	| me |
	me := self new initializeNamed: aSymbol.
	me list: anEL.
	me dur: theDur.
	^me! !

!Ostinato class methodsFor: 'instance creation'!
newNamed: aSymbol onList: anEL 
	"create an Ostinato on the given EventList."

	| me |
	me := self new initializeNamed: aSymbol.
	me list: anEL.
	^me! !

!Ostinato class methodsFor: 'instance creation'!
onList: anEL 
	"create an Ostinato on the given EventList."

	| me |
	me := self new initializeAnonymous.
	me list: anEL.
	me events: anEL events.
	^me! !

!Ostinato class methodsFor: 'instance creation'!
onList: anEL dur: theDur
	"create an Ostinato on the given EventList."

	| me |
	me := self new initializeAnonymous.
	me list: anEL.
	me dur: theDur.
	^me! !


!Ostinato class methodsFor: 'examples'!
ostinatoExamples
	"Create and edit some ostinati."

	"Ostinato onList: (EventList named: #scale1)"
	"Ostinato onList: (EventList named: #scale1) edit"
	"Ostinato newNamed: #scaleO onList: (EventList named: #scale1)"
	"(Ostinato named: #scaleO) play"
	"(Ostinato named: #scaleO) playOn: 1 at: 0"
	"(Ostinato named: #scaleO) stop"! !
