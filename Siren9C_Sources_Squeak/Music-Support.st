Model subclass: #DeviceModel	instanceVariableNames: 'name port index stream debug direction rate'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/2/2023 12:00'!direction	^direction! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/2/2023 12:02'!direction: aSymbol 	"Set the receiver's direction flag to #in, #out or #inOut"		direction := aSymbol! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/2/2023 12:00'!in	^ direction == #in! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/5/2023 07:14'!in: aValue 	index := aValue.	direction := #in! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/5/2023 07:12'!index	^ index! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/5/2023 07:13'!index: anInt	index := anInt! !!DeviceModel methodsFor: 'accessing'!name

	^name! !!DeviceModel methodsFor: 'accessing'!name: aValue

	name := aValue! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/5/2023 11:48'!out	^ direction == #out! !!DeviceModel methodsFor: 'accessing' stamp: 'stp 1/5/2023 07:14'!out: aValue 	index := aValue.	direction := #out! !!DeviceModel methodsFor: 'accessing'!port

	^port! !!DeviceModel methodsFor: 'accessing'!port: aValue

	port := aValue! !!DeviceModel methodsFor: 'printing' stamp: 'stp 1/2/2023 12:10'!printOn: aStream 	"Append to the argument aStream a sequence of characters 
	that	identifies the collection."	aStream print: 'A ', self class;		 nextPutAll: ' ('.	name		ifNotNil: [aStream nextPutAll: 'A ', name asString , ' - '].	port		ifNotNil: [aStream nextPutAll: 'port ' , port printString].	direction		ifNotNil: [aStream nextPutAll: ' - dir ' , direction printString].	aStream nextPut: $)! !!DeviceModel methodsFor: 'printing' stamp: 'stp 1/4/2023 08:36'!printString	"Append to the argument aStream a sequence of characters 
	that	identifies the collection."	^String		streamContents: [:strm | self printOn: strm]! !Model subclass: #EditorModel	instanceVariableNames: 'selections copyBuffer view'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!EditorModel methodsFor: 'accessing'!copyBuffer
	"Answer the receiver's copybuffer"

	^copyBuffer! !!EditorModel methodsFor: 'accessing'!copyBuffer: anObject
	"Set the receiver's copybuffer"

	copyBuffer := anObject! !!EditorModel methodsFor: 'accessing'!select: sel
	"Set the receiver's selection."

"	sel == nil ifTrue: [Transcript show: 'clear'; cr]."
	selections := sel.
	self changed: #selection with: sel! !!EditorModel methodsFor: 'accessing'!selection
	"Answer the receiver's selection."

	^selections! !!EditorModel methodsFor: 'accessing'!view: aView
	"Assign into the receiver's view instVar"

	view := aView! !Association subclass: #EventAssociation	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!EventAssociation commentStamp: '<historical>' prior: 0!An EventAssociation is an Association with some special semantics for Siren. It can be created with the '=>' message to a Number, meaning start-time assicoated with event.!!EventAssociation methodsFor: 'accessing'!, anArgument
	"Add the argument as a property of the receiver."
	"((0 beat) => (1/16 beat,  'c3' pitch)),
	  ((1/16 beat) => (1/16 beat, 'b2'  pitch))"

	^EventList new add: self; add: anArgument! !!EventAssociation methodsFor: 'accessing'!event
	"Answer the receiver's event (value)."

	^self value! !!EventAssociation methodsFor: 'accessing'!start
	"Answer the receiver's start (key)."

	^self key! !!EventAssociation methodsFor: 'accessing' stamp: 'stp 12/30/2022 11:59'!start: aTime
	"Set the receiver's start (key)."

	self key: (MDuration value: aTime)! !!EventAssociation methodsFor: 'accessing'!stop
	"Answer the stop time of the event association."

	^self start + self event duration! !!EventAssociation methodsFor: 'accessing'!time
	"Answer the receiver's start (key)."

	^self key! !!EventAssociation methodsFor: 'printing'!printOn: aStream 
	"Append to the argument, aStream, the two elements of the 
	EventAssociation separated by a double-right arrow (=>)."

	key printOn: aStream.
	aStream nextPutAll: ' => '.
	value printOn: aStream! !!EventAssociation methodsFor: 'comparing'!< anAssociation 
	"Handle events, associations, and time/order sorting."

	anAssociation isAssociation
		ifFalse: [^value < anAssociation].
	key ifNil:
		[anAssociation key ifNil:
			[^((key order) < (anAssociation order))]].
	^key < anAssociation key! !!EventAssociation methodsFor: 'comparing'!= anAssociation
	"If the argument's not an association, compare it to the receiver's value, otherwise answer whether the receiver's key and value are both equal to the argument's."

	(anAssociation isAssociation)
		ifFalse: [^value = anAssociation].
	key = anAssociation key
		ifTrue: [^value = anAssociation value]
		ifFalse: [^false]! !Model subclass: #EventScheduler	instanceVariableNames: 'appointments timers threads running doWait startTime delay logTime'	classVariableNames: 'ESchedule'	poolDictionaries: ''	category: 'Music-Support'!!EventScheduler commentStamp: '<historical>' prior: 0!An instance of EventScheduler is a simple driver for real-time message-passing among any classes that can return timed event association values.  One uses Schedules by setting up clients which a master scheduler process continuously evaluates for their next timed activations. Clients can return appointments which are sorted into the scheduler's list of (time -> event) associations that is evaluated continuously when running. Changed messages are sent from the accessor messages to simplify EventScheduler browsers and inspectors.

Instance variables:
	clients 		<Collection> client objects
	appointments <SEventQueue> the schedule
	running 		<Boolean> running schedule?
	doWait		<Boolean> whether or not the scheduler should do real-time 
					scheduling--set to false if there is a lower level of scheduling 
					(e.g., primitive-level) or if output need not be real-time
	startTime <Integer> the clock time I was started at

Class variable:
	ESchedule 	<EventScheduler> shared instance accessed by the class message masterSchedule

See the class examples.!!EventScheduler methodsFor: 'accessing'!addClient: theApp
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	appointments add: theApp loop: false.
	self changed: #appointments! !!EventScheduler methodsFor: 'accessing'!addClient: theApp at: start
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: start loop: false! !!EventScheduler methodsFor: 'accessing'!addClient: theApp at: start loop: aBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	appointments add: (start -> theApp) loop: aBool.
	self changed: #appointments! !!EventScheduler methodsFor: 'accessing' stamp: 'stp 12/30/2022 15:56'!addClient: theApp in: start
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: (Time utcMicrosecondClock + start asUsec value) loop: false! !!EventScheduler methodsFor: 'accessing'!addClient: theApp in: start loop: loopBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: (Time microsecondClock + start asUsec value) loop: loopBool! !!EventScheduler methodsFor: 'accessing'!addClient: theApp loop: aBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: Time microsecondClock + 1000 loop: false! !!EventScheduler methodsFor: 'accessing'!addTimer: start
	"Add the argument, 'theApp', to the instance variable 'timers'."

	^self addTimer: start interval: 1.0! !!EventScheduler methodsFor: 'accessing'!addTimer: start interval: int
	"Add the argument, 'theApp', to the instance variable 'timers'."

	| timer |
	timer := Timer new interval: int.
	timer name: ('Timer', (timers size + 1) printString) asSymbol.
	timers add: (timer startTime usec => timer).
	self changed: #timers.
	self run.
	^timer! !!EventScheduler methodsFor: 'accessing'!clientNamed: aName
	"Answer a named client"

	^appointments detect: [ :ass | ass value name == aName]! !!EventScheduler methodsFor: 'accessing'!clients
	"Get the instance's clients"

	^appointments! !!EventScheduler methodsFor: 'accessing'!clock
	"Answer the instance's clock (in usec)"

	startTime ifNil: [startTime := Time microsecondClock].
	^Time microsecondClock - startTime! !!EventScheduler methodsFor: 'accessing'!delay
	"Get the instance's delay"

	^delay! !!EventScheduler methodsFor: 'accessing'!delay: aTime
	"Set the instance's delay"

	^delay delay: aTime! !!EventScheduler methodsFor: 'accessing'!dontWait
	"Specify that the scheduler should NOT do the waiting--i.e., there is a lower level of scheduling going on"
	"ESchedule dontWait"

	doWait := false! !!EventScheduler methodsFor: 'accessing'!isRunning
	"Answer the instance variable 'running'."

	^running! !!EventScheduler methodsFor: 'accessing'!removeClient: aClient
	"Add the argument, 'theApp', to the instance variable 'timers'."

	appointments removeAllSuchThat: [ :ass | ass value == aClient].
	self changed: #appointments.! !!EventScheduler methodsFor: 'accessing'!removeClientNamed: aClient
	"Add the argument, 'theApp', to the instance variable 'timers'."

	appointments removeAllSuchThat: [ :it | it value name = aClient].
	self changed: #appointments.! !!EventScheduler methodsFor: 'accessing'!removeTimer: aTimer
	"Add the argument, 'theApp', to the instance variable 'timers'."

	timers removeAllSuchThat: [ :ass | ass value == aTimer].
	self changed: #timers.! !!EventScheduler methodsFor: 'accessing'!removeTimerNamed: aTimer
	"Add the argument, 'theApp', to the instance variable 'timers'."

	timers removeAllSuchThat: [ :ass | ass key == aTimer].
	self changed: #timers.! !!EventScheduler methodsFor: 'accessing'!resetClock
	"Reset the instances clock (in usec)"
	"ESchedule resetClock"

	startTime := Time microsecondClock
! !!EventScheduler methodsFor: 'accessing'!timerNamed: aName
	"Answer a named timer"

	^timers detect: [ :ass | ass value name == aName]! !!EventScheduler methodsFor: 'accessing'!timers
	"Answer the instance's timers"

	^timers! !!EventScheduler methodsFor: 'accessing'!wait
	"Specify that the scheduler should do the waiting--i.e., Smalltalk-level real-time."
	"ESchedule wait"

	doWait := true! !!EventScheduler methodsFor: 'updating'!update: anAspect
	"Check if clients are waiting"

	(appointments isEmpty and: [timers isEmpty])
		ifTrue: [Transcript show: 'Sched stopped'; cr]! !!EventScheduler methodsFor: 'running' stamp: 'stp 12/30/2022 15:57'!callNextAppointment
	"Calls upon the next appointment to be made and then reschedules the next one"

	| entry nextTime now todo |
	now := Time utcMicrosecondClock.
	todo := false.
	(((SirenUtility verbosity > 1) 					"if really verbose and <shift>, print time and update log time"
		and: [Sensor shiftPressed])
		and: [now - logTime > 250000])			"only print 4 per second"
			ifTrue: [logTime := now.
				SirenUtility log: '  S: ', ((now - startTime) / 1000000.0 printString: 5)].
	(timers ready: now) 							"If there's a timer ready"
		ifTrue: [todo := true.
			entry := timers removeFirst.
			nextTime := self processEvent: entry at: now.
			nextTime ifNotNil:
				[timers add: (nextTime => entry value)].
			entry clear.
			^self changed: #timer with: entry].
	(appointments ready: now) 					"If there's an event ready"
		ifTrue: [todo := true.
			entry := appointments removeFirst.
			nextTime := self processEvent: entry at: now.
			nextTime ifNotNil:
				[appointments add: (nextTime => entry value) loop: entry loop].
			entry clear.
			^self changed: #event with: entry].
	todo ifFalse: [delay wait]! !!EventScheduler methodsFor: 'running'!flush
	"Reset the appointment list."
	"EventScheduler flush"

	self interrupt.
	appointments := SEventQueue new.
	timers := SEventQueue new.
	threads := OrderedCollection new.
	SEventQueue initialize! !!EventScheduler methodsFor: 'running'!interrupt
	"Stop a running scheduler."

	self run: false.
	threads isEmpty not
		ifTrue: [threads first terminate].
	^self! !!EventScheduler methodsFor: 'running'!processEvent: entry at: now
	"Handle an event or timer"

	| nextTime time event thisTime |
	time := entry key.							"get the delta time"
	event := entry value.						"get the event or timer"

	doWait														"schedule the event now"
		ifFalse: [nextTime := event scheduleAt: time]
		ifTrue: [thisTime := time usec value.	"if running in own thread, wait"
			thisTime > now
				ifTrue: [(Delay forMicroseconds: (thisTime - now)) wait].
			nextTime := event scheduleAt: thisTime].			"schedule the event later"

"	nextTime ifNotNil: [Transcript show: 'Wait: ', (thisTime - now) printString, 
					' next ', (nextTime - startTime) printString; cr].
"	nextTime ifNil:
		[entry loop ifTrue: 
			[event reset.
			nextTime := now]].
	^nextTime! !!EventScheduler methodsFor: 'running' stamp: 'stp 12/30/2022 15:56'!run
	"Set up the first meetings and then run them all till no one wants a meeting anymore."

	| proc |
	running ifTrue: [^nil].
	startTime := Time utcMicrosecondClock.
	self run: true.					"set flag"
	threads isEmpty not
		ifTrue: [threads do: [ :pr | pr terminate]].
	threads := OrderedCollection new.			"enforce single-threading for now"
									"LOOP: as long as on, try next appt or timer"
	proc := [[running] whileTrue: [self callNextAppointment]] forkAt: (Processor userBackgroundPriority - 1).
	threads add: proc.
	self changed: #running with: true! !!EventScheduler methodsFor: 'running' stamp: 'stp 12/26/2022 21:54:08'!run: theBool
	"Accept the argument, 'theBool', as the instance variable 'running'."

	running := theBool.
	SirenUtility log: 'Sched: ', (theBool ifTrue: ['run'] ifFalse: ['stop']) level: 0.
	threads isEmpty
		ifFalse: [threads first ifNil: [threads removeFirst]
			ifNotNil: [theBool 
				ifTrue: [threads first resume]
				ifFalse: [threads first terminate]]].
	self changed: #running with: theBool! !!EventScheduler methodsFor: 'initialize release' stamp: 'stp 12/29/2022 03:04'!initialize
	"Set up the default Schedule"
	"ESchedule initialize"

	doWait := true.
	running := false.
	appointments := SEventQueue new.
	timers := SEventQueue new.
	threads := OrderedCollection new.
	delay := Delay forMilliseconds: 1.			"default tick delay"
	logTime := Time utcMicrosecondClock.! !!EventScheduler methodsFor: 'initialize release' stamp: 'Install-Music-Support 11/30/2022 15:51:57'!release
	"Clean up the schedule."

	delay unschedule.
	running ifTrue: [self run: false].
	threads isEmpty not
		ifTrue: [threads do: [ :pr | pr terminate]].
	self initialize! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!EventScheduler class	instanceVariableNames: ''!!EventScheduler class methodsFor: 'examples'!scheduleExample
	"Play MIDI using the event scheduler."
	"EventScheduler scheduleExample"
	"ESchedule interrupt; flush"

	 | scale start |
	scale := EventList scaleFrom: 36 to: 60 in: 2.0.
	scale scale: #duration by: 2.
	start := Time microsecondClock + 100000.
	scale events do: [ :ass |
		ass event voice: MIDIVoice default.
		ass key: (ass key value + start) usec.
		EventScheduler addClient: ass].
	"self halt."
	EventScheduler run! !!EventScheduler class methodsFor: 'examples'!scheduleExample2
	"Play MIDI using the event scheduler."
	"EventScheduler scheduleExample2"
	"EventScheduler interrupt; flush"

	| scale |
	scale := EventList scaleFrom: 36 to: 60 in: 2.0.
	scale scale: #duration by: 2.
	scale voice: MIDIVoice default.
	EventScheduler addClient: (Time microsecondClock + 500000) usec -> scale.
	"self halt."
	EventScheduler run! !!EventScheduler class methodsFor: 'examples'!timerExample
	"Start some timers in the event scheduler."
	"EventScheduler timerExample"
	"EventScheduler interrupt; flush"

	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run.! !!EventScheduler class methodsFor: 'class initialization'!initialize
	"Set up the global Scheduler."
	"EventScheduler initialize "
	"EventScheduler instanceCount "

	ESchedule == nil
		ifFalse: [self release].
	ESchedule := EventScheduler new initialize.! !!EventScheduler class methodsFor: 'class initialization'!release
	"Clear away the global EventScheduler."
	"EventScheduler release"

	EventScheduler instance release.
	ESchedule := nil! !!EventScheduler class methodsFor: 'instance control'!addClient: aT
	"Add a timer to the instance"

	self instance addClient: aT! !!EventScheduler class methodsFor: 'instance control'!addClient: aT at: theTime
	"Add a timer to the instance"

	self instance addClient: aT at: theTime! !!EventScheduler class methodsFor: 'instance control'!addClient: aT in: theTime
	"Add a timer to the instance"

	self instance addClient: aT in: theTime! !!EventScheduler class methodsFor: 'instance control'!addClient: aT in: theTime loop: loopBool
	"Add a timer to the instance"

	self instance addClient: aT in: theTime loop: loopBool! !!EventScheduler class methodsFor: 'instance control'!addTimer: aT
	"Add a timer to the instance"

	self instance addTimer: aT! !!EventScheduler class methodsFor: 'instance control'!addTimer: aT interval: int
	"Add a timer to the instance"

	self instance addTimer: aT interval: int! !!EventScheduler class methodsFor: 'instance control'!clients
	"Get the instance's clients"

	^self instance clients! !!EventScheduler class methodsFor: 'instance control'!clock
	"Answer the instance's clock"

	^self instance clock! !!EventScheduler class methodsFor: 'instance control'!flush
	"Reset all running schedules by brute force."
	"ESchedule interrupt; flush"

	EventScheduler allInstances do:
		[ :thisOne |
		thisOne flush]! !!EventScheduler class methodsFor: 'instance control'!interrupt
	"Stop all running schedules by brute force."
	"Schedule interrupt; flush"

	EventScheduler allInstances do:
		[ :thisOne |
		thisOne interrupt]! !!EventScheduler class methodsFor: 'instance control'!isRunning
	"Ask the instance if it's on"

	^self instance isRunning! !!EventScheduler class methodsFor: 'instance control'!resetClock
	"Answer the instance's clock"

	^self instance resetClock! !!EventScheduler class methodsFor: 'instance control'!run
	"Turn the instance on"

	self instance run! !!EventScheduler class methodsFor: 'instance control'!timers
	"Get the instance's timers"

	^self instance timers! !!EventScheduler class methodsFor: 'instance creation'!default
	"EventScheduler instance"

	ESchedule ifNil: [self initialize].
	^ ESchedule! !!EventScheduler class methodsFor: 'instance creation'!instance
	"EventScheduler instance"

	ESchedule ifNil: [self initialize].
	^ ESchedule! !Model subclass: #PortModel	instanceVariableNames: 'name status device in out'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!PortModel methodsFor: 'initialize-release'!initialize
	"no-op"

	^self! !!PortModel methodsFor: 'initialize-release' stamp: 'stp 1/2/2023 14:52'!open	^self! !!PortModel methodsFor: 'mutex support'!critical: aBlock
	"Execute the given block as a critical section"

	self class mutex critical: aBlock! !!PortModel methodsFor: 'accessing'!device

	^device! !!PortModel methodsFor: 'accessing'!device: aValue

	device := aValue! !!PortModel methodsFor: 'accessing'!in

	^in! !!PortModel methodsFor: 'accessing'!in: aValue

	in := aValue! !!PortModel methodsFor: 'accessing'!name

	^name! !!PortModel methodsFor: 'accessing'!name: aValue

	name := aValue! !!PortModel methodsFor: 'accessing'!out

	^out! !!PortModel methodsFor: 'accessing'!out: aValue

	out := aValue! !!PortModel methodsFor: 'accessing'!status

	^status! !!PortModel methodsFor: 'accessing'!status: aValue

	status := aValue! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PortModel class	instanceVariableNames: 'instance useSingleton devices mutex'!!PortModel class methodsFor: 'instance creation'!concreteClass
	"Answer the class to use for ports"

	^self! !!PortModel class methodsFor: 'instance creation' stamp: 'stp 1/18/2023 07:45'!default	"Answer a new instance, or the singleton"	"MIDIPort default"	"PortModel initialize"	"PortMIDIPort instanceCount"		^ useSingleton		ifTrue: [instance				ifNil: [instance := self concreteClass new].			instance]		ifFalse: [self concreteClass new]! !!PortModel class methodsFor: 'instance creation'!instance
	"Answer a new instance, or the singleton"
	"MIDIPort default"
	"MIDIPort instanceCount"

	^useSingleton
		ifTrue:  [self default]
		ifFalse: [self new]! !!PortModel class methodsFor: 'instance creation'!new
	"Answer a new instance, or the singleton"
	"MIDIPort new"
	"MIDIPort instanceCount"

	useSingleton
		ifTrue:  [instance ifNil: [instance := self concreteClass basicNew initialize]. ^instance]
		ifFalse: [ | inst |
			[inst := self concreteClass basicNew initialize]
				on: Error
				do: [ :ex | Transcript show: 'MIDI driver not installed'.
					^nil].
			^inst]! !!PortModel class methodsFor: 'instance creation'!release
	"Release the unique instance"
	"MIDIPort release"

	instance := nil! !!PortModel class methodsFor: 'class initialization'!initialize
	"Set up the defaults for the class constants."
	"PortModel initialize"

	self initializeData.
	PortModel allSubclassesDo: [ :cl | cl initializeData]! !!PortModel class methodsFor: 'class initialization' stamp: 'stp 1/18/2023 07:47'!initializeData	"Set up the defaults for the class constants."	"PortModel initialize"	instance := nil.	devices := OrderedCollection new.	useSingleton := false.	mutex := Semaphore forMutualExclusion.	self resetDevices! !!PortModel class methodsFor: 'class var accessing' stamp: 'stp 1/5/2023 11:32'!addDevice: aDev 	devices addLast: aDev! !!PortModel class methodsFor: 'class var accessing'!blockSize
	^blockSize! !!PortModel class methodsFor: 'class var accessing'!blockSize: value
	blockSize := value! !!PortModel class methodsFor: 'class var accessing'!devices
	^devices! !!PortModel class methodsFor: 'class var accessing'!devices: anArr
	devices := anArr! !!PortModel class methodsFor: 'class var accessing' stamp: 'stp 1/4/2023 15:48'!inDeviceNamed: nam 	"PortModel inDeviceNamed: '*Faders*'" self halt.	^ devices		detect: [:aD | nam match: aD name]		ifNone: [self error: 'No Port by that name']! !!PortModel class methodsFor: 'class var accessing'!instance: value
	instance := value! !!PortModel class methodsFor: 'class var accessing'!mutex
	^mutex! !!PortModel class methodsFor: 'class var accessing' stamp: 'stp 1/4/2023 15:44'!outDeviceNamed: nam 	^ devices		detect: [:aD | aD name match: nam]		ifNone: [self error: 'No Port by that name']! !!PortModel class methodsFor: 'class var accessing'!properties
	^properties! !!PortModel class methodsFor: 'class var accessing'!properties: anArr
	properties := anArr! !!PortModel class methodsFor: 'class var accessing'!rate
	^rate! !!PortModel class methodsFor: 'class var accessing'!rate: value
	rate := value! !!PortModel class methodsFor: 'class var accessing'!resetDevices
	"Flush the device list"

	devices := OrderedCollection new! !!PortModel class methodsFor: 'class var accessing'!sampleRate
	^rate! !!PortModel class methodsFor: 'class var accessing'!sampleRate: value
	rate := value! !SequenceableCollection subclass: #SEventQueue	instanceVariableNames: 'first last'	classVariableNames: 'CurrentRecord RecordPool'	poolDictionaries: ''	category: 'Music-Support'!!SEventQueue commentStamp: '<historical>' prior: 0!An SEventQueue is a doubly-linked list that has methods for inserting elements in time-sorted order.
These are used for real-time schedules because they can have faster search methods than using SortedCollections of OrderedCollections for the EventScheduler.
The current implementation uses a simple linear search. For really big schedules, this could be made faster with a binary search or tree-based schedule.

Instance Variables:
	first <ScheduleRecord> The first event in the Q
	last <ScheduleRecord> The last event in the Q!!SEventQueue methodsFor: 'accessing'!add: eventAss
	"Add the given eventAssociation to the receiver in the right place."

	^self add: eventAss loop: false! !!SEventQueue methodsFor: 'accessing'!add: eventAss loop: aBool
	"Add the given eventAssociation to the receiver in the right place."

	| key which |
	key := eventAss key usec value.
	which := self locateSlotFor: key.
	which key: key.
	which value: eventAss value.
	which loop: aBool! !!SEventQueue methodsFor: 'accessing'!asOrderedCollection
	"Answer a copy of the receiver's items"

	| list |
	list := OrderedCollection new.
	self do: [ :item | list addLast: item value name].
	^list! !!SEventQueue methodsFor: 'accessing'!detect: aBlock
	"Walk the list looking"

	| item |
	self isEmpty ifTrue: [^nil].
	item := first.
		[(aBlock value: item) ifTrue: [^item]. 
		item := item next.
		item notNil] whileTrue.
	^nil! !!SEventQueue methodsFor: 'accessing'!do: aBlock
	"Refer to the comment in Collection|do:."

	| item |
	self isEmpty ifTrue: [^nil].
	item := first.
		[aBlock value: item. 
		item := item next.
		item notNil] whileTrue.! !!SEventQueue methodsFor: 'accessing'!includes: sought

	^(self detect: [ :item | item == sought]) notNil! !!SEventQueue methodsFor: 'accessing'!isEmpty

	^first isNil! !!SEventQueue methodsFor: 'accessing'!itemNamed: theName
	"Answer a copy of the receiver's items"

	self do: [ :item | (item value name == theName) ifTrue: [^item]].
	^nil! !!SEventQueue methodsFor: 'accessing' stamp: 'stp 1/20/2023 11:04'!ready: now
	"Answer whether there's an event ready within 5000 usec of the given time"

	first ifNil: [^false].
	first start ifNil: [^false].	first start isBehavior ifTrue: [^false]."	Transcript show: (now - first start) printString; cr."
"	^(now - first start) abs < 1000"
	^(now - first start) > 1000! !!SEventQueue methodsFor: 'accessing'!removeAllSuchThat: aBlock 
	"Remove each element for which aBlock evaluates to true. "

	| item prev next |
	self isEmpty ifTrue: [^nil].
	item := first.
	[item isNil]
		whileFalse: [(aBlock value: item)
			ifFalse: [item := item next]
			ifTrue: [prev := item previous.
				prev ifNotNil: [prev next: item next].
				next := item next.
				next ifNotNil: [next previous: prev].
				((item == first) and: [next isNil])
					ifTrue: [first := nil].
				item := next]]! !!SEventQueue methodsFor: 'accessing'!removeFirst
	"Remove and answer the first item from the list."

	| item |
	first ifNil: [last := nil. ^nil].
	item := first.
	first := first next.
	^item! !!SEventQueue methodsFor: 'accessing' stamp: 'stp 1/19/2023 09:55'!size
	"Answer how many elements the receiver contains."

"	| item tally |
"	self isEmpty
		ifTrue: [^0].			^1 					"lie"
"	tally := 0.
	item := first.
	[(item := item next) notNil] whileTrue: [tally := tally + 1].
	^ tally"! !!SEventQueue methodsFor: 'private' stamp: 'stp 1/20/2023 11:08'!locateSlotFor: start
	"Find the proper slot for inserting a new event with the given start time."

	| new item |
	new := self class nextRecord.				"get a record from the class's pool"
	new ifNil: [self error: 'No scheduler slots available'].
	(first isNil or: [first start isBehavior]) ifTrue:
		[first := new.
		last := nil.
		^new].
	first start ifNil: [^first].
	first start >= start							"If it's before the first scheduled event, prepend."
		ifTrue: [first previous: new.
			new next: first.
			last ifNil: [last := first].
			first := new.
			^new].
	(last isNil or: [last start isNil]) ifTrue:		"if it's after the first and only entry"
		[first next: new.
		new previous: first.
		last := new.
		^new].
	last start <= start							"If it's after the last scheduled event, append."
		ifTrue: [last next: new.
			new previous: last.
			last := new.
			^new].
	item := first.								"Else do linear search and then insert."
	[item key < start]
		whileTrue: [item := item next].
	item previous next: new.
	new previous: item previous.
	item previous: new.
	new next: item.
	^new! !!SEventQueue methodsFor: 'printing'!printOn: aStream 
	"Append to the argument aStream a sequence of characters that identifies the receiver."

	aStream print: self class; nextPutAll: ' ('.
	aStream nextPutAll: first printString, ' - ', last printString.
	aStream nextPut: $)! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!SEventQueue class	instanceVariableNames: ''!!SEventQueue class methodsFor: 'record pool' stamp: 'stp 1/18/2023 17:55'!initialize
	"Set up the class's record pool"
	"SEventQueue initialize"

	| poolSize |
	poolSize := 256.
	CurrentRecord := 1.
	RecordPool := Array new: poolSize.
	1 to: poolSize do:
		[ :count |
		RecordPool at: count put: ScheduleRecord new]! !!SEventQueue class methodsFor: 'record pool'!nextRecord
	"Set up the class's record pool"
	"SEventQueue initialize"
	"SEventQueue nextRecord"

	| rec looped |
	looped := false.
		[rec := RecordPool at: CurrentRecord.
		CurrentRecord := (CurrentRecord + 1).
		CurrentRecord >= RecordPool size
			ifTrue: [looped ifTrue: [^nil].
				CurrentRecord := 1.
				looped := true].
		rec key isNil]
	whileFalse.
	^rec! !Association subclass: #ScheduleRecord	instanceVariableNames: 'next previous loop'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!ScheduleRecord commentStamp: 'stp 1/18/2023 17:56' prior: 0!A ScheduleRecord is used as an entry in the SEventQueue. 
It's a doubly-linked record with a key (the start time) and value (the scheduled event).

Instance Variables
	next <ScheduleRecord | nil> the next record in the Q (nil for the last)
	previous <ScheduleRecord | nil> the previous record in the Q (nil for the first)
	loop <Boolean> whether to loop
	key <Integer> (inherited from LookupKey) the record's start time in msec
	value <Event> (inherited from Association) the record's event.!!ScheduleRecord methodsFor: 'accessing'!loop
	^loop! !!ScheduleRecord methodsFor: 'accessing'!loop: aBool
	loop := aBool! !!ScheduleRecord methodsFor: 'accessing'!next
	"Answer the receiver's 'next'."

	^next! !!ScheduleRecord methodsFor: 'accessing'!next: anObject
	"Set the receiver's instance variable 'next' to be anObject."

	next := anObject! !!ScheduleRecord methodsFor: 'accessing'!previous
	"Answer the receiver's 'previous'."

	^previous! !!ScheduleRecord methodsFor: 'accessing'!previous: anObject
	"Set the receiver's instance variable 'previous' to be anObject."

	previous := anObject! !!ScheduleRecord methodsFor: 'accessing'!start
	^self key! !!ScheduleRecord methodsFor: 'control'!clear
	key := nil.
	value := nil.
! !!ScheduleRecord methodsFor: 'control'!stop
	value ifNotNil: [value stop]! !!ScheduleRecord methodsFor: 'testing'!isActive
	^value isActive! !Model subclass: #SirenSession	instanceVariableNames: 'scheduleList scheduleListText verbosity clock startedAt inChannels blockSize sampleRate outChannels interfaceParams transport eventList sound voice timer soundPort midiPort loadedSounds loadedLists oscAddress defaultVoiceClass verboseButton'	classVariableNames: 'EventLists Sounds Voices'	poolDictionaries: ''	category: 'Music-Support'!!SirenSession commentStamp: '<historical>' prior: 0!The class SirenSession represents the user session state of the Siren Framework. The class is also the implementor of the Siren configuration panel, see the interface window spec, and the instance methods.

Instance Variables (used by the pop-up utility window)
	sampleRate - the sample rate
	inChannels outChannels - # of snd I/O channels
	oscIP oscPort - OSC IP & port
	defaultVoiceClass - voice class to use
	blockSize - snd IO block size
	interfaceParams - snd IO properties
	transport - value used by GUI
	eventList - value used by GUI
	sound - value used by GUI
	voice - value used by GUI
	timer - value used by GUI
	scheduleList - value used by GUI
	soundPort - value used by GUI
	midiPort - value used by GUI

Class Variables:
	EventLists <Dictionary> Event lists
	Sounds <Dictionary> Sounds
	Voices <Dictionary> Voices
!!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 10:45'!acceptOSCAddr	"Set the OSC address from the text box."		self inform: 'Not implemented yet.'! !!SirenSession methodsFor: 'actions'!allOff
	"Turn off anything that's playing"

	self stopSound.
	self stopMIDI.
	self resetSound.
	EventScheduler resetClock.! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/26/2022 17:11:59'!cleanUp

	self stopSound.
	self stopMIDI.
	MIDIPort cleanUp.
	self resetSound.
	EventScheduler resetClock.
! !!SirenSession methodsFor: 'actions' stamp: 'stp 1/19/2023 12:58'!configureMIDI	"Run the MIDI config menu & submenus."		| menu inMenu outMenu devices |	menu := MenuMorph new defaultTarget: self.	inMenu := MenuMorph new defaultTarget: self.	outMenu := MenuMorph new defaultTarget: self.	menu addItem: [ :item | item contents: 'Reset MIDI driver'; target: PortMIDILibrary; selector: #resetMIDIDriver ].	menu addItem: [ :item | item contents: 'Dump device table'; target: PortMIDILibrary; selector: #listDeviceTable ].	devices := MIDIPort concreteClass devices select: [:item | item direction == #in].	devices do: [ :dev |		inMenu addItem: [ :item | item contents: dev name; target: SirenUtility; 					selector: #defaultMIDIIn: ; arguments: { dev name } ] ].	devices := MIDIPort concreteClass devices select: [:item | item direction == #out].	devices do: [ :dev |		outMenu addItem: [ :item | item contents: dev name; target: SirenUtility; 					selector: #defaultMIDIOut: ; arguments: { dev name }  ] ].			menu addItem: [ :item | item contents: 'Pick output port'; subMenu: outMenu ].	menu addItem: [ :item | item contents: 'Pick input port'; subMenu: inMenu ].	^ menu invokeModal! !!SirenSession methodsFor: 'actions' stamp: 'stp 1/17/2023 18:33'!configureMIDI0	| sel dev devices ind |	MIDIPort concreteClass devices		ifEmpty: [PortMIDIPort initializeData].	sel := (PopUpMenu labelArray: #('reset MIDI driver' 'dump device table' 'pick input port' 'pick output port') lines: #(1 2)) startUp.	sel = 0		ifTrue: [^ self].	sel = 1		ifTrue: [^ PortMIDILibrary resetMIDIDriver].	sel = 2		ifTrue: [^ PortMIDILibrary listDeviceTable].	sel = 3		ifTrue: [devices := MIDIPort concreteClass devices						select: [:item | item direction == #in]].	sel = 4		ifTrue: [devices := MIDIPort concreteClass devices						select: [:item | item direction == #out]].	ind := (PopUpMenu				labelArray: (devices						collect: [:item | item name])) startUp.	ind = 0		ifTrue: [^ self].	sel = 3		ifTrue: [dev := (devices at: ind) port.			Transcript show: 'Default MIDI input device set to ' , (devices at: ind) name , '(' , dev , ')';				 cr.			SirenUtility defaultMIDIIn: dev].	sel = 4		ifTrue: [dev := (devices at: ind) port.			Transcript show: 'Default MIDI output device set to ' , (devices at: ind) name , '(' , dev , ')';				 cr.			SirenUtility defaultMIDIOut: dev]! !!SirenSession methodsFor: 'actions' stamp: 'stp 1/21/2023 13:15'!freeLists

	(self confirm: 'Really delete all lists, sounds and voices?')		ifFalse: [^self].	SirenSession flushTempEventLists.	SirenSession initializeEventLists.	SirenSession initializeSounds.	SirenSession initializeVoices.
	Smalltalk garbageCollect! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 11:10'!inspectLists	"Inspect the shared data"	(Array		with: EventLists		with: Sounds		with: Voices) explore! !!SirenSession methodsFor: 'actions'!inspectSession

	self inspect! !!SirenSession methodsFor: 'actions'!loadAll
	"SirenSession initialize. SirenSession loadDemoData"
	"EventScheduler interrupt; flush"
	"EventScheduler release"
"Create some timers"
"	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run."

	SirenSession initialize. SirenSession loadDemoData! !!SirenSession methodsFor: 'actions'!openSirenUtility

	SirenSession openUtility! !!SirenSession methodsFor: 'actions' stamp: 'stp 1/11/2023 06:58'!openTools	"Open athe other tools"	| sel |	sel := (PopUpMenu labelArray: #('Open workbook' 'Open transport' 'Open voice editor' 'Explore voices')) startUp.	sel = 0		ifTrue: [^ self].	sel = 1		ifTrue: [^self openWorkbook].	sel = 2		ifTrue: [^self openTransport].	sel = 3		ifTrue: [^VoiceDialog open].	sel = 4		ifTrue: [^SirenSession voices explore].! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 10:46'!openTransport	"Open a transport control"			self inform: 'Not implemented yet.'! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 05:09'!openWorkbookWorkbookWindow open! !!SirenSession methodsFor: 'actions'!resetSound
	"Reset the parameters of the sound port"

	SoundPort initialize.
	blockSize value: SoundPort blockSize.
	inChannels value: SoundPort in.
	outChannels value: SoundPort out.
	sampleRate value: SoundPort sampleRate.! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 10:47'!stopMIDI	"Turn off MIDI"	EventScheduler interrupt.	EventScheduler flush.	[MIDIPort default allNotesOff]		on: Error		do: [:ex | Transcript show: 'MIDI prims not installed';				 cr].	ScorePlayer shutDown: true ! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 10:52'!stopSound	"Stop the sound player if it's active"	"| port |	port := SoundPort defaultOrNil.	port	ifNotNil: [port stop; close]."	AbstractSound sounds		do: [:anS | anS stopGracefully].	SoundPlayer stopPlayingAll.	 SoundPlayer stopPlayerProcess: true! !!SirenSession methodsFor: 'actions' stamp: 'stp 12/29/2022 11:11'!storeLists			self inform: 'Not implemented yet.'! !!SirenSession methodsFor: 'aspects'!blockSize

	^blockSize! !!SirenSession methodsFor: 'aspects'!clock
	^clock! !!SirenSession methodsFor: 'aspects'!defaultVoiceClass

	^defaultVoiceClass! !!SirenSession methodsFor: 'aspects'!eventList
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^eventList isNil
		ifTrue:
			[eventList := nil asValue]
		ifFalse:
			[eventList]! !!SirenSession methodsFor: 'aspects'!inChannels

	^inChannels! !!SirenSession methodsFor: 'aspects'!midiPort
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^midiPort isNil
		ifTrue:
			[midiPort := nil asValue]
		ifFalse:
			[midiPort]! !!SirenSession methodsFor: 'aspects'!oscAddress
	"Answer the formatted OSC IP address + port"

	^oscAddress! !!SirenSession methodsFor: 'aspects' stamp: 'stp 12/26/2022 17:02:50'!oscAddress: value
	"Answer the formatted OSC IP address + port"

	self halt! !!SirenSession methodsFor: 'aspects'!outChannels

	^outChannels! !!SirenSession methodsFor: 'aspects'!sampleRate

	^sampleRate! !!SirenSession methodsFor: 'aspects'!scheduleList
	"Answer the scheduler's list"

	^scheduleList! !!SirenSession methodsFor: 'aspects'!scheduleListText
	"Answer the scheduler's list"

	^scheduleListText! !!SirenSession methodsFor: 'aspects'!scheduleListText0
	"Answer the scheduler's list"

	^SelectionInList new list: (self scheduleList list collect:
		[ :item | | obj |
		obj := self objectFor: item.
		obj ifNotNil: [(obj isActive ifTrue: [' X	'] ifFalse: ['	']), item]])! !!SirenSession methodsFor: 'aspects'!sound

	^sound isNil
		ifTrue:
			[sound := nil asValue]
		ifFalse:
			[sound]! !!SirenSession methodsFor: 'aspects'!soundPort

	^soundPort isNil
		ifTrue:
			[soundPort := nil asValue]
		ifFalse:
			[soundPort]! !!SirenSession methodsFor: 'aspects'!timer
	"Answer the timer"

	^timer isNil
		ifTrue: [timer := nil asValue]
		ifFalse: [timer]! !!SirenSession methodsFor: 'aspects'!updateClock
	"Answer the clock display"

	clock value: ((EventScheduler clock asFloat / 1000000.0) printString: 5).
	self changed: #clock.! !!SirenSession methodsFor: 'aspects'!updateScheduleList
	"Build the transport view's list"

	| list textList |
	list := OrderedCollection new.
	list addAll: EventScheduler timers asOrderedCollection.
	list addAll: EventScheduler clients asOrderedCollection.
	list addAll: loadedSounds.
	list addAll: loadedLists.
	self scheduleList list: list.
	self changed: #scheduleList.
	textList := list collect:
		[ :item | | obj |
		obj := self objectFor: item.
		obj ifNotNil: [(obj isActive ifTrue: [' X	'] ifFalse: ['	']), item]].
	self scheduleListText list: textList.
	self changed: #scheduleListText.! !!SirenSession methodsFor: 'aspects'!updateTimer
	"Answer the timer display"

	timer value: ((Time millisecondClockValue asFloat / 1000.0) printString: 6).
	self changed: #timer.! !!SirenSession methodsFor: 'aspects'!verbosity
	"Answer the verbosity level 0, 1, or 2"

	^verbosity! !!SirenSession methodsFor: 'aspects'!voice

	^voice isNil
		ifTrue: [voice := nil asValue]
		ifFalse: [voice]! !!SirenSession methodsFor: 'initialize release' stamp: 'stp 1/21/2023 13:14'!initialize
	"Set up the defaults values."
	"SirenSession initialize"

	interfaceParams := #(0 0 0 0)	.
	inChannels := interfaceParams first.
	outChannels := (interfaceParams at: 2).
	sampleRate := (interfaceParams at: 3).
	blockSize := 1024.
	defaultVoiceClass := #MIDIVoice.
	transport := nil.
	clock := String new.
	loadedSounds := OrderedCollection new.
	loadedLists := OrderedCollection new.
	verbosity := SirenUtility verbosity.
	oscAddress := (SirenUtility defaultOSCHost printIP, ':', SirenUtility defaultOSCPort printString).
! !!SirenSession methodsFor: 'initialize release' stamp: 'stp 12/29/2022 03:30'!release

	super release.
	(EventScheduler instance dependents includes: self)
		ifTrue: [EventScheduler instance removeDependent: self].
	loadedSounds := OrderedCollection new.
	loadedLists := OrderedCollection new.
	Smalltalk garbageCollect.! !!SirenSession methodsFor: 'actions--change'!chBlockSize
	"Respond to a selection in the block size menu"

	SoundPort blockSize: blockSize value.
"	Transcript show: 'Sound IO block size set to ', SoundPort blockSize printString; cr"! !!SirenSession methodsFor: 'actions--change'!chInChans
	"Respond to a selection in the input channel menu"

	SoundPort in: inChannels value.
"	Transcript show: 'Sound IO # input channels set to ', SoundPort inChannels printString; cr"! !!SirenSession methodsFor: 'actions--change'!chListSelection
	"Respond to a selection in the main list menu"

	transport value: self selectedObject.
"	Transcript show: 'Select: ', transport value printString; cr."! !!SirenSession methodsFor: 'actions--change' stamp: 'stp 12/31/2022 11:27'!chOSCAddr	"Respond to a selection in the OSC address field"	| str addr port toks |	str := oscAddress value.	(str includes: $:)		ifFalse: [^self error: 'Broken OSC address format'].	addr := str copyUpTo: $:.	port := str copyAfter: $:.	addr first isDigit		ifTrue: ["if the IP is an address like '127.0.0.1'"			toks := addr findTokens: '.'.			toks size = 4 ifFalse: [^self error: 'Broken OSC address format'].			addr := SocketAddress						hostAddress: (ByteArray								with: (toks at: 1)								with: (toks at: 2)								with: (toks at: 3)								with: (toks at: 4))]		ifFalse: [addr := NetNameResolver addressesForName: addr].	OSCPort defaultIP: addr.	OSCPort defaultPort: port asNumber.	Transcript show: 'OSC address set to ' , str;		 cr! !!SirenSession methodsFor: 'actions--change'!chOutChans
	"Respond to a selection in the output channel menu"

	SoundPort out value.
"	Transcript show: 'Sound IO # output channels set to ', SoundPort out printString; cr"! !!SirenSession methodsFor: 'actions--change'!chSRate
	"Respond to a selection in the sampleRate menu"

	SoundPort sampleRate: sampleRate value.
"	Transcript show: 'Sound IO sample rate set to ', SoundPort sampleRate printString; cr"! !!SirenSession methodsFor: 'actions--change'!chVerbosity
	"Respond to a selection in the verbosity menu"

	SirenUtility verbosity: verbosity value.
	Transcript show: 'Global verbosity set to ', SirenUtility verbosity printString; cr.
	self changed: #verbosity! !!SirenSession methodsFor: 'actions--change'!chVoiceClass
	"Respond to a selection in the voice class menu"

	Voice setDefault: defaultVoiceClass value.
	Transcript show: 'Default IO voice class set to ', defaultVoiceClass value; cr! !!SirenSession methodsFor: 'actions--change'!newEventList
	"Respond to a selection in the score menu"

	| sco |
	sco := eventList value.
	sco ifNil: [^nil].
	sco isString ifFalse: [sco := sco name].
	sco isEmpty ifTrue: [self halt].
	Transcript show: 'Score ', sco, ' selected'; cr.
	loadedLists add: sco.
	self updateScheduleList! !!SirenSession methodsFor: 'actions--change'!newMIDIPort
	"Respond to a selection in the MIDI port menu"

	Transcript show: 'MIDI port set to ', (MIDIPort devices at: midiPort value) name; cr.! !!SirenSession methodsFor: 'actions--change'!newSound
	"Respond to a selection in the sound menu"

	| snd |
	snd := sound value.
	snd ifNil: [^nil].
	snd isString ifFalse: [snd := snd name].
	snd isEmpty ifTrue: [self error: 'Sound not found'. "snd := snd name"].
	Transcript show: 'Sound ', snd, ' selected'; cr.
	loadedSounds add: snd.
	self updateScheduleList.! !!SirenSession methodsFor: 'actions--change'!newSoundPort
	"Respond to a selection in the sound port menu"

	Transcript show: 'Sound port set to ', 
			(SoundPort concreteClass devices at: soundPort value) name; cr.! !!SirenSession methodsFor: 'actions--change'!newTimer
	"Respond to a selection in the timer menu"

	Transcript show: 'Timer  ', timer value printString, ' selected'; cr.! !!SirenSession methodsFor: 'actions--change'!newVoice
	"Respond to a selection in the sound port menu"

	Transcript show: 'Voice set to ', voice value printString; cr.! !!SirenSession methodsFor: 'accessing'!objectFor: identifier
	"Find the object that corresponds to the given ID"

	| obj |
	identifier ifNil: [^nil].
	(loadedSounds includes: identifier)
		ifTrue: [^SirenSession soundNamed: identifier].
	(loadedLists includes: identifier)
		ifTrue: [^SirenSession eventListNamed: identifier].
	obj := EventScheduler timers itemNamed: identifier.
	obj ifNotNil: [^obj].
	obj := EventScheduler clients itemNamed: identifier.
	obj ifNotNil: [^obj].
	^nil! !!SirenSession methodsFor: 'accessing'!selectedObject
	"Select an item in the list"

	| it obj |
	it := self selectedObjectName.
	it ifNil: [^nil].
	obj := self objectFor: it.
	obj ifNotNil: [^obj].
	^nil! !!SirenSession methodsFor: 'accessing'!selectedObjectName
	"Select an item in the list"

	| si it |
	si := scheduleListText selectionIndexHolder value.
	si = 0 ifTrue: [^nil].
	it := scheduleList list at: si.
	it ifNil: [^nil].
	^it! !!SirenSession methodsFor: 'accessing'!transport
	"it's me"

	^self! !!SirenSession methodsFor: 'accessing' stamp: 'stp 1/22/2023 18:12'!verboseButton: aButton	verboseButton := aButton! !!SirenSession methodsFor: 'resources' stamp: 'stp 12/31/2022 11:39'!addItemsFrom: aDict to: aMenu path: aPath 	"Add items from the given dictionary to a hierarchical menu"	| fils dirs |	aDict isEmpty		ifTrue: [^ PopUpMenu labelArray: #('Empty')].	fils := OrderedCollection new: aDict size.	dirs := OrderedCollection new: aDict size.	aDict		associationsDo: [:ass | 			| key val |			key := ass key.			val := ass value.			(val isKindOf: Dictionary)				ifTrue: ["kludj"					dirs add: key]				ifFalse: [fils add: key]].	dirs isEmpty		ifFalse: [dirs				do: [:sub | 					| child |					"recurse through sub-dicts creating submenus"					aMenu beginSubMenuLabeled: sub.					sub == #/						ifTrue: [child := aPath , sub]						ifFalse: [child := aPath , sub , '/'].					self						addItemsFrom: (aDict at: sub)						to: aMenu						path: child.					aMenu endSubMenu]].	fils		do: [:it | aMenu add: it -> (aPath , it)			"(aDict at: it)"]! !!SirenSession methodsFor: 'resources' stamp: 'stp 12/31/2022 11:41'!listMenu	"Answer the event list menu"	| devs list |	devs := SirenSession eventLists.	devs isEmpty		ifTrue: [^ PopUpMenu labelArray: #('No scores')].	list := OrderedCollection new.	devs do:  [ :dev |		list addLast: dev name].	^  PopUpMenu labelArray: list asArray! !!SirenSession methodsFor: 'resources' stamp: 'stp 12/31/2022 11:42'!soundMenu	"Answer the hierarchical sound menu"	| snds list |	snds := SirenSession sounds.	snds isEmpty		ifTrue: [^ PopUpMenu labelArray: #('No sounds')].	list := OrderedCollection new.	snds		do: [:dev | list addLast: dev name].	^ PopUpMenu labelArray: list asArray! !!SirenSession methodsFor: 'resources' stamp: 'stp 12/31/2022 11:43'!timerMenu	"Answer the timer list menu"	| devs labels |	devs := EventScheduler timers.	devs isEmpty		ifTrue: [^ PopUpMenu labelArray: #('No timers')].	labels := Array new: devs size.	1		to: devs size		do: [:ind | labels at: ind put: (devs at: ind) name].	^ PopUpMenu labelArray: labels! !!SirenSession methodsFor: 'resources'!voiceMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #voiceMenu"

	! !!SirenSession methodsFor: 'actions--menu'!deleteItem
	"Remove an item from the list"

	| it |
	it := scheduleList selection.
	it ifNil: [^self].
	Transcript show: 'Remove ', it; cr.
	(loadedSounds includes: it)			"if snd"
		ifTrue: [loadedSounds remove: it]
		ifFalse: [(loadedLists includes: it)			"if sco"
			ifTrue: [loadedLists remove: it]
			ifFalse: [(EventScheduler timers includes: it)			"if timer"
				ifTrue: [EventScheduler removeTimerNamed: it]
				ifFalse: [(EventScheduler clients includes: it)			"if client"
					ifTrue: [EventScheduler removeClientNamed: it]
					ifFalse: [Transcript show: '		not found '; cr]]]].
	scheduleList list remove: it.
	self changed: #scheduleList! !!SirenSession methodsFor: 'actions--menu'!playItem
	"Remove an item from the list"

	| it obj |
	it := self selectedObjectName.
	it ifNil: [^self].
	obj := nil.
	Transcript show: 'Play ', it; cr.
	(loadedSounds includes: it)
		ifTrue: [obj := SirenSession soundNamed: it]
		ifFalse: [(loadedLists includes: it)
			ifTrue: [obj := SirenSession eventListNamed: it]
			ifFalse: [(EventScheduler timers includes: it)
				ifTrue: [obj := EventScheduler timerNamed: it]
				ifFalse: [(EventScheduler clients includes: it)
					ifTrue: [obj := EventScheduler clientNamed: it]
					ifFalse: [Transcript show: '		not found '; cr]]]].
	obj ifNil: [Transcript show: 'Play: object not found'; cr. ^nil].
	obj play.
	transport value: obj.
	self changed: #transport! !!SirenSession methodsFor: 'actions--menu' stamp: 'stp 12/30/2022 16:45'!setVerbose: aBool	^ OSCVoice verbose: aBool! !!SirenSession methodsFor: 'actions--menu'!spawnItem
	"Remove an item from the list"

	| it obj |
	it := self selectedObjectName.
	it ifNil: [^self].
	obj := nil.
	Transcript show: 'Play ', it; cr.
	(loadedSounds includes: it)
		ifTrue: [obj := SirenSession soundNamed: it]
		ifFalse: [(loadedLists includes: it)
			ifTrue: [obj := SirenSession eventListNamed: it]
			ifFalse: [(EventScheduler timers includes: it)
				ifTrue: [obj := EventScheduler timerNamed: it]
				ifFalse: [(EventScheduler clients includes: it)
					ifTrue: [obj := EventScheduler clientNamed: it]
					ifFalse: [Transcript show: '		not found '; cr]]]].
	obj ifNil: [Transcript show: 'Play: object not found'; cr. ^nil].
	obj open.
	transport value: obj.
	self changed: #transport! !!SirenSession methodsFor: 'actions--menu' stamp: 'stp 1/22/2023 18:26'!toggleVerbose	SirenUtility verbosity > 0		ifTrue: [SirenUtility verbosity: 0.				verboseButton label: 'Be more verbose']		ifFalse: [SirenUtility verbosity: 1.				verboseButton label: 'Be less verbose'].				Transcript show: 'SirenUtility verbosity set to ', SirenUtility verbosity printString; cr! !!SirenSession methodsFor: 'actions--menu'!zeroTimer
	EventScheduler instance resetClock! !!SirenSession methodsFor: 'actions--transport'!forward

	"This stub method was generated by UIDefiner"

	^self! !!SirenSession methodsFor: 'actions--transport'!play
	"Play the selection"

	transport value
		ifNil: [Transcript show: 'Nothing to play'; cr]
		ifNotNil: [^transport value play]! !!SirenSession methodsFor: 'actions--transport'!record

	"This stub method was generated by UIDefiner"

	^self! !!SirenSession methodsFor: 'actions--transport'!rewind

	"This stub method was generated by UIDefiner"

	^self! !!SirenSession methodsFor: 'actions--transport'!stop
	"Stop the selection"

	| obj | 
	transport value ifNotNil: [^transport value stop].
	obj := self selectedObject.
	obj ifNotNil: [^obj stop].
	EventScheduler instance run: false! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 1/19/2023 12:58'!testMIDI	| menu inMenu outMenu |	menu := MenuMorph new defaultTarget: self.	inMenu := MenuMorph new defaultTarget: self.	outMenu := MenuMorph new defaultTarget: self.		inMenu addItem: [ :item | item contents: 'Start'; target: MIDIPort; selector: #testInput ].	inMenu addItem: [ :item | item contents: 'Stop'; target: MIDIPort; selector: #testInputStop ].		outMenu addItem: [ :item | item contents: 'Single note'; target: MIDIPort; selector: #testANote ].	outMenu addItem: [ :item | item contents: 'Scale'; target: MIDIPort; selector: #testAScale ].	outMenu addItem: [ :item | item contents: 'Clouds'; target: MIDIPort; selector: #testACloud ].				menu addItem: [ :item | item contents: 'Test output'; subMenu: outMenu ].	menu addItem: [ :item | item contents: 'Test input'; subMenu: inMenu ].	^ menu invokeModal! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 1/19/2023 06:58'!testMIDI0	| lis sel |	sel := (PopUpMenu labelArray: #('test input' 'test output')) startUp.	sel == 0		ifTrue: [^ self].	sel == 1		ifTrue: [sel := (PopUpMenu labelArray: #('start input' 'stop input') lines: #(1)) startUp.			sel == 0				ifTrue: [^ self].			sel == 1				ifTrue: [^ MIDIPort testInput].			sel == 2				ifTrue: [^ MIDIPort testInputStop]].	sel := (PopUpMenu labelArray: #('single note' 'scale' 'clouds')) startUp.	sel == 0		ifTrue: [^ self].	sel == 1		ifTrue: [^ MIDIPort testANote].	"play a scale"	sel == 2		ifTrue: [^ (EventList				scaleFrom: 48				to: 72				in: 1.5)				playOn: (MIDIVoice defaultOut: 1)].	"play merged clouds"	lis := EventList newAnonymous.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {50 to: 64. 78 to: 80}			ampl: {80 to: 90. 20 to: 50}			density: 8) eventList.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {22 to: 40. 48 to: 50}			ampl: {70 to: 80. 20 to: 60}			density: 9) eventList.	lis addAll: (DynamicCloud			dur: 6 sec			pitch: {72 to: 72. 88 to: 90}			ampl: {40 to: 50. 90 to: 90}			density: 10) eventList.	lis		playOn: (MIDIVoice defaultOut: 0)! !!SirenSession methodsFor: 'actions--tests'!testMIDIin

	MIDIPort testInput! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 1/19/2023 12:58'!testOSCout	| sel |	sel := (PopUpMenu labelArray: #('Scale example' 'CSL strings example' )) startUp.	sel == 0		ifTrue: [^ self].	sel == 1		ifTrue: [^ OSCVoice scaleExample].	^ OSCVoice pluckExample1! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/31/2022 07:07'!testSndFileIn	"Run the default sound file test"	! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/29/2022 10:55'!testSndFiles	"Run the default sound file test""	LibSndFileInterface example1: 'unbelichtet.aiff'"		(LoopedSampledSound fromAIFFFileNamed: './Siren9C/Data/unbelichtet.aiff' mergeIfStereo: false) play! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 1/19/2023 14:24'!testSoundFiles	"Run the default sound file test"	"SirenSession testSoundFiles"		(LoopedSampledSound fromAIFFFileNamed: './Siren9C/Data/unbelichtet.aiff' mergeIfStereo: false) play! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/29/2022 10:45'!testSoundIn	"This stub method was generated by UIDefiner"	self inform: 'Not implemented yet.'! !!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/29/2022 10:43'!testSoundOut
	"Play a sound"

	FMSound marimba play"	PortAudioPort playSweep"! !!SirenSession methodsFor: 'updating'!executeAutoSave

	^self! !!SirenSession methodsFor: 'updating'!update: anAspectSymbol with: aParameter from: aSender
	"Catch an update"

	(anAspectSymbol == #timer)
		ifTrue: [^self updateClock
				"^Transcript show: 'timer update'; cr"].
	(anAspectSymbol == #event)
		ifTrue: [^self updateClock 
				"^Transcript show: 'event update'; cr"].
	(anAspectSymbol == #running)
		ifTrue: [^Transcript show: 'status update: ', aParameter printString; cr].
	(anAspectSymbol == #appointments)
		ifTrue: [^self updateScheduleList 
				"^Transcript show: 'clients update'; cr"].
	Transcript show: 'other update: ', anAspectSymbol printString; cr! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!SirenSession class	instanceVariableNames: 'instance useSingleton'!!SirenSession class methodsFor: 'class initialization' stamp: 'stp 12/28/2022 14:05:29'!flushTempEventLists
	"Flush the class's temporary event list dictionary."
	"SirenSession flushTempEventLists"

	EventLists at: #Temp put: Dictionary new.
	Smalltalk garbageCollect! !!SirenSession class methodsFor: 'class initialization' stamp: 'stp 1/19/2023 15:08'!initialize	"Edit these to taste for your installation."	"SirenSession initialize"	"SirenSession instanceCount"	"SirenSession refresh"		self initializeEventLists.	"Flush cache dictionaries"	self initializeSounds.	self initializeVoices.	self release.	instance := nil.	useSingleton := true.	Smalltalk garbageCollect.	self new! !!SirenSession class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:55:34'!initializeEventLists
	"Flush the class's event list dictionary."
	"SirenSession initializeEventLists"

	EventLists := Dictionary new.
	EventLists at: #Temp put: Dictionary new.
	Smalltalk garbageCollect! !!SirenSession class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:55:45'!initializeSounds
	"Empty the class's sound dictionary."

	Sounds := Dictionary new.
	Smalltalk garbageCollect! !!SirenSession class methodsFor: 'class initialization'!initializeVoices
	"SirenSession initializeVoices"

	Voices := Dictionary new.! !!SirenSession class methodsFor: 'class initialization' stamp: 'stp 12/31/2022 11:11'!update: anAspect with: arguments from: anObject 	"You can put code here to be done before or after a snapshot 
		(i.e., do	you want to store events lists and sounds in the image or not)"	anAspect == #aboutToSnapshot		ifTrue: [instance release			"never store an IO object across snapshots"]! !!SirenSession class methodsFor: 'class var accessing'!atVoice: nam put: vox
	"Add to the class's voice dictionary."

	^Voices at: nam put: vox! !!SirenSession class methodsFor: 'class var accessing'!eventList: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession eventListNamed: 'aa/bb/cc' "

	| sco |
	sco := self dataAt: aName in: EventLists.
	sco ifNotNil: [^sco].
	sco := SirenUtility findFile: (aName asFilename tail).
	sco ifNotNil: [^EventList fromFile: sco].
	^nil! !!SirenSession class methodsFor: 'class var accessing' stamp: 'stp 1/19/2023 15:16'!eventList: aName put: anEventList
	"Store the given event list in the shared dictionary; handle hierarchical names"
	"SirenSession eventList: 'aaa/bbb/ccc' put: EventList new"
	"SirenSession eventListNamed: 'aaa/bbb/ccc'"
	| dict tok |	dict := EventLists.	tok := aName asSymbol.	(aName includes: $/) ifTrue:		[ | ind1 ind2 |		ind1 := 1.		ind2 := aName indexOf: $/.		tok := (aName copyFrom: ind1 to: ind2 - 1) asSymbol.			[ind2 < aName size] whileTrue:			[(dict includesKey: tok) 				ifFalse: [Transcript show: 'Creating dict ', tok; cr.						dict at: tok put: Dictionary new].			dict := dict at: tok.			ind1 := ind2 + 1.			ind2 := aName indexOf: $/ startingAt: ind1.			(ind2 == 0) ifTrue: [ind2 := aName size + 1].			tok := (aName copyFrom: ind1 to: ind2 - 1) asSymbol]].
	dict at: tok put: anEventList.! !!SirenSession class methodsFor: 'class var accessing' stamp: 'stp 1/19/2023 14:16'!eventListNamed: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession eventListNamed: 'aa/bb/cc' "

	| sco |
	sco := self dataAt: aName in: EventLists.
	sco ifNotNil: [^sco].				"no file search for now"
"	sco := SirenUtility findFile: (aName ).
	sco ifNotNil: [^EventList fromFile: sco].
"	^nil! !!SirenSession class methodsFor: 'class var accessing'!eventLists
	"Answer the class's event list dictionary."

	^EventLists! !!SirenSession class methodsFor: 'class var accessing'!schedule
	"Answer the 'global' scheduler."
	"Siren schedule"

	EventScheduler instance! !!SirenSession class methodsFor: 'class var accessing'!sound: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession sound: 'aa/bb/cc' "

	| snd |
	snd := self dataAt: aName in: Sounds.
	snd ifNotNil: [^snd].
	snd := (aName asFilename tail).
	snd := SirenUtility findFile: snd.
	snd ifNotNil: [^Sound fromFile: snd].
	^nil! !!SirenSession class methodsFor: 'class var accessing'!sound: aName put: aSound
	"Store the give n event list in the shared dictionary; handle hierarchical names"
	"SirenSession sound: 'Tests/sawtooth' put: SampledSound sawtooth"
	"SirenSession sound: 'Tests/sawtooth' put: nil"

	^self data: aName in: Sounds put: aSound! !!SirenSession class methodsFor: 'class var accessing'!soundNamed: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession soundNamed: 'aa/bb/cc' "

	| snd |
	snd := self dataAt: aName in: Sounds.
	snd ifNotNil: [^snd].
	snd := (aName asFilename tail).
	snd := SirenUtility findFile: snd.
	snd ifNotNil: [^Sound fromFile: snd].
	^nil! !!SirenSession class methodsFor: 'class var accessing'!sounds
	"Answer the class's sound dictionary."

	^Sounds! !!SirenSession class methodsFor: 'class var accessing'!voice: aName put: vox
	"Add to the class's voice dictionary."

	^self data: aName in: Voices put: vox! !!SirenSession class methodsFor: 'class var accessing'!voiceNamed: aName
	"Add to the class's voice dictionary."

	^self dataAt: aName in: Voices! !!SirenSession class methodsFor: 'class var accessing'!voices
	"Answer the class's voice dictionary."

	^Voices! !!SirenSession class methodsFor: 'resources'!blocksizeMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #blocksizeMenu"

	! !!SirenSession class methodsFor: 'resources'!chMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #chMenu"

	! !!SirenSession class methodsFor: 'resources'!forwardButton
	"UIMaskEditor new openOnClass: self andSelector: #forwardButton"

	! !!SirenSession class methodsFor: 'resources' stamp: 'stp 12/31/2022 11:43'!midiPortMenu	"Answer the midi port menu"	| devs labels |	devs := MIDIPort concreteClass devices.	devs isEmpty		ifTrue: [^ PopUpMenu labelArray: #('No MIDI')].	labels := OrderedCollection new: devs size.	devs		do: [:item | item out == 0				ifTrue: [labels add: item name , '  ->']				ifFalse: [labels add: item name , '  <-']].	^ PopUpMenu labelArray: labels asArray! !!SirenSession class methodsFor: 'resources'!playButton
	"UIMaskEditor new openOnClass: self andSelector: #playButton"

	! !!SirenSession class methodsFor: 'resources'!rateMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #rateMenu"

	! !!SirenSession class methodsFor: 'resources'!recordButton
	"UIMaskEditor new openOnClass: self andSelector: #recordButton"

	! !!SirenSession class methodsFor: 'resources'!rewindButton
	"UIMaskEditor new openOnClass: self andSelector: #rewindButton"

	! !!SirenSession class methodsFor: 'resources' stamp: 'stp 1/23/2023 06:37'!windowColor	^ DisplayListView backgroundColor! !!SirenSession class methodsFor: 'data load/store' stamp: 'stp 12/31/2022 07:19'!data: aName in: theDict put: anItem 	"Store the given item (sound or event list) in the shared dictionary;	
		handle hierarchical names"	"SirenSession eventList: 'aa/bb/cc' put: EventList new"	"SirenSession eventList: 'aa/bb/cc'"	| path dict |	(aName includes: FileDirectory separatorString)		ifFalse: [(theDict includesKey: #Temp)				ifFalse: [theDict at: #Temp put: IdentityDictionary new].			^ (theDict at: #Temp)				at: aName asSymbol				put: anItem].	"If given a hierarchical-style name, descend into the dictionary"	path := aName asFilename components.	dict := theDict.	1		to: path size - 1		do: [:ind | 			| lev |			lev := (path at: ind) asSymbol.			(dict includesKey: lev)				ifFalse: [dict at: lev put: IdentityDictionary new].			dict := dict at: lev].	dict at: path last asSymbol put: anItem! !!SirenSession class methodsFor: 'data load/store' stamp: 'stp 1/21/2023 13:22'!dataAt: aName in: theDict 	"Get the event list at the given (possibly hierarchical) name in the	shared dictionary"	"SirenSession eventList: 'aaa/bbb/ccc'"		| path dict |	(aName includes: $/)		ifFalse: [(theDict includesKey: aName)					ifTrue: [^ theDict at: aName].			(theDict includesKey: #Temp)				ifFalse: [^ nil].			^ (theDict at: #Temp)				at: aName				ifAbsent: [nil]].	path := aName findTokens: '/' .	dict := theDict.	1 to: path size		do: [:ind | 			| lev |			lev := (path at: ind) asSymbol.			(dict includesKey: lev)				ifFalse: [^ nil].			dict := dict at: lev].	^ dict! !!SirenSession class methodsFor: 'data load/store' stamp: 'stp 12/29/2022 11:08'!loadDemoData
	"Load a few scores and sounds for demos"
	"Customize this to load your personal session data."
	"SirenSession initialize."
	"SirenSession loadDemoData"
	"EventScheduler interrupt; flush"
	"EventScheduler release"

	| snd list item |
"Load some sounds"
	list := SirenUtility findFiles: 'aiff'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' AU files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('AU', FileDirectory separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'snd'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' SND files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('SND', FileDirectory separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'aif'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' AIFF files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('AIFF', FileDirectory separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'wav'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' WAV files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('WAV', FileDirectory separatorString, each) asSymbol put: item]]].
"Load some MIDI"
	list := SirenUtility findFiles: 'mid'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' MIDI files?')
	ifTrue: [list do: 
		[ :each | item := EventList fromFile: each.
		item ifNotNil: [self eventList: ('MIDI', FileDirectory separatorString, each) asSymbol put: item]]].
"Load some noteLists"
	list := SirenUtility findFiles: 'ev'.
	(FillInTheBlank confirm: 'Should I load ', list size printString, ' EV files?')
	ifTrue: [list do: 
		[ :each | item := EventList fromFile: each.
		item ifNotNil: [self eventList: ('EV', FileDirectory separatorString, each) asSymbol put: item]]].

"Create some test sounds"
"	snd := SampledSound sineDur: 1 rate: 44100 freq: 80 chans: 1.
	SirenSession sound: 'Tests/sine' put: snd.
	SirenSession sound: 'Tests/silence' put: snd.
	SirenSession sound: 'Tests/pink' put: snd.
	SirenSession sound: 'Sweeps/sine' put: snd.
	SirenSession sound: 'Sweeps/slower' put: snd.
	SirenSession sound: 'Bursts/sine' put: snd.
	SirenSession sound: 'Bursts/impulse' put: snd.
	SirenSession sound: 'Bursts/noise' put: snd.
	SirenSession sound: 'Notes/adsr' put: snd.
	SirenSession sound: 'Notes/fm' put: snd.
"
"Create some voices"
	Voices at: #MIDI put: Dictionary new.
	Voices at: #OSC put: Dictionary new.
	Voices at: #Snd put: Dictionary new.

"Create some timers"
"	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run."! !!SirenSession class methodsFor: 'interface opening' stamp: 'stp 1/13/2023 12:27'!buttonFont	^ (TextStyle named: 'Palatino') fontOfSize: 14! !!SirenSession class methodsFor: 'interface opening' stamp: 'stp 1/13/2023 12:27'!createButton: label action: action 	"SirenSession openUtility"	| button |	button := PluggableButtonMorph				on: instance				getState: nil				action: action.	button label: label.	button font: self buttonFont.	^ button! !!SirenSession class methodsFor: 'interface opening' stamp: 'stp 1/13/2023 12:27'!createButton: label action: action on: victim	"SirenSession openUtility"	| button |	button := PluggableButtonMorph				on: victim				getState: nil				action: action.	button label: label.	button font: self buttonFont.	button hResizing: #spaceFill;		vResizing: #spaceFill.	^ button! !!SirenSession class methodsFor: 'interface opening'!openTransport
	"Open the ApplicationModel's user interface."
	"SirenSession openTransport"

	^self openOn: self new withSpec: #transportWindowSpec! !!SirenSession class methodsFor: 'interface opening' stamp: 'stp 1/23/2023 06:36'!openUtility	"Open the ApplicationModel's user interface."	"SirenSession openUtility"	| topLevel midi snd1 general osc window font labelSize vb |	labelSize := 15.	font := 'ParcPlaceLegacy'.	"BitstreamVeraSans'."	"Preferences standardButtonFont familyName."	topLevel := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 12;				 listCentering: #topLeft;				 listDirection: #leftToRight;				 cellPositioning: #topLeft.	midi := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	snd1 := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	general := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	osc := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	midi		addMorph: (TextMorph				string: '  MIDI/OSC  '				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Configure MIDI' action: #configureMIDI);		addMorphBack: (self createButton: 'Test MIDI I/O' action: #testMIDI);		addMorphBack: (self createButton: 'Test OSC output' action: #testOSCout);		addMorphBack: (self createButton: 'Stop Scheduler' action: #stopMIDI).	snd1		addMorph: (TextMorph				string: '  Sound I/O  '				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Test sound out' action: #testSoundOut);		addMorphBack: (self createButton: 'Test sound in' action: #testSoundIn);		addMorphBack: (self createButton: 'Test sound file' action: #testSoundFiles);		addMorphBack: (self createButton: 'Stop sound' action: #stopSound).	general		addMorph: (TextMorph				string: '  Data  '				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Load all' action: #loadAll);		addMorphBack: (self createButton: 'Store all' action: #storeLists);		addMorphBack: (self createButton: 'Inspect DB' action: #inspectLists);		addMorphBack: (self createButton: 'Flush all' action: #freeLists).	vb := self createButton: 'Be more verbose' action: #toggleVerbose.	instance verboseButton: vb.	osc		addMorph: (TextMorph				string: '  OSC Port'				fontName: font				size: labelSize);		 addMorphBack: ((TextMorph			string: '  ' , instance oscAddress , '  '			fontName: 'BitstreamVeraSans'			size: 14) borderWidth: 1;			 backgroundColor: DisplayListView backgroundColor);		addMorphBack: (self createButton: 'New OSC port' action: #acceptOSCAddr);		addMorphBack: vb;		addMorphBack: (self createButton: 'Open tools' action: #openTools).	topLevel addMorph: midi;		 addMorphBack: snd1;		 addMorphBack: general;		 addMorphBack: osc.	self new.	"initialize singleton"	window := SystemWindow new model: self new.	window setLabel: 'Siren Utility'.	window		addMorph: topLevel		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: 490 @ 150! !!SirenSession class methodsFor: 'interface opening' stamp: 'stp 1/23/2023 06:37'!openUtilityVert	"Open the ApplicationModel's user interface."	"SirenSession openUtilityVert"	| topLevel midi snd1 general osc window font labelSize vb |	labelSize := 16.	font := 'ParcPlaceLegacy'.		"for section labels"	"BitstreamVeraSans'."	"Preferences standardButtonFont familyName."	topLevel := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	midi := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	snd1 := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	general := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	osc := Morph new hResizing: #shrinkWrap;				 vResizing: #spaceFill;				 color: Color transparent;				 layoutPolicy: TableLayout new;				 cellGap: 6;				 listCentering: #topLeft;				 listDirection: #topToBottom;				 cellPositioning: #topLeft.	midi		addMorph: (TextMorph				string: 'MIDI/OSC I/O'				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Configure MIDI' action: #configureMIDI);		addMorphBack: (self createButton: 'Test MIDI I/O' action: #testMIDI);		addMorphBack: (self createButton: 'Test OSC output' action: #testOSCout);		addMorphBack: (self createButton: 'Stop Scheduler' action: #stopMIDI).	snd1		addMorph: (TextMorph				string: 'Sound I/O'				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Test sound out' action: #testSoundOut);		addMorphBack: (self createButton: 'Test sound in' action: #testSoundIn);		addMorphBack: (self createButton: 'Test sound file' action: #testSoundFiles);		addMorphBack: (self createButton: 'Stop sound' action: #stopSound).	general		addMorph: (TextMorph				string: 'Database'				fontName: font				size: labelSize);		addMorphBack: (self createButton: 'Load all' action: #loadAll);		addMorphBack: (self createButton: 'Store all' action: #storeLists);		addMorphBack: (self createButton: 'Inspect DB' action: #inspectLists);		addMorphBack: (self createButton: 'Flush all' action: #freeLists).			vb := self createButton: 'Be more verbose' action: #toggleVerbose.	instance verboseButton: vb.	osc		addMorph: (TextMorph				string: 'OSC Set-up'				fontName: font				size: labelSize);		 addMorphBack: ((TextMorph			string: '  ' , instance oscAddress , '  '			fontName: 'BitstreamVeraSans'			size: 14) borderWidth: 1;			 backgroundColor: DisplayListView backgroundColor);		addMorphBack: (self createButton: 'New OSC port' action: #acceptOSCAddr);		addMorphBack: vb;		addMorphBack: (self createButton: 'Open tools' action: #openTools).	topLevel addMorph: midi;		 addMorphBack: snd1;		 addMorphBack: general;		 addMorphBack: osc.	self new.	"initialize singleton"	window := SystemWindow new model: self new.	window setLabel: 'Siren Utility'.	window		addMorph: topLevel		frame: (0 @ 0 corner: 1 @ 1).	"topLevel extent: 480 @ 160."	"window widgetsColor: Color lightGray."	window openInWorldExtent: 160 @ 530! !!SirenSession class methodsFor: 'instance creation'!instance
	"Answer the 'global' session."
	"SirenSession instance"

	^self new! !!SirenSession class methodsFor: 'instance creation'!new
	"Make sure there's only ever one of me."

	useSingleton ifFalse: [^super new initialize].
	instance ifNil: [instance := super new initialize].
	^instance! !!SirenSession class methodsFor: 'instance creation'!refresh
	"Recreate the instance"
	"SirenSession refresh"

	instance ifNotNil: [instance release].
	useSingleton ifTrue: [instance := super new initialize]! !!SirenSession class methodsFor: 'instance creation'!release
	"Make sure there's only ever one of me."
	"SirenSession release"

	instance ifNotNil: 
		[instance release.
		instance := nil]! !Model subclass: #SirenUtility	instanceVariableNames: ''	classVariableNames: 'DataDir DefaultMIDIIn DefaultMIDIOut DefaultOSCHost DefaultOSCPort LoggingStream ScoreDir SoundDir Verbosity'	poolDictionaries: ''	category: 'Music-Support'!!SirenUtility commentStamp: '<historical>' prior: 0!The class SirenUtility represents the package-level state of the Siren Framework. Its class methods answer a variety of Siren system variables such as default file directories.

Class Variables:
	DataDir <String> the default sound/score directory.
	ScoreDir <String> the score directory.
	SoundDir <String> the sound directory.
!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!SirenUtility class	instanceVariableNames: ''!!SirenUtility class methodsFor: 'class initialization' stamp: 'stp 1/20/2023 11:35'!initialize	"Edit these to taste for your installation."	"SirenUtility initialize"	"SirenSession initialize"	"SirenUtility dataDir"	"SirenSession eventLists"	"See also system-dependent setup in SirenUtility>>playSoundFile"		| sirenDir |	DefaultMIDIIn := 4.		"Tune these to your setup"	DefaultMIDIOut := 10.	DefaultOSCHost := #[127 0 0 1].	DefaultOSCPort := 54321.	PortModel initialize.	Verbosity := 1.				"0 = pretty silent;
								 1 = medium-verbose; 
								 2 = full scheduler and interface logging"	LoggingStream = nil.		"logging output file"								"Look for Siren data and set search paths"	DataDir := #().	sirenDir := FileDirectory default name, '/Siren9C/'.	ScoreDir := OrderedCollection with: (sirenDir , 'Data') asSymbol.	"Scores"	self addDir: 'Scores' to: ScoreDir.	self addDir: 'MID' to: ScoreDir.	SoundDir := OrderedCollection with: (sirenDir , 'Data') asSymbol.	"Sound files"	self addDir: 'Sound' to: SoundDir.	self addDir: 'Samples' to: SoundDir.	DataDir := SoundDir copy.				"Basic data directory"	self addDir: 'Databases/*' to: SoundDir.	"STP-specific"	self addDir: '3-Credo/*' to: SoundDir.	"You can add using wildcards"	DataDir := SoundDir copy.				"Basic data directory"	DataDir addAll: SoundDir.	DataDir := DataDir asSet asOrderedCollection.	SirenSession initialize 					"Session setup"						! !!SirenUtility class methodsFor: 'logging' stamp: 'stp 1/17/2023 18:45'!ifVerbose: aBlock 	"Answer the class's default verbosity, 0/1/2."	"(SirenUtility verbosity > 0)"	"(SirenUtility verbosity > 1)"		Verbosity > 0		ifTrue: [aBlock value]! !!SirenUtility class methodsFor: 'logging' stamp: 'stp 12/26/2022 22:16:06'!log: theMessage
	"Log the given string to the Transcript (or otgher output stream) if the system verbosity is >= the given verbosity level"
	"SirenUtility log: 'hello there' level: 0"

	LoggingStream
		ifNil: [Transcript show: theMessage; cr]
		ifNotNil: [LoggingStream nextPutAll: theMessage; cr].
! !!SirenUtility class methodsFor: 'logging'!log: theMessage level: verbosityInt
	"Log the given string to the Transcript (or otgher output stream) if the system verbosity is >= the given verbosity level"
	"SirenUtility log: 'hello there' level: 2"

	Verbosity > verbosityInt
		ifTrue: [self log: theMessage]! !!SirenUtility class methodsFor: 'logging'!verbosity
	"Answer the class's default verbosity, 0/1/2."
	"(SirenUtility verbosity > 0)"
	"(SirenUtility verbosity > 1)"

	^Verbosity! !!SirenUtility class methodsFor: 'logging'!verbosity: aNum
	"Set the class's default verbosity, 0/1/2."

	Verbosity := aNum! !!SirenUtility class methodsFor: 'class var accessing'!dataDir
	"Answer the class's default data storage directory."

	^DataDir! !!SirenUtility class methodsFor: 'class var accessing'!defaultMIDIIn
	"Answer the class's default MIDI input device."

	^DefaultMIDIIn! !!SirenUtility class methodsFor: 'class var accessing' stamp: 'stp 1/5/2023 11:47'!defaultMIDIIn: num 	"Set the class's default MIDI input device."	DefaultMIDIIn := num.! !!SirenUtility class methodsFor: 'class var accessing'!defaultMIDIOut
	"Answer the class's default MIDI output device."

	^DefaultMIDIOut! !!SirenUtility class methodsFor: 'class var accessing' stamp: 'stp 1/18/2023 12:58'!defaultMIDIOut: ind	"Set the class's default MIDI output device."		| num  |	ind isString		ifTrue: [num := (MIDIPort concreteClass devices detect: [ :d | d name = ind]) port]		ifFalse: [num := ind].	DefaultMIDIOut := num.	"	PortMIDIPort default openOutput: num"! !!SirenUtility class methodsFor: 'class var accessing'!defaultOSCHost
	"Answer the class's default OSC host IP."

	^DefaultOSCHost! !!SirenUtility class methodsFor: 'class var accessing'!defaultOSCHost: ip
	"Set the class's default OSC host IP."

	DefaultOSCHost := ip! !!SirenUtility class methodsFor: 'class var accessing'!defaultOSCPort
	"Answer the class's default OSC port #."

	^DefaultOSCPort! !!SirenUtility class methodsFor: 'class var accessing'!defaultOSCPort: num
	"Set the class's default OSC port #."

	DefaultOSCPort := num! !!SirenUtility class methodsFor: 'class var accessing'!scoreDir
	"Answer the class's default score storage directory."

	^ScoreDir! !!SirenUtility class methodsFor: 'class var accessing'!soundDir
	"Answer the class's default sound storage directory."

	^SoundDir! !!SirenUtility class methodsFor: 'utilities' stamp: 'stp 1/19/2023 10:23'!categoryList
	"Answer the class categories for all of Siren"

	^#( 'Music-Models-Representation'
		'Music-Models-Implementation'
		'Music-Events'
		'Music-EventGenerators'
		'Music-EventModifiers'
		'Music-Functions'
		'Music-PitchClasses'
		'Music-PitchScales'
		'Music-Sound'
		'Music-Support'

		'MusicIO-Voices'
		'MusicIO-MIDI'
		'MusicIO-OSC'
"		'MusicIO-Sound'
		'MusicIO-External'
"
		'MusicUI-DisplayLists'
		'MusicUI-DisplayListViews'
		'MusicUI-Layout'
		'MusicUI-Editors'
		'MusicUI-Functions'
"		'MusicUI-Sound'

		'MusicApps-CSL'
		'MusicApps-Loris'
		'MusicApps-LPC'
		'MusicApps-SHARC'  "
	)! !!SirenUtility class methodsFor: 'utilities' stamp: 'stp 1/17/2023 07:08'!fileoutCategories	"Create the source directory of old-fashioned ST80 file-outs."	"SirenUtility fileoutCategories"	"SirenUtility categoryList"	| fold |	fold := FillInTheBlank request: 'Folder name' initialAnswer: 'Siren9C_Sources_Squeak'.	fold		ifEmpty: [^ self].	(FileDirectory default directoryExists: fold)		ifFalse: [FileDirectory default createDirectory: fold].	Transcript cr; nextPutAll: '------ Storing Siren Source to folder ' , fold.	fold := fold , FileDirectory slash.	self categoryList		do: [ :cat | 			| ent out |			Transcript cr; cr; show: '------' , cat; cr.			ent := fold , cat , '.st'.			(FileDirectory default fileExists: fold)				ifTrue: [FileDirectory default deleteFileNamed: fold].			out := FileStream fileNamed: ent.			SystemOrganization fileOutCategory: cat on: out.			(SystemOrganization classesIn: cat) do: [ :cl | cl removeFromChanges]].	Transcript cr; nextPutAll: 'Done.'.! !!SirenUtility class methodsFor: 'utilities'!playSoundFile: nam
	"Play a sound file using UNIX shell and libsndfile's play program"
	"Edit this to your platform's method, which might use a SoundPort or a simple shell command"
	"Sound playFile: 'a.snd' "

	| fullName |
	fullName := SirenUtility findFile: nam.
	fullName ifNil: [self error: 'File not found'].
	Transcript show: 'Playing snd file ', nam, '...'; cr.
	UnixProcess cshOne: ('/usr/local/bin/sndfile-play ', fullName)! !!SirenUtility class methodsFor: 'example access'!formNamed: key
	"Answer the given item from the global music constants."
	"(SirenUtility formNamed: #altoClef) display"

	^(DisplayListView constants at: #forms) at: key! !!SirenUtility class methodsFor: 'example access'!musicConstants
	"Answer the global music constants (mostly icons)."
	"SirenUtility musicConstants inspect"

	^DisplayListView constants! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 11/25/2022 09:54:37'!addDir: dir to: list
	"Try to locate the requested directory and, if found, add it to the given list"
	"SirenSession initialize"

"	| theDir sDir |
	(dir includes: $*) 
		ifTrue: [ | base |
			base := dir copyUpTo: $*.
			(base last == Filename separator) ifTrue: 
				[base := base copyFrom: 1 to: base size - 1 ].
			theDir := SirenUtility findDir: base tryHard: true.
			theDir ifNotNil: [self addDir: theDir to: list.
				((Filename named: theDir) filesMatching: '*') do: 
					[ :newD | 
					(Filename named: newD) isDirectory
						ifTrue: [self addDir: newD to: list]]]]
		ifFalse: [theDir := SirenUtility findDir: dir tryHard: true.
			theDir ifNil: [^self].
			sDir := theDir asSymbol.
			(list includes: sDir)
				ifFalse: [list add: sDir]]
"! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 12/31/2022 07:24'!createS7: aFullPathName 	"Create an s7 folder and copy any files with the same name into it"	"SirenUtility createS7: (SirenUtility dataDir first, FileDirectory	separatorString, 'testing')"	"SirenUtility nextName: 'testing' type: 'aiff'"	"| strm |
	strm := (SirenUtility nextName: 'testing' type: 'aiff' )	asFilename writeStream.
	strm nextPut: $..
	strm close."	| froot fnam list |	froot := FileStream named: aFullPathName.	fnam := FileStream named: aFullPathName , '.s7'.	fnam exists		ifTrue: [fnam isDirectory				ifTrue: [Transcript show: aFullPathName , ' exists.';						 cr.					^ nil]				ifFalse: [Transcript show: aFullPathName , ' exists as a regular file!!';						 cr.					^ nil]].	list := froot filesMatching: '*'.	fnam makeDirectory.	list		do: [:fil | (FileStream named: fil)				renameTo: aFullPathName , '.s7' , FileDirectory separatorString , (FileDirectory localNameFor: fil)]! !!SirenUtility class methodsFor: 'file support'!findDir: dir
	"Try to locate the requested directory either locally or globally"
	"SirenUtility findDir: 'Data' tryHard: false"
	"SirenUtility findDir: 'Frameworks' tryHard: true"

	^self findDir: dir tryHard: false! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 1/21/2023 12:35'!findDir: dir tryHard: aggressive 	"Try to locate the requested directory either locally or globally"	"SirenUtility findDir: 'Data' tryHard: false"	"SirenUtility findDir: 'Frameworks' tryHard: true"	"SirenUtility findDir: '3-Credo' tryHard: true"		| fsep home |	((FileDirectory default) fileExists: dir)		ifTrue: [ ^ dir].									"if local"	fsep := FileDirectory separatorString.	((FileDirectory default) fileExists: (fsep , dir))		ifTrue: [ ^ fsep , dir].							"if at /X"	home := MacOSShell new getenv: 'HOME'.	((FileDirectory default) fileExists: (home , fsep , dir))		ifTrue: [ ^ home , fsep , dir].					"if in $HOME"	(FileDirectory default fileNamesMatching: '*') do: 			[ :each |									 "if in *"			((FileDirectory default) fileExists: (each , fsep , dir))				ifTrue: [^ each , fsep , dir]].	DataDir do: [ :dDir | | fnd | 						"if in DataDir"			((FileDirectory default directoryNamed: dDir) directoryNamesMatching: '*') do: 					[ :sDir |					 (sDir findString: dir startingAt: 1) ~= 0						ifTrue: [ ^ dDir , fsep , dir]].			(dDir findString: dir asString startingAt: 1) = 0				ifTrue: [fnd := self findFile: dir in: dDir.					fnd isEmpty ifFalse: [ ^ fnd]]				ifFalse: []].	aggressive ifFalse: [^ nil].	(FileDirectory default fileNamesMatching: (home , fsep , '*')) do: 			[ :each |									"if in ~/*"			((FileDirectory default) fileExists: (each , fsep , dir))				ifTrue: [^ each , fsep , dir]].	(FileDirectory default fileNamesMatching: home , fsep , 'Smalltalk' , fsep , '*') do: 			[ :each |									"if in ~/Smalltalk/*"			((FileDirectory default) fileExists: (each , fsep , dir))				ifTrue: [^ each , fsep , dir]].	^ nil! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 12/31/2022 07:43'!findFile: fil 	"Try to locate the requested file in the data directories, being smart	about s7 files"	"SirenUtility findFile: 'b.snd'"	"SirenUtility findFile: '1.1a.aiff'"	| nam ans base s7 |	(FileDirectory default fileExists: fil)		ifTrue: [^FileDirectory default fullNameFor: fil].	DataDir isString		ifTrue: [nam := DataDir , FileDirectory separatorString , fil.			(FileDirectory default fileExists: nam)				ifTrue: [^ nam]]		ifFalse: [DataDir				do: [:dir | 					ans := self findFile: fil in: dir.					ans isEmpty						ifFalse: [^ ans]]].	(fil findString: '.s7' startingAt: 1)			= 0		ifFalse: [^ nil].	(fil includes: $.)		ifTrue: ["if not found, look for an s7 folder"			base := FileDirectory default fullNameFor: fil.			s7 := base , 's7'.			s7 := self findFile: s7.			s7				ifNotNil: [nam := s7 , FileDirectory separatorString , fil.					(FileDirectory default fileExists: nam)						ifTrue: [^ nam]].			base := base copyFrom: 1 to: base size - 1.			"try again removing the next-to-last suffix"			s7 := (FileDirectory default fullNameFor: fil) , 's7'.			s7 := self findFile: s7.			s7				ifNotNil: [nam := s7 , FileDirectory separatorString , fil.					(FileDirectory default fileExists: nam)						ifTrue: [^ nam]]].	^ nil! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 1/20/2023 11:36'!findFile: fil in: theDir 	"Try to locate the requested file in the given directory, being smart	about s7 files"	"SirenUtility findFile: 'b.snd'"	"SirenUtility findFile: '1.1a.aiff'"	"SirenUtility findFile: '1.2a1.aiff'"	| nam ans |	(theDir includes: $*)		ifTrue: [(FileDirectory default fileNamesMatching: theDir)				do: [:dir2 | 					(FileDirectory localNameFor: dir2) = fil						ifTrue: [^ dir2].					 (FileDirectory directoryExists: dir2)						ifTrue: [ans := self findFile: fil in: dir2.							ans isEmpty								ifFalse: [^ ans]]]]		ifFalse: [nam := theDir , FileDirectory separatorString , fil.			(FileDirectory default fileExists: nam)				ifTrue: [^ nam].			(FileDirectory default fileNamesMatching: theDir , FileDirectory separatorString , '*.s7')				do: [:dir2 | 					nam := theDir , FileDirectory separatorString , fil.					(FileDirectory fileOrDirectoryExists: nam)						ifTrue: [^FileDirectory fullPathFor: nam]]].	^ ''! !!SirenUtility class methodsFor: 'file support'!findFiles: ext
	"Answer all of the files in the user's data folders with the given filename extension"
	"SirenUtility findFiles: 'au'"
	"SirenUtility findFiles: 'mid'"

	| coll |
	coll := Set new.
	DataDir do:
		[ :dir | self findFiles: ext in: dir into: coll].
	^coll asOrderedCollection! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 1/21/2023 12:42'!findFiles: ext in: theDir into: coll
	"Add the files with the given name extension to the given collection"
	"SirenUtility findFiles: 'au' into: OrderedCollection new"
	"SirenUtility findFiles: 'mid' into: OrderedCollection new"

	((FileDirectory default directoryNamed: theDir) fileNamesMatching: '*') do:
			[ :dir2 | | dex |
			dex := dir2 copyAfterLast: $..
			dex ifNotNil: [(('*', ext, '*') match: dex)
					ifTrue: [coll add: dir2 asSymbol]]]! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 12/31/2022 07:58'!listS7: aName 	"List the contents of the requested s7 file"	"SirenUtility listS7: '1.2a1'"	| fobj fnam |	fnam := SirenUtility findFile: aName , '.s7'.	fnam isEmpty		ifTrue: [^ #()].	fobj :=FileDirectory on: fnam.	^ fobj fileNamesMatching: '*'! !!SirenUtility class methodsFor: 'file support' stamp: 'stp 12/31/2022 07:59'!nextName: aName type: extension 	"Answer the next free name with the given extension in the given s7	folder "	"SirenUtility createS7: (SirenUtility dataDir first, Filename	separatorString, 'testing')"	"SirenUtility nextName: 'testing' type: 'aiff'"	| fnam base list ind |	fnam := SirenUtility findFile: aName , '.s7'.	fnam isEmpty		ifTrue: [^ nil].	ind := '_1.'.	base := FileDirectory on: fnam.	list := base fileNamesMatching: '*.' , extension.	list isEmpty		ifFalse: [list := list asSortedCollection last.			((list includes: $_)					and: [list includes: $.])				ifTrue: [ind := (list copyFrom: (list lastIndexOf: $_)									+ 1 to: (list lastIndexOf: $.)									- 1) asNumber.					ind := '_' , (ind + 1) printString , '.']].	^ base asString , FileDirectory separatorString , aName , ind , extension! !Model subclass: #Timer	instanceVariableNames: 'startTime accumulator lastUpdate interval running name'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Support'!!Timer methodsFor: 'accessing'!accumulator

	^accumulator! !!Timer methodsFor: 'accessing'!accumulator: aValue

	accumulator := aValue! !!Timer methodsFor: 'accessing'!interval

	^interval! !!Timer methodsFor: 'accessing'!interval: aValue

	interval := aValue! !!Timer methodsFor: 'accessing'!lastUpdate

	^lastUpdate! !!Timer methodsFor: 'accessing'!lastUpdate: aValue

	lastUpdate := aValue! !!Timer methodsFor: 'accessing'!name

	^name! !!Timer methodsFor: 'accessing'!name: aValue

	name := aValue! !!Timer methodsFor: 'accessing'!running

	^running! !!Timer methodsFor: 'accessing'!running: aValue

	running := aValue! !!Timer methodsFor: 'accessing'!startTime

	^startTime! !!Timer methodsFor: 'accessing'!startTime: aValue

	startTime := aValue.
	running := false! !!Timer methodsFor: 'control'!play
	"Start a timer"

	^self start! !!Timer methodsFor: 'control'!reset
	"Start a timer"

	startTime := Time microsecondClock.
	lastUpdate := startTime.
	self changed: #start! !!Timer methodsFor: 'control'!restart
	"Start a timer"

	^self start! !!Timer methodsFor: 'control'!scheduleAt: aTime
	"update a timer"

	running 
		ifFalse: [self error: 'Scheduling a stopped timer'].
	lastUpdate := aTime.
	self changed: #timer with: aTime.
"	Transcript show: '		T ', ((aTime - startTime) / 1000000.0 printString: 3); cr."
	^aTime + (interval * 1000000) rounded! !!Timer methodsFor: 'control'!start
	"Start a timer"

	running := true.
	startTime ifNil: [startTime := Time microsecondClock].
	lastUpdate := startTime.
	self changed: #start! !!Timer methodsFor: 'control'!stop
	"Start a timer"

	running := false.
	lastUpdate := Time microsecondClock.
	EventScheduler instance removerTimer: self
	self changed! !