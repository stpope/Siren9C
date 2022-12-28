!classDefinition: #DeviceModel category: #'Music-Support'!
Model subclass: #DeviceModel
	instanceVariableNames: 'name port stream debug in out rate'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!

!DeviceModel methodsFor: 'accessing'!
debug

	^debug! !

!DeviceModel methodsFor: 'accessing'!
debug: aValue

	debug := aValue! !

!DeviceModel methodsFor: 'accessing'!
in

	^in! !

!DeviceModel methodsFor: 'accessing'!
in: aValue

	in := aValue! !

!DeviceModel methodsFor: 'accessing'!
name

	^name! !

!DeviceModel methodsFor: 'accessing'!
name: aValue

	name := aValue! !

!DeviceModel methodsFor: 'accessing'!
out

	^out! !

!DeviceModel methodsFor: 'accessing'!
out: aValue

	out := aValue! !

!DeviceModel methodsFor: 'accessing'!
port

	^port! !

!DeviceModel methodsFor: 'accessing'!
port: aValue

	port := aValue! !

!DeviceModel methodsFor: 'accessing'!
rate

	^rate! !

!DeviceModel methodsFor: 'accessing'!
rate: aValue

	rate := aValue! !

!DeviceModel methodsFor: 'accessing'!
stream

	^stream! !

!DeviceModel methodsFor: 'accessing'!
stream: aValue

	stream := aValue! !


!DeviceModel methodsFor: 'printing'!
printOn: aStream 
	"Append to the argument aStream a sequence of characters 
	that identifies the collection."

	aStream print: self class; nextPutAll: ' ('.
	name ifNotNil: [aStream nextPutAll: name asString].
	port ifNotNil: [aStream nextPutAll: ' - p ', port printString].
	rate ifNotNil: [aStream nextPutAll: ' - r ', rate printString].
	in ifNotNil: [aStream nextPutAll: ' - ', in printString, ' i '].
	out ifNotNil: [aStream nextPutAll: ' - ', out printString, ' o '].
	aStream nextPut: $)! !


!classDefinition: #EditorModel category: #'Music-Support'!
Model subclass: #EditorModel
	instanceVariableNames: 'selections copyBuffer view'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!

!EditorModel methodsFor: 'accessing'!
copyBuffer
	"Answer the receiver's copybuffer"

	^copyBuffer! !

!EditorModel methodsFor: 'accessing'!
copyBuffer: anObject
	"Set the receiver's copybuffer"

	copyBuffer := anObject! !

!EditorModel methodsFor: 'accessing'!
select: sel
	"Set the receiver's selection."

"	sel == nil ifTrue: [Transcript show: 'clear'; cr]."
	selections := sel.
	self changed: #selection with: sel! !

!EditorModel methodsFor: 'accessing'!
selection
	"Answer the receiver's selection."

	^selections! !

!EditorModel methodsFor: 'accessing'!
view: aView
	"Assign into the receiver's view instVar"

	view := aView! !


!classDefinition: #EventScheduler category: #'Music-Support'!
Model subclass: #EventScheduler
	instanceVariableNames: 'appointments timers threads running doWait startTime delay logTime'
	classVariableNames: 'ESchedule'
	poolDictionaries: ''
	category: 'Music-Support'!
!EventScheduler commentStamp: '<historical>' prior: 0!
An instance of EventScheduler is a simple driver for real-time message-passing among any classes that can return timed event association values.  One uses Schedules by setting up clients which a master scheduler process continuously evaluates for their next timed activations. Clients can return appointments which are sorted into the scheduler's list of (time -> event) associations that is evaluated continuously when running. Changed messages are sent from the accessor messages to simplify EventScheduler browsers and inspectors.

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

See the class examples.!


!EventScheduler methodsFor: 'accessing'!
addClient: theApp
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	appointments add: theApp loop: false.
	self changed: #appointments! !

!EventScheduler methodsFor: 'accessing'!
addClient: theApp at: start
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: start loop: false! !

!EventScheduler methodsFor: 'accessing'!
addClient: theApp at: start loop: aBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	appointments add: (start -> theApp) loop: aBool.
	self changed: #appointments! !

!EventScheduler methodsFor: 'accessing'!
addClient: theApp in: start
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: (Time microsecondClock + start asUsec value) loop: false! !

!EventScheduler methodsFor: 'accessing'!
addClient: theApp in: start loop: loopBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: (Time microsecondClock + start asUsec value) loop: loopBool! !

!EventScheduler methodsFor: 'accessing'!
addClient: theApp loop: aBool
	"Add the argument, 'theApp', to the instance variable 'appointments'."

	^self addClient: theApp at: Time microsecondClock + 1000 loop: false! !

!EventScheduler methodsFor: 'accessing'!
addTimer: start
	"Add the argument, 'theApp', to the instance variable 'timers'."

	^self addTimer: start interval: 1.0! !

!EventScheduler methodsFor: 'accessing'!
addTimer: start interval: int
	"Add the argument, 'theApp', to the instance variable 'timers'."

	| timer |
	timer := Timer new interval: int.
	timer name: ('Timer', (timers size + 1) printString) asSymbol.
	timers add: (timer startTime usec => timer).
	self changed: #timers.
	self run.
	^timer! !

!EventScheduler methodsFor: 'accessing'!
clientNamed: aName
	"Answer a named client"

	^appointments detect: [ :ass | ass value name == aName]! !

!EventScheduler methodsFor: 'accessing'!
clients
	"Get the instance's clients"

	^appointments! !

!EventScheduler methodsFor: 'accessing'!
clock
	"Answer the instance's clock (in usec)"

	startTime ifNil: [startTime := Time microsecondClock].
	^Time microsecondClock - startTime! !

!EventScheduler methodsFor: 'accessing'!
delay
	"Get the instance's delay"

	^delay! !

!EventScheduler methodsFor: 'accessing'!
delay: aTime
	"Set the instance's delay"

	^delay delay: aTime! !

!EventScheduler methodsFor: 'accessing'!
dontWait
	"Specify that the scheduler should NOT do the waiting--i.e., there is a lower level of scheduling going on"
	"ESchedule dontWait"

	doWait := false! !

!EventScheduler methodsFor: 'accessing'!
isRunning
	"Answer the instance variable 'running'."

	^running! !

!EventScheduler methodsFor: 'accessing'!
removeClient: aClient
	"Add the argument, 'theApp', to the instance variable 'timers'."

	appointments removeAllSuchThat: [ :ass | ass value == aClient].
	self changed: #appointments.! !

!EventScheduler methodsFor: 'accessing'!
removeClientNamed: aClient
	"Add the argument, 'theApp', to the instance variable 'timers'."

	appointments removeAllSuchThat: [ :it | it value name = aClient].
	self changed: #appointments.! !

!EventScheduler methodsFor: 'accessing'!
removeTimer: aTimer
	"Add the argument, 'theApp', to the instance variable 'timers'."

	timers removeAllSuchThat: [ :ass | ass value == aTimer].
	self changed: #timers.! !

!EventScheduler methodsFor: 'accessing'!
removeTimerNamed: aTimer
	"Add the argument, 'theApp', to the instance variable 'timers'."

	timers removeAllSuchThat: [ :ass | ass key == aTimer].
	self changed: #timers.! !

!EventScheduler methodsFor: 'accessing'!
resetClock
	"Reset the instances clock (in usec)"
	"ESchedule resetClock"

	startTime := Time microsecondClock
! !

!EventScheduler methodsFor: 'accessing'!
timerNamed: aName
	"Answer a named timer"

	^timers detect: [ :ass | ass value name == aName]! !

!EventScheduler methodsFor: 'accessing'!
timers
	"Answer the instance's timers"

	^timers! !

!EventScheduler methodsFor: 'accessing'!
wait
	"Specify that the scheduler should do the waiting--i.e., Smalltalk-level real-time."
	"ESchedule wait"

	doWait := true! !


!EventScheduler methodsFor: 'updating'!
update: anAspect
	"Check if clients are waiting"

	(appointments isEmpty and: [timers isEmpty])
		ifTrue: [Transcript show: 'Sched stopped'; cr]! !


!EventScheduler methodsFor: 'running'!
callNextAppointment
	"Calls upon the next appointment to be made and then reschedules the next one"

	| entry nextTime now todo |
	now := Time microsecondClock.
	todo := false.
	(((SirenUtility verbosity > 1) 					"if really verbose and <shift>, print time and update log time"
		and: [InputState default shiftDown])
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
	todo ifFalse: [delay wait]! !

!EventScheduler methodsFor: 'running'!
flush
	"Reset the appointment list."
	"EventScheduler flush"

	self interrupt.
	appointments := SEventQueue new.
	timers := SEventQueue new.
	threads := OrderedCollection new.
	SEventQueue initialize! !

!EventScheduler methodsFor: 'running'!
interrupt
	"Stop a running scheduler."

	self run: false.
	threads isEmpty not
		ifTrue: [threads first terminate].
	^self! !

!EventScheduler methodsFor: 'running'!
processEvent: entry at: now
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
	^nextTime! !

!EventScheduler methodsFor: 'running'!
run
	"Set up the first meetings and then run them all till no one wants a meeting anymore."

	| proc |
	running ifTrue: [^nil].
	startTime := Time microsecondClock.
	self run: true.					"set flag"
	threads isEmpty not
		ifTrue: [threads do: [ :pr | pr terminate]].
	threads := OrderedCollection new.			"enforce single-threading for now"
									"LOOP: as long as on, try next appt or timer"
	proc := [[running] whileTrue: [self callNextAppointment]] forkAt: (Processor userBackgroundPriority - 1).
	threads add: proc.
	self changed: #running with: true! !

!EventScheduler methodsFor: 'running' stamp: 'stp 12/26/2022 21:54:08'!
run: theBool
	"Accept the argument, 'theBool', as the instance variable 'running'."

	running := theBool.
	SirenUtility log: 'Sched: ', (theBool ifTrue: ['run'] ifFalse: ['stop']) level: 0.
	threads isEmpty
		ifFalse: [threads first ifNil: [threads removeFirst]
			ifNotNil: [theBool 
				ifTrue: [threads first resume]
				ifFalse: [threads first terminate]]].
	self changed: #running with: theBool! !


!EventScheduler methodsFor: 'initialize release'!
initialize
	"Set up the default Schedule"
	"ESchedule initialize"

	doWait := true.
	running := false.
	appointments := SEventQueue new.
	timers := SEventQueue new.
	threads := OrderedCollection new.
	delay := Delay forMicroseconds: 1000.			"default tick delay"
	logTime := Time microsecondClock.! !

!EventScheduler methodsFor: 'initialize release' stamp: 'Install-Music-Support 11/30/2022 15:51:57'!
release
	"Clean up the schedule."

	delay unschedule.
	running ifTrue: [self run: false].
	threads isEmpty not
		ifTrue: [threads do: [ :pr | pr terminate]].
	self initialize! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'EventScheduler class' category: #'Music-Support'!
EventScheduler class
	instanceVariableNames: ''!

!EventScheduler class methodsFor: 'examples'!
scheduleExample
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
	EventScheduler run! !

!EventScheduler class methodsFor: 'examples'!
scheduleExample2
	"Play MIDI using the event scheduler."
	"EventScheduler scheduleExample2"
	"EventScheduler interrupt; flush"

	| scale |
	scale := EventList scaleFrom: 36 to: 60 in: 2.0.
	scale scale: #duration by: 2.
	scale voice: MIDIVoice default.
	EventScheduler addClient: (Time microsecondClock + 500000) usec -> scale.
	"self halt."
	EventScheduler run! !

!EventScheduler class methodsFor: 'examples'!
timerExample
	"Start some timers in the event scheduler."
	"EventScheduler timerExample"
	"EventScheduler interrupt; flush"

	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run.! !


!EventScheduler class methodsFor: 'class initialization'!
initialize
	"Set up the global Scheduler."
	"EventScheduler initialize "
	"EventScheduler instanceCount "

	ESchedule == nil
		ifFalse: [self release].
	ESchedule := EventScheduler new initialize.! !

!EventScheduler class methodsFor: 'class initialization'!
release
	"Clear away the global EventScheduler."
	"EventScheduler release"

	EventScheduler instance release.
	ESchedule := nil! !


!EventScheduler class methodsFor: 'instance control'!
addClient: aT
	"Add a timer to the instance"

	self instance addClient: aT! !

!EventScheduler class methodsFor: 'instance control'!
addClient: aT at: theTime
	"Add a timer to the instance"

	self instance addClient: aT at: theTime! !

!EventScheduler class methodsFor: 'instance control'!
addClient: aT in: theTime
	"Add a timer to the instance"

	self instance addClient: aT in: theTime! !

!EventScheduler class methodsFor: 'instance control'!
addClient: aT in: theTime loop: loopBool
	"Add a timer to the instance"

	self instance addClient: aT in: theTime loop: loopBool! !

!EventScheduler class methodsFor: 'instance control'!
addTimer: aT
	"Add a timer to the instance"

	self instance addTimer: aT! !

!EventScheduler class methodsFor: 'instance control'!
addTimer: aT interval: int
	"Add a timer to the instance"

	self instance addTimer: aT interval: int! !

!EventScheduler class methodsFor: 'instance control'!
clients
	"Get the instance's clients"

	^self instance clients! !

!EventScheduler class methodsFor: 'instance control'!
clock
	"Answer the instance's clock"

	^self instance clock! !

!EventScheduler class methodsFor: 'instance control'!
flush
	"Reset all running schedules by brute force."
	"ESchedule interrupt; flush"

	EventScheduler allInstances do:
		[ :thisOne |
		thisOne flush]! !

!EventScheduler class methodsFor: 'instance control'!
interrupt
	"Stop all running schedules by brute force."
	"Schedule interrupt; flush"

	EventScheduler allInstances do:
		[ :thisOne |
		thisOne interrupt]! !

!EventScheduler class methodsFor: 'instance control'!
isRunning
	"Ask the instance if it's on"

	^self instance isRunning! !

!EventScheduler class methodsFor: 'instance control'!
resetClock
	"Answer the instance's clock"

	^self instance resetClock! !

!EventScheduler class methodsFor: 'instance control'!
run
	"Turn the instance on"

	self instance run! !

!EventScheduler class methodsFor: 'instance control'!
timers
	"Get the instance's timers"

	^self instance timers! !


!EventScheduler class methodsFor: 'instance creation'!
default
	"EventScheduler instance"

	ESchedule ifNil: [self initialize].
	^ ESchedule! !

!EventScheduler class methodsFor: 'instance creation'!
instance
	"EventScheduler instance"

	ESchedule ifNil: [self initialize].
	^ ESchedule! !


!classDefinition: #PortModel category: #'Music-Support'!
Model subclass: #PortModel
	instanceVariableNames: 'name status device in out'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!

!PortModel methodsFor: 'initialize-release'!
initialize
	"no-op"

	^self! !


!PortModel methodsFor: 'mutex support'!
critical: aBlock
	"Execute the given block as a critical section"

	self class mutex critical: aBlock! !


!PortModel methodsFor: 'accessing'!
device

	^device! !

!PortModel methodsFor: 'accessing'!
device: aValue

	device := aValue! !

!PortModel methodsFor: 'accessing'!
in

	^in! !

!PortModel methodsFor: 'accessing'!
in: aValue

	in := aValue! !

!PortModel methodsFor: 'accessing'!
name

	^name! !

!PortModel methodsFor: 'accessing'!
name: aValue

	name := aValue! !

!PortModel methodsFor: 'accessing'!
out

	^out! !

!PortModel methodsFor: 'accessing'!
out: aValue

	out := aValue! !

!PortModel methodsFor: 'accessing'!
status

	^status! !

!PortModel methodsFor: 'accessing'!
status: aValue

	status := aValue! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'PortModel class' category: #'Music-Support'!
PortModel class
	instanceVariableNames: 'instance useSingleton'!

!PortModel class methodsFor: 'instance creation'!
concreteClass
	"Answer the class to use for ports"

	^self! !

!PortModel class methodsFor: 'instance creation'!
default
	"Answer a new instance, or the singleton"
	"MIDIPort default"
	"PortMIDIPort instanceCount"

	^useSingleton
		ifTrue:  [instance ifNil: [instance := self concreteClass basicNew initialize]. instance]
		ifFalse: [self new]! !

!PortModel class methodsFor: 'instance creation'!
instance
	"Answer a new instance, or the singleton"
	"MIDIPort default"
	"MIDIPort instanceCount"

	^useSingleton
		ifTrue:  [self default]
		ifFalse: [self new]! !

!PortModel class methodsFor: 'instance creation'!
new
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
			^inst]! !

!PortModel class methodsFor: 'instance creation'!
release
	"Release the unique instance"
	"MIDIPort release"

	instance := nil! !


!PortModel class methodsFor: 'class initialization'!
initialize
	"Set up the defaults for the class constants."
	"PortModel initialize"

	self initializeData.
	PortModel allSubclassesDo: [ :cl | cl initializeData]! !

!PortModel class methodsFor: 'class initialization'!
initializeData
	"Set up the defaults for the class constants."
	"PortModel initialize"

	instance := nil.
	devices := OrderedCollection new.
	properties := Array new.
	useSingleton := true.
	mutex := Semaphore forMutualExclusion.

	out := 0.
	in := 0.
	rate := 0.
	blockSize := 0.! !


!PortModel class methodsFor: 'class var accessing'!
blockSize
	^blockSize! !

!PortModel class methodsFor: 'class var accessing'!
blockSize: value
	blockSize := value! !

!PortModel class methodsFor: 'class var accessing'!
devices
	^devices! !

!PortModel class methodsFor: 'class var accessing'!
devices: anArr
	devices := anArr! !

!PortModel class methodsFor: 'class var accessing'!
in
	^in! !

!PortModel class methodsFor: 'class var accessing'!
in: value
	in := value! !

!PortModel class methodsFor: 'class var accessing'!
instance: value
	instance := value! !

!PortModel class methodsFor: 'class var accessing'!
mutex
	^mutex! !

!PortModel class methodsFor: 'class var accessing'!
out
	^out! !

!PortModel class methodsFor: 'class var accessing'!
out: value
	out := value! !

!PortModel class methodsFor: 'class var accessing'!
properties
	^properties! !

!PortModel class methodsFor: 'class var accessing'!
properties: anArr
	properties := anArr! !

!PortModel class methodsFor: 'class var accessing'!
rate
	^rate! !

!PortModel class methodsFor: 'class var accessing'!
rate: value
	rate := value! !

!PortModel class methodsFor: 'class var accessing'!
resetDevices
	"Flush the device list"

	devices := OrderedCollection new! !

!PortModel class methodsFor: 'class var accessing'!
sampleRate
	^rate! !

!PortModel class methodsFor: 'class var accessing'!
sampleRate: value
	rate := value! !


!classDefinition: #SirenSession category: #'Music-Support'!
Model subclass: #SirenSession
	instanceVariableNames: 'scheduleList scheduleListText verbosity clock startedAt inChannels blockSize sampleRate outChannels interfaceParams transport eventList sound voice timer soundPort midiPort loadedSounds loadedLists oscAddress defaultVoiceClass'
	classVariableNames: 'EventLists Sounds Voices'
	poolDictionaries: ''
	category: 'Music-Support'!
!SirenSession commentStamp: '<historical>' prior: 0!
The class SirenSession represents the user session state of the Siren Framework. The class is also the implementor of the Siren configuration panel, see the interface window spec, and the instance methods.

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
!


!SirenSession methodsFor: 'actions'!
allOff
	"Turn off anything that's playing"

	self stopSound.
	self stopMIDI.
	self resetSound.
	EventScheduler resetClock.! !

!SirenSession methodsFor: 'actions' stamp: 'stp 12/26/2022 17:11:59'!
cleanUp

	self stopSound.
	self stopMIDI.
	MIDIPort cleanUp.
	self resetSound.
	EventScheduler resetClock.
! !

!SirenSession methodsFor: 'actions' stamp: 'stp 12/26/2022 17:33:37'!
configureMIDI

	| sel dev devices |
	MIDIPort concreteClass devices ifNil: [MIDIPort testOpenClose].
	sel := (PopUpMenu labelArray: #('pick input port' 'pick output port')) startUp.
	sel = 0 ifTrue: [^self].
	sel = 1 ifTrue: [devices := MIDIPort concreteClass devices select: [ :item | item out = 1]].
	sel = 2 ifTrue: [devices := MIDIPort concreteClass devices select: [ :item | item out = 0]].
	dev := (PopUpMenu labelArray: (devices collect: [ :item | item name])) startUp.
	dev = 0 ifTrue: [^self].
	sel = 1 ifTrue: [Transcript show: 'Default MIDI input device set to ', ((devices at: dev) name); cr.
			dev := (devices at: dev) port.
			SirenUtility defaultMIDIIn: dev.
			MIDIPort concreteClass in: dev].
	sel = 2 ifTrue: [Transcript show: 'Default MIDI output device set to ', ((devices at: dev) name); cr.
			dev := (devices at: dev) port.
			SirenUtility defaultMIDIOut: dev.
			MIDIPort concreteClass out: dev].! !

!SirenSession methodsFor: 'actions' stamp: 'stp 12/26/2022 17:11:21'!
freeLists

	SirenSession flushTempEventLists.
	SirenSession initializeSounds.
	Smalltalk garbageCollect! !

!SirenSession methodsFor: 'actions'!
inspectSession

	self inspect! !

!SirenSession methodsFor: 'actions'!
loadAll
	"SirenSession initialize. SirenSession loadDemoData"
	"EventScheduler interrupt; flush"
	"EventScheduler release"
"Create some timers"
"	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run."

	SirenSession initialize. SirenSession loadDemoData! !

!SirenSession methodsFor: 'actions'!
openSirenUtility

	SirenSession openUtility! !

!SirenSession methodsFor: 'actions'!
openTransport
	"Open a transport control"

	^self class openTransport! !

!SirenSession methodsFor: 'actions'!
resetSound
	"Reset the parameters of the sound port"

	SoundPort initialize.
	blockSize value: SoundPort blockSize.
	inChannels value: SoundPort in.
	outChannels value: SoundPort out.
	sampleRate value: SoundPort sampleRate.! !

!SirenSession methodsFor: 'actions' stamp: 'stp 12/26/2022 22:29:21'!
stopMIDI
	"Turn off MIDI"

	EventScheduler interrupt.
	EventScheduler flush.
	[MIDIPort default allNotesOff]
		on: Error
		do: [ :ex | Transcript show: 'MIDI prims not installed'; cr].! !

!SirenSession methodsFor: 'actions'!
stopSound
	"Stop the sound player if it's active"

	| port |
	port := SoundPort defaultOrNil.
	port ifNotNil: [port stop; close]! !


!SirenSession methodsFor: 'aspects'!
blockSize

	^blockSize! !

!SirenSession methodsFor: 'aspects'!
clock
	^clock! !

!SirenSession methodsFor: 'aspects'!
defaultVoiceClass

	^defaultVoiceClass! !

!SirenSession methodsFor: 'aspects'!
eventList
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^eventList isNil
		ifTrue:
			[eventList := nil asValue]
		ifFalse:
			[eventList]! !

!SirenSession methodsFor: 'aspects'!
inChannels

	^inChannels! !

!SirenSession methodsFor: 'aspects'!
midiPort
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^midiPort isNil
		ifTrue:
			[midiPort := nil asValue]
		ifFalse:
			[midiPort]! !

!SirenSession methodsFor: 'aspects'!
oscAddress
	"Answer the formatted OSC IP address + port"

	^oscAddress! !

!SirenSession methodsFor: 'aspects' stamp: 'stp 12/26/2022 17:02:50'!
oscAddress: value
	"Answer the formatted OSC IP address + port"

	self halt! !

!SirenSession methodsFor: 'aspects'!
outChannels

	^outChannels! !

!SirenSession methodsFor: 'aspects'!
sampleRate

	^sampleRate! !

!SirenSession methodsFor: 'aspects'!
scheduleList
	"Answer the scheduler's list"

	^scheduleList! !

!SirenSession methodsFor: 'aspects'!
scheduleListText
	"Answer the scheduler's list"

	^scheduleListText! !

!SirenSession methodsFor: 'aspects'!
scheduleListText0
	"Answer the scheduler's list"

	^SelectionInList new list: (self scheduleList list collect:
		[ :item | | obj |
		obj := self objectFor: item.
		obj ifNotNil: [(obj isActive ifTrue: [' X	'] ifFalse: ['	']), item]])! !

!SirenSession methodsFor: 'aspects'!
sound

	^sound isNil
		ifTrue:
			[sound := nil asValue]
		ifFalse:
			[sound]! !

!SirenSession methodsFor: 'aspects'!
soundPort

	^soundPort isNil
		ifTrue:
			[soundPort := nil asValue]
		ifFalse:
			[soundPort]! !

!SirenSession methodsFor: 'aspects'!
timer
	"Answer the timer"

	^timer isNil
		ifTrue: [timer := nil asValue]
		ifFalse: [timer]! !

!SirenSession methodsFor: 'aspects'!
updateClock
	"Answer the clock display"

	clock value: ((EventScheduler clock asFloat / 1000000.0) printString: 5).
	self changed: #clock.! !

!SirenSession methodsFor: 'aspects'!
updateScheduleList
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
	self changed: #scheduleListText.! !

!SirenSession methodsFor: 'aspects'!
updateTimer
	"Answer the timer display"

	timer value: ((Time millisecondClockValue asFloat / 1000.0) printString: 6).
	self changed: #timer.! !

!SirenSession methodsFor: 'aspects'!
verbosity
	"Answer the verbosity level 0, 1, or 2"

	^verbosity! !

!SirenSession methodsFor: 'aspects'!
verbosityLabel
	"Answer the verbosity level 0, 1, or 2"

	^'Verbosity: ', verbosity printString! !

!SirenSession methodsFor: 'aspects'!
voice

	^voice isNil
		ifTrue: [voice := nil asValue]
		ifFalse: [voice]! !


!SirenSession methodsFor: 'interface opening'!
postOpenWith: aBuilder 
	"This message is sent by the builder after it has opened a completed window."

	defaultVoiceClass onChangeSend: #chVoiceClass to: self.
	blockSize onChangeSend: #chBlockSize to: self.
	inChannels onChangeSend: #chInChans to: self.
	outChannels onChangeSend: #chOutChans to: self.
	sampleRate onChangeSend: #chSRate to: self.
	oscAddress onChangeSend: #chOSCAddr to: self.
	verbosity onChangeSend: #chVerbosity to: self.
	scheduleListText selectionHolder onChangeSend: #chListSelection to: self.

	self soundPort onChangeSend: #newSoundPort to: self.
	self sound onChangeSend: #newSound to: self.
	self voice onChangeSend: #newVoice to: self.
	self midiPort onChangeSend: #newMIDIPort to: self.
	self eventList onChangeSend: #newEventList to: self.
	self timer onChangeSend: #newTimer to: self.! !


!SirenSession methodsFor: 'initialize release' stamp: 'stp 12/26/2022 12:28:56'!
initialize
	"Set up the defaults values."
	"self open"

"	interfaceParams := SoundPort concreteClass properties.
	interfaceParams isEmpty ifTrue: [interfaceParams := #(0 0 0 0)].  "
	interfaceParams := #(0 0 0 0)	.
	inChannels := interfaceParams first.
	outChannels := (interfaceParams at: 2).
	sampleRate := (interfaceParams at: 3).
	blockSize := 1024.  "SoundPort concreteClass blockSize asValue."
	defaultVoiceClass := #MIDIVoice.
	transport := nil.
"	scheduleList := SelectionInList new.
	scheduleListText := SelectionInList new."
	clock := String new.
	loadedSounds := OrderedCollection new.
	loadedLists := OrderedCollection new.
	verbosity := SirenUtility verbosity.
	oscAddress := (SirenUtility defaultOSCHost printIP, ':', SirenUtility defaultOSCPort printString).

"	(EventScheduler instance dependents includes: self)
		ifFalse: [EventScheduler instance addDependent: self]"! !

!SirenSession methodsFor: 'initialize release'!
release

	super release.
	(EventScheduler instance dependents includes: self)
		ifTrue: [EventScheduler instance removeDependent: self].
	loadedSounds := OrderedCollection new.
	loadedLists := OrderedCollection new.
	ObjectMemory compactingGC.! !


!SirenSession methodsFor: 'actions--change'!
chBlockSize
	"Respond to a selection in the block size menu"

	SoundPort blockSize: blockSize value.
"	Transcript show: 'Sound IO block size set to ', SoundPort blockSize printString; cr"! !

!SirenSession methodsFor: 'actions--change'!
chInChans
	"Respond to a selection in the input channel menu"

	SoundPort in: inChannels value.
"	Transcript show: 'Sound IO # input channels set to ', SoundPort inChannels printString; cr"! !

!SirenSession methodsFor: 'actions--change'!
chListSelection
	"Respond to a selection in the main list menu"

	transport value: self selectedObject.
"	Transcript show: 'Select: ', transport value printString; cr."! !

!SirenSession methodsFor: 'actions--change'!
chOSCAddr
	"Respond to a selection in the OSC address field"

	| str addr port |
	str := oscAddress value.
	(str includes: $:)
		ifFalse: [self error: 'Broken OSC address format'].
	addr := str copyUpTo: $:.
	port := str copyAfter: $:.
	addr first isDigit			"if the IP is an address like '127.0.0.1' "
		ifTrue: [addr := IPSocketAddress stringToBytes: addr]
		ifFalse: [addr := IPSocketAddress hostAddressByName: str].
	OSCPort defaultIP: addr.
	OSCPort defaultPort: port asNumber.
	Transcript show: 'OSC address set to ', str; cr! !

!SirenSession methodsFor: 'actions--change'!
chOutChans
	"Respond to a selection in the output channel menu"

	SoundPort out value.
"	Transcript show: 'Sound IO # output channels set to ', SoundPort out printString; cr"! !

!SirenSession methodsFor: 'actions--change'!
chSRate
	"Respond to a selection in the sampleRate menu"

	SoundPort sampleRate: sampleRate value.
"	Transcript show: 'Sound IO sample rate set to ', SoundPort sampleRate printString; cr"! !

!SirenSession methodsFor: 'actions--change'!
chVerbosity
	"Respond to a selection in the verbosity menu"

	SirenUtility verbosity: verbosity value.
	Transcript show: 'Global verbosity set to ', SirenUtility verbosity printString; cr.
	self changed: #verbosity! !

!SirenSession methodsFor: 'actions--change'!
chVoiceClass
	"Respond to a selection in the voice class menu"

	Voice setDefault: defaultVoiceClass value.
	Transcript show: 'Default IO voice class set to ', defaultVoiceClass value; cr! !

!SirenSession methodsFor: 'actions--change'!
newEventList
	"Respond to a selection in the score menu"

	| sco |
	sco := eventList value.
	sco ifNil: [^nil].
	sco isString ifFalse: [sco := sco name].
	sco isEmpty ifTrue: [self halt].
	Transcript show: 'Score ', sco, ' selected'; cr.
	loadedLists add: sco.
	self updateScheduleList! !

!SirenSession methodsFor: 'actions--change'!
newMIDIPort
	"Respond to a selection in the MIDI port menu"

	Transcript show: 'MIDI port set to ', (MIDIPort devices at: midiPort value) name; cr.! !

!SirenSession methodsFor: 'actions--change'!
newSound
	"Respond to a selection in the sound menu"

	| snd |
	snd := sound value.
	snd ifNil: [^nil].
	snd isString ifFalse: [snd := snd name].
	snd isEmpty ifTrue: [self error: 'Sound not found'. "snd := snd name"].
	Transcript show: 'Sound ', snd, ' selected'; cr.
	loadedSounds add: snd.
	self updateScheduleList.! !

!SirenSession methodsFor: 'actions--change'!
newSoundPort
	"Respond to a selection in the sound port menu"

	Transcript show: 'Sound port set to ', 
			(SoundPort concreteClass devices at: soundPort value) name; cr.! !

!SirenSession methodsFor: 'actions--change'!
newTimer
	"Respond to a selection in the timer menu"

	Transcript show: 'Timer  ', timer value printString, ' selected'; cr.! !

!SirenSession methodsFor: 'actions--change'!
newVoice
	"Respond to a selection in the sound port menu"

	Transcript show: 'Voice set to ', voice value printString; cr.! !


!SirenSession methodsFor: 'accessing'!
objectFor: identifier
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
	^nil! !

!SirenSession methodsFor: 'accessing'!
selectedObject
	"Select an item in the list"

	| it obj |
	it := self selectedObjectName.
	it ifNil: [^nil].
	obj := self objectFor: it.
	obj ifNotNil: [^obj].
	^nil! !

!SirenSession methodsFor: 'accessing'!
selectedObjectName
	"Select an item in the list"

	| si it |
	si := scheduleListText selectionIndexHolder value.
	si = 0 ifTrue: [^nil].
	it := scheduleList list at: si.
	it ifNil: [^nil].
	^it! !

!SirenSession methodsFor: 'accessing'!
transport
	"it's me"

	^self! !


!SirenSession methodsFor: 'resources'!
addItemsFrom: aDict to: aMenu path: aPath
	"Add items from the given dictionary to a hierarchical menu"

	| fils dirs |
	aDict isEmpty
		ifTrue: [^Menu labelArray: #('Empty')].
	fils := OrderedCollection new: aDict size.
	dirs := OrderedCollection new: aDict size.
	aDict associationsDo: 
		[ :ass | | key val |
			key := ass key.
			val := ass value.
			(val isKindOf: Dictionary)			"kludj"
				ifTrue: [dirs add: key]
				ifFalse: [fils add: key]].
	dirs isEmpty
		ifFalse: [dirs do:							"recurse through sub-dicts creating submenus"
			[ :sub | | child |
			aMenu beginSubMenuLabeled: sub. 
			(sub == #/)
				ifTrue: [child := aPath, sub]
				ifFalse: [child := aPath, sub, '/'].
			self addItemsFrom: (aDict at: sub) to: aMenu path: child.
			aMenu endSubMenu]].
	fils do:
		[ :it | aMenu add: (it -> (aPath, it)"(aDict at: it)")].! !

!SirenSession methodsFor: 'resources'!
listMenu
	"Answer the event list menu"

	| devs mb |
	devs := SirenSession eventLists.
	devs isEmpty
		ifTrue: [^Menu labelArray: #('No scores')].
	mb := MenuBuilder new.
	self addItemsFrom: devs to: mb path: ''.
	^mb menuHolder
! !

!SirenSession methodsFor: 'resources'!
soundMenu
	"Answer the hierarchical sound menu"

	| devs mb |
	devs := SirenSession sounds.
	devs isEmpty
		ifTrue: [^Menu labelArray: #('No sounds')].
	mb := MenuBuilder new.
	self addItemsFrom: devs to: mb path: ''.
	^mb menuHolder
! !

!SirenSession methodsFor: 'resources'!
timerMenu
	"Answer the timer list menu"

	| devs labels |
	devs := EventScheduler timers.
	devs isEmpty
		ifTrue: [^Menu labelArray: #('No timers')].
	labels := Array new: devs size.
	1 to: devs size do: 
		[ :ind | labels at: ind put: (devs at: ind) name].
	^Menu labelArray: labels! !

!SirenSession methodsFor: 'resources'!
voiceMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #voiceMenu"

	! !


!SirenSession methodsFor: 'actions--menu'!
deleteItem
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
	self changed: #scheduleList! !

!SirenSession methodsFor: 'actions--menu'!
playItem
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
	self changed: #transport! !

!SirenSession methodsFor: 'actions--menu'!
spawnItem
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
	self changed: #transport! !

!SirenSession methodsFor: 'actions--menu'!
zeroTimer
	EventScheduler instance resetClock! !


!SirenSession methodsFor: 'actions--transport'!
forward

	"This stub method was generated by UIDefiner"

	^self! !

!SirenSession methodsFor: 'actions--transport'!
play
	"Play the selection"

	transport value
		ifNil: [Transcript show: 'Nothing to play'; cr]
		ifNotNil: [^transport value play]! !

!SirenSession methodsFor: 'actions--transport'!
record

	"This stub method was generated by UIDefiner"

	^self! !

!SirenSession methodsFor: 'actions--transport'!
rewind

	"This stub method was generated by UIDefiner"

	^self! !

!SirenSession methodsFor: 'actions--transport'!
stop
	"Stop the selection"

	| obj | 
	transport value ifNotNil: [^transport value stop].
	obj := self selectedObject.
	obj ifNotNil: [^obj stop].
	EventScheduler instance run: false! !


!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/26/2022 17:37:29'!
testMIDI

	| lis sel |
	sel := (PopUpMenu labelArray: #('test input' 'test output')) startUp.
	sel == 0 ifTrue: [^self].
	sel == 1 ifTrue: 
		[sel := (PopUpMenu labelArray: #('start input' 'stop input')) startUp.
		sel == 0 ifTrue: [^self].
		sel == 1 ifTrue: [^MIDIPort testInput].
		sel == 2 ifTrue: [^MIDIPort testInputStop]].
	sel := (PopUpMenu labelArray: #('single note' 'scale' 'dense chaos')) startUp.
	sel == 0 ifTrue: [^self].
	sel == 1 ifTrue: [^MIDIPort testANote].
	sel == 2 ifTrue: [^(EventList scaleFrom: 48 to: 72 in: 1.5) playOn: MIDIVoice default].
	lis := EventList newAnonymous.
	2 timesRepeat: 
		[lis addAll: ((DynamicCloud  dur: 6  
			pitch: #((50 to: 64) (78 to: 80))
			ampl: #((60 to: 80) (90 to: 110))
			voice: #((1) (1))
			density: 8) eventList).
		lis addAll: ((DynamicCloud  dur: 6  
			pitch: #((22 to: 48) (38 to: 40))
			ampl: #((80 to: 100) (60 to: 120))
			voice: #((1) (1))
			density: 9) eventList)].
	lis playOn: MIDIVoice default! !

!SirenSession methodsFor: 'actions--tests'!
testMIDIin

	MIDIPort testInput! !

!SirenSession methodsFor: 'actions--tests'!
testMIDIout

	| lis sel |
	sel := (Menu labelArray: #('single note' 'scale' 'dense chaos')) startUp.
	sel == 0 ifTrue: [^self].
	sel == 1 ifTrue: [^MIDIPort testANote].
	sel == 2 ifTrue: [^(EventList scaleFrom: 48 to: 72 in: 1.5) playOn: MIDIVoice default].
	lis := EventList newAnonymous.
	1 timesRepeat: 
		[lis addAll: ((DynamicCloud  dur: 6  
			pitch: #((50 to: 64) (78 to: 80))
			ampl: #((60 to: 80) (90 to: 110))
			voice: #((1) (1))
			density: 8) eventList) .
		lis addAll: ((DynamicCloud  dur: 6  
			pitch: #((22 to: 48) (38 to: 40))
			ampl: #((80 to: 100) (60 to: 120))
			voice: #((1) (1))
			density: 9) eventList)].
	lis playOn: MIDIVoice default! !

!SirenSession methodsFor: 'actions--tests' stamp: 'stp 12/26/2022 18:03:49'!
testOSCout

	| sel |
	sel := (PopUpMenu labelArray: #('CSL strings example' 'scale example')) startUp.
	sel == 0 ifTrue: [^self].
	sel == 1 ifTrue: [^OSCVoice pluckExample1].
		^OSCVoice scaleExample! !

!SirenSession methodsFor: 'actions--tests'!
testSndFileIn
	"Run the default sound file test"

	LibSndFileInterface example1: 'unbelichtet.aiff'! !

!SirenSession methodsFor: 'actions--tests'!
testSoundIn
	"This stub method was generated by UIDefiner"

	^self! !

!SirenSession methodsFor: 'actions--tests'!
testSoundOut
	"Play a sound"

	PortAudioPort playSweep! !


!SirenSession methodsFor: 'updating'!
executeAutoSave

	^self! !

!SirenSession methodsFor: 'updating'!
update: anAspectSymbol with: aParameter from: aSender
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
	Transcript show: 'other update: ', anAspectSymbol printString; cr! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SirenSession class' category: #'Music-Support'!
SirenSession class
	instanceVariableNames: 'instance useSingleton'!

!SirenSession class methodsFor: 'class initialization'!
flushTempEventLists
	"Flush the class's temporary event list dictionary."
	"SirenSession flushTempEventLists"

	EventLists at: #Temp put: Dictionary new.
	ObjectMemory compactingGC! !

!SirenSession class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:55:57'!
initialize
	"Edit these to taste for your installation."
	"SirenSession initialize"
	"SirenSession instanceCount"
	"SirenSession refresh"

	self initializeEventLists.						"Flush cache dictionaries"
	self initializeSounds.
	self initializeVoices.
	self release.
	instance := nil.
	useSingleton := true.
	Smalltalk garbageCollect! !

!SirenSession class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:55:34'!
initializeEventLists
	"Flush the class's event list dictionary."
	"SirenSession initializeEventLists"

	EventLists := Dictionary new.
	EventLists at: #Temp put: Dictionary new.
	Smalltalk garbageCollect! !

!SirenSession class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:55:45'!
initializeSounds
	"Empty the class's sound dictionary."

	Sounds := Dictionary new.
	Smalltalk garbageCollect! !

!SirenSession class methodsFor: 'class initialization'!
initializeVoices
	"SirenSession initializeVoices"

	Voices := Dictionary new.! !

!SirenSession class methodsFor: 'class initialization'!
update: anAspect with: arguments from: anObject
	"You can put code here to be done before or after a snapshot 
		(i.e., do you want to store events lists and sounds in the image or not)"

	anAspect == #aboutToSnapshot
		ifTrue: [CSL.IO release]		"never store an IO object across snapshots"! !


!SirenSession class methodsFor: 'class var accessing'!
atVoice: nam put: vox
	"Add to the class's voice dictionary."

	^Voices at: nam put: vox! !

!SirenSession class methodsFor: 'class var accessing'!
eventList: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession eventListNamed: 'aa/bb/cc' "

	| sco |
	sco := self dataAt: aName in: EventLists.
	sco ifNotNil: [^sco].
	sco := SirenUtility findFile: (aName asFilename tail).
	sco ifNotNil: [^EventList fromFile: sco].
	^nil! !

!SirenSession class methodsFor: 'class var accessing'!
eventList: aName put: anEventList
	"Store the give n event list in the shared dictionary; handle hierarchical names"
	"SirenSession eventList: 'aa/bb/cc' put: EventList new"
	"SirenSession eventListNamed: 'aa/bb/cc'"

	^self data: aName in: EventLists put: anEventList! !

!SirenSession class methodsFor: 'class var accessing'!
eventListNamed: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession eventListNamed: 'aa/bb/cc' "

	| sco |
	sco := self dataAt: aName in: EventLists.
	sco ifNotNil: [^sco].
	sco := SirenUtility findFile: (aName asFilename tail).
	sco ifNotNil: [^EventList fromFile: sco].
	^nil! !

!SirenSession class methodsFor: 'class var accessing'!
eventLists
	"Answer the class's event list dictionary."

	^EventLists! !

!SirenSession class methodsFor: 'class var accessing'!
schedule
	"Answer the 'global' scheduler."
	"Siren schedule"

	EventScheduler instance! !

!SirenSession class methodsFor: 'class var accessing'!
sound: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession sound: 'aa/bb/cc' "

	| snd |
	snd := self dataAt: aName in: Sounds.
	snd ifNotNil: [^snd].
	snd := (aName asFilename tail).
	snd := SirenUtility findFile: snd.
	snd ifNotNil: [^Sound fromFile: snd].
	^nil! !

!SirenSession class methodsFor: 'class var accessing'!
sound: aName put: aSound
	"Store the give n event list in the shared dictionary; handle hierarchical names"
	"SirenSession sound: 'Tests/sawtooth' put: SampledSound sawtooth"
	"SirenSession sound: 'Tests/sawtooth' put: nil"

	^self data: aName in: Sounds put: aSound! !

!SirenSession class methodsFor: 'class var accessing'!
soundNamed: aName
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession soundNamed: 'aa/bb/cc' "

	| snd |
	snd := self dataAt: aName in: Sounds.
	snd ifNotNil: [^snd].
	snd := (aName asFilename tail).
	snd := SirenUtility findFile: snd.
	snd ifNotNil: [^Sound fromFile: snd].
	^nil! !

!SirenSession class methodsFor: 'class var accessing'!
sounds
	"Answer the class's sound dictionary."

	^Sounds! !

!SirenSession class methodsFor: 'class var accessing'!
voice: aName put: vox
	"Add to the class's voice dictionary."

	^self data: aName in: Voices put: vox! !

!SirenSession class methodsFor: 'class var accessing'!
voiceNamed: aName
	"Add to the class's voice dictionary."

	^self dataAt: aName in: Voices! !

!SirenSession class methodsFor: 'class var accessing'!
voices
	"Answer the class's voice dictionary."

	^Voices! !


!SirenSession class methodsFor: 'resources'!
blocksizeMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #blocksizeMenu"

	! !

!SirenSession class methodsFor: 'resources'!
chMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #chMenu"

	! !

!SirenSession class methodsFor: 'resources'!
clockMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #clockMenu"

	! !

!SirenSession class methodsFor: 'resources'!
forwardButton
	"UIMaskEditor new openOnClass: self andSelector: #forwardButton"

	! !

!SirenSession class methodsFor: 'resources'!
midiPortMenu
	"Answer the midi port menu"

	| devs labels |
	devs := MIDIPort concreteClass devices.
	devs isEmpty
		ifTrue: [^Menu labelArray: #('No MIDI')].
	labels := OrderedCollection new: devs size.
	devs do: 
		[ :item | (item out == 0)
			ifTrue: [labels add: item name, '  ->']
			ifFalse: [labels add: item name, '  <-']].
	^Menu labelArray: labels asArray! !

!SirenSession class methodsFor: 'resources'!
playButton
	"UIMaskEditor new openOnClass: self andSelector: #playButton"

	! !

!SirenSession class methodsFor: 'resources'!
rateMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #rateMenu"

	! !

!SirenSession class methodsFor: 'resources'!
recordButton
	"UIMaskEditor new openOnClass: self andSelector: #recordButton"

	! !

!SirenSession class methodsFor: 'resources'!
rewindButton
	"UIMaskEditor new openOnClass: self andSelector: #rewindButton"

	! !

!SirenSession class methodsFor: 'resources'!
scheduleMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #scheduleMenu"

	! !

!SirenSession class methodsFor: 'resources'!
soundPortMenu
	"Answer the sound port menu"

	| devs labels |
	devs := SoundPort concreteClass devices.
	devs isEmpty
		ifTrue: [^Menu labelArray: #('No sound IO')].
	labels := Array new: devs size.
	1 to: devs size do: 
		[ :ind | labels at: ind put: (devs at: ind) name].
	^Menu labelArray: labels! !

!SirenSession class methodsFor: 'resources'!
stopButton
	"UIMaskEditor new openOnClass: self andSelector: #stopButton"

	! !

!SirenSession class methodsFor: 'resources'!
timerMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #timerMenu"

	! !

!SirenSession class methodsFor: 'resources'!
verbosityMenu
	"Tools.MenuEditor new openOnClass: self andSelector: #verbosityMenu"

	! !

!SirenSession class methodsFor: 'resources' stamp: 'stp 12/26/2022 12:26:31'!
windowColor
	^ Theme current object! !


!SirenSession class methodsFor: 'data load/store'!
data: aName in: theDict put: anItem
	"Store the given item (sound or event list) in the shared dictionary; 
		handle hierarchical names"
	"SirenSession eventList: 'aa/bb/cc' put: EventList new"
	"SirenSession eventList: 'aa/bb/cc'"

	| path dict |
	(aName includes: Filename separator)
		ifFalse: [(theDict includesKey: #Temp)
				ifFalse: [theDict at: #Temp put: IdentityDictionary new].
			^(theDict at: #Temp) at: aName asSymbol put: anItem].
"If given a hierarchical-style name, descend into the dictionary"
	path := aName asFilename components.
	dict := theDict.
	1 to: path size - 1 do:
		[ :ind | | lev |
		lev := (path at: ind) asSymbol.
		(dict includesKey: lev)
			ifFalse: [dict at: lev put: IdentityDictionary new].
		dict := dict at: lev].
	dict at: path last asSymbol put: anItem.! !

!SirenSession class methodsFor: 'data load/store'!
dataAt: aName in: theDict
	"Get the event list at the given (possibly hierarchical) name in the shared dictionary"
	"SirenSession eventList: 'aa/bb/cc' "

	| path dict |
	(aName includes: Filename separator)
		ifFalse: [(theDict includesKey: #Temp)
				ifFalse: [^nil].
			^(theDict at: #Temp) at: aName ifAbsent: [nil]].
	path := aName asFilename components.
	dict := theDict.
	1 to: path size do:
		[ :ind | | lev |
		lev := (path at: ind) asSymbol.
		(dict includesKey: lev)
			ifFalse: [^nil].
		dict := dict at: lev].
	^dict! !

!SirenSession class methodsFor: 'data load/store'!
loadDemoData
	"Load a few scores and sounds for demos"
	"Customize this to load your personal session data."
	"SirenSession initialize."
	"SirenSession loadDemoData"
	"EventScheduler interrupt; flush"
	"EventScheduler release"

	| snd list item |
"Load some sounds"
	list := SirenUtility findFiles: 'au'.
	(Dialog confirm: 'Should I load ', list size printString, ' AU files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('AU', Filename separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'snd'.
	(Dialog confirm: 'Should I load ', list size printString, ' SND files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('SND', Filename separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'aif'.
	(Dialog confirm: 'Should I load ', list size printString, ' AIFF files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('AIFF', Filename separatorString, each) asSymbol put: item]]].
	list := SirenUtility findFiles: 'wav'.
	(Dialog confirm: 'Should I load ', list size printString, ' WAV files?')
	ifTrue: [list do: 
		[ :each | item := SampledSound fromFile: each.
		item ifNotNil: [self sound: ('WAV', Filename separatorString, each) asSymbol put: item]]].
"Load some MIDI"
	list := SirenUtility findFiles: 'mid'.
	(Dialog confirm: 'Should I load ', list size printString, ' MIDI files?')
	ifTrue: [list do: 
		[ :each | item := EventList fromFile: each.
		item ifNotNil: [self eventList: ('MIDI', Filename separatorString, each) asSymbol put: item]]].
"Load some noteLists"
	list := SirenUtility findFiles: 'ev'.
	(Dialog confirm: 'Should I load ', list size printString, ' EV files?')
	ifTrue: [list do: 
		[ :each | item := EventList fromFile: each.
		item ifNotNil: [self eventList: ('EV', Filename separatorString, each) asSymbol put: item]]].

"Create some test sounds"
	snd := SampledSound sineDur: 1 rate: 44100 freq: 80 chans: 1.
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

"Create some voices"
	Voices at: #MIDI put: Dictionary new.
	Voices at: #OSC put: Dictionary new.
	Voices at: #Snd put: Dictionary new.

"Create some timers"
"	EventScheduler addTimer: 0.0 interval: 2.0.
	EventScheduler addTimer: 1.0 interval: 2.0.
	EventScheduler  run."! !


!SirenSession class methodsFor: 'interface opening'!
open
	"Open the ApplicationModel's user interface."
	"SirenSession open"

	^self openOn: self new withSpec: #utilWindowSpec! !

!SirenSession class methodsFor: 'interface opening'!
openTransport
	"Open the ApplicationModel's user interface."
	"SirenSession openTransport"

	^self openOn: self new withSpec: #transportWindowSpec! !

!SirenSession class methodsFor: 'interface opening' stamp: 'stp 12/26/2022 22:11:48'!
openUtility
	"Open the ApplicationModel's user interface."
	"SirenSession openUtility"

	| topLevel midi snd1 general window font |
	topLevel _ LayoutMorph newRow.
	midi _ LayoutMorph newColumn borderWidth: 8.
	snd1 _ LayoutMorph newColumn borderWidth: 8.
	general _ LayoutMorph newColumn borderWidth: 8.

	font _ FontFamily familyName: 'Parc Place Legacy' pointSize: 12.
	midi addMorph: (LabelMorph contents: '    MIDI/OSC  ' font: font emphasis: AbstractFont boldCode) 
				proportionalHeight: 0.2;
		addMorph: self createConfButton proportionalHeight: 0.2;
		addMorph: self createTestMButton proportionalHeight: 0.2;
		addMorph: self createTestOSCButton proportionalHeight: 0.2;
		addMorph: self createStopMButton proportionalHeight: 0.2.
	snd1 addMorph: (LabelMorph contents: '    Sound I/O  ' font: font emphasis: AbstractFont boldCode) 
				proportionalHeight: 0.2;
		addMorph: self createTestSoutButton proportionalHeight: 0.2;
		addMorph: self createTestSinButton proportionalHeight: 0.2;
		addMorph: self createTestSiFileButton proportionalHeight: 0.2;
		addMorph: self createStopSButton proportionalHeight: 0.2.
	general addMorph: (LabelMorph contents: '    Data  ' font: font emphasis: AbstractFont boldCode) 
				proportionalHeight: 0.25;
		addMorph: (LabelMorph contents: '          OSC Port' ) proportionalHeight: 0.15;
		addMorph: ((TextModelMorph textProvider: instance
				textGetter: #oscAddress textSetter: #oscAddress:) acceptOnCR: true; 
					alwaysHideVerticalScrollbar) proportionalHeight: 0.12;
	addMorph: self createLoadButton proportionalHeight: 0.16;
	addMorph: self createStoreButton proportionalHeight: 0.16;
	addMorph: self createFlushButton proportionalHeight: 0.16.

	topLevel addMorph: midi proportionalWidth: 0.33;
		addMorph: snd1 proportionalWidth: 0.33;
		addMorph: general proportionalWidth: 0.34.

	self new.				"initialize singleton"
	window _ SystemWindow new model: self new.
	window setLabel: 'Siren Utility'.
	window layoutMorph addMorph: topLevel.
	topLevel morphExtent: 500@100.
	window morphExtent: 500@100.
"	window widgetsColor: Color lightGray."
	UISupervisor runningWorld addMorph: window
! !

!SirenSession class methodsFor: 'interface opening' stamp: 'stp 12/26/2022 11:57:19'!
openUtilityOld
	"Open the ApplicationModel's user interface."
	"SirenSession openUtility"

	^self openOn: self new withSpec: #utilWindowSpec! !


!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 22:07:57'!
createConfButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #configureMIDI.
	button 
		label: 'Configure MIDI';
		setBalloonText: 'Configure MIDI I/O ports'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:13:08'!
createFlushButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #freeLists.
	button 
		label: 'Flush all';
		setBalloonText: 'Flush all persistent event lists'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:13:30'!
createLoadButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #loadAll.
	button 
		label: 'Load all';
		setBalloonText: 'Load all stored event lists'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 21:56:07'!
createStopButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #stopMIDI.
	button 
		label: 'Stop Scheduler';
		setBalloonText: 'Stop the MIDI/OSC output and scheduler'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 22:08:23'!
createStopMButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #stopMIDI.
	button 
		label: 'Stop Scheduler';
		setBalloonText: 'Stop the MIDI/OSC output and scheduler'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 16:59:44'!
createStopSButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #stopSound.
	button 
		label: 'Stop sound';
		setBalloonText: 'Stop the sound output'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:12:58'!
createStoreButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #storeLists.
	button 
		label: 'Store all';
		setBalloonText: 'Store all persistent event lists'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 16:59:49'!
createTestMButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #testMIDI.
	button 
		label: 'Test MIDI I/O';
		setBalloonText: 'Test the Midi I/O ports'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:38:53'!
createTestOSCButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #testOSCout.
	button 
		label: 'Test OSC output';
		setBalloonText: 'Test the OSC output'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 16:59:59'!
createTestSiFileButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #testSoundFiles.
	button 
		label: 'Test sound file';
		setBalloonText: 'Test the sound file playback'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:00:04'!
createTestSinButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #testSoundIn.
	button 
		label: 'Test sound in';
		setBalloonText: 'Test the sound input'.
	^button ! !

!SirenSession class methodsFor: 'interface specs' stamp: 'stp 12/26/2022 17:00:12'!
createTestSoutButton

	| button |
	button := PluggableButtonMorph 
		model: instance
		action: #testSoundOut.
	button 
		label: 'Test sound out';
		setBalloonText: 'Test the sound output'.
	^button ! !


!SirenSession class methodsFor: 'instance creation'!
instance
	"Answer the 'global' session."
	"SirenSession instance"

	^self new! !

!SirenSession class methodsFor: 'instance creation'!
new
	"Make sure there's only ever one of me."

	useSingleton ifFalse: [^super new initialize].
	instance ifNil: [instance := super new initialize].
	^instance! !

!SirenSession class methodsFor: 'instance creation'!
refresh
	"Recreate the instance"
	"SirenSession refresh"

	instance ifNotNil: [instance release].
	useSingleton ifTrue: [instance := super new initialize]! !

!SirenSession class methodsFor: 'instance creation'!
release
	"Make sure there's only ever one of me."
	"SirenSession release"

	instance ifNotNil: 
		[instance release.
		instance := nil]! !


!classDefinition: #SirenUtility category: #'Music-Support'!
Model subclass: #SirenUtility
	instanceVariableNames: ''
	classVariableNames: 'DataDir DefaultMIDIIn DefaultMIDIOut DefaultOSCHost DefaultOSCPort LoggingStream ScoreDir SoundDir Verbosity'
	poolDictionaries: ''
	category: 'Music-Support'!
!SirenUtility commentStamp: '<historical>' prior: 0!
The class SirenUtility represents the package-level state of the Siren Framework. Its class methods answer a variety of Siren system variables such as default file directories.

Class Variables:
	DataDir <String> the default sound/score directory.
	ScoreDir <String> the score directory.
	SoundDir <String> the sound directory.
!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SirenUtility class' category: #'Music-Support'!
SirenUtility class
	instanceVariableNames: ''!

!SirenUtility class methodsFor: 'class initialization' stamp: 'stp 11/25/2022 09:54:01'!
initialize
	"Edit these to taste for your installation."
	"SirenUtility initialize"
	"SirenSession initialize"
	"SirenUtility dataDir"
	"SirenSession eventLists"
	"See also system-dependent setup in SirenUtility>>playSoundFile"

	| sirenDir |
	DefaultMIDIIn := 4.					"Tune these to your setup"
	DefaultMIDIOut := 10.
	DefaultOSCHost := #[127 0 0 1].
	DefaultOSCPort := 54321.
	PortModel initialize.
	Verbosity := 1.				"0 = pretty silent;
								1 = medium-verbose; 
								2 = full scheduler and interface logging"
	LoggingStream = nil.		"logging output file"

						"Look for Siren data and set search paths"
	DataDir := #().
	sirenDir := './'.   " SirenUtility findDir: 'Siren7.5' tryHard: true.
	sirenDir ifNil: [self error: 'Cannot find Siren release folder'].
	sirenDir := sirenDir, Filename separatorString.
"
	ScoreDir := OrderedCollection with: (sirenDir, 'Data') asSymbol.	"Scores"
	self addDir: 'Scores' to: ScoreDir.
	self addDir: 'MID' to: ScoreDir.

	SoundDir := OrderedCollection with: (sirenDir, 'Data') asSymbol.	"Sound files"
	self addDir: 'Sound' to: SoundDir.
	self addDir: 'Samples' to: SoundDir.
	DataDir := SoundDir copy.							"Basic data directory"
	self addDir: 'Databases/*' to: SoundDir.			"STP-specific"
	self addDir: '3-Credo/*' to: SoundDir.				"You can add using wildcards"

	DataDir := SoundDir copy.							"Basic data directory"
	DataDir addAll: SoundDir.
	DataDir := DataDir asSet asOrderedCollection.

	SirenSession initialize								"Session setup"! !

!SirenUtility class methodsFor: 'class initialization'!
postLoad: aParcel
	"Load the missing pieces and put up a dialog after loading the parcel"

	| book text found |
	Transcript show: 'Loading the MusicConstants from the web'; cr.
	found := true.
	['MusicConstants.st' asFilename fileIn] 
		on: Error do: [ :ex | found := false].
	found ifFalse: [	[('Siren7.5', Filename separatorString, 'MusicConstants.st') asFilename fileIn]
		on: Error do: [ :ex | Transcript tab; show: 'Cannot find file Siren7.5/MusicConstants.st.'; cr]].
	Transcript tab; show: 'done.'; cr.
	found := true.
	[(ListWorkBook new loadFromBoss: 'Siren7.5.Workbook.bos') open]
		on: Error
		do: [ :ex | found := false].
	found ifFalse: [	[(ListWorkBook new loadFromBoss: 
						('Siren7.5', Filename separatorString, 'Siren7.5_Workbook.bos')) open]
		on: Error
		do: [ :ex | Transcript tab; show: 'Cannot find file Siren7.5/Siren7.5.Workbook.bos.'; cr.
			found := false]].
	text := 'Welcome to Siren!!

To use the package more fully, you should down-load the support files (Siren7.5.zip) from the Siren web site at http://FASTLabInc.com/Siren. 

These include an additional file-in of icons and constants (MusicConstants.st -- I just tried to file it in) as well as a documentation workbook (see below) as well as the external DLLCC files for using the sound and MIDI I/O facilities, and the CSL and Loris interfaces.

To load the MusicConstants from the web,
	''Siren7.5/MusicConstants.st'' asFilename fileIn.

To load the Siren workbook from the web (in case you haven''t downloaded the Siren zip file), do this,
	[(ListWorkBook new loadFromBoss: ''Siren7.5/Siren7.5.Workbook.bos'') open]

Enjoy!!

Stephen Pope
Santa Barbara, May, 2007'.
	book := ListWorkBook new
		replaceAllTextPagesWith: (WorkspacePage 
			labeled: (#installationWorkspace << #labels >> 'Siren Instructions')
			on: (text asValue));
		yourself.
	book open! !


!SirenUtility class methodsFor: 'logging' stamp: 'stp 12/26/2022 22:16:06'!
log: theMessage
	"Log the given string to the Transcript (or otgher output stream) if the system verbosity is >= the given verbosity level"
	"SirenUtility log: 'hello there' level: 0"

	LoggingStream
		ifNil: [Transcript show: theMessage; cr]
		ifNotNil: [LoggingStream nextPutAll: theMessage; cr].
! !

!SirenUtility class methodsFor: 'logging'!
log: theMessage level: verbosityInt
	"Log the given string to the Transcript (or otgher output stream) if the system verbosity is >= the given verbosity level"
	"SirenUtility log: 'hello there' level: 2"

	Verbosity > verbosityInt
		ifTrue: [self log: theMessage]! !

!SirenUtility class methodsFor: 'logging'!
verbosity
	"Answer the class's default verbosity, 0/1/2."
	"(SirenUtility verbosity > 0)"
	"(SirenUtility verbosity > 1)"

	^Verbosity! !

!SirenUtility class methodsFor: 'logging'!
verbosity: aNum
	"Set the class's default verbosity, 0/1/2."

	Verbosity := aNum! !


!SirenUtility class methodsFor: 'class var accessing'!
dataDir
	"Answer the class's default data storage directory."

	^DataDir! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultMIDIIn
	"Answer the class's default MIDI input device."

	^DefaultMIDIIn! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultMIDIIn: num
	"Set the class's default MIDI input device."

	DefaultMIDIIn := num! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultMIDIOut
	"Answer the class's default MIDI output device."

	^DefaultMIDIOut! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultMIDIOut: num
	"Set the class's default MIDI output device."

	DefaultMIDIOut := num! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultOSCHost
	"Answer the class's default OSC host IP."

	^DefaultOSCHost! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultOSCHost: ip
	"Set the class's default OSC host IP."

	DefaultOSCHost := ip! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultOSCPort
	"Answer the class's default OSC port #."

	^DefaultOSCPort! !

!SirenUtility class methodsFor: 'class var accessing'!
defaultOSCPort: num
	"Set the class's default OSC port #."

	DefaultOSCPort := num! !

!SirenUtility class methodsFor: 'class var accessing'!
scoreDir
	"Answer the class's default score storage directory."

	^ScoreDir! !

!SirenUtility class methodsFor: 'class var accessing'!
soundDir
	"Answer the class's default sound storage directory."

	^SoundDir! !


!SirenUtility class methodsFor: 'utilities' stamp: 'stp 12/2/2022 03:12:53'!
categoryList
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
"		'MusicUI-Functions'
		'MusicUI-Sound'

		'MusicApps-CSL'
		'MusicApps-Loris'
		'MusicApps-LPC'
		'MusicApps-SHARC'  "
	)! !

!SirenUtility class methodsFor: 'utilities' stamp: 'stp 12/2/2022 03:28:53'!
fileoutCategories
	"Create the source directory of old-fashioned ST80 file-outs."
	"SirenUtility fileoutCategories"
	"SirenUtility categoryList"
	
	self request: 'Folder name' 
		initialAnswer: 'SirenSources'
		verifying: [ :aString | aString notEmpty]
		do: [ :aString | | dir fold |
			dir _ DirectoryEntry currentDirectory fileAccessor.
			fold _ DirectoryEntry withPathName: aString.
			fold exists ifFalse: [dir createDirectory: fold pathName].
		Transcript cr; cr; nextPutAll: '------ Storing Siren Source to folder ', fold pathName; cr; cr.
		self categoryList do:
			[ :cat | | ent |
			Transcript cr; show: '------', cat; cr; cr.
			ent _ fold // (cat asFileName, '.st').
			ent exists
				ifTrue: [ent delete].
			ent writeStreamDo: 
				[ :fileStream |
				(SystemOrganization withSubCategoriesOf: cat) do: 
					[ :category | 
					SystemOrganization fileOutCategory: category on: fileStream initializing: true ]]]]! !

!SirenUtility class methodsFor: 'utilities'!
playSoundFile: nam
	"Play a sound file using UNIX shell and libsndfile's play program"
	"Edit this to your platform's method, which might use a SoundPort or a simple shell command"
	"Sound playFile: 'a.snd' "

	| fullName |
	fullName := SirenUtility findFile: nam.
	fullName ifNil: [self error: 'File not found'].
	Transcript show: 'Playing snd file ', nam, '...'; cr.
	UnixProcess cshOne: ('/usr/local/bin/sndfile-play ', fullName)! !


!SirenUtility class methodsFor: 'example access'!
formNamed: key
	"Answer the given item from the global music constants."
	"(SirenUtility formNamed: #altoClef) display"

	^(DisplayListView constants at: #forms) at: key! !

!SirenUtility class methodsFor: 'example access'!
musicConstants
	"Answer the global music constants (mostly icons)."
	"SirenUtility musicConstants inspect"

	^DisplayListView constants! !


!SirenUtility class methodsFor: 'file support' stamp: 'stp 11/25/2022 09:54:37'!
addDir: dir to: list
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
"! !

!SirenUtility class methodsFor: 'file support'!
createS7: aFullPathName
	"Create an s7 folder and copy any files with the same name  into it"
	"SirenUtility createS7: (SirenUtility dataDir first, Filename separatorString, 'testing')"
	"SirenUtility nextName: 'testing' type: 'aiff' "
	"| strm |
	strm := (SirenUtility nextName: 'testing' type: 'aiff' ) asFilename writeStream.
	strm nextPut: $..
	strm close."

	| froot fnam list |
	froot := Filename named: aFullPathName.
	fnam := Filename named: aFullPathName, '.s7'.
	fnam exists ifTrue: [fnam isDirectory
		ifTrue: [Transcript show: aFullPathName, ' exists.'; cr. ^nil]
		ifFalse: [Transcript show: aFullPathName, ' exists as a regular file!!'; cr. ^nil]].
	list := froot filesMatching: '*'.
	fnam makeDirectory.
	list do: [ :fil |
		(Filename named: fil) renameTo: (aFullPathName, '.s7', Filename separatorString, 
									(Filename named: fil) tail)]! !

!SirenUtility class methodsFor: 'file support'!
findDir: dir
	"Try to locate the requested directory either locally or globally"
	"SirenUtility findDir: 'Data' tryHard: false"
	"SirenUtility findDir: 'Frameworks' tryHard: true"

	^self findDir: dir tryHard: false! !

!SirenUtility class methodsFor: 'file support'!
findDir: dir tryHard: aggressive
	"Try to locate the requested directory either locally or globally"
	"SirenUtility findDir: 'Data' tryHard: false"
	"SirenUtility findDir: 'Frameworks' tryHard: true"
	"SirenUtility findDir: '3-Credo' tryHard: true"

	| fsep home |
	dir asFilename existsSafely ifTrue: [^dir].				"if local"
	fsep := Filename separatorString.
	(fsep, dir) asFilename existsSafely ifTrue: [^(fsep, dir)].	"if at /X"
	home := SystemUtils getEnvironmentVariable: 'HOME'.
	(home, fsep, dir) asFilename existsSafely ifTrue: [^(home, fsep, dir)].	"if in $HOME"
	(Filename filesMatching: '*')						"if in *"
		do: [ :each | ((each, fsep, dir) asFilename existsSafely)
				ifTrue: [^(each, fsep, dir)]].
	DataDir do: [ :dDir | | fnd |
		((dir findString: dDir asString startingAt: 1) = 0)
			ifTrue: [fnd := self findFile: dir in: dDir.
				fnd isEmpty ifFalse: [^fnd]]].
	aggressive ifFalse: [^nil].
	(Filename filesMatching: home, fsep, '*')			"if in ~/*"
		do: [ :each | ((each, fsep, dir) asFilename existsSafely)
				ifTrue: [^(each, fsep, dir)]].
	(Filename filesMatching: fsep, 'Smalltalk', fsep, '*')					"if in /Smalltalk/*"
		do: [ :each | ((each, fsep, dir) asFilename existsSafely)
				ifTrue: [^(each, fsep, dir)]].
	(Filename filesMatching: home, fsep, 'Smalltalk', fsep, '*')		"if in ~/Smalltalk/*"
		do: [ :each | ((each, fsep, dir) asFilename existsSafely)
				ifTrue: [^(each, fsep, dir)]].

	^nil! !

!SirenUtility class methodsFor: 'file support'!
findFile: fil
	"Try to locate the requested file in the data directories, being smart about s7 files"
	"SirenUtility findFile: 'b.snd'"
	"SirenUtility findFile: '1.1a.aiff'"

	| nam ans base s7 |
	fil asFilename exists ifTrue: [^fil asFilename asAbsoluteFilename asString].
	DataDir isString
		ifTrue: [nam := (DataDir, Filename separatorString, fil).
			nam asFilename exists ifTrue: [^nam]]
		ifFalse: [DataDir do:
			[ :dir |
			ans := self findFile: fil in: dir.
			ans isEmpty ifFalse: [^ans]]].
	((fil findString: '.s7' startingAt: 1) = 0) ifFalse: [^nil].
	(fil includes: $.) ifTrue:									"if not found, look for an s7 folder"
		[base := fil asFilename base asString.
		s7 := base, 's7'.
		s7 := self findFile: s7.
		s7 ifNotNil:
			[nam := (s7, Filename separatorString, fil).
			nam asFilename exists ifTrue: [^nam]].
		base := base copyFrom: 1 to: base size - 1.		"try again removing the next-to-last suffix"
		s7 := base asFilename base asString, 's7'.
		s7 := self findFile: s7.
		s7 ifNotNil:
			[nam := (s7, Filename separatorString, fil).
			nam asFilename exists ifTrue: [^nam]]].
	^nil! !

!SirenUtility class methodsFor: 'file support'!
findFile: fil in: theDir
	"Try to locate the requested file in the given directory, being smart about s7 files"
	"SirenUtility findFile: 'b.snd'"
	"SirenUtility findFile: '1.1a.aiff'"
	"SirenUtility findFile: '1.2a1.aiff'"

	| nam ans |
	(theDir includes: $*)
		ifFalse: [nam := (theDir, Filename separatorString, fil).
			nam asFilename exists
				ifTrue: [^nam asFilename asAbsoluteFilename asString].
			(Filename filesMatching: (theDir, Filename separatorString, '*.s7')) do:
				[ :dir2 |
				nam := (theDir, Filename separatorString, fil).
				nam asFilename exists
					ifTrue: [^nam asFilename asAbsoluteFilename asString]]]
		ifTrue: [(Filename filesMatching: theDir) do:
			[ :dir2 |
			(dir2 asFilename tail = fil) ifTrue: [^dir2].
			dir2 asFilename isDirectory
				ifTrue: [ans := self findFile: fil in: dir2.
						ans isEmpty ifFalse: [^ans]]]].
	^''! !

!SirenUtility class methodsFor: 'file support'!
findFiles: ext
	"Answer all of the files in the user's data folders with the given filename extension"
	"SirenUtility findFiles: 'au'"
	"SirenUtility findFiles: 'mid'"

	| coll |
	coll := Set new.
	DataDir do:
		[ :dir | self findFiles: ext in: dir into: coll].
	^coll asOrderedCollection! !

!SirenUtility class methodsFor: 'file support'!
findFiles: ext in: theDir into: coll
	"Add the files with the given name extension to the given collection"
	"SirenUtility findFiles: 'au' into: OrderedCollection new"
	"SirenUtility findFiles: 'mid' into: OrderedCollection new"

	(theDir includes: $*)
			ifFalse: [(Filename filesMatching: (theDir, Filename separatorString, '*')) do:
				[ :dir2 | | dex |
				dex := dir2 asFilename extension.
				dex ifNotNil: [(('*', ext, '*') match: dex ignoreCase: true)
						ifTrue: [coll add: dir2 asSymbol]]]]
			ifTrue: [(Filename filesMatching: theDir) do:
				[ :dir2 |
				dir2 asFilename isDirectory
					ifTrue: [self findFiles: ext in: dir2 into: coll]]].
	^coll! !

!SirenUtility class methodsFor: 'file support'!
listS7: aName
	"List the contents of the requested s7 file"
	"SirenUtility listS7: '1.2a1'"

	| fobj fnam |
	fnam := (SirenUtility findFile: (aName, '.s7')).
	fnam isEmpty ifTrue: [^#()].
	fobj := Filename named: fnam.
	^fobj filesMatching: '*'! !

!SirenUtility class methodsFor: 'file support'!
nextName: aName type: extension
	"Answer the next free name with the given extension in the given s7 folder"
	"SirenUtility createS7: (SirenUtility dataDir first, Filename separatorString, 'testing')"
	"SirenUtility nextName: 'testing' type: 'aiff'"

	| fnam base list ind |
	fnam := (SirenUtility findFile: (aName, '.s7')).
	fnam isEmpty ifTrue: [^nil].
	ind := '_1.'.
	base := Filename named: fnam.
	list := (base filesMatching: ('*.', extension)).
	list isEmpty
		ifFalse: [list := list asSortedCollection last.
			((list includes: $_) and: [list includes: $.])
				ifTrue: [ind := (list copyFrom: (list lastIndexOf: $_) + 1 to: (list lastIndexOf: $.) - 1) asNumber.
					ind := '_', (ind + 1) printString, '.']].
	^(base asString, Filename separatorString, aName, ind, extension)! !


!classDefinition: #Timer category: #'Music-Support'!
Model subclass: #Timer
	instanceVariableNames: 'startTime accumulator lastUpdate interval running name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!

!Timer methodsFor: 'accessing'!
accumulator

	^accumulator! !

!Timer methodsFor: 'accessing'!
accumulator: aValue

	accumulator := aValue! !

!Timer methodsFor: 'accessing'!
interval

	^interval! !

!Timer methodsFor: 'accessing'!
interval: aValue

	interval := aValue! !

!Timer methodsFor: 'accessing'!
lastUpdate

	^lastUpdate! !

!Timer methodsFor: 'accessing'!
lastUpdate: aValue

	lastUpdate := aValue! !

!Timer methodsFor: 'accessing'!
name

	^name! !

!Timer methodsFor: 'accessing'!
name: aValue

	name := aValue! !

!Timer methodsFor: 'accessing'!
running

	^running! !

!Timer methodsFor: 'accessing'!
running: aValue

	running := aValue! !

!Timer methodsFor: 'accessing'!
startTime

	^startTime! !

!Timer methodsFor: 'accessing'!
startTime: aValue

	startTime := aValue.
	running := false! !


!Timer methodsFor: 'control'!
play
	"Start a timer"

	^self start! !

!Timer methodsFor: 'control'!
reset
	"Start a timer"

	startTime := Time microsecondClock.
	lastUpdate := startTime.
	self changed: #start! !

!Timer methodsFor: 'control'!
restart
	"Start a timer"

	^self start! !

!Timer methodsFor: 'control'!
scheduleAt: aTime
	"update a timer"

	running 
		ifFalse: [self error: 'Scheduling a stopped timer'].
	lastUpdate := aTime.
	self changed: #timer with: aTime.
"	Transcript show: '		T ', ((aTime - startTime) / 1000000.0 printString: 3); cr."
	^aTime + (interval * 1000000) rounded! !

!Timer methodsFor: 'control'!
start
	"Start a timer"

	running := true.
	startTime ifNil: [startTime := Time microsecondClock].
	lastUpdate := startTime.
	self changed: #start! !

!Timer methodsFor: 'control'!
stop
	"Start a timer"

	running := false.
	lastUpdate := Time microsecondClock.
	EventScheduler instance removerTimer: self
	self changed! !


!classDefinition: #EventAssociation category: #'Music-Support'!
Association subclass: #EventAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!
!EventAssociation commentStamp: '<historical>' prior: 0!
An EventAssociation is an Association with some special semantics for Siren. It can be created with the '=>' message to a Number, meaning start-time assicoated with event.!


!EventAssociation methodsFor: 'accessing'!
, anArgument
	"Add the argument as a property of the receiver."
	"((0 beat) => (1/16 beat,  'c3' pitch)),
	  ((1/16 beat) => (1/16 beat, 'b2'  pitch))"

	^EventList new add: self; add: anArgument! !

!EventAssociation methodsFor: 'accessing'!
event
	"Answer the receiver's event (value)."

	^self value! !

!EventAssociation methodsFor: 'accessing'!
start
	"Answer the receiver's start (key)."

	^self key! !

!EventAssociation methodsFor: 'accessing'!
start: aTime
	"Set the receiver's start (key)."

	self key: (Duration value: aTime)! !

!EventAssociation methodsFor: 'accessing'!
stop
	"Answer the stop time of the event association."

	^self start + self event duration! !

!EventAssociation methodsFor: 'accessing'!
time
	"Answer the receiver's start (key)."

	^self key! !


!EventAssociation methodsFor: 'printing'!
printOn: aStream 
	"Append to the argument, aStream, the two elements of the 
	EventAssociation separated by a double-right arrow (=>)."

	key printOn: aStream.
	aStream nextPutAll: ' => '.
	value printOn: aStream! !


!EventAssociation methodsFor: 'comparing'!
< anAssociation 
	"Handle events, associations, and time/order sorting."

	anAssociation isAssociation
		ifFalse: [^value < anAssociation].
	key ifNil:
		[anAssociation key ifNil:
			[^((key order) < (anAssociation order))]].
	^key < anAssociation key! !

!EventAssociation methodsFor: 'comparing'!
= anAssociation
	"If the argument's not an association, compare it to the receiver's value, otherwise answer whether the receiver's key and value are both equal to the argument's."

	(anAssociation isAssociation)
		ifFalse: [^value = anAssociation].
	key = anAssociation key
		ifTrue: [^value = anAssociation value]
		ifFalse: [^false]! !


!classDefinition: #ScheduleRecord category: #'Music-Support'!
Association subclass: #ScheduleRecord
	instanceVariableNames: 'next previous loop'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Support'!
!ScheduleRecord commentStamp: '<historical>' prior: 0!
A ScheduleRecord is used as an entry in the SEventQueue. 
It's a doubly-linked record with a key (the start time) and value (the scheduled event).

Instance Variables
	next <ScheduleRecord | nil> the next record in the Q (nil for the last)
	previous <ScheduleRecord | nil> the previous record in the Q (nil for the first)

	key <Integer> (inherited from LookupKey) the record's start time in msec
	value <Event> (inherited from Association) the record's event.!


!ScheduleRecord methodsFor: 'accessing'!
loop
	^loop! !

!ScheduleRecord methodsFor: 'accessing'!
loop: aBool
	loop := aBool! !

!ScheduleRecord methodsFor: 'accessing'!
next
	"Answer the receiver's 'next'."

	^next! !

!ScheduleRecord methodsFor: 'accessing'!
next: anObject
	"Set the receiver's instance variable 'next' to be anObject."

	next := anObject! !

!ScheduleRecord methodsFor: 'accessing'!
previous
	"Answer the receiver's 'previous'."

	^previous! !

!ScheduleRecord methodsFor: 'accessing'!
previous: anObject
	"Set the receiver's instance variable 'previous' to be anObject."

	previous := anObject! !

!ScheduleRecord methodsFor: 'accessing'!
start
	^self key! !


!ScheduleRecord methodsFor: 'control'!
clear
	key := nil.
	value := nil.
! !

!ScheduleRecord methodsFor: 'control'!
stop
	value ifNotNil: [value stop]! !


!ScheduleRecord methodsFor: 'testing'!
isActive
	^value isActive! !


!classDefinition: #SEventQueue category: #'Music-Support'!
SequenceableCollection subclass: #SEventQueue
	instanceVariableNames: 'first last'
	classVariableNames: 'CurrentRecord RecordPool'
	poolDictionaries: ''
	category: 'Music-Support'!
!SEventQueue commentStamp: '<historical>' prior: 0!
An SEventQueue is a doubly-linked list that has methods for inserting elements in time-sorted order.
These are used for real-time schedules because they can have faster search methods than using SortedCollections of OrderedCollections for the EventScheduler.
The current implementation uses a simple linear search. For really big schedules, this could be made faster with a binary search or tree-based schedule.

Instance Variables:
	first <ScheduleRecord> The first event in the Q
	last <ScheduleRecord> The last event in the Q!


!SEventQueue methodsFor: 'accessing'!
add: eventAss
	"Add the given eventAssociation to the receiver in the right place."

	^self add: eventAss loop: false! !

!SEventQueue methodsFor: 'accessing'!
add: eventAss loop: aBool
	"Add the given eventAssociation to the receiver in the right place."

	| key which |
	key := eventAss key usec value.
	which := self locateSlotFor: key.
	which key: key.
	which value: eventAss value.
	which loop: aBool! !

!SEventQueue methodsFor: 'accessing'!
asOrderedCollection
	"Answer a copy of the receiver's items"

	| list |
	list := OrderedCollection new.
	self do: [ :item | list addLast: item value name].
	^list! !

!SEventQueue methodsFor: 'accessing'!
detect: aBlock
	"Walk the list looking"

	| item |
	self isEmpty ifTrue: [^nil].
	item := first.
		[(aBlock value: item) ifTrue: [^item]. 
		item := item next.
		item notNil] whileTrue.
	^nil! !

!SEventQueue methodsFor: 'accessing'!
do: aBlock
	"Refer to the comment in Collection|do:."

	| item |
	self isEmpty ifTrue: [^nil].
	item := first.
		[aBlock value: item. 
		item := item next.
		item notNil] whileTrue.! !

!SEventQueue methodsFor: 'accessing'!
includes: sought

	^(self detect: [ :item | item == sought]) notNil! !

!SEventQueue methodsFor: 'accessing'!
isEmpty

	^first isNil! !

!SEventQueue methodsFor: 'accessing'!
itemNamed: theName
	"Answer a copy of the receiver's items"

	self do: [ :item | (item value name == theName) ifTrue: [^item]].
	^nil! !

!SEventQueue methodsFor: 'accessing' stamp: 'stp 12/27/2022 10:04:20'!
ready: now
	"Answer whether there's an event ready within 5000 usec of the given time"

	first ifNil: [^false].
	first start ifNil: [^false].
"	Transcript show: (now - first start) printString; cr."
"	^(now - first start) abs < 1000"
	^(now - first start) > 1000! !

!SEventQueue methodsFor: 'accessing'!
removeAllSuchThat: aBlock 
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
				item := next]]! !

!SEventQueue methodsFor: 'accessing'!
removeFirst
	"Remove and answer the first item from the list."

	| item |
	first ifNil: [last := nil. ^nil].
	item := first.
	first := first next.
	^item! !

!SEventQueue methodsFor: 'accessing'!
size
	"Answer how many elements the receiver contains."

	| item tally |
	self isEmpty
		ifTrue: [^0].
	tally := 0.
	item := first.
	[(item := item next) notNil] whileTrue: [tally := tally + 1].
	^ tally! !


!SEventQueue methodsFor: 'private' stamp: 'stp 12/27/2022 10:03:54'!
locateSlotFor: start
	"Find the proper slot for inserting a new event with the given start time."

	| new item |
	new := self class nextRecord.		"get a record from the class's pool"
	new ifNil: [self error: 'No scheduler slots available'].
	first ifNil:
		[first := new.
		last := nil.
		^new].
	first start ifNil: [^first].
	first start >= start					"If it's before the first scheduled event, prepend."
		ifTrue: [first previous: new.
			new next: first.
			last ifNil: [last := first].
			first := new.
			^new].
	last ifNil:							"if it's after the first and only entry"
		[first next: new.
		new previous: first.
		last := new.
		^new].
	last start <= start					"If it's after the last scheduled event, append."
		ifTrue: [last next: new.
			new previous: last.
			last := new.
			^new].
	item := first.						"Else do linear search and then insert."
	[item key < start]
		whileTrue: [item := item next].
	item previous next: new.
	new previous: item previous.
	item previous: new.
	new next: item.
	^new! !


!SEventQueue methodsFor: 'printing'!
printOn: aStream 
	"Append to the argument aStream a sequence of characters that identifies the receiver."

	aStream print: self class; nextPutAll: ' ('.
	aStream nextPutAll: first printString, ' - ', last printString.
	aStream nextPut: $)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SEventQueue class' category: #'Music-Support'!
SEventQueue class
	instanceVariableNames: ''!

!SEventQueue class methodsFor: 'record pool' stamp: 'stp 12/27/2022 11:57:58'!
initialize
	"Set up the class's record pool"
	"SEventQueue initialize"

	| poolSize |
	poolSize := 32.
	CurrentRecord := 1.
	RecordPool := Array new: poolSize.
	1 to: poolSize do:
		[ :count |
		RecordPool at: count put: ScheduleRecord new]! !

!SEventQueue class methodsFor: 'record pool'!
nextRecord
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
	^rec! !

EventScheduler initialize!
PortModel initialize!
SirenSession initialize!
SirenUtility initialize!
SEventQueue initialize!