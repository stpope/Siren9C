Object subclass: #EventModifier
	instanceVariableNames: 'selector function scale start stop index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventModifiers'!
!EventModifier commentStamp: '<historical>' prior: 0!
EventModifier is the abstract superclass of the classes whose instances operate on event lists. There are operations that can be done eagerly (at definition time) or lazily (at run time)
Instance Variables:
	selector	<Symbol>  What aspect of the event list do I modify
	function	<Function>  What function do I aply to the aspect
	scale	<Number>  Do I apply a scalar scale?
	start	<Number>  When do I start in the event list?
	stop	<Number>  When do I stop in the event list?
	index	<Number>  Used internally to count through events
!


!EventModifier methodsFor: 'accessing'!
function
	"Answer the receiver's 'function'."

	^function! !

!EventModifier methodsFor: 'accessing'!
function: anObject
	"Set the receiver's instance variable 'function' to be anObject."

	function := anObject! !

!EventModifier methodsFor: 'accessing'!
index
	"Answer the receiver's 'index'."

	^index! !

!EventModifier methodsFor: 'accessing'!
index: anObject
	"Set the receiver's instance variable 'index' to be anObject."

	index := anObject! !

!EventModifier methodsFor: 'accessing'!
scale
	"Answer the receiver's 'scale'."

	^scale! !

!EventModifier methodsFor: 'accessing'!
scale: anObject
	"Set the receiver's instance variable 'scale' to be anObject."

	scale := anObject! !

!EventModifier methodsFor: 'accessing'!
selector
	"Answer the receiver's 'selector'."

	^selector! !

!EventModifier methodsFor: 'accessing'!
selector: anObject
	"Set the receiver's instance variable 'selector' to be anObject."

	selector := anObject! !

!EventModifier methodsFor: 'accessing'!
start
	"Answer the receiver's 'start'."

	^start! !

!EventModifier methodsFor: 'accessing'!
start: anObject
	"Set the receiver's instance variable 'start' to be anObject."

	start := anObject! !

!EventModifier methodsFor: 'accessing'!
stop
	"Answer the receiver's 'stop'."

	^stop! !

!EventModifier methodsFor: 'accessing'!
stop: anObject
	"Set the receiver's instance variable 'stop' to be anObject."

	stop := anObject! !


!EventModifier methodsFor: 'application'!
applyTo: evtList

	evtList applyFunction: function to: selector startingAt: 0! !

!EventModifier methodsFor: 'application'!
valueIn: evtList at: time

	^function at: (time / evtList dur)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

EventModifier class
	instanceVariableNames: ''!

!EventModifier class methodsFor: 'instance creation'!
function: aFunction
	"Create a new modifier and initialize it"

	^self new function: aFunction! !

!EventModifier class methodsFor: 'instance creation'!
new
	"Create a new modifier and initialize it"

	^ super new initialize.! !


EventModifier subclass: #Rubato
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventModifiers'!
!Rubato commentStamp: '<historical>' prior: 0!
Rubato allows you to apply a function of time to the start times of events.!


!Rubato methodsFor: 'initialize-release'!
initialize
	selector := #tempo! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Rubato class
	instanceVariableNames: ''!

!Rubato class methodsFor: 'examples'!
example
	"Rubato example"

	| roll rub |
	roll := ((Roll length: 2 rhythm: 100 note: 60) ampl: 80) eventList.	"100 msec = 10/sec"
"	rub := Rubato new function: (LinearFunction from: #((0 1) (0.5 0.8) (1 1.2))); scale: 10. "
	rub := Rubato new function: (LinearFunction from: #((0 1) (1 1.5))); scale: 10.
	rub applyTo: roll.
	roll play! !


EventModifier subclass: #Swell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-EventModifiers'!
!Swell commentStamp: '<historical>' prior: 0!
A swell applies a fnuction to the amplitudes of events in an event list.!


!Swell methodsFor: 'initialize-release'!
initialize
	selector := #loudness! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Swell class
	instanceVariableNames: ''!

!Swell class methodsFor: 'as yet unclassified'!
example
	"Swell example"

	| roll decresc |
	roll := ((Roll length: 2 rhythm: 100 note: 60) ampl: 80) eventList.	"100 msec = 10/sec"
	decresc := Swell new function: (LinearFunction from: #((0 1) (1 0.5))); scale: 60.
	decresc applyTo: roll.
	roll play! !
