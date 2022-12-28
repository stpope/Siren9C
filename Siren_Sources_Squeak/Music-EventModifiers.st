Object subclass: #EventModifier
Instance Variables:
	selector	<Symbol>  What aspect of the event list do I modify
	function	<Function>  What function do I aply to the aspect
	scale	<Number>  Do I apply a scalar scale?
	start	<Number>  When do I start in the event list?
	stop	<Number>  When do I stop in the event list?
	index	<Number>  Used internally to count through events
!
	"Answer the receiver's 'function'."

	^function! !
	"Set the receiver's instance variable 'function' to be anObject."

	function := anObject! !
	"Answer the receiver's 'index'."

	^index! !
	"Set the receiver's instance variable 'index' to be anObject."

	index := anObject! !
	"Answer the receiver's 'scale'."

	^scale! !
	"Set the receiver's instance variable 'scale' to be anObject."

	scale := anObject! !
	"Answer the receiver's 'selector'."

	^selector! !
	"Set the receiver's instance variable 'selector' to be anObject."

	selector := anObject! !
	"Answer the receiver's 'start'."

	^start! !
	"Set the receiver's instance variable 'start' to be anObject."

	start := anObject! !
	"Answer the receiver's 'stop'."

	^stop! !
	"Set the receiver's instance variable 'stop' to be anObject."

	stop := anObject! !

	evtList applyFunction: function to: selector startingAt: 0! !

	^function at: (time / evtList dur)! !
	"Create a new modifier and initialize it"

	^self new function: aFunction! !
	"Create a new modifier and initialize it"

	^ super new initialize.! !
	selector := #tempo! !
	"Rubato example"

	| roll rub |
	roll := ((Roll length: 2 rhythm: 100 note: 60) ampl: 80) eventList.	"100 msec = 10/sec"
"	rub := Rubato new function: (LinearFunction from: #((0 1) (0.5 0.8) (1 1.2))); scale: 10. "
	rub := Rubato new function: (LinearFunction from: #((0 1) (1 1.5))); scale: 10.
	rub applyTo: roll.
	roll play! !
	selector := #loudness! !
	"Swell example"

	| roll decresc |
	roll := ((Roll length: 2 rhythm: 100 note: 60) ampl: 80) eventList.	"100 msec = 10/sec"
	decresc := Swell new function: (LinearFunction from: #((0 1) (1 0.5))); scale: 60.
	decresc applyTo: roll.
	roll play! !