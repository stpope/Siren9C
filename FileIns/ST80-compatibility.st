'From Cuis 6.0 [latest update: #5115] on 3 May 2022 at 3:56:03 pm'!

!SystemDictionary methodsFor: 'VW-compatibility' stamp: 'STP 4/22/2022 11:36:12'!
defineClass: clName superclass: superClass indexedType: ind private: priv instanceVariableNames: iVars classInstanceVariableNames: cIVars imports: imps category: cat
	"Visualworks-compatible class creation method"
	
	^superClass subclass: clName
	instanceVariableNames: iVars
	classVariableNames: ''
	poolDictionaries: ''
	category: cat! !

'From Cuis 6.0 [latest update: #5542] on 25 November 2022 at 2:16:32 pm'!
!classDefinition: #Model category: #'Kernel-Objects'!
Object subclass: #Model
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

'From Cuis 6.0 [latest update: #5115] on 4 May 2022 at 10:50:02 am'!

!Object methodsFor: 'events-old protocol' stamp: 'stp 5/4/2022 10:48:37'!
changed: anAspect with: aParameter 
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

"	self changed: anAspect with: aParameter from: self"
	self 
		triggerEvent: #changed:
		with: { anAspect . aParameter }! !


!Object methodsFor: 'events-old protocol' stamp: 'stp 5/4/2022 10:49:31'!
changed: anAspect with: aParameter from: anObject
	"Receiver changed. The change is denoted by the argument aParameter. 
	Usually the argument is a Symbol that is part of the dependent's change 
	protocol. Inform all of the dependents."

"	self dependents do:
		[ :dep |
		dep update: anAspect with: aParameter from: self] "
	self 
		triggerEvent: #changed:
		with: { anAspect . aParameter . anObject }
! !


!Time class methodsFor: 'general inquiries' stamp: 'stp 11/25/2022 09:52:40'!
microsecondClock
	"Answer the number of milliseconds since the millisecond clock was last reset or rolled over.
	Answer 0 if the primitive fails.
	
	Note: Prefer #localMillisecondClock, that doesn't rollover."

	^ self primMillisecondClock * 1000! !


!Delay class methodsFor: 'instance creation' stamp: 'stp 11/25/2022 09:50:54'!
forMicroseconds: anInteger
	"Return a new Delay for the given number of microseconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time."

	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ self new
		setDelay: (anInteger / 1000) asInteger
		forSemaphore: Semaphore new
! !

