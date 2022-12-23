'From Squeak3.0 of 4 February 2001 [latest update: #3545] on 21 May 2007 at 9:39:58 am'!
PitchTimeLayoutManager subclass: #CMNLayoutManager
	instanceVariableNames: 'stepArray staffTop '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-Layout'!
!CMNLayoutManager commentStamp: '<historical>' prior: 0!
Instances of CMNLayoutManager are used to generate display lists which common-practise Western music notation.
They handle (together with CMNViews) the nasty heuristics of CMN generation.!


!CMNLayoutManager methodsFor: 'accessing'!
staffTop: y
	"Set the receiver's min y."

	staffTop := y! !

!CMNLayoutManager methodsFor: 'accessing'!
stepArray: steps
	"Set the receiver's pitch/y table."

	stepArray := steps! !


!CMNLayoutManager methodsFor: 'generating'!
addItem: anAss atNode: aPath in: aDisplayList
	"Add the given item to the display list."

	| pitch anE displayPoint line llineOffset |
					"add the note head"
	super addItem: anAss atNode: aPath in: aDisplayList.
	anE := anAss value.
	anE hasItems ifTrue: [^self].
	pitch := anE pitch asMIDI value - pitchOffset.
	displayPoint := self displayPointFor: anAss.
	llineOffset := 6@20.
	line := DisplayLine new extent: (12@0).
					"add the accidental, if any"
"	(view sharps includes: (pitch \\ 12))
		ifTrue: [(anE pitch asSymbol value includes: $#)
			ifTrue: [aDisplayList add: ((DisplayItem new)
						offset: displayPoint - (13@8);
						visual: (Form constantNamed: #sharp))]
			ifFalse: [aDisplayList add: ((DisplayItem new)
						offset: displayPoint - (13@8);
						visual: (Form constantNamed: #flat))]].
"					"add ledger lines, if any"
	pitch > 47			"top ledger line--c5"
		ifTrue: [displayPoint y: (view ledgers at: 1).
			aDisplayList add: (line copy offset: displayPoint - llineOffset)
						atPath: aPath].
	pitch > 44			"second ledger line--a5"
		ifTrue: [displayPoint y: (view ledgers at: 2).
			aDisplayList add: (line copy offset: displayPoint - llineOffset)
						atPath: aPath].
	(pitch = 24 or: [pitch = 25])	"middle-c ledger line"
		ifTrue: [displayPoint y: (view ledgers at: 3).
			aDisplayList add: (line copy offset: displayPoint - llineOffset)
						atPath: aPath].
	pitch < 5				"lower ledger line--e1"
		ifTrue: [displayPoint y: (view ledgers at: 4).
			aDisplayList add: (line copy offset: displayPoint - llineOffset)
						atPath: aPath].
	pitch < 2				"lowest ledger line--c1"
		ifTrue: [displayPoint y: (view ledgers at: 5).
			aDisplayList add: (line copy offset: displayPoint - llineOffset)
						atPath: aPath].! !

!CMNLayoutManager methodsFor: 'generating'!
displayPointFor: anAss
	"Answer the display point for the argument."

	^(view clefWidth + (anAss key asMS value - timeOffset 
				/ timeScale) asInteger)
		@ ((self formYFromPitch: anAss event pitch) - view class inset y)! !

!CMNLayoutManager methodsFor: 'generating'!
formYFromPitch: aPitch
	"take a note's pitch and return a display point y coordinate relative to the form."

	| pch |
	pch := aPitch asMIDI value - pitchOffset.
	((pch >= 0) and: [pch < stepArray size])
		ifTrue: [^stepArray at: (pch + 1)].
	pch < 0 ifTrue: [^staffTop + (28 * pitchScale)].
	pch >= stepArray size ifTrue: [^staffTop]! !
