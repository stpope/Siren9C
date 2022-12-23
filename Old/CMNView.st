'From Squeak3.0 of 4 February 2001 [latest update: #3545] on 21 May 2007 at 9:39:46 am'!
PitchTimeView subclass: #CMNView
	instanceVariableNames: 'steps sharps staves ledgers xTable '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-Editors'!
!CMNView commentStamp: '<historical>' prior: 0!
Instances of CMNView display a simple version of common-practise Western music notation.
For reasons of size and readability, it is kept simple. It handles clefs, accidentals and ledger lines, but has no beaming or fancy page layout; this should be added in a subclass.

Instance Variables:
	staves	<OrderedCollection of (Integer -> Symbol)> the x positions of the staves 
				and their symbolic names (clefs), e.g., 40 -> #piano.
	steps	<Array of Integers> the step array (pitch number mapped to y value)
	sharps	<Array of Integers> the sharp array (pitch offsets that take accidentals)
	ledgers	<Array of Integers> The y positions of the ledger lines (5 by default)
	xTable	<OrderedCollection of Integers> the table of x positions of the events;
			needed for fancy layout or lots of notes or accidentals!


!CMNView methodsFor: 'initialize release'!
initialize
	"Initialize the instance variables of the receiver."

	scale := 1@3.
	staves := OrderedCollection new.
	staves add: (40 -> #piano).	"add a two-staff piano sheet"
	pitchOffset := 36.				"its lowest note is C0"
	self initSteps.
	xTable := OrderedCollection new.
	super initialize.
! !


!CMNView methodsFor: 'accessing'!
clefWidth
	"Answer the receiver's clef form width."

	^40! !

!CMNView methodsFor: 'accessing'!
ledgers
	"Answer the receiver's array of ledger line y offsets."

	^ledgers! !

!CMNView methodsFor: 'accessing'!
sharps
	"Answer the receiver's array of note values which require accidentals."

	^sharps! !


!CMNView methodsFor: 'layout manager access'!
generateItemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	| dur tempo note |
	dur := anAss value duration.
	tempo := model list at: #tempoScale.
	dur := (dur isMemberOf: RatioDuration)
		ifTrue: [dur value asFraction printString]
		ifFalse: [tempo == nil 
			ifFalse: [(1 / (tempo / dur asMS value)) printString]
			ifTrue: [((dur asMS value roundTo: 125) / 1000) printString]].
	dur := dur copyFrom: 2 to: dur size - 1.
	note := ((MusicConstants at: #StdNoteIcons) associations
				detect: [ :ass | ass key asString = dur]
				ifNone: [nil->nil]) value visual.
	^(DisplayItem new)
		model: anAss;
		visual: (note == nil
				ifTrue: [(MusicConstants at: #StdNoteIcons) 
								at: ('1' asSymbol) visual]
				ifFalse: [note])! !

!CMNView methodsFor: 'layout manager access'!
itemFor0: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	| dur tempo item dl v |
	dur := anAss value duration.
	tempo := model list at: #tempoScale.
	dur := (dur isMemberOf: RatioPitch)
		ifTrue: [dur value asFraction asSymbol]
		ifFalse: [tempo == nil 
			ifFalse: [(1 / (tempo / dur asMS value)) asSymbol]
			ifTrue: [((dur asMS value roundTo: 125) / 1000) asSymbol]].
	item := (DisplayItem new)
		model: anAss;
		visual: ((MusicConstants at: #StdNoteIcons) 
				at: dur
				ifAbsent: [(MusicConstants at: #StdNoteIcons) 
						at: ('1' asSymbol)]).
"	(sharps includes: (anAss value pitch asMIDI value \\ 12))
		ifTrue: [dl := DisplayList new offset: (-6@0).
			dl add: ((DisplayItem new)
				offset: 0@0;
				visual: (Form constantNamed: #sharp)).
			item offset: 6@0.
			dl add: item]."
	^dl == nil
		ifTrue: [item]
		ifFalse: [dl]! !

!CMNView methodsFor: 'layout manager access'!
itemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."
	"CMNView openOn: ((EventListEditor new) 
		eventList: (EventList scaleExampleFrom: 48 to: 60 in: 3000))"

	| dur tempo note item dl |
	dur := anAss value duration.
	tempo := model list at: #tempoScale.
	dur := (dur isMemberOf: RatioDuration)
		ifTrue: [dur value asFraction printString]
		ifFalse: [tempo == nil 
			ifFalse: [(1 / (tempo / dur asMS value)) printString]
			ifTrue: [((dur asMS value roundTo: 125) / 1000) printString]].
	dur := dur copyFrom: 2 to: dur size - 1.
	note := ((MusicConstants at: #StdNoteIcons) associations
				detect: [ :ass | ass key asString = dur]
				ifNone: [nil->nil]) value.
	item := (DisplayVisual new)
		model: anAss;
		visual: (note == nil
				ifTrue: [(MusicConstants at: #StdNoteIcons) 
								at: ('1' asSymbol)]
				ifFalse: [note]).
	(sharps includes: (anAss value pitch asMIDI value \\ 12))
		ifTrue: [dl := DisplayList new offset: (-6@0).
			dl add: ((DisplayVisual new)
				offset: 0@0;
				visual: (Siren formNamed: #sharp)).
			item offset: 6@0.
			dl add: item].
	^dl == nil
		ifTrue: [item]
		ifFalse: [dl]! !

!CMNView methodsFor: 'layout manager access' stamp: 'stp 10/29/1998 22:09'!
layoutManager
	"Answer a default layout manager for the receiver."

	^(CMNLayoutManager new)
			timeScale: scale x;
			timeOffset: 0;
			pitchScale: scale y;
			pitchOffset: pitchOffset;
			staffTop: (staves first key);
			stepArray: steps! !


!CMNView methodsFor: 'transforming' stamp: 'stp 10/29/1998 22:09'!
pitchFromY: aPoint
	"Answer the transformed pitch represented by the y-coordinate of the argument."

	| index |
	(aPoint y / scale y) > (pitchOffset + 48)
		ifTrue: [^pitchOffset + 48].
	index := 1.
	[(steps at: index) < (aPoint y + 4)]
		whileTrue: [index := index + 1].
	^MIDIPitch value: (index + pitchOffset)! !


!CMNView methodsFor: 'displaying'!
flushForm
	"Clear out the cached form and redraw staff."

	self staff: background! !


!CMNView methodsFor: 'private'!
initForm
	"Copy the default backgroundForm and draw a H-S staff on it."

	background := ColorForm extent: 1000@600 depth: Display depth.
	background fillColor: self backgroundColor.
	clefForm := ColorForm extent: 32@600 depth: Display depth.
	clefForm fillColor: self backgroundColor.
	scale y == nil
		ifFalse: [self staff: background]! !

!CMNView methodsFor: 'private' stamp: 'stp 10/29/1998 22:09'!
initSteps
	"Set up the step array; it holds the y-offsets of pitches in a 4-octave  range."

	| count stepSizes | 
	steps := Array new: 49.		"array of note offsets"
	count := 1.
	stepSizes := #(0 0 1 1 2 3 3 4 4 5 5 6). "half-step's distances from c"
	0 to: 1 do: [:oct |			"lower 2 octaves"
		stepSizes do: [:step |
			steps at: count 
				put: ((staves first key + (28 * scale y))
						 - (oct * scale y * 7 + (step * scale y))).
			count := count + 1]].
	0 to: 1 do: [:oct |			"upper 2 octaves"
		stepSizes do: [:step |
			steps at: count 
				put: ((staves first key + (14 * scale y)) 
						- (oct * scale y * 7 + (step * scale y))).
			count := count + 1]].
	steps at: 49 put: staves first key.
	sharps := #(1 3 6 8 10).	"which half-steps have accidentals"
	ledgers := Array new: 5.
	ledgers at: 1 put: (steps at: 49).
	ledgers at: 2 put: (steps at: 46).
	ledgers at: 3 put: (steps at: 25).
	ledgers at: 4 put: (steps at: 5).
	ledgers at: 5 put: (steps at: 1)! !

!CMNView methodsFor: 'private'!
staff: staff
	"Display a CMN staff onto the given staff using the staves collection to find out where." 

	| box top1 left1 right1 aNum blt |
	box := staff boundingBox.
	top1 := box top.
	left1 := box left + 1.
	right1 := box right - 1.
	blt := nil.
	blt :=	BitBlt destForm: staff sourceForm: (Form dotOfSize: 1) 
			halftoneForm: nil
			combinationRule: Form over 
			destOrigin: 0@0 sourceOrigin: 0@0
			extent: (right1-left1)@1 
			clipRect: box.
	staves do:
		[ :ass |
		aNum := top1 + ass key.
		blt drawFrom: left1@aNum to: left1+8@aNum.
		aNum := aNum + (4 * scale y). 		"allow space for 2 ledger lines"
		5 timesRepeat: 		" loop drawing lines "
			[blt drawFrom: left1@aNum to: right1@aNum.
			aNum := aNum + (scale y * 2)].
		ass value == #piano
			ifTrue: [aNum := aNum + (scale y * 2).
				5 timesRepeat:
					[blt drawFrom: left1@aNum to: right1@aNum.
					aNum := aNum + (scale y * 2)]].
		aNum := aNum + (4 * scale y). 		"allow space for 2 ledger lines"
		blt drawFrom: left1@aNum to: left1+8@aNum.
		ass value == #piano
			ifTrue: [(Siren formNamed: #trebClef) 
				displayOn: staff at: -3@(ass key + (scale y * 4) - 5)
						rule: Form paint.
			(Siren formNamed: #bassClef) 
				displayOn: staff at: 0@(ass key + (scale y * 16) - 2)
						rule: Form paint].
		ass value == #treble
			ifTrue: [(Siren formNamed: #trebClef) 
				displayOn: staff at: 0@(ass key + (scale y * 4) - 5)
						rule: Form paint].
		ass value == #bass
			ifTrue: [(Siren formNamed: #bassClef) 
				displayOn: staff at: 0@(ass key + (scale y * 4) - 5)
						rule: Form paint]]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

CMNView class
	instanceVariableNames: ''!

!CMNView class methodsFor: 'examples'!
example
	"CMNView example"
	"NOT WORKING"

	CMNView openOn: ((EventListEditor new) eventList: (EventList scaleExampleFrom: 48 to: 60 in: 3000))! !
