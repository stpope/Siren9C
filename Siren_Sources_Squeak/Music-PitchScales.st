Object subclass: #Scale
	^ AllNotes at: self ifAbsent: [AllNotes at: self put: (self class allIntervals collect: [:s | root perform: s])]! !
	"aNote can be a pitchClass, a pitch (ODNote) or even a PlayableNote.
	coputation takes into accont circularity and is approximative  wrt aNote
	alterations"
	"N B majorScale noteAfter: ( N B flat  @ 3)"
	| targetIndex answer |
	targetIndex := notes indexOf: (self closestPitchClassTo: aNote pitchClass).
	targetIndex = notes size
		ifTrue: [answer := notes first]
		ifFalse: [answer := notes at: targetIndex + 1].
	(aNote isKindOf: PitchClass) 
		ifTrue: [^answer].
	"octave computation"
	(aNote pitchClass natural pitchEqual: N B)	
		ifTrue: [^answer octave: aNote oct +1]
		ifFalse: [^answer octave: aNote oct]! !
	"aNote can be a pitchClass, a pitch (ODNote) or even a PlayableNote.
	coputation takes into accont circularity and is approximative  wrt aNote
	alterations"
	"N C majorScale noteBefore: ( N C  @ 3)"
	| targetIndex answer |
	targetIndex := notes indexOf: (self closestPitchClassTo: aNote pitchClass).
	targetIndex = 1
		ifTrue: [answer := notes last]
		ifFalse: [answer := notes at: targetIndex - 1].
	(aNote isKindOf: PitchClass) 
		ifTrue: [^answer].
	"octave computation"
	(aNote pitchClass natural pitchEqual: N C)	
		ifTrue: [^answer octave: aNote oct - 1]
		ifFalse: [^answer octave: aNote oct]! !
	^notes! !
	^root! !
	root := value! !
	"yields n notes from startNote by intervals of int"

	| arp x |
	arp := OrderedCollection with: startNote.
	x := startNote.
	(1 to: n - 1)
		do: [:i | arp add: (x := self following: x by: int)].
	^arp! !
	^PitchClassChord newFromNotes: l! !
	"rend la x ieme note a partir de noteDepart dans mes notes"

	| i |
	i := self indexDe: noteDepart.
	^notes at: (((x + i - 2) \\ 7) + 1)! !
	"generate the list of 7 scale tone chords from my notes with polyphony n"

	| o startNote theNotes |
	o := Array new: 7.
	startNote := self root.
	(1 to: 7)
		do: 
			[:i | 
			theNotes := self arpeggioFrom: startNote poly: n interval: 3.
			startNote := self following: startNote by: 2.
			o at: i put: (self chordFromNotes: theNotes copy)].
	^o! !
	"generate the list of 7 scale tone chords from my notes with polyphony n"
	"(HungarianMinor root: N do) generateChordsPoly: 5 inOctave: 3"

	| o p startNote theNotes |
	o := Array new: 7.
	startNote := self root.
	(1 to: 7) do: 
		[ :i | 
		theNotes := self arpeggioFrom: startNote poly: n interval: 3.
		startNote := self following: startNote by: 2.
		o at: i put: (self chordFromNotes: theNotes copy)].
	p := Array new: 7.
	(1 to: 7) do: 
		[ :i | 
		p at: i put: ((o at: i) notes collect: [ :each | (each octave: oct) semiToneCount])].
	^p! !
	^notes indexOf: uneNote! !
	"N C majorScale scaleToneTriads 
	(NeapolitanMinor root: N D ) scaleToneTriads"

	^self generateChordsPoly: 3! !
	^self notes collect: [ :n| (n octave: o) semiToneCount]! !
	"first note is in octave o. others follow (and may shift octave!!)"

	| oc |
	oc := o.
	^self notes
		collect: 
			[:n | 
			n natural = PitchClass C ifTrue: [oc := oc + 1].
			n octave: oc]! !
	self notes do: [:pc | pc natural == aPc natural ifTrue: [^pc]].
	self error: 'impossible'! !
	"first note is in octave o. others follow (and may shift octave!!)"

	| oc |
	oc := o.
	^self reverseNotes
		collect: 
			[:n | 
			n natural = PitchClass B ifTrue: [oc := oc - 1].
			n octave: oc]! !
	"assumes a and b belong to the scale.
	N re  minorScale diatonicNotesFrom: (N do sharp  octave: 3) to: (N re octave: 4)"

	|  currentNote currentPc currentOctave res a b pc |

	dep <= arr ifTrue: [a:= dep. b := arr] ifFalse: [a:= arr. b := dep].
	pc := self closestPitchClassTo: a pitchClass.
	currentNote := pc octave: a oct.
	currentPc := self notes indexOf: pc.
	currentOctave := a oct.
	res := OrderedCollection new.
	[currentNote <= b] whileTrue: 
			[res add: currentNote.
			currentNote pitchClass natural == N si
				ifTrue: [currentOctave := currentOctave + 1].
			currentPc = 7 ifTrue: [currentPc := 1] ifFalse: [currentPc := currentPc + 1].
			currentNote := (notes at: currentPc) octave: currentOctave].
	^res! !
	^self notes collect: [:n| n octave: o]! !
	"intervalList depends on the type of the scale. It is redefined in each 
	subclass of Scale"

	notes := self class intervalList collect: [:s | root perform: s]! !
	"yields the list of interval. Redefined in subclasses"

	^self class intervalList! !
	"N la minorScale leadingNote"

	^self notes last! !
	"first is root, others following descending order"

	^(notes copyFrom: 2 to: notes size) asOrderedCollection reverse
			addFirst: notes first; yourself! !
	"N re minorScale tonic"

	^self notes first! !
	| str |
	str := root printString , (self isMajor ifTrue: ['M'] ifFalse: ['m']).
	str size = 2 ifTrue: [str := str , ' '].
	s nextPutAll: str! !
	s nextPutAll: '(', self class name asString.
	s nextPutAll: ' root: ', root storeString.
	s nextPut: $).! !
	^(root intervalBetween: unAccord root) type! !
	^(root intervalBetween: aNote) type! !
	^self class root: (root transposeOf: anInterval)! !
	^root = g root and: [self class == g class]! !
	listOfNotes do: 
		[:each | 
		(self containsNote: each) 
			ifFalse: [^false]].
	^true! !
	^(self notes detect: [:each | each pitchClass = n pitchClass]
			ifNone: [nil]) notNil! !
	| tonic int |
	tonic := notes first.
	notes do: 
			[:each | 
			int := tonic intervalBetween: each.
			int type = 3
				ifTrue: [^int isA: #majorThird]].
	^false! !
	| tonic int |
	tonic := notes first.
	notes do: 
			[:each | 
			int := tonic intervalBetween: each.
			int type = 3
				ifTrue: [^int isA: #minorThird]].
	^false! !
	^(notes detect: [:n | (n isKindOf: PitchClassDoubleSharp)
			or: [n isKindOf: PitchClassDoubleFlat]]
		ifNone: [nil]) == nil! !
	"Warning: doubles sharps are not sharps!!"

	^(notes select: [:n | n isFlat])! !
	"Attention les doubles bemols ne sont pas des bemols!!"

	^(notes select: [:n | n isFlat]) size! !
	"Attention les doubles dieses ne sont pas des dieses!!"

	^(notes select: [:n | n isSharp]) size! !
	"Warning: doubles sharps are not sharps!!"

	^(notes select: [:n | n isSharp])! !

	^root hash + self class hash! !
	self subclassResponsibility! !
	^ self subclassResponsibility! !
	"N C majorScale pitchClassFor: N A"
	| count |
	count := root numberOfSemiTonesBetween: p.
	count = 0 ifTrue: [^ root].
	^ self allNotes at: (1 + (count \\ 12))! !
	"self askScale"

	| scales scaleType pitch |
	scales := Scale subclasses asArray.
	scaleType := (PopUpMenu labelArray: (scales collect: [:s | s printString])) startUp.
	scaleType = 0 ifTrue: [^nil].
	pitch := N noteNamed: (FillInTheBlank request: 'note' initialAnswer: 'Do').
	^(scales at: scaleType)
		root: pitch! !
	| g |
	g := self new.
	g root: aNote; computeNotes.
	"g isValid ifFalse: [self warn: 'invalid scale']."
	^g! !
	^Array
		with: MajorScale
		with: HarmonicMinorScale
		with: MelodicMinorScale! !
	Transcript show: s; cr! !
"
	N la flat majorScale notes ->  #(Lab Sib Do Reb Mib Fa Sol )
	N do harmonicMinorScale notes ->  #(Do Re Mib Fa Sol Lab Si )
	
	N re majorScale numberOfSharps -> 2

	N do majorScale generateChordsPoly: 4 ->
OrderedCollection ([Do #maj7 ] [Re #min 7 ] [Mi #min 7 ] [Fa #maj7 ] [Sol 7 ] [La #min 7 ] [Si #halfDim7 ] ) 

	N do majorScale generateChordsPoly: 6
-> OrderedCollection (Do #maj7 9 11  Re #min 7 9 11  Mi #min 7 #dim9 11  Fa #maj7 9 #aug11  Sol 7 9 11  La #min 7 9 11  Si #min #dim5 7 #dim9 11  )

	N do majorScale generateChordsPoly: 7
-> OrderedCollection (Do #maj7 9 11 13  Re #min 7 9 11 13  Mi #min 7 #dim9 11 #dim13  Fa #maj7 9 #aug11 13  Sol 7 9 11 13  La #min 7 9 11 #dim13  Si #min #dim5 7 #dim9 11 #dim13  )

N re  HarmonicMinorScale generateChordsPoly: 3

	N do majorScale degreDeAccord: (FPChord new fromString: 'Fa min 7')
-> 4
"! !
	^self subclassResponsibility! !
	AllNotes := Dictionary new! !

	^#(#unison #augmentedUnison #majorSecond #minorThird #majorThird #perfectFourth #augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #majorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !
	(notes includes: n) ifTrue: [ ^ 0].
	root majorSeventh = n ifTrue: [^ 0].
	root majorSixth = n ifTrue: [^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !
	root = N C sharp ifTrue: [^ N D flat harmonicMinorScale].
	root = N D sharp ifTrue: [^ N E flat harmonicMinorScale].
	root = N F sharp ifTrue: [^ N G flat harmonicMinorScale].
	root = N G sharp ifTrue: [^ N A flat harmonicMinorScale].
	root = N A sharp ifTrue: [^ N B flat harmonicMinorScale].
	root = N D flat ifTrue: [^ N C sharp harmonicMinorScale].
	root = N E flat ifTrue: [^ N D sharp harmonicMinorScale].
	root = N G flat ifTrue: [^ N F sharp harmonicMinorScale].
	root = N A flat ifTrue: [^ N G sharp harmonicMinorScale].
	root = N B flat ifTrue: [^ N A sharp harmonicMinorScale].
	^ nil! !
	^#(#unison #diminishedSecond #second #minorThird #majorThird #perfectFourth augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !
"
	(HungarianMinor root: N do) notes ->   #(Do Re Mib Fa# Sol Lab Si )

	(HungarianMinor root: N do) generateChordsPoly: 4
->  OrderedCollection (Do #min #maj7  Re #dim5 7  Mib #aug5 #maj7  Fa# #dim5 #dim7  Sol #maj7  Lab #maj7  Si #min #dim7  )

	(FPChord newFromString: 'D min ') possibleTonalitiesInScaleClass: self ->
ListeDAnalyses ({I de Re HungarianMinor} {VII de Mib HungarianMinor} )

"! !
	"tire de McLaughlin et le Mahavischnu Orchestra"

	^#(#unison #second #minorThird #augmentedFourth #perfectFifth #minorSixth #majorSeventh )! !
	(notes includes: n) ifTrue: [ ^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !
	root = N C sharp ifTrue: [^ N D flat majorScale].
	root = N D sharp ifTrue: [^ N E flat majorScale].
	root = N F sharp ifTrue: [^ N G flat majorScale].
	root = N G sharp ifTrue: [^ N A flat majorScale].
	root = N A sharp ifTrue: [^ N B flat majorScale].
	root = N D flat ifTrue: [^ N C sharp majorScale].
	root = N E flat ifTrue: [^ N D sharp majorScale].
	root = N G flat ifTrue: [^ N F sharp majorScale].
	root = N A flat ifTrue: [^ N G sharp majorScale].
	root = N B flat ifTrue: [^ N A sharp majorScale].
	^ nil! !
	^#(#unison #augmentedUnison #second #augmentedSecond #majorThird #perfectFourth augmentedFourth #perfectFifth #augmentedFifth #majorSixth #minorSeventh #majorSeventh )! !
	^#(#unison #second #majorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !
	(notes includes: n) ifTrue: [ ^ 0].
	root majorSeventh = n ifTrue: [^ 0].
	root majorSixth = n ifTrue: [^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !
	root = N C sharp ifTrue: [^ N D flat melodicMinorScale].
	root = N D sharp ifTrue: [^ N E flat melodicMinorScale].
	root = N F sharp ifTrue: [^ N G flat melodicMinorScale].
	root = N G sharp ifTrue: [^ N A flat melodicMinorScale].
	root = N A sharp ifTrue: [^ N B flat melodicMinorScale].
	root = N D flat ifTrue: [^ N C sharp melodicMinorScale].
	root = N E flat ifTrue: [^ N D sharp melodicMinorScale].
	root = N G flat ifTrue: [^ N F sharp melodicMinorScale].
	root = N A flat ifTrue: [^ N G sharp melodicMinorScale].
	root = N B flat ifTrue: [^ N A sharp melodicMinorScale].
	^ nil! !
	^#(#unison #diminishedSecond #second #minorThird #majorThird #perfectFourth augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #diminishedFifth #majorSixth #majorSeventh )! !
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !