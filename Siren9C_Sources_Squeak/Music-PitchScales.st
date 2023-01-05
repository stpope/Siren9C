Object subclass: #Scale	instanceVariableNames: 'root notes'	classVariableNames: 'AllNotes'	poolDictionaries: ''	category: 'Music-PitchScales'!!Scale methodsFor: 'accessing'!allNotes
	^ AllNotes at: self ifAbsent: [AllNotes at: self put: (self class allIntervals collect: [:s | root perform: s])]! !!Scale methodsFor: 'accessing'!noteAfter: aNote
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
		ifFalse: [^answer octave: aNote oct]! !!Scale methodsFor: 'accessing'!noteBefore: aNote
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
		ifFalse: [^answer octave: aNote oct]! !!Scale methodsFor: 'accessing'!notes
	^notes! !!Scale methodsFor: 'accessing' stamp: 'stp 12/26/2022 18:13:15'!octaveScale
	"Most scales repeat in octaves of 2.0"

	^2.0! !!Scale methodsFor: 'accessing'!root
	^root! !!Scale methodsFor: 'accessing'!root: value
	root := value! !!Scale methodsFor: 'scale tone chords'!arpeggioFrom: startNote poly: n interval: int 
	"yields n notes from startNote by intervals of int"

	| arp x |
	arp := OrderedCollection with: startNote.
	x := startNote.
	(1 to: n - 1)
		do: [:i | arp add: (x := self following: x by: int)].
	^arp! !!Scale methodsFor: 'scale tone chords'!chordFromNotes: l
	^PitchClassChord newFromNotes: l! !!Scale methodsFor: 'scale tone chords'!following: noteDepart by: x 
	"rend la x ieme note a partir de noteDepart dans mes notes"

	| i |
	i := self indexDe: noteDepart.
	^notes at: (((x + i - 2) \\ 7) + 1)! !!Scale methodsFor: 'scale tone chords'!generateChordsPoly: n 
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
	^o! !!Scale methodsFor: 'scale tone chords'!generateChordsPoly: n inOctave: oct
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
	^p! !!Scale methodsFor: 'scale tone chords'!indexDe: uneNote 
	^notes indexOf: uneNote! !!Scale methodsFor: 'scale tone chords'!scaleToneTriads
	"N C majorScale scaleToneTriads 
	(NeapolitanMinor root: N D ) scaleToneTriads"

	^self generateChordsPoly: 3! !!Scale methodsFor: 'octave-dependent notes'!asPitchesInOctave: o
	^self notes collect: [ :n| (n octave: o) semiToneCount]! !!Scale methodsFor: 'octave-dependent notes'!ascendingFromOctave: o 
	"first note is in octave o. others follow (and may shift octave!!)"

	| oc |
	oc := o.
	^self notes
		collect: 
			[:n | 
			n natural = PitchClass C ifTrue: [oc := oc + 1].
			n octave: oc]! !!Scale methodsFor: 'octave-dependent notes'!closestPitchClassTo: aPc 
	self notes do: [:pc | pc natural == aPc natural ifTrue: [^pc]].
	self error: 'impossible'! !!Scale methodsFor: 'octave-dependent notes'!decendingFromOctave: o 
	"first note is in octave o. others follow (and may shift octave!!)"

	| oc |
	oc := o.
	^self reverseNotes
		collect: 
			[:n | 
			n natural = PitchClass B ifTrue: [oc := oc - 1].
			n octave: oc]! !!Scale methodsFor: 'octave-dependent notes'!diatonicNotesFrom: dep to: arr 
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
	^res! !!Scale methodsFor: 'octave-dependent notes'!octave: o
	^self notes collect: [:n| n octave: o]! !!Scale methodsFor: 'computing notes'!computeNotes
	"intervalList depends on the type of the scale. It is redefined in each 
	subclass of Scale"

	notes := self class intervalList collect: [:s | root perform: s]! !!Scale methodsFor: 'computing notes'!intervalList
	"yields the list of interval. Redefined in subclasses"

	^self class intervalList! !!Scale methodsFor: 'computing notes'!leadingNote
	"N la minorScale leadingNote"

	^self notes last! !!Scale methodsFor: 'computing notes' stamp: 'stp 12/18/2022 11:32'!nearestNoteTo: aHzPitch
	"Answer the frequency of the scale degree closest to the given pitch in Hz."
	"(PentatonicScale root: N re) nearestNoteTo: 450"

	| oct nNum bNote |
	nNum := (((aHzPitch / 8.17579891564371) log: 2) * 12) truncated.
	oct:= (nNum / 12) truncated.
	bNote := oct * 12.

"	Transcript show: 'nearestNoteTo: ', aHzPitch printString; cr."
	0 to: notes size do: 
		[ :ind | | nt0 nt1 t0 t1 p0 p1 |
		(ind = 0)
			ifFalse: [nt0 := notes at: ind.						"get the note degrees"
				t0 := bNote + nt0 semiToneCount.				"adjust to oct base"
				p0 := 8.17579891564371 * (2.0 raisedTo: ((t0 asFloat) / 12.0))]		"lower/higher pitch of interval"
			ifTrue:  [nt0 := notes last.					
				t0 := bNote + nt0 semiToneCount.				
				p0 := 8.17579891564371 * (2.0 raisedTo: ((t0 asFloat) / 12.0)) / self octaveScale].
		(ind < notes size)
			ifTrue: [nt1 := notes at: ind + 1.
				t1 := bNote + nt1 semiToneCount.
				p1 := 8.17579891564371 * (2.0 raisedTo: ((t1 asFloat) / 12.0))]
			ifFalse: [nt1 := (notes at: 1).
				t1 := bNote + nt1 semiToneCount.				
				p1 := 8.17579891564371 * (2.0 raisedTo: ((t1 asFloat) / 12.0)) * self octaveScale].

"		Transcript tab; show: 'Between: ', p0 printString, ' and: ', p1 printString; cr."
		(p0 <= aHzPitch and: [p1 >= aHzPitch])			"loop over intervals looking for the one that includes the given freq"
			ifTrue:  [ |  a b  | 
				a := (aHzPitch - p0).
				b := (p1 - aHzPitch).
				^(a >= b)
					ifTrue:  [p1]
					ifFalse: [p0]]].

	self error: 'Pitch class member not found'! !!Scale methodsFor: 'computing notes'!reverseNotes
	"first is root, others following descending order"

	^(notes copyFrom: 2 to: notes size) asOrderedCollection reverse
			addFirst: notes first; yourself! !!Scale methodsFor: 'computing notes'!tonic
	"N re minorScale tonic"

	^self notes first! !!Scale methodsFor: 'printing'!printOn: s 
	| str |
	str := root printString , (self isMajor ifTrue: ['M'] ifFalse: ['m']).
	str size = 2 ifTrue: [str := str , ' '].
	s nextPutAll: str! !!Scale methodsFor: 'printing'!storeOn: s
	s nextPutAll: '(', self class name asString.
	s nextPutAll: ' root: ', root storeString.
	s nextPut: $).! !!Scale methodsFor: 'degres'!degreDeAccord: unAccord
	^(root intervalBetween: unAccord root) type! !!Scale methodsFor: 'degres'!degreeOfNote: aNote 
	^(root intervalBetween: aNote) type! !!Scale methodsFor: 'transpose'!transposeOf: anInterval 
	^self class root: (root transposeOf: anInterval)! !!Scale methodsFor: 'testing'!= g  
	^root = g root and: [self class == g class]! !!Scale methodsFor: 'testing'!containsAllOfTheseNotes: listOfNotes
	listOfNotes do: 
		[:each | 
		(self containsNote: each) 
			ifFalse: [^false]].
	^true! !!Scale methodsFor: 'testing'!containsNote: n
	^(self notes detect: [:each | each pitchClass = n pitchClass]
			ifNone: [nil]) notNil! !!Scale methodsFor: 'testing'!isMajor
	| tonic int |
	tonic := notes first.
	notes do: 
			[:each | 
			int := tonic intervalBetween: each.
			int type = 3
				ifTrue: [^int isA: #majorThird]].
	^false! !!Scale methodsFor: 'testing'!isMinor
	| tonic int |
	tonic := notes first.
	notes do: 
			[:each | 
			int := tonic intervalBetween: each.
			int type = 3
				ifTrue: [^int isA: #minorThird]].
	^false! !!Scale methodsFor: 'testing'!isValid
	^(notes detect: [:n | (n isKindOf: PitchClassDoubleSharp)
			or: [n isKindOf: PitchClassDoubleFlat]]
		ifNone: [nil]) == nil! !!Scale methodsFor: 'querying'!flatNotesInSignature
	"Warning: doubles sharps are not sharps!!"

	^(notes select: [:n | n isFlat])! !!Scale methodsFor: 'querying'!numberOfFlats
	"Attention les doubles bemols ne sont pas des bemols!!"

	^(notes select: [:n | n isFlat]) size! !!Scale methodsFor: 'querying'!numberOfSharps
	"Attention les doubles dieses ne sont pas des dieses!!"

	^(notes select: [:n | n isSharp]) size! !!Scale methodsFor: 'querying'!sharpNotesInSignature
	"Warning: doubles sharps are not sharps!!"

	^(notes select: [:n | n isSharp])! !!Scale methodsFor: 'comparing'!hash

	^root hash + self class hash! !!Scale methodsFor: 'Paleo'!costForPitch: n
	self subclassResponsibility! !!Scale methodsFor: 'Paleo'!enharmonicScale
	^ self subclassResponsibility! !!Scale methodsFor: 'Paleo'!pitchClassFor: p
	"N C majorScale pitchClassFor: N A"
	| count |
	count := root numberOfSemiTonesBetween: p.
	count = 0 ifTrue: [^ root].
	^ self allNotes at: (1 + (count \\ 12))! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Scale class	instanceVariableNames: ''!!Scale class methodsFor: 'creation' stamp: 'stp 12/31/2022 07:10'!askScale	"self askScale"	| scales scaleType pitch |	scales := Scale subclasses asArray.	scaleType := (PopUpMenu				labelArray: (scales						collect: [:s | s printString])) startUp.	scaleType = 0		ifTrue: [^ nil].	pitch := N				noteNamed: (FillInTheBlank request: 'note' initialAnswer: 'Do').	^ (scales at: scaleType)		root: pitch! !!Scale class methodsFor: 'creation'!root: aNote 
	| g |
	g := self new.
	g root: aNote; computeNotes.
	"g isValid ifFalse: [self warn: 'invalid scale']."
	^g! !!Scale class methodsFor: 'creation'!standardScales
	^Array
		with: MajorScale
		with: HarmonicMinorScale
		with: MelodicMinorScale! !!Scale class methodsFor: 'creation'!warn: s 
	Transcript show: s; cr! !!Scale class methodsFor: 'examples'!example
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
"! !!Scale class methodsFor: 'interval list'!intervalList
	^self subclassResponsibility! !!Scale class methodsFor: 'Paleo class initialize'!initialize
	AllNotes := Dictionary new! !Scale subclass: #BohlenPierceScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!!BohlenPierceScale methodsFor: 'computing notes'!computeNotes
	"Make the note list for the 13th root of 3."
	"(BohlenPierceScale root: N mi) inspect"

	| bas |
	bas := 8.17579891564371 * (2.0 raisedTo: (((root semiToneCount + 60) asFloat) / 12.0)).
	notes := (0 to: 12) collect:
		[ :ind |
		bas * (3.0 raisedTo: (ind / 13) asFloat)]! !!BohlenPierceScale methodsFor: 'computing notes'!nearestNoteTo: aHzPitch
	"Answer the frequency of the scale degree closest to the given pitch in Hz."
	"(PentatonicScale root: N re) nearestNoteTo: 450"

	| f0 fN oct |
	oct := 0.0.
	f0 := notes first.
	fN := notes last.
	[f0 > aHzPitch] whileTrue:
		[oct := oct - 1.0.
		f0 := f0 / 3.0. 
		fN := fN / 3.0.].
	[fN < aHzPitch] whileTrue: 
		[oct := oct + 1.0.
		f0 := f0 * 3.0.
		fN := fN * 3.0].

"	Transcript show: 'nearestNoteTo: ', aHzPitch printString; cr."
	0 to: notes size do: 
		[ :ind | | p0 p1 |
		(ind = 0)
			ifFalse: [p0 := (notes at: ind) * (3.0 raisedTo: oct)]		"get the note degrees"
			ifTrue: [p0 := notes last * (3.0 raisedTo: oct - 1)].
		(ind < notes size)
			ifTrue: [p1 := (notes at: ind + 1)* (3.0 raisedTo: oct)]
			ifFalse: [p1 := notes first * (3.0 raisedTo: oct + 1)].

"		Transcript tab; show: 'Between: ', p0 printString, ' and: ', p1 printString; cr."
		(p0 <= aHzPitch and: [p1 >= aHzPitch])			"loop over intervals looking for the one that includes the given freq"
			ifTrue:  [ |  a b  | 
				a := (aHzPitch - p0).
				b := (p1 - aHzPitch).
				^(a >= b)
					ifTrue:  [p1]
					ifFalse: [p0]]].

	self error: 'Pitch class member not found'! !!BohlenPierceScale methodsFor: 'testing'!isMajor
	^false! !!BohlenPierceScale methodsFor: 'testing'!isMinor
	^false! !!BohlenPierceScale methodsFor: 'transpose'!octaveScale
	"Most scales repeate in octaves of 2.0, but not BP"

	^3.0! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!BohlenPierceScale class	instanceVariableNames: ''!!BohlenPierceScale class methodsFor: 'instance creation'!root: aNote 
	| g |
	g := self new.
	g root: aNote; computeNotes.
	^g! !Scale subclass: #ChromaticScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!ChromaticScale class	instanceVariableNames: ''!!ChromaticScale class methodsFor: 'interval list'!intervalList

	^#(#unison #augmentedUnison #majorSecond #minorThird #majorThird #perfectFourth #augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !Scale subclass: #DoubleHarmonic	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DoubleHarmonic class	instanceVariableNames: ''!!DoubleHarmonic class methodsFor: 'interval list'!intervalList
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #majorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !Scale subclass: #HarmonicMinorScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!!HarmonicMinorScale methodsFor: 'Paleo'!costForPitch: n
	(notes includes: n) ifTrue: [ ^ 0].
	root majorSeventh = n ifTrue: [^ 0].
	root majorSixth = n ifTrue: [^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !!HarmonicMinorScale methodsFor: 'Paleo'!enharmonicScale
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
	^ nil! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!HarmonicMinorScale class	instanceVariableNames: ''!!HarmonicMinorScale class methodsFor: 'interval list'!allIntervals
	^#(#unison #diminishedSecond #second #minorThird #majorThird #perfectFourth augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !!HarmonicMinorScale class methodsFor: 'interval list'!intervalList
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !Scale subclass: #HungarianMinor	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!HungarianMinor class	instanceVariableNames: ''!!HungarianMinor class methodsFor: 'examples'!exemple	
"
	(HungarianMinor root: N do) notes ->   #(Do Re Mib Fa# Sol Lab Si )

	(HungarianMinor root: N do) generateChordsPoly: 4
->  OrderedCollection (Do #min #maj7  Re #dim5 7  Mib #aug5 #maj7  Fa# #dim5 #dim7  Sol #maj7  Lab #maj7  Si #min #dim7  )

	(FPChord newFromString: 'D min ') possibleTonalitiesInScaleClass: self ->
ListeDAnalyses ({I de Re HungarianMinor} {VII de Mib HungarianMinor} )

"! !!HungarianMinor class methodsFor: 'interval list'!intervalList
	"tire de McLaughlin et le Mahavischnu Orchestra"

	^#(#unison #second #minorThird #augmentedFourth #perfectFifth #minorSixth #majorSeventh )! !Scale subclass: #MajorScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!!MajorScale methodsFor: 'Paleo'!costForPitch: n
	(notes includes: n) ifTrue: [ ^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !!MajorScale methodsFor: 'Paleo'!enharmonicScale
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
	^ nil! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MajorScale class	instanceVariableNames: ''!!MajorScale class methodsFor: 'interval list'!allIntervals
	^#(#unison #augmentedUnison #second #augmentedSecond #majorThird #perfectFourth augmentedFourth #perfectFifth #augmentedFifth #majorSixth #minorSeventh #majorSeventh )! !!MajorScale class methodsFor: 'interval list'!intervalList
	^#(#unison #second #majorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !Scale subclass: #MelodicMinorScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!!MelodicMinorScale methodsFor: 'Paleo'!costForPitch: n
	(notes includes: n) ifTrue: [ ^ 0].
	root majorSeventh = n ifTrue: [^ 0].
	root majorSixth = n ifTrue: [^ 0].
	(self allNotes includes: n) ifTrue: [^ 1].
	^ 2! !!MelodicMinorScale methodsFor: 'Paleo'!enharmonicScale
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
	^ nil! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MelodicMinorScale class	instanceVariableNames: ''!!MelodicMinorScale class methodsFor: 'interval list'!allIntervals
	^#(#unison #diminishedSecond #second #minorThird #majorThird #perfectFourth augmentedFourth #perfectFifth #minorSixth #majorSixth #minorSeventh #majorSeventh )! !!MelodicMinorScale class methodsFor: 'interval list'!intervalList
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !Scale subclass: #NeapolitanMajor	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!NeapolitanMajor class	instanceVariableNames: ''!!NeapolitanMajor class methodsFor: 'interval list'!intervalList
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !Scale subclass: #NeapolitanMinor	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!!NeapolitanMinor commentStamp: '<historical>' prior: 0!<historical>!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!NeapolitanMinor class	instanceVariableNames: ''!!NeapolitanMinor class methodsFor: 'interval list'!intervalList
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #perfectFifth #minorSixth #majorSeventh )! !Scale subclass: #Oriental	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Oriental class	instanceVariableNames: ''!!Oriental class methodsFor: 'interval list'!intervalList
	"tire de McLaughlin et le Mahavishnu Orchestra"

	^#(#unison #minorSecond #minorThird #perfectFourth #diminishedFifth #majorSixth #majorSeventh )! !Scale subclass: #PentatonicScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PentatonicScale class	instanceVariableNames: ''!!PentatonicScale class methodsFor: 'interval list' stamp: 'stp 12/18/2022 11:35'!intervalList

	^#(#unison #majorSecond #perfectFourth #perfectFifth #majorSixth)! !Scale subclass: #PseudoMinorScale	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchScales'!"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PseudoMinorScale class	instanceVariableNames: ''!!PseudoMinorScale class methodsFor: 'interval list'!intervalList
	^#(#unison #second #minorThird #perfectFourth #perfectFifth #majorSixth #majorSeventh )! !Scale initialize!