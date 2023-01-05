Object subclass: #MusicalInterval	instanceVariableNames: 'type semiTones'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!MusicalInterval commentStamp: '<historical>' prior: 0!MusicalInterval commentStamp: '<historical>' prior: 0!!
Intervalle represente un intervalle entre deux notes.
type represente l'ecart entre les deux notes extremites. Cet ecart ne tient compte
que des notes naturelles. Par exemple, les tierces (majeures ou mineures) ont comme type 3,
les quarte 4 etc.
demisTons est le nombre de demis tons entre les deux notes. Par exemple une tierce
majeure a comme type 3 et demisTons = 4.
!!MusicalInterval methodsFor: 'computing notes'!topIfBottomIs: aNote 
	"yields the note making the interval self with aNote"
	"two cases :
	- aNote is a pitch-class, in which case, no interval is 
	greater than an octave
	- aNote is anOctave-DependentNote, in which case octaves
	should be counted"

	"we want 1 -1, 2-2, 7-7, 8-1, 9-2, etc ..."
	(aNote isKindOf: PitchClass)
		ifTrue: [^aNote alterate: (aNote nthFollowing: type - 1 \\ 7)
				toReach: semiTones \\ 12].
	^aNote alterate: (aNote nthFollowing: type - 1)
		toReach: semiTones! !!MusicalInterval methodsFor: 'printing' stamp: 'stp 11/30/2022 12:23:09'!isPrintable
	[self printPrintableOn: (WriteStream on: ''). ^true] 
		on: Error 
		do: [^false]! !!MusicalInterval methodsFor: 'printing' stamp: 'stp 11/30/2022 12:23:53'!printOn: s 
	[self printPrintableOn: s]
		on: Error
		do: [s nextPutAll: 'non printable interval']! !!MusicalInterval methodsFor: 'printing'!printPrintableOn: s 
	"Watch out : all Intervals cant print themselves.
	You can convert a musicalInterval into a ChromaticInterval.
	E.g. : Interval between Cbb and C ## (super super augmented 
	unison!!!!).
	There are here 40 printable intervals. Cf. method MusicalInterval allPrintableIntervals
(put in protocol constants)"

	type = 1 ifTrue: [s nextPutAll: (#(#superDiminished #Diminished '' #Augmented #superAugmented ) at: semiTones + 3)].
	type = 2 ifTrue: [s nextPutAll: (#(#Diminished #Minor #Major #Augmented ) at: semiTones + 1)].
	type = 3 ifTrue: [s nextPutAll: (#(#Minor #Major ) at: semiTones - 2)].
	type = 4 ifTrue: [s nextPutAll: (#(#Diminished #Perfect #Augmented ) at: semiTones - 3)].
	type = 5 ifTrue: [s nextPutAll: (#(#Diminished #Perfect #Augmented ) at: semiTones - 5)].
	type = 6 ifTrue: [s nextPutAll: (#(#Minor #Major #Augmented ) at: semiTones - 7)].
	type = 7 ifTrue: [s nextPutAll: (#(#Diminished #Minor #Major ) at: semiTones - 8)].
	type = 8 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 10)].
	type = 9 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 12)].
	type = 10 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 14)].
	type = 11 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 15)].
	type = 12 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 17)].
	type = 13 ifTrue: [s nextPutAll: (#(#Diminished '' #Augmented ) at: semiTones - 19)].
	s nextPutAll: ' '.
	s nextPutAll: (#(#unisson #second #third #fourth #fifth #sixth #seventh #octave #ninth #tenth #eleventh #twelvth #thirteenth ) at: type)! !!MusicalInterval methodsFor: 'printing'!storeOn: s
	s nextPutAll: '(', self class name asString.
	s nextPutAll: ' type: ', type printString, ' semiTones: ', semiTones printString, ')'.! !!MusicalInterval methodsFor: 'automatic access'!orientedSemiTones
	^semiTones! !!MusicalInterval methodsFor: 'automatic access'!semiTones
	^semiTones! !!MusicalInterval methodsFor: 'automatic access'!semiTones: aValue
	semiTones := aValue! !!MusicalInterval methodsFor: 'automatic access'!type
   ^type! !!MusicalInterval methodsFor: 'automatic access'!type: aValue
   type := aValue! !!MusicalInterval methodsFor: 'arithmetics'!+ anInterval 
	"(MusicalInterval majorSecond) + (MusicalInterval perfectFifth)"

	| note1 note2 |
	note1 := self topIfBottomIs: PitchClassDo.
	note2 := anInterval topIfBottomIs: note1.
	^PitchClass do intervalBetween: note2! !!MusicalInterval methodsFor: 'testing'!isA: anInterval
	^self = (MusicalInterval perform: anInterval).! !!MusicalInterval methodsFor: 'testing'!isAscending
	^true! !!MusicalInterval methodsFor: 'testing'!isDescending
	^false! !!MusicalInterval methodsFor: 'comparing'!< anInterval
	^semiTones < anInterval semiTones! !!MusicalInterval methodsFor: 'comparing'!<= anInterval
	^semiTones <= anInterval semiTones! !!MusicalInterval methodsFor: 'comparing'!= anInterval 
	^type = anInterval type and: [semiTones = anInterval semiTones]! !!MusicalInterval methodsFor: 'comparing'!> anInterval
	^semiTones > anInterval semiTones! !!MusicalInterval methodsFor: 'comparing'!>= anInterval
	^semiTones >= anInterval semiTones! !!MusicalInterval methodsFor: 'comparing'!hasSameDirectionAs: anInterval
	^(self isAscending and: [anInterval isAscending])
		or: [self isDescending and: [anInterval isDescending]]! !!MusicalInterval methodsFor: 'mutations'!asChromaticInterval
	^ChromaticInterval semiTones: semiTones! !!MusicalInterval methodsFor: 'mutations'!ascending
	^self! !!MusicalInterval methodsFor: 'mutations'!descending
	^MusicalDescendingInterval type: type semiTones: semiTones! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MusicalInterval class	instanceVariableNames: ''!!MusicalInterval class methodsFor: 'examples'!allPrintableIntervals
	"MusicalInterval allPrintableIntervals"

	| m |
	m := MusicalInterval class organization listAtCategoryNamed: 'all-printable' asSymbol.
	^m collect: [:met | MusicalInterval perform: met]! !!MusicalInterval class methodsFor: 'examples'!example
	"
	MusicalInterval majorThird inverse
	
	MusicalInterval perfectFourth topIfBottomIs: N C sharp -> Fa#
	MusicalInterval perfectFourth bottomIfTopIs: N F sharp ->  Do#

	N do intervalBetween: N re -> Major second 
	N do flat intervalBetween: N sol -> Augmented fifth	
	N do flat intervalBetween: N do sharp ->  superAugmented unisson
	N do flat intervalBetween: N do sharp sharp ->  non printable interval
	(N do flat intervalBetween: N do sharp sharp) asChromaticInterval -> Chromatic 3
	(N do intervalBetween: N sol) = (N re intervalBetween: N la) -> true
	(N do intervalBetween: N fa sharp) = (N do intervalBetween: N sol flat)  -> false
	MusicalInterval allIntervalsType: 3 ->  #(Major third Minor third )
	MusicalInterval allIntervalsType: 2 ->
	#(Augmented second Diminished second Major second Minor second )
	MusicalInterval allIntervalsType: 1 -> #(Augmented unisson Diminished unisson superAugmented unisson superDiminished unisson  unisson )
	"! !!MusicalInterval class methodsFor: 'examples'!exampleTranspositions
	"three ways of of transposing pitch classes and od notes"
"(N re  flat) transposeOf: 3 E
(N re  flat) transposeOf: -3 A#
(N re  flat) transposeOf: #fourth Gb
(N re  flat) transposeOf: (MusicalDescendingInterval augmentedOctave)  Dbb
(N re  flat) transposeOf: (MusicalInterval fourth)   Gb


(N re  flat @2) transposeOf: 3 E2
(N re  flat @2) transposeOf: -3 A#1
(N re  flat @2) transposeOf: #fourth Gb2
(N re  flat @2) transposeOf: (MusicalDescendingInterval augmentedOctave)  Dbb1
(N re  flat @2) transposeOf: (MusicalInterval fourth)  Gb2
"! !!MusicalInterval class methodsFor: 'creation'!allIntervalsType: t
	"MusicalInterval allIntervalsType: 2"

	^MusicalInterval allPrintableIntervals select: [:i | i type = t]! !!MusicalInterval class methodsFor: 'creation'!type: aType semiTones: d 
	^(self new) type: aType; semiTones: d! !!MusicalInterval class methodsFor: 'all-printable'!augmentedEleventh
	^self type: 11 semiTones: 18! !!MusicalInterval class methodsFor: 'all-printable'!augmentedFifth
	"8 semitones"

	^self type: 5 semiTones: 8! !!MusicalInterval class methodsFor: 'all-printable'!augmentedFourth
	"6 semitones"

	^self type: 4 semiTones: 6! !!MusicalInterval class methodsFor: 'all-printable'!augmentedNinth
	"15  semitones"

	^self type: 9 semiTones: 15! !!MusicalInterval class methodsFor: 'all-printable'!augmentedOctave
	"3 semitones"

	^self type: 8 semiTones: 13! !!MusicalInterval class methodsFor: 'all-printable'!augmentedSecond
	"3 semitones"

	^self type: 2 semiTones: 3! !!MusicalInterval class methodsFor: 'all-printable'!augmentedTenth
	"17 semitones"

	^self type: 10 semiTones: 17! !!MusicalInterval class methodsFor: 'all-printable'!augmentedTwelvth
	"20 semitones"

	^self type: 12 semiTones: 20! !!MusicalInterval class methodsFor: 'all-printable'!augmentedUnison
	^self type: 1 semiTones: 1! !!MusicalInterval class methodsFor: 'all-printable'!diminishedFifth
	"6 semitones"

	^self type: 5 semiTones: 6! !!MusicalInterval class methodsFor: 'all-printable'!diminishedFourth
	"5 semitones"

	^self type: 4 semiTones: 4! !!MusicalInterval class methodsFor: 'all-printable'!diminishedNinth
	"1 octave + 1  semitones"

	^self type: 9 semiTones: 13! !!MusicalInterval class methodsFor: 'all-printable'!diminishedOctave
	"11 semitones"

	^self type: 8 semiTones: 11! !!MusicalInterval class methodsFor: 'all-printable'!diminishedSecond
	"0 semitones"

	^self type: 2 semiTones: 0! !!MusicalInterval class methodsFor: 'all-printable'!diminishedSeventh
	"9  semitones"

	^self type: 7 semiTones: 9! !!MusicalInterval class methodsFor: 'all-printable'!diminishedTenth
	"15 semitones"

	^self type: 10 semiTones: 15! !!MusicalInterval class methodsFor: 'all-printable'!diminishedThirteenth
	^self type: 13 semiTones: 20! !!MusicalInterval class methodsFor: 'all-printable'!diminishedTwelvth
	"18 semitones"

	^self type: 12 semiTones: 18! !!MusicalInterval class methodsFor: 'all-printable'!diminishedUnison
	"MusicalInterval diminishedUnison topIfBottomIs: N C"

	^self type: 1 semiTones: -1! !!MusicalInterval class methodsFor: 'all-printable'!majorNinth
	"14 semitones"

	^self type: 9 semiTones: 14! !!MusicalInterval class methodsFor: 'all-printable'!majorSecond
	"2 semitones"

	^self type: 2 semiTones: 2! !!MusicalInterval class methodsFor: 'all-printable'!majorSeventh
	"11  semitones"
	^self type: 7 semiTones: 11! !!MusicalInterval class methodsFor: 'all-printable'!majorSixth
	"9  semitones"

	^self type: 6 semiTones: 9! !!MusicalInterval class methodsFor: 'all-printable'!majorTenth
	"14 semitones"

	^self type: 10 semiTones: 16! !!MusicalInterval class methodsFor: 'all-printable'!majorThird
	"4 semitones"

	^self type: 3 semiTones: 4! !!MusicalInterval class methodsFor: 'all-printable'!majorThirteenth
	^self type: 13 semiTones: 21! !!MusicalInterval class methodsFor: 'all-printable'!majorTwelvth
	"19 semitones"

	^self type: 12 semiTones: 19! !!MusicalInterval class methodsFor: 'all-printable'!minorNinth
	"1 octave + 1  semitones"

	^self type: 9 semiTones: 13! !!MusicalInterval class methodsFor: 'all-printable'!minorSecond
	"1 semitone"

	^self type: 2 semiTones: 1! !!MusicalInterval class methodsFor: 'all-printable'!minorSeventh
	"10  semitones"

	^self type: 7 semiTones: 10! !!MusicalInterval class methodsFor: 'all-printable'!minorSixth
	"8 semitones"

	^self type: 6 semiTones: 8! !!MusicalInterval class methodsFor: 'all-printable'!minorThird
	"3 semitones"

	^self type: 3 semiTones: 3! !!MusicalInterval class methodsFor: 'all-printable'!minorThirteenth
	^self type: 13 semiTones: 20! !!MusicalInterval class methodsFor: 'all-printable'!octave
	^self type: 8 semiTones: 12! !!MusicalInterval class methodsFor: 'all-printable'!perfectEleventh
	"5 semitones"

	^self type: 11 semiTones: 17! !!MusicalInterval class methodsFor: 'all-printable'!perfectFifth
	"7 semitones"

	^self type: 5 semiTones: 7! !!MusicalInterval class methodsFor: 'all-printable'!perfectFourth
	"5 semitones"

	^self type: 4 semiTones: 5! !!MusicalInterval class methodsFor: 'all-printable'!superAugmentedUnison
	"MusicalInterval superAugmentedUnison topIfBottomIs: N C"

	^self type: 1 semiTones: 2! !!MusicalInterval class methodsFor: 'all-printable'!superDiminishedUnison
	"MusicalInterval superDiminishedUnison topIfBottomIs: N C"

	^self type: 1 semiTones: -2! !!MusicalInterval class methodsFor: 'all-printable'!unison
	^self type: 1 semiTones: 0! !!MusicalInterval class methodsFor: 'constants nick-names'!aug11
	^self augmentedEleventh! !!MusicalInterval class methodsFor: 'constants nick-names'!aug4
	^self augmentedFourth! !!MusicalInterval class methodsFor: 'constants nick-names'!aug5
	^self augmentedFifth! !!MusicalInterval class methodsFor: 'constants nick-names'!aug9
	"15 semitones"

	^self augmentedNinth! !!MusicalInterval class methodsFor: 'constants nick-names'!dim13
	^self diminishedThirteenth! !!MusicalInterval class methodsFor: 'constants nick-names'!dim5
	"6 semitones"

	^self diminishedFifth! !!MusicalInterval class methodsFor: 'constants nick-names'!dim7
	"9  semitones"

	^self diminishedSeventh! !!MusicalInterval class methodsFor: 'constants nick-names'!dim9

	^self diminishedNinth! !!MusicalInterval class methodsFor: 'constants nick-names'!eleventh
	^self type: 11 semiTones: 17! !!MusicalInterval class methodsFor: 'constants nick-names'!fifth
	^self perfectFifth! !!MusicalInterval class methodsFor: 'constants nick-names'!flatFifth
	^self diminishedFifth! !!MusicalInterval class methodsFor: 'constants nick-names'!flatNinth
	"1 octave + 1  semitones"

	^self diminishedNinth! !!MusicalInterval class methodsFor: 'constants nick-names'!flatThirteenth
	^self diminishedThirteenth! !!MusicalInterval class methodsFor: 'constants nick-names'!fourth
	^self perfectFourth! !!MusicalInterval class methodsFor: 'constants nick-names'!ninth
	^self majorNinth! !!MusicalInterval class methodsFor: 'constants nick-names'!second
	"2 semitones"

	^self majorSecond! !!MusicalInterval class methodsFor: 'constants nick-names'!seventh
	^self minorSeventh! !!MusicalInterval class methodsFor: 'constants nick-names'!sixth
	^self majorSixth! !!MusicalInterval class methodsFor: 'constants nick-names'!thirteenth
	^self majorThirteenth! !MusicalInterval subclass: #ChromaticInterval	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!ChromaticInterval commentStamp: '<historical>' prior: 0!ChromaticInterval commentStamp: '<historical>' prior: 0!!
used to produce intervals with no names, measured only in terms
of a number of semie tones (for non-tonal music)!!ChromaticInterval methodsFor: 'printing'!printOn: s 
	s nextPutAll: 'Chromatic ' , semiTones printString! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!ChromaticInterval class	instanceVariableNames: ''!!ChromaticInterval class methodsFor: 'examples'!examples
"(N C octave: 3) transposeBySemiTones: 4  ->  E3"! !!ChromaticInterval class methodsFor: 'creation'!semiTones: x 
	"anInterval without type, only chromatic"

	^self type: -1 semiTones: x! !MusicalInterval subclass: #DiatonicInterval	instanceVariableNames: 'ascending'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!DiatonicInterval methodsFor: 'private'!ascending: t 
	ascending := t! !!DiatonicInterval methodsFor: 'computing notes'!bottomIfTopIs: aNote 
	"yields the note making the interval self with aNote"

	^ascending
		ifTrue: [aNote preceding]
		ifFalse: [aNote following]! !!DiatonicInterval methodsFor: 'computing notes'!topIfBottomIs: aNote 
	"yields the note making the interval self with aNote"

	^ascending ifFalse: [aNote preceding]
		ifTrue: [aNote following]! !!DiatonicInterval methodsFor: 'printing'!printOn: s 
	s nextPutAll: 'Diatonic interval '! !!DiatonicInterval methodsFor: 'initialize'!initialize
	ascending := true! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DiatonicInterval class	instanceVariableNames: ''!!DiatonicInterval class methodsFor: 'creation'!ascending
	^DiatonicInterval new ascending: true! !!DiatonicInterval class methodsFor: 'creation'!descending
	^DiatonicInterval new ascending: false! !!DiatonicInterval class methodsFor: 'creation'!new
	^super new initialize! !MusicalInterval subclass: #MusicalDescendingInterval	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!MusicalDescendingInterval commentStamp: '<historical>' prior: 0!La theorie des intervalles "descendants" n'est pas tres claire:
Que voudrait dire DescendingInterval topIfBottomIs: N C ?
(quelle difference avec ascending ?).
Introduit juste pour les besoins de la cause, mais un peu incoherent.
Herite tout de MusicalInterval, et donc se comporte presque pareil.
Utilisee uniquement en creation dans la methode intervalBetween: de OctaveDependentNote.
Il faudrait d'ailleurs modifier aussi la methode similaire de PitchClass!!MusicalDescendingInterval methodsFor: 'testing'!isAscending
	^false! !!MusicalDescendingInterval methodsFor: 'testing'!isDescending
	^true! !!MusicalDescendingInterval methodsFor: 'computing notes'!bottomIfTopIs: aNote 
	"yields the note for which I am the interval self"
self halt.
	^super topIfBottomIs: aNote! !!MusicalDescendingInterval methodsFor: 'computing notes'!topIfBottomIs: aNote 
	"yields the note for which I am the interval self"

	^super bottomIfTopIs: aNote! !!MusicalDescendingInterval methodsFor: 'mutations'!ascending
	^MusicalInterval type: type semiTones: semiTones! !!MusicalDescendingInterval methodsFor: 'mutations'!descending
	^self! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!MusicalDescendingInterval class	instanceVariableNames: ''!!MusicalDescendingInterval class methodsFor: 'examples'!example

"(N C octave: 3) intervalBetween: (N D octave: 2) descending Minor seventh
(N B flat octave: 3) intervalBetween: (N B flat flat octave: 2) descending augmented octave"! !Object subclass: #OctaveDependentNote	instanceVariableNames: 'oct pc midiPitch natural'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!OctaveDependentNote commentStamp: '<historical>' prior: 0!Instances of this class represent octave dependent notes.
	pc : the pitch class of the note (takes enharmonic spelling into account)
	octave: integer from 0 to .., represent the octave. 
Middle C is therefore represented by 
	pc : C
	octave: 4
Defines methods to compute intervals, midiPitches, and lots
of thing that PitchClass implements too.
So why not make this class and PitchClass have
a common superclass, to factor out common computations ?
Ontological questions :
	how to compute an interval between two octave-dependent notes,
since their ambitus may be arbitrarily large (no question of computing 25th intervals).
So intervals should be reduced to thirteenth, i.e. :
	octaves should be taken into account only in a 0/1 manner!!OctaveDependentNote methodsFor: 'access'!oct
	^oct! !!OctaveDependentNote methodsFor: 'access'!oct: o 
	oct := o.
	midiPitch := nil.
	natural := nil! !!OctaveDependentNote methodsFor: 'access'!octave: o
	self oct: o.! !!OctaveDependentNote methodsFor: 'access'!pitchClass
	^pc! !!OctaveDependentNote methodsFor: 'access'!pitchClass: aPc 
	pc := aPc.
	midiPitch := nil.
	natural := nil! !!OctaveDependentNote methodsFor: 'alterations'!flat
	^pc flat octave: oct! !!OctaveDependentNote methodsFor: 'alterations'!isDoubleFlat
	^pc isDoubleFlat! !!OctaveDependentNote methodsFor: 'alterations'!isDoubleSharp
	^pc isDoubleSharp! !!OctaveDependentNote methodsFor: 'alterations'!isFlat
	^pc isFlat! !!OctaveDependentNote methodsFor: 'alterations'!isNatural
	^pc isNatural! !!OctaveDependentNote methodsFor: 'alterations'!isSharp
	^pc isSharp! !!OctaveDependentNote methodsFor: 'alterations'!natural
	natural == nil ifTrue: [natural := pc natural octave: oct].
	^natural! !!OctaveDependentNote methodsFor: 'alterations'!sharp
	^pc sharp octave: oct! !!OctaveDependentNote methodsFor: 'printing'!printOn: s 
	pc printOn: s.
	s nextPutAll: oct printString.! !!OctaveDependentNote methodsFor: 'printing'!storeOn: s 
	"(N C sharp sharp octave: 3) storeString = '(N C sharp sharp @ 3)'"

	s nextPut: $(.
	s nextPutAll: pc storeString.
	s nextPutAll: ' @ ' , oct printString.
	s nextPut: $)! !!OctaveDependentNote methodsFor: 'intervals'!closestEnharmonic

	| new |
	new :=  self pitchClass enharmonic octave: oct.
	new midiPitch = self midiPitch
		ifTrue: [^new].
	new midiPitch < self midiPitch
		ifTrue: [^new octave: oct + 1].
	new midiPitch > self midiPitch
		ifTrue: [^new octave: oct - 1].! !!OctaveDependentNote methodsFor: 'intervals'!diatonicStepsTo: aNote 
	"returns the number of diatonic steps to aNote"
	"(N B octave: 3) diatonicStepsTo: (N C octave: 4)"

	| all |
	all := PitchClass naturalNotes.
	^((aNote oct - oct) * 7) + ((all indexOf: aNote pitchClass natural) - (all indexOf: pc natural))! !!OctaveDependentNote methodsFor: 'intervals'!following
	"the next diatonic note of self's natural note"
	"(N C octave: 2) following"
	| nat |
	nat := self natural.
	^ nat pitchClass following octave: (nat pitchClass = N si
			ifTrue: [oct + 1]
			ifFalse: [oct])! !!OctaveDependentNote methodsFor: 'intervals'!newdiatonicStepsTo: aNote 
	"returns the number of diatonic steps to aNote"
	"(N B octave: 3) diatonicStepsTo: (N C octave: 4)"

	| all |
	all := PitchClass naturalNotes.
	^((aNote oct - oct) * 7) + ((all indexOf: aNote pitchClass natural) - (all indexOf: pc natural))! !!OctaveDependentNote methodsFor: 'intervals'!nthFollowing: i 
	"yields the nth diatonic note following self. 
	Takes octave shifts into account"

	| nth |
	(i < 0) ifTrue: [^self nthPreceding: i negated].
	nth := self.
	i timesRepeat: [nth := nth following].
	^nth! !!OctaveDependentNote methodsFor: 'intervals'!nthPreceding: i 
	"yields the nth diatonic note preceding self. 
	Takes octave shifts into account"

	| nth |
	(i < 0) ifTrue: [^self nthFollowing: i negated].
	nth := self.
	i timesRepeat: [nth := nth preceding].
	^nth! !!OctaveDependentNote methodsFor: 'intervals'!oldEquals: x 
	"important pour l'integrite des operations sur les melodies"

	^self == x! !!OctaveDependentNote methodsFor: 'intervals'!olddiatonicStepsTo: aNote 
	"returns the number of diatonic steps to aNote"
	"(N C octave: 4) diatonicStepsTo: (N D octave: 2)"

	| b b2 n |
	n := 0.
	b := self natural.
	b2 := aNote natural. self halt.
	b <= b2
		ifTrue: 
			[[b2 <= b]
				whileFalse: 
					[b := b following.
					n := n + 1].
			^n].
	b >= b2
		ifTrue: 
			[[b2 >= b]
				whileFalse: 
					[b := b preceding.
					n := n - 1].
			^n]! !!OctaveDependentNote methodsFor: 'intervals'!preceding
	"the next diatonic note of self's natural note"
	"(N C sharp octave: 3) preceding"
	| nat |
	nat := self natural.
	^nat pitchClass preceding octave: (nat pitchClass == PitchClassDo
			ifTrue: [oct - 1]
			ifFalse: [oct])! !!OctaveDependentNote methodsFor: 'intervals'!semiToneCount
	"related to OctaveDependentNote fromMidiPitch"

	^pc semiToneCount + (oct + 2 * 12) - 1! !!OctaveDependentNote methodsFor: 'intervals'!semiTonesWith: aNote
	^aNote semiToneCount - self semiToneCount! !!OctaveDependentNote methodsFor: 'interval testing'!intervalBetween: aNote 
	"returns the interval between the two octaveDependentNotes. 
		Assume that aNote is really a OctaveDependentNote"

	| type |

	type := self intervalTypeBetween: aNote.
	(self natural < aNote natural
		or: [self natural = aNote natural 
			and: [self <= aNote]])
		ifTrue: [^MusicalInterval type: type semiTones: (self semiTonesWith: aNote)].
	(self natural > aNote natural
		or: [self natural = aNote natural 
			and: [self > aNote]])
		ifTrue: [^MusicalDescendingInterval type: type semiTones: (aNote semiTonesWith: self)].
	^self error: 'it doesnt work'.! !!OctaveDependentNote methodsFor: 'interval testing'!intervalTypeBetween: aNote 
	"returns the type of the interval between the two notes modulo one 
	octave
	(N C octave: 6) intervalTypeBetween: (N re octave: 3)	"

	| b b2 type |
	type := 1.
	b := self natural.
	b2 := aNote natural.
	b <= b2
		ifTrue: 
			["ascending interval"
			[b2 midiPitchEquals: b]
			whileFalse: 
				[b := b following.
				type := type + 1]]
		ifFalse: 
			["descending interval"
			[b2 midiPitchEquals: b]
			whileFalse: 
				[b := b preceding.
				type := type + 1]].
	^type! !!OctaveDependentNote methodsFor: 'interval testing'!isLessThanA: interval from: od
	" ascending or descending direction"
	| int |
	interval isSymbol
		ifTrue: [int := MusicalInterval perform: interval]
		ifFalse: [int := interval].
	^(self intervalBetween: od)  < int! !!OctaveDependentNote methodsFor: 'interval testing'!realIntervalTypeBetween: aNote 
	"returns the type of the interval between the two notes	
	(N C octave: 6) realIntervalTypeBetween: (N C octave: 3) "

	| b b2 type temp |
	type := 1.
	b := self natural.
	b2 := aNote natural.
	b > b2
		ifTrue: 
			[temp := b2.
			b2 := b.
			b := temp].
	[b2 oct = b oct and: [b2 pitchClass = b pitchClass]]
		whileFalse: 
			[b := b following.
			type := type + 1].
	^type! !!OctaveDependentNote methodsFor: 'interval testing'!testIntervalsFrom: pitch1 to: pitch2
	"(N C sharp @ 3) testIntervalsFrom: (N A @ 1) to: (N A @ 4)"
	| odn ena |
	pitch1 midiPitch to: pitch2 midiPitch do: [:mp |
		odn := OctaveDependentNote fromMidiPitch: mp.
		Transcript show: self printString, ' intervalBetween: ', odn printString, ' = ' ,
					(self intervalBetween: odn) printString; cr.
		ena := odn closestEnharmonic.
		ena = odn
			ifFalse: [Transcript show: self printString, ' intervalBetween: ', ena printString, ' = ' ,
					(self intervalBetween: ena) printString; cr]]! !!OctaveDependentNote methodsFor: 'public intervals'!alterate: note toReach: i 
	"Join semitons to note in order to obtain the required number of 
	semitones with self. 
	Redefined here because interval computation must take octave 
	into account ..."

	| delta |
	delta := (self semiTonesWith: note) - i.
	delta = 0 ifTrue: [^note].
	delta = 1 ifTrue: [^note flat].
	delta = -1 ifTrue: [^note sharp].
	delta = 2 ifTrue: [^note flat flat].
	delta = -2 ifTrue: [^note sharp sharp].
	delta = 3 ifTrue: [^note flat flat flat].	"to deal with cases like N B flat flat augmentedOctave"
	delta = -3 ifTrue: [^note sharp sharp sharp].
	^self error: 'illegal interval !!'! !!OctaveDependentNote methodsFor: 'public intervals'!alterateBelow: note toReach: i 
	"Join semitons to note in order to obtain the required number of 
	semitones with self. 
	Redefined here because interval computation must take octave 
	into account ..."

	| delta |
	delta := (note semiTonesWith: self) - i.
	delta = 0 ifTrue: [^note].
	delta = 1 ifTrue: [^note sharp].
	delta = -1 ifTrue: [^note flat].
	delta = 2 ifTrue: [^note sharp sharp ].
	delta = -2 ifTrue: [^note flat flat].
	delta = 3 ifTrue: [^note flat flat flat].	"to deal with cases like N B flat flat augmentedOctave"
	delta = -3 ifTrue: [^note sharp sharp sharp].
	^self error: 'illegal interval !!'! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedEleventh
	^MusicalInterval augmentedEleventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedFifth
	^MusicalInterval augmentedFifth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedFourth
	^MusicalInterval augmentedFourth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedNinth
	^MusicalInterval augmentedNinth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedOctave
	^MusicalInterval augmentedOctave topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!augmentedSecond
	^MusicalInterval augmentedSecond topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!diminishedFifth
	^MusicalInterval diminishedFifth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!diminishedNinth
	^MusicalInterval diminishedNinth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!diminishedOctave
	^MusicalInterval diminishedOctave topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!diminishedSeventh
	^MusicalInterval diminishedSeventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!diminishedThirteenth
	^MusicalInterval diminishedThirteenth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!eleventh
	^MusicalInterval eleventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!fifth
	^MusicalInterval fifth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!flatFifth
	^MusicalInterval flatFifth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!flatNinth
	^MusicalInterval flatNinth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!flatThirteenth
	^MusicalInterval flatThirteenth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!fourth
	^MusicalInterval fourth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorNinth
	^MusicalInterval majorNinth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorSecond
	^MusicalInterval majorSecond topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorSeventh
	^MusicalInterval majorSeventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorSixth
	^MusicalInterval majorSixth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorThird
	^MusicalInterval majorThird topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!majorThirteenth
	^MusicalInterval majorThirteenth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorNinth
	^MusicalInterval minorNinth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorSecond
	^MusicalInterval minorSecond topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorSeventh
	^MusicalInterval minorSeventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorSixth
	^MusicalInterval minorSixth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorThird
	^MusicalInterval minorThird topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!minorThirteenth
	^MusicalInterval minorThirteenth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!ninth
	^MusicalInterval ninth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!octave
	^MusicalInterval octave topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!perfectEleventh
	^MusicalInterval perfectEleventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!perfectFifth
	^MusicalInterval perfectFifth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!perfectFourth
	^MusicalInterval perfectFourth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!second
	^MusicalInterval second topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!seventh
	^MusicalInterval seventh topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!sixth
	^MusicalInterval sixth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!thirteenth
	^MusicalInterval thirteenth topIfBottomIs: self! !!OctaveDependentNote methodsFor: 'public intervals'!unison
	^self! !!OctaveDependentNote methodsFor: 'transpose'!beDownAnOctave
	^self octave: oct - 1! !!OctaveDependentNote methodsFor: 'transpose'!beTransposedBy: i 
	| n |
	n := self transposeOf: i.
	oct := n oct.
	pc := n pitchClass.
	midiPitch := n midiPitch.
	natural := nil! !!OctaveDependentNote methodsFor: 'transpose'!beTransposedOneStepDown
	"used by the score editor as diatonic step!!"

	| n |
	n := self preceding.
	oct := n oct.
	pc := n pitchClass.
	midiPitch := nil.
	natural := nil! !!OctaveDependentNote methodsFor: 'transpose'!beTransposedOneStepUp
	"used by the score editor as diatonic step!!"

	| n |
	n := self following.
	oct := n oct.
	pc := n pitchClass.
	midiPitch := nil.
	natural := nil! !!OctaveDependentNote methodsFor: 'transpose'!beUpAnOctave
	^self octave: oct + 1! !!OctaveDependentNote methodsFor: 'transpose'!downAnOctave
	^OctaveDependentNote octave: oct - 1 pc: pc! !!OctaveDependentNote methodsFor: 'transpose'!setPitchTo: aPitch 
	oct := aPitch oct.
	pc := aPitch pitchClass.
	midiPitch := aPitch midiPitch.
	natural := nil! !!OctaveDependentNote methodsFor: 'transpose'!switchTo: aNote 
	pc := aNote pitchClass.
	oct := aNote oct.
	midiPitch := aNote midiPitch.
	natural := nil! !!OctaveDependentNote methodsFor: 'transpose'!transposeOf: anInterval 
	"three cases: integer (+/-), method name (= ascending interval), or interval 
	object"

	anInterval isInteger ifTrue: [^(ChromaticInterval semiTones: anInterval)
			from: self].
	anInterval isSymbol ifTrue: [^self perform: anInterval].
	(anInterval isKindOf: MusicalInterval) ifTrue: [^anInterval topIfBottomIs: self].
	^self error: 'type d''intervalle indefini'! !!OctaveDependentNote methodsFor: 'transpose'!upAnOctave
	^OctaveDependentNote octave: oct + 1 pc: pc! !!OctaveDependentNote methodsFor: 'comparing'!< aNote
	^self midiPitch < aNote midiPitch! !!OctaveDependentNote methodsFor: 'comparing'!<= aNote
	^self midiPitch <= aNote midiPitch! !!OctaveDependentNote methodsFor: 'comparing'!= x 

	^(pc == x pitchClass) and: [oct == x oct]! !!OctaveDependentNote methodsFor: 'comparing'!> aNote
	^self midiPitch > aNote midiPitch! !!OctaveDependentNote methodsFor: 'comparing'!>= aNote
	^self midiPitch >= aNote midiPitch! !!OctaveDependentNote methodsFor: 'comparing'!hash
	^super hash * oct! !!OctaveDependentNote methodsFor: 'comparing'!isNearerTo: n1 than: n2

	^(self semiTonesWith: n1) abs < (self semiTonesWith: n2) abs! !!OctaveDependentNote methodsFor: 'comparing'!isbetween: n1 and: n2
	^n1 < self and: [self < n2]! !!OctaveDependentNote methodsFor: 'comparing'!midiPitchEquals: aNote
	^self midiPitch = aNote midiPitch! !!OctaveDependentNote methodsFor: 'comparing'!pitchEqual: aNote 
	^self midiPitchEquals: aNote! !!OctaveDependentNote methodsFor: 'comparing'!theNearestPitch: list
	"(N A flat @ 2) theNearestPitch: (Array with:  (N C  @ 2) with: (N C @ 3))"
	| nearest |
	nearest := list at: 1.
	2 to: list size do: 
		[:i |
		(self isNearerTo: (list at: i) than: nearest)
			ifTrue: [nearest :=  (list at: i)]].
	^nearest! !!OctaveDependentNote methodsFor: 'mutations'!duration: d
	^MusicEvent dur: d pitch: self! !!OctaveDependentNote methodsFor: 'leading'!isLeadingToneTo: n
	| int |
	int := (self intervalBetween: n).
	^int type = 2 
		or: [int semiTones = 1]! !!OctaveDependentNote methodsFor: 'leading'!lowerLeadingToneInScale: s
	^s noteBefore: self! !!OctaveDependentNote methodsFor: 'leading'!upperLeadingToneInScale: s
	^s noteAfter: self! !!OctaveDependentNote methodsFor: 'modifying'!inTessitura: qualTessitura
	"modify octave according to the given tessitura"

	self oct: (pc inTessitura: qualTessitura) oct! !!OctaveDependentNote methodsFor: 'accessing delegation'!downAndUpOctaves: odNote
	^pc downAndUpOctaves: odNote! !!OctaveDependentNote methodsFor: 'constraining'!intervalTypeModuloOctaveBetween: n 



	^(self intervalTypeBetween: n) - 1 \\ 7+ 1! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OctaveDependentNote class	instanceVariableNames: ''!!OctaveDependentNote class methodsFor: 'creation'!from: aNote to: n2 
	| r b |
	r := OrderedCollection new.
	b := aNote.
	r add: b.
	[b following >= n2]
		whileFalse: 
			[b := b following.
			r add: b].
	^r! !!OctaveDependentNote class methodsFor: 'creation'!fromMidiPitch: n 
	"yields the note that has n as midiPitch. Only natural and sharp 
	notes are created as there is no possible way of knowing the 
	intention"
	"C3 is 60.
	C0 is 24
	C-2 is 0"
"OctaveDependentNote fromMidiPitch: 24"

	| o delta  |
	o := (n  quo: 12) - 2.  
	delta := (n + 1  \\ 12).
	delta = 1 ifTrue: [^(N C  octave: o)].
	delta = 2 ifTrue: [^(N C sharp octave: o)].
	delta = 3 ifTrue: [^(N D octave: o)].
	delta = 4 ifTrue: [^(N D sharp octave: o)].
	delta = 5 ifTrue: [^(N E octave: o)].
	delta = 6 ifTrue: [^(N F octave: o)].
	delta = 7 ifTrue: [^(N F sharp octave: o)].
	delta = 8 ifTrue: [^(N G octave: o)].
	delta = 9 ifTrue: [^(N G sharp octave: o)].
	delta = 10 ifTrue: [^(N A octave: o)].
	delta = 11 ifTrue: [^(N A sharp octave: o)].
	delta = 0 ifTrue: [^(N B octave: o)]! !!OctaveDependentNote class methodsFor: 'creation'!named: s 
	"self named: 'Re#2'"

	| o |
	o := s last.
	^(PitchClass noteNamed: (s copyFrom: 1 to: s size - 1))
		octave: o asInteger - 48! !!OctaveDependentNote class methodsFor: 'creation'!octave: o pc: pc
	^self new oct: o; pitchClass: pc! !!OctaveDependentNote class methodsFor: 'examples'!examples
"(N do octave: 3) minorThird sharp sharp E#3

(N C octave: 3) semiTonesWith: (N la  octave: 4) 21

(#(do re mi fa sol la si ) collect: [:x | (N perform: x) octave: 3])
	collect: [:y | y - #octave]

(N B octave: 3) intervalBetween: (N B flat octave: 4)
(N B octave: 3) intervalBetween: (N B flat octave: 2)

(N C octave: 3) intervalBetween: (N C octave: 2)    descending  octave
(N C octave: 2) intervalBetween: (N C octave: 3)   octave
 "! !!OctaveDependentNote class methodsFor: 'vocal ranges'!altoRange
	"retourne la collection des notes de l'alto"

	^self from: (N sol octave: 3)
		to: (N re octave: 5)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!altoRangeInScale: aScale
	"OctaveDependentNote sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N sol octave: 3)
		to: (N re octave: 5)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!baseRange
	"retourne la collection des notes de la basse"

	^self from: (N sol octave: 2)
		to: (N re octave: 4)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!baseRangeInScale: aScale
	"OctaveDependentNote sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N sol octave: 2)
		to: (N re octave: 4)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!sopranoRange
	"retourne la collection des notes du soprano"

	^self from: (N re octave: 4)
		to: (N do octave: 6)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!sopranoRangeInScale: aScale
	"OctaveDependentNote sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N re octave: 4)
		to: (N do octave: 6)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!tenorRange
	"retourne la collection des notes du tenor"

	^self from: (N re octave: 3)
		to: (N la octave: 4)! !!OctaveDependentNote class methodsFor: 'vocal ranges'!tenorRangeInScale: aScale
	"OctaveDependentNote sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N re octave: 3)
		to: (N la octave: 4)! !Object subclass: #PitchClass	instanceVariableNames: ''	classVariableNames: 'A AllNatural AllNotes B C D E English F G'	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClass commentStamp: '<historical>' prior: 0!Instances of (subclasses of) this class represent pitch-classes.
A pitch class is an octave-independent note.
There are 35 (sub) instances of this class.
Octave-dependent notes are represented by instances of class
OctaveDependentNote.
Ideally PitchClass should be a metaclass, so that its instances
be classes, and octave-dependent notes could then be instances
of pitchClasses !!!!
Unfortunately this is not possible straightforwadly in Smaltalk, so we
use aggregation instead to represent octave-dependent notes!!PitchClass methodsFor: 'private intervals'!descendingNumberOfSemiTonesBetween: aNote 
	"Important method in the theory. It is a 3-stage computation of the interval between 2 notes"

	^self semiTonesToNatural + 
	(self natural semiTonesWithNaturalNoteBelow:  aNote natural) - 
	aNote semiTonesToNatural! !!PitchClass methodsFor: 'private intervals'!descendingSemiTonesToNatural
	^self semiTonesToNatural negated! !!PitchClass methodsFor: 'private intervals'!intervalBetween: aNote 
	"returns the interval between the two notes"

	|  b b2 type |
	type := 1.
	b := self natural.
	b2 := aNote natural.
	[b2 = b] whileFalse: 
			[b := b following.
			type := type + 1].
	^MusicalInterval type: type semiTones: (self numberOfSemiTonesBetween: aNote)! !!PitchClass methodsFor: 'private intervals'!intervalTypeBetween: aNote 
	"returns the interval between the two notes"
	| b b2 type |
	type := 1.
	b := self natural.
	b2 := aNote natural.
	[b2 = b]
		whileFalse: 
			[b := b following.
			type := type + 1].
	^ type! !!PitchClass methodsFor: 'private intervals'!nthFollowing: i
	"returns the i th natural note following self"

	| nth |
	nth := self.
	i timesRepeat: [nth := nth following].
	^nth! !!PitchClass methodsFor: 'private intervals'!nthPreceding: i
	"returns the i th natural note preceding self"

	| nth |
	nth := self.
	i timesRepeat: [nth := nth preceding].
	^nth! !!PitchClass methodsFor: 'private intervals'!numberOfSemiTonesBetween: aNote 
	"Important method in the theory. It is a 3-stage computation of the interval between 2 notes"

	^self semiTonesToNatural + 
	(self natural semiTonesWithNaturalNote:  aNote natural) - 
	aNote semiTonesToNatural! !!PitchClass methodsFor: 'private intervals'!semiToneCount
	^self subclassResponsibility! !!PitchClass methodsFor: 'private intervals'!smallestIntervalBetween: aNote 
	"returns the descending interval between the two notes"
	self semiToneCount <= aNote semiToneCount
		ifTrue: [^self intervalBetween: aNote]
		ifFalse: [^self descendingIntervalBetween: aNote]! !!PitchClass methodsFor: 'public intervals'!alterate: note toReach: i 
	self subclassResponsibility! !!PitchClass methodsFor: 'public intervals'!alterateBelow: note toReach: i 
	self subclassResponsibility! !!PitchClass methodsFor: 'public intervals'!augmentedEleventh
	^MusicalInterval augmentedEleventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!augmentedFifth
	^MusicalInterval augmentedFifth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!augmentedFourth
	^MusicalInterval augmentedFourth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!augmentedNinth
	^MusicalInterval augmentedNinth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!augmentedSecond
	^MusicalInterval augmentedSecond topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!augmentedUnison
	^MusicalInterval augmentedUnison topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!diminishedFifth
	^MusicalInterval diminishedFifth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!diminishedNinth
	^MusicalInterval diminishedNinth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!diminishedSecond
	^MusicalInterval minorSecond topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!diminishedSeventh
	^MusicalInterval diminishedSeventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!diminishedThirteenth
	^MusicalInterval diminishedThirteenth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!eleventh
	^MusicalInterval eleventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!fifth
	^MusicalInterval fifth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!flatFifth
	^MusicalInterval flatFifth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!flatNinth
	^MusicalInterval flatNinth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!flatThirteenth
	^MusicalInterval flatThirteenth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!fourth
	^MusicalInterval fourth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorNinth
	^MusicalInterval majorNinth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorSecond
	^MusicalInterval majorSecond topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorSeventh
	^MusicalInterval majorSeventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorSixth
	^MusicalInterval majorSixth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorThird
	^MusicalInterval majorThird topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!majorThirteenth
	^MusicalInterval majorThirteenth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorNinth
	^MusicalInterval minorNinth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorSecond
	^MusicalInterval minorSecond topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorSeventh
	^MusicalInterval minorSeventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorSixth
	^MusicalInterval minorSixth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorThird
	^MusicalInterval minorThird topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!minorThirteenth
	^MusicalInterval minorThirteenth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!ninth
	^MusicalInterval ninth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!octave
	^self! !!PitchClass methodsFor: 'public intervals'!perfectEleventh
	^MusicalInterval perfectEleventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!perfectFifth
	^MusicalInterval perfectFifth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!perfectFourth
	^MusicalInterval perfectFourth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!second
	^MusicalInterval second topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!seventh
	^MusicalInterval seventh topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!sixth
	^MusicalInterval sixth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!thirteenth
	^MusicalInterval thirteenth topIfBottomIs: self! !!PitchClass methodsFor: 'public intervals'!unison
	^self! !!PitchClass methodsFor: 'testing'!isDoubleFlat
	^false! !!PitchClass methodsFor: 'testing'!isDoubleSharp
	^false! !!PitchClass methodsFor: 'testing'!isFlat
	^false! !!PitchClass methodsFor: 'testing'!isNatural
	^true! !!PitchClass methodsFor: 'testing'!isSharp
	^false! !!PitchClass methodsFor: 'testing'!pitchEqual: aNote 
	^(self semiToneCount - aNote semiToneCount) \\ 12 = 0! !!PitchClass methodsFor: 'chord creation'!chordFromTokens: st 
	^(PitchClassChord new) root: self; structure: st! !!PitchClass methodsFor: 'chord creation'!majorTriad
	^PitchClassChord root: self structure: ''! !!PitchClass methodsFor: 'chord creation'!minorTriad
	^PitchClassChord root: self structure: #(#min )! !!PitchClass methodsFor: 'mutations'!@ o 
	(o isKindOf: Integer)
		ifFalse: [^super @ o].
	^self octave: o! !!PitchClass methodsFor: 'mutations'!chordFromString: st 
	^self chordFromTokens: (Scanner new scanTokens: st)! !!PitchClass methodsFor: 'mutations'!inTessitura: qualTessitura
	"N C pitchInTessitura: QTessitura high"
	| tv octs |
	tv := qualTessitura typicalValue.
	octs := self downAndUpOctaves: tv.
	(tv isNearerTo: (octs at: 1) than: (octs at: 2))
		ifTrue: [^octs at: 1]
		ifFalse: [^octs at: 2].! !!PitchClass methodsFor: 'mutations'!octave: o
	^OctaveDependentNote octave: o pc: self! !!PitchClass methodsFor: 'scales'!chromaticScale
	^ChromaticScale root: self! !!PitchClass methodsFor: 'scales'!harmonicMinorScale
	^HarmonicMinorScale root: self! !!PitchClass methodsFor: 'scales'!majorScale
	^MajorScale root: self! !!PitchClass methodsFor: 'scales'!melodicMinorScale
	^MelodicMinorScale root: self! !!PitchClass methodsFor: 'scales'!minorScale
	^self harmonicMinorScale! !!PitchClass methodsFor: 'scales'!pseudoMinorScale
	^PseudoMinorScale root: self! !!PitchClass methodsFor: 'making octave-dep notes'!downAndUpOctaves: odNote
	"returns two octave dependent notes of self who are repectively
	lower and higher than the given odNote"
	| col n |
	col := OrderedCollection new.
	n := self octave: odNote oct.
	(n > odNote)
		ifTrue: [col add: (self octave: odNote oct - 1).
			     col add: n]
		ifFalse:  [ col add: n.
			        col add:  (self octave: odNote oct + 1) ].
	^col! !!PitchClass methodsFor: 'making octave-dep notes'!pitchBetween: n1 and: n2
	"return, if it exists, a pitch (octave dependent note) between the two given notes"
	| candidate |
	candidate := (self downAndUpOctaves: n1) at: 2.
	candidate <= n2
		ifTrue: [^candidate]
		ifFalse: [^nil].! !!PitchClass methodsFor: 'making octave-dep notes'!the: nb octavesBeginningFrom: initialOctave 
	"N D the: 3 octavesBeginningFrom: 0"

	| col |
	col := OrderedCollection new.
	initialOctave to: initialOctave + nb - 1 do: [:o | col add: (self octave: o)].
	^col! !!PitchClass methodsFor: 'access'!doubleFlat
	^self flat flat! !!PitchClass methodsFor: 'access'!doubleSharp
	^self sharp sharp! !!PitchClass methodsFor: 'access'!natural
	self subclassResponsibility! !!PitchClass methodsFor: 'access'!pitchClass
	^self! !!PitchClass methodsFor: 'saving'!representBinaryOn: s 
	"to ensure uniqueness, pitch classes save themselves as messages sent 
	to the appropriate class, so that no duplicate are created"

	^self subclassResponsibility! !!PitchClass methodsFor: 'printing'!printOn: s 
	self subclassResponsibility! !!PitchClass methodsFor: 'printing'!storeOn: s 
	"N C storeString"

	s nextPutAll: 'N ' , self name! !!PitchClass methodsFor: 'transpose'!transposeOf: anInterval 
	"three cases: integer (+/-), method name (= ascending interval), or interval 
	object"

	anInterval isInteger ifTrue: [^(ChromaticInterval semiTones: anInterval)
			from: self].
	anInterval isSymbol ifTrue: [^self perform: anInterval].
	(anInterval isKindOf: MusicalInterval) ifTrue: [^anInterval topIfBottomIs: self].
	^self error: 'type d''intervalle indefini'! !!PitchClass methodsFor: 'constraining'!intervalTypeModuloOctaveBetween: n 



	^(self intervalTypeBetween: n) - 1 \\ 7+ 1! !!PitchClass methodsFor: 'Paleo'!copy
	self shouldNotImplement! !!PitchClass methodsFor: 'Paleo'!nameInScale: aScale
	^ aScale allNotes detect: [:n | self pitchEqual: n] ifNone: [self error: 'Should not reach here. Unknown note ' , self printString]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PitchClass class	instanceVariableNames: ''!!PitchClass class methodsFor: 'initialization'!englishOrFrench
	English := true "DialogView confirm: 'English (Yes) or French (No)'"! !!PitchClass class methodsFor: 'initialization'!initialize
	"There are 35 pitch classes. That's too much for Squeak, so the method was split into two methods"
	"PitchClass initialize"

	| la si do re mi fa sol lad sid dod red mid fad sold lab sib dob reb mib fab solb ladd sidd dodd redd midd fadd soldd |
	la := (PitchClassNatural new) semiToneCount: 10; name: #A.
	si := (PitchClassNatural new) semiToneCount: 12; name: #B.
	do := (PitchClassNatural new) semiToneCount: 1; name: #C.
	re := (PitchClassNatural new) semiToneCount: 3; name: #D.
	mi := (PitchClassNatural new) semiToneCount: 5; name: #E.
	fa := (PitchClassNatural new) semiToneCount: 6; name: #F.
	sol := (PitchClassNatural new) semiToneCount: 8; name: #G.
	lad := PitchClassSharp new natural: la.
	sid := PitchClassSharp new natural: si.
	dod := PitchClassSharp new natural: do.
	red := PitchClassSharp new natural: re.
	mid := PitchClassSharp new natural: mi.
	fad := PitchClassSharp new natural: fa.
	sold := PitchClassSharp new natural: sol.
	lab := PitchClassFlat new natural: la.
	sib := PitchClassFlat new natural: si.
	dob := PitchClassFlat new natural: do.
	reb := PitchClassFlat new natural: re.
	mib := PitchClassFlat new natural: mi.
	fab := PitchClassFlat new natural: fa.
	solb := PitchClassFlat new natural: sol.
	ladd := PitchClassDoubleSharp new natural: la.
	sidd := PitchClassDoubleSharp new natural: si.
	dodd := PitchClassDoubleSharp new natural: do.
	redd := PitchClassDoubleSharp new natural: re.
	midd := PitchClassDoubleSharp new natural: mi.
	fadd := PitchClassDoubleSharp new natural: fa.
	soldd := PitchClassDoubleSharp new natural: sol.
	la following: si; preceding: sol; sharp: lad; flat: lab.
	si following: do; preceding: la; sharp: sid; flat: sib.
	do following: re; preceding: si; sharp: dod; flat: dob.
	re following: mi; preceding: do; sharp: red; flat: reb.
	mi following: fa; preceding: re; sharp: mid; flat: mib.
	fa following: sol; preceding: mi; sharp: fad; flat: fab.
	sol following: la; preceding: fa; sharp: sold; flat: solb.
	lad sharp: ladd.
	sid sharp: sidd.
	dod sharp: dodd.
	red sharp: redd.
	mid sharp: midd.
	fad sharp: fadd.
	sold sharp: soldd.
	A := la. B := si. C := do. D := re. E := mi. F := fa. G := sol.
	self initializeDoubleFlat.
	AllNotes := OrderedCollection new.
	AllNotes add: do; add: re; add: mi; add: fa; add: sol; add: la; add: si; add: dod; add: red; add: mid; add: fad; add: sold; add: lad; add: sid; add: dob; add: reb; add: mib; add: fab; add: solb; add: lab; add: sib; add: dodd; add: redd; add: midd; add: fadd; add: soldd; add: ladd; add: sidd.
	self initializeFrenchNames.
	English := true.
	self initializeAllNaturalNotes;  initializeGlobals! !!PitchClass class methodsFor: 'initialization'!initializeAllNaturalNotes
	AllNatural := #(#C #D #E #F #G #A #B ) collect: [:n | self perform: n]! !!PitchClass class methodsFor: 'initialization'!initializeClass
	"There are 35 pitch classes. That's too much for Squeak, so the method was split into two methods"
	"PitchClass initializeClass"

	| la si do re mi fa sol lad sid dod red mid fad sold lab sib dob reb mib fab solb ladd sidd dodd redd midd fadd soldd |
	la := (PitchClassNatural new) semiToneCount: 10; name: #A.
	si := (PitchClassNatural new) semiToneCount: 12; name: #B.
	do := (PitchClassNatural new) semiToneCount: 1; name: #C.
	re := (PitchClassNatural new) semiToneCount: 3; name: #D.
	mi := (PitchClassNatural new) semiToneCount: 5; name: #E.
	fa := (PitchClassNatural new) semiToneCount: 6; name: #F.
	sol := (PitchClassNatural new) semiToneCount: 8; name: #G.
	lad := PitchClassSharp new natural: la.
	sid := PitchClassSharp new natural: si.
	dod := PitchClassSharp new natural: do.
	red := PitchClassSharp new natural: re.
	mid := PitchClassSharp new natural: mi.
	fad := PitchClassSharp new natural: fa.
	sold := PitchClassSharp new natural: sol.
	lab := PitchClassFlat new natural: la.
	sib := PitchClassFlat new natural: si.
	dob := PitchClassFlat new natural: do.
	reb := PitchClassFlat new natural: re.
	mib := PitchClassFlat new natural: mi.
	fab := PitchClassFlat new natural: fa.
	solb := PitchClassFlat new natural: sol.
	ladd := PitchClassDoubleSharp new natural: la.
	sidd := PitchClassDoubleSharp new natural: si.
	dodd := PitchClassDoubleSharp new natural: do.
	redd := PitchClassDoubleSharp new natural: re.
	midd := PitchClassDoubleSharp new natural: mi.
	fadd := PitchClassDoubleSharp new natural: fa.
	soldd := PitchClassDoubleSharp new natural: sol.
	la following: si; preceding: sol; sharp: lad; flat: lab.
	si following: do; preceding: la; sharp: sid; flat: sib.
	do following: re; preceding: si; sharp: dod; flat: dob.
	re following: mi; preceding: do; sharp: red; flat: reb.
	mi following: fa; preceding: re; sharp: mid; flat: mib.
	fa following: sol; preceding: mi; sharp: fad; flat: fab.
	sol following: la; preceding: fa; sharp: sold; flat: solb.
	lad sharp: ladd.
	sid sharp: sidd.
	dod sharp: dodd.
	red sharp: redd.
	mid sharp: midd.
	fad sharp: fadd.
	sold sharp: soldd.
	A := la. B := si. C := do. D := re. E := mi. F := fa. G := sol.
	self initializeDoubleFlat.
	AllNotes := OrderedCollection new.
	AllNotes add: do; add: re; add: mi; add: fa; add: sol; add: la; add: si; add: dod; add: red; add: mid; add: fad; add: sold; add: lad; add: sid; add: dob; add: reb; add: mib; add: fab; add: solb; add: lab; add: sib; add: dodd; add: redd; add: midd; add: fadd; add: soldd; add: ladd; add: sidd.
	self initializeFrenchNames.
	English := true.
	self initializeAllNaturalNotes;  initializeGlobals! !!PitchClass class methodsFor: 'initialization'!initializeDoubleFlat
	| labb sibb dobb rebb mibb fabb solbb |
	labb := PitchClassDoubleFlat new natural: A.
	sibb := PitchClassDoubleFlat new natural: B.
	dobb := PitchClassDoubleFlat new natural: C.
	rebb := PitchClassDoubleFlat new natural: D.
	mibb := PitchClassDoubleFlat new natural: E.
	fabb := PitchClassDoubleFlat new natural: F.
	solbb := PitchClassDoubleFlat new natural: G.
	A flat flat: labb.
	B flat flat: sibb.
	C flat flat: dobb.
	D flat flat: rebb.
	E flat flat: mibb.
	F flat flat: fabb.
	G flat flat: solbb! !!PitchClass class methodsFor: 'initialization'!initializeFrenchNames
	C nom: #Do.
	D nom: #Re.
	E nom: #Mi.
	F nom: #Fa.
	G nom: #Sol.
	A nom: #La.
	B nom: #Si! !!PitchClass class methodsFor: 'initialization'!initializeGlobals

	Smalltalk at: #N put: self. "for short-cuts"
	Smalltalk at: #PitchClassDo put: C. "for short-cuts"! !!PitchClass class methodsFor: 'global access'!A
	^A! !!PitchClass class methodsFor: 'global access'!B
	^B! !!PitchClass class methodsFor: 'global access'!C
	^C! !!PitchClass class methodsFor: 'global access'!D
	^D! !!PitchClass class methodsFor: 'global access'!E
	^E! !!PitchClass class methodsFor: 'global access'!F
	^F! !!PitchClass class methodsFor: 'global access'!G
	^G! !!PitchClass class methodsFor: 'global access'!allNotesButDoubles
	^self naturalNotes, self sharpNotes, self flatNotes! !!PitchClass class methodsFor: 'global access'!allPlausibleRootNotes
	^(self naturalNotes, self sharpNotes, self flatNotes) asOrderedCollection remove: (self B sharp); remove: (self E sharp); remove: (self C flat); remove: (self F flat); yourself! !!PitchClass class methodsFor: 'global access'!allPlausibleRootsForMajorScales
	"self allPlausibleRootsForMajorScales"
	| col |
	col := OrderedCollection new.
	col add: self C; 
		add: self D flat; 
		add: self D;
		add: self E flat; 
		add: self E; 
		add: self F;
		add: self F sharp; 
		add: self G; 
		add: self A flat;
		add: self A; 
		add: self B flat; 
		add: self B.
	^col! !!PitchClass class methodsFor: 'global access'!allPlausibleRootsForMinorScales
	"self allPlausibleRootsForMinorScales"
	| col |
	col := OrderedCollection new.
	col add: self C; 
		add: self C sharp; 
		add: self D;
		add: self D sharp; 
		add: self E; 
		add: self F;
		add: self F sharp; 
		add: self G; 
		add: self G sharp;
		add: self A; 
		add: self B flat; 
		add: self B.
	^col! !!PitchClass class methodsFor: 'global access'!do
	^C! !!PitchClass class methodsFor: 'global access'!fa
	^F! !!PitchClass class methodsFor: 'global access'!flatNotes
	^self naturalNotes collect: [:n | n flat]! !!PitchClass class methodsFor: 'global access'!fromSemiTones: n 
	"arbitrary method, used for transposing pitch classes (a strange notion...)"

	| delta |
	delta := n \\ 12.
	delta = 1 ifTrue: [^N C].
	delta = 2 ifTrue: [^N C sharp].
	delta = 3 ifTrue: [^N D].
	delta = 4 ifTrue: [^N D sharp].
	delta = 5 ifTrue: [^N E].
	delta = 6 ifTrue: [^N F].
	delta = 7 ifTrue: [^N F sharp].
	delta = 8 ifTrue: [^N G].
	delta = 9 ifTrue: [^N G sharp].
	delta = 10 ifTrue: [^N A].
	delta = 11 ifTrue: [^N A sharp].
	delta = 0 ifTrue: [^N B]! !!PitchClass class methodsFor: 'global access'!la
	^A! !!PitchClass class methodsFor: 'global access'!mi
	^E! !!PitchClass class methodsFor: 'global access'!naturalNotes
	^AllNatural! !!PitchClass class methodsFor: 'global access'!noteNamed: c 
	"takes the case of flat into account. Sharps are naturally parsed out 
	from the note name by the smalltalk parser.
	Since the algorithm proceeds from the left, it accepts any number of sharps and 	flats (using the common algebra) :
	N noteNamed: 'C#b#b#b#' ->  C# "

	|  note |
	(c size > 1 and: [c last = $b])
		ifTrue: 
			[note := self noteNamed: (c copyFrom: 1 to: c size - 1).
			^note flat].
	(c size > 1 and: [c last = $#])
		ifTrue: 
			[note := self noteNamed: (c copyFrom: 1 to: c size - 1).
			^note sharp].
	^self naturalNotes detect: [:n | n nameIs: c asSymbol]
		ifNone: [^self error: 'unknown note name : ', c]! !!PitchClass class methodsFor: 'global access'!re
	^D! !!PitchClass class methodsFor: 'global access'!sharpNotes
	^self naturalNotes collect: [:n | n sharp]! !!PitchClass class methodsFor: 'global access'!si
	^B! !!PitchClass class methodsFor: 'global access'!sol
	^G! !!PitchClass class methodsFor: 'vocal ranges'!altoRange
	"retourne la collection des notes de l'alto"

	^self from: (N sol octave: 3)
		to: (N re octave: 5)! !!PitchClass class methodsFor: 'vocal ranges'!altoRangeInScale: aScale
	"PitchClass sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N sol octave: 3)
		to: (N re octave: 5)! !!PitchClass class methodsFor: 'vocal ranges'!baseRange
	"retourne la collection des notes de la basse"

	^self from: (N sol octave: 2)
		to: (N re octave: 4)! !!PitchClass class methodsFor: 'vocal ranges'!baseRangeInScale: aScale
	"PitchClass sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N sol octave: 2)
		to: (N re octave: 4)! !!PitchClass class methodsFor: 'vocal ranges'!sopranoRange
	"retourne la collection des notes du soprano"

	^self from: (N re octave: 4)
		to: (N do octave: 6)! !!PitchClass class methodsFor: 'vocal ranges'!sopranoRangeInScale: aScale
	"PitchClass sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N re octave: 4)
		to: (N do octave: 6)! !!PitchClass class methodsFor: 'vocal ranges'!tenorRange
	"retourne la collection des notes du tenor"

	^self from: (N re octave: 3)
		to: (N la octave: 4)! !!PitchClass class methodsFor: 'vocal ranges'!tenorRangeInScale: aScale
	"PitchClass sopranoRangeInScale: (N do sharp minorScale)"

	^aScale diatonicNotesFrom: (N re octave: 3)
		to: (N la octave: 4)! !!PitchClass class methodsFor: 'ordering'!flatOrdering
	^#(#B #E #A #D #G #C #F ) collect: [:n | self perform: n]! !!PitchClass class methodsFor: 'ordering'!nFirstFlats: n 
	"PitchClass nFirstFlats: 3"

	^(1 to: n)
		collect: [:i | (self flatOrdering at: i) flat]! !!PitchClass class methodsFor: 'ordering'!nFirstSharps: n 
	"PitchClass nFirstSharps: 3"

	^(1 to: n)
		collect: [:i | (self sharpOrdering at: i) sharp]! !!PitchClass class methodsFor: 'ordering'!sharpOrdering
	^#(#F #C #G #D #A #E #B ) collect: [:n | self perform: n]! !!PitchClass class methodsFor: 'examples'!chordExamples
"N C sharp chordFromString: '' ->  [C# ]

(N C sharp chordFromString: 'min 7 dim5') notes -> OrderedCollection (C# E G B )
"! !!PitchClass class methodsFor: 'examples'!closestEnharmonic
"
N D sharp closestEnharmonic Eb
N E flat closestEnharmonic D#

N B sharp closestEnharmonic C 
N C flat closestEnharmonic D#

N D sharp sharp closestEnharmonic  E
N E flat flat closestEnharmonic  D
------
(N D sharp @ 2)  closestEnharmonic Eb2 
(N E flat @ 2) closestEnharmonic D#2

(N B sharp @ 2) closestEnharmonic C3
(N C flat @ 3) closestEnharmonic B2
"! !!PitchClass class methodsFor: 'examples'!majorScaleExample
"PitchClass B majorScale notes- >  #(B C# D# E F# G# A# )"! !!PitchClass class methodsFor: 'examples'!melodicMinorScaleExample
	^PitchClass D flat melodicMinorScale! !!PitchClass class methodsFor: 'examples'!minorScaleExample
	^PitchClass C sharp minorScale! !!PitchClass class methodsFor: 'examples'!pitchInTessituraExamples

"
	N C pitchInTessitura: QTessitura high ->  C5
	N A pitchInTessitura: QTessitura high -> A4	
	N G pitchInTessitura: QTessitura high ->  G4
	N F pitchInTessitura: QTessitura high ->  F5

	N A pitchInTessitura: QTessitura low ->  A2	"! !!PitchClass class methodsFor: 'examples'!sharpflatAlgebraExample
	"
	sharp and flat's algebra
	N C sharp ->Do# 
	N C sharp sharp ->Do## 
	N C sharp sharp sharp ->error 
	N C flat sharp -> Do
	N re sharp sharp natural -> re

	Intervals computation :
	N C diminishedFifth -> Solb
	N C augmentedFourth -> Fa#
	N C diminishedThirteenth -> Lab 
	N C flat minorSeventh -> Sibb 
	N C majorThird majorThird -> Sol#
	
	Notes equivalence : pitchEgal methode
	N C sharp pitchEqual: N re flat -> true
	N C augmentedFourth pitchEqual: N C diminishedFifth -> true
	N C diminishedFifth pitchEqual: N F minorSecond -> true
	"! !PitchClass subclass: #PitchClassAltered	instanceVariableNames: 'natural'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassAltered methodsFor: 'accessing'!following
	^natural following! !!PitchClassAltered methodsFor: 'accessing'!name
	"yields a symbol"

	^self printString asSymbol! !!PitchClassAltered methodsFor: 'accessing'!natural
	^natural! !!PitchClassAltered methodsFor: 'accessing'!natural: value
	natural := value! !!PitchClassAltered methodsFor: 'accessing'!nom
	"yields a symbol"

	^self printString asSymbol! !!PitchClassAltered methodsFor: 'accessing'!preceding
	^natural preceding! !!PitchClassAltered methodsFor: 'testing'!isNatural
	^false! !Object subclass: #PitchClassChord	instanceVariableNames: 'root structure notes possibleTonalities'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassChord methodsFor: 'automatic access'!root
   ^root! !!PitchClassChord methodsFor: 'automatic access'!root: aValue
  	 root := aValue.
	self resetNotes! !!PitchClassChord methodsFor: 'automatic access'!rootPitchClass
	^root pitchClass! !!PitchClassChord methodsFor: 'automatic access'!structure
   ^structure! !!PitchClassChord methodsFor: 'automatic access'!structure: aValue
	structure := aValue.
	self resetNotes! !!PitchClassChord methodsFor: 'printing'!name
	"produces a printable name (without the sharps before symbols)"

	| s |
	s := WriteStream on: ''.
	root printOn: s.
	s nextPutAll: ' '.
	structure
		do: 
			[:x | 
			(x isKindOf: Integer)
				ifTrue: [s nextPutAll: x printString]
				ifFalse: [s nextPutAll: x].
			s nextPutAll: ' '].
	^s contents! !!PitchClassChord methodsFor: 'printing'!notesString
	| s |
	s := ''.
	self notes do: [:n | s := s , n printString , ' '].
	^s! !!PitchClassChord methodsFor: 'printing'!printOn: s  
	s nextPutAll: '['.
	(root isKindOf: PitchClass)
		ifTrue: [root printOn: s]
		ifFalse: [root pitchClass printOn: s].
	s nextPutAll: ' '.
	structure  do: 

	[:x | 
		(x isKindOf: SmallInteger)
			ifTrue: [x  printOn: s]
			ifFalse: [s nextPutAll: x asString].
		s nextPutAll: ' '].
	s nextPutAll: ']'.! !!PitchClassChord methodsFor: 'printing'!quotedPrintOn: s
	"as printOn but without the brackets [] and with quotes"
	s nextPut: $'.
	(root isKindOf: PitchClass) 
		ifTrue: [root printOn: s]
		ifFalse: [root pitchClass printOn: s].
	s nextPutAll: ' '.
	structure  do: 
	[:x | 
		(x isKindOf: SmallInteger)
			ifTrue: [x  printOn: s]
			ifFalse: [s nextPutAll: x asString].
		s nextPutAll: ' '].
	s nextPut: $'.! !!PitchClassChord methodsFor: 'printing'!storeOn: s
	"returns a String representation of the receiver from which the receiver
	can be reconstructed"
	s nextPut: $(.
	self quotedPrintOn: s.
	s nextPutAll: ' asChord)'.! !!PitchClassChord methodsFor: 'printing'!structureString
	| s |
	s := WriteStream on: ''.
	structure  do: 
	[:x | 
		(x isKindOf: SmallInteger)
			ifTrue: [x  printOn: s]
			ifFalse: [s nextPutAll: x asString].
		s nextPutAll: ' '].
	^s contents! !!PitchClassChord methodsFor: 'note testing'!allNotesInScale: aScale  
	"The previous version couldnt deal with situations like
	  (FPChord newFromString: 'C  7 aug9')  possibleTonalitiesInScaleClass:  MajorScale"

	self notesWithinOctave do: [:n | (aScale notes includes: n)
			ifFalse: [^false]].
	^true! !!PitchClassChord methodsFor: 'note testing'!chordNotesAmong: list
	"select among given notes those that are chord notes"
	^list select: [:each |
		self containsNote: each]! !!PitchClassChord methodsFor: 'note testing'!containsNote: n
	^(self notes detect: [:each | each pitchClass pitchEqual: n pitchClass]
			ifNone: [nil]) notNil! !!PitchClassChord methodsFor: 'note testing'!nextIn: aCS
	^aCS chordAfter: self! !!PitchClassChord methodsFor: 'note testing'!nonChordNotesAmong: list
	"select among given notes those that are chord notes"

	^list select: [:each |
		(self containsNote: each) not]! !!PitchClassChord methodsFor: 'note testing'!notes: aListOfNotes contains: anArrayOfIntervals 
	anArrayOfIntervals do: [:i | (aListOfNotes includes: (root perform: i))
			ifTrue: [^true]].
	^false! !!PitchClassChord methodsFor: 'creation'!format
	"format the structure. Assumes root is not nil"

	structure := self structureFromNotes: self notes root: root! !!PitchClassChord methodsFor: 'creation'!fromNotes: l 
	"assumes the first note is the root"

	^self fromNotes: l root: l first! !!PitchClassChord methodsFor: 'creation'!fromNotes: aList root: r
	"l isa list of pitch-classes.
	This method computes the correct (and canonical) name for the chord"

	| l |
	root := r. 
	l := aList  asOrderedCollection.
	notes := l copy.
	structure := OrderedCollection new.
	
	(l includes: r) ifTrue: [l remove: r] ifFalse: [structure add: #noRoot].

	(self notes: l contains: #(majorThird minorThird fourth)) ifFalse: [structure add: #no3].
	(((self notes: l contains: #(majorThird minorThird)) not) and: [l includes: r fourth]) ifTrue: [structure add: #sus4. l remove: r fourth].
	(l includes: root majorThird) ifTrue: [l remove: root majorThird].
	(l includes: root minorThird) ifTrue: [structure add: #min. l remove: root minorThird].
	(self notes: l contains: #(fifth augmentedFifth diminishedFifth)) ifFalse: [structure add: #no5].		
	(l includes: root diminishedFifth) ifTrue: [structure add: #dim5. l remove: root diminishedFifth].
	(l includes: root augmentedFifth) ifTrue: [structure add: #aug5. l remove: root augmentedFifth].
	(l includes: root perfectFifth) ifTrue: [ l remove: root perfectFifth].

	((l includes: root minorSeventh) and: [(structure includes: #dim5) and: [structure includes: #min]])
			ifTrue: [structure add: #halfDim7; remove: #dim5; remove: #min. l remove: root minorSeventh].
	(l includes: root minorSeventh) ifTrue: [structure add: 7. l remove: root minorSeventh].
	(l includes: root majorSeventh) ifTrue: [structure add: #maj7. l remove: root majorSeventh].
	((l includes: root diminishedSeventh) and: [(structure includes: #dim5) and: [structure includes: #min]])
			ifTrue: [structure add: #dim7; remove: #dim5; remove: #min. l remove: root diminishedSeventh].

	((structure includes: #no7) not and: [((self notes: l contains: #(ninth augmentedNinth diminishedNinth)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [structure add: #no7].
	(l includes: root ninth) ifTrue: [structure add: 9. l remove: root ninth].
	(l includes: root diminishedNinth) ifTrue: [structure add: #dim9. l remove: root diminishedNinth].
	(l includes: root augmentedNinth) ifTrue: [structure add: #aug9. l remove: root augmentedNinth].

	((structure includes: #no9) not and:
		[((self notes: l contains: #(eleventh augmentedEleventh)) and:
			[(self notes: notes contains: #(ninth augmentedNinth diminishedNinth)) not])])
	ifTrue: [structure add: #no9].
	((structure includes: #no7) not and: [((self notes: l contains: #(eleventh augmentedEleventh)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [structure add: #no7].
	(l includes: root eleventh) ifTrue: [structure add: 11. l remove: root eleventh]. 
	(l includes: root augmentedEleventh) ifTrue: [structure add: #aug11. l remove: root augmentedEleventh]. 

	((self notes: l contains: #(thirteenth)) and:
		[(self notes: notes contains: #(minorSeventh majorSeventh)) not]) ifTrue: [structure add: #sixth. l remove: r thirteenth].
	((structure includes: #no11) not and:
		[((self notes: l contains: #(thirteenth diminishedThirteenth)) and:
			[(self notes: notes contains: #(eleventh augmentedEleventh)) not])]) ifTrue: [structure add: #no11].
	((structure includes: #no9) not and: [((self notes: l contains: #(thirteenth diminishedThirteenth)) and: [(self notes: notes contains: #(ninth augmentedNinth diminishedNinth)) not])]) ifTrue: [structure add: #no9].
	((structure includes: #no7) not and: [((self notes: l contains: #(thirteenth  diminishedThirteenth)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [structure add: #no7].
	(l includes: root thirteenth) ifTrue: [structure add: 13. l remove: root thirteenth].
	(l includes: root diminishedThirteenth) ifTrue: [structure add: #dim13. l remove: root diminishedThirteenth].! !!PitchClassChord methodsFor: 'creation'!fromString: l
	"l is a list whose first element is a note and the rest a structure, in our
	standardized chord syntax"
	
	| secondElement |
	structure := (Scanner new scanTokens: l) asOrderedCollection.
	root := PitchClass noteNamed: structure first.
	structure removeFirst.

"the following is done twice to take double sharp and double flats into acount
although those should not be allowed to form chords"

	2 timesRepeat: [
	structure isEmpty ifFalse:
		[secondElement:= structure first.
		(secondElement = '#' asSymbol)
			ifTrue: [root := root sharp. structure removeFirst].
		(secondElement = #b) ifTrue: [root:= root flat. structure removeFirst]]].
	self format! !!PitchClassChord methodsFor: 'creation'!structureFromNotes: aList root: r
	"l isa list of pitch-classes.
	This method computes the correct (and canonical) name for the chord"

	| l struct |
	l := aList  asOrderedCollection.
	struct := OrderedCollection new. 
	(l includes: r) ifTrue: [l remove: r] ifFalse: [struct add: #noRoot].

	(self notes: l contains: #(majorThird minorThird fourth)) ifFalse: [struct add: #no3].
	(((self notes: l contains: #(majorThird minorThird)) not) and: [l includes: r fourth]) ifTrue: [struct add: #sus4. l remove: r fourth].
	(l includes: root majorThird) ifTrue: [l remove: root majorThird].
	(l includes: root minorThird) ifTrue: [struct add: #min. l remove: root minorThird].
	(self notes: l contains: #(fifth augmentedFifth diminishedFifth)) ifFalse: [struct add: #no5].		
	(l includes: root diminishedFifth) ifTrue: [struct add: #dim5. l remove: root diminishedFifth].
	(l includes: root augmentedFifth) ifTrue: [struct add: #aug5. l remove: root augmentedFifth].
	(l includes: root perfectFifth) ifTrue: [ l remove: root perfectFifth].

	((l includes: root minorSeventh) and: [(struct includes: #dim5) and: [struct includes: #min]])
			ifTrue: [struct add: #halfDim7; remove: #dim5; remove: #min. l remove: root minorSeventh].
	(l includes: root minorSeventh) ifTrue: [struct add: 7. l remove: root minorSeventh].
	(l includes: root majorSeventh) ifTrue: [struct add: #maj7. l remove: root majorSeventh].
	((l includes: root diminishedSeventh) and: [(struct includes: #dim5) and: [struct includes: #min]])
			ifTrue: [struct add: #dim7; remove: #dim5; remove: #min. l remove: root diminishedSeventh].
	(l includes: root diminishedSeventh)
			ifTrue: [struct add: #dim7. l remove: root diminishedSeventh].

	((struct includes: #no7) not and: [((self notes: l contains: #(ninth augmentedNinth diminishedNinth)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [struct add: #no7].
	(l includes: root ninth) ifTrue: [struct add: 9. l remove: root ninth].
	(l includes: root diminishedNinth) ifTrue: [struct add: #dim9. l remove: root diminishedNinth].
	(l includes: root augmentedNinth) ifTrue: [struct add: #aug9. l remove: root augmentedNinth].

	((struct includes: #no9) not and:
		[((self notes: l contains: #(eleventh augmentedEleventh)) and:
			[(self notes: notes contains: #(ninth augmentedNinth diminishedNinth)) not])])
	ifTrue: [struct add: #no9].
	((struct includes: #no7) not and: [((self notes: l contains: #(eleventh augmentedEleventh)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [struct add: #no7].
	(l includes: root eleventh) ifTrue: [struct add: 11. l remove: root eleventh]. 
	(l includes: root augmentedEleventh) ifTrue: [struct add: #aug11. l remove: root augmentedEleventh]. 

	((self notes: l contains: #(thirteenth)) and:
		[(self notes: notes contains: #(minorSeventh majorSeventh)) not]) ifTrue: [struct add: #sixth. l remove: r thirteenth].
	((struct includes: #no11) not and:
		[((self notes: l contains: #(thirteenth diminishedThirteenth)) and:
			[(self notes: notes contains: #(eleventh augmentedEleventh)) not])]) ifTrue: [struct add: #no11].
	((struct includes: #no9) not and: [((self notes: l contains: #(thirteenth diminishedThirteenth)) and: [(self notes: notes contains: #(ninth augmentedNinth diminishedNinth)) not])]) ifTrue: [struct add: #no9].
	((struct includes: #no7) not and: [((self notes: l contains: #(thirteenth  diminishedThirteenth)) and: [(self notes: notes contains: #(minorSeventh majorSeventh diminishedSeventh)) not])]) ifTrue: [struct add: #no7].
	(l includes: root thirteenth) ifTrue: [struct add: 13. l remove: root thirteenth].
	(l includes: root diminishedThirteenth) ifTrue: [struct add: #dim13. l remove: root diminishedThirteenth].
	^struct! !!PitchClassChord methodsFor: 'notes computation'!computeAllNotes
	"computes the list of notes from the structure. The job is the opposite 
	of what is written is method fromListOfNotes. 
	Assumes root is not nil. 
	| c | 
	c := FPChord newFromString: 'A halfDim7'. 
	c computeAllNotes. 
	c notes"

	| structCopy |
	notes := OrderedCollection new.
	structCopy := structure copy.
	self computeRoot; computeThird; computeFifth; computeSixth; computeSeventh; computeNinth; computeEleventh; computeThirteenth	"; computeDiminished".
	structure isEmpty ifFalse: [^self error: 'bad structure : ', structure printString].
	structure := structCopy! !!PitchClassChord methodsFor: 'notes computation'!computeDiminished
	(structure includes: #dim7)
		ifTrue: 
			[notes := OrderedCollection
						with: root
						with: root minorThird
						with: root diminishedFifth
						with: root diminishedSeventh.
			structure remove: #dim7].
	(structure includes: #halfDim7)
		ifTrue: 
			[notes := OrderedCollection
						with: root
						with: root minorThird
						with: root diminishedFifth
						with: root minorSeventh.
			structure remove: #halfDim7]! !!PitchClassChord methodsFor: 'notes computation'!computeEleventh
	(structure includes: #no11)
		ifTrue: 
			[structure remove: #no11.
			^nil].
	(structure includes: 11)
		ifTrue: 
			[structure remove: 11.
			^notes add: root eleventh].
	(structure includes: #aug11)
		ifTrue: 
			[structure remove: #aug11.
			^notes add: root augmentedEleventh].
	(self structureHasEitherOf: #(13 #dim13 ))
		ifTrue: [^notes add: root eleventh]! !!PitchClassChord methodsFor: 'notes computation'!computeFifth
	((structure includes: #dim7)
		or: [structure includes: #halfDim7]) ifTrue: [^notes add: root diminishedFifth].
	(structure includes: #no5) ifTrue: [structure remove: #no5. ^nil].
	(structure includes: #dim5) ifTrue: [structure remove: #dim5. ^notes add: root diminishedFifth].
	(structure includes: #b5) ifTrue: [structure remove: #b5. ^notes add: root diminishedFifth].
	(structure includes: #aug5) ifTrue: [structure remove: #aug5. ^notes add: root augmentedFifth].
	notes add: root perfectFifth! !!PitchClassChord methodsFor: 'notes computation'!computeNinth
	(structure includes: #no9) ifTrue: [structure remove: #no9. ^nil].
	(structure includes: 9) ifTrue: [structure remove: 9. ^notes add: root ninth].
	(structure includes: #aug9) ifTrue: [structure remove: #aug9. ^notes add: root augmentedNinth].
	(structure includes: #dim9) ifTrue: [structure remove: #dim9. ^notes add: root diminishedNinth].
	(structure includes: #b9) ifTrue: [structure remove: #b9. ^notes add: root diminishedNinth].

	(self structureHasEitherOf: #(11 aug11 13 dim13))
		ifTrue: [notes add: root majorNinth]! !!PitchClassChord methodsFor: 'notes computation'!computeRoot
	(structure includes: #noRoot)
		ifTrue: [structure remove: #noRoot]
		ifFalse: [notes add: root]! !!PitchClassChord methodsFor: 'notes computation'!computeSeventh
	(structure includes: #dim7) ifTrue: [structure remove: #dim7. ^notes add: root diminishedSeventh].
	(structure includes: #halfDim7) ifTrue: [structure remove: #halfDim7. ^notes add: root minorSeventh].
	(structure includes: #no7) ifTrue: [structure remove: #no7. ^nil].
	(structure includes: 7) ifTrue: [structure remove: 7. ^notes add: root minorSeventh].
	(structure includes: #maj7) ifTrue: [structure remove: #maj7. ^notes add: root majorSeventh].

	
	(self structureHasEitherOf: #(9 dim9 aug9 11 aug11 13 dim13))
		ifTrue: [notes add: root minorSeventh]! !!PitchClassChord methodsFor: 'notes computation'!computeSixth
	(structure includes: 6)
		ifTrue: [structure remove: 6. ^notes add: root sixth].
	(structure includes: #sixth)
		ifTrue: [structure remove: #sixth. ^notes add: root sixth]! !!PitchClassChord methodsFor: 'notes computation'!computeThird
	((structure includes: #dim7)
		or: [structure includes: #halfDim7]) ifTrue: [^notes add: root minorThird].
	(structure includes: #no3) ifTrue: [structure remove: #no3. ^nil].
	(structure includes: #sus4) ifTrue: [structure remove: #sus4. ^notes add: root fourth].
	(structure includes: #min) ifTrue: [structure remove: #min. ^notes add: root minorThird].
	(structure includes: #m) ifTrue: [structure remove: #m. ^notes add: root minorThird].
	notes add: root majorThird! !!PitchClassChord methodsFor: 'notes computation'!computeThirteenth

	(structure includes: 13) ifTrue: [structure remove: 13. ^notes add: root thirteenth].
	(structure includes: #dim13) ifTrue: [structure remove: #dim13. ^notes add: root diminishedThirteenth].
	(structure includes: #b13) ifTrue: [structure remove: #b13. ^notes add: root diminishedThirteenth].
	(structure includes: #no13)
		ifTrue: 	[structure remove: #no13.	^nil].! !!PitchClassChord methodsFor: 'notes computation'!notes
	notes isEmpty ifTrue: [self computeAllNotes].
	^notes! !!PitchClassChord methodsFor: 'notes computation'!notesWithinOctave
	"accordingly, return the notes as in the case of a four note chord
	with no higher dissonances"

	| col |
	col := OrderedCollection  with: self theTonic
							with: self theThird
							with: self theFifth
							with: self theSeventh.
	^col select: [:each | (each == nil) not].! !!PitchClassChord methodsFor: 'compute possible tonalities'!= c 
	^root = c root and: [structure = c structure]! !!PitchClassChord methodsFor: 'compute possible tonalities'!matchWith: c 
	"((Chord new fromString: 'D  min 7') computeAllNotes) format 
	matchAvec: ((N re majorScale genereAccordsPoly: 4) at: 3)."

	^structure = c structure! !!PitchClassChord methodsFor: 'compute possible tonalities'!standardPossibleTonalities
	"possible tonalities in scale class major, minorMel and minorHarm 
	(Chord new fromString: 'C min') standardPossibleTonalities 
	ListeDAnalyses ({II de Sib MajorScale} {III de Lab MajorScale} {VI de 
	Mib MajorScale} {I de Do HarmonicMinorScale} {IV de Sol 
	HarmonicMinorScale} {I de Do MelodicMinorScale} {II de Sib 
	MelodicMinorScale} )"

	| res |
	res := OrderedCollection new.
	Scale standardScales do: [:s | res addAll: (self possibleTonalitiesInScaleClass: s)].
	^res! !!PitchClassChord methodsFor: 'compute possible tonalities'!tonalitesCommunesAvec: unAccord 
	"(FPChord newFromString: 'C min') tonalitesCommunesAvec: (FPChord 
	newFromString: 'D b ') "

	^self possibleTonalities tonalitesCommunesAvec: unAccord possibleTonalities! !!PitchClassChord methodsFor: 'compute possible tonalities'!tonalitesCommunesAvec: unAccord et: autreAccord
	"(FPChord newFromString: 'C min') tonalitesCommunesAvec: (FPChord 
	newFromString: 'D b ') "

	^self possibleTonalities tonalitesCommunesAvec: unAccord possibleTonalities 
						   et: autreAccord possibleTonalities! !!PitchClassChord methodsFor: 'transposing'!beTransposedBy: int 
	| no |
	no := self notes.
	root := root transposeOf: int.
	notes := no collect: [:n | n transposeOf: int]! !!PitchClassChord methodsFor: 'transposing'!transposeOf: interval 
	^self class root: (root transposeOf: interval)
		structure: structure! !!PitchClassChord methodsFor: 'FP'!natural
	^root natural! !!PitchClassChord methodsFor: 'accessing'!possibleScales
	^self possibleTonalities collect: [:a | a scale]! !!PitchClassChord methodsFor: 'accessing'!possibleTonalities 

	possibleTonalities isEmpty ifTrue: [self computePossibleTonalities].
	^possibleTonalities! !!PitchClassChord methodsFor: 'initializations'!initAnalyse 
	possibleTonalities:= OrderedCollection new.
	notes := OrderedCollection new.
	self changed: #analyse; changed: #analysesPossibles! !!PitchClassChord methodsFor: 'initializations' stamp: 'stp 12/31/2022 06:55'!initialize	notes := OrderedCollection new.	structure := ''."	possibleTonalities := AnalysisList new"! !!PitchClassChord methodsFor: 'initializations' stamp: 'stp 12/31/2022 06:56'!resetNotes	"reset notes to an empty collection when an important change	occurred "	(notes isEmpty			or: [notes == nil])		ifFalse: [notes := OrderedCollection new]."	(possibleTonalities isEmpty			or: [possibleTonalities == nil])		ifFalse: [possibleTonalities := AnalysisList new]"! !!PitchClassChord methodsFor: 'accessing by  intervals'!theFifth

	self  hasNoFifth
		ifTrue: [^nil].
	self hasADiminishedFifth
		ifTrue: [^root diminishedFifth].
	self hasAnAugmentedFifth
		ifTrue: [^root augmentedFifth].
	^root fifth.! !!PitchClassChord methodsFor: 'accessing by  intervals'!theFourth
	
	^ root fourth.! !!PitchClassChord methodsFor: 'accessing by  intervals'!theOctave
	
	^ root octave.! !!PitchClassChord methodsFor: 'accessing by  intervals'!theSeventh
	self hasNoSeventh ifTrue: [^nil].
	self hasAMajorSeventh ifTrue: [^root majorSeventh].
	self hasADiminishedSeventh ifTrue: [^root diminishedSeventh].
	self hasAMinorSeventh ifTrue: [^root minorSeventh].
	^nil! !!PitchClassChord methodsFor: 'accessing by  intervals'!theThird
	
	self isSuspended
		ifTrue: [^nil].
	self isMinor
		ifTrue: [^root  minorThird].
	^ root majorThird.! !!PitchClassChord methodsFor: 'accessing by  intervals'!theTonic
	^root unison! !!PitchClassChord methodsFor: 'testing ok'!commonTonalitiesWith: aChord
	^self possibleTonalities tonalitesCommunesAvec: aChord possibleTonalities! !!PitchClassChord methodsFor: 'testing ok'!hasA: anInterval 
	"(FPChord newFromString: 'C min 7') hasA: #majorThird"

	^self notes includes: (self root perform: anInterval)! !!PitchClassChord methodsFor: 'testing ok'!hasADiminishedFifth
	^(((structure includes: #halfDim7) 
		or: [structure includes: #dim7])
		or: [structure includes: #dim5])! !!PitchClassChord methodsFor: 'testing ok'!hasADiminishedSeventh
	^structure includes: #dim7! !!PitchClassChord methodsFor: 'testing ok'!hasAMajorSeventh
	^(structure includes: #maj7)! !!PitchClassChord methodsFor: 'testing ok'!hasAMinorSeventh
	^self structureHasEitherOf: #(7 #halfDim7 )! !!PitchClassChord methodsFor: 'testing ok'!hasAnAugmentedFifth
	^structure includes: #aug5! !!PitchClassChord methodsFor: 'testing ok'!hasNoFifth
	^structure includes: #no5! !!PitchClassChord methodsFor: 'testing ok'!hasNoSeventh
	^(structure includes: #no7)! !!PitchClassChord methodsFor: 'testing ok'!isAlterate
	^(self isDominant 
		and: [self hasNoFifth])
		and: [self structureHasEitherOf: #(dim9 13)].! !!PitchClassChord methodsFor: 'testing ok'!isDiminished
	^(structure includes: #dim7)! !!PitchClassChord methodsFor: 'testing ok'!isDominant
	^self isMajor
		and: [self hasAMinorSeventh].! !!PitchClassChord methodsFor: 'testing ok'!isHalfDiminished
	^structure includes: #halfDim7! !!PitchClassChord methodsFor: 'testing ok'!isMajor
	^self isMinor not! !!PitchClassChord methodsFor: 'testing ok'!isMinor
	^self structureHasEitherOf: #( min  halfDim7 dim7)! !!PitchClassChord methodsFor: 'testing ok'!isSuspended
	^(structure includes: #sus4)! !!PitchClassChord methodsFor: 'testing ok'!structureHasEitherOf: aList 
	^(aList detect: [:x | structure includes: x]
		ifNone: [ nil]) notNil! !!PitchClassChord methodsFor: 'testing ok'!structureHasNeitherOf: aList 
	^(aList detect: [:x | structure includes: x]
		ifNone: [^true]) notNil! !!PitchClassChord methodsFor: 'mutations'!@ o 
	o isInteger ifFalse: [^super @ o].
	^self octave: o! !!PitchClassChord methodsFor: 'mutations'!asPlayableObject
	^(self octave: self defaultOctave) asPlayableObject! !!PitchClassChord methodsFor: 'mutations'!defaultOctave
	^2! !!PitchClassChord methodsFor: 'mutations'!fondamental
	| t n |
	t := self root octave: 3.
	n := self notes collect: [:nn | nn octave: 4].
	n addFirst: t.
	^(OctaveDependentChord root: t notes: n)! !!PitchClassChord methodsFor: 'mutations'!inTessitura: qt
	^ OctaveDependentChord root: (root inTessitura: qt)! !!PitchClassChord methodsFor: 'mutations'!lapse: l
	^(self octave: self defaultOctave) lapse: l! !!PitchClassChord methodsFor: 'mutations'!octave: n 
	^(OctaveDependentChord new) structure: structure copy; root: (root octave: n)! !!PitchClassChord methodsFor: 'mutations'!startBeat: s duration: d
	"a default octave is used"
	^(self octave: self defaultOctave) startBeat: s duration: d! !!PitchClassChord methodsFor: 'mutations'!startTime: s duration: d
	^(self octave: self defaultOctave) startBeat: s duration: d! !!PitchClassChord methodsFor: 'tessitura'!rootInTessitura: qt
	^root inTessitura: qt! !!PitchClassChord methodsFor: 'other computations'!intervalBetweenRoots: aChord
	"remark: not that this method answers the interval between 
	the root *pitch classes*!!!!"
	"('G min 7' asChordOct: 2) intervalBetweenRoots:  ('F maj7' asChordOct: 2)"

	^self root pitchClass intervalBetween: aChord root pitchClass! !!PitchClassChord methodsFor: 'private-pact'!bestTonalityInIsolatedCase
	"answer the most plausible tonality when the chor is isolated.
	Later, I will rewrite it to bypass possibleTonalities for sake of efficience"

	| list  candidate |
	list := self possibleTonalities.
	candidate := list first.
	self isDominant
		ifTrue: [^list detect: [:each | each degree = 5] ifNone: [candidate]].
	"the minor scale whose first degree is equal to root is preferred"
	self isMinor
		ifTrue: [^list detect: [:each | each scale isMinor and: [each degree = 1]] 
				     ifNone: [list detect: [:each | each scale isMinor]
						    ifNone: [candidate]]].
	"root is the first degree"
	^list detect: [:each | each degree = 1] ifNone: [candidate].! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PitchClassChord class	instanceVariableNames: ''!!PitchClassChord class methodsFor: 'creation'!new
	^super new initialize! !!PitchClassChord class methodsFor: 'creation'!newFromNotes: l 
	"computes the name from the list of notes, according to our syntax"

	^self new fromNotes: l! !!PitchClassChord class methodsFor: 'creation'!newFromNotesNames: aStringOfNoteNames 
	"computes the name from the list of notes, according to our syntax
	PitchClassChord newFromNotesNames: 'A C# E G'  [La 7 ] "

	| l notes lastNote aNote |
	l := Scanner new scanTokens: aStringOfNoteNames.
	notes := OrderedCollection new.
	l do: [:x | x = '#' asSymbol
			ifTrue: 
				[lastNote := notes last.
				notes removeLast; addLast: lastNote sharp]
			ifFalse: [x = 'b' asSymbol
					ifTrue: 
						[lastNote := notes last.
						notes removeLast; addLast: lastNote flat]
					ifFalse: 
						[aNote := PitchClass noteNamed: x.
						notes addLast: aNote]]].
	^self new fromNotes: notes! !!PitchClassChord class methodsFor: 'creation'!newFromString: l 
	"computes the list of notes fom the name acording to our syntax"

	^self new fromString: l! !!PitchClassChord class methodsFor: 'creation'!root: r structure: s
	^self new root: r; structure: s! !!PitchClassChord class methodsFor: 'examples'!allChordsFromListOfNotes: aList 
	"self allChordsFromListOfNotes: (Array with: N do with: N mi with: N sol)
 #(Do  Mi #min #no5 #no11 #no9 #no7 #dim13  Sol #sus4 #no5 #sixth  )
	self allChordsFromListOfNotes: (Array with: N do with: N mi with: N sol with: N si)"

	"tries in all possible inversion, assuming that the root is actually present"
	^aList collect: [:n | self new fromNotes: aList root: n]! !!PitchClassChord class methodsFor: 'examples'!commonTonalities

"
(PitchClassChord new fromString: 'C maj7') 
	possibleTonalities AnalysisList ({I of C MajorScale} {IV of G MajorScale} {VI of E HarmonicMinorScale} )

(PitchClassChord new fromString: 'F maj7')
	possibleTonalities AnalysisList ({I of F MajorScale} {IV of C MajorScale} {VI of A HarmonicMinorScale} )

(PitchClassChord new fromString: 'E min 7')
	possibleTonalities  AnalysisList ({II of D MajorScale} {III of C MajorScale} {VI of G MajorScale} {IV of B HarmonicMinorScale} {II of D MelodicMinorScale} )
----
(PitchClassChord new fromString: 'C maj7') 
	tonalitesCommunesAvec: (PitchClassChord new fromString: 'F maj7')
	et: (PitchClassChord new fromString: 'E min 7') Set (C MajorScale )

"! !!PitchClassChord class methodsFor: 'examples'!exampleShort
	"'C min 7 dim5' asChord. 
	('C min 7 dim5 9' asChordOct: 4) notes
		OrderedCollection (C4 Eb4 Gb4 Bb4 D5 )"! !!PitchClassChord class methodsFor: 'examples'!examples
"
(PitchClassChord new fromString: 'Re maj7') notes OrderedCollection (Re Fa# La Do# )
(PitchClassChord new fromString: 'Re# maj7') notes  OrderedCollection (Re# Fa## La# Do## )
(PitchClassChord new fromString: 'C') notes    OrderedCollection (Do Mi Sol )
(PitchClassChord new fromString: 'D min 7 dim5') notes OrderedCollection (Re Fa Lab Do )
(PitchClassChord new fromString: 'C aug9') notes   OrderedCollection (Do Mi Sol Sib Re# )
(PitchClassChord new fromString: 'C aug9 dim5') notes OrderedCollection (Do Mi Solb Sib Re# )   
(PitchClassChord new fromString: 'C 13') notes OrderedCollection (Do Mi Sol Sib Re Fa La )
(PitchClassChord new fromString: 'C 13 aug9') notes OrderedCollection (Do Mi Sol Sib Re# Fa La )
(PitchClassChord new fromString: 'C 13 aug9 no7') notes OrderedCollection (Do Mi Sol Re# Fa La )
(PitchClassChord new fromString: 'C halfDim7') notes  OrderedCollection (Do Mib Solb Sib )

'F min 7' asChord notes > OrderedCollection (F Ab C Eb ) 	
'F min7' asChord notes  > OrderedCollection (F A C )		
'F m 7' asChord notes  > OrderedCollection (F A C Eb )		
'Fm 7' asChord notes > error	
'F maj 7' asChord notes > error	
'F m7' asChord notes  > OrderedCollection (F A C )		
'F maj 7' asChord notes > OrderedCollection (F A C Eb )		
'F maj7' asChord notes > OrderedCollection (F A C E )		
'F halfDim7' asChord notes  > OrderedCollection (F Ab Cb Eb ) 	
'F min dim5 7' asChord notes  OrderedCollection (F Ab Cb Eb )
'F min b5 7' asChord notes OrderedCollection (F Ab Cb Eb )
'F dim7' asChord notes OrderedCollection (F A C Ebb )

PitchClassChord new fromNotes: (Array with: N do with: N mi with: N sol)  Do
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N sol sharp) Do #aug5 
PitchClassChord new fromNotes: (Array with: N do with: N fa with: N sol )   	 Do #sus4
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N sol with: N la)  Do #sixth  
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N la)  Do #no5 #sixth 
PitchClassChord new fromNotes: (Array with: N do with: N la)  Do #no3 #no5 #sixth 
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N sol sharp with: N si)  Do #aug5 #maj7 
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N sol flat with: N si flat) Do #dim5 7
PitchClassChord new fromNotes: (Array with: N do with: N mi flat with: N sol flat with: N si flat)   Do #halfDim7
PitchClassChord new fromNotes: (Array with: N do with: N mi flat with: N sol flat with: N si flat flat)  Do #dim7 

PitchClassChord new fromNotes: (Array with: N do with: N mi flat) Do #min #no5 
PitchClassChord new fromNotes: (Array with: N do with: N mi with: N fa  with: N fa sharp )  Do #no5 #no9 #no7 11 #aug11 
PitchClassChord new fromNotes: (Array with: N do with: N sol flat with: N sol sharp) Do #no3 #dim5 #aug5

A chord of A. holdsworth
PitchClassChord new fromNotes: (Array with: N re sharp with: N fa sharp sharp  with: N la with: N do sharp sharp)   Re# #dim5 #maj7 


(PitchClassChord new fromString: 'C min') standardPossibleTonalities OrderedCollection ({II de Sib MajorScale} {III de Lab MajorScale} {VI de Mib MajorScale} {I de Do HarmonicMinorScale} {IV de Sol HarmonicMinorScale} {I de Do MelodicMinorScale} {II de Sib MelodicMinorScale} )

(PitchClassChord new fromString: 'Do') possibleTonalities
 ListeDAnalyses ({V de Fa HungarianMinor} {VI de Mi HungarianMinor} {I de Do MajorScale} {IV de Sol MajorScale} {V de Fa MajorScale} {I de Do DoubleHarmonic} {II de Si DoubleHarmonic} {IV de Sol MelodicMinorScale} {V de Fa MelodicMinorScale} {IV de Sol Oriental} {IV de Sol NeapolitanMajor} {V de Fa HarmonicMinorScale} {VI de Mi HarmonicMinorScale} {II de Si NeapolitanMinor} {VI de Mi NeapolitanMinor} )

(PitchClassChord new fromString: 'D min ') possibleTonalities
ListeDAnalyses ({I de Re HungarianMinor} {VII de Mib HungarianMinor} {II de Do MajorScale} {III de Sib MajorScale} {VI de Fa MajorScale} {III de Sib DoubleHarmonic} {IV de La DoubleHarmonic} {I de Re MelodicMinorScale} {II de Do MelodicMinorScale} {I de Re NeapolitanMajor} {I de Re HarmonicMinorScale} {IV de La HarmonicMinorScale} {I de Re NeapolitanMinor} {IV de La NeapolitanMinor} )
"! !!PitchClassChord class methodsFor: 'examples'!holdsworth
	^PitchClassChord new fromNotes: (Array
			with: N re sharp
			with: N fa sharp sharp
			with: N la
			with: N do sharp sharp)! !!PitchClassChord class methodsFor: 'examples'!reallyAllChordsFromListOfNotes: aList 
	"self reallyAllChordsFromListOfNotes: (Array with: N do with: N mi with: N sol)
 OrderedCollection ([La #noRoot #min 7 ] [Si #noRoot #sus4 #no5 #no7 #dim9 #dim13 ] [Do ] [Re #noRoot #sus4 #no5 7 9 ] [Mi #min #no5 #no11 #no9 #no7 #dim13 ] [Fa #noRoot #no3 #maj7 9 ] [Sol #sus4 #no5 #sixth ] [La# #noRoot #no3 #dim5 ] [Do# #noRoot #min #dim5 ] [Re# #noRoot #no3 #no5 #dim9 ] [Fa# #noRoot #no3 #dim5 7 #dim9 ] [Sol# #noRoot #no3 #no5 #no11 #no9 #no7 #dim13 ] [Lab #noRoot #aug5 #maj7 ] [Sib #noRoot #no3 #no5 #no7 9 #aug11 #sixth ] [Reb #noRoot #no3 #no5 #maj7 #aug9 #aug11 ] [Mib #noRoot #no5 #sixth ] [Solb #noRoot #no3 #no5 #no9 #no7 #aug11 ] )
"

	"tries in all possible inversion, assuming that the root is actually present"
	^PitchClass allPlausibleRootNotes collect: [:n | self new fromNotes: aList root: n]! !PitchClassChord subclass: #OctaveDependentChord	instanceVariableNames: 'notesByInterval'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!OctaveDependentChord commentStamp: '<historical>' prior: 0!notesBytInterval provides a fast access to the chord notes
by the names of their intervals with the root. The calculation 
is done once forever.  On the contrary,  for FPChord the
calculation is done all the times a note is required since 
its rarely required.
Note that for all request (e.g. theFifth) the copy of the note is 
answer). it is necessary in order to avoid some confusions. For instance,
the pitches (ODNotes) of  an simple arpeggio on a chord ch may be 
obtained by: 
	o := OredredCollection new.
	o add: ch theTonic; add: ch theThird; ch theFifth; theTonic.
the first and last must be different objects in the case we will assign 
them todifferents PlayableNotes.!!OctaveDependentChord methodsFor: 'saving'!storeOn: s
	s nextPut: $(.
	self quotedPrintOn: s.
	s nextPutAll: ' asChordOct: '.
	root oct printOn: s.
	s nextPut: $).! !!OctaveDependentChord methodsFor: 'mutations'!asMelody
	^self asPlayableObject asMelody! !!OctaveDependentChord methodsFor: 'accessing'!addNote: c 
	notes add: c! !!OctaveDependentChord methodsFor: 'accessing'!notes: value
	notes := value! !!OctaveDependentChord methodsFor: 'accessing'!removeNote: c 
	notes remove: c! !!OctaveDependentChord methodsFor: 'alteration'!octave: o
	root oct: o.
	self resetNotes! !!OctaveDependentChord methodsFor: 'alteration'!octaveFromNote: n 
	"set octave from new root or chord note"
	| int |
	int :=  n pitchClass intervalBetween: root pitchClass.
	int type = 1 	"n is the  root in a probably different octave"
		ifTrue: [self octave: n oct].
	"n is another chord note"
	(self containsNote: n) 
		ifFalse: [^self error: 'the given note is not note a chord note'].
	self octave: (n - int) oct! !!OctaveDependentChord methodsFor: 'copy'!copy
	| new |
	new := self shallowCopy.
	new root: root copy; structure: structure copy.
	^new! !!OctaveDependentChord methodsFor: 'private-pact'!closestDownTriadNoteTo: n
	"((FPChord newFromString: 'C  maj7') @ 3) closestTriadNoteTo: N B flat @ 2"
	| l best smallestInt testInt |
	l := self downTriadNotes.
	best := l detect: [:nt | nt  respectsBassTessitura] ifNone: [nil].
	best == nil ifTrue: [^nil].
	smallestInt := n intervalBetween: best.
	1 to: l size do: 
		[:i | 
		testInt := n intervalBetween: (l at: i).
		(testInt < smallestInt and: [(l at: i) respectsBassTessitura])
			ifTrue:
			 [best := l at: i.
			  smallestInt := testInt]].
	^best! !!OctaveDependentChord methodsFor: 'private-pact'!closestTriadNoteTo: n
	"((FPChord newFromString: 'C  maj7') @ 3) closestTriadNoteTo: N B flat @ 2"
	| l best smallestInt testInt |
	l := self upAndDownTriadNotes.
	best := l detect: [:nt | nt  respectsBassTessitura] ifNone: [nil].
	best == nil ifTrue: [^nil].
	smallestInt := n intervalBetween: best.
	1 to: l size do: 
		[:i | 
		testInt := n intervalBetween: (l at: i).
		(testInt < smallestInt and: [(l at: i) respectsBassTessitura])
			ifTrue:
			 [best := l at: i.
			  smallestInt := testInt]].
	^best! !!OctaveDependentChord methodsFor: 'private-pact'!closestUpTriadNoteTo: n
	"((PitchClassChord newFromString: 'C  maj7') @ 3) closestTriadNoteTo: N B flat @ 2"
	| l best smallestInt testInt |
	l := self upTriadNotes.
	best := l detect: [:nt | nt  respectsBassTessitura] ifNone: [nil].
	best == nil ifTrue: [^nil].
	smallestInt := n intervalBetween: best.
	1 to: l size do: 
		[:i | 
		testInt := n intervalBetween: (l at: i).
		(testInt < smallestInt and: [(l at: i) respectsBassTessitura])
			ifTrue:
			 [best := l at: i.
			  smallestInt := testInt]].
	^best! !!OctaveDependentChord methodsFor: 'private-pact'!containsLegalNote: n 
	n respectsBassTessitura
		ifFalse: [^false].
	^(self upTriadNotes detect: [:each | each pitchClass = n pitchClass]
					   ifNone: [nil]) notNil! !!OctaveDependentChord methodsFor: 'private-pact'!downTriadNotes
	"return all triad notes within 2 octaves in a particular order"

	| o |
	o := OrderedCollection new.
	o add: self theLowOctave.
	o add: self theLowFifth.
	o  add: self theLowThird.
	o add: self theTonic.
	^o! !!OctaveDependentChord methodsFor: 'private-pact'!triadDownNoteLeadingTo: n
	"for the conflicting cases uses the priority given by  the ordering
	of upAndDownTriadNotes "
	^self downTriadNotes 
		detect: [:each | 
				(each isLeadingToneTo: n)
				and: [each respectsBassTessitura]]
		ifNone: [nil]! !!OctaveDependentChord methodsFor: 'private-pact'!triadNoteLeadingTo: n
	"for the conflicting cases uses two priority criteria: the proximity and 
	the ordering given by triadNotesLeadingTo: "
	"('A  maj7' asChordOct: 3) triadNoteLeadingTo: N D  @ 4"

	| list |
	list := self triadNotesLeadingTo: n.
	list isEmpty ifTrue: [^nil].
	list size = 1 ifTrue: [^list first].
	(n isNearerTo: (list at: 2) than: list first)
		ifTrue: [^(list at: 2)]
		ifFalse: [^list first].! !!OctaveDependentChord methodsFor: 'private-pact'!triadNotesLeadingTo: n
	"answer the chord 'triad' notes  (extend to 7?) within an octave, 
	 that are leading notes to the given note n"
	"('A  maj7' asChordOct: 3) triadNotesLeadingTo: N D @ 4"
	^self upAndDownTriadNotes select: 
		[:each |
		(each isLeadingToneTo: n) 
			and: [each respectsBassTessitura]].! !!OctaveDependentChord methodsFor: 'private-pact'!triadUpNoteLeadingTo: n
	"for the conflicting cases uses the priority given by  the ordering
	of upAndDownTriadNotes "
	^self upTriadNotes 
		detect: [:each | 
				(each isLeadingToneTo: n)
				and: [each respectsBassTessitura]]
		ifNone: [nil]! !!OctaveDependentChord methodsFor: 'private-pact'!upAndDownTriadNotes
	"return all triad notes within 2 octaves in a particular order"
	| o |
	o := OrderedCollection new.
	o add: self theOctave.
	o  add: self theFifth.
	o  add: self theThird.
	o  add: self theTonic.
	o add: self theLowOctave.
	o add: self theLowFifth.
	o  add: self theLowThird.
	^o! !!OctaveDependentChord methodsFor: 'private-pact'!upTriadNotes
	"return all triad notes within 2 octaves in a particular order"
	| o |
	o := OrderedCollection new.
	o add: self theOctave.
	o  add: self theFifth.
	o  add: self theThird.
	o  add: self theTonic.
	^o! !!OctaveDependentChord methodsFor: 'initialize'!initialize
	super  initialize.
	notesByInterval := Dictionary new.! !!OctaveDependentChord methodsFor: 'initialize'!resetNotes
	"extend super class method for notesByIntevals"
	super resetNotes.
	(notesByInterval isEmpty or: [notesByInterval == nil])
		ifFalse: [notesByInterval := Dictionary new]! !!OctaveDependentChord methodsFor: 'notes by  intervals'!computeNotesByInterval
	"only for triad notes"
	notesByInterval := Dictionary new.
	notesByInterval at: #theTonic put: self reallyTheTonic.
	notesByInterval at: #theThird put: self  reallyTheThird.
	notesByInterval at: #theFifth put: self reallyTheFifth.
	notesByInterval at: #theOctave put: self reallyTheOctave.
	notesByInterval at: #theLowThird put: self reallyTheLowThird.
	notesByInterval at: #theLowFifth put: self reallyTheLowFifth.
	notesByInterval at: #theLowOctave put: self reallyTheLowOctave.! !!OctaveDependentChord methodsFor: 'notes by  intervals'!notesByInterval
	notesByInterval isEmpty ifTrue: [self computeNotesByInterval].
	^notesByInterval! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheFifth
	^super theFifth! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheLowFifth
	self  hasNoFifth
		ifTrue: [^nil].
	self hasADiminishedFifth
		ifTrue: [^root transposeOf: MusicalDescendingInterval augmentedFourth].
	self hasAnAugmentedFifth
		ifTrue: [^root transposeOf: MusicalDescendingInterval diminishedFourth].
	^root transposeOf: MusicalDescendingInterval fourth.! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheLowOctave

	^root transposeOf: MusicalDescendingInterval octave.! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheLowThird
	"attention: do not cofound with 'root downThird  interval"
	self isSuspended
		ifTrue: [^root transposeOf: MusicalDescendingInterval fifth]. 
	self isMinor
		ifTrue: [^root  transposeOf: MusicalDescendingInterval majorSixth].
	^ root transposeOf: MusicalDescendingInterval minorSixth.! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheOctave
	^super theOctave! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheThird
	^super theThird! !!OctaveDependentChord methodsFor: 'notes by  intervals'!reallyTheTonic
	^super theTonic! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theFifth
	^(self notesByInterval at: #theFifth) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theLowFifth
	^(self notesByInterval at: #theLowFifth) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theLowOctave
	^(self notesByInterval at: #theLowOctave) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theLowThird
	^(self notesByInterval at: #theLowThird) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theOctave
	^(self notesByInterval at: #theOctave) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theThird
	^(self notesByInterval at: #theThird) copy! !!OctaveDependentChord methodsFor: 'notes by  intervals'!theTonic
	^(self notesByInterval at: #theTonic) copy! !!OctaveDependentChord methodsFor: 'As yet unclassified'!theLowSeventh
	"WARN: the calculatin of the seven is done in every request"

	self  hasNoSeventh
		ifTrue: [^nil].
	self hasMajorSeventh
		ifTrue: [^root - #minorSecond].
	self hasADiminishedSeventh 
		ifTrue: [^root - #minorThird].
	^root - #majorSecond! !!OctaveDependentChord methodsFor: 'FP'!allPitchClasses
	^notes collect: [:n | n pitchClass]! !!OctaveDependentChord methodsFor: 'transposing'!beTransposedOneStepDown
	root beTransposedOneStepDown.
	self resetNotes! !!OctaveDependentChord methodsFor: 'transposing'!beTransposedOneStepUp
	root beTransposedOneStepUp.
	self resetNotes! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!OctaveDependentChord class	instanceVariableNames: ''!!OctaveDependentChord class methodsFor: 'cation'!newWithValues: anArray 
	^self new newWithValues: anArray! !!OctaveDependentChord class methodsFor: 'cation' stamp: 'stp 12/31/2022 07:11'!readFromFile	| n |	n := FillInTheBlank request: 'name of the file' initialAnswer: 'cc.chord'.	n isEmpty		ifTrue: [^ nil].	^ self readFromFile: n! !!OctaveDependentChord class methodsFor: 'cation'!root: r notes: n 
	^(self new) root: r; notes: n! !!OctaveDependentChord class methodsFor: 'examples'!example
	^N C majorTriad fondamental! !!OctaveDependentChord class methodsFor: 'examples'!example1
	"self example1"
	^(PitchClassChord newFromString: 'A  maj7') octave: 3! !!OctaveDependentChord class methodsFor: 'examples'!example2
	^N C majorTriad fondamental! !PitchClassAltered subclass: #PitchClassDoubleFlat	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassDoubleFlat methodsFor: 'saving'!representBinaryOn: s
	^MessageSend receiver: natural selector:  #doubleFlat! !!PitchClassDoubleFlat methodsFor: 'intervals'!alterate: note toReach: i 
	| n |
	n := natural alterate: note toReach: i.
	^n flat flat! !!PitchClassDoubleFlat methodsFor: 'intervals'!alterateBelow: note toReach: i 
	| n |
	n := natural alterateBelow: note toReach: i.
	^n flat flat! !!PitchClassDoubleFlat methodsFor: 'intervals'!closestEnharmonic

	^self alterate: self preceding toReach: 0.! !!PitchClassDoubleFlat methodsFor: 'intervals'!semiTonesToNatural
	^2! !!PitchClassDoubleFlat methodsFor: 'access'!flat
	"Sorry, I have to do that (Cf. Bluesette) otherwise I can't 
	compute possibleTonalities properly ..."

	^nil! !!PitchClassDoubleFlat methodsFor: 'access'!semiToneCount
	^natural semiToneCount - 2! !!PitchClassDoubleFlat methodsFor: 'access'!sharp
	"Methode flat is not defined. 
	Exemple C flat diminishedSeventh.."

	^natural flat! !!PitchClassDoubleFlat methodsFor: 'printing'!printOn: s 
	natural printOn: s.
	s nextPutAll: 'bb'! !!PitchClassDoubleFlat methodsFor: 'printing'!storeOn: s 
	"N C flat flat storeString"

	s nextPutAll: 'N ' , self natural name, ' flat flat'! !!PitchClassDoubleFlat methodsFor: 'testing'!isDoubleFlat
	^true! !PitchClassAltered subclass: #PitchClassDoubleSharp	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassDoubleSharp methodsFor: 'saving'!representBinaryOn: s
	^MessageSend receiver: natural selector:  #doubleSharp! !!PitchClassDoubleSharp methodsFor: 'intervals'!alterate: note toReach: i 
	| n |
	n := natural alterate: note toReach: i.
	^n sharp sharp! !!PitchClassDoubleSharp methodsFor: 'intervals'!alterateBelow: note toReach: i 
	| n |
	n := natural alterateBelow: note toReach: i.
	^n sharp sharp! !!PitchClassDoubleSharp methodsFor: 'intervals'!closestEnharmonic
	^self alterate: self following toReach: 0.! !!PitchClassDoubleSharp methodsFor: 'intervals'!semiTonesToNatural
	^-2! !!PitchClassDoubleSharp methodsFor: 'access'!flat
	"la methode diese n'est pas definie et provoque donc une erreur"

	^natural sharp! !!PitchClassDoubleSharp methodsFor: 'access'!semiToneCount
	^natural semiToneCount + 2! !!PitchClassDoubleSharp methodsFor: 'access'!sharp
	"Sorry, I have to do that (Cf. Bluesette) otherwise I can't 
	compute possibleTonalities properly ..."

	^nil! !!PitchClassDoubleSharp methodsFor: 'printing'!printOn: s 
	natural printOn: s.
	s nextPutAll: '##'! !!PitchClassDoubleSharp methodsFor: 'printing'!storeOn: s 
	"N C sharp sharp storeString"

	s nextPutAll: 'N ' , self natural name , ' sharp sharp'! !!PitchClassDoubleSharp methodsFor: 'testing'!isDoubleSharp
	^true! !PitchClassAltered subclass: #PitchClassFlat	instanceVariableNames: 'flat'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassFlat methodsFor: 'intervals'!alterate: note toReach: i 
	| n |
	n := natural alterate: note toReach: i.
	^n flat! !!PitchClassFlat methodsFor: 'intervals'!alterateBelow: note toReach: i 
	| n |
	n := natural alterateBelow: note toReach: i.
	^n flat! !!PitchClassFlat methodsFor: 'intervals'!closestEnharmonic
	^self alterate: self preceding toReach: 0.! !!PitchClassFlat methodsFor: 'intervals'!semiTonesToNatural
	^1! !!PitchClassFlat methodsFor: 'saving'!representBinaryOn: s 
	^MessageSend receiver: natural selector: #flat! !!PitchClassFlat methodsFor: 'printing'!printOn: s 
	natural printOn: s.
	s nextPut: $b! !!PitchClassFlat methodsFor: 'printing'!storeOn: s 
	"N C flat storeString"

	s nextPutAll: 'N ' , self natural name , ' flat'! !!PitchClassFlat methodsFor: 'testing'!isFlat
	^true! !!PitchClassFlat methodsFor: 'accessing'!flat
	^flat! !!PitchClassFlat methodsFor: 'accessing'!flat: value
	flat := value! !!PitchClassFlat methodsFor: 'accessing'!semiToneCount
	^natural semiToneCount - 1! !!PitchClassFlat methodsFor: 'accessing'!sharp
	^natural! !PitchClass subclass: #PitchClassNatural	instanceVariableNames: 'semiToneCount nom name following preceding sharp flat'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassNatural methodsFor: 'intervals'!alterate: note toReach: i
	"Join semitons to note in order to obtain the required number of semitones with self" 

	| delta |
	delta := (self  semiTonesWithNaturalNote: note) - i.
	delta := ((delta + 6) \\ 12) -6.
	delta = 0 ifTrue: [^note].
	delta = 1 ifTrue: [^note flat].
	delta = -1 ifTrue: [^note sharp].
	delta = 2 ifTrue: [^note flat flat].
	delta = -2 ifTrue: [^note sharp sharp].
	delta = 3 ifTrue: [^note flat flat flat].	"to deal with cases like N B flat flat augmentedOctave"
	delta = -3 ifTrue: [^note sharp sharp sharp].
	^self error: 'illegal interval !!'! !!PitchClassNatural methodsFor: 'intervals'!alterateBelow: note toReach: i
	"Join semitons to note in order to obtain the required number of semitones with self" 

	| delta |
	delta := (self  semiTonesWithNaturalNoteBelow: note) - i.
	delta := ((delta + 6) \\ 12) -6.
	delta = 0 ifTrue: [^note].
	delta = -1 ifTrue: [^note flat].
	delta = 1 ifTrue: [^note sharp].
	delta = -2 ifTrue: [^note flat flat].
	delta = 2 ifTrue: [^note sharp sharp].
	delta = -3 ifTrue: [^note flat flat flat].	"to deal with cases like N B flat flat augmentedOctave"
	delta = 3 ifTrue: [^note sharp sharp sharp].
	^self error: 'illegal interval !!'! !!PitchClassNatural methodsFor: 'intervals'!closestEnharmonic
	^self! !!PitchClassNatural methodsFor: 'intervals'!semiTonesToNatural
	^0! !!PitchClassNatural methodsFor: 'intervals'!semiTonesWithNaturalNote: aNote 
	"number of semitones between self and aNote"

	| r |
	r := aNote semiToneCount - semiToneCount.
	"^r + 6 \\ 12 - 6"
	^r \\ 12! !!PitchClassNatural methodsFor: 'intervals'!semiTonesWithNaturalNoteBelow: aNote 
	"number of semitones between aNote and self"

	| r |
	r :=  semiToneCount - aNote semiToneCount .
	"^r + 6 \\ 12 - 6"
	^r \\ 12! !!PitchClassNatural methodsFor: 'accessing'!flat
	^flat! !!PitchClassNatural methodsFor: 'accessing'!flat: value
	flat := value! !!PitchClassNatural methodsFor: 'accessing'!name
	^name! !!PitchClassNatural methodsFor: 'accessing'!name: x 
	name := x! !!PitchClassNatural methodsFor: 'accessing'!natural
	^self! !!PitchClassNatural methodsFor: 'accessing'!nom
	^nom! !!PitchClassNatural methodsFor: 'accessing'!nom: value
	nom := value! !!PitchClassNatural methodsFor: 'accessing'!sharp
	^sharp! !!PitchClassNatural methodsFor: 'accessing'!sharp: value
	sharp := value! !!PitchClassNatural methodsFor: 'private-accessing'!following
	^following! !!PitchClassNatural methodsFor: 'private-accessing'!following: value
	following := value! !!PitchClassNatural methodsFor: 'private-accessing'!preceding
	^preceding! !!PitchClassNatural methodsFor: 'private-accessing'!preceding: value
	preceding := value! !!PitchClassNatural methodsFor: 'private-accessing'!semiToneCount
	^semiToneCount! !!PitchClassNatural methodsFor: 'private-accessing'!semiToneCount: s
	semiToneCount := s! !!PitchClassNatural methodsFor: 'printing'!nameIs: aSymbol 
	name = aSymbol ifTrue: [^true].
	^nom = aSymbol! !!PitchClassNatural methodsFor: 'printing'!printOn: s 
	s nextPutAll: (English
			ifTrue: [name]
			ifFalse: [nom])! !!PitchClassNatural methodsFor: 'saving'!representBinaryOn: s
	^MessageSend receiver: self class selector:  name! !PitchClassAltered subclass: #PitchClassSharp	instanceVariableNames: 'sharp'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!PitchClassSharp methodsFor: 'intervals'!alterate: note toReach: i 
	| n |
	n := natural alterate: note toReach: i.
	^n sharp! !!PitchClassSharp methodsFor: 'intervals'!alterateBelow: note toReach: i 
	| n |
	n := natural alterateBelow: note toReach: i.
	^n sharp! !!PitchClassSharp methodsFor: 'intervals'!closestEnharmonic

	^self alterate: self following toReach: 0.! !!PitchClassSharp methodsFor: 'intervals'!semiTonesToNatural
	^-1! !!PitchClassSharp methodsFor: 'saving'!representBinaryOn: s
	^MessageSend receiver: natural selector:  #sharp! !!PitchClassSharp methodsFor: 'printing'!printOn: s 
	natural printOn: s.
	s nextPut: $#! !!PitchClassSharp methodsFor: 'printing'!storeOn: s 
	"N C sharp storeString"

	s nextPutAll: 'N ' , self natural name , ' sharp '! !!PitchClassSharp methodsFor: 'testing'!isSharp
	^true! !!PitchClassSharp methodsFor: 'accessing'!flat
	^natural! !!PitchClassSharp methodsFor: 'accessing'!semiToneCount
	^natural semiToneCount + 1! !!PitchClassSharp methodsFor: 'accessing'!sharp
	^sharp! !!PitchClassSharp methodsFor: 'accessing'!sharp: value
	sharp := value! !Object subclass: #Signature	instanceVariableNames: 'sharps flats'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!Signature commentStamp: '<historical>' prior: 0!Notion of signature is related to the notion of "legal" scales, i.e.
scales having only sharps OR flats in their signature.
So method tonality here works only for legal scales.
It should possible to compute the tonality for arbitrary scales though,
but I am not sure how!!Signature methodsFor: 'printing'!printOn: s
	s nextPutAll: sharps size printString,' sharps; ',flats size printString,' flats'! !!Signature methodsFor: 'printing'!storeOn: s 
	self isLegal ifTrue: [^s nextPutAll: 'Signature fromTonality: ' , self tonality root storeString , ' majorScale'].
	s nextPutAll: 'Signature sharps: ' , sharps storeString , ' flats: ' , flats storeString! !!Signature methodsFor: 'testing'!flatsInRightOrder
	| allFlats |
	allFlats := PitchClass flatOrdering.
	1 to: flats size do: [:i | (flats at: i) natural = (allFlats at: i) natural ifFalse: [^false]].
	^true! !!Signature methodsFor: 'testing'!isEmpty
	^sharps isEmpty and: [flats isEmpty]! !!Signature methodsFor: 'testing'!isLegal
	^self flatsInRightOrder and: [self sharpsInRightOrder]! !!Signature methodsFor: 'testing'!sharpsInRightOrder
	| allSharps |
	allSharps := PitchClass sharpOrdering.
	1 to: sharps size do: [:i | (sharps at: i) natural = (allSharps at: i) natural ifFalse: [^false]].
	^true! !!Signature methodsFor: 'tonality'!tonality
	(sharps isEmpty not and: [flats isEmpty not])
		ifTrue: [^self error: 'tonality is difficult to compute'].
	(sharps isEmpty and: [flats isEmpty])
		ifTrue: [^N C majorScale].
	flats isEmpty ifTrue: [^self sharpsInRightOrder
			ifTrue: [sharps last natural following majorScale]
			ifFalse: [self error: 'tonality is difficult to compute']].
	sharps isEmpty ifTrue: [^self flatsInRightOrder
			ifTrue: [flats size = 1
					ifTrue: [^N F majorScale]
					ifFalse: [(flats at: flats size - 1) majorScale]]
			ifFalse: [self error: 'tonality is difficult to compute']]! !!Signature methodsFor: 'access'!alterate: aNaturalNote
	sharps do: [:s | s natural = aNaturalNote pitchClass ifTrue: [^aNaturalNote sharp]].
	flats do: [:s | s natural = aNaturalNote pitchClass ifTrue: [^aNaturalNote flat]].
	^aNaturalNote! !!Signature methodsFor: 'access'!flats
	^flats! !!Signature methodsFor: 'access'!initialize
	sharps := OrderedCollection new.
	flats := OrderedCollection new! !!Signature methodsFor: 'access'!nbFlats: n
	sharps := OrderedCollection new.
	flats := PitchClass nFirstFlats: n.! !!Signature methodsFor: 'access'!nbSharps: n 
	flats := OrderedCollection new.
	sharps := PitchClass nFirstSharps: n! !!Signature methodsFor: 'access'!reOrderSharpsAndFlats
	"attempts to reorder flats and sharps according to the natural order as found in 
	PitchClass"

	| allFlats allSharps |
	allFlats := PitchClass flatOrdering.
	allSharps := PitchClass sharpOrdering.
	flats := (flats asSortedCollection: [:a :b | (allFlats indexOf: a natural)
					<= (allFlats indexOf: b natural)]) asOrderedCollection.
	sharps := (sharps asSortedCollection: [:a :b | (allSharps indexOf: a natural)
					<= (allSharps indexOf: b natural)]) asOrderedCollection! !!Signature methodsFor: 'access'!sharps
	^sharps! !!Signature methodsFor: 'access'!sharps: s flats: f 
	"attempts to reorder flats and sharps according to the natural order as found in 
	PitchClass"

	sharps := s.
	flats := f.
	self reOrderSharpsAndFlats! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Signature class	instanceVariableNames: ''!!Signature class methodsFor: 'examples'!example
	^Signature choseSignature! !!Signature class methodsFor: 'examples'!example2
	"Signature new tonality -> C MajorScale
	(Signature new nbSharps: 4) tonality -> E MajorScale	 
	(Signature new nbFlats: 3) tonality -> Eb MajorScale"! !!Signature class methodsFor: 'examples'!example3
"	Signature fromTonality: N D majorScale
	Signature fromTonality: N E flat majorScale"! !!Signature class methodsFor: 'examples'!example4
	"Signatures may be created for illegal scales. However, method 
	tonality yields an error for these scales: 
	
	Signature fromTonality: N D minorScale -> 1 sharps; 1 flats 
	Signature sharps: (Array with: N F sharp) flats: (Array with: (N E flat)) -> 1 sharps; 1 flats
	(Signature fromTonality: N D minorScale) tonality -> error"! !!Signature class methodsFor: 'creation' stamp: 'stp 12/31/2022 07:10'!choseSignature	"Signature choseSignature"	| choice s |	s := Signature new.	choice := FillInTheBlank				choose: 'Choose a signature'				fromList: #('no sharp/falt [C Major]' '1 sharp [G Major]' '2 sharps [D Major]' '3 sharps [A Major]' '4 sharps [E Major]' '5 sharps [B Major]' '6 sharps [F# Major]' '7 sharps [C# Major]' '1 flat [F Major]' '2 flats [Bb Major]' '3 flats [Eb Major]' '4 flats [Ab Major]' '5 flats [Dd Major]' '6 flats [Gg Major]' '7 flats [Cb Major]')				values: (1 to: 15)				lines: 15				cancel: [].	choice == nil		ifTrue: [^ s].	choice < 9		ifTrue: [s nbSharps: choice - 1]		ifFalse: [s nbFlats: choice - 8].	^ s! !!Signature class methodsFor: 'creation'!fromTonality: aScale 
	"Signature fromTonality: N E flat majorScale"

	^self new sharps: aScale sharpNotesInSignature flats: aScale flatNotesInSignature! !!Signature class methodsFor: 'creation'!new
^super new initialize! !!Signature class methodsFor: 'creation'!sharps: s flats: f
	^self new sharps: s flats: f! !PitchClassChord subclass: #Triad	instanceVariableNames: 'type'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-PitchClasses'!!Triad methodsFor: 'type'!type
	^type! !!Triad methodsFor: 'type'!type: t 
	type := t! !!Triad methodsFor: 'notes'!notes: x 
	notes := x! !PitchClass initialize!