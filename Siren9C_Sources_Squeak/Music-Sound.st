SampledSound subclass: #FloatSound	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!FloatSound commentStamp: '<historical>' prior: 0!Instances of FloatSound are used for sounds with 32-bit floating-point numbers as samples.
There are behaviors for mapping into other formats.!!FloatSound methodsFor: 'accessing'!format
	"Return the receiver's format--a symbol constant."

	^#float32Bit! !!FloatSound methodsFor: 'accessing'!sampleSize
	"Answer the size in bytes of the receiver's samples (4)."

	^4! !!FloatSound methodsFor: 'private'!privateSampleAt: index
	"Answer the sample at the given index."

	^data floatAt: (index -1 * 4 + 1)! !!FloatSound methodsFor: 'private'!privateSampleAt: index put: aValue
	"Stuff the given sample into the data array at the given index (no checking)."

	data floatAt: (index -1 * 4 + 1) put: aValue asFloat! !!FloatSound methodsFor: 'sample accessing'!intSampleAt: index
	"Answer the sample at the given index as a scaled 16-bit integer."

	^((self sampleAt: index) * 32767.0) rounded! !!FloatSound methodsFor: 'sample accessing'!intSampleAt: index put: aValue
	"Put the given integer sample at the given index after scaling."

	self sampleAt: index put: (aValue asFloat / 32767.0)! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!FloatSound class	instanceVariableNames: ''!!FloatSound class methodsFor: 'class constants'!formatSymbol
	"Answer the symbolic code used for the format of the receiver's instances."

	^#float32Bit! !!FloatSound class methodsFor: 'class constants'!maxSample
	"Answer the maximum value of the receiver class."

	^1.0! !!FloatSound class methodsFor: 'class constants'!minSample
	"Answer the minimum value of the receiver class."

	^-1.0! !!FloatSound class methodsFor: 'examples'!fileExample
	"Answer a typical float sound read in from a file."
	"SoundView openOn: FloatSound fileExample"

	^FloatSound fromFile: 'snd/new/orchestra.f.snd'! !Function subclass: #Sound	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!Sound commentStamp: '<historical>' prior: 0!Instances of the subclasses of Sound are used to represent sound objects.
The abstract class Sound is vacuous.
Sounds use their Function and DurationEvent behaviors actively.
!!Sound methodsFor: 'testing'!isSound
	"Answer true."

	^true! !!Sound methodsFor: 'initialize-release'!finalize
	"Free the receiver's external data, if any"

	| fileNumber |
	fileNumber := self at: #fileIndex.
	fileNumber ifNotNil:
		[SoundFile releaseFileIndex: fileNumber]! !!Sound methodsFor: 'initialize-release'!initialize
	"Set up a default sound."

	self propCheck.
	range := -1.0 to: 1.0! !!Sound methodsFor: 'cue accessing'!addCueNamed: cName from: start to: stop
	"Add the given named cue region to the receiver's list."

	| list key |
	self cueList == nil
		ifTrue: [self cueList: OrderedCollection new].
	list := self cueList.
	key := cName asSymbol.
	1 to: list size do:
		[ :ind |
		((list at: ind) key == key)
			ifTrue: [^list at: ind put: (key -> (start to: stop))]].
	list add: (cName asSymbol -> (start to: stop)).! !!Sound methodsFor: 'cue accessing'!cueList
	"Answer the receiver's 'cueList'."

	properties ifNil: [^nil].
	^properties at: #cueList ifAbsent: [nil]! !!Sound methodsFor: 'cue accessing'!cueList: CollectionOfCues
	"Set the receiver's 'cueList' to be the given OrderedCollection of (#name -> (start to: stop)) cues."

	^properties at: #cueList put: CollectionOfCues! !!Sound methodsFor: 'cue accessing'!cueNamed: cName
	"Answer a sound derived from the receiver using the samples between the named cue points."

	^self subclassResponsibility! !!Sound methodsFor: 'cue accessing'!cueRegionNamed: cName
	"Answer a indices for the samples between the named cue points."

	| q |
	self cueList == nil
		ifTrue: [self error: 'There is no cue by the name ', 
				cName printString, ' in this sound.'].
	q := self cueList detect: [ :c | c key == cName] ifNone: [nil].
	q == nil
		ifTrue: [self error: 'There is no cue by the name ', 
				cName printString, ' in this sound.'].
	^q value! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!Sound class	instanceVariableNames: ''!!Sound class methodsFor: 'instance accessing'!named: aName
	"Answer the sound by the given name, or nil."

	^SirenSession sounds at: aName asSymbol ifAbsent: [nil]! !!Sound class methodsFor: 'instance accessing'!named: aName put: aSound
	"Put the given sound in the shared dictionary under the given name."

	SirenSession sounds ifNil: [^nil].
	^SirenSession sounds at: aName asSymbol put: aSound! !!Sound class methodsFor: 'utilities'!fromFile: nameString
	"Open the given file (AIFF, IRCAM, NeXT, or SPARC soundfile format)"
	"Sound fromFile: 'kombination.snd'"

	| snd |
	[snd := (SoundFile readFileNamed: nameString).
	snd ifNotNil: [snd := snd sound]]
		on: Core.Error
		do: [ :ex |  "ignore " "self error: 'Cannot read sound file named ', nameString"].
	^snd! !!Sound class methodsFor: 'utilities'!playFile: theName
	"Play the sound file with the given name"

	SirenUtility playSoundFile: theName! !Sound subclass: #ComponentSound	instanceVariableNames: 'start stop sound'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!ComponentSound commentStamp: '<historical>' prior: 0!A ComponentSound is used as a component (a "splice" element, if you will).
It represents another sound that "composes" the composite.

Instance Variables:
	sound <StoredSound> the "subject" sound 
	start <Integer> the starting sample in the composite
	stop <Integer> the ending sample in the composite
	offset <Number> the offset into the selection!!ComponentSound methodsFor: 'accessing'!offset
	"Answer the receiver's 'offset'."

	^offset! !!ComponentSound methodsFor: 'accessing'!offset: anObject
	"Set the receiver's 'offset' to be anObject."

	offset := anObject! !!ComponentSound methodsFor: 'accessing'!sound
	"Answer the receiver's 'sound'."

	^sound! !!ComponentSound methodsFor: 'accessing'!sound: anObject
	"Set the receiver's 'sound' to be anObject."

	sound := anObject! !!ComponentSound methodsFor: 'accessing'!start
	"Answer the receiver's 'start'."

	^start! !!ComponentSound methodsFor: 'accessing'!start: anObject
	"Set the receiver's 'start' to be anObject."

	start := anObject! !!ComponentSound methodsFor: 'accessing'!stop
	"Answer the receiver's 'stop'."

	^stop! !!ComponentSound methodsFor: 'accessing'!stop: anObject
	"Set the receiver's 'stop' to be anObject."

	stop := anObject.
	((start ~~ nil) & (sound ~~ nil))
		ifTrue: [self duration: (stop - start / sound rate) asFloat]! !!ComponentSound methodsFor: 'printing'!printOn: aStream
	"Pretty-print the receiver on the argument."

	| ra |
	sound ifNil: [^super printOn: aStream].
	ra := sound rate asFloat.
	aStream nextPutAll: '(ComponentSound on ', sound name, 
			' from ', ((start asFloat / ra) printWidth: 6), 
			' to ', ((stop asFloat / ra) printWidth: 6).
	offset ifNotNil: 
		[aStream nextPutAll: ' starting at ', offset printString].
	aStream nextPutAll: ')'; cr.! !!ComponentSound methodsFor: 'testing'!includes: anIndex
	"Answer whether the argument is within the range of the receiver."

	^(anIndex >= start) & (anIndex <= stop)! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!ComponentSound class	instanceVariableNames: ''!!ComponentSound class methodsFor: 'instance creation'!on: sound from: start to: stop
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop! !!ComponentSound class methodsFor: 'instance creation'!on: sound start: start stop: stop
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop! !!ComponentSound class methodsFor: 'instance creation'!on: sound start: start stop: stop offset: offset
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop; offset: offset! !SampledSound subclass: #VirtualSound	instanceVariableNames: 'source'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!VirtualSound commentStamp: '<historical>' prior: 0!An instance of VirtualSound is a "reference" to another sound, typically by a named cue list entry.

Instance Variables:
	source	<StoredSound> the sound to which the VirtualSound points!!VirtualSound methodsFor: 'accessing'!cue: cueName
	"Set the cue name of the receiver"

	properties at: #cue put: cueName.
	domain := source cueRegionNamed: cueName.
	duration := SecondDuration value: (self size / self rate) asFloat.
	self name: self name, '.', cueName! !!VirtualSound methodsFor: 'accessing'!source
	"Answer the receiver's source sound."

	^source! !!VirtualSound methodsFor: 'accessing'!source: aSound
	"Set the receiver's source sound."

	source := aSound.
	self rate: source rate.
	self channels: source channels.
	self name: source name.
	self domain: aSound domain! !!VirtualSound methodsFor: 'accessing'!start: start
	"Set the argument as the starting sample of the receiver."

	properties at: #start put: start.
	(properties includesKey: #stop)
		ifTrue: [self size: (properties at: #stop) - start].! !!VirtualSound methodsFor: 'accessing'!stop: stop
	"Set the argument as the ending sample of the receiver."

	properties at: #stop put: stop.
	(properties includesKey: #start)
		ifTrue: [self size: stop - (properties at: #start)].! !!VirtualSound methodsFor: 'sample accessing'!sampleAt: index put: aValue
	"Stuff the given sample at the given index **after transforming into a 'real' sound**."

	self getReal.
	^super sampleAt: index put: aValue! !!VirtualSound methodsFor: 'private'!getReal
	"Answer a 'real sound' based on the receiver."

	| newSound |
	newSound := self copyAllButSamples.
	1 to: self size do:
		[ :ind |
		newSound sampleAt: ind put: (self sampleAt: ind)].
	self become: newSound! !!VirtualSound methodsFor: 'private'!mapSampleIndex: index
	"Map the given sample index according to the 'page' (firstIndex) of the source and the receiver's relative offset."

	^(source mapSampleIndex: index) + firstIndex! !!VirtualSound methodsFor: 'private'!realSound
	"Answer the real sound for the receiver."

	^source! !!VirtualSound methodsFor: 'testing'!isVirtual
	"Answer whether the receiver is a virtual sound."

	^true! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!VirtualSound class	instanceVariableNames: ''!!VirtualSound class methodsFor: 'instance creation'!from: source cue: cName
	"Answer a VirtualSound derived from the argument and named cue region."
	"VirtualSound exampleView"

	| me |
	me := self new initialize.
	me source: source.
	me cue: cName.
	^me! !!VirtualSound class methodsFor: 'examples'!exampleView
	"Open a sound view on a ramp with a chunk cut out of it."
	"VirtualSound exampleView"

	SoundView openOn: (VirtualSound from: 
			(SampledSound fromFile: 'snd/AllGatesAreOpen/nml1.8e.snd') 
				cue: #stream3)! !VirtualSound subclass: #CompositeSound	instanceVariableNames: 'components'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!CompositeSound commentStamp: '<historical>' prior: 0!An instance of CompositeSound can be used to represent a sound constructed by "splicing together" sections of other sounds.
It uses its components collection to maintain sounds and sample ranges that have been pasted together.
It responds to sampleAt: and sampleAt:put: just like other sounds, but stores only its components list on files unless explicitly told to store samples.

Instance Variables:
	components <SortedCollection of ComponentSounds> the "sub-sounds" of the CompositeSound!!CompositeSound methodsFor: 'sample accessing'!sampleAt: index
	"Answer the sample at the given index, mapping and paging as necessary."

	| q |
	1 to: components size do:
		[ :ind | 
		q := components at: ind.
		(q includes: index)
			ifTrue: [^q sound sampleAt: (index - q start + q offset)]].
	self error: 'No sample by that name, Sir.'! !!CompositeSound methodsFor: 'private'!rippleUpFrom: start adding: insert
	"Shift all the cues above the given one up by the given insert count."

	| item |
	start to: components size do:
		[ :ind | 
		item := (components at: ind).
		item start: (item start + insert).
		item stop: (item stop + insert)].! !!CompositeSound methodsFor: 'accessing'!on: aSound
	"Set the receiver's source sound."

	components := SortedCollection sortBlock: [ :x :y | x start <= y start].
	components add: (ComponentSound on: aSound start: 1 stop: aSound size).
	self rate: aSound rate.
	self channels: aSound channels.
	self size: aSound size.
	properties := aSound properties copy! !!CompositeSound methodsFor: 'accessing'!paste: sound from: start to: stop at: index
	"Paste the designated section into the receiver."

	| newItem item number extent oldStop |
	extent := stop - start.
	newItem := ComponentSound on: sound 
				start: index stop: stop - start + index offset: start.
	index >= self size						"if pasting onto the end of the sound"
		ifTrue: [components add: newItem.
				^self].
	number := 1.							" else locate where to put it"
	[(number > components size) | ((components at: number) includes: index)] 
		whileFalse: [number := number + 1].
	item := components at: number.
	index = item stop						"if we're pasting at the end of another splice"
		ifTrue: [self rippleUpFrom: (number + 1) adding: extent.
			components add: newItem.
			^self].
	index = item start						"if we're pasting at the start of another splice"
		ifTrue: [self rippleUpFrom: number adding: extent.
			components add: newItem.
			^self].
											"else we have to break up an existing component"
	self rippleUpFrom: (number + 1) adding: extent.
	oldStop := item stop.
	item stop: index - item start.			"edit the old entry in place"
	components add: newItem.			"insert the new item"
								"add a copy of the old item with new interval and offset"
	components add: (ComponentSound on: item sound 
			start: (index + extent + 1) stop: (oldStop + extent) 
			offset: (item offset + (index - item start) + extent)).
	^self! !!CompositeSound methodsFor: 'testing'!isComposite
	"Answer whether the receiver is a composite sound."

	^true! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!CompositeSound class	instanceVariableNames: ''!!CompositeSound class methodsFor: 'instance creation'!on: source paste: newSound from: start to: stop at: index
	"Answer a CompositeSound derived from the argument pasting in the given sound at the given index."
	"CompositeSound exampleView"

	| me |
	me := self new initialize.
	me on: source.
	me paste: newSound from: start to: stop at: index.
	^me! !!CompositeSound class methodsFor: 'examples'!exampleView
	"Open a sound view on a ramp with a chunk of a sine pasted into it."
	"CompositeSound exampleView"

	| ramp sine |
	ramp := SampledSound rampDur: 1.0 rate: 8000 chans: 1.
	sine := SampledSound sineDur: 0.5 rate: 8000 freq: 400 chans: 1.
	SoundView openOn: (ramp paste: sine from: 200 to: 400 at: 1000)! !VirtualSound subclass: #GapSound	instanceVariableNames: 'cutList'	classVariableNames: ''	poolDictionaries: ''	category: 'Music-Sound'!!GapSound commentStamp: '<historical>' prior: 0!An instance of GapSound can be used to represent a sound with samples deleted from it (i.e., a gap).
It uses its cut list instance variable to maintain sample ranges that have been deleted from the sound that comprises it (the source).
It responds to sampleAt: and sampleAt:put: just like other sounds, but stores only its cut list on files unless explicitly told to store samples.

Instance Variables:
	cutList		<SortedCollection of Integers> the list of deleted sections by sample index i.e., pointers denoting sections that have been deleted. For example, having a cutList of (1000 2000) means that samples (1000, 2000] have been virtually cut.
!!GapSound methodsFor: 'private'!mapSampleIndex: index
	"Map the given sample index according to the cut list (i.e., deleted sections) of the receiver."

	| mappedIndex |
	((cutList == nil) or: [cutList size < 2])
		ifTrue: [^index - firstIndex].
	mappedIndex := index.
	mappedIndex < cutList first
		ifTrue: [^mappedIndex].
	1 to: cutList size by: 2 do:
		[ :cutIndex |
		mappedIndex := mappedIndex - 
				((cutList at: cutIndex) - (cutList at: cutIndex + 1)).
		(((cutIndex + 1) = cutList size) or: 
				[mappedIndex < (cutList at: cutIndex + 2)])
			ifTrue: [^mappedIndex]].
		^mappedIndex - firstIndex.! !!GapSound methodsFor: 'testing'!hasGaps
	"Answer whether the receiver has any gaps."

	^true! !!GapSound methodsFor: 'accessing'!allocateData
	"no-op"

	^self! !!GapSound methodsFor: 'accessing'!cutFrom: startSample to: stopSample
	"Cut the designated section from the receiver."

	| realStart realStop |
	realStart := self mapSampleIndex: startSample.
	realStop := self mapSampleIndex: stopSample.
	cutList == nil
		ifTrue: [cutList := SortedCollection new].
	cutList add: realStart.
	cutList add: realStop.
	self size: (self size - (stopSample - startSample)).
	^self! !!GapSound methodsFor: 'accessing'!samples

	^source samples! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!GapSound class	instanceVariableNames: ''!!GapSound class methodsFor: 'instance creation'!on: source cutFrom: start to: stop
	"Answer a CompositeSound derived from the argument missing the given range."
	"GapSound exampleView"

	| me |
	me := self new initialize.
	me source: source.
	me samplesInMemory: source samplesInMemory.
	me data: source data.
	me cutFrom: start to: stop.
	^me! !!GapSound class methodsFor: 'examples'!exampleView
	"Open a sound view on a ramp with a chunk cut out of it."
	"GapSound exampleView"

	SoundView openOn: ((SampledSound rampDur: 1.0 rate: 16000 chans: 1) 
			cutFrom: 1000 to: 10000)! !