!classDefinition: #Sound category: 'Music-Sound'!
Function subclass: #Sound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!Sound commentStamp: '<historical>' prior: 0!
Instances of the subclasses of Sound are used to represent sound objects.
The abstract class Sound is vacuous.
Sounds use their Function and DurationEvent behaviors actively.
!


!Sound methodsFor: 'testing'!
isSound
	"Answer true."

	^true! !


!Sound methodsFor: 'initialize-release'!
finalize
	"Free the receiver's external data, if any"

	| fileNumber |
	fileNumber := self at: #fileIndex.
	fileNumber ifNotNil:
		[SoundFile releaseFileIndex: fileNumber]! !

!Sound methodsFor: 'initialize-release'!
initialize
	"Set up a default sound."

	self propCheck.
	range := -1.0 to: 1.0! !


!Sound methodsFor: 'cue accessing'!
addCueNamed: cName from: start to: stop
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
	list add: (cName asSymbol -> (start to: stop)).! !

!Sound methodsFor: 'cue accessing'!
cueList
	"Answer the receiver's 'cueList'."

	properties ifNil: [^nil].
	^properties at: #cueList ifAbsent: [nil]! !

!Sound methodsFor: 'cue accessing'!
cueList: CollectionOfCues
	"Set the receiver's 'cueList' to be the given OrderedCollection of (#name -> (start to: stop)) cues."

	^properties at: #cueList put: CollectionOfCues! !

!Sound methodsFor: 'cue accessing'!
cueNamed: cName
	"Answer a sound derived from the receiver using the samples between the named cue points."

	^self subclassResponsibility! !

!Sound methodsFor: 'cue accessing'!
cueRegionNamed: cName
	"Answer a indices for the samples between the named cue points."

	| q |
	self cueList == nil
		ifTrue: [self error: 'There is no cue by the name ', 
				cName printString, ' in this sound.'].
	q := self cueList detect: [ :c | c key == cName] ifNone: [nil].
	q == nil
		ifTrue: [self error: 'There is no cue by the name ', 
				cName printString, ' in this sound.'].
	^q value! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'Sound class' category: 'Music-Sound'!
Sound class
	instanceVariableNames: ''!

!Sound class methodsFor: 'instance accessing'!
named: aName
	"Answer the sound by the given name, or nil."

	^SirenSession sounds at: aName asSymbol ifAbsent: [nil]! !

!Sound class methodsFor: 'instance accessing'!
named: aName put: aSound
	"Put the given sound in the shared dictionary under the given name."

	SirenSession sounds ifNil: [^nil].
	^SirenSession sounds at: aName asSymbol put: aSound! !


!Sound class methodsFor: 'utilities'!
fromFile: nameString
	"Open the given file (AIFF, IRCAM, NeXT, or SPARC soundfile format)"
	"Sound fromFile: 'kombination.snd'"

	| snd |
	[snd := (SoundFile readFileNamed: nameString).
	snd ifNotNil: [snd := snd sound]]
		on: Core.Error
		do: [ :ex |  "ignore " "self error: 'Cannot read sound file named ', nameString"].
	^snd! !

!Sound class methodsFor: 'utilities'!
playFile: theName
	"Play the sound file with the given name"

	SirenUtility playSoundFile: theName! !


!classDefinition: #ComponentSound category: 'Music-Sound'!
Sound subclass: #ComponentSound
	instanceVariableNames: 'start stop sound'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!ComponentSound commentStamp: '<historical>' prior: 0!
A ComponentSound is used as a component (a "splice" element, if you will).
It represents another sound that "composes" the composite.

Instance Variables:
	sound <StoredSound> the "subject" sound 
	start <Integer> the starting sample in the composite
	stop <Integer> the ending sample in the composite
	offset <Number> the offset into the selection!


!ComponentSound methodsFor: 'accessing'!
offset
	"Answer the receiver's 'offset'."

	^offset! !

!ComponentSound methodsFor: 'accessing'!
offset: anObject
	"Set the receiver's 'offset' to be anObject."

	offset := anObject! !

!ComponentSound methodsFor: 'accessing'!
sound
	"Answer the receiver's 'sound'."

	^sound! !

!ComponentSound methodsFor: 'accessing'!
sound: anObject
	"Set the receiver's 'sound' to be anObject."

	sound := anObject! !

!ComponentSound methodsFor: 'accessing'!
start
	"Answer the receiver's 'start'."

	^start! !

!ComponentSound methodsFor: 'accessing'!
start: anObject
	"Set the receiver's 'start' to be anObject."

	start := anObject! !

!ComponentSound methodsFor: 'accessing'!
stop
	"Answer the receiver's 'stop'."

	^stop! !

!ComponentSound methodsFor: 'accessing'!
stop: anObject
	"Set the receiver's 'stop' to be anObject."

	stop := anObject.
	((start ~~ nil) & (sound ~~ nil))
		ifTrue: [self duration: (stop - start / sound rate) asFloat]! !


!ComponentSound methodsFor: 'printing'!
printOn: aStream
	"Pretty-print the receiver on the argument."

	| ra |
	sound ifNil: [^super printOn: aStream].
	ra := sound rate asFloat.
	aStream nextPutAll: '(ComponentSound on ', sound name, 
			' from ', ((start asFloat / ra) printWidth: 6), 
			' to ', ((stop asFloat / ra) printWidth: 6).
	offset ifNotNil: 
		[aStream nextPutAll: ' starting at ', offset printString].
	aStream nextPutAll: ')'; cr.! !


!ComponentSound methodsFor: 'testing'!
includes: anIndex
	"Answer whether the argument is within the range of the receiver."

	^(anIndex >= start) & (anIndex <= stop)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ComponentSound class' category: 'Music-Sound'!
ComponentSound class
	instanceVariableNames: ''!

!ComponentSound class methodsFor: 'instance creation'!
on: sound from: start to: stop
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop! !

!ComponentSound class methodsFor: 'instance creation'!
on: sound start: start stop: stop
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop! !

!ComponentSound class methodsFor: 'instance creation'!
on: sound start: start stop: stop offset: offset
	"Answer a new instance initialized with the arguments."

	^(self new) sound: sound; start: start; stop: stop; offset: offset! !


!classDefinition: #SampledSound category: 'Music-Sound'!
Sound subclass: #SampledSound
	instanceVariableNames: 'name rate format channels samplesInMemory firstIndex'
	classVariableNames: 'MagicNumbers MaxIntSample MinIntSample'
	poolDictionaries: ''
	category: 'Music-Sound'!
!SampledSound commentStamp: '<historical>' prior: 0!
Instances of SampledSound represent digitally-sampled sound objects.  Their "samples" are stored in the "data" instance variable inherited from Function. They can be read/written to/from files (using SoundFile objects), displayed (in SoundViews) and played (via SoundPorts).

The class SampledSound is concrete and assumes 16-bit linear encoding of samples; there are subclasses for floating-point and 8-bit Mu-law sample types. There are useful class methods for creating a number of standard sounds such as silence, impulses, swept sine waves, etc. Note that SampledSound uses the 'range' instance variable inherited from Function as its (integer) size.

Instance variables:
	name <String> the file name or object ID
	rate <Number> the sample rate
	format <Symbol> the sample format, e.g., #lin16Bit
	channels <Integer> the # of channels
	samplesInMemory	<Integer> The number of samples held in memory (may be smaller than 
			size for very large sounds)
	firstIndex	<Integer> The sample index of the start of the in-memory samples  (may be non-zero 
			for very large "paged" sounds)

Other properties, such as the sound's (optional) file name, its sample rate, or the number of channels, are stored in the property list dictionary that is inherited by virtue of being an event subclass.

KNOWN BUGS:

Note that not all of this class is finished--there are methods that call non-existent user primitives and have no Smalltalk implementations (like the heavy DSP).

!


!SampledSound methodsFor: 'accessing'!
allocateData

	| samps |
	samps := self size * channels.
"	((#lin16Bit -> [data := WordArray new: samps withAll: 0]),
"	((#lin16Bit -> [data := UninterpretedBytes new: samps * 4]),
	(#float32Bit -> [data := UninterpretedBytes new: samps * 4.
			0 to: samps - 1 do: [ :i | data floatAt: (i * 4 + 1) put: 0.0]]))
		case: format
		otherwise: [self error: 'Unknown sound format.'].
	samplesInMemory := samps! !

!SampledSound methodsFor: 'accessing'!
channels
	"Answer the receiver's 'channels'."

	^channels! !

!SampledSound methodsFor: 'accessing'!
channels: anObject
	"Set the receiver's instance variable 'channels' to be anObject."

	channels := anObject! !

!SampledSound methodsFor: 'accessing' stamp: 'di 12/7/2000 16:04'!
compressWith: codecClass
	^ codecClass new compressSound: self! !

!SampledSound methodsFor: 'accessing' stamp: 'RAA 12/24/2000 08:49'!
compressWith: codecClass atRate: aSamplingRate

	^ codecClass new compressSound: self atRate: aSamplingRate! !

!SampledSound methodsFor: 'accessing'!
copy
	"Answer a 'clean' copy of the receiver."

	| newMe |
	newMe := SampledSound named: self name
			size: self size format: self format.
	self properties associationsDo:
		[ :ass |
		newMe at: (ass key) put: (ass value copy)].
	1 to: self size do:
		[ :ind |
		newMe sampleAt: ind put: (self sampleAt: ind)].
	^newMe! !

!SampledSound methodsFor: 'accessing'!
copyAllButSamples
	"Answer a 'clean' copy of the receiver, but don't write in the samples yet."

	| newMe |
	newMe := SampledSound named: self name
			size: self size 
			rate: self rate
			channels: self channels 
			format: self format.
	self properties associationsDo:
		[ :ass |
		newMe at: (ass key) put: (ass value copy)].
	^newMe! !

!SampledSound methodsFor: 'accessing'!
copyFrom: start to: stop
	"Answer a copy of the receiver for the given sample range."
	"((StoredSound sweepDur: 1.0 rate: 44100 from: 50 to: 5000 chans: 1)
				copyFrom: 10000 to: 10512) edit"

	| newMe |
	newMe := self copyAllButSamples.
	newMe size: stop - start + 1.
	newMe allocateData.
	start to: stop - 1 do:
		[ :ind |
		newMe sampleAt: ind - start + 1 put: (self sampleAt: ind)].
	^newMe! !

!SampledSound methodsFor: 'accessing'!
cueNamed: cName
	"Answer a sound derived from the receiver using the samples between the named cue points."

	| snd start stop region |
	region := self cueRegionNamed: cName.
	start := region first.
	stop := region last.
	snd := self copyAllButSamples.
	snd size: (stop - start).
	snd samples: (ByteArray new: (stop - start * self sampleSize)).
	start to: stop do:
		[ :ind |
		snd sampleAt: (ind - start + 1) put: (self sampleAt: ind)].
	^snd.! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 3/28/98 05:46'!
duration

	^ initialCount asFloat / self samplingRate asFloat
! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 9/11/1998 15:39'!
duration: seconds

	super duration: seconds.
	count _ initialCount _ (seconds * self samplingRate) rounded.
! !

!SampledSound methodsFor: 'accessing'!
envelope
	"Answer the receiver's envelope."

	properties ifNil: [^nil].
	^properties at: #envelope ifAbsent: [nil]! !

!SampledSound methodsFor: 'accessing'!
envelope: anEnv
	"Set the receiver's envelope property."

	properties at: #envelope put: anEnv! !

!SampledSound methodsFor: 'accessing'!
file
	"Answer the sound file for the receiver (if present)."

	| file |
	self hasFile
		ifTrue: [file := SoundFile named: self name.
			file sound: self.
			^file]
		ifFalse: [^nil]! !

!SampledSound methodsFor: 'accessing'!
firstIndex
	"Answer the receiver's 'firstIndex'."

	^firstIndex! !

!SampledSound methodsFor: 'accessing'!
firstIndex: anObject
	"Set the receiver's instance variable 'firstIndex' to be anObject."

	firstIndex := anObject! !

!SampledSound methodsFor: 'accessing'!
floatsFrom: aStart to: aStop
	"Answer a copy of the receiver's data within the given sample range."

	| samps |
	samps := ByteArray new: (aStop - aStart) * 4.
	aStart to: aStop - 1 do:
		[ :i | samps floatAt: (i - aStart + 1) put: (self floatSampleAt: i)].
	^samps! !

!SampledSound methodsFor: 'accessing'!
format
	"Answer the receiver's 'format'."

	^format! !

!SampledSound methodsFor: 'accessing'!
format: fSymbol
	"Set the receiver's instance variable 'format' to be fSymbol."

	format := fSymbol.
	range := ((#lin16Bit -> [-32768 to: 32767]), (#float32Bit -> [-1.0 to: 1.0])) 
		case: fSymbol otherwise: [-1.0 to: 1.0]! !

!SampledSound methodsFor: 'accessing'!
frameRate
	"Answer the receiver's 'rate'."

	^rate! !

!SampledSound methodsFor: 'accessing'!
from: aStart to: aStop
	"Answer a copy of the receiver within the given time range."

	| sound |
	sound := SampledSound named: self name
			size: ((aStart - aStop) asSec * self rate) asInteger
			format: self format.
	((aStart * 2 - 1) max: 1) to: (aStop * 2) by: 2 do:
		[ :i | sound sampleAt: i put: (self sampleAt: i)].
	^sound! !

!SampledSound methodsFor: 'accessing'!
fromSample: start toSample: finish
	"Answer a copy of the receiver within the given sample range."

	| ssize sdata |
	((start == nil) or: [finish == nil])
		ifTrue: [^SampledSound new].
	ssize := finish - start.
	sdata := Array new: ssize.
	1 to: ssize do:
		[ :ind |
		sdata at: ind put: (self sampleAt: ind + start)].
	^SampledSound fromData: sdata
		rate: self rate
		channels: self channels
		format: self format! !

!SampledSound methodsFor: 'accessing'!
hasFile
	"Answer whether the receiver is stored on a file of the same name."

	self name isEmpty ifTrue: [^false].
	^self name asFilename exists! !

!SampledSound methodsFor: 'accessing'!
longFormat
	"Answer the receiver's format as a descriptive string."

	^(	(#lin16Bit -> ['16-bit linear ints']),
		(#float32Bit -> ['32-bit linear floats']),
		(#Mu8Bit -> ['8-bit Mu-law']))
	case: (properties at: #format ifAbsent: [nil])
	otherwise: ['**unknown**']! !

!SampledSound methodsFor: 'accessing'!
name
	"Answer the receiver's 'name'."

	^name! !

!SampledSound methodsFor: 'accessing'!
name: anObject
	"Set the receiver's instance variable 'name' to be anObject."

	name := anObject! !

!SampledSound methodsFor: 'accessing' stamp: 'jm 12/15/97 22:51'!
originalSamplingRate

	^ originalSamplingRate
! !

!SampledSound methodsFor: 'accessing'!
rate
	"Answer the receiver's 'rate'."

	^rate! !

!SampledSound methodsFor: 'accessing'!
rate: anObject
	"Set the receiver's instance variable 'rate' to be anObject."

	rate := anObject! !

!SampledSound methodsFor: 'accessing'!
sampleRate
	"Answer the receiver's 'rate'."

	^rate! !

!SampledSound methodsFor: 'accessing'!
sampleSize
	"Answer the size in bytes of the receiver's samples (2)."

	^2! !

!SampledSound methodsFor: 'accessing'!
samples
	"Answer the instance variable 'data'."

	data ifNil: [self getData].
	^data! !

!SampledSound methodsFor: 'accessing'!
samples: theArray
	"Accept the argument, 'theArray', (a Byte, Word, or Float array, or an UninterpretedBytes)
	as the new instance variable 'samples'."

	data := theArray.
	samplesInMemory := theArray size "/ self sampleSize" .
	self size: data size.! !

!SampledSound methodsFor: 'accessing'!
samplesInMemory
	"Answer the receiver's 'samplesInMemory'."

	^samplesInMemory! !

!SampledSound methodsFor: 'accessing'!
samplesInMemory: anObject
	"Set the receiver's instance variable 'samplesInMemory' to be anObject."

	samplesInMemory := anObject! !

!SampledSound methodsFor: 'accessing'!
size
	"Return the receiver's size in sample frames."

	domain ifNil: [^0].
	domain isInteger ifTrue: [^domain].
	^domain size! !

!SampledSound methodsFor: 'accessing'!
size: aNumber
	"Set the receiver's 'size' in samples."

	domain := 1 to: aNumber.
	self duration: (Duration value: (aNumber asFloat / self rate)).
	data ifNil: [self allocateData]! !

!SampledSound methodsFor: 'accessing'!
sizeInBytes
	"Answer the size in bytes of the receiver."

	^self size * self sampleSize! !

!SampledSound methodsFor: 'accessing'!
sizeInFrames
	"Return the receiver's size in sample frames."

	domain ifNil: [^0].
	domain isInteger ifTrue: [^domain].
	^domain size! !


!SampledSound methodsFor: 'playing' stamp: 'di 10/28/2000 17:08'!
endGracefully
	"See stopGracefully, which affects initialCOunt, and I don't think it should (di)."

	| decayInMs env |
	envelopes isEmpty
		ifTrue: [
			self adjustVolumeTo: 0 overMSecs: 10.
			decayInMs _ 10]
		ifFalse: [
			env _ envelopes first.
			decayInMs _ env attackTime + env decayTime].
	count _ decayInMs * self samplingRate // 1000.
! !

!SampledSound methodsFor: 'playing' stamp: 'ar 2/3/2001 15:23'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex leftVol: leftVol rightVol: rightVol
	"Mix the given number of samples with the samples already in the given buffer starting at the given index. Assume that the buffer size is at least (index + count) - 1."

	| lastIndex outIndex sampleIndex sample i s overflow |
	<primitive:'primitiveMixSampledSound' module:'SoundGenerationPlugin'>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #samples declareC: 'short int *samples'.

	lastIndex _ (startIndex + n) - 1.
	outIndex _ startIndex.    "index of next stereo output sample pair"
	sampleIndex _ indexHighBits + (scaledIndex >> IncrementFractionBits).
	[(sampleIndex <= samplesSize) and: [outIndex <= lastIndex]] whileTrue: [
		sample _ ((samples at: sampleIndex) * scaledVol) // ScaleFactor.
		leftVol > 0 ifTrue: [
			i _ (2 * outIndex) - 1.
			s _ (aSoundBuffer at: i) + ((sample * leftVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].
		rightVol > 0 ifTrue: [
			i _ 2 * outIndex.
			s _ (aSoundBuffer at: i) + ((sample * rightVol) // ScaleFactor).
			s >  32767 ifTrue: [s _  32767].  "clipping!!"
			s < -32767 ifTrue: [s _ -32767].  "clipping!!"
			aSoundBuffer at: i put: s].

		scaledVolIncr ~= 0 ifTrue: [
			scaledVol _ scaledVol + scaledVolIncr.
			((scaledVolIncr > 0 and: [scaledVol >= scaledVolLimit]) or:
			 [scaledVolIncr < 0 and: [scaledVol <= scaledVolLimit]])
				ifTrue: [  "reached the limit; stop incrementing"
					scaledVol _ scaledVolLimit.
					scaledVolIncr _ 0]].

		scaledIndex _ scaledIndex + scaledIncrement.
		scaledIndex >= ScaledIndexOverflow ifTrue: [
			overflow _ scaledIndex >> IncrementFractionBits.
			indexHighBits _ indexHighBits + overflow.
			scaledIndex _ scaledIndex - (overflow << IncrementFractionBits)].

		sampleIndex _ indexHighBits + (scaledIndex >> IncrementFractionBits).
		outIndex _ outIndex + 1].
	count _ count - n.
! !

!SampledSound methodsFor: 'playing' stamp: 'di 10/28/2000 22:31'!
playSilentlyUntil: startTime
	"Used to fast foward to a particular starting time.
	Overridden to be instant for sampled sounds."

"true ifTrue: [^ super playSilentlyUntil: startTime]."
	indexHighBits _ (startTime * originalSamplingRate) asInteger.
	scaledIndex _ IncrementScaleFactor.
	count _ initialCount - (startTime * self samplingRate).
	mSecsSinceStart _ (startTime * 1000) asInteger.

! !

!SampledSound methodsFor: 'playing' stamp: 'jm 7/9/1999 18:29'!
reset
	"Details: The sample index and increment are scaled to allow fractional increments without having to do floating point arithmetic in the inner loop."

	super reset.
	scaledIncrement _
		((originalSamplingRate asFloat / self samplingRate) * IncrementScaleFactor) rounded.
	count _ initialCount.
	scaledIndex _ IncrementScaleFactor.  "index of the first sample, scaled"
	indexHighBits _ 0.
! !

!SampledSound methodsFor: 'playing' stamp: 'jm 9/13/97 19:07'!
samplesRemaining

	^ count
! !

!SampledSound methodsFor: 'playing' stamp: 'RAA 8/12/2000 15:11'!
setScaledIncrement: aNumber

	scaledIncrement _ (aNumber * IncrementScaleFactor) rounded.

! !

!SampledSound methodsFor: 'playing' stamp: 'jm 9/9/1998 21:58'!
stopAfterMSecs: mSecs
	"Terminate this sound this note after the given number of milliseconds."

	count _ (mSecs * self samplingRate) // 1000.
! !


!SampledSound methodsFor: 'initialization' stamp: 'jm 1/18/1999 06:42'!
pitch: pitchNameOrNumber

	| p |
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	originalSamplingRate _
		((self samplingRate asFloat * p asFloat) / NominalSamplePitch asFloat) asInteger.
	self reset.
! !

!SampledSound methodsFor: 'initialization' stamp: 'jm 3/24/1999 12:01'!
setPitch: pitchNameOrNumber dur: d loudness: vol
	"Used to play scores using the default sample table."
	"(SampledSound pitch: 880.0 dur: 1.5 loudness: 0.6) play"

	| p |
	super setPitch: pitchNameOrNumber dur: d loudness: vol.
	p _ self nameOrNumberToPitch: pitchNameOrNumber.
	samples _ DefaultSampleTable.
	samplesSize _ samples size.
	initialCount _ (d * self samplingRate asFloat) rounded.
	originalSamplingRate _
		((self samplingRate asFloat * p asFloat) / NominalSamplePitch asFloat) asInteger.
	self loudness: vol.
	self reset.
! !

!SampledSound methodsFor: 'initialization' stamp: 'jm 7/9/1999 19:23'!
setSamples: anArray samplingRate: rate
	"Set my samples array to the given array with the given nominal sampling rate. Altering the rate parameter allows the sampled sound to be played back at different pitches."
	"Note: There are two ways to use sampled sound: (a) you can play them through once (supported by this method) or (b) you can make them the default waveform with which to play a musical score (supported by the class method defaultSampleTable:)."
	"Assume: anArray is either a SoundBuffer or a collection of signed 16-bit sample values."
	"(SampledSound
		samples: SampledSound coffeeCupClink
		samplingRate: 5000) play"

	"copy the array into a SoundBuffer if necessary"
	anArray class isWords
		ifTrue: [samples _ anArray]
		ifFalse: [samples _ SoundBuffer fromArray: anArray].

	samplesSize _ samples size.
	samplesSize >= SmallInteger maxVal ifTrue: [  "this is unlikely..."
		self error: 'sample count must be under ',  SmallInteger maxVal printString].
	originalSamplingRate _ rate.
	initialCount _ (samplesSize * self samplingRate) // originalSamplingRate.
	self loudness: 1.0.
	self reset.
! !


!SampledSound methodsFor: 'file i/o' stamp: 'jmv 2/26/2016 16:05'!
storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream
	"Store my samples on the given stream at the current SoundPlayer sampling rate. If bigFlag is true, then each 16-bit sample is stored most-significant byte first (AIFF files), otherwise it is stored least-significant byte first (WAV files)."

	self samplingRate ~= originalSamplingRate ifTrue: [
		^ super storeSampleCount: samplesToStore bigEndian: bigEndianFlag on: aBinaryStream].

	"optimization: if sampling rates match, just store my buffer"
	(aBinaryStream isKindOf: StandardFileStream)
		ifTrue: [  "optimization for files: write sound buffer directly to file"
			aBinaryStream next: (samples size // 2) putAll: samples startingAt: 1]  "size in words"
		ifFalse: [  "for non-file streams:"
			1 to: samples monoSampleCount do: [:i | aBinaryStream nextSignedInt16Put: (samples at: i) bigEndian: bigEndianFlag ]]! !


!SampledSound methodsFor: 'sound tracks' stamp: 'jmv 12/4/2012 23:42'!
volumeForm: height from: start to: stop nSamplesPerPixel: nPerPixel
	"Note: nPerPixel can be Integer or Float for pixel-perfect alignment."
	"In an inspector of a samplesSound...
		self currentWorld addMorph: (ImageMorph new image:
			(self volumeForm: 32 from: 1 to: samples size nSamplesPerPixel: 225))
	"
	| volPlot width sample min max vol |
	width _ stop-start//nPerPixel.
	volPlot _ Form extent: width@height.
	(start max: 1) to: (stop min: samples size)-nPerPixel by: nPerPixel do:
		[:i | min _ max _ 0.
		i asInteger to: (i+nPerPixel-1) asInteger by: 4 do:  "by: 4 makes it faster yet looks the same"
			[:j | sample _ samples at: j.
			sample < min ifTrue: [min _ sample].
			sample > max ifTrue: [max _ sample]].
		vol _ (max - min) * height // 65536.
		volPlot fillBlack: ((i-start//nPerPixel) @ (height-vol//2) extent: 1@(vol+1))].
	^ volPlot
	
! !


!SampledSound methodsFor: 'sample accessing'!
at: anIndex 
	"Answer the value at the given index (between 0 and 1)."

	anIndex isSymbol
		ifTrue: [^ super at: anIndex].
	^ self sampleAt: anIndex					"ignore scale/offset here"! !

!SampledSound methodsFor: 'sample accessing'!
at: anIndex put: aValue
	"Stuff the given sample at the given index (after lots of checking and testing)."

	| newIndex |
	anIndex isSymbol
		ifTrue: [^ super at: anIndex put: aValue].
	newIndex := self mapSampleIndex: anIndex.
	newIndex <= self size
		ifFalse: [self error: 'Attempt to store past the end of a sound.'].
	(newIndex <= samplesInMemory)
		ifFalse: ["Transcript show: 'Reading next page of sample file.'; cr."
				self realSound readSamplesFrom: newIndex + firstIndex.
				^self sampleAt: newIndex put: aValue].
	^self privateSampleAt: anIndex put: aValue! !

!SampledSound methodsFor: 'sample accessing'!
cutFrom: startSample to: stopSample
	"Cut the designated section from the receiver; answer a new sound (!!)."

	^GapSound on: self cutFrom: startSample to: stopSample! !

!SampledSound methodsFor: 'sample accessing'!
floatSampleAt: ind
	"Answer the sample at the given index as a normalized floating-point number."

	^(self intSampleAt: ind) asFloat / self class maxSample! !

!SampledSound methodsFor: 'sample accessing'!
floatSampleAt: ind put: aValue
	"Stuff the given floating-point sample at the given index after scaling."

	self intSampleAt: ind put: (aValue * self class maxSample) truncated! !

!SampledSound methodsFor: 'sample accessing'!
intSampleAt: ind
	"Answer the sample at the given index as an integer."

	^self sampleAt: ind! !

!SampledSound methodsFor: 'sample accessing'!
intSampleAt: ind put: aValue
	"Stuff the given integer sample at the given index."

	self sampleAt: ind put: aValue! !

!SampledSound methodsFor: 'sample accessing'!
paste: aSound from: startSample to: stopSample at: ind
	"Paste the given sound into the receiver; answer a new sound (!!)."

	^CompositeSound on: self paste: aSound from: startSample to: stopSample at: ind! !

!SampledSound methodsFor: 'sample accessing'!
sampleAt: ind
	"Answer the sample at the given index, mapping and paging as necessary."

	| newIndex |
	self samples == nil
			ifTrue: [^self error: 'There are no samples for this sound.'].
	newIndex := self mapSampleIndex: ind.
	newIndex > domain last		"answer 0 after end of samples."
		ifTrue: [^0].
	newIndex < domain first		"or before start of samples."
		ifTrue: [^0].
	newIndex <= samplesInMemory
		ifTrue: [^self privateSampleAt: newIndex]
		ifFalse: ["Transcript show: 'Reading next page of sample file.'; cr."
				self realSound readSamplesFrom: newIndex + firstIndex.
				^self sampleAt: newIndex].
	self error: 'Indexing off the end of a sound.'! !

!SampledSound methodsFor: 'sample accessing'!
sampleAt: anIndex put: aValue
	"Stuff the given sample at the given index (after lots of checking and testing)."

	| newIndex |
	newIndex := self mapSampleIndex: anIndex.
	newIndex <= self size
		ifFalse: [self error: 'Attempt to store past the end of a sound.'].
	(newIndex <= samplesInMemory)
		ifFalse: ["Transcript show: 'Reading next page of sample file.'; cr."
				self realSound readSamplesFrom: newIndex + firstIndex.
				^self sampleAt: newIndex put: aValue].
	^self privateSampleAt: anIndex put: aValue! !


!SampledSound methodsFor: 'interpolating'!
autoCorrelationBetween: index1 and: index2 length: length
	"Answer the cummulative error between the portions of my waveform starting at the given two indices and extending for the given length. The larger this error, the greater the difference between the two waveforms."

	| error i1 e |
	error := 0.
	i1 := index1.
	index2 to: (index2 + length - 1) do: [:i2 |
		e := (self sampleAt: i1) - (self sampleAt: i2).
		e < 0 ifTrue: [e := 0 - e].
		error := error + e.
		i1 := i1 + 1].
	^ error! !

!SampledSound methodsFor: 'interpolating'!
errorBetween: sampleArray1 and: sampleArray2
	"Answer the cummulative error between the two sample arrays, which are assumed to be the same size."

	| error e |
	error := 0.
	1 to: sampleArray1 size do: [:i |
		e := (sampleArray1 at: i) - (sampleArray2 at: i).
		e < 0 ifTrue: [e := 0 - e].
		error := error + e].
	^ error! !

!SampledSound methodsFor: 'interpolating'!
fadeInOver: fTime
	"Apply a fade-in ramp to the receiver with the given time."

	| fDur |
	fTime > self duration asSec value
		ifTrue: [Transcript show: 'Illegal fade-in time'; cr. ^nil].
	fDur := fTime * self rate * self channels.
	1 to: fDur do:
		[ :ind |
		self sampleAt: ind
			put: (self sampleAt: ind) * (ind / fDur)]! !

!SampledSound methodsFor: 'interpolating'!
fadeOutOver: fTime
	"Apply a fade-in ramp to the receiver with the given time."

	| fDur s0 |
	fTime > self duration asSec value
		ifTrue: [Transcript show: 'Illegal fade-in time'; cr. ^nil].
	fDur := fTime * self rate * self channels.
	s0 := domain - fDur.
	s0 to: domain do:
		[ :ind |
		self sampleAt: ind
			put: (self sampleAt: ind) * (1.0 - (ind - s0 / fDur))]! !

!SampledSound methodsFor: 'interpolating'!
interpolatedWindowAt: anIndex width: nSamples
	"Return an array of N samples starting at the given index in my data."

	| scale baseIndex scaledFrac scaledOneMinusFrac prevSample nextSample v |
	scale := 10000.
	anIndex isInteger
		ifTrue: [^ (anIndex to: anIndex + nSamples - 1) collect: [ :i | self sampleAt: i]].
	baseIndex := anIndex truncated.
	scaledFrac := ((anIndex asFloat - baseIndex) * scale) truncated.
	scaledOneMinusFrac := scale - scaledFrac.
	prevSample := data at: baseIndex.
	^ (baseIndex + 1 to: baseIndex + nSamples) collect: [:i |
		nextSample := self sampleAt: i.
		v := ((nextSample * scaledFrac) + (prevSample * scaledOneMinusFrac)) // scale.
		prevSample := nextSample.
		v].! !

!SampledSound methodsFor: 'interpolating'!
sampleMaxFrom: aStart to: aStop into: anArray
	"Place a version of the root-mean-square energy of the receiver in the argument array."
	
"	| arr snd |
	arr := Array new: 64 withAll: 0.0.
	snd := StoredSound fileExample.
	snd sampleMaxFrom: 1 to: snd size into: arr.
	(Function from: arr) edit."

"	<primitive: 17082>
"
	| factor max |
	anArray size > self size
		ifTrue: [^self error: 'Cannot take and rms into a larger array...'].
	factor := ((aStop - aStart) / (anArray size)) asFloat.
	0 to: anArray size - 1 do:
		[ :i | 
		max := 0.
		(i * factor) asInteger to: (i + 1 * factor) asInteger do:
			[ :j |
			max := max max: (self sampleAt: (aStart + j))].
		anArray at: i + 1 put: (max asFloat / 32767.0) asFloat].! !

!SampledSound methodsFor: 'interpolating'!
sampledFrom: aStart to: aStop into: anArray
	"Place a down-sampled version of the receiver in the argument array."

	<primitive: 17081> "ca n'existe pas"! !

!SampledSound methodsFor: 'interpolating'!
sampledTo: anArray
	"Place a down-sampled version of the receiver in the argument array."

	^self sampledFrom: 1 to: self size into: anArray! !


!SampledSound methodsFor: 'merging'!
extractChannel: cNumber into: aSound
	"Extract the given channel from the receiver into the argument sound."

	<primitive: 17021>! !

!SampledSound methodsFor: 'merging'!
mergeChannel: cNumber from: aSound
	"Merge the argument sound into the given channel of the receiver."

	<primitive: 17022>! !


!SampledSound methodsFor: 'envelopes'!
average
	"Answer the average sample value of the receiver (normally the DC offset)."

	| value min max samp |
	min := max := self sampleAt: 1.
	value := 0.
	1 to: self size do:
		[ :i |
		samp := self sampleAt: i.
		value := value + samp.
		(samp > max) ifTrue: [max := samp].
		(samp < min) ifTrue: [min := samp]].
	self domain: (min to: max).
	value := (value / self size) asFloat.
	self at: #average put: value.
	^value! !

!SampledSound methodsFor: 'envelopes'!
computeEnvelope: type
	"Compute a sample envelope for the receiver with 128 points per second."

	| eSize env fcn |
	self size = 0 ifTrue: [^self].
	Cursor execute showWhile:
		[eSize := (self duration value truncated max: 1) * 128.
"		Transcript show: 'Creating envelope of size: ', eSize printString; cr."
		env := (Array new: eSize).
		type == #peak
			ifTrue: [self peaksFrom: 1 to: self size into: env]
			ifFalse: [self rmsFrom: 1 to: self size into: env]].
	fcn := Function from: env.
	self envelope: fcn.
	self hasFile
		ifTrue: [Cursor write showWhile: [self writeEnvelope: type]].! !

!SampledSound methodsFor: 'envelopes'!
computePeakEnvelope
	"Compute a windowed peak sample envelope for the receiver with 1024 points per 4 seconds."

	self computeEnvelope: #peak! !

!SampledSound methodsFor: 'envelopes'!
computeRMSEnvelope
	"Compute a windowed root-mean-square sample envelope for the receiver."

	self computeEnvelope: #RMS! !

!SampledSound methodsFor: 'envelopes'!
edit
	"Edit the receiver."

	SoundView openOn: self! !

!SampledSound methodsFor: 'envelopes'!
max
	"Answer the maximum value of the samples."

	| max t |
	max := 0.
	1 to: self size do:
		[ :i | 
		t := self sampleAt: i.
		(t abs > max) ifTrue: [max := t abs]].
	^max! !

!SampledSound methodsFor: 'envelopes'!
offsetBy: aFactor
	"Answer a copy of the receiver offset by the given factor."

	^self offsetFrom: 1 to: self size by: aFactor! !

!SampledSound methodsFor: 'envelopes'!
offsetFrom: aStart to: aStop by: anOffset
	"Answer a copy of the receiver offset by the given factor."

	| newSound |
	newSound := self copyAllButSamples.
	1 to: self size do:
		[ :i |
		newSound sampleAt: i put: ((self sampleAt: i) + anOffset)].
	^newSound! !

!SampledSound methodsFor: 'envelopes'!
peakEnvelope
	"Answer the receiver's peak-detected envelope, or compute a new one."

	| env |
	env := self envelope.
	(env ~~ nil and: [env size > 0])
		ifTrue: [^env].
	(self readEnvelope: #peak)
		ifFalse: [self computePeakEnvelope].
	^self envelope! !

!SampledSound methodsFor: 'envelopes'!
peaksFrom: aStart to: aStop into: anArray
	"Place a version of the root-mean-square energy of the receiver in the argument array."
	
"	| arr snd |
	arr := Array new: 1024.
	snd := 'Nomad:Squeak:Siren:sounds:kombination.1.aiff' asSoundFile.
	TimeProfileBrowser onBlock: [10 timesRepeat: 
		[snd rmsFrom: 1 to: snd size into: arr]].
	(Function from: arr) edit."

"	<primitive: 17082>
"
	| aSize factor max maxPos peak sample |
	aSize := anArray size.
	aSize > self size
		ifTrue: [^self error: 'Cannot take and rms into a larger array...'].
	factor := ((aStop - aStart) asFloat / (aSize)).
	max := 0.
	aStart = 0
		ifTrue: [0 to: aSize - 1 do:
				[ :i | 
				peak := 0.
				(i * factor) truncated to: (i + 1 * factor) truncated do:
					[ :j |
					sample := (self sampleAt: j) abs.
					sample > max
						ifTrue: [max := sample.
							maxPos := j].
					sample > peak
						ifTrue: [peak := sample]].
				anArray at: i + 1 put: peak truncated]]
		ifFalse: [0 to: aSize - 1 do:
				[ :i |
				peak := 0.0.
				(i * factor) truncated to: (i + 1 * factor) truncated do:
					[ :j |
					sample := (self sampleAt: (aStart + j)) abs.
					sample > max
						ifTrue: [max := sample.
							maxPos := aStart + j].
					sample > peak
						ifTrue: [peak := sample]].
				anArray at: i + 1 put: peak truncated]].
	self at: #maxSample put: max.
	self at: #maxSamplePos put: maxPos! !

!SampledSound methodsFor: 'envelopes'!
readEnvelope: type
	"Try to read the sample envelope for the receiver from a file named XXX.env."

	| sfName ext ind eName eStream eSize env |
	sfName := self name.
	sfName isEmpty ifTrue: [^false].
	ext := (type == #peak ifTrue: ['pk'] ifFalse: ['env']).
	^self hasFile
		ifTrue: [ind := sfName indexOfSubCollection: '.snd' startingAt: 1.
			eName := (sfName copyFrom: 1 to: ind), ext.
			eName asFilename exists
				ifTrue: [eStream := eName asFilename readStream binary.
					eSize := eName asFilename fileSize / 2.
"					Transcript show: 'Reading env ', eName, 
							' size ', eSize printString; cr."
					env := Array new: eSize.
					1 to: eSize do:
						[ :i | env at: i put: eStream nextWord].
					eStream close.
					self envelope: env.
					true]
			ifFalse: ["Transcript show: 'No envelope file found ', 
						(sfName copyFrom: 1 to: ind), ext; cr. "
					false]]
		ifFalse: [self envelope ~~ nil]! !

!SampledSound methodsFor: 'envelopes'!
rmsEnvelope
	"Answer the receiver's envelope, or compute a new one."

	| env |
	env := properties at: #envelope ifAbsent: [nil].
	(env ~~ nil and: [env size > 0])
		ifTrue: [^env].
	(self readEnvelope: #RMS)
		ifFalse: [self computeRMSEnvelope].
	^self envelope! !

!SampledSound methodsFor: 'envelopes'!
rmsFrom: aStart to: aStop into: anArray
	"Place a version of the root-mean-square energy of the receiver in the argument array."
	
"	| arr snd |
	arr := Array new: 512.
	snd := StoredSound fileExample.
	snd rmsFrom: 1 to: snd size into: arr.
	(Function from: arr) edit."

	| aSize factor max maxPos sum sample |
	aSize := anArray size.
	aSize > self size
		ifTrue: [^self error: 'Cannot take and rms into a larger array...'].
	factor := ((aStop - aStart) asFloat / (aSize)).
	max := -1.0e30.
	0 to: aSize - 1 do:
		[ :i | 
		sum := 0.0.
		(i * factor) truncated to: (i + 1 * factor) truncated do:
			[ :j |
			sample := self sampleAt: (aStart + j) abs.
			sample > max
				ifTrue: [max := sample.
						maxPos := aStart + j].
			sum := sum + (sample * sample)].
		anArray at: i + 1 put: (sum sqrt / factor) truncated].
	self at: #maxSample put: max.
	self at: #maxSamplePos put: maxPos! !

!SampledSound methodsFor: 'envelopes'!
rmsTo: anArray
	"Place a version of the root-mean-square energy of the receiver in the argument array."

	^self rmsFrom: 1 to: self size into: anArray! !

!SampledSound methodsFor: 'envelopes'!
scaleBy: aScaleFactor
	"Scale the receiver's samples by the given factor."

	^self scaleFrom: 1 to: self size by: aScaleFactor! !

!SampledSound methodsFor: 'envelopes'!
scaleFrom: aStart to: aStop by: aScaleFactor
	"Scale the receiver's damples by the given factor."

	1 to: self size do:
		[ :i |
		self sampleAt: i put: ((self sampleAt: i) * aScaleFactor)].
	^self! !

!SampledSound methodsFor: 'envelopes'!
scaledBy: aScaleFactor
	"Answer a copy of the receiver scaled by the given factor."

	^self scaledFrom: 1 to: self size by: aScaleFactor! !

!SampledSound methodsFor: 'envelopes'!
scaledByEnvelopeArray: anArray
	"Answer a copy of the receiver scaled by the given envelope array."

	^self scaledFrom: 1 to: self size byEnvelopeArray: anArray! !

!SampledSound methodsFor: 'envelopes'!
scaledByFunction: aFunction
	"Answer a copy of the receiver scaled by the given breakpoint envelope expressed as points."

	^self scaledFrom: 1 to: self size byFunction: aFunction! !

!SampledSound methodsFor: 'envelopes'!
scaledFrom: aStart to: aStop by: aScaleFactor
	"Answer a copy of the receiver scaled by the given factor."

"	<primitive: 17084>"
	| newSound |
	newSound := self copyAllButSamples.
	1 to: self size do:
		[ :i |
		newSound sampleAt: i put: ((self sampleAt: i) * aScaleFactor)].
	^newSound! !

!SampledSound methodsFor: 'envelopes'!
scaledFrom: aStart to: aStop byEnvelopeArray: anArray
	"Answer a copy of the receiver scaled by the given envelope array."

	<primitive: 17086>! !

!SampledSound methodsFor: 'envelopes'!
scaledFrom: aStart to: aStop byFunction: aFunction
	"Answer a copy of the receiver scaled by the given breakpoint envelope expressed as points."
	"SoundView openOn: (StoredSound sawtooth scaledByFunction: (LinearFunction from: #((0 0) (1 1))))"

"	<primitive: 17085>"
	| newSound ssize |
	newSound := self copyAllButSamples.
	ssize := 1.0 / self size.
	1 to: self size do:
		[ :i |
		newSound sampleAt: i put: ((self sampleAt: i) * (aFunction at: (i * ssize)))].
	^newSound! !

!SampledSound methodsFor: 'envelopes'!
writeEnvelope: type
	"Write the sample envelope for the receiver to a file named XXX.env or XXX.pk."

	| sfName ind eName env eStream eSize |
	sfName := self name.
	ind := sfName indexOfSubCollection: '.snd' startingAt: 1.
	type == #peak		"or #RMS"
		ifTrue: [eName := (sfName copyFrom: 1 to: ind), 'pk']
		ifFalse: [eName := (sfName copyFrom: 1 to: ind), 'env'].
	eName asFilename exists
		ifTrue: [(Dialog confirm: 
					'Do you really want to overwrite the envelope file ', eName)
			ifFalse: [^self]].
	eStream := eName asFilename writeStream binary.
	env := self envelope.
	eSize := env size.
"	Transcript show: 'Writing envelope file ', eName, 
						' size ', eSize printString; cr."
	1 to: eSize do:
		[ :i | 
		 eStream nextWordPut: (env at: i)].
	eStream commit; close! !


!SampledSound methodsFor: 'enumerating'!
do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self samples do: aBlock! !


!SampledSound methodsFor: 'printing'!
printOn: aStream
	"pretty-print the receiver."

	aStream nextPutAll: '(', self class name, ' '; cr.
	name == nil
		ifFalse: [aStream tab; nextPutAll: 'named: ', name printString; cr].
	duration == nil
		ifFalse: [aStream tab; nextPutAll: 'duration: ', duration printString; cr].
	rate == nil
		ifFalse: [aStream tab; nextPutAll: 'rate: ', rate printString; cr].
	channels == nil
		ifFalse: [aStream tab; nextPutAll: 'channels: ', channels printString; cr].
	format == nil
		ifFalse: [aStream tab; nextPutAll: 'format: ', format printString; cr].
	range == nil
		ifFalse: [aStream tab; nextPutAll: 'range: ', range printString; cr].
	properties == nil
		ifFalse: [properties associationsDo:
			[ :ass |
			ass key == #envelope
				ifFalse: [aStream tab; nextPutAll: ass key, ': ', ass value printString; cr]]].
	self envelope == nil
		ifFalse: [aStream tab; nextPutAll: 'envelope: ', self envelope size printString, ' values'; cr].
	self cueList == nil
		ifFalse: [aStream tab; nextPutAll: 'cue list: ', self cueList size printString, ' regions'; cr].
	self printSamplesOn: aStream.
	aStream nextPutAll: ')'; cr.! !

!SampledSound methodsFor: 'printing'!
printSamples
	"pretty-print a few samples."

	| stream |
	stream := WriteStream on: (String new: 4096).
	1 to: 512 do:
		[ :i | stream nextPutAll: ((self sampleAt: i) printString); cr].
	^stream contents! !

!SampledSound methodsFor: 'printing'!
printSamplesOn: aStream
	"Pretty-print a few samples--32 by default, 1024 if shift-down, *all* of ctrl and shift down."

	| number |
	data ifNil: [^aStream nextPutAll: '(no samples)'].
	number := 128 min: self size.
	aStream tab; nextPutAll: 'samples: #('.
	1 to: number do:
		[ :i | aStream nextPutAll: ((self sampleAt: i) printString), ' '].
	aStream nextPutAll: '...'.
	aStream nextPut: $);  cr.! !

!SampledSound methodsFor: 'printing'!
storeOnFileNamed: aName
	"Save the receiver on the sound file named by the argument."

	(SoundFile named: aName) saveSound: self! !


!SampledSound methodsFor: 'private'!
getData
	"Read samples in from the file if none in memory."

	| realme |
	samplesInMemory > 0
		ifTrue: [data := nil].
	realme := SampledSound fromFile: self fullName.
	data :=  realme data.
	samplesInMemory := realme samplesInMemory.
	domain := realme domain.! !

!SampledSound methodsFor: 'private'!
mapSampleIndex: ind
	"Map the given sample index according to the 'page' (firstIndex) of the receiver."

	^ind truncated - firstIndex! !

!SampledSound methodsFor: 'private'!
privateSampleAt: anIndex
	"Answer the sample at the given index as an integer."

	^data isCPointer
		ifTrue: [data isValid
			ifTrue: [data at: anIndex - 1]
			ifFalse: [0]]
		ifFalse: [data shortAt: (anIndex * 2 - 1) bigEndian: (UninterpretedBytes isBigEndian)]! !

!SampledSound methodsFor: 'private'!
privateSampleAt: anIndex in: cPtr bigEndian: isBE
	"Answer the sample at the given index as an integer, 
		assuming the output is a cPointer; handle endianness."

	| byteIndex word |
	byteIndex := anIndex - 1 * 2.			"zero-based"
	word := isBE
		ifTrue: [((cPtr at: byteIndex) bitShift: 8) + (cPtr at: byteIndex + 1)]
		ifFalse: [((cPtr at: byteIndex + 1) bitShift: 8) + (cPtr at: byteIndex)].
	^(word bitAnd: 16r7FFF) - (word bitAnd: 16r8000)! !

!SampledSound methodsFor: 'private'!
privateSampleAt: anIndex in: cPtr put: value bigEndian: isBE
	"Answer the sample at the given index as an integer, 
		assuming the output is a cPointer; handle endianness."

	| byteIndex word |
	byteIndex := anIndex - 1 * 2.			"zero-based"
	word := (value bitAnd: 16r7FFF) - (value bitAnd: -16r8000).
	isBE
		ifTrue:
			[cPtr at: byteIndex put: (word bitShift: -8).
			cPtr at: byteIndex + 1 put: (word bitAnd: 16rFF)]
		ifFalse:
			[cPtr at: byteIndex + 1 put: (word bitShift: -8).
			cPtr at: byteIndex put: (word bitAnd: 16rFF)]! !

!SampledSound methodsFor: 'private'!
privateSampleAt: anIndex put: aValue
	"Stuff the given sample at the given index (safely)."

	data shortAt: (anIndex * 2 - 1) put: (aValue truncated) bigEndian: (UninterpretedBytes isBigEndian)! !

!SampledSound methodsFor: 'private'!
readSamplesFrom: ind
	"Read in samples from disk starting a bit before the given index."

	| file fSize offset |
	file := self file.
	file == nil
		ifTrue: [self error: 'There is no sound file associated with this sound.'].
	fSize := file size.
	offset := ind - 128.
	(offset + samplesInMemory) > fSize
		ifTrue: [offset := fSize - samplesInMemory].
	Cursor read showWhile: [file readSamplesStartingAt: offset].
	firstIndex := offset! !

!SampledSound methodsFor: 'private'!
realSound
	"Answer the real sound for the receiver (overridden in virtual subclasses)."

	^self! !

!SampledSound methodsFor: 'private'!
scaleSampleIndex: ind
	"Scale the given sample index according to the sample size of the receiver."

	^ind * self sampleSize - 1.! !


!SampledSound methodsFor: 'play/record'!
nextBufferInto: outBuffer frames: bufferSize channels: outChannels format: sformat startingAt: startFrame
	"Copy data from the receiver into the given output buffer; answer the current sample index."

	| frames numToDo isBE |
"	Transcript show: startFrame printString; cr."
	frames := self size.
	startFrame >= frames ifTrue: [^nil].
	sformat == format ifFalse: [self error: 'Output format not supported.'].
	numToDo := bufferSize min: (frames - startFrame).
	isBE := UninterpretedBytes isBigEndian.
"If the sound and the output buffer are the same # of channels -- copy samples"
	outChannels = channels
		ifTrue: [1 to: (numToDo * channels) do:
				[ :frm | | current samp |
				current := self privateSampleAt: frm in: outBuffer bigEndian: isBE.
				samp := self sampleAt: (startFrame + frm).
				self privateSampleAt: frm in: outBuffer put: (current + samp) bigEndian: isBE].
			^numToDo = bufferSize
				ifFalse: [nil]
				ifTrue: [startFrame + numToDo]].
"else copy mono sound to multi-channel output"
	channels = 1
		ifTrue: [1 to: numToDo do:
				[ :frm | | samp |
				samp := self sampleAt: (startFrame + frm).
				0 to: outChannels - 1 do:
					[ :cha | | pos current |
					pos := frm + cha.
					current := self privateSampleAt: pos in: outBuffer bigEndian: isBE.
					self privateSampleAt: pos in: outBuffer put: (current + samp) bigEndian: isBE]].
			^numToDo = bufferSize
				ifFalse: [nil]
				ifTrue: [startFrame + numToDo]].
	self error: 'unhandled sound output channel mismatch.'! !

!SampledSound methodsFor: 'play/record'!
play
	"Play the receiver out over the default sound port."
	"(SampledSound sweepDur: 2 rate: 44100 from: 20 to: 1000 chans: 1) play"

	SoundPort default play: self! !


!SampledSound methodsFor: 'testing'!
hasGaps
	"Answer whether the receiver has any gaps."

	^false! !

!SampledSound methodsFor: 'testing'!
isComposite
	"Answer whether the receiver is a composite sound."

	^false! !

!SampledSound methodsFor: 'testing'!
isEmpty
	"Answer whether the receiver is empty."

	^data isNil! !

!SampledSound methodsFor: 'testing'!
isVirtual
	"Answer whether the receiver is a virtual sound."

	^false! !


!SampledSound methodsFor: 'initialize-release'!
initialize
	"Set up a default sound."

	super initialize.
	firstIndex := 0.
"	folder := ''."
	name := ''.
	rate := 44100.
	channels := 1.
	format := #lin16Bit.
	samplesInMemory := 0! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SampledSound class' category: 'Music-Sound'!
SampledSound class
	instanceVariableNames: ''!

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 6/17/2015 11:26'!
addLibrarySoundNamed: aString fromAIFFfileNamed: fileName
	"Add a sound from the given AIFF file to the library."
	"SampledSound
		addLibrarySoundNamed: 'shutterClick'
		fromAIFFfileNamed: '7.aif'"
	"Add all .aif files in the current directory to the sound library:
	| fileNames |
	fileNames _ FileDirectory smalltalkImageDirectory fileNamesMatching: '*.aif'.
	fileNames do: [:fName |
		SampledSound
			addLibrarySoundNamed: (fName copyUpTo: $.)
			fromAIFFfileNamed: fName]"

	| snd |
	snd _ self fromAIFFfileNamed: fileName.
	self addLibrarySoundNamed: aString
		samples: snd samples
		samplingRate: snd originalSamplingRate.
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 10:41'!
addLibrarySoundNamed: aString samples: sampleData samplingRate: samplesPerSecond
	"Add the given sound to the sound library. The sample data may be either a ByteArray or a SoundBuffer. If the former, it is take to be 8-bit unsigned samples. If the latter, it is taken to be 16 bit signed samples."

	SoundLibrary
		at: aString
		put: (Array with: sampleData with: samplesPerSecond).
! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 4/14/2003 00:01'!
assimilateSoundsFrom: aDictionary
	"assimilate sounds with new keys from the given dictionary"

	aDictionary associationsDo:
		[:assoc | (SoundLibrary includesKey: assoc key) ifFalse:
			[SoundLibrary add: assoc]]! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 11:04'!
playSoundNamed: aString
	"Play the sound with given name. Do nothing if there is no sound of that name in the library."
	"SampledSound playSoundNamed: 'croak'"

	| snd |
	snd _ self soundNamed: aString.
	snd ifNotNil: [snd play].
	^ snd
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jm 1/14/1999 10:40'!
putCoffeeCupClinkInSoundLibrary
	"SampledSound putCoffeeCupClinkInSoundLibrary"

	self addLibrarySoundNamed: 'clink'
		samples: self coffeeCupClink
		samplingRate: 11025! !

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 3/2/2010 10:06'!
removeSoundNamed: aString
	"Remove the sound with the given name from the sound library."

	SoundLibrary removeKey: aString ifAbsent: nil! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 9/27/2001 14:46'!
soundNamed: aString
	"Answer the sound of the given name, or, if there is no sound of that name, put up an informer so stating, and answer nil"

	"(SampledSound soundNamed: 'shutterClick') play"

	^ self soundNamed: aString ifAbsent:
		[self inform: aString, ' not found in the Sound Library'.
		nil]! !

!SampledSound class methodsFor: 'sound library' stamp: 'sw 9/27/2001 14:45'!
soundNamed: aString ifAbsent: aBlock
	"Answer the sound of the given name, or if there is no sound of that name, answer the result of evaluating aBlock"
	"(SampledSound soundNamed: 'shutterClick') play"

	| entry samples |
	entry _ SoundLibrary
		at: aString
		ifAbsent:
			[^ aBlock value].
	entry ifNil: [^ aBlock value].
	samples _ entry at: 1.
	samples class isBytes ifTrue: [samples _ self convert8bitSignedTo16Bit: samples].
	^ self samples: samples samplingRate: (entry at: 2)
! !

!SampledSound class methodsFor: 'sound library' stamp: 'jmv 1/16/2011 23:55'!
soundNames
	"Answer a list of sound names for the sounds stored in the sound library."
	"| s |
	 SampledSound soundNames asSortedCollection do: [:n |
		n asParagraph display.
		s _ SampledSound soundNamed: n.
		s ifNotNil: [s playAndWaitUntilDone]]"

	^ SoundLibrary keys! !

!SampledSound class methodsFor: 'sound library' stamp: 'jhm 10/15/97 14:57'!
unusedSoundNameLike: desiredName
	"Pick an unused sound name based on the given string. If necessary, append digits to avoid name conflicts with existing sounds."
	"SampledSound unusedSoundNameLike: 'chirp'"

	| newName i |
	newName _ desiredName.
	i _ 2.
	[SoundLibrary includesKey: newName] whileTrue: [
		newName _ desiredName, i printString.
		i _ i + 1].
	^ newName
! !


!SampledSound class methodsFor: 'instance creation' stamp: 'gk 2/24/2004 08:50'!
beep
	"Beep in the presence of the sound system.
	Not to be used directly - use Beeper class>>beep
	or Beeper class>>beepPrimitive instead."

	(self new
			setSamples: self coffeeCupClink
			samplingRate: 12000) play
			! !

!SampledSound class methodsFor: 'instance creation'!
default
	"Answer the default empty sound."
	"StoredSound default"

	^self duration: 0.5 rate: 44100 channels: 1! !

!SampledSound class methodsFor: 'instance creation'!
duration: aDur named: nameString rate: aRate channels: aChannels format: aFormat
	"Answer a new 16-bit linear sound with the given properties."

	^self named: nameString size: (aDur * aRate) truncated rate: aRate 
				channels: aChannels format: aFormat! !

!SampledSound class methodsFor: 'instance creation'!
duration: aDur rate: aRate channels: aChannels
	"Answer a new sound with the given properties."

	^self named: nil size: (aDur * aRate) truncated rate: aRate 
				channels: aChannels format: self formatSymbol! !

!SampledSound class methodsFor: 'instance creation'!
duration: aDur rate: aRate channels: aChannels format: aFormat
	"Answer a new sound with the given properties."

	^self named: nil size: (aDur * aRate) truncated rate: aRate 
				channels: aChannels format: aFormat! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jmv 6/17/2015 11:26'!
fromAIFFfileNamed: fileName
	"Read a SampledSound from the AIFF file of the given name, merging stereo to mono if necessary."
	"(SampledSound fromAIFFfileNamed: '1.aif') play"
	"| snd |
	 FileDirectory smalltalkImageDirectory fileNames do: [:n |
		(n endsWith: '.aif')
			ifTrue: [
				snd _ SampledSound fromAIFFfileNamed: n.
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	| aiffFileReader |
	aiffFileReader _ AIFFFileReader new.
	aiffFileReader readFromFile: fileName
		mergeIfStereo: true
		skipDataChunk: false.
	^ self
		samples: (aiffFileReader channelData at: 1)
		samplingRate: aiffFileReader samplingRate
! !

!SampledSound class methodsFor: 'instance creation'!
fromData: anArray named: nameString rate: aRate channels: aChannels format: aFormat
	"Answer a new sound from the given data."
	"Example: 
		| t |
		 t := Array new: 65536. 
		 0 to: 255 do: 
			[ :i | 1 to: 256 do:
				[ :j | t at: ((i * 256) + j) put: (j / 256 * 32000 - 16000) asInteger]].
		^StoredSound fromData: t named: 'ramp' 
				rate: 16000 channels: 1 format: #lin16Bit"

	| sound |
	sound := self named: nameString size: (anArray size / aChannels) truncated
				rate: aRate channels: aChannels format: aFormat.
	1 to: anArray size do:
		[ :i | sound sampleAt: i  put: (anArray at: i)].
	^sound! !

!SampledSound class methodsFor: 'instance creation'!
fromData: anArray rate: aRate channels: aChannels format: aFormat
	"Answer a new 16-bit linear sound from the given data."
	"StoredSound immediateInspect"

	^self fromData: anArray named: nil rate: aRate channels: aChannels format: aFormat! !

!SampledSound class methodsFor: 'instance creation'!
fromDblData: anArray rate: aRate channels: aChannels size: siz
	"Answer a new sound from the given data."

	| sound scale |
	sound := self size: (siz * aChannels) truncated rate: aRate channels: aChannels format: #lin16Bit.
	scale := self maxSample.
	1 to: siz do:
		[ :i | sound sampleAt: i  put: ((anArray valueAt: (i asFloat / aRate)) * scale) truncated].
	^sound! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jmv 7/21/2017 16:19:57'!
fromFileEntry: aFileEntry
	"(SampledSound fromWaveFileNamed: 'c:\windows\media\chimes.wav') play"
	"| snd fd |
	fd := FileDirectory on:'c:\windows\media\'.
	fd fileNames do: [:n |
		(n asLowercase endsWith: '.wav')
			ifTrue: [
				snd _ SampledSound fromWaveFileNamed: (fd pathName,n).
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	^ aFileEntry readStreamDo: [ :strm |
		self fromWaveStream: strm ]
! !

!SampledSound class methodsFor: 'instance creation' stamp: 'pb 5/25/2016 01:33'!
fromWaveFileNamed: fileName
	"(SampledSound fromWaveFileNamed: 'c:\windows\media\chimes.wav') play"
	"| snd fd |
	fd := FileDirectory on:'c:\windows\media\'.
	fd fileNames do: [:n |
		(n asLowercase endsWith: '.wav')
			ifTrue: [
				snd _ SampledSound fromWaveFileNamed: (fd pathName,n).
				snd play.
				SoundPlayer waitUntilDonePlaying: snd]]."

	^ fileName asFileEntry readStreamDo: [ :strm |
		self fromWaveStream: strm ]
! !

!SampledSound class methodsFor: 'instance creation'!
fromWaveStream: fileStream

	| stream header data type channels samplingRate blockAlign bitsPerSample leftAndRight |
	header _ self readWaveChunk: 'fmt ' inRIFF: fileStream.
	data _ self readWaveChunk: 'data' inRIFF: fileStream.
	fileStream close.
	stream _ ReadStream on: header.
	type _ self next16BitWord: false from: stream.
	type = 1 ifFalse: [^ self error:'Unexpected wave format'].
	channels _ self next16BitWord: false from: stream.
	(channels < 1 or: [channels > 2])
		ifTrue: [^ self error: 'Unexpected number of wave channels'].
	samplingRate _ self next32BitWord: false from: stream.
	stream skip: 4. "skip average bytes per second"
	blockAlign _ self next16BitWord: false from: stream.
	bitsPerSample _ self next16BitWord: false from: stream.
	(bitsPerSample = 8 or: [bitsPerSample = 16])
		ifFalse: [  "recompute bits per sample"
			bitsPerSample _ (blockAlign // channels) * 8].

	bitsPerSample = 8
		ifTrue: [data _ self convert8bitUnsignedTo16Bit: data]
		ifFalse: [data _ self convertBytesTo16BitSamples: data mostSignificantByteFirst: false].

	channels = 2 ifTrue: [
		leftAndRight _ data splitStereo.
		^ MixedSound new
			add: (self samples: leftAndRight first samplingRate: samplingRate) pan: 0.0;
			add: (self samples: leftAndRight last samplingRate: samplingRate) pan: 1.0;
			yourself].

	^ self samples: data samplingRate: samplingRate
! !

!SampledSound class methodsFor: 'instance creation'!
headerFromFile: nameString
	"Open the given file (EBICSF, NeXT, or SPARC soundfile format)"
	"StoredSound headerFromFile: 'snd/new/orchestra.snd'"

	"FileDirectory on: (FileDirectory dirPathFor: nameString) entries"

	| file sound size |
	file := SoundFile openFileNamed: (SirenUtility findFile: nameString).
	file == nil ifTrue: [^nil].
"	ver := nameString betweenPeriodsIfEmpty: '1.0.0'."
	size := file size.
	sound := self named: nameString size: size rate: file rate 
				channels: file channels format: file format.
	sound duration: (Duration value: (size / sound channels / sound rate) asFloat).
"	ver isEmpty
		ifTrue: [sound version: '1a00']
		ifFalse: [sound version: ver].
"	file properties == nil
		ifFalse: [file properties associationsDo: 
			[ :ass | sound at: ass key put: ass value]].
	file close.
	^sound! !

!SampledSound class methodsFor: 'instance creation'!
named: aName size: aSize format: formatSymbol
	"Answer a new Sound with the given name and storage size."

	| sound |
	sound := self size: aSize format: formatSymbol channels: 1.
	sound name: aName.
	^sound! !

!SampledSound class methodsFor: 'instance creation'!
named: nameString size: size rate: rate channels: chans format: formatSymbol
	"Answer a new Sound with the given storage size (in sample frames), etc."

	| sound  nam |
	(((#lin16Bit -> [sound := SampledSound new initialize])),
	(#float32Bit -> [sound := FloatSound new initialize]))
		case: formatSymbol
		otherwise: [sound := SampledSound new initialize
				"self error: 'Unknown sound format.'"].
	sound channels: chans.
	sound rate: rate.
	sound format: formatSymbol.
	sound size: size.
	sound allocateData.
	((nameString == nil) or: [nameString isEmpty])
		ifFalse: [nam :=  nameString asFilename tail.
			sound name: nam.
			sound folder: nameString asFilename head.
			self named: nam put: sound].
	^sound! !

!SampledSound class methodsFor: 'instance creation'!
named: nameString size: size rate: rate channels: chans format: formatSymbol data: data
	"Answer a new Sound with the given storage size (in sample frames), etc."

	| sound nam |
	((#lin16Bit -> [sound := self new initialize.
			sound samples: data]),
	(#float32Bit -> [sound := FloatSound new initialize.
			sound samples: data]))
		case: formatSymbol
		otherwise: [self error: 'Unknown sound format.'].
	sound channels: chans.
	sound rate: rate.
	sound format: formatSymbol.
	sound size: size.
	nameString == nil
		ifFalse: [nam :=  nameString asFilename tail.
			sound name: nam.
			sound folder: nameString asFilename head.
			self named: nam put: sound].
	^sound! !

!SampledSound class methodsFor: 'instance creation' stamp: 'jm 9/12/97 19:20'!
samples: anArrayOf16BitSamples samplingRate: samplesPerSecond
	"Return a SampledSound with the given samples array and sampling rate."

	^ self new setSamples: anArrayOf16BitSamples samplingRate: samplesPerSecond
! !

!SampledSound class methodsFor: 'instance creation'!
size: size format: formatSymbol channels: chans
	"Answer a new Sound with the given storage size, etc."

	^self named: nil size: size rate: (SoundPort default class rate) 
				channels: chans format: formatSymbol! !

!SampledSound class methodsFor: 'instance creation'!
size: size rate: aRate channels: chans
	"Answer a new Sound with the given storage size, etc."

	^self named: nil size: size rate: aRate channels: chans format: self formatSymbol! !

!SampledSound class methodsFor: 'instance creation'!
size: size rate: rate channels: chans format: formatSymbol
	"Answer a new Sound with the given storage size, etc."

	^self named: nil size: size rate: rate channels: chans format: formatSymbol! !

!SampledSound class methodsFor: 'instance creation'!
size: size rate: rate format: formatSymbol channels: chans
	"Answer a new Sound with the given storage size, etc."

	^self named: nil size: size rate: rate channels: chans format: formatSymbol! !


!SampledSound class methodsFor: 'coffee cup clink' stamp: 'jm 8/23/97 22:25'!
coffeeCupClink
	"Return the samples array for the sound of a spoon being tapped against a coffee cup."

	CoffeeCupClink ifNil: [self initializeCoffeeCupClink].
	^ CoffeeCupClink
! !

!SampledSound class methodsFor: 'coffee cup clink' stamp: 'jm 9/17/97 12:50'!
initializeCoffeeCupClink
	"Initialize the samples array for the sound of a spoon being tapped against a coffee cup."
	"SampledSound initializeCoffeeCupClink"

	| samples |
	samples _ #(768 1024 -256 2304 -13312 26624 32512 19200 6400 -256 5888 32512 28928 32512 -32768 32512 -32768 18688 26368 -26112 32512 32512 2304 32512 5632 2816 10240 -4608 -1792 32512 32512 -5376 10752 32512 32512 32512 8192 15872 32512 -3584 -32768 -23296 -24832 -32768 -32768 -32768 -2304 32512 32512 -32768 32512 -15360 6400 8448 -18176 -32768 -256 -32768 -29440 9472 20992 17920 32512 32512 -256 32512 -32768 -32768 -23040 -32768 -25088 -32768 -27648 -1536 24320 -32768 32512 20480 27904 22016 16384 -32768 32512 -27648 -32768 -7168 28160 -6400 5376 32512 -256 32512 -7168 -11776 -19456 -27392 -24576 -32768 -24064 -19456 12800 32512 27136 2048 25344 15616 8192 -4608 -28672 -32768 -30464 -2560 17664 256 -8192 8448 32512 27648 -6144 -512 -7424 -18688 7936 -256 -22272 -14080 2048 27648 15616 -12288 -768 5376 3328 5632 3072 -6656 -20480 10240 27136 -10752 -11008 -768 -2048 6144 -7168 -3584 -1024 -7680 19712 26112 1024 -11008 3072 16384 -8960 -14848 -4864 -23808 -11264 12288 8192 7168 4864 23040 32512 512 -11776 -5632 -16896 -21504 -12800 -6144 -16896 -4352 32512 32512 23296 21760 5632 2816 -9472 -20992 -11264 -29440 -32768 -3584 7680 8448 15360 32512 32512 15616 15104 -2048 -27904 -27904 -25600 -12288 -12032 -13568 17152 22272 15360 30208 28160 7680 -5632 -8192 -16384 -31744 -25856 -10752 -3840 6656 13056 24320 26368 12800 20736 12288 -19200 -20992 -16640 -21504 -17920 -6912 8448 11264 14080 23040 18176 8192 -1024 0 256 -20992 -19712 -4608 -11264 -2048 14080 12032 8192 6912 13056 9216 -5632 -5376 -3840 -6656 -9984 -5632 4864 -3584 -1280 17408 7680 -1280 4096 2816 -1024 -4864 3328 8448 -768 -5888 -2048 5120 0 3072 11008 -7680 -15360 2560 6656 -3840 0 11776 7680 2816 1536 -1280 -3840 -8704 -1536 3584 -9728 -9728 11776 18688 7680 6656 6400 -4864 -3840 -256 -6912 -13312 -11264 2304 9728 1792 3328 18944 18432 6912 6144 -1536 -17664 -14336 -2304 -10496 -15616 -4096 9728 17152 14848 13312 11520 2304 -1024 2560 -8704 -26624 -18688 -256 -256 2816 14080 13824 12544 14080 9728 -512 -12032 -8960 -3328 -9984 -15872 -5120 8192 3584 10496 20224 7936 4608 6144 1280 -8704 -12800 -7424 -8448 -8960 -3840 7424 13056 8704 13312 13056 -2304 -4864 -768 -7168 -10496 -4608 -1536 -3072 -1280 6144 13312 11008 4864 4864 1536 -8960 -7680 1792 -4864 -7680 2816 5632 3328 2560 5376 7936 3584 -512 512 -4608 -9728 0 9216 768 -4096 7680 7168 256 4608 -768 -8704 -6400 2048 6144 -3072 -3328 6400 9472 3840 -768 1792 -3840 -5120 6144 768 -9984 -4352 5120 9472 6912 2816 1792 1280 768 512 -2816 -9728 -6912 6912 6912 -3328 -768 8448 11776 10752 3328 -6912 -10752 -8704 -1536 0 -6912 -3328 9984 13568 7424 6144 6656 256 0 256 -12032 -17920 -8192 3584 8960 4096 5632 12032 8704 6912 5632 -3584 -10496 -7936 -2048 -9216 -11776 2304 9472 15104 14848 5888 512 -2816 1024 2560 -9984 -13312 -5120 768 1792 768 8448 12032 11264 12800 -256 -11264 -9728 -2304 3072 -4352 -6912 256 2304 5376 9984 8192 2816 1280 3584 -2048 -11008 -8448 -2048 3072 4864 2304 3072 3072 3072 7168 3328 -5376 -4864 512 512 -1792 -1792 1792 5376 5888 5888 512 -5888 -3584 4096 3584 -6400 -4864 4608 3072 3840 5376 1024 768 2816 5888 -768 -12288 -7936 2304 5888 3328 2048 6144 3072 3072 6400 -3328 -7168 256 4096 -512 -9472 -6656 3328 6912 9216 8704 3840 -2560 -256 6656 -2560 -11264 -4608 -768 -1280 1536 3072 4096 5120 9984 11264 1024 -8192 -6144 -1024 -3840 -5632 -512 1024 2304 9728 9728 1280 512 4096 2816 -3584 -9984 -6912 -2304 512 5632 7680 3584 1024 5632 5888 -1280 -3584 -2304 -2560 -1536 -1024 -1792 -512 1536 7680 9984 2048 -2048 2048 3328 -1280 -4096 -3328 -4608 -1280 4352 3328 1280 1792 5120 6912 1024 -2560 0 -768 -1024 1280 -256 -4608 -1280 6400 5120 768 1792 2560 2048 0 -1536 -1280 -2304 1024 5376 2560 -2560 -512 4096 2048 512 768 -1280 -256 2560 2560 -256 -1024 768 3584 1280 -3328 -1536 1792 2816 3328 2304 -256 256 2816 2304 -1280 -3328 -1536 2304 2304 -256 -256 1024 1536 3840 5120 1024 -2048 0 1536 -768 -2560 -1792 256 2304 2048 1536 256 768 5888 6656 256 -3840 -2304 -1280 -1536 256 0 -512 2304 4352 3840 768 0 2304 3072 256 -3072 -2560 -2560 256 4608 2560 256 1536 3072 3072 1792 256 256 512 -256 -768 -1280 -1536 768 4352 2816 -512 768 2560 2560 2304 -256 -1792 -768 768 1792 256 -2304 -256 3328 3840 2304 2304 1536 256 2048 1024 -1536 -1792 -1024 512 256 -512 0 2304 4864 5120 4352 1024 -1280 0 -768 -2816 -2304 -512 1024 2048 2304 2048 3072 3840 2816 2048 -512 -3072 -1792 -1536 -1280 768 1280 1536 2304 2816 2048 1536 2048 1536 1536 -768 -3840 -2048 0 1280 2816 1792 1536 2560 3584 2816 1024 256 -768 -768 -1280 -2816 -768 1792 3328 5120 3072 1280 1536 1792 768 -1024 -1280 -1536 -768 512 256 1536 2560 2560 3328 1280 0 768 1536 768 -256 -512 -1536 -1280 768 1280 2304 2560 2560 2560 1024 -256 -512 0 1280 1536 768 -1280 -512 2048 1536 2048 1280 -256 256 512 768 768 1280 2304 1792 512 -1280 -1024 768 1536 1536 256 -768 1536 3584 3072 1792 -256 -1536 -512 256 -512 -512 768 2048 2048 1792 1280 1280 3072 2816 768 -1024 -2304 -1024 256 256 1280 1792 2304 2816 2304 1280 512 1024 768 -768 -1280 -1280 -512 1536 2560 2816 2048 512 1024 1792 1280 768 0 -768 -768 0 256 256 1280 2560 2304 2304 1536 512 512 1024 1280 0 -1792 -1536 -512 1280 3072 2816 1792 512 1024 1536 256 -256 768 768 256 256 -256 512 1280 1280 1536 768 1024 1792 1536 1024 0 256 -512 -256 1024 512 256 768 1792 2304 1280 256 768 1024 1280 1792 768 -768 -768 768 512 256 1024 1792 1536 1280 1536 1792 1280 768 512 -512 -1792 -512 512 768 2304 2816 1792 768 1536 2304 1536 0 -256 -256 -768 -768 256 1536 1536 2304 2048 256 768 2048 2304 1280 0 -256 -1024 -1024 0 1024 1792 2304 2304 1280 512 1280 2048 1280 256 -512 -1792 -1536 256 1536 1792 2048 2048 2048 1536 512 512 768 256 -256 0 -512 -1024 768 2048 2304 2304 1280 1280 1024 1024 1024 0 -512 256 768 0 -256 1536 2304 1792 2304 1280 -512 -256 768 1536 1024 256 512 512 1024 1792 1792 1536 1024 1280 0 -1280 256 2048 2560 2048 1024 -256 -256 1024 1280 1536 1024 0 0 256 768 1792 2304 2048 1280 1024 0 -512 -256 256 1024 1024 512 768 768 1280 2048 1792 1024 768 768 -256 -1024 0 256 1024 1536 1024 1280 1536 1792 1792 1024 512 512 0 -512 -256 512 768 1280 1280 1024 1280 1792 1792 1280 512 -256 -256 256 512 1280 1024 1280 1280 1024 1024 768 1024 1024 1024 1280 256 256 768 768 1024 512 256 768 1280 2560 2560 1280 512 -256 -512 -256 1024 1536 768 1024 1280 768 1024 1536 1536 1024 256 0 0 0 768 768 512 1280 1536 1280 1280 1280 1280 768 768 256 -256 768 768 256 768 1280 1792 1536 1536 1536 256 512 1024 0 -768 -256 768 512 1024 2048 1536 1024 1536 1536 768 0 0 -256).

	CoffeeCupClink _ SoundBuffer fromArray: samples.
! !


!SampledSound class methodsFor: 'utilities' stamp: 'ar 2/3/2001 16:14'!
convert8bitSignedFrom: aByteArray to16Bit: aSoundBuffer
	"Copy the contents of the given array of signed 8-bit samples into the given array of 16-bit signed samples."

	| n s |
	<primitive: 'primitiveConvert8BitSigned' module: 'MiscPrimitivePlugin'>
	self var: #aByteArray declareC: 'unsigned char *aByteArray'.
	self var: #aSoundBuffer declareC: 'unsigned short *aSoundBuffer'.
	n _ aByteArray size.
	1 to: n do: [:i |
		s _ aByteArray at: i.
		s > 127
			ifTrue: [aSoundBuffer at: i put: ((s - 256) bitShift: 8)]
			ifFalse: [aSoundBuffer at: i put: (s bitShift: 8)]].
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 2/15/98 18:13'!
convert8bitSignedTo16Bit: aByteArray
	"Convert the given array of samples--assumed to be 8-bit signed, linear data--into 16-bit signed samples. Return an array containing the resulting samples. Typically used to read uncompressed AIFF sound data."

	| result |
	result _ SoundBuffer newMonoSampleCount: aByteArray size.
	self convert8bitSignedFrom: aByteArray to16Bit: result.
	^ result
! !

!SampledSound class methodsFor: 'utilities' stamp: 'ar 1/27/98 23:11'!
convert8bitUnsignedTo16Bit: anArray
	"Convert the given array of samples--assumed to be 8-bit unsigned, linear data--into 16-bit signed samples. Return an array containing the resulting samples. Typically used to read uncompressed WAVE sound data."

	| n samples s |
	n _ anArray size.
	samples _ SoundBuffer newMonoSampleCount: n.
	1 to: n do: [:i |
		s _ anArray at: i.
		samples at: i put: (s - 128 * 256)].
	^ samples
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 3/17/98 21:07'!
convertBytesTo16BitSamples: aByteArray mostSignificantByteFirst: msbFirst
	"Convert the given ByteArray (with the given byte ordering) into 16-bit sample buffer."

	| n data src b1 b2 w |
	n _ aByteArray size // 2.
	data _ SoundBuffer newMonoSampleCount: n.
	src _ 1.
	1 to: n do: [:i |
		b1 _ aByteArray at: src.
		b2 _ aByteArray at: src + 1.
		msbFirst
			ifTrue: [w _ (b1 bitShift: 8) + b2]
			ifFalse: [w _ (b2 bitShift: 8) + b1].
		w > 32767 ifTrue: [w _ w - 65536].
		data at: i put: w.
		src _ src + 2].
	^ data
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/17/97 13:11'!
uLawDecode: aByteArray
	"Convert the given array of uLaw-encoded 8-bit samples into a SoundBuffer of 16-bit signed samples."

	| n out decodingTable |
	n _ aByteArray size.
	out _ SoundBuffer newMonoSampleCount: n.
	decodingTable _ self uLawDecodeTable.
	1 to: n do: [:i | out at: i put: (decodingTable at: (aByteArray at: i) + 1)].
	^ out
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 16:41'!
uLawDecodeTable
	"Return a 256 entry table to be used to decode 8-bit uLaw-encoded samples."
	"Details: This table was computed as follows:
		| d encoded lastEncodedPos lastEncodedNeg |
		d _ Array new: 256.
		lastEncodedPos _ nil.
		lastEncodedNeg _ nil.
		4095 to: 0 by: -1 do: [:s |
			encoded _ SampledSound uLawEncodeSample: s.
			lastEncodedPos = encoded
				ifFalse: [
					d at: (encoded + 1) put: (s bitShift: 3).
					lastEncodedPos _ encoded].
			encoded _ encoded bitOr: 16r80.
			lastEncodedNeg = encoded
				ifFalse: [
					d at: (encoded + 1) put: (s bitShift: 3) negated.
					lastEncodedNeg _ encoded]].
		d "

	^ #(32760 31608 30584 29560 28536 27512 26488 25464 24440 23416 22392 21368 20344 19320 18296 17272 16248 15736 15224 14712 14200 13688 13176 12664 12152 11640 11128 10616 10104 9592 9080 8568 8056 7800 7544 7288 7032 6776 6520 6264 6008 5752 5496 5240 4984 4728 4472 4216 3960 3832 3704 3576 3448 3320 3192 3064 2936 2808 2680 2552 2424 2296 2168 2040 1912 1848 1784 1720 1656 1592 1528 1464 1400 1336 1272 1208 1144 1080 1016 952 888 856 824 792 760 728 696 664 632 600 568 536 504 472 440 408 376 360 344 328 312 296 280 264 248 232 216 200 184 168 152 136 120 112 104 96 88 80 72 64 56 48 40 32 24 16 8 0 -32760 -31608 -30584 -29560 -28536 -27512 -26488 -25464 -24440 -23416 -22392 -21368 -20344 -19320 -18296 -17272 -16248 -15736 -15224 -14712 -14200 -13688 -13176 -12664 -12152 -11640 -11128 -10616 -10104 -9592 -9080 -8568 -8056 -7800 -7544 -7288 -7032 -6776 -6520 -6264 -6008 -5752 -5496 -5240 -4984 -4728 -4472 -4216 -3960 -3832 -3704 -3576 -3448 -3320 -3192 -3064 -2936 -2808 -2680 -2552 -2424 -2296 -2168 -2040 -1912 -1848 -1784 -1720 -1656 -1592 -1528 -1464 -1400 -1336 -1272 -1208 -1144 -1080 -1016 -952 -888 -856 -824 -792 -760 -728 -696 -664 -632 -600 -568 -536 -504 -472 -440 -408 -376 -360 -344 -328 -312 -296 -280 -264 -248 -232 -216 -200 -184 -168 -152 -136 -120 -112 -104 -96 -88 -80 -72 -64 -56 -48 -40 -32 -24 -16 -8 0)
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 15:52'!
uLawEncode: anArray
	"Convert the given array of 16-bit signed samples into a ByteArray of uLaw-encoded 8-bit samples."

	| n out s |
	n _ anArray size.
	out _ ByteArray new: n.
	1 to: n do: [:i |
		s _ anArray at: i.
		s _ s bitShift: -3.  "drop 4 least significant bits"
		s < 0
			ifTrue: [s _ (self uLawEncodeSample: s negated) bitOr: 16r80]
			ifFalse: [s _ (self uLawEncodeSample: s)].
		out at: i put: s].
	^ out
! !

!SampledSound class methodsFor: 'utilities' stamp: 'jm 9/13/97 15:40'!
uLawEncodeSample: s
	"Encode the given 16-bit signed sample using the uLaw 8-bit encoding."

	s < 496 ifTrue: [
		s < 112 ifTrue: [
			s < 48 ifTrue: [
				s < 16
					ifTrue: [^ 16r70 bitOr: (15 - s)]
					ifFalse: [^ 16r60 bitOr: (15 - ((s - 16) bitShift: -1))]].
			^ 16r50 bitOr: (15 - ((s - 48) bitShift: -2))].
		s < 240
			ifTrue: [^ 16r40 bitOr: (15 - ((s - 112) bitShift: -3))]
			ifFalse: [^ 16r30 bitOr: (15 - ((s - 240) bitShift: -4))]].

	s < 2032 ifTrue: [
		s < 1008
			ifTrue: [^ 16r20 bitOr: (15 - ((s - 496) bitShift: -5))]
			ifFalse: [^ 16r10 bitOr: (15 - ((s - 1008) bitShift: -6))]].

	s < 4080
		ifTrue: [^ 15 - ((s - 2032) bitShift: -7)]
		ifFalse: [^ 0].
! !


!SampledSound class methodsFor: 'default sound' stamp: 'jm 9/17/97 12:49'!
defaultSampleTable: anArray
	"Set the sample table to be used as the default waveform for playing a score such as the Bach fugue. Array is assumed to contain monaural signed 16-bit sample values."

	DefaultSampleTable _ SoundBuffer fromArray: anArray.
! !

!SampledSound class methodsFor: 'default sound' stamp: 'jm 9/17/97 13:10'!
defaultSamples: anArray repeated: n

	| data |
	data _ WriteStream on: (SoundBuffer newMonoSampleCount: anArray size * n).
	n timesRepeat: [
		anArray do: [:sample | data nextPut: sample truncated]].
	DefaultSampleTable _ data contents.
! !

!SampledSound class methodsFor: 'default sound'!
nominalSamplePitch: aNumber
	"Record an estimate of the normal pitch of the sampled sound."

	NominalSamplePitch _ aNumber.
! !

!SampledSound class methodsFor: 'default sound' stamp: 'jm 5/8/1998 18:53'!
useCoffeeCupClink
	"Set the sample table to be used as the default waveform to the sound of a coffee cup being tapped with a spoon."
	"SampledSound useCoffeeCupClink bachFugue play"

	DefaultSampleTable _ self coffeeCupClink.
	NominalSamplePitch _ 400.
! !


!SampledSound class methodsFor: 'class initialization' stamp: 'jmv 6/16/2013 12:36'!
initialize
	"SampledSound initialize"

	SoundPlayer initialize.
	IncrementFractionBits _ 16.
	IncrementScaleFactor _ 2 raisedTo: IncrementFractionBits.
	ScaledIndexOverflow _ 2 raisedTo: 29.  "handle overflow before needing LargePositiveIntegers"
	self useCoffeeCupClink.
	SoundLibrary ifNil: [SoundLibrary _ Dictionary new].
	Beeper setDefault: (self new
						setSamples: self coffeeCupClink
						samplingRate: 12000).
! !


!SampledSound class methodsFor: 'WAV reading' stamp: 'ar 1/27/98 23:06'!
next16BitWord: msbFirst from: stream
	"Read a 16-bit positive integer from the input stream."
	"Assume: Stream has at least two bytes left."

	| n |
	n _ stream next: 2.
	^msbFirst
		ifTrue:[(n at: 1) * 256 + (n at: 2)]
		ifFalse:[(n at: 2) * 256 + (n at: 1)]
! !

!SampledSound class methodsFor: 'WAV reading' stamp: 'ar 1/27/98 23:06'!
next32BitWord: msbFirst from: stream
	"Read a 32-bit positive integer from the input stream."
	"Assume: Stream has at least four bytes left."

	| n |
	n _ stream next: 4.
	^msbFirst
		ifTrue:[(n at: 1) * 256 + (n at: 2) * 256 + (n at: 3) * 256 + (n at: 4)]
		ifFalse:[(n at: 4) * 256 + (n at: 3) * 256 + (n at: 2) * 256 + (n at: 1)]
! !

!SampledSound class methodsFor: 'WAV reading' stamp: 'jm 3/17/98 21:03'!
readWaveChunk: chunkType inRIFF: stream
	"Search the stream for a format chunk of the given type and return its contents."

	| id count |
	stream reset; binary.
	stream skip: 8.  "skip 'RIFF' and total length"
	id _ (stream next: 4) asString.  "contents type"
	id = 'WAVE' ifFalse: [^ ''].     "content type must be WAVE"

	"search for a chunk of the given type"
	[id _ (stream next: 4) asString.
	 count _ self next32BitWord: false from: stream.
	 id = chunkType] whileFalse: [
		"skip this chunk, rounding length up to a word boundary"
		stream skip: (count + 1 bitAnd: 16rFFFFFFFE).
		stream atEnd ifTrue: [^ '']].

	^ stream next: count  "return raw chunk data"
! !


!SampledSound class methodsFor: 'examples'!
immediateInspect
	"Answer a sampled sound from immediate data; inspect it."
	"StoredSound immediateInspect"

	| t |
	 t := Array new: 64000. 
	 0 to: 255 do: 
		[ :i | 1 to: 250 do:
			[ :j | t at: ((i * 250) + j) 
			put: (j / 250 * 32000 - 16000) asInteger]].
	(SampledSound fromData: t named: 'ramp' 
			rate: 32000 channels: 1 format: #lin16Bit) inspect! !

!SampledSound class methodsFor: 'examples'!
rmsViewExample
	"Read a sampled sound from the file, take the rms into 64 values, and edit that."
	"SampledSound rmsViewExample"

	| arr snd |
	arr := Array new: 128 withAll: 0.0.
	snd := SampledSound fileExample.
	snd sampleMaxFrom: 1 to: snd size into: arr.
	(Function from: arr) edit.! !

!SampledSound class methodsFor: 'examples'!
sweepExample
	"Open a sound view on a swept sine wave."
	"SampledSound sweepExample"

	^self sweepDur: 2.0 rate: 44100 from: 20 to: 100 chans: 1! !

!SampledSound class methodsFor: 'examples'!
sweepView
	"Open a sound view on a swept sine wave."
	"SampledSound sweepView"

	(SampledSound sweepDur: 2.0 rate: 44100 from: 20 to: 100 chans: 1) edit! !


!SampledSound class methodsFor: 'standard wave forms'!
constantOfDur: dur value: value rate: rate chans: chans
	"Answer a sound with a constant value."
	"SoundView openOn: (StoredSound constantOfDur: 1 value: 10000 rate: 16000 chans: 1)"

	| snd |
	snd := self duration: dur rate: rate channels: chans 
						format: self formatSymbol.
	1 to: (dur * rate * chans) truncated do: 
		[ :i | snd sampleAt: i put: value].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
expSweepDur: dur rate: rate from: start to: stop chans: chans
	"Answer a SampledSound with a swept sine wave."
	"SoundView openOn: (SampledSound expSweepDur: 1.0 rate: 44100 from: 20 to: 2000 chans: 1)"
	"SoundView openOn: (SampledSound expSweepDur: 1.0 
			rate: 44100 from: 20 to: 2000 chans: 1)"

	| scale1 snd incr |
	snd := self duration: dur rate: rate channels: chans format: self formatSymbol.
	scale1 := start asFloat / rate * Float pi * 2.
	incr := (stop / start) ln / (rate * dur) + 1.0.
	chans = 1
		ifTrue: [1 to: (dur * rate) truncated do: 
			[ :i | snd intSampleAt: i put: ((i - 1 * scale1) sin * 32767) truncated.
				scale1 := scale1 * incr]]
		ifFalse: [(1 to: ((dur * rate) truncated - 1) by: 2) do: 
			[ :i | | val |
				val := ((i - 1 * scale1) sin * 32767) truncated.
				snd intSampleAt: i put: val.
				snd intSampleAt: ( i + 1) put: val.
				scale1 := scale1 * incr]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
impulseOfDur: dur width: width rate: rate chans: chans
	"Create a sound with an impulse of the given characteristics."
	"SoundView openOn: (StoredSound impulseOfDur: 1 width: 0.001 rate: 16000 chans: 1)"
	"(SoundFile named: 'impulse.snd') saveSound: 
			(StoredSound impulseOfDur: 1 width: 0.001 
			rate: 16000)"

	| snd pulseLen nsamps samps |
	nsamps := (rate * dur * chans) truncated.
	samps := Array new: nsamps.
	pulseLen := (width * rate * chans) truncated.
	1 to: pulseLen do: 
		[ :i | samps at: i put: 32766].
	pulseLen  to: nsamps do: 
		[ :i | samps at: i put: 0].
	snd := SampledSound fromData: samps 
			rate: rate channels: chans format: self formatSymbol.
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
linearSweepDur: dur rate: rate from: start to: stop chans: chans
	"Answer a StoredSound with a swept sine wave."
	"SoundView openOn: (StoredSound linearSweepDur: 1.0 rate: 8000 from: 20 to: 2000 chans: 1)"

	| scale snd incr |
	snd := self duration: dur rate: rate channels: chans format: self formatSymbol.
	scale := start asFloat / rate * Float pi * 2.
	incr := ((stop asFloat / rate * Float pi * 2) - scale) / (rate * dur).
	chans = 1
		ifTrue: [1 to: (dur * rate) truncated do: 
			[ :i | snd intSampleAt: i put: ((i - 1 * scale) sin * 32767) truncated.
				scale := scale + incr]]
		ifFalse: [(1 to: (dur * rate) truncated by: 2) do: 
			[ :i | snd intSampleAt: i put: ((i - 1 * scale) sin * 32767) truncated.
				snd intSampleAt: ( i + 1) put: ((i - 1 * scale) sin * 32767) truncated.
				scale := scale + incr]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
pulseTrainDur: dur rate: rate freq: freq width: width chans: chans
	"Answer a StoredSound with a pulse train."
	"SoundView openOn: (StoredSound pulseTrainDur: 1.0 rate: 22050 freq: 10 
				width: 0.001 chans: 1 zero: 0)"
	"(StoredSound pulseTrainDur: 5.0 rate: 16000 freq: 250 width: 0.001 chans: 1) storeOnFileNamed: 'snd/new/square2.snd'"

	^self pulseTrainDur: dur rate: rate freq: freq width: width chans: chans zero: 0! !

!SampledSound class methodsFor: 'standard wave forms'!
pulseTrainDur: dur rate: rate freq: freq width: width chans: chans zero: zero
	"Answer a StoredSound with a pulse train."
	"SoundView openOn: (StoredSound pulseTrainDur: 1.0 rate: 22050 freq: 10 
				width: 0.001 chans: 1 zero: 0)"
	"(StoredSound pulseTrainDur: 5.0 rate: 16000 freq: 250 width: 0.001 chans: 1) storeOnFileNamed: 'snd/new/square2.snd'"

	| waveLen snd pSamps |
	snd := self duration: dur rate: rate channels: chans 
					format: self formatSymbol.
	waveLen := (rate / freq) truncated.
	pSamps := (rate * width) truncated.
	chans = 1
		ifTrue: [0 to: (dur * freq - 1) truncated do: 
			[ :i |1 to: pSamps do:
				[ :j | snd intSampleAt: ((i * waveLen + j) min: pSamps) put: 32767].
			pSamps + 1 to: waveLen do:
				[ :j | snd intSampleAt: (i * waveLen + j) put: zero]]]
		ifFalse: [(0 to: (dur * freq - 1) truncated by: 2) do: 
			[ :i |1 to: (pSamps * 2) do:
				[ :j | snd intSampleAt: (i * waveLen + j) put: 32767].
			(pSamps * 2 + 1) to: (waveLen * 2) do:
				[ :j | snd intSampleAt: (i * waveLen + j) put: zero]]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
rampDur: dur rate: rate chans: chans
	"Answer a StoredSound with a single ramp of samples."
	"SoundView openOn: (StoredSound rampDur: 2.0 rate: 16000 chans: 1)"

	| incr snd |
	snd := self duration: dur rate: rate channels: chans format: self formatSymbol.
	incr :=  65536.0 / (rate * dur) asFloat.
	snd size: (dur * rate * chans) truncated.
	chans = 1
		ifTrue: [1 to: snd size do: 
			[ :i | snd intSampleAt: i put: (-32768.0 + (i - 1 * incr)) truncated]]
	ifFalse: [(1 to: snd size by: 2) do: 
			[ :i | snd intSampleAt: i put: (-32768.0 + (i - 1 * incr)) truncated.
				snd intSampleAt: (i + 1) put: (-32768.0 + (i - 1 * incr)) truncated]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
sawtooth
	"Answer a StoredSound with raw sawtooth samples."
	"SampledSound sawtooth"
	"SoundView openOn: SampledSound sawtooth"

	^self sawtoothDur: 1.0 rate: 44100 freq: 50 chans: 1! !

!SampledSound class methodsFor: 'standard wave forms'!
sawtoothDur: dur rate: rate freq: freq chans: chans
	"Answer a StoredSound with raw sawtooth samples."
	"StoredSound sawtooth"
	"SoundView openOn: (SampledSound sawtoothDur: 1.0 rate: 22050 freq: 100 chans: 1)"

	| waveLen incr snd ind |
	snd := self duration: dur rate: rate channels: chans format: self formatSymbol.
	waveLen := (rate / freq) asFloat.
	incr :=  65536.0 / waveLen.
	chans = 1
		ifTrue: [1 to: (dur * rate) do: 
			[ :i | 
			ind := ((i \\ waveLen) asFloat) max: 1.0.
			snd intSampleAt: i put: (-32768.0 + (ind - 1.0 * incr))]]
		ifFalse: [(1 to: (dur * rate) truncated by: 2) do: 
			[ :i |
			ind := ((i \\ waveLen) asFloat) max: 1.0.
			snd intSampleAt: i put: (-32768.0 + (ind - 1.0 * incr)).
			snd intSampleAt: (i + 1) put: (-32768.0 + (ind - 1.0 * incr))]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
sineDur: dur rate: rate freq: freq chans: chans
	"Answer a StoredSound with a pulse train."
	"SoundView openOn: (StoredSound sineDur: 0.5 rate: 22050 freq: 200 chans: 1)"
	"(StoredSound sineDur: 5 rate: 16000 freq: 800 chans: 1) storeOnFileNamed: 'snd/new/sine.snd'"

	| scale snd |
	snd := self duration: dur rate: rate channels: chans format: self formatSymbol.
	scale := freq / rate * Float pi * 2.
	chans = 1
		ifTrue: [1 to: (dur * rate) truncated do: 
			[ :i | snd intSampleAt: i put: ((i - 1 * scale) sin * 32767) truncated]]
		ifFalse: [(1 to: (dur * rate) truncated by: 2) do: 
			[ :i | snd intSampleAt: i put: ((i - 1 * scale) sin * 32767) truncated.
				snd intSampleAt: ( i + 1) put: ((i - 1 * scale) sin * 32767) truncated]].
	^snd! !

!SampledSound class methodsFor: 'standard wave forms'!
squareDur: dur rate: rate freq: freq chans: chans
	"Answer a StoredSound with a square wave."
	"SoundView openOn: (StoredSound squareDur: 1.0 rate: 1000 freq: 20 chans: 1)"

	^self pulseTrainDur: dur rate: rate freq: freq width: (0.5 / freq) chans: chans zero: -32767! !

!SampledSound class methodsFor: 'standard wave forms'!
sweepDur: dur rate: rate from: start to: stop chans: chans
	"Answer a StoredSound with a swept sine wave."
	"SoundView openOn: (SampledSound sweepDur: 0.5 rate: 8000 from: 20 to: 500 chans: 1)"
	"SoundView openOn: (FloatSound sweepDur: 0.5 rate: 8000 from: 20 to: 500 chans: 1)"
	"(SampledSound sweepDur: 2.0 rate: 44100 from: 50 to: 500 chans: 1) 
				storeOnFileNamed: 'snd/new/sweep.snd'"

	^self expSweepDur: dur rate: rate from: start to: stop chans: chans! !


!SampledSound class methodsFor: 'class constants'!
formatSymbol
	"Answer the symbolic code used for the format of the receiver's instances."

	^#lin16Bit! !

!SampledSound class methodsFor: 'class constants'!
maxSample
	"Answer the maximum value of the receiver class."

	^32767! !

!SampledSound class methodsFor: 'class constants'!
minSample
	"Answer the minimum value of the receiver class."

	^-32768! !


!classDefinition: #FloatSound category: 'Music-Sound'!
SampledSound subclass: #FloatSound
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!FloatSound commentStamp: '<historical>' prior: 0!
Instances of FloatSound are used for sounds with 32-bit floating-point numbers as samples.
There are behaviors for mapping into other formats.!


!FloatSound methodsFor: 'accessing'!
format
	"Return the receiver's format--a symbol constant."

	^#float32Bit! !

!FloatSound methodsFor: 'accessing'!
sampleSize
	"Answer the size in bytes of the receiver's samples (4)."

	^4! !


!FloatSound methodsFor: 'private'!
privateSampleAt: ind
	"Answer the sample at the given index."

	^data floatAt: (ind - 1 * 4 + 1)! !

!FloatSound methodsFor: 'private'!
privateSampleAt: ind put: aValue
	"Stuff the given sample into the data array at the given index (no checking)."

	data floatAt: (ind - 1 * 4 + 1) put: aValue asFloat! !


!FloatSound methodsFor: 'sample accessing'!
intSampleAt: ind
	"Answer the sample at the given index as a scaled 16-bit integer."

	^((self sampleAt: ind) * 32767.0) rounded! !

!FloatSound methodsFor: 'sample accessing'!
intSampleAt: ind put: aValue
	"Put the given integer sample at the given index after scaling."

	self sampleAt: ind put: (aValue asFloat / 32767.0)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'FloatSound class' category: 'Music-Sound'!
FloatSound class
	instanceVariableNames: ''!

!FloatSound class methodsFor: 'class constants'!
formatSymbol
	"Answer the symbolic code used for the format of the receiver's instances."

	^#float32Bit! !

!FloatSound class methodsFor: 'class constants'!
maxSample
	"Answer the maximum value of the receiver class."

	^1.0! !

!FloatSound class methodsFor: 'class constants'!
minSample
	"Answer the minimum value of the receiver class."

	^-1.0! !


!FloatSound class methodsFor: 'examples'!
fileExample
	"Answer a typical float sound read in from a file."
	"SoundView openOn: FloatSound fileExample"

	^FloatSound fromFile: 'snd/new/orchestra.f.snd'! !


!classDefinition: #VirtualSound category: 'Music-Sound'!
SampledSound subclass: #VirtualSound
	instanceVariableNames: 'source'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!VirtualSound commentStamp: '<historical>' prior: 0!
An instance of VirtualSound is a "reference" to another sound, typically by a named cue list entry.

Instance Variables:
	source	<StoredSound> the sound to which the VirtualSound points!


!VirtualSound methodsFor: 'accessing'!
cue: cueName
	"Set the cue name of the receiver"

	properties at: #cue put: cueName.
	domain := source cueRegionNamed: cueName.
	duration := SecondDuration value: (self size / self rate) asFloat.
	self name: self name, '.', cueName! !

!VirtualSound methodsFor: 'accessing'!
source
	"Answer the receiver's source sound."

	^source! !

!VirtualSound methodsFor: 'accessing'!
source: aSound
	"Set the receiver's source sound."

	source := aSound.
	self rate: source rate.
	self channels: source channels.
	self name: source name.
	self domain: aSound domain! !

!VirtualSound methodsFor: 'accessing'!
start: start
	"Set the argument as the starting sample of the receiver."

	properties at: #start put: start.
	(properties includesKey: #stop)
		ifTrue: [self size: (properties at: #stop) - start].! !

!VirtualSound methodsFor: 'accessing'!
stop: stop
	"Set the argument as the ending sample of the receiver."

	properties at: #stop put: stop.
	(properties includesKey: #start)
		ifTrue: [self size: stop - (properties at: #start)].! !


!VirtualSound methodsFor: 'sample accessing'!
sampleAt: ind put: aValue
	"Stuff the given sample at the given index **after transforming into a 'real' sound**."

	self getReal.
	^super sampleAt: ind put: aValue! !


!VirtualSound methodsFor: 'private'!
getReal
	"Answer a 'real sound' based on the receiver."

	| newSound |
	newSound := self copyAllButSamples.
	1 to: self size do:
		[ :ind |
		newSound sampleAt: ind put: (self sampleAt: ind)].
	self become: newSound! !

!VirtualSound methodsFor: 'private'!
mapSampleIndex: ind
	"Map the given sample index according to the 'page' (firstIndex) of the source and the receiver's relative offset."

	^(source mapSampleIndex: ind) + firstIndex! !

!VirtualSound methodsFor: 'private'!
realSound
	"Answer the real sound for the receiver."

	^source! !


!VirtualSound methodsFor: 'testing'!
isVirtual
	"Answer whether the receiver is a virtual sound."

	^true! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'VirtualSound class' category: 'Music-Sound'!
VirtualSound class
	instanceVariableNames: ''!

!VirtualSound class methodsFor: 'instance creation'!
from: source cue: cName
	"Answer a VirtualSound derived from the argument and named cue region."
	"VirtualSound exampleView"

	| me |
	me := self new initialize.
	me source: source.
	me cue: cName.
	^me! !


!VirtualSound class methodsFor: 'examples'!
exampleView
	"Open a sound view on a ramp with a chunk cut out of it."
	"VirtualSound exampleView"

	SoundView openOn: (VirtualSound from: 
			(SampledSound fromFile: 'snd/AllGatesAreOpen/nml1.8e.snd') 
				cue: #stream3)! !


!classDefinition: #CompositeSound category: 'Music-Sound'!
VirtualSound subclass: #CompositeSound
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!CompositeSound commentStamp: '<historical>' prior: 0!
An instance of CompositeSound can be used to represent a sound constructed by "splicing together" sections of other sounds.
It uses its components collection to maintain sounds and sample ranges that have been pasted together.
It responds to sampleAt: and sampleAt:put: just like other sounds, but stores only its components list on files unless explicitly told to store samples.

Instance Variables:
	components <SortedCollection of ComponentSounds> the "sub-sounds" of the CompositeSound!


!CompositeSound methodsFor: 'sample accessing'!
sampleAt: ind
	"Answer the sample at the given index, mapping and paging as necessary."

	| q |
	1 to: components size do:
		[ :in2 | 
		q := components at: in2.
		(q includes: ind)
			ifTrue: [^q sound sampleAt: (ind - q start + q offset)]].
	self error: 'No sample by that name, Sir.'! !


!CompositeSound methodsFor: 'private'!
rippleUpFrom: start adding: insert
	"Shift all the cues above the given one up by the given insert count."

	| item |
	start to: components size do:
		[ :ind | 
		item := (components at: ind).
		item start: (item start + insert).
		item stop: (item stop + insert)].! !


!CompositeSound methodsFor: 'accessing'!
on: aSound
	"Set the receiver's source sound."

	components := SortedCollection sortBlock: [ :x :y | x start <= y start].
	components add: (ComponentSound on: aSound start: 1 stop: aSound size).
	self rate: aSound rate.
	self channels: aSound channels.
	self size: aSound size.
	properties := aSound properties copy! !

!CompositeSound methodsFor: 'accessing'!
paste: sound from: start to: stop at: ind
	"Paste the designated section into the receiver."

	| newItem item number extent oldStop |
	extent := stop - start.
	newItem := ComponentSound on: sound 
				start: ind stop: stop - start + ind offset: start.
	ind >= self size						"if pasting onto the end of the sound"
		ifTrue: [components add: newItem.
				^self].
	number := 1.							" else locate where to put it"
	[(number > components size) | ((components at: number) includes: ind)] 
		whileFalse: [number := number + 1].
	item := components at: number.
	ind = item stop						"if we're pasting at the end of another splice"
		ifTrue: [self rippleUpFrom: (number + 1) adding: extent.
			components add: newItem.
			^self].
	ind = item start						"if we're pasting at the start of another splice"
		ifTrue: [self rippleUpFrom: number adding: extent.
			components add: newItem.
			^self].
											"else we have to break up an existing component"
	self rippleUpFrom: (number + 1) adding: extent.
	oldStop := item stop.
	item stop: ind - item start.			"edit the old entry in place"
	components add: newItem.			"insert the new item"
								"add a copy of the old item with new interval and offset"
	components add: (ComponentSound on: item sound 
			start: (ind + extent + 1) stop: (oldStop + extent) 
			offset: (item offset + (ind - item start) + extent)).
	^self! !


!CompositeSound methodsFor: 'testing'!
isComposite
	"Answer whether the receiver is a composite sound."

	^true! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'CompositeSound class' category: 'Music-Sound'!
CompositeSound class
	instanceVariableNames: ''!

!CompositeSound class methodsFor: 'instance creation'!
on: source paste: newSound from: start to: stop at: ind
	"Answer a CompositeSound derived from the argument pasting in the given sound at the given index."
	"CompositeSound exampleView"

	| me |
	me := self new initialize.
	me on: source.
	me paste: newSound from: start to: stop at: ind.
	^me! !


!CompositeSound class methodsFor: 'examples'!
exampleView
	"Open a sound view on a ramp with a chunk of a sine pasted into it."
	"CompositeSound exampleView"

	| ramp sine |
	ramp := SampledSound rampDur: 1.0 rate: 8000 chans: 1.
	sine := SampledSound sineDur: 0.5 rate: 8000 freq: 400 chans: 1.
	SoundView openOn: (ramp paste: sine from: 200 to: 400 at: 1000)! !


!classDefinition: #GapSound category: 'Music-Sound'!
VirtualSound subclass: #GapSound
	instanceVariableNames: 'cutList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!
!GapSound commentStamp: '<historical>' prior: 0!
An instance of GapSound can be used to represent a sound with samples deleted from it (i.e., a gap).
It uses its cut list instance variable to maintain sample ranges that have been deleted from the sound that comprises it (the source).
It responds to sampleAt: and sampleAt:put: just like other sounds, but stores only its cut list on files unless explicitly told to store samples.

Instance Variables:
	cutList		<SortedCollection of Integers> the list of deleted sections by sample index i.e., pointers denoting sections that have been deleted. For example, having a cutList of (1000 2000) means that samples (1000, 2000] have been virtually cut.
!


!GapSound methodsFor: 'private'!
mapSampleIndex: ind
	"Map the given sample index according to the cut list (i.e., deleted sections) of the receiver."

	| mappedIndex |
	((cutList == nil) or: [cutList size < 2])
		ifTrue: [^ind - firstIndex].
	mappedIndex := ind.
	mappedIndex < cutList first
		ifTrue: [^mappedIndex].
	1 to: cutList size by: 2 do:
		[ :cutIndex |
		mappedIndex := mappedIndex - 
				((cutList at: cutIndex) - (cutList at: cutIndex + 1)).
		(((cutIndex + 1) = cutList size) or: 
				[mappedIndex < (cutList at: cutIndex + 2)])
			ifTrue: [^mappedIndex]].
		^mappedIndex - firstIndex.! !


!GapSound methodsFor: 'testing'!
hasGaps
	"Answer whether the receiver has any gaps."

	^true! !


!GapSound methodsFor: 'accessing'!
allocateData
	"no-op"

	^self! !

!GapSound methodsFor: 'accessing'!
cutFrom: startSample to: stopSample
	"Cut the designated section from the receiver."

	| realStart realStop |
	realStart := self mapSampleIndex: startSample.
	realStop := self mapSampleIndex: stopSample.
	cutList == nil
		ifTrue: [cutList := SortedCollection new].
	cutList add: realStart.
	cutList add: realStop.
	self size: (self size - (stopSample - startSample)).
	^self! !

!GapSound methodsFor: 'accessing'!
samples

	^source samples! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'GapSound class' category: 'Music-Sound'!
GapSound class
	instanceVariableNames: ''!

!GapSound class methodsFor: 'instance creation'!
on: source cutFrom: start to: stop
	"Answer a CompositeSound derived from the argument missing the given range."
	"GapSound exampleView"

	| me |
	me := self new initialize.
	me source: source.
	me samplesInMemory: source samplesInMemory.
	me data: source data.
	me cutFrom: start to: stop.
	^me! !


!GapSound class methodsFor: 'examples'!
exampleView
	"Open a sound view on a ramp with a chunk cut out of it."
	"GapSound exampleView"

	SoundView openOn: ((SampledSound rampDur: 1.0 rate: 16000 chans: 1) 
			cutFrom: 1000 to: 10000)! !

SampledSound initialize!