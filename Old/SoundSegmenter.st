

Object subclass: #SoundSegmenter
	instanceVariableNames: 'sound steadyState segments envelope pitch spectrum '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Music-Sound'!

!SoundSegmenter methodsFor: 'accessing'!

envelope

	^envelope!

envelope: aValue

	envelope := aValue!

pitch

	^pitch!

pitch: aValue

	pitch := aValue!

segments

	^segments!

segments: aValue

	segments := aValue!

sound

	^sound!

sound: aValue

	sound := aValue!

spectrum

	^spectrum!

spectrum: aValue

	spectrum := aValue! !

!SoundSegmenter methodsFor: 'envelope'!

computeEnvelope
	"Get the RMS envelope of the receiver's sound."

	sound envelope == nil
		ifTrue: [sound computeRMSEnvelope].
	envelope := sound envelope!

findLocaMinimalSegmentsLongerThan: lenThresh
	"Look for regions and local minima in the envelope."

	| v1 v2 cues removals sign scale nam |
	scale := sound size / envelope size.
"Accumulate local max/minima"
	v1 := envelope at: 1.
	v2 := envelope at: 2.
	cues := OrderedCollection new.
	removals := OrderedCollection new.
	cues add: 0.
	sign := (v2 - v1) sign.
	3 to: envelope size do:
		[ :ind |
		v2 := envelope at: ind.
		(v2 - v1) sign == sign
			ifFalse: [cues add: ind.
				sign := (v2 - v1) sign.
				v1 := v2]].
	cues add: envelope size.
"Delete any segments shorter than the threshold"
	2 to: cues size do:
		[ :ind |
		v1 := cues at: (ind - 1).
		v2 := cues at: ind.
		(v2 - v1) < lenThresh
			ifTrue: [((envelope at: v1) < (envelope at: v2))
				ifTrue: [removals add: v1]
				ifFalse: [removals add: v2]]].
	removals do: [ :val | cues remove: val ifAbsent: []].
"Set the cues into the receiver's sound"
	2 to: cues size do:
		[ :ind |
		v1 := ((cues at: (ind - 1)) * scale) max: 1.
		v2 := (cues at: ind) * scale.
		nam := 'cue', (ind - 1) printString.
		sound addCueNamed: nam from: v1 rounded to: v2 rounded]!

findSilence: silenceThresh longerThan: lenThresh
	"Identify silent segments"

	| index count v1 v2 nam scale |
	index := 1.
	count := 1.
	scale := sound size / envelope size.
	[index < envelope size] whileTrue:
		[ | start stop |
		v1 := envelope at: index.
		v1 < silenceThresh
			ifTrue: [start := index.
					[index := index + 1.
					((index < envelope size) and: [(envelope at: index) < silenceThresh])] whileTrue.
				stop := index.
				(stop - start) > lenThresh
					ifTrue: [v1 := (start - 1 * scale) max: 1.
						v2 := stop - 1 * scale.
						nam := 'silence', count printString.
						count := count + 1.
						sound addCueNamed: nam from: v1 rounded to: v2 rounded]]
				ifFalse: [index := index + 1]].!

rmsSegmentation
	"Look for regions and local minima in the envelope."

	| lenThresh  silenceThresh |
	silenceThresh := 0.005 * sound class maxSample.		"RMS window declared 'silent' "
	lenThresh := 16.				"# of frames for minimum segment (128 frames/sec)"

	^self rmsSegmentation: silenceThresh lenThresh: lenThresh!

rmsSegmentation: silenceThresh lenThresh: lenThresh
	"Look for regions and local minima in the envelope."

"Identify silent segments"
	self findSilence: silenceThresh longerThan: lenThresh.

"Accumulate local max/minima"
	self findLocaMinimalSegmentsLongerThan: lenThresh! !

!SoundSegmenter methodsFor: 'spectrum'!

findSpectralDiscontinuitiesIn: snd deltaThresh: dThresh lenThresh: lThresh
	"Take the spectrum of the sound and segment it according to spectral changes greater than dThresh"

	| n data fft spect bands |
	n := 1024.
	fft := FFT new: n.
	data := (snd channels = 1
		ifTrue: [snd samples]
		ifFalse: [((0 to: n - 1) collect:
			[ :i | (((snd intSampleAt: (i * 2)) 
				+ (snd intSampleAt: (i * 2) + 1)) / 2) rounded])]).
	fft realData: data.
	Cursor execute showWhile: [fft transformForward: true].
	spect := (1 to: n // 2) collect:
		[ :i | ((fft realData at: i) squared + (fft imagData at: i) squared) sqrt].
	bands := spect reduce toBands: 8.
	1 to: spect size do:
		[ :i |
"
				(stop - start) > lenThresh
					ifTrue: [v1 := (start - 1 * scale) max: 1.
						v2 := stop - 1 * scale.
						nam := 'silence', count printString.
						count := count + 1.
						sound addCueNamed: nam from: v1 rounded to: v2 rounded]]

"
		].


!

findSpectralDiscontinuitiesIn: snd deltaThresh: dThresh lenThresh: lThresh n: n bands: nBands
	"Take the spectrum of the sound and segment it according to spectral changes greater than dThresh"

	| spect |
	spect := Spectrum fromSound: snd N: n.
	spect displayAt: 64@64.
	spect reduceToOctaves.
	spect displayAt: 64@500.
self halt.
	1 to: spect size do:
		[ :i |
"
				(stop - start) > lenThresh
					ifTrue: [v1 := (start - 1 * scale) max: 1.
						v2 := stop - 1 * scale.
						nam := 'silence', count printString.
						count := count + 1.
						sound addCueNamed: nam from: v1 rounded to: v2 rounded]]

"
		].


! !

SoundSegmenter class
	instanceVariableNames: ''!

!SoundSegmenter class methodsFor: 'examples'!

example
	"SoundSegmenter example"

	| snd inst |
	snd := StoredSound fromFile: '/snd/truth/06.ice.1.snd'.
	inst := self new.
	inst sound: snd.
	inst computeEnvelope.
	^inst!

findSilenceIn: snd
	"Process the given sound."

	| inst |
	inst := self new.
	inst sound: snd.
	inst computeEnvelope.
	inst rmsSegmentation: 100 lenThresh: 16.
	^inst!

findSilenceIn: snd silenceThresh: silenceThresh lenThresh: lenThresh
	"Process the given sound."

	| inst |
	inst := self new.
	inst sound: snd.
	inst computeEnvelope.
	inst rmsSegmentation: silenceThresh lenThresh: lenThresh.
	^inst!

processFile: nam
	"Load and process the given sound file, answering it"

	| snd inst |
	snd := StoredSound fromFile: nam.
	inst := self new.
	inst sound: snd.
	inst computeEnvelope.
	inst rmsSegmentation.
	^snd!

processSound: snd
	"Process the given sound."

	| inst |
	inst := self new.
	inst sound: snd.
	inst computeEnvelope.
	inst rmsSegmentation.
! !
