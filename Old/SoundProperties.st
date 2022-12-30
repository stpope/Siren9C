

!SoundProperties methodsFor: 'accessing'!

amplShape
	"Answer the receiver's 'amplShape'."

	^amplShape!

amplShape: anObject
	"Set the receiver's instance variable 'amplShape' to be anObject."

	amplShape := anObject!

attack
	"Answer the receiver's 'attack'."

	^attack!

attack: anObject
	"Set the receiver's instance variable 'attack' to be anObject."

	attack := anObject!

chans
	"Answer the receiver's 'chans'."

	^chans!

chans: anObject
	"Set the receiver's instance variable 'chans' to be anObject."

	chans := anObject!

decay
	"Answer the receiver's 'decay'."

	^decay!

decay: anObject
	"Set the receiver's instance variable 'decay' to be anObject."

	decay := anObject!

dur
	"Answer the receiver's 'dur'."

	^dur!

dur: anObject
	"Set the receiver's instance variable 'dur' to be anObject."

	dur := anObject!

envelope
	"Answer the receiver's 'envelope'."

	^envelope!

envelope: anObject
	"Set the receiver's instance variable 'envelope' to be anObject."

	envelope := anObject!

folder
	"Answer the receiver's 'folder'."

	^folder!

folder: anObject
	"Set the receiver's instance variable 'folder' to be anObject."

	folder := anObject!

format
	"Answer the receiver's 'format'."

	^format!

format: anObject
	"Set the receiver's instance variable 'format' to be anObject."

	format := anObject!

harmonicity
	"Answer the receiver's 'harmonicity'."

	^harmonicity!

harmonicity: anObject
	"Set the receiver's instance variable 'harmonicity' to be anObject."

	harmonicity := anObject!

length
	"Answer the receiver's 'length'."

	^length!

length: anObject
	"Set the receiver's instance variable 'length' to be anObject."

	length := anObject!

maxAmpl
	"Answer the receiver's 'maxAmpl'."

	^maxAmpl!

maxAmpl: anObject
	"Set the receiver's instance variable 'maxAmpl' to be anObject."

	maxAmpl := anObject!

name
	"Answer the receiver's 'name'."

	^name!

name: anObject
	"Set the receiver's instance variable 'name' to be anObject."

	name := anObject!

pitch
	"Answer the receiver's 'pitch'."

	^pitch!

pitch: anObject
	"Set the receiver's instance variable 'pitch' to be anObject."

	pitch := anObject!

rate
	"Answer the receiver's 'rate'."

	^rate!

rate: anObject
	"Set the receiver's instance variable 'rate' to be anObject."

	rate := anObject!

rmsAmpl
	"Answer the receiver's 'rmsAmpl'."

	^rmsAmpl!

rmsAmpl: anObject
	"Set the receiver's instance variable 'rmsAmpl' to be anObject."

	rmsAmpl := anObject!

spectralCentroid
	"Answer the receiver's 'spectralCentroid'."

	^spectralCentroid!

spectralCentroid: anObject
	"Set the receiver's instance variable 'spectralCentroid' to be anObject."

	spectralCentroid := anObject!

spectralShape
	"Answer the receiver's 'spectralShape'."

	^spectralShape!

spectralShape: anObject
	"Set the receiver's instance variable 'spectralShape' to be anObject."

	spectralShape := anObject!

spectrum
	"Answer the receiver's 'spectrum'."

	^spectrum!

spectrum: anObject
	"Set the receiver's instance variable 'spectrum' to be anObject."

	spectrum := anObject!

start
	"Answer the receiver's 'start'."

	^start!

start: anObject
	"Set the receiver's instance variable 'start' to be anObject."

	start := anObject!

steadyState
	"Answer the receiver's 'steadyState'."

	^steadyState!

steadyState: anObject
	"Set the receiver's instance variable 'steadyState' to be anObject."

	steadyState := anObject!

stop
	"Answer the receiver's 'stop'."

	^stop!

stop: anObject
	"Set the receiver's instance variable 'stop' to be anObject."

	stop := anObject! !

!SoundProperties methodsFor: 'printing'!

printOn: aStream

	#(name dur maxAmpl rmsAmpl spectralCentroid) do:
		[ :prop |
		aStream nextPutAll: (prop asString, ':  	', (self perform: prop) printString); cr]! !
