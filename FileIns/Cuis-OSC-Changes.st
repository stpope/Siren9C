'From Cuis 6.0 [latest update: #5625] on 9 January 2023 at 9:59:59 am'!

!Boolean methodsFor: 'printing'!
toOSCBytes: aConvertor
	^self! !

!Integer methodsFor: 'printing' stamp: 'stp 1/7/2023 22:05:46'!
toOSCBytes: aConvertor 
	"pad beginning with 0's"

	aConvertor next: 4 - (self numberOfDigitsInBase: 10) put: 0.
	(self numberOfDigitsInBase: 10)
		to: 1
		by: -1
		do: [:i | aConvertor nextPut: (self digitAt: i)]! !

!Float methodsFor: 'converting' stamp: 'stp 1/8/2023 17:16:22'!
toOSCTypeOn: aConvertor 
 
	aConvertor nextPutType: $f asCharacter.
	^$f asCharacter! !

!Float methodsFor: 'converting' stamp: 'stp 1/7/2023 22:03:16'!
toOSCBytes: aConvertor 
	"Store a float in the proper byte-endian order"
	"(1.5 asIEEE32BitWord bitShift: -16) bitAnd: 16rFF"
	
	| int |
	int := self asIEEE32BitWord.	Smalltalk isBigEndian		ifTrue: [aConvertor nextPut: (int bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF)]		ifFalse: [aConvertor nextPut: ((int bitShift: -24) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -16) bitAnd: 16rFF).			aConvertor nextPut: ((int bitShift: -8) bitAnd: 16rFF).			aConvertor nextPut: (int bitAnd: 16rFF)]! !

!False methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:06'!
toOSCTypeOn: aConvertor

	aConvertor nextPutType: $F asCharacter.
	^$F asCharacter! !

!UndefinedObject methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:30'!
toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $N asCharacter.
	^$N asCharacter! !

!UndefinedObject methodsFor: 'printing'!
toOSCBytes: aConvertor
	^self! !

!True methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:17'!
toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $T asCharacter.
	^$T asCharacter! !

!Symbol methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:57'!
toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s asCharacter.
	^$s asCharacter! !

!String methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:52'!
toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $s asCharacter.
	^$s asCharacter! !

!String methodsFor: 'printing'!
toOSCBytes: aConvertor 
	self isEmpty ifTrue: [^self].
	aConvertor nextPutAll: self asByteArray.
				"Align to 4 byte boundry"
	4 - (self size \\ 4) timesRepeat: [aConvertor nextPut: 0]! !

!SmallInteger methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:26'!
toOSCTypeOn: aConvertor 
	aConvertor nextPutType: $i asCharacter.
	^$i asCharacter! !

!SequenceableCollection methodsFor: 'printing' stamp: 'stp 1/8/2023 17:16:46'!
toOSCTypeOn: aConvertor 
	| coll |
	aConvertor nextPut: $[ asCharacter.
	coll _ self collect: [:each | each toOSCTypeOn: aConvertor].
	aConvertor nextPut: $] asCharacter.
	^coll! !

!SequenceableCollection methodsFor: 'printing'!
toOSCBytes: aConvertor 
	self do: [:each | each toOSCBytes: aConvertor]! !
