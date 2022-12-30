'From Cuis 6.0 [latest update: #5569] on 2 December 2022 at 1:46:49 am'!
!classDefinition: #ZPoint category: #'Graphics-Primitives'!
Point subclass: #ZPoint
	instanceVariableNames: 'z'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
!ZPoint commentStamp: '<historical>' prior: 0!
ZPoints define a third instance variable (z) in addition to Point's x and y to hold 3-dimensional point values.!


!ZPoint methodsFor: 'testing'!
= aPoint
	^super = aPoint and: [z = aPoint z]! !

!ZPoint methodsFor: 'testing'!
hash
	^super hash bitXor: z hash! !

!ZPoint methodsFor: 'testing'!
isZero
	^super isZero and: [z isZero]! !


!ZPoint methodsFor: 'converting'!
asPoint
	^x@y! !


!ZPoint methodsFor: 'printing'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: '@'.
	z printOn: aStream! !

!ZPoint methodsFor: 'printing'!
storeOn: aStream
	"copied & modified from class Point"

	aStream nextPut: $(;
	nextPutAll: self species name;
	nextPutAll: ' x: ';
	store: x;
	nextPutAll: ' y: ';
	store: y;
	nextPutAll: ' z: ';
	store: z;
	nextPut: $).! !


!ZPoint methodsFor: 'accessing'!
z
	^z! !

!ZPoint methodsFor: 'accessing'!
z: aNumber
	z := aNumber! !


!ZPoint methodsFor: 'private'!
convertToFloats
	x := x asFloat.
	y := y asFloat.
	z := z asFloat! !

!ZPoint methodsFor: 'private'!
setX: xValue setY: yValue setZ: zValue
	x := xValue.
	y := yValue.
	z := zValue! !


!ZPoint methodsFor: 'point functions'!
dotProduct: aZPoint
	| temp |
	temp := self * aZPoint.
	^temp x abs + temp y abs + temp z abs! !


!ZPoint methodsFor: 'arithmetic'!
* aZPointOrNumber 
	"only works if the argument is a ZPoint or Number"

	| scalePoint |
	scalePoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x * scalePoint x
		y: y * scalePoint y
		z: z * scalePoint z! !

!ZPoint methodsFor: 'arithmetic'!
+ aZPointOrNumber 
	"Answer a new ZPoint that is the sum of the receiver and delta 
	(which is a ZPoint  
	or Number)."

	| deltaPoint |
	deltaPoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x + deltaPoint x
		y: y + deltaPoint y
		z: z + deltaPoint z! !

!ZPoint methodsFor: 'arithmetic'!
- aZPointOrNumber 
	"Answer a new ZPoint that is the sum of the receiver and delta 
	(which is a ZPoint  
	or Number)."

	| deltaPoint |
	deltaPoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x - deltaPoint x
		y: y - deltaPoint y
		z: z - deltaPoint z! !

!ZPoint methodsFor: 'arithmetic'!
/ aZPointOrNumber 
	"only works if the argument is a ZPoint or Number"

	| scalePoint |
	scalePoint := (aZPointOrNumber isMemberOf: ZPoint)
				ifTrue: [aZPointOrNumber]
				ifFalse: [ZPoint
						x: aZPointOrNumber
						y: aZPointOrNumber
						z: aZPointOrNumber].
	^ZPoint
		x: x / scalePoint x
		y: y / scalePoint y
		z: z / scalePoint z! !

!ZPoint methodsFor: 'arithmetic'!
rounded
	^self class basicNew
		setX: x rounded
		setY: y rounded
		setZ: z rounded! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'ZPoint class' category: #'Graphics-Primitives'!
ZPoint class
	instanceVariableNames: ''!

!ZPoint class methodsFor: 'instance creation'!
x: xValue y: yValue z: zValue
	^self basicNew setX: xValue setY: yValue setZ: zValue! !
