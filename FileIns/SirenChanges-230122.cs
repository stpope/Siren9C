'From Squeak6.1alpha of 18 January 2023 [latest update: #22377] on 23 January 2023 at 1:12 pm'!
	"Answer a String whose characters are a description of the receiver."
	"5.2376923461109 printString: 4 "
	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream maxDecimalPlaces: digits.
	^aStream contents! !