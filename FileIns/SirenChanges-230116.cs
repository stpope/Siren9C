'From Squeak6.0 of 9 January 2023 [latest update: #22120] on 17 January 2023 at 7:11:05 am'!
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream fractionDigits: digits.
	^aStream contents! !
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil! !