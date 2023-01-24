'From Cuis 6.0 [latest update: #5625] on 7 January 2023 at 12:14:17 pm'!

!#Debugger methodsFor: 'shout styling' stamp: 'stp 1/7/2023 12:12:48'!
hasBindingOf: aString
	self selectedContext ifNil: [^false].
	^self selectedContext tempNames includes: aString! !


!#Debugger methodsFor: 'shout styling' stamp: 'stp 1/7/2023 12:13:19'!
bindingNamesDo: aBlock

	self selectedContext ifNil: [^self].
	self selectedContext tempNames do: aBlock! !

