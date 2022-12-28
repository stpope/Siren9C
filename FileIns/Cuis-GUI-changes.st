'From Cuis 6.0 [latest update: #5115] on 1 May 2022 at 11:35:52 am'!

!Theme methodsFor: 'other options' stamp: 'STP 5/1/2022 11:34:57'!
scrollbarShowButtons
	^ false! !

'From Cuis 6.0 [latest update: #5115] on 3 May 2022 at 4:48:32 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'stp 5/3/2022 15:17:49'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."

	^Preferences standardListFont pointSize - 7! !


'From Cuis 6.0 [latest update: #5115] on 3 May 2022 at 3:29:01 pm'!

!PreferenceSet methodsFor: 'bigger and smaller GUI' stamp: 'STP 4/26/2022 16:51:11'!
scrollbarSide
	"Answer #left or #right for vertical scroll bar placement."
	^ #left! !

!PreferenceSet methodsFor: 'bigger and smaller GUI' stamp: 'STP 5/1/2022 11:20:38'!
scrollbarButtons
	"Answer true or false scroll bar up/down buttons."
	^ false! !

!ScrollBar methodsFor: 'initialization' stamp: 'STP 5/1/2022 11:32:42'!
updateUpButton: aPluggableButtonMorph
	"Update the argument as an upButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawUpIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded.
	aPluggableButtonMorph visible: Preferences scrollbarButtons! !

!ScrollBar methodsFor: 'initialization' stamp: 'STP 5/1/2022 11:32:35'!
updateRightButton: aPluggableButtonMorph
	"Update the argument as a rightButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawRightIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded.
	aPluggableButtonMorph visible: Preferences scrollbarButtons! !

!ScrollBar methodsFor: 'initialization' stamp: 'STP 5/1/2022 11:32:26'!
updateLeftButton: aPluggableButtonMorph
	"Update the argument as a leftButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawLeftIcon;
		action: #scrollUp;
		roundButtonStyle: false;
		redrawNeeded.
	aPluggableButtonMorph visible: Preferences scrollbarButtons! !

!ScrollBar methodsFor: 'initialization' stamp: 'STP 5/1/2022 11:32:13'!
updateDownButton: aPluggableButtonMorph
	"Update the argument as a downButton."

	aPluggableButtonMorph
		iconDrawSelector: #drawDownIcon;
		action: #scrollDown;
		roundButtonStyle: false;
		redrawNeeded.
	aPluggableButtonMorph visible: Preferences scrollbarButtons! !

!ScrollBar methodsFor: 'geometry' stamp: 'STP 5/1/2022 11:23:48'!
buttonExtent

	^Preferences scrollbarButtons
		ifTrue: [self class scrollbarThickness]
		ifFalse: [0]! !


!ScrollBar methodsFor: 'initialization' stamp: 'stp 5/3/2022 16:04:17'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ PluggableButtonMorph new.
	upButton model: self.
	upButton morphExtent: e@e.
	Preferences scrollbarButtons 
		ifTrue: [self addMorph: upButton position: borderWidth@borderWidth].
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'stp 5/3/2022 16:04:35'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ PluggableButtonMorph new.
	downButton model: self.
	downButton morphExtent: e@e.
	Preferences scrollbarButtons 
		ifTrue: [self addMorph: downButton position: extent - borderWidth - e].
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'STP 5/1/2022 11:17:36'!
updateScrollBarsBounds
	| t |
	hideScrollBars = #hide ifTrue: [ ^ self ].
	t _ ScrollBar scrollbarThickness.
	hideScrollBars = #hideVertical ifFalse: [
		Preferences scrollbarSide = #left
		ifTrue: [scrollBar
			morphPosition: 0 @ borderWidth
			extent: t @ self vScrollBarHeight ]
		ifFalse: [scrollBar
			morphPosition: extent x - t - borderWidth @ borderWidth
			extent: t @ self vScrollBarHeight ]].
	Preferences scrollbarSide = #left
		ifTrue: [hScrollBar
			morphPosition: (borderWidth + t) @ (extent y - t - borderWidth)
			extent: self hScrollBarWidth @ t]
		ifFalse: [hScrollBar
			morphPosition: borderWidth @ (extent y - t - borderWidth)
			extent: self hScrollBarWidth @ t]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'STP 4/26/2022 17:16:23'!
focusIndicatorRight
	^ self vIsScrollbarShowing
		ifTrue: [Preferences scrollbarSide = #left
				ifTrue: [extent x - borderWidth]
				ifFalse: [ extent x - borderWidth - ScrollBar scrollbarThickness ]]
		ifFalse: [ extent x - borderWidth ]! !

!PluggableScrollPane methodsFor: 'geometry' stamp: 'STP 4/26/2022 17:11:25'!
focusIndicatorLeft
	^Preferences scrollbarSide = #left
		ifTrue: [ScrollBar scrollbarThickness + borderWidth]
		ifFalse: [^ borderWidth]! !


!Form class methodsFor: 'instance creation' stamp: 'stp 12/2/2022 11:48:00'!
extent: extentPoint depth: bitsPerPixel fromBytes: aByteArray
	"Answer an instance of me with bitmap of the given dimensions and depth from the given 32-bit PADDED-ROWS data."

	^self extent: extentPoint depth: bitsPerPixel bits: aByteArray swapWords swapBytes	! !

!LightTheme methodsFor: 'colors' stamp: 'stp 12/22/2022 14:45:55'!
defaultWindowColor
	^ `Color lightGray`
"	^ `Color white duller`"! !

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'stp 12/26/2022 18:09:19'!
fontSizes
	^ `{
		#tinyFonts -> 8 .
		#verySmallFonts -> 10 .
		#smallFonts ->11 .
		#standardFonts -> 14 .
		#largeFonts -> 18 .
		#veryLargeFonts -> 24 .
		#hugeFonts -> 32} asDictionary`! !

!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'stp 12/26/2022 23:03:28'!
placeWindowsAtPointer: aBool
	"Value of the place-windows-at-pointer preference."
	PlaceWindowsAtPointer := aBool!

placeWindowsAtPointer
	"Value of the place-windows-at-pointer preference."
"	<preference: 'Place Windows At Pointer'
		category: 'windows'
		description: 'When true, place new windows near the mouse pointer'
		type: #Boolean>"
	PlaceWindowsAtPointer ifNil: [false]!
	

initialFrameAtPointerFor: aView initialExtent: scaledExtent world: aWorld
	"Answer the frame for aView so that it's title is under the cursor position. Add an offset so that the user does not click on the close or menu button by accident."
	
	| offset |
	offset := 32@32.   "ToolBuilder default windowPadding asPoint
		+ (self defaultIconExtent // 2)
		+ (self defaultIconExtent x * 2 @ 0)" "... move to the right, even beyond menu button if it is on the left ..."

"	offset := (offset * self scaleFactor) truncated."
	
	 (Sensor cursorPoint - offset
		extent: scaledExtent)
			translatedAndSquishedToBeWithin: (self maximumUsableAreaInWorld: aWorld)! !



