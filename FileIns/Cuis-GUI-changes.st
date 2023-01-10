'From Cuis 6.0 [latest update: #5115] on 1 May 2022 at 11:35:52 am'!


!PreferenceSet class methodsFor: 'sys fonts' stamp: 'stp 1/7/2023 07:53:25'!
fontSizes
	^ `{
		#tinyFonts -> 7 .
		#verySmallFonts -> 10 .
		#smallFonts ->11 .
		#standardFonts -> 14 .
		#largeFonts -> 18 .
		#veryLargeFonts -> 24 .
		#hugeFonts -> 32} asDictionary`! !

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

!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'stp 1/7/2023 21:32:52'!
show: anObject
	"Old TranscriptStream protocol."
	(anObject asString findTokens: Character newLineCharacter)
		do: [ :line | self nextPutAll: line"; newLine "]! !

!SoundPlayer class methodsFor: 'playing' stamp: 'stp 1/9/2023 07:38:00'!
canStartPlayer
	"Some platforms do no support simultaneous record and play. If this is one of those platforms, return false if there is a running SoundRecorder."

"	Preferences canRecordWhilePlaying ifTrue: [^ true].
"	SoundRecorder anyActive ifTrue:[^false].
	^ true
! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'stp 1/7/2023 10:46:55'!
startUp
	"Display and make a selection from the receiver as long as the button 
	is pressed. Answer the current selection."
	
	^ self startUpWithCaption: nil! !

!DebuggerWindow methodsFor: 'GUI building' stamp: 'stp 1/9/2023 15:03:51'!
initialExtent
	^ RealEstateAgent standardWindowExtent ! !

!Debugger methodsFor: 'shout styling' stamp: 'stp 1/7/2023 12:12:30'!
hasBindingOf: aString
	self selectedContext ifNil: [^false].
	^self selectedContext tempNames includes: aString! !
