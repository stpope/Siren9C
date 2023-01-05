Controller subclass: #DisplayListController	instanceVariableNames: 'selection'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListController methodsFor: 'accessing' stamp: 'stp 12/29/2022 00:09'!select: item	selection addLast: item! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 12/29/2022 00:09'!selection	^ selection! !PluggableScrollPane subclass: #DisplayListView	instanceVariableNames: 'dList backgroundColor background page'	classVariableNames: 'Constants'	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:20:16'!background
	"Answer the receiver's background form or color, if any"

	^background! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:26:10'!backgroundColor
	"Amswer the receiver's background color."

	backgroundColor ifNotNil: [^backgroundColor].
	^Color lightYellow muchLighter
! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:23:54'!backgroundColor: back
	"Set the receiver's background color, if any"

	^backgroundColor _ back! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:29:29'!clefForm
	"Answer the receiver's clef form."

	^nil! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/28/2022 20:12'!defaultBounds	"answer the default bounds for the receiver"	^ 0 @ 0 corner: 2000 @ 1000! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/28/2022 20:29'!initialize	"Set up the receiver."	super initialize.	page := 0 @ 0.	bounds := 0@0 extent: 2000 @ 1000! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 11/30/2022 17:10:27'!innerMorphClass
	"Answer the class to use for the inner morph."

	^InnerDisplayListMorph! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/1/2022 23:54:33'!list
	"Assign the receiver's display list"

	^dList! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/29/2022 00:14'!list: aDList 	"Assign the receiver's display list"	dList := aDList.	model ifNil: [self model: aDList].	bounds := aDList bounds expandBy: 128 @ 128! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 12/28/2022 20:33'!drawOn: aCanvas
	"Iterate over my items and display them on the given canvas."

	dList drawOn: aCanvas
! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 02:24:25'!clearSelection
	"Clear the scroller's selection."

	scroller clearSelection! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:33:14'!getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		add: 'Inspect model' 					action: #inspectModel;
		add: 'Inspect selection' 					action: #inspectSelection;
		add: 'Clear selection' 					action: #clearSelection;
		add: 'Inspect display list' 					action: #inspectDList.
	^ aMenu! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:16'!inspectDList
	"Open an inspector on the model of the DLV"

	dList inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:20'!inspectModel
	"Open an inspector on the model of the DLV"

	model inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:25'!inspectSelection
	"Open an inspector on the model of the DLV"

	scroller selection inspect! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListView class	instanceVariableNames: ''!!DisplayListView class methodsFor: 'instance creation' stamp: 'stp 11/29/2022 14:08:17'!newOn: aDList
	"Create a DLV on the given DL"

	| view |
	view _ self new initialize.
	view list: aDList.
	^view.! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:22:15'!exampleLines
	"Test/example of using a DLV."
	"DisplayListView exampleLines"
	
	| list view |
	list _ DisplayList linesX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:49:12'!exampleMixed
	"Test/example of using a DLV."
	"DisplayListView exampleMixed"
	
	| list view |
	list _ DisplayList mixedExample.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/30/2022 17:29:01'!examplePolylines
	"Test/example of using a DLV."
	"DisplayListView examplePolylines"
	
	| list view |
	list _ DisplayList polylinesX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/28/2022 19:56'!exampleRectangles	"Test/example of using a DLV."	"DisplayListView exampleRectangles"	| list view window |	list := DisplayList rectanglesX: 2000 byY: 2000.	view := self newOn: list.	DisplayListWindow open: view label: 'DisplayListView'		"	window := (DisplayListWindow labelled: 'DisplayListView')				model: view.	window addMorph: view"! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:39:58'!exampleStrings
	"Test/example of using a DLV."
	"DisplayListView exampleStrings"
	
	| list view |
	list _ DisplayList stringsX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/1/2022 10:54:13'!exampleVisuals
	"Test/example of using a DLV."
	"DisplayListView exampleVisuals"
	
	| list view |
	list _ DisplayList visualsX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/30/2022 07:44'!openOn: list 	"Test/example of using a DLV."	"DisplayListView openOn: (DisplayList stringsX: 2000 byY: 2000)"	| view |	view := self newOn: list.	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/30/2022 07:49'!backgroundColor	^ Color lightYellow! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/1/2022 11:16:11'!constants
	"Answer the class' dictionary of images"
	"DisplayListView constants"
	
	Constants ifNil: [Constants _ Dictionary new.].
	^Constants! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/30/2022 07:49'!foregroundColor	^ Color gray! !SystemWindow subclass: #DisplayListWindow	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListWindow methodsFor: 'initialization' stamp: 'stp 11/29/2022 15:05:42'!buildMorphicWindow
	"Set up the receiver window"

	self layoutMorph addMorph: model layoutSpec: LayoutSpec useAll! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListWindow class	instanceVariableNames: ''!!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 19:51'!open0: aDLV label: lab 	"Open an instance of the window on the given view."	"DisplayListView displayListViewExample"	super open: aDLV label: lab! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 20:41'!open: aDLV label: lab 	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: 900 @ 500! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 11:28'!open: aDLV label: lab extent: ext	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 11:33'!open: aDLV label: lab extent: ext scrollBars: scroll 	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scroll		ifTrue: [scrollPane alwaysShowScrollBars]		ifFalse: [scrollPane hideScrollBarsIndefinitely].	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !ProtoObject subclass: #InnerDisplayListMorph	instanceVariableNames: 'dList selection isDragging dragPos'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!InnerDisplayListMorph methodsFor: 'drawing' stamp: 'stp 12/2/2022 10:25:42'!drawOn: aCanvas
	"Iterate over my items and display them on the given canvas."

	| off xform |
	off _ 0.
	xform := aCanvas currentTransformation.

	aCanvas fillRectangle: self morphLocalBounds 
			color: owner backgroundColor.
	owner clefForm ifNotNil:
		[aCanvas image: owner clefForm at: 0 @ 0.
			off _ owner clefForm width].
	owner background ifNotNil:
		[aCanvas image: owner background at: owner clefForm width @ 0 ].
	aCanvas geometryTransformation: (xform translatedBy: off).
	dList do: [ :item |
		item drawOn: aCanvas].
	selection ifNotNil: 
		[selection do: 
			[ :item |
			item drawSelectionOn: aCanvas]].
	aCanvas geometryTransformation: (xform translatedBy: off negated)
! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 02:24:58'!clearSelection
	"Clear the receiver's selected item list."

	selection _ OrderedCollection new.
	self redrawNeeded! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:30:23'!initialize
	"Initialize the receiver."

	super initialize.
	self setProperty: #handlesMouseStillDown: toValue: true.
	selection _ OrderedCollection new.
	^self! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 11/29/2022 16:02:18'!list: aDList
	"Assign the receiver's display list"

	dList _ aDList.
	self morphExtent: dList extent! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 02:13:28'!selected
	"Answer the receiver's selected item list."

	^selection isEmpty
		ifTrue: [nil]
		ifFalse: [selection first]! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 00:39:20'!selection
	"Answer the receiver's selected item list."

	^selection! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 11/30/2022 17:02:47'!click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self halt.! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 01:08:26'!handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	
	^true! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 00:55:01'!handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	
	^true! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/1/2022 23:52:30'!keyStroke: aKeyboardEvent

	self halt.! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:16:14'!mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse-down event."

	| dist sel prevSel |
	dist _ 8. 		"hit-detect range"
	dragPos _ Sensor peekMousePt.
	isDragging _ true.
	prevSel _ self selected.
	sel _ owner list itemWithin: dist of: localEventPosition.
	sel ifNotNil: [aMouseButtonEvent shiftPressed
			ifFalse: [self clearSelection].
		sel == prevSel ifFalse: [self select: sel].
		^self redrawNeeded].
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	
"	Transcript show: 'MB1D: ', dragPos printString, '  -  ', sel offset printString; cr.
"! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 01:05:24'!mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	isDragging _ false.
! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:23:57'!mouseButton2Activity
	"Invoke the menu"
	
	owner getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.]! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:19:05'!mouseStillDown

	| mPos delta |
	Sensor isMouseButton1Pressed ifFalse: [^super mouseStillDown].
	selection ifEmpty: [^self].
	mPos _ Sensor peekMousePt.
	delta _ mPos - dragPos.
"	Transcript show: 'MSD: m ', mPos printString, '  -   r ', rPos printString, '  -   d ', delta printString,
				 '  -  o ', selection first offset printString; cr.
	self halt."
	dragPos _ mPos.
	selection do: 
			[ :item |
			item offset: (item offset + delta) truncated].
	self redrawNeeded! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 01:22:54'!select: item
	"Answer the selected item"

	(selection includes: item) ifTrue: [^selection remove: item].
	selection addLast: item
	"	aMouseButtonEvent shiftPressed
		ifFalse: []
"! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!InnerDisplayListMorph class	instanceVariableNames: ''!!InnerDisplayListMorph class methodsFor: 'instance creation' stamp: 'stp 11/30/2022 17:17:31'!new
	"Initialize all instances"

	^super new initialize! !!InnerDisplayListMorph class methodsFor: 'instance creation' stamp: 'stp 11/30/2022 17:14:29'!new0
	"Initialize all instances"

	^super new initialize! !!InnerDisplayListMorph class methodsFor: 'class variables' stamp: 'stp 12/2/2022 00:43:05'!backgroundColor
	"Answer the background color to use."

	^Color lightYellow muchLighter! !