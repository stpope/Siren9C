Controller subclass: #DisplayListController	instanceVariableNames: 'selection'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListController commentStamp: 'stp 1/5/2023 15:45' prior: 0!Instances of DisplayListController (or its subclasses) are used by DisplayListViews (or subclass instances) for menu management and selection.By default, a simple YellowButton menu allows for group/inspect/redraw/zoom.The RedButton is used for display item selection (by pointing or boxing-in).LeftShift extends the selection, and dragging a selection moves it.Instance Variables:	selection		<DisplayList> the object''s selected items!!DisplayListController methodsFor: 'events' stamp: 'stp 1/5/2023 15:29'!handleKeystroke: anEvent 	self halt! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/6/2023 18:41'!handleMouseDown: anEvent 	anEvent redButtonPressed		ifTrue: [self redButtonActivity: anEvent position.			 	anEvent wasHandled: true.				^self].	anEvent yellowButtonPressed		ifTrue: [self processMenu: anEvent.			 	anEvent wasHandled: true.				^self]! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/5/2023 15:37'!handleMouseUp: anEvent 	anEvent redButtonPressed ifTrue: [^ self selectAt: anEvent position].	anEvent yellowButtonPressed ifTrue: [^ self processMenu: anEvent].	! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/5/2023 16:13'!initialize	"Initialise the default state of the receiver."	selection := OrderedCollection new! !!DisplayListController methodsFor: 'events'!mouseMovedEvent: anEvent
	"Mouse dragging"

	anEvent redButtonPressed ifFalse: [^self].
	model selection == nil
		ifTrue: [self selectEvent: anEvent]
		ifFalse: [self dragSelection: anEvent]! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/5/2023 15:28'!processEvent: anEvent 	self halt! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/6/2023 10:19'!processMenu: position 	| sel |	sel := (PopUpMenu labelArray: #('group' 'edit' 'inspect' 'display' 'options')) startUp.	sel == 0		ifTrue: [^ self].	sel == 1		ifTrue: [sel := (PopUpMenu labelArray: #('group' 'ungroup' 'flatten')) startUp.			sel == 0				ifTrue: [^ self].			sel == 1				ifTrue: [^ self groupItems].			sel == 2				ifTrue: [^ self ungroupItems].			sel == 3				ifTrue: [^ self flattenItems]].	sel == 2		ifTrue: [sel := (PopUpMenu labelArray: #('copy' 'cut' 'paste' 'paste from buffer' 'again' 'undo')) startUp.			sel == 0				ifTrue: [^ self].			sel == 1				ifTrue: [^ self copyItem].			sel == 2				ifTrue: [^ self cutItem].			sel == 3				ifTrue: [^ self pasteItem].			sel == 4				ifTrue: [^ self pasteFromBuffer].			sel == 5				ifTrue: [^ self again].			sel == 6				ifTrue: [^ self undo]].	sel == 3		ifTrue: [sel := (PopUpMenu labelArray: #('model' 'editor')) startUp.			sel == 0				ifTrue: [^ self].			sel == 1				ifTrue: [^ self inspectDisplayList].			sel == 2				ifTrue: [^ self explore]].	sel == 4		ifTrue: [sel := (PopUpMenu labelArray: #('redraw' 'zoom 1:1' 'zoom to point')) startUp.			sel == 0				ifTrue: [^ self].			sel == 1				ifTrue: [^ self redraw].			sel == 2				ifTrue: [^ self zoomTo1].			sel == 3				ifTrue: [^ self zoomInPoint]].	sel == 5		ifTrue: [^ self inform: 'Not implemented yet']! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/6/2023 18:22'!clearSelection	selection := OrderedCollection new.	model changed.! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/6/2023 18:28'!select: item 	item		ifNil: [^ self].	Transcript show: 'Select ' , item printString.	(selection includes: item)		ifTrue: [selection remove: item]		ifFalse: [selection addLast: item].	model changed! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/6/2023 10:32'!selection
	"Answer the receiver's 'selection'."

	^ selection! !!DisplayListController methodsFor: 'control defaults' stamp: 'stp 1/7/2023 06:40'!redButtonActivity: pos 	"Hit-detect items from the display list or drag a selection."	| pt1 offs selRect winBounds wOff |	model == nil		ifTrue: [^ nil].	"if click rather than drag"	(Delay forMilliseconds: 250) wait.	Sensor anyButtonPressed		ifFalse: [^ self selectAt: pos].	offs := Sensor cursorPoint - pos.	winBounds := view owner boundsInWorld.	wOff := winBounds origin.	"if no selection"	(selection isEmpty			or: [Sensor shiftPressed])		ifTrue: [pt1 := Sensor cursorPoint.			self				selectRangeWhile: [Sensor anyButtonPressed]]		ifFalse: [selRect := self selectionBounds.			pt1 := Sensor cursorPoint. 			"if you click on the selection, drag it"			(selRect containsPoint: pt1 - wOff)				ifTrue: [self						dragSelectionWhile: [Sensor anyButtonPressed]]				ifFalse: ["if you click off the selection, clear it"					self select: nil.					Transcript show: 'clear';						 cr.					Sensor waitNoButton]]! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/7/2023 06:45'!dragSelectionWhile: durationBlock 	| pt0 pt1 pt2 rect canvas backup wOff winBounds |	pt0 := Sensor cursorPoint.	pt1 := Sensor cursorPoint.	pt2 := pt1.	winBounds := view owner boundsInWorld.	wOff := winBounds origin. self halt.	rect := self selectionBounds translateBy: wOff negated.	canvas := Display getCanvas.	backup := canvas				contentsOfArea: (rect expandBy: 4 @ 4).	durationBlock		whileTrue: [pt2 := Sensor cursorPoint.			pt1 = pt2				ifFalse: [backup						displayOn: Display						at: pt1 - (4 @ 4)						clippingBox: rect						rule: Form over						fillColor: nil.					rect := rect translateBy: pt2 - pt1.					pt1 := pt2.					backup := canvas								contentsOfArea: (rect expandBy: 4 @ 4).					pt2 := pt1 + (16 @ 16)]].	backup		displayOn: Display		at: pt1 - (4 @ 4)		clippingBox: rect		rule: Form over		fillColor: nil.	selection		do: [:item | item translateBy: pt2 - pt0].	model changed! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/6/2023 18:33'!selectAt: aPoint 	"Hit-detect the model for the given pt."	"transPt := view transformPoint: aPoint	"	| item |	item := view list itemWithin: 8 of: aPoint.	Transcript show: 'SelectAt ' , item printString.	"Transcript show: aPoint printString, '(', transPt rounded	printString, ') -- sel: ', item printString; cr.
	model select: item.	item == nil	ifTrue: [selOffset := nil]	ifFalse: [selOffset := aPoint - item bounds origin]."	self select: item.	^ item! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/6/2023 18:31'!selectFromPoint: aPoint toPoint: anotherPoint 	"Hit-detect the model for the given range."	| item |	(aPoint dist: anotherPoint)			< 8		ifTrue: [item := view list itemWithin: 24 of: aPoint.			Transcript show: 'Select1 ' , item printString;				 cr.			item == nil				ifTrue: [self clearSelection]				ifFalse: [self select: item]]		ifFalse: [| list |			list := view list						itemsWithin: (aPoint corner: anotherPoint).			list components				do: [:di | self select: di]]! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/6/2023 18:14'!selectRangeWhile: durationBlock 	"Hit-detect items from the display list or drag a selection as long as	the block is true."	| pt1 pt2 pt3 color rect canvas backup sel |	color := Color lightGray.	pt1 := Sensor cursorPoint.	pt2 := pt1 + (16 @ 16).	pt3 := pt2.	Sensor cursorPoint: pt2.	canvas := Display getCanvas.	rect := pt1 corner: pt2.	backup := canvas				contentsOfArea: (rect expandBy: 4 @ 4).	canvas		frameRectangle: rect		width: 2		color: color.	durationBlock		whileTrue: [pt3 := Sensor cursorPoint.			pt3 = pt2				ifFalse: [backup						displayOn: Display						at: pt1 - (4 @ 4)						clippingBox: rect						rule: Form over						fillColor: nil.					pt2 := pt3.					rect := pt1 corner: pt2.					backup := canvas								contentsOfArea: (rect expandBy: 4 @ 4).					canvas						frameRectangle: rect						width: 1						color: color]].	backup		displayOn: Display		at: pt1 - (4 @ 4)		clippingBox: rect		rule: Form over		fillColor: nil.	sel := model				itemsWithin: (rect translateBy: view owner bounds origin negated).	sel		ifNotNil: [sel components				do: [:item | self select: item]]! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/6/2023 10:25'!selectionBounds	"Answer the bounds of the selection"	| rect |	selection ifEmpty: [^ 0@0 extent: 0@0].	rect := selection first bounds.	selection do:		[ :sel |		sel ifNotNil:			[rect := rect merge: sel bounds]].	^rect! !!DisplayListController methodsFor: 'menu messages'!again
	"Repeat the last operation"! !!DisplayListController methodsFor: 'menu messages'!copyItem
	"Copy the selection."

	model copyBuffer: model selection copy.! !!DisplayListController methodsFor: 'menu messages'!cutItem
	"Cut the selection."

	| sel |
	sel := model selection.
	model copyBuffer: sel.
	model list remove: sel.
	model select: nil.
	model changed! !!DisplayListController methodsFor: 'menu messages'!flattenItems
	"Flatten the hierarchy of the selection."! !!DisplayListController methodsFor: 'menu messages'!groupItems
	"Group the selection."

	self halt! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/6/2023 10:20'!inspectDisplayList	"Inspect the display list."	model explore! !!DisplayListController methodsFor: 'menu messages'!inspectEditor
	"Inspect the display list editor."

	model inspect! !!DisplayListController methodsFor: 'menu messages'!inspectModel
	"Inspect the model."

	model list inspect! !!DisplayListController methodsFor: 'menu messages'!optionsDialog
	"Run the options dialog box."! !!DisplayListController methodsFor: 'menu messages'!pasteFromBuffer
	"Paste the selection."! !!DisplayListController methodsFor: 'menu messages'!pasteItem
	"Paste the selection."

	model list add: model copyBuffer.
	model changed! !!DisplayListController methodsFor: 'menu messages'!undo
	"Un-do the last operation"! !!DisplayListController methodsFor: 'menu messages'!ungroupItems
	"Un-group the selection."! !!DisplayListController methodsFor: 'menu messages'!zoom
	"Prompt the user for a scale point, zoom the view's scale, and redraw."

self halt.
"	| pt prompt |
	prompt := (view xScale == nil)
		ifTrue: ['1@1']
		ifFalse: [(view xScale @ view yScale) printString].
	pt := DialogView request: 'zoom to scale:'
			initialAnswer: prompt.
	pt = '' ifFalse: [pt := Compiler evaluate: pt.
			pt class == Point
				ifTrue: [view xScale: pt x; yScale: pt y; redraw]]
"! !!DisplayListController methodsFor: 'menu messages'!zoomInPoint
	"Zoom the view's scale, and redraw."

	view xScale: 1.0; yScale: 1.0; redraw! !!DisplayListController methodsFor: 'menu messages'!zoomOutPoint
	"Zoom the view's scale, and redraw."

	view xScale: 1.0; yScale: 1.0; redraw! !!DisplayListController methodsFor: 'menu messages'!zoomTo1
	"Zoom the view's scale, and redraw."

	view zoom: (1.0 @ 1.0)! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListController class	instanceVariableNames: ''!!DisplayListController class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 16:14'!new	^super new initialize! !PluggableScrollPane subclass: #DisplayListView	instanceVariableNames: 'dList backgroundColor background page controller'	classVariableNames: 'Constants'	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:20:16'!background
	"Answer the receiver's background form or color, if any"

	^background! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:26:10'!backgroundColor
	"Amswer the receiver's background color."

	backgroundColor ifNotNil: [^backgroundColor].
	^Color lightYellow muchLighter
! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:23:54'!backgroundColor: back
	"Set the receiver's background color, if any"

	^backgroundColor _ back! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:29:29'!clefForm
	"Answer the receiver's clef form."

	^nil! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/28/2022 20:12'!defaultBounds	"answer the default bounds for the receiver"	^ 0 @ 0 corner: 2000 @ 1000! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/5/2023 16:16'!initialize	"Set up the receiver."	super initialize.	page := 0 @ 0.	bounds := 0 @ 0 extent: 2000 @ 1000.	controller := DisplayListController new.	controller view: self! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 11/30/2022 17:10:27'!innerMorphClass
	"Answer the class to use for the inner morph."

	^InnerDisplayListMorph! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/1/2022 23:54:33'!list
	"Assign the receiver's display list"

	^dList! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/6/2023 11:16'!list: aDList 	"Assign the receiver's display list"	dList := aDList.	dList addDependent: self.	controller		ifNotNil: [controller model: aDList].	bounds := aDList bounds expandBy: 128 @ 128! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 1/7/2023 06:24'!displaySelectionOn: aCanvas 	"Show handles on the selected items"	| sel ext |	ext := 3.	sel := controller selection.	sel		do: [:item | item bounds corners				do: [:pt | aCanvas						frameRectangle: (pt - (ext @ ext) extent: ((ext * 2) @ (ext * 2)))						width: 1						color: Color gray]]! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 1/6/2023 13:40'!drawOn: aCanvas 	"Iterate over my items and display them on the given canvas."	dList drawOn: aCanvas.	self displaySelectionOn: aCanvas	! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 02:24:25'!clearSelection
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
	^ aMenu! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:25'!handleKeystroke: anEvent 	controller handleKeystroke: anEvent! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:38'!handleMouseDown: anEvent 	controller handleMouseDown: anEvent! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:25'!handleMouseUp: anEvent 	controller handleMouseUp: anEvent ! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:29'!handlesMouseDown: anEvent 	^ true! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:17'!hasYellowButtonMenu0	self halt.	^ true! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:16'!inspectDList
	"Open an inspector on the model of the DLV"

	dList inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:20'!inspectModel
	"Open an inspector on the model of the DLV"

	model inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:25'!inspectSelection
	"Open an inspector on the model of the DLV"

	scroller selection inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/6/2023 13:46'!update: aspect 	"Old-school MVC - update redraws"	| canv |	super update: aspect.	canv := Display getCanvas.	owner drawSubmorphsOn: canv! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListView class	instanceVariableNames: ''!!DisplayListView class methodsFor: 'instance creation' stamp: 'stp 11/29/2022 14:08:17'!newOn: aDList
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

	self layoutMorph addMorph: model layoutSpec: LayoutSpec useAll! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListWindow class	instanceVariableNames: ''!!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/28/2022 20:41'!open: aDLV label: lab 	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: 900 @ 500! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 11:28'!open: aDLV label: lab extent: ext	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 11:33'!open: aDLV label: lab extent: ext scrollBars: scroll 	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scroll		ifTrue: [scrollPane alwaysShowScrollBars]		ifFalse: [scrollPane hideScrollBarsIndefinitely].	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !ProtoObject subclass: #InnerDisplayListMorph	instanceVariableNames: 'dList selection isDragging dragPos'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!InnerDisplayListMorph methodsFor: 'drawing' stamp: 'stp 12/2/2022 10:25:42'!drawOn: aCanvas
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