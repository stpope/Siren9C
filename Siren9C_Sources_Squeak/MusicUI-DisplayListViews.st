Morph subclass: #DLVControlMorph	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DLVControlMorph commentStamp: 'stp 1/20/2023 18:34' prior: 0!A DLVControlMorph is a small control panel for display list views.It contains zoom/scroll buttons and the like and tries to stay attached to the view it operates on.!!DLVControlMorph methodsFor: 'button' stamp: 'stp 1/20/2023 18:34'!close	"Respond to a close button press by disappearing"	self delete! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DLVControlMorph class	instanceVariableNames: ''!!DLVControlMorph class methodsFor: 'instance creation' stamp: 'stp 1/20/2023 18:09'!createButton: label action: action on: victim	"Create an iconic button for the transport controls"	| button |	button := IconicButton new target: victim;		borderWidth: 0;		labelGraphic: ((DisplayListView glyphs at: #Transport) at: label);		color: Color transparent; 		actWhen: #buttonDown;		actionSelector: action.	^ button! !!DLVControlMorph class methodsFor: 'instance creation' stamp: 'stp 1/23/2023 11:01'!newOn: aDLV	"Create a 3x4 button panel for controlling a display list view"	"DisplayList rectangleExample"		| me top mid bot all |	me := BorderedMorph new		color: Color lightBlue; "DisplayListView backgroundColor;"		position: 0@0;		extent: 48@48;		layoutPolicy: TableLayout new;		gridOrigin: 0@0;		gridModulus: 16@16;		listDirection: #leftToRight;		wrapCentering: #topLeft;		hResizing: #shrinkWrap;		vResizing: #shrinkWrap;		borderWidth: 2;		borderColor: Color lightGray;		yourself.	top := AlignmentMorph newRow vResizing: #spaceFill; color: Color transparent.	mid := AlignmentMorph newRow vResizing: #spaceFill; color: Color transparent.	bot := AlignmentMorph newRow vResizing: #spaceFill; color: Color transparent.	all := AlignmentMorph newColumn vResizing: #spaceFill; color: Color transparent.	top addMorph: (self createButton: #close action: #delete on: me).	top addMorphBack: (self createButton: #'up-arrow' action: #upButton on: aDLV).	top addMorphBack: (self createButton: #pencil action: #exploreIt on: aDLV).	mid addMorph: (self createButton: #'left-arrow' action: #leftButton on: aDLV).	mid addMorphBack: (self createButton: #o action: #zeroButton on: aDLV).	mid addMorphBack: (self createButton:#'right-arrow' action: #rightButton on: aDLV).	bot addMorph: (self createButton: #'zoom-in' action: #zoomIn on: aDLV).	bot addMorphBack: (self createButton: #'down-arrow' action: #downButton on: aDLV).	bot addMorphBack: (self createButton:  #'zoom-out' action: #zoomOut on: aDLV).		all addMorph: top.	all addMorphBack: mid.	all addMorphBack: bot.										"add playback buttons for event list editors"	(aDLV model isKindOf: EventListEditor) ifTrue: 		[ | xpo |		me extent: 48@64.		xpo := AlignmentMorph newRow vResizing: #spaceFill; color: Color transparent.		xpo addMorph: (self createButton:  #'rewind' action: #rewindButton on: aDLV).		xpo addMorphBack: (self createButton: #'stop-button' action: #stopButton on: aDLV).		xpo addMorphBack: (self createButton: #'play' action: #playButton on: aDLV).		all addMorphBack: xpo].		me addMorph: all.	^ me! !Controller subclass: #DisplayListController	instanceVariableNames: 'selection copyBuffer'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListController commentStamp: 'stp 1/5/2023 15:45' prior: 0!Instances of DisplayListController (or its subclasses) are used by DisplayListViews (or subclass instances) for menu management and selection.By default, a simple YellowButton menu allows for group/inspect/redraw/zoom.The RedButton is used for display item selection (by pointing or boxing-in).LeftShift extends the selection, and dragging a selection moves it.Instance Variables:	selection		<DisplayList> the object''s selected items!!DisplayListController methodsFor: 'events' stamp: 'stp 1/22/2023 11:59'!handleKeystroke: anEvent	"Handle a keyboard command - edit, scroll, etc."		| cmds arrows |	cmds := (($x -> #cutItem),		"CMD-key items"		($c -> #copyItem),		($v -> #pasteItem),		($z -> #undo)).										"arrow paging"	arrows := ((Character arrowRight -> #rightButton),		(Character arrowLeft -> #leftButton),		(Character arrowUp -> #upButton),		(Character arrowDown -> #downButton),										"other keys"		(Character home -> #zeroButton), 		(Character end -> #rightButton),"		(Character pageUp -> #leftButton),					these come up as arrow keys for some rason		(Character pageDown -> #rightButton),"		(Character space -> #rightButton)).										"handle cmd keys"	((anEvent commandKeyPressed) or: [anEvent controlKeyPressed]) ifTrue:		[(cmds keys includes: anEvent keyCharacter)			ifTrue: ["Transcript show: 'DLC key: cmd-',  anEvent keyValue asString; cr."				 ^ view controller perform: (cmds at: anEvent keyCharacter)]			ifFalse: [ ^ nil]].										"handle other keys"	(arrows keys includes: anEvent keyCharacter) ifTrue:		["Transcript show: 'DLC key: ',  anEvent keyCharacter asString; cr."		 ^ view perform: (arrows at: anEvent keyCharacter)].										"otherwise, complain"	Transcript show: 'DLC key: ',  anEvent keyValue asString; cr.	self inform: 'Unknown keyboard command.'.! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/22/2023 11:02'!handleMouseDown: anEvent 	"Process a mouse-down event"		anEvent redButtonPressed		ifTrue: [self redButtonActivity: anEvent position.			 	anEvent wasHandled: true.				^self].	anEvent yellowButtonPressed		ifTrue: [self processMenu: anEvent.			 	anEvent wasHandled: true.				^self]! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/22/2023 11:02'!handleMouseUp: anEvent	anEvent wasHandled: true! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/14/2023 07:56'!initialize	"Initialize the default state of the receiver."		selection := OrderedCollection new! !!DisplayListController methodsFor: 'events'!mouseMovedEvent: anEvent
	"Mouse dragging"

	anEvent redButtonPressed ifFalse: [^self].
	model selection == nil
		ifTrue: [self selectEvent: anEvent]
		ifFalse: [self dragSelection: anEvent]! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/5/2023 15:28'!processEvent: anEvent 	self halt! !!DisplayListController methodsFor: 'events' stamp: 'stp 1/14/2023 07:45'!processMenu: event	"Create and run the hierarchical DLV menu."		| menu |	menu := MenuMorph new defaultTarget: self.	menu addItem: [ :item |		item contents: 'Group'; subMenuUpdater: self selector: #groupMenuOn: ].	menu addItem: [ :item |		item contents: 'Inspect'; subMenuUpdater: self selector: #inspectMenuOn: ].	menu addItem: [ :item |		item contents: 'Edit'; subMenuUpdater: self selector: #editMenuOn: ].	menu addItem: [ :item |		item contents: 'Display'; subMenuUpdater: self selector: #displayMenuOn: ].	menu invokeModal! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/13/2023 07:32'!clearSelection	selection := OrderedCollection new.	self changed.! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/22/2023 08:40'!select: item 	item ifNil: 		[selection :=  OrderedCollection new.		^ model changed]."	Transcript show: 'Select ' , item printString."	(selection includes: item)		ifTrue: [selection remove: item]		ifFalse: [selection addLast: item].	model changed! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/6/2023 10:32'!selection
	"Answer the receiver's 'selection'."

	^ selection! !!DisplayListController methodsFor: 'accessing' stamp: 'stp 1/12/2023 18:08'!winOffset 	"Answer the offset of the window relative to screen coordinates."		^view owner boundsInWorld origin.! !!DisplayListController methodsFor: 'control defaults' stamp: 'stp 1/22/2023 12:02'!redButtonActivity: pos 	"Hit-detect items from the display list or drag a selection."		| pt1 offs selRect winBounds wOff |	model == nil		ifTrue: [^ nil].	(Delay forMilliseconds: 250) wait.				"if click rather than drag"	Sensor anyButtonPressed		ifFalse: [^ self selectAt: pos].	offs := Sensor cursorPoint - pos.	winBounds := view owner boundsInWorld.	wOff := winBounds origin.	(selection isEmpty or: [Sensor shiftPressed])		"if no selection"		ifTrue: [pt1 := Sensor cursorPoint.			self selectRangeWhile: [Sensor anyButtonPressed]]		ifFalse: [selRect := self selectionBounds.			pt1 := Sensor cursorPoint. 			(selRect containsPoint: pt1 - wOff)		"if you click on the selection, drag it"				ifTrue: [self						dragSelectionWhile: [Sensor anyButtonPressed]]				ifFalse: [self select: nil.				"if you click off the selection, clear it"					Transcript show: 'clear';  cr.					Sensor waitNoButton]]! !!DisplayListController methodsFor: 'menus' stamp: 'stp 1/12/2023 17:52'!displayMenuOn: aMenu	aMenu addItem: [ :item |		item contents: 'redraw'; target: self; selector: #redraw].	aMenu addItem: [ :item |		item contents: 'zoom 1:1'; target: self; selector: #zoomTo1].	aMenu addItem: [ :item |		item contents: 'zoom to point'; target: self; selector: #zoomInPoint].	aMenu addItem: [ :item |		item contents: 'options'; target: self; selector: #displayOptions].! !!DisplayListController methodsFor: 'menus' stamp: 'stp 1/14/2023 07:44'!editMenuOn: aMenu	aMenu addItem: [ :item |		item contents: 'copy'; target: self; selector: #copyItem].	aMenu addItem: [ :item |		item contents: 'cut'; target: self; selector: #cutItem].	aMenu addItem: [ :item |		item contents: 'paste'; target: self; selector: #pasteItem]."	aMenu addItem: [ :item |		item contents: 'again'; target: self; selector: #again].	aMenu addItem: [ :item |		item contents: 'undo'; target: self; selector: #undo]."! !!DisplayListController methodsFor: 'menus' stamp: 'stp 1/12/2023 17:44'!groupMenuOn: aMenu	aMenu addItem: [ :item |		item contents: 'clear selection'; target: self; selector: #clearSelection].	aMenu addItem: [ :item |		item contents: 'group'; target: self; selector: #groupItems].	aMenu addItem: [ :item |		item contents: 'ungroup'; target: self; selector: #ungroupItems].	aMenu addItem: [ :item |		item contents: 'flatten'; target: self; selector: #flattenItems].! !!DisplayListController methodsFor: 'menus' stamp: 'stp 1/12/2023 17:56'!inspectMenuOn: aMenu	aMenu addItem: [ :item |		item contents: 'model'; target: self; selector: #inspectModel].	aMenu addItem: [ :item |		item contents: 'd.list'; target: self; selector: #inspectDisplayList].	aMenu addItem: [ :item |		item contents: 'editor'; target: self; selector: #explore].! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/22/2023 08:30'!dragSelectionWhile: durationBlock	"Do mouse-drag region selection"		| pt1 pt2 canvas |	pt1 := Sensor cursorPoint.	pt2 := pt1.	canvas := Display getCanvas.	durationBlock whileTrue: 			[pt2 := Sensor cursorPoint.			pt1 = pt2				ifFalse: [selection do: [:item | item translateBy: pt2 - pt1].						model changed.						pt1 := pt2]]! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/22/2023 08:39'!selectAt: aPoint 	"Hit-detect the model for the given pt."	| item |	item := view list itemWithin: 8 of: aPoint."	Transcript show: 'SelectAt ' , item printString.""	Transcript show: aPoint printString, '(', transPt rounded printString, ') -- sel: ', item printString; cr."	self select: item.	^ item! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/22/2023 12:01'!selectFromPoint: aPoint toPoint: anotherPoint 	"Hit-detect the model for the given range."	| item |	(aPoint dist: anotherPoint) < 8		ifTrue: [item := view list itemWithin: 24 of: aPoint.			Transcript show: 'Select1 ' , item printString; cr.			item == nil				ifTrue: [self clearSelection]				ifFalse: [self select: item]]		ifFalse: [| list |			list := view list itemsWithin: (aPoint corner: anotherPoint).			list components				do: [:di | self select: di]]! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/22/2023 12:13'!selectRangeWhile: durationBlock 	"Hit-detect items from the display list or drag a selection as long as the block is true."		| pt1 pt2 pt3 color rect canvas backup sel |	color := Color lightGray.	pt1 := Sensor cursorPoint.	pt2 := pt1 + (16 @ 16).	pt3 := pt2.	Sensor cursorPoint: pt2.	canvas := Display getCanvas.	rect := pt1 corner: pt2.													"grab a region to cache withut the selection box on it"	backup := canvas contentsOfArea: (rect expandBy: 4 @ 4).	canvas frameRectangle: rect		width: 2		color: color.	durationBlock 								"drag loop"		whileTrue: [pt3 := Sensor cursorPoint.			pt3 = pt2				ifFalse: [backup 					"display cached image"							displayOn: Display							at: pt1 - (4 @ 4)							clippingBox: rect							rule: Form over							fillColor: nil.					pt2 := pt3.					rect := pt1 corner: pt2.		"grab new cached region"					backup := canvas contentsOfArea: (rect expandBy: 4 @ 4).					canvas frameRectangle: rect 	"draw frame wround selection region"							width: 1							color: color]].	backup displayOn: Display 					"when done, restore display"			at: pt1 - (4 @ 4)			clippingBox: rect			rule: Form over			fillColor: nil.	sel := view itemsWithin: 						"get selected objects"					(rect translateBy: view owner bounds origin negated).	sel ifNotNil: [sel components do: 			[ :item | self select: item]].! !!DisplayListController methodsFor: 'selecting' stamp: 'stp 1/6/2023 10:25'!selectionBounds	"Answer the bounds of the selection"	| rect |	selection ifEmpty: [^ 0@0 extent: 0@0].	rect := selection first bounds.	selection do:		[ :sel |		sel ifNotNil:			[rect := rect merge: sel bounds]].	^rect! !!DisplayListController methodsFor: 'menu messages'!again
	"Repeat the last operation"! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 10:12'!copyItem
	"Copy the selection."

	selection size = 1		ifTrue: [copyBuffer := selection first copy]		ifFalse: [copyBuffer := DisplayList new.			selection do: [ :item | copyBuffer add: item copy at: item offset]]! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 07:32'!cutItem
	"Cut the selection."

	| sel |
	sel := model selection.
	copyBuffer := sel.
	model list remove: sel.
	self select: nil.
	self changed! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/23/2023 06:31'!displayOptions
	"Display options menu"

	self inform: 'Not implemented yet.'! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 10:14'!flattenItems
	"Flatten the hierarchy of the selection."		model flatten.	self changed! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 07:55'!groupItems
	"Group the selection."

	| list box |	list := DisplayList new.	box := self selectionBounds.	selection do: [ :item |		model remove: item.		list add: item at: item offset - box origin].	model add: list at: box origin.	selection := OrderedCollection new.	selection addLast: list.	self changed! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 10:13'!inspectDisplayList	"Inspect the display list."		view model explore! !!DisplayListController methodsFor: 'menu messages'!inspectEditor
	"Inspect the display list editor."

	model inspect! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 10:13'!inspectModel
	"Inspect the model."

	model inspect! !!DisplayListController methodsFor: 'menu messages'!optionsDialog
	"Run the options dialog box."! !!DisplayListController methodsFor: 'menu messages'!pasteFromBuffer
	"Paste the selection."! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 10:07'!pasteItem
	"Paste the copyBuffer."
	| wOff  | 	wOff := view owner boundsInWorld origin.
	model add: copyBuffer at: Sensor cursorPoint - wOff.
	self changed! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/13/2023 07:36'!redraw	self changed! !!DisplayListController methodsFor: 'menu messages' stamp: 'stp 1/22/2023 11:35'!undo
	"Un-do the last operation - not implemented yet"		self halt.! !!DisplayListController methodsFor: 'menu messages'!ungroupItems
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

	view zoom: (1.0 @ 1.0)! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListController class	instanceVariableNames: ''!!DisplayListController class methodsFor: 'instance creation' stamp: 'stp 1/5/2023 16:14'!new	^super new initialize! !PluggableScrollPane subclass: #DisplayListView	instanceVariableNames: 'dList controller backgroundColor background page inset scale'	classVariableNames: 'Constants Glyphs'	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListView methodsFor: 'initialize-release' stamp: 'stp 1/23/2023 07:40'!initialize	"Set up the receiver."		super initialize.	page := 0 @ 0.	bounds := 0 @ 0 extent: 2000 @ 1000. 	inset := 32@32.	scale := 1.0 @ 1.0.	backgroundColor := self class backgroundColor.	self initializeController! !!DisplayListView methodsFor: 'initialize-release' stamp: 'stp 1/14/2023 07:40'!initializeController	"Set up the receiver's controller."		controller := DisplayListController new.	controller view: self.	controller addDependent: self.! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:20:16'!background
	"Answer the receiver's background form or color, if any"

	^background! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/23/2023 06:35'!backgroundColor
	"Amswer the receiver's background color."

	backgroundColor ifNotNil: [^backgroundColor].
	^self class backgroundColor! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:23:54'!backgroundColor: back
	"Set the receiver's background color, if any"

	^backgroundColor := back! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:29:29'!clefForm
	"Answer the receiver's clef form."

	^nil! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/22/2023 11:55'!controller	"Answer the receiver's controller."	^ controller! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/23/2023 10:25'!defaultBounds	"answer the default bounds for the receiver"		^ 0 @ 0 corner: 2000 @ 1000! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/10/2023 14:53'!displayList
	"Assign the receiver's display list"

	^dList! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/23/2023 10:25'!displayList: aDList 	"Assign the receiver's display list"		dList := aDList.	dList addDependent: self.	controller		ifNotNil: [controller model: aDList].	bounds := aDList bounds expandBy: 128 @ 128! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 11/30/2022 17:10:27'!innerMorphClass
	"Answer the class to use for the inner morph."

	^InnerDisplayListMorph! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/11/2023 13:08'!inset
	"Answer the receiver's inset point."

	^inset! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/22/2023 12:11'!itemsWithin: aRect	"Answer the items within the given rectangle."	^ dList itemsWithin: aRect! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/1/2022 23:54:33'!list
	"Assign the receiver's display list"

	^dList! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/21/2023 06:16'!list: aDList 	"Assign the receiver's display list"	dList := aDList.	dList addDependent: self.	dList container: self."	controller		ifNotNil: [controller model: aDList]."	bounds := aDList bounds expandBy: 128 @ 128! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/22/2023 08:37'!model: anObject	"Set my model and make me me a dependent of the given object."	model ifNotNil: [model removeDependent: self].	anObject ifNotNil: [anObject addDependent: self].	model := anObject.	anObject isDisplayItem		ifTrue: [controller model: anObject]		ifFalse: [controller model: anObject eventList]	! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 1/20/2023 19:32'!scrollPane	"Isn't there a better way to do this?"	^owner owner! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 1/23/2023 10:24'!displaySelectionOn: aCanvas 	"Show handles on the selected items"		| ext |	ext := 3.	controller selection do: 		[ :item | item bounds corners do:			[ :pt | aCanvas frameRectangle: (pt - (ext @ ext) extent: ((ext * 2) @ (ext * 2)))					width: 1					color: Color gray]]! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 1/23/2023 10:38'!displaySelectionOn: aCanvas scale: scale	"Show handles on the selected items"	aCanvas transformBy: (MorphicTransform offset: 0 angle: 0 scale: scale) 			clippingTo: Display boundingBox 			during:  [ :canvas | | ext |					ext := 3.					controller selection do: 						[ :item | item bounds corners do: 							[ :pt | aCanvas frameRectangle: (pt - (ext @ ext) extent: ((ext * 2) @ (ext * 2)))										width: 1										color: Color lightGray]]]! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 1/23/2023 10:39'!drawOn: aCanvas 	"Iterate over my items and display them on the given canvas."		aCanvas fillRectangle: owner bounds color: self backgroundColor.	(scale = (1.0 @ 1.0))		ifTrue: [dList drawOn: aCanvas.			self displaySelectionOn: aCanvas]		ifFalse: [aCanvas transformBy: (MorphicTransform offset: 0 angle: 0 scale: scale) 			clippingTo: Display boundingBox 			during:  [ :canv |				dList drawOn: canv.				self displaySelectionOn: canv]]! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/20/2023 19:35'!downButton	"Respond to a button press from the control widget"	| delta |	delta := owner bounds height / -2.	self scrollPane scrollBy: (0 @ delta)! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/23/2023 10:54'!exploreIt	"Respond to a button press from the control widget"	model ifNil: [dList explore ]			ifNotNil: [model explore]! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/20/2023 19:37'!leftButton	"Respond to a button press from the control widget"	| delta |	delta := owner bounds width / 2.	self scrollPane scrollBy: (delta @ 0)! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/20/2023 19:36'!rightButton	"Respond to a button press from the control widget"	| delta |	delta := owner bounds width / -2.	self scrollPane scrollBy: (delta @ 0)! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/20/2023 19:35'!upButton	"Respond to a button press from the control widget"		| delta |	delta := owner bounds height / 2.	self scrollPane scrollBy: (0 @ delta)! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/23/2023 08:06'!zeroButton	"Respond to a button press from the control widget"	scale := 1.0 @ 1.0. 	bounds := dList bounds expandBy: 128 @ 128.	self scrollPane scrollToLeft.	self scrollPane scrollToTop.	self update: #zoom.	self layoutChanged! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/23/2023 11:22'!zoomIn	"Respond to a button press from the control widget"	Sensor shiftPressed		ifTrue: [scale := (scale x * 1.25) @ scale y]		ifFalse: [scale := scale * 1.25].	bounds := (dList bounds scaleBy: scale) expandBy: 128 @ 128.	owner changed.	self layoutChanged! !!DisplayListView methodsFor: 'controls' stamp: 'stp 1/23/2023 11:22'!zoomOut	"Respond to a button press from the control widget"	Sensor shiftPressed		ifTrue: [scale := (scale x / 1.25) @ scale y]		ifFalse: [scale := scale / 1.25].	bounds := (dList bounds scaleBy: scale) expandBy: 128 @ 128.	owner changed.	self layoutChanged! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 02:24:25'!clearSelection
	"Clear the scroller's selection."

	scroller clearSelection! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/14/2023 07:43'!getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	aMenu
		add: 'Inspect model' 					action: #inspectModel;
		add: 'Inspect selection' 				action: #inspectSelection;
		add: 'Clear selection' 					action: #clearSelection;
		add: 'Inspect display list' 				action: #inspectDList.
	^ aMenu! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/22/2023 11:08'!handleKeystroke: anEvent 	controller handleKeystroke: anEvent! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/22/2023 11:05'!handleMouseDown: anEvent	"Answer true if the receiver wishes to handle the given mouse event"	controller handleMouseDown: anEvent! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:25'!handleMouseUp: anEvent 	controller handleMouseUp: anEvent ! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/22/2023 11:05'!handlesKeyboard: evt	"Answer true if the receiver wishes to handle the given keyboard event"	^ true! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:29'!handlesMouseDown: anEvent 	^ true! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/5/2023 15:17'!hasYellowButtonMenu0	self halt.	^ true! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:16'!inspectDList
	"Open an inspector on the model of the DLV"

	dList inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:20'!inspectModel
	"Open an inspector on the model of the DLV"

	model inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 12/2/2022 01:34:25'!inspectSelection
	"Open an inspector on the model of the DLV"

	scroller selection inspect! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/22/2023 11:07'!keyStroke: anEvent	"Handle a keystroke event."	controller handleKeystroke: anEvent! !!DisplayListView methodsFor: 'events' stamp: 'stp 1/10/2023 17:50'!update: aspect 	"Old-school MVC - update redraws"		| canv |	super update: aspect.	canv := Display getCanvas.	canv clipBy: self clippingBounds during: 		[ :canvas |		canvas frameAndFillRectangle: owner bounds fillStyle: backgroundColor borderStyle: self borderStyle.		owner drawSubmorphsOn: canvas]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListView class	instanceVariableNames: ''!!DisplayListView class methodsFor: 'instance creation' stamp: 'stp 1/22/2023 08:35'!newOn: aDList
	"Create a DLV on the given DL"

	| view |
	view := self new initialize.
	view list: aDList.	view model: aDList.
	^view.! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 1/23/2023 11:29'!backgroundColor	^ SolarizedTheme lightBackground! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/1/2022 11:16:11'!constants
	"Answer the class' dictionary of images"
	"DisplayListView constants"
	
	Constants ifNil: [Constants := Dictionary new.].
	^Constants! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/30/2022 07:49'!foregroundColor	^ Color gray! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 1/12/2023 01:32'!glyphs
	"Answer the class' dictionary of images"
	"DisplayListView glyphs"
	
	Glyphs ifNil: [Glyphs := Dictionary new.].
	^Glyphs! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 1/16/2023 16:02'!loadNoteGlyphsFromFontSized: size
	"Load glyphs from a font into a dictionary of opaque forms."
	"DisplayListView loadNoteGlyphsFromFontSized: 100"	"DisplayListView remapNoteHeadGlyphs"	"DisplayVisual displayMusicGlyphs"		| keys |	Glyphs := Dictionary new..	self loadNoteGlyphsSized: size from: 16r00E1D0 to: 16r00E1E7			keys: #('4' '2' '1' '1/2' '1/2d' '1/4' '1/4d' '1/8' '1/8d' '1/16' '1/16d' '1/32' '1/32d'  '1/64' '1/64d' )			offsets: (5 @ 66)			into: #StdNoteIcons.	keys := Array new: 70.	1 to: 70 do: [ :ind | keys at: ind put: ind printString asSymbol].	self loadNoteGlyphsSized: size from: 16r00E0A0 + 31 to: 16r00E0FC			keys: keys 			offsets: (-4 @ -66) "(2 @ -62)"			into: #OtherNoteIcons.			
"	self glyphs explore"! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 1/12/2023 03:13'!loadNoteGlyphsSized: size from: start to: stop keys: keys offsets: offs into: dictNam
	"Load glyphs from a font into a dictionary of opaque forms."
	"DisplayListView loadNoteGlyphsFrom: 16r00E1D0 to: 16r00E1E7			keys: #('4' '2' '1' '1/2' '1/2d' '1/4' '1/4d' '1/8' '1/8d' '1/16' '1/16d' '1/32' '1/32d'  '1/64' '1/64d' )			into: #StdNoteIcons"	"| keys |	keys := Array new: 70.	1 to: 70 do: [ :ind | keys at: ind put: ind printString asSymbol].	DisplayListView loadNoteGlyphsFrom: 16r00E0A0 + 28 to: 16r00E0FC			keys: keys into: #OtherNoteIcons"			
	DisplayListView glyphs at: dictNam put: IdentityDictionary new.	start to: start + keys size - 1 do: [ :item | | img |		img := ((StrikeFont familyName: 'Bravura' size: size) asPointSize: size) characterFormAtCode: item.		(DisplayListView glyphs at: dictNam) 				at: (keys at: item - start + 1) asSymbol 				put: ((DisplayVisual visual: (OpaqueImage image: (img) mask: (img))) offset: offs)].! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 1/16/2023 16:03'!remapNoteHeadGlyphs
	"Make a new map of 16 note heads to use."
	"DisplayListView remapNoteHeadGlyphs"
	"DisplayVisual displayMusicGlyphs"	
	| map |	map := #(29 2 4 9 11 36 61 50 54 1 3 6 10 32 39 46 ).	Glyphs at: #NewNoteHeads put: Dictionary new.	1 to: map size do: [ :ind |		(Glyphs at: #NewNoteHeads) at: ind 							put: ((Glyphs at: #OtherNoteIcons) at: (map at: ind) printString asSymbol)]! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:22:15'!exampleLines
	"Test/example of using a DLV."
	"DisplayListView exampleLines"
	
	| list view |
	list := DisplayList linesX: 2000 byY: 2000.
	view := self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:49:12'!exampleMixed
	"Test/example of using a DLV."
	"DisplayListView exampleMixed"
	
	| list view |
	list := DisplayList mixedExample.
	view := self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/30/2022 17:29:01'!examplePolylines
	"Test/example of using a DLV."
	"DisplayListView examplePolylines"
	
	| list view |
	list := DisplayList polylinesX: 2000 byY: 2000.
	view := self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 1/22/2023 08:34'!exampleRectangles	"Test/example of using a DLV."	"DisplayListView exampleRectangles"		| list view |	list := DisplayList rectanglesX: 2000 byY: 2000.	view := self newOn: list.	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:39:58'!exampleStrings
	"Test/example of using a DLV."
	"DisplayListView exampleStrings"
	
	| list view |
	list := DisplayList stringsX: 2000 byY: 2000.
	view := self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/1/2022 10:54:13'!exampleVisuals
	"Test/example of using a DLV."
	"DisplayListView exampleVisuals"
	
	| list view |
	list := DisplayList visualsX: 2000 byY: 2000.
	view := self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/30/2022 07:44'!openOn: list 	"Test/example of using a DLV."	"DisplayListView openOn: (DisplayList stringsX: 2000 byY: 2000)"	| view |	view := self newOn: list.	DisplayListWindow open: view label: 'DisplayListView'! !SystemWindow subclass: #DisplayListWindow	instanceVariableNames: 'ctrlMorph'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListWindow methodsFor: 'accessing' stamp: 'stp 1/20/2023 18:27'!comeToFront	super comeToFront.	ctrlMorph ifNotNil: 		[ctrlMorph bounds: ((self bounds origin - (78 @ 0)) extent: ctrlMorph bounds extent).		ctrlMorph refreshWorld]."	Transcript show: 'ctf'; cr"! !!DisplayListWindow methodsFor: 'accessing' stamp: 'stp 1/20/2023 16:07'!ctrlMorph: ctrl	"Set the receiver's control morph."		ctrlMorph := ctrl! !!DisplayListWindow methodsFor: 'accessing' stamp: 'stp 1/20/2023 18:46'!delete	super delete.	ctrlMorph ifNotNil: 		[ctrlMorph delete]! !!DisplayListWindow methodsFor: 'accessing' stamp: 'stp 1/20/2023 18:44'!position: newPos	super position: newPos.	ctrlMorph ifNotNil: 		[ctrlMorph bounds: ((self bounds origin - (78 @ 0)) extent: ctrlMorph bounds extent).		ctrlMorph refreshWorld]! !!DisplayListWindow methodsFor: 'focus' stamp: 'stp 1/23/2023 10:47'!activate	"An active window delegate user input events as usual to its contents such as buttons or lists."	super activate.	ctrlMorph ifNotNil: [ctrlMorph comeToFront]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListWindow class	instanceVariableNames: ''!!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 1/20/2023 16:23'!open: aDLV label: lab 	"Open an instance of the window on the given view."	"DisplayListView exampleRectangles"		| window scrollPane ctrl |	window := self new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: 900 @ 500. 	ctrl := DLVControlMorph newOn: aDLV.	window ctrlMorph: ctrl.	 ctrl openNearMorph: window ! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 1/23/2023 10:50'!open: aDLV label: lab extent: ext	"Open an instance of the window on the given view."	" | list view |	list := DisplayList rectanglesX: 2000 byY: 2000.	view := DisplayListView newOn: list.	self open: view label: 'DisplayListView' extent:  900 @ 500"	| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scrollPane alwaysShowScrollBars.	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 1/23/2023 10:50'!open: aDLV label: lab extent: ext scrollBars: scroll 	"Open an instance of the window on the given view."	" | list view |	list := DisplayList rectanglesX: 2000 byY: 2000.	view := DisplayListView newOn: list.	self open: view label: 'DisplayListView' extent:  900 @ 500 scrollBars: false"		| window scrollPane |	window := SystemWindow new.	window setLabel: lab.	scrollPane := ScrollPane new.	scroll		ifTrue: [scrollPane alwaysShowScrollBars]		ifFalse: [scrollPane hideScrollBarsIndefinitely].	scrollPane scroller addMorph: aDLV.	window		addMorph: scrollPane		frame: (0 @ 0 corner: 1 @ 1).	window openInWorldExtent: ext! !Morph subclass: #InnerDisplayListMorph	instanceVariableNames: 'dList selection isDragging dragPos'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!InnerDisplayListMorph methodsFor: 'drawing' stamp: 'stp 1/10/2023 17:42'!drawOn: aCanvas
	"Iterate over my items and display them on the given canvas."

	| off xform owner | self halt.
	off := 0.
	xform := aCanvas currentTransformation.

	aCanvas fillRectangle: self morphLocalBounds 
			color: owner backgroundColor.
	owner clefForm ifNotNil:
		[aCanvas image: owner clefForm at: 0 @ 0.
			off := owner clefForm width].
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

	selection := OrderedCollection new.
	self redrawNeeded! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:30:23'!initialize
	"Initialize the receiver."

	super initialize.
	self setProperty: #handlesMouseStillDown: toValue: true.
	selection := OrderedCollection new.
	^self! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 11/29/2022 16:02:18'!list: aDList
	"Assign the receiver's display list"

	dList := aDList.
	self morphExtent: dList extent! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 02:13:28'!selected
	"Answer the receiver's selected item list."

	^selection isEmpty
		ifTrue: [nil]
		ifFalse: [selection first]! !!InnerDisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/2/2022 00:39:20'!selection
	"Answer the receiver's selected item list."

	^selection! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 11/30/2022 17:02:47'!click: aMouseButtonEvent localPosition: localEventPosition
	"Handle a single-click event. This message is only sent to clients that request it by sending one of the #waitForClicksOrDrag:... messages to the initiating hand in their mouseDown: method. This default implementation does nothing."
	
	self halt.! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 1/22/2023 08:20'!handlesKeyboard: evt	"Return true if the receiver wishes to handle the given keyboard event"	^ true! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 01:08:26'!handlesMouseDown: aMouseButtonEvent
	"Do I want to receive mouseDown events (mouseDown:, mouseMove:, mouseUp:)?"
	
	^true! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 00:55:01'!handlesMouseStillDown: evt
	"Return true if the receiver wants to get repeated #mouseStillDown messages between #mouseDown: and #mouseUp"
	
	^true! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 1/22/2023 08:29'!keyDown: anEvent	"Handle a key down event."	self halt! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/1/2022 23:52:30'!keyStroke: aKeyboardEvent

	self halt.! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:16:14'!mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse-down event."

	| dist sel prevSel |
	dist := 8. 		"hit-detect range"
	dragPos := Sensor peekMousePt.
	isDragging := true.
	prevSel := self selected.
	sel := owner list itemWithin: dist of: localEventPosition.
	sel ifNotNil: [aMouseButtonEvent shiftPressed
			ifFalse: [self clearSelection].
		sel == prevSel ifFalse: [self select: sel].
		^self redrawNeeded].
	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	
"	Transcript show: 'MB1D: ', dragPos printString, '  -  ', sel offset printString; cr.
"! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 01:05:24'!mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	isDragging := false.
! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:23:57'!mouseButton2Activity
	"Invoke the menu"
	
	owner getMenu ifNotNil: [ :menu |
		menu popUpInWorld: self world.]! !!InnerDisplayListMorph methodsFor: 'events' stamp: 'stp 12/2/2022 02:19:05'!mouseStillDown

	| mPos delta |
	Sensor isMouseButton1Pressed ifFalse: [^super mouseStillDown].
	selection ifEmpty: [^self].
	mPos := Sensor peekMousePt.
	delta := mPos - dragPos.
"	Transcript show: 'MSD: m ', mPos printString, '  -   r ', rPos printString, '  -   d ', delta printString,
				 '  -  o ', selection first offset printString; cr.
	self halt."
	dragPos := mPos.
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

	^super new initialize! !!InnerDisplayListMorph class methodsFor: 'class variables' stamp: 'stp 1/23/2023 06:36'!backgroundColor
	"Answer the background color to use."

	^DisplayListView backgroundColor! !