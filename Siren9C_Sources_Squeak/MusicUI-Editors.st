Object subclass: #EventListEditor	instanceVariableNames: 'eventList ostinato defaultEvent'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!EventListEditor commentStamp: '<historical>' prior: 0!Instances of EventListEditor are used as the models of EventListViews.
They hold onto EventLists and send messages to them.

Instance variables:
	list 			<EventList> my EventList (subject)
	ostinato 	<Ostinato> my loop process (if i'm looping)
	selection 	<EventList> The "selected" notes
	ampl 		<Number or Amplitude> My default amplitude
!!EventListEditor methodsFor: 'list access'!add: anAss
	"Add the given EventAssociation to the model list."

	list add: anAss.
	self changed: #added.! !!EventListEditor methodsFor: 'list access' stamp: 'stp 12/31/2022 11:33'!doInspect	"Inspect the receiver's list."	Sensor shiftPressed		ifTrue: [self inspect]		ifFalse: [eventList inspect]! !!EventListEditor methodsFor: 'list access'!eraseList
	"Flush the receiver's list."

	list empty.
	self changed: #deleted.! !!EventListEditor methodsFor: 'list access'!play
	"Play the receiver's eventList."

"	Sensor leftShiftDown 
		ifTrue: [self startStop]
		ifFalse: [list play]
"
	eventList play! !!EventListEditor methodsFor: 'list access'!remove: anEvent
	"Remove the given event from the receiver's list"

"	Transcript show: 'remove: ', anEvent printString; cr."
	list remove: anEvent.
	self changed: #deleted.! !!EventListEditor methodsFor: 'list access'!startStop
	"If not playing, create ostinato and start; if playing, stop and release."

	ostinato == nil
		ifTrue: [ostinato := Ostinato onList: list.
				[ostinato play]]
		ifFalse: [ostinato stop.
				ostinato := nil]! !!EventListEditor methodsFor: 'links and groups'!group: events
	"Group the event list argument within the receiver's list."

	list group: events.
	self changed: #events! !!EventListEditor methodsFor: 'links and groups'!link: anEvent with: anOtherEvent
	"Set up a link between the two events or lists."

	^self! !!EventListEditor methodsFor: 'links and groups'!select: anAorEL
	"Add the given association to the list of selected items."

	| mo |
	anAorEL == nil
		ifTrue: [^selections := nil].
	mo := anAorEL isEvent
		ifTrue: [anAorEL]
		ifFalse: [anAorEL model].
	mo hasItems
		ifTrue: [selections := mo]
		ifFalse: [selections == nil
				ifTrue: [selections := EventList newNamed: EventList nextName ].
			selections add: mo]! !!EventListEditor methodsFor: 'links and groups' stamp: 'stp 12/29/2022 00:17'!selections	"Answer the list of selected items."	^selections! !!EventListEditor methodsFor: 'accessing'!ampl
	"Answer the amplitude of the default event."

	^defaultEvent == nil
		ifTrue: [0]
		ifFalse: [defaultEvent  loudness value]! !!EventListEditor methodsFor: 'accessing'!ampl: newValue
	"Set the amplitude of the default event."

	defaultEvent == nil
		ifTrue: [^self].
	defaultEvent loudness: newValue asInteger.
	self changed: #ampl! !!EventListEditor methodsFor: 'accessing'!defaultEvent
	"Answer the receiver's default event."

	^defaultEvent == nil
		ifTrue: [list defaultEventClass new]
		ifFalse: [defaultEvent]! !!EventListEditor methodsFor: 'accessing'!defaultEvent: anEvent
	"Set the receiver's default event."

	defaultEvent := anEvent! !!EventListEditor methodsFor: 'accessing'!displayList
	"Answer nil."

	^nil! !!EventListEditor methodsFor: 'accessing'!eventList
	"Answer the receiver's subject eventList."

	^eventList! !!EventListEditor methodsFor: 'accessing'!eventList: anEL
	"Set the given EventList as the receiver's model."

	eventList := anEL.
	defaultEvent := eventList defaultEventClass new.
	anEL addDependent: self.
	self changed: #list! !!EventListEditor methodsFor: 'accessing'!items
	"Answer the subject eventList's events."

	^eventList events! !!EventListEditor methodsFor: 'initialize-release'!release
	"Release the receiver's list."

	eventList removeDependent: self.
	list removeDependent: self.
	list release! !Object subclass: #EventListFileViewer	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!EventListFileViewer methodsFor: 'accessing'!printBlurbFor: aFilename on: stream

	self
		print: (#Path << #labels >> 'Path') value: [aFilename asString] on: stream;
		print: (#Size << #labels >> 'Size') value: [aFilename fileSize printString] on: stream;
		print: (#LastModified << #labels >> 'Last modified') 
				value: [self timestampStringAt: #modified in: aFilename dates] on: stream
! !!EventListFileViewer methodsFor: 'actions'!editFile
	"Edit the selected sound file"

	(EventList fromFile: self selectedFilename asString) edit! !!EventListFileViewer methodsFor: 'actions'!loadFile
	"Load the selected file into the SirenSession"

	| nam |
	nam := self selectedFilename asString.
	SirenSession eventList: nam put: (EventList fromFile: nam)! !!EventListFileViewer methodsFor: 'actions'!playFile
	"Play the selected sound file"

	(EventList fromFile: self selectedFilename asString) play! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!EventListFileViewer class	instanceVariableNames: ''!!EventListFileViewer class methodsFor: 'accessing'!isInterestedInFile: aFilename

	('*.ev' match: aFilename tail) ifTrue: [^true].
	('*.sm' match: aFilename tail) ifTrue: [^true].
	('*.mid' match: aFilename tail) ifTrue: [^true].
	('*.midi' match: aFilename tail) ifTrue: [^true].
	^false! !!EventListFileViewer class methodsFor: 'accessing'!tabLabel

	^(#FileInformation << #labels >> 'Score File') asString! !Model subclass: #EventListPanel	instanceVariableNames: 'editorView scoreView textView'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!EventListPanel methodsFor: 'accessing'!editorView
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^editorView isNil
		ifTrue:
			[editorView := EventListDialog new]
		ifFalse:
			[editorView]! !!EventListPanel methodsFor: 'accessing'!scoreView
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^scoreView isNil
		ifTrue:
			[scoreView := ScoreView new]
		ifFalse:
			[scoreView]! !!EventListPanel methodsFor: 'accessing'!textView
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^textView isNil
		ifTrue:
			[textView := EventListText new]
		ifFalse:
			[textView]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!EventListPanel class	instanceVariableNames: ''!!EventListPanel class methodsFor: 'interface specs'!windowSpec
	"Tools.UIPainter new openOnClass: self andSelector: #windowSpec"

	! !Model subclass: #EventListText	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!Model subclass: #EventListTreeEditor	instanceVariableNames: 'scoreView listTree'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!EventListTreeEditor methodsFor: 'accessing'!scoreView
	"Answer up the score view"

	^scoreView! !!EventListTreeEditor methodsFor: 'aspects'!listTree
	"Answer up the event list dictionary tree"

	^listTree! !!EventListTreeEditor methodsFor: 'initialize/release'!initialize
	"EventListTreeEditor open"

	| model |
	scoreView := DisplayListSubcanvas new.
	listTree := SelectionInTree new.
	model := TreeModel on: (EventListTreeItem new lists: SirenSession eventLists)
					displayRoot: false.
	listTree list: model! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!EventListTreeEditor class	instanceVariableNames: ''!!EventListTreeEditor class methodsFor: 'interface specs'!windowSpec
	"Tools.UIPainter new openOnClass: self andSelector: #windowSpec"

	! !!EventListTreeEditor class methodsFor: 'resources' stamp: 'stp 12/31/2022 11:39'!treeMenu	^ PopUpMenu labelArray: #(#copy #cut #paste #again #undo)! !Model subclass: #EventListTreeItem	instanceVariableNames: 'lists'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!EventListTreeItem methodsFor: 'initialize-release'!lists: aCollection

	lists := aCollection! !!EventListTreeItem methodsFor: 'accessing'!children
	
	^lists! !!EventListTreeItem methodsFor: 'accessing'!hasChildren

	^lists notEmpty! !Model subclass: #ScoreView	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!DisplayListController subclass: #TimeSequenceController	instanceVariableNames: 'eventList'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 1/14/2023 07:59'!doInspect
	"Inspect the editor (if leftShiftDown) or the list."

	Sensor shiftDown
		ifTrue: [model inspect]
		ifFalse: [model eventList inspect]! !!TimeSequenceController methodsFor: 'menu messages'!group
	"Group the current selection as a separate sub-event list in the model."

	model selection == nil
		ifTrue: [view flash.
			^self].
	model group: model selection.
	model select: nil! !!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 1/14/2023 08:00'!pan
	"Pan the view in x highlighting the time value until mouse press."

	| time0 startTime now |
	Sensor waitNoButton.
	time0 := Time microsecondClock.
	startTime := view offsetX * view xScale.
	now := startTime.
	[Sensor anyButtonPressed or: [now > model list duration value]]
		whileFalse: [now := startTime + Time microsecondClock - time0.
			view highlightTime: now.
"			Transcript show: now printString; cr."
"			(Delay forMilliseconds: 100) wait."
			view highlightTime: now].
	Sensor waitNoButton! !!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 1/14/2023 12:20'!play
	"Play the list."

	model eventList play.
"	MIDIPort allNotesOffIn: (model eventList duration asSec value + 3)"! !!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 1/19/2023 07:52'!playAll
	"Play the list."

	model play.
"	MIDIPort allNotesOffIn: (model eventList duration asSec value + 3)"! !!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 07:10'!readList	"Copy the model list from the global dictionary."	| name model |	name := FillInTheBlank request: 'EventList name for storing model:' initialAnswer: model list name.	name = ''		ifTrue: [^ self].	(SirenSession eventLists includesKey: name)		ifTrue: [model eventList: (SirenSession eventLists at: name) copy]		ifFalse: [Transcript cr; show: 'No such event list.'; cr]! !!TimeSequenceController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 07:11'!storeList	"Copy the model list into the global dictionary."	| name count model |	name := FillInTheBlank request: 'EventList name for storing model:' initialAnswer: model list name.	name = ''		ifTrue: [^ self].	(SirenSession eventLists includesKey: name)		ifTrue: [(FillInTheBlank confirm: 'Store over existing event list?')				ifTrue: [SirenSession eventLists at: name put: model list copy]				ifFalse: [count := 2.					[SirenSession eventLists includesKey: name , '.' , count printString]						whileTrue: [count := count + 1].					SirenSession eventLists						at: (name , '.' , count printString) asSymbol						put: (SirenSession eventLists at: name).					SirenSession eventLists at: name put: model list copy]]		ifFalse: [SirenSession eventLists at: name put: model list copy]! !!TimeSequenceController methodsFor: 'events' stamp: 'stp 1/14/2023 07:47'!processMenu: event	"Create and run the hierarchical DLV menu."		| menu |	menu := MenuMorph new defaultTarget: self.	menu addItem: [ :item | item contents: 'Play all'; target: self; selector: #playAll].	menu addItem: [ :item | item contents: 'Play selection'; target: self; selector: #playSelection].	menu addItem: [ :item |		item contents: 'Group'; subMenuUpdater: self selector: #groupMenuOn: ].	menu addItem: [ :item |		item contents: 'Inspect'; subMenuUpdater: self selector: #inspectMenuOn: ].	menu addItem: [ :item |		item contents: 'Edit'; subMenuUpdater: self selector: #editMenuOn: ].	menu addItem: [ :item |		item contents: 'Display'; subMenuUpdater: self selector: #displayMenuOn: ].	menu invokeModal! !!TimeSequenceController methodsFor: 'selecting'!selectFromPoint: aPoint toPoint: anotherPoint
	"Hit-detect the model for the given range."

	| collection eList |
	collection := view list itemsFromX: aPoint x toX: anotherPoint x.
	eList := EventList newAnonymous.
	collection do:
		[ :i | 
		i model == nil
			ifFalse: [eList add: i model]].
	model select: eList.
	self select: collection.
	view invalidate! !!TimeSequenceController methodsFor: 'initialize-release'!initializeMenu
	"Set up the receiver's YellowButtonMenu"

	super initializeMenu
"	self menuHolder: (ValueHolder with: (PopUpMenu
			labelList: #(('group' 'inspect') ('play' 'pan') ('redraw') ('store' 'read'))
			values: #(group doInspect play pan redraw storeList readList)))"! !!TimeSequenceController methodsFor: 'initialize-release' stamp: 'stp 1/14/2023 08:09'!view: aView 	super view: aView.	model := aView model.	model ifNotNil: [self halt]! !TimeSequenceController subclass: #PitchTimeController	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!PitchTimeController methodsFor: 'selecting'!selectFromPoint: aPoint toPoint: anotherPoint
	"Hit-detect the model for the given range."

	| collection eList |
	collection := view list itemsWithin: (aPoint corner: anotherPoint).
	collection == nil ifTrue: [^self].
	eList := EventList newAnonymous.
	collection do:
		[ :i | 
		i model == nil
			ifFalse: [eList add: i model]].
	model select: eList.
	self select: collection.
	view invalidate! !DisplayListView subclass: #TimeSequenceView	instanceVariableNames: 'clefForm xScaleDefault xScale xColor yColor headColor clefColor itemAccessors pianoRoll'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!TimeSequenceView commentStamp: 'stp 1/22/2023 12:26' prior: 0!Instances of TimeSequenceView (and its subclasses) are time-line oriented DisplayListViews.
Time may run horizontally (default) or vertically, depending on the layout manager.

Instance Variables:
	clefForm	<Form> the view's clef form or axes (optional), may be staff or scale/grid/ruler
	xScale	<Number> the time-to-x-coordinate scale
	xColor	<ColorValue> the color of the lines for note duration
	yColor	<ColorValue> the color of the lines for note amplitude
	headColor	<ColorValue> the note head color
	clefColor	<ColorValue> the clef color
	itemAccessors	<Symbol or Dictionary of (Voice -> Symbol)> the item accessor(s) to use to generate the display list items for the notes

This implementation is MODE 1.1, STEIM, Amsterdam, May/June 1990; updated at the Lagoon in Palo Alto, July, 1991-May, 1992.

The entirety of this software is Copyright (c) 1990, Stephen Travis Pope, Nomad Object Design/Nomad Computer Music Research. 
All Rights Reserved.!!TimeSequenceView methodsFor: 'initialize-release' stamp: 'stp 1/22/2023 13:17'!initialize
	"Initialize the instance variables of the receiver."

	super initialize.
	xScaleDefault := 15.
	xScale := 15.
	page := 0@0.	pianoRoll := false! !!TimeSequenceView methodsFor: 'initialize-release' stamp: 'stp 1/14/2023 07:41'!initializeController	"Set up the receiver's controller."		controller := TimeSequenceController new.	controller view: self.	controller addDependent: self.! !!TimeSequenceView methodsFor: 'accessing'!clefColor
	"Answer the receiver's 'clefColor'."

	^clefColor! !!TimeSequenceView methodsFor: 'accessing'!clefColor: anObject
	"Set the receiver's 'clefColor' to be anObject."

	clefColor := anObject! !!TimeSequenceView methodsFor: 'accessing'!clefForm
	"Answer the receiver's clef form."

	^clefForm! !!TimeSequenceView methodsFor: 'accessing'!clefWidth
	"Answer the receiver's clef form width."

	^clefForm width! !!TimeSequenceView methodsFor: 'accessing'!headColor
	"Answer the receiver's 'headColor'."

	^headColor! !!TimeSequenceView methodsFor: 'accessing'!headColor: anObject
	"Set the receiver's 'headColor' to be anObject."

	headColor := anObject! !!TimeSequenceView methodsFor: 'accessing' stamp: 'stp 1/22/2023 12:39'!itemsWithin: aRect	"Answer the items within the given rectangle."	^ dList itemsWithin: aRect! !!TimeSequenceView methodsFor: 'accessing' stamp: 'stp 1/22/2023 13:12'!pianoRoll: bool	"Set the flag to use piano-roll notation instead of note heads."	pianoRoll := bool! !!TimeSequenceView methodsFor: 'accessing'!xColor
	"Answer the receiver's 'xColor'."

	^xColor! !!TimeSequenceView methodsFor: 'accessing'!xColor: anObject
	"Set the receiver's 'xColor' to be anObject."

	xColor := anObject! !!TimeSequenceView methodsFor: 'accessing'!xScale
	"Answer the receiver's 'xScale'."

	^xScale! !!TimeSequenceView methodsFor: 'accessing'!xScale: anObject
	"Set the receiver's 'xScale' to be anObject."

	xScale := anObject! !!TimeSequenceView methodsFor: 'accessing'!yColor
	"Answer the receiver's 'yColor'."

	^yColor! !!TimeSequenceView methodsFor: 'accessing'!yColor: anObject
	"Set the receiver's 'yColor' to be anObject."

	yColor := anObject! !!TimeSequenceView methodsFor: 'transforming'!formXFromTime: aTime
	"take a note's time and return a display point x coordinate relative to the form."

	^((aTime - (page x) / xScale) asInteger)! !!TimeSequenceView methodsFor: 'transforming'!timeFromX: aNumber
	"Answer an event list time from the argument."

	^(aNumber x - inset x - self bounds width * xScale + page x) max: 0! !!TimeSequenceView methodsFor: 'layout manager access' stamp: 'stp 1/19/2023 06:39'!generateItemFor: anAss 	"Answer the correct visual item for the given assoc. (sent from layoutManagers). "		| dItem vox arr |"	arr := DisplayListView constants at: #NoteIcons."	arr := DisplayListView glyphs at: #NewNoteHeads.	vox := anAss value voice.	vox == nil		ifTrue: [vox := 1]		ifFalse: [vox isInteger				ifTrue: [vox := (vox max: 1) min: arr size]													"hack -- take the first letter as the key for note heads"				ifFalse: [vox := vox first asInteger \\ arr size]].	dItem := (arr at: vox) copy.			"get the right notehead"	dItem model: anAss.	^ dItem! !!TimeSequenceView methodsFor: 'layout manager access'!itemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	itemAccessors == nil
		ifTrue: [^self generateItemFor: anAss].
	itemAccessors isSymbol
		ifTrue: [^self perform: itemAccessors with: anAss].
	(itemAccessors isMemberOf: BlockClosure)
		ifTrue: [^itemAccessors value: anAss].
	(itemAccessors isMemberOf: Dictionary)
		ifTrue: [^self perform: (itemAccessors at: anAss value voice ifAbsent: [0]) 
						with: anAss].! !!TimeSequenceView methodsFor: 'layout manager access' stamp: 'stp 1/11/2023 13:15'!layoutManager
	"Answer a default layout manager for the receiver."

	^(TimeSequenceLayoutManager new)
			timeScale: xScale;
			timeOffset:  0! !!TimeSequenceView methodsFor: 'displaying' stamp: 'stp 1/23/2023 10:29'!drawOn: gc 	"Display the receiver on its cached form."		| ins |	ins := inset.	gc fillColor: self class backgroundColor.	clefForm == nil		ifFalse: [gc drawImage: clefForm at: 0 @ 0.			ins := clefForm width].	background == nil		ifFalse: [gc drawImage: background at: ins @ 0].	dList drawOn: gc at: ins."	gc  frameRectangle: ((20 @ 20) extent: (20 @ 20)) width: 2 color: Color gray."	model selections == nil		ifFalse: [gc displayRectangularBorder: model selections bounds]! !!TimeSequenceView methodsFor: 'displaying'!highlightFrom: pt1 to: pt2
	"Show selection of the range pt1--pt2."

	| gc |
	gc := self graphicsContext.
	gc paint: DisplayList foregroundColor.
	gc displayRectangularBorder: (pt1 corner: pt2)! !!TimeSequenceView methodsFor: 'displaying'!highlightTime: value
	"Show selection of the time value value."

"	| x |
	x := (value / xScale - page x - cachedForm offset x 
				+ insetDisplayBox left + self class inset x) min: (insetDisplayBox right).
	(x > (insetDisplayBox right - 64))
		ifTrue: [self scroll: ((insetDisplayBox width - 128) negated@0).
			x := x - (insetDisplayBox width - 128)].
	Display fill: ((x @ insetDisplayBox top)
			extent: (2 @insetDisplayBox height))
		rule: Form reverse
		mask: Form black."! !!TimeSequenceView methodsFor: 'displaying'!reallyDisplayOn: gc
	"Display the receiver on its cached form."

	gc paint: self backgroundColor.
	gc displayRectangle: gc clippingBounds.
	gc paint: self foregroundColor.
	gc translateBy: inset.
	clefForm == nil 
		ifFalse: [clefForm displayOn: gc.
				gc translateBy: clefForm width@0].
	background == nil ifFalse: [background displayOn: gc].
	displayList displayOn: gc.
	model selection == nil
		ifFalse: 
			[controller sensor shiftDown ifTrue: [self halt].
			gc displayRectangularBorder: controller selection bounds].
	gc translateBy: inset negated! !!TimeSequenceView methodsFor: 'displaying'!zoom: aPoint
	"Set the receiver's 'zoom' to be aPoint."

	xScale := xScaleDefault * aPoint x.
	Cursor wait showWhile:
		[self displayList: (self layoutManager displayListFor: self).
		super zoom: aPoint]! !!TimeSequenceView methodsFor: 'controller access'!defaultControllerClass
	"Answer the class of the default controller for the receiver."

	^TimeSequenceController! !!TimeSequenceView methodsFor: 'controls' stamp: 'stp 1/22/2023 12:48'!pauseButton	"Respond to a button press from the control widget"		self inform: 'Cannot play time sequences.'! !!TimeSequenceView methodsFor: 'controls' stamp: 'stp 1/22/2023 12:46'!playButton	"Respond to a button press from the control widget"	self inform: 'Cannot play time sequences.'! !!TimeSequenceView methodsFor: 'controls' stamp: 'stp 1/22/2023 12:48'!rewindButton	"Respond to a button press from the control widget"	self inform: 'Cannot play time sequences.'! !!TimeSequenceView methodsFor: 'controls' stamp: 'stp 1/22/2023 12:47'!stopButton	"Respond to a button press from the control widget"	self inform: 'Cannot play time sequences.'! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!TimeSequenceView class	instanceVariableNames: ''!!TimeSequenceView class methodsFor: 'examples'!randomExample
	"Open a sequence view on a random event list."
	"TimeSequenceView randomExample"

	TimeSequenceView openOnEventList: EventList randomExample! !!TimeSequenceView class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 15:59'!openOnEventList: anEventList
	"Open the receiver on the given event list."
	"TimeSequenceView openOnEventList: EventList randomExample"

	^self openOnEventList: anEventList label: self class name! !!TimeSequenceView class methodsFor: 'instance creation' stamp: 'stp 1/22/2023 13:23'!openOnEventList: anEventList label: lab 	"Open the receiver on the given event list."	"TimeSequenceView openOnEventList: EventList randomExample"		 ^ self openOnEventList: anEventList label: lab pianoRoll: false! !!TimeSequenceView class methodsFor: 'instance creation' stamp: 'stp 1/22/2023 13:19'!openOnEventList: anEventList label: lab pianoRoll: roll	"Open the receiver on the given event list."	"PitchTimeView openOnEventList: EventList randomExample label: 'P T V' pianoRoll: true"	| me lay |	me := self new initialize model: (EventListEditor new eventList: anEventList).	me pianoRoll: roll.	lay := me layoutManager.	lay inset: 0@0.	me list: (lay displayListFor: me).	DisplayListWindow open: me label: lab! !!TimeSequenceView class methodsFor: 'constant access'!defaultCacheExtent
	"Answer the default extent of the receiver's instances' cache."

	^600@200! !!TimeSequenceView class methodsFor: 'constant access'!inset
	"Answer the default display inset."

	^16@16! !TimeSequenceView subclass: #PhraseView	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!PhraseView commentStamp: 'stp 1/22/2023 12:25' prior: 0!A PhraseView is not done.!!PhraseView methodsFor: 'layout manager access' stamp: 'stp 1/19/2023 06:03'!generateItemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	| ampV |
	ampV := anAss value loudness value * 32.
	^ (DisplayList new
		model: anAss;
		add: ((DisplayString new)
			offset: 2@12;
			model: anAss);
		add: ((DisplayLine new)
			offset: (0@(26 - ampV));
			extent: (0@ampV));
		add: ((DisplayLine new)
			offset: 0@26;
			extent: (anAss value duration value / xScale - 4)@0))! !!PhraseView methodsFor: 'layout manager access'!itemFor0: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	^(DisplayList new
		model: anAss;
		add: ((DisplayItem new)
			offset: 1@0;
			model: anAss;
			visual: (anAss value phoneme asDisplayText form));
		add: ((DisplayLine new)
"			offset: 0@12;
			extent: 0@(anAss value loudness value * 24 negated);
"			offset: 0@18;
			extent: (anAss value loudness value * 32)@0);
		add: ((DisplayLine new)
			offset: 0@14;
			extent: (anAss value duration value / xScale - 4)@0))! !!PhraseView methodsFor: 'layout manager access'!itemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	| ampV |
	ampV := anAss value loudness value * 32.
	^(DisplayList new
		model: anAss;
		add: ((DisplayString new)
			offset: 2@12;
			model: anAss);
		add: ((DisplayLine new)
			offset: (0@(26 - ampV));
			extent: (0@ampV));
		add: ((DisplayLine new)
			offset: 0@26;
			extent: (anAss value duration value / xScale - 4)@0))! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PhraseView class	instanceVariableNames: ''!!PhraseView class methodsFor: 'examples'!example
	"PhraseView example"
	"NOT WORKING"

	PhraseView openOn:
		((EventListEditor new) eventList: (EventList named: #phrase1) copy)! !TimeSequenceView subclass: #PitchTimeView	instanceVariableNames: 'pitchOffset yScale yScaleDefault'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!PitchTimeView commentStamp: '<historical>' prior: 0!Instances of PitchTimeView (and its subclasses) are time-line oriented DisplayListViews which make the assumption that the y-coordinate is related to pitch.
The relationship may be a function (as in piano-roll and Hauer-Steffens notation), or involve a look-up table (as in CMN).

Instance Variables:
	pitchOffset	<Number> the view's lowest pitch (normally a key number)!!PitchTimeView methodsFor: 'initialize-release' stamp: 'stp 1/22/2023 13:10'!initialize	"Initialize the instance variables of the receiver."		super initialize.	xScale := 12.	xScaleDefault := xScale.	yScale := 6.	yScaleDefault := yScale.	pitchOffset := 32! !!PitchTimeView methodsFor: 'initialize-release' stamp: 'stp 1/14/2023 07:41'!initializeController	"Set up the receiver's controller."		controller := PitchTimeController new.	controller view: self.	controller addDependent: self.! !!PitchTimeView methodsFor: 'controller access'!defaultControllerClass
	"Answer the class of the default controller for the receiver."

	^PitchTimeController! !!PitchTimeView methodsFor: 'transforming' stamp: 'stp 1/22/2023 12:51'!itemsWithin: aRect	"Answer the items within the given rectangle."	^ dList itemsWithin: aRect! !!PitchTimeView methodsFor: 'transforming'!pitchFromY: aPoint
	"Answer the transformed pitch represented by the y-coordinate of the argument."

	^(aPoint y - self class inset y / yScale + pitchOffset) max: 0! !!PitchTimeView methodsFor: 'layout manager access' stamp: 'stp 1/22/2023 13:19'!generateBarItemFor: anAss 	"Answer the correct visual item for the given assoc. (sent from layoutManagers). "	"ColorValue constantNames"	"PitchTimeView openOnEventList: EventList randomExample label: 'P T V' pianoRoll: true"			| yHeight x list event |	event := anAss value.	yHeight := 24.	event duration == nil		ifTrue: [x := 250 / xScale]		ifFalse: [x := event duration asMS value / xScale]."	(event ampl == nil or: [event ampl value == nil])		ifTrue: [y := 0]		ifFalse: [y := (event ampl asRatio value * yHeight) truncated]."	list := DisplayList new model: anAss. 					"create a display list"	list add: (DisplayRectangle new offset: 0 @ 0;			"add the duration line"			extent: x @ (yHeight / 3);			color: (xColor == nil					ifTrue: [Color black]					ifFalse: [xColor])).	^ list! !!PitchTimeView methodsFor: 'layout manager access' stamp: 'stp 1/22/2023 12:56'!generateItemFor: anAss 	"Answer the correct visual item for the given assoc. (sent from layoutManagers). "	"EventList randomSWSSExample2 editPTV"		pianoRoll		ifTrue: [ ^ self generateBarItemFor: anAss ]		ifFalse: [ ^ self generateNoteItemFor: anAss ]! !!PitchTimeView methodsFor: 'layout manager access' stamp: 'stp 1/22/2023 12:54'!generateNoteItemFor: anAss 	"Answer the correct visual item for the given assoc. (sent from layoutManagers). "	"ColorValue constantNames"	"EventList randomSWSSExample2 editPTV"			| yHeight x y list event |"	^super generateItemFor: anAss."		"If you want just the note head and not the lines"	event := anAss value.	yHeight := 24.	event duration == nil		ifTrue: [x := 250 / xScale]		ifFalse: [x := event duration asMS value / xScale].	(event ampl == nil			or: [event ampl value == nil])		ifTrue: [y := 0]		ifFalse: [y := (event ampl asRatio value * yHeight) truncated].	list := DisplayList new model: anAss. 			"create a display list"	list add: (DisplayLine new offset: 0 @ 0;			"add the duration line"			 extent: x @ 0;			color: (xColor == nil					ifTrue: [Color black]					ifFalse: [xColor])).	list add: (DisplayLine new offset: 0 @ 0; 		"add the red volume line"			extent: 0 @ y negated;			color: (yColor == nil					ifTrue: [Color red]					ifFalse: [yColor])).	(event at: #envelope) == nil 						"draw the envelope"		ifFalse: [ | fcn points |			fcn := event at: #envelope.			points := fcn data collect: [ :pt | pt x * x @ (pt y * y negated)].			list add: ((DisplayPolyline offset: 1 @ 0 vertices: points)						color: Color blue darker)].	(event at: #position) == nil 						"show the position"		ifFalse: [list add: (DisplayLine new offset: -2 @ 0;					 extent: 0 @ ((event at: #position)							- 0.5 * yHeight);					color: (yColor == nil							ifTrue: [Color blue darker]							ifFalse: [yColor]))].	(event at: #duty) == nil 							"show the duty cycle"		ifFalse: [list add: (DisplayLine new offset: 0 @ 2;					 extent: (event at: #duty) * x @ 0;					color: (yColor == nil							ifTrue: [Color brown]							ifFalse: [yColor]))].	list add: (super generateItemFor: anAss).		"add the note head"	^ list! !!PitchTimeView methodsFor: 'layout manager access' stamp: 'stp 1/11/2023 13:15'!layoutManager
	"Answer a default layout manager for the receiver."

	^(PitchTimeLayoutManager new)
			timeScale: xScale;
			timeOffset: page x;
			pitchScale: yScale;
			pitchOffset: pitchOffset! !!PitchTimeView methodsFor: 'controls' stamp: 'stp 1/22/2023 17:56'!playButton	"Respond to a button press from the control widget"	model eventList play! !!PitchTimeView methodsFor: 'controls' stamp: 'stp 1/22/2023 17:58'!rewindButton	"Respond to a button press from the control widget"	model eventList index: 1! !!PitchTimeView methodsFor: 'controls' stamp: 'stp 1/22/2023 17:58'!stopButton	"Respond to a button press from the control widget"	model eventList stop! !!PitchTimeView methodsFor: 'displaying'!highlightFrom: pt1 to: pt2
	"Show selection of the range pt1--pt2"

"	pt1 x = pt2 x
		ifTrue: [^self].
	Display border: (pt1 corner: pt2)
		width: 2
		mask: Form gray."! !!PitchTimeView methodsFor: 'displaying'!zoom: aPoint
	"Set the receiver's 'zoom' to be aPoint."

	xScale := xScaleDefault * aPoint x.
	yScale := yScaleDefault * aPoint y.
	Cursor wait showWhile:
		[self displayList: (self layoutManager displayListFor: self).
		super zoom: aPoint]! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!PitchTimeView class	instanceVariableNames: ''!!PitchTimeView class methodsFor: 'examples' stamp: 'stp 1/22/2023 18:01'!cloudExample	"Open a PTView on a dynamic cloud event lists."	"PitchTimeView cloudExample"	"HauerSteffensView cloudExample"		self openOnEventList: (DynamicCloud			dur: 8000			pitch: #(#(60 #to: 84) #(71 #to: 72))			ampl: #(#(90 #to: 100) #(20 #to: 20))			voice: (1 to: 4)			density: 10) eventList label: 'Dynamic Cloud Event Generator'! !!PitchTimeView class methodsFor: 'examples' stamp: 'stp 1/22/2023 18:07'!cloudsExample	"Open a HSView on two merged DynamicClouds with timing and duration quantized."	"PitchTimeView cloudsExample2"			| list1 list2 mix |	list1 := (DynamicCloud			dur: 8 sec			pitch: { (60 to: 84) . (71 to: 72) }			ampl: { (90 to: 100) . (20 to: 20) }			voice: (1 to: 4)			durations: #(125 125 250 250 250 250 500)) eventList.	list2 := (DynamicCloud			dur: 8 sec			pitch: { (60 to: 84) . (71 to: 72) }			ampl: { (90 to: 100) . (20 to: 20) }			voice: (1 to: 4)			durations: #(250 250 500 500 500)) eventList.															"justify the start times and durations to a grid"	list1 roundStartTimesTo: 200 msec.	list1 roundDurationsTo: 100 msec.	list1 patchTimes.										"make the notes non-overlapping and remove rests"	list2 roundStartTimesTo: 200 msec.	list2 roundDurationsTo: 100 msec.	list2 patchTimes.	mix := EventList new addAll: list1; addAll: list2.		"sum the lists into 1"															"open a notation editor on the event list"	self openOnEventList: mix label: 'Dynamic Clouds'! !!PitchTimeView class methodsFor: 'examples' stamp: 'stp 1/22/2023 18:04'!cloudsExample2	"Open a HSView on two merged DynamicClouds with timing and duration quantized."	"PitchTimeView cloudsExample2"			| list1 list2 mix |	list1 := (DynamicCloud			dur: 8 sec			pitch: { (60 to: 84) . (71 to: 72) }			ampl: { (90 to: 100) . (20 to: 20) }			voice: (1 to: 4)			durations: #(125 125 250 250 250 250 500)) eventList.	list2 := (DynamicCloud			dur: 8 sec			pitch: { (60 to: 84) . (71 to: 72) }			ampl: { (90 to: 100) . (20 to: 20) }			voice: (1 to: 4)			durations: #(250 250 500 500 500)) eventList.															"justify the start times and durations to a grid"	list1 roundStartTimesTo: 200 msec.	list1 roundDurationsTo: 100 msec.	list1 patchTimes.										"make the notes non-overlapping and remove rests"	list2 roundStartTimesTo: 200 msec.	list2 roundDurationsTo: 100 msec.	list2 patchTimes.	mix := EventList new addAll: list1; addAll: list2.		"sum mthe lists into 1"															"open a notation editor on the event list"	self openOnEventList: mix label: 'Dynamic Clouds'! !!PitchTimeView class methodsFor: 'examples'!randomExample
	"Open a p-t view on a random event list."
	"PitchTimeView randomExample"

	PitchTimeView openOnEventList: EventList randomExample! !!PitchTimeView class methodsFor: 'examples' stamp: 'stp 1/16/2023 16:06'!randomExample2
	"Open a p-t view on a random event list with mult. voices."
	"PitchTimeView randomExample2"
	
	| list rand |
	list := EventList randomExample."	list addAll: EventList randomExample.
"	rand := Random new.	list eventsDo: [ :ev | ev voice: (rand nextIntFrom: 1 to: 8)].
	PitchTimeView openOnEventList: list! !!PitchTimeView class methodsFor: 'examples'!randomExampleLong
	"Open a p-t view on a random event list."
	"PitchTimeView randomExampleLong"

	| list |
	list := EventList randomExample: 1024.
	list addAll: (EventList randomExample: 1024).
	PitchTimeView openOnEventList: list! !!PitchTimeView class methodsFor: 'instance creation'!label
	"Answer the view label for the receiver's instances."

	^'Pitch-Time Editor'! !!PitchTimeView class methodsFor: 'subview creation'!addButtonsOn: me for: dled to: container in: rect
	"Add the default button panel to the instance in the given rectangle."

	| x0 y0 x1 y1 |
	x0 := rect left.	x1 := rect right.
	y0 := rect top.	y1 := rect bottom.
	self addButtonOn: me labeled: 'Play'
		doing: [:model :value | dled play] 
		in: (LayoutFrame new leftFraction: 0; topFraction: y0; 
							rightFraction: 0.12;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'File'
		doing: [:model :value | dled doFile] 
		in: (LayoutFrame new leftFraction: 0.12; topFraction: y0; 
							rightFraction: 0.25;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Edit'
		doing: [:model :value | dled doEdit] 
		in: (LayoutFrame new leftFraction: 0.25;	topFraction: y0; 
							rightFraction: 0.37;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Inspect'
		doing: [:model :value | dled doInspect] 
		in: (LayoutFrame new leftFraction: 0.37;	topFraction: 0.0; 
							rightFraction: 0.5;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Display'
		doing: [:model :value | dled doDisplay] 
		in: (LayoutFrame new leftFraction: 0.5;	topFraction: y0; 
							rightFraction: 0.62;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Zoom'
		doing: [:model :value | dled doZoom] 
		in: (LayoutFrame new leftFraction: 0.62;	topFraction: y0; 
							rightFraction: 0.75;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'View'
		doing: [:model :value | dled doViewEdit] 
		in: (LayoutFrame new leftFraction: 0.75;	topFraction: y0; 
							rightFraction: 0.87;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Properties'
		doing: [:model :value | dled doPropEdit] 
		in: (LayoutFrame new leftFraction: 0.87;	topFraction: y0; 
							rightFraction: 1.0;	bottomFraction: y1) 
		to: container.! !PitchTimeView subclass: #HauerSteffensView	instanceVariableNames: 'formY tempoScale'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!HauerSteffensView commentStamp: 'stp 1/11/2023 14:13' prior: 0!A HauerSteffensView is a notation editor based on the linear-pitch music notation from Hauer and Steffens.!!HauerSteffensView methodsFor: 'initialize-release' stamp: 'stp 1/23/2023 06:35'!initialize	"Initialize the instance variables of the receiver."		super initialize.	inset := 0@0.	formY := 500.	xScale := 8.	xScaleDefault := xScale.	self initForm.	pitchOffset := 20.	tempoScale := 50.	self backgroundColor: self class backgroundColor! !!HauerSteffensView methodsFor: 'private' stamp: 'stp 12/2/2022 05:03:16'!clef: anInteger at: aPoint bottom: aBottom form: cForm
	"Display a C clef for the given octave at the given point."
self halt.
	(aPoint y + 20) < aBottom ifTrue: 
		[(Text string: ('c', anInteger printString) emphasis: 2) 
				displayOn: cForm at: aPoint + (2 @ -3)]! !!HauerSteffensView methodsFor: 'private' stamp: 'stp 1/19/2023 14:30'!drawStaff	"Display a Hauer-Stephens-type staff onto the given pixmap."		| box top1 bottom1 left1 right1 left2 right2 aGC cGC aNum count octave co wp |	box := background boundingBox.	co := Color darkGray.	wp := 1.	aGC := background getCanvas.	cGC := clefForm getCanvas.	top1 := box top.	bottom1 := box bottom.	left1 := box left.	right1 := box right.	left2 := clefForm boundingBox left.	right2 := clefForm boundingBox right.											"count number of octaves"	octave := (bottom1 - top1 / (3 + (yScale * 8) + yScale)) asInteger - 2.	aNum := top1.	count := 3.	[aNum < bottom1]					"loop from top1 to bottom1 drawing lines"		whileTrue: [aGC line: left1 @ aNum to: right1 @ aNum width: wp color: co.			cGC line: left2 @ aNum to: right2 @ aNum width: wp color: co.			count := count + 1.			count \\ 5 = 1					"skip @ c"				ifTrue: [aNum + 20 < bottom1						ifTrue: [ | str |							str := 'c' , octave printString.							cGC drawString: str at: left1 @ aNum + (4 @ 0) 									font: (StrikeFont familyName: 'Amaranth' pointSize: 12) color: co].							octave == 3 		"dashed blue line for middle C"								ifTrue: [aGC line: left1 @ aNum + yScale to: right1 @ aNum + yScale width: 1 color: Color blue									dashLength: 10 secondColor: Color transparent secondDashLength: 60 startingOffset: 0].					octave := octave - 1.					aNum := aNum + yScale].			count \\ 5 = 4					"skip @ e"				ifTrue: [aNum := aNum + yScale].			aNum := aNum + (yScale * 2)].	cGC line: right2 - 2 @ 0 to: right2 - 2 @ bottom1 width: wp color: co			"draw left-most measure line"! !!HauerSteffensView methodsFor: 'private' stamp: 'stp 1/11/2023 15:18'!initForm
	"Copy the default backgroundForm and draw a H-S staff on it."

	| depth |
	depth := 32.
	background := Form extent: 2400@formY depth: depth.
	background fillColor: self backgroundColor.
	clefForm := Form extent: 32@formY depth: depth.
	clefForm fillColor: self backgroundColor.
	yScale == nil
		ifFalse: [self drawStaff]! !!HauerSteffensView methodsFor: 'layout manager access' stamp: 'stp 1/22/2023 13:16'!generateItemFor: anAss 	"Answer the correct visual item for the given assoc. (sent from layoutManagers). "	"HauerSteffensView scaleExample"		| dItem dur base keys arr dict which dVis scale |	arr := DisplayListView glyphs at: #StdNoteIcons.	dur := anAss value duration asMsec value.	base := tempoScale.						"select the note-head based on the note duration - does not create dotted notes"	dict := ((base -> '1/32' asSymbol), 			(base * 2 -> '1/16' asSymbol), 			(base * 4 -> '1/8' asSymbol), 			(base * 8 -> '1/4' asSymbol), 			(base * 16 -> '1/2' asSymbol), 			(base * 32 -> '1' asSymbol)).	keys := dict keys asSortedCollection.	which := 1.	[(keys at: which) < dur and: [which < keys size]] whileTrue: [which := which + 1].	dVis := (arr at: (dict at: (keys at: which))).	scale := 1.5."	dItem := DisplayVisual image: (dVis image scaledBy: scale) offset: (dVis offset * scale)."	dItem := DisplayVisual image: dVis image offset: dVis offset.	dItem model: anAss.	^ dItem! !!HauerSteffensView methodsFor: 'layout manager access' stamp: 'stp 1/11/2023 15:36'!itemFor: anAss	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."	"ColorValue constantNames "	"PitchTimeView openOnEventList: (EventList scaleFrom: 48 to: 84 in: 8)"	| yHeight list event |	event := anAss value.	yHeight := 24.	list := (DisplayList new) model: anAss.	list add: (self generateItemFor: anAss).	^list! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!HauerSteffensView class	instanceVariableNames: ''!!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 1/22/2023 18:06'!cloudsExample
	"Open a HSView on two merged random event lists - quantized to 50 msec timing"
	"HauerSteffensView cloudsExample"
		
	| list1 list2 mix |	list1 := (DynamicCloud			dur: 8 sec			pitch: #(#(60 #to: 84) #(71 #to: 72))			ampl: #(#(90 #to: 100) #(20 #to: 20))			voice: (1 to: 4)			durations: #(125 125 250 250 500)) eventList.	list2 := (DynamicCloud			dur: 8 sec			pitch: #(#(60 #to: 84) #(71 #to: 72))			ampl: #(#(90 #to: 100) #(20 #to: 20))			voice: (1 to: 4)			durations: #(250 250 500 500 500)) eventList.															"justify the start times and durations to a grid"	list1 roundStartTimesTo: 200 msec.	list1 roundDurationsTo: 100 msec.	list1 patchTimes.										"make the notes non-overlapping and remove rests"	list2 roundStartTimesTo: 200 msec.	list2 roundDurationsTo: 100 msec.	list2 patchTimes.	mix := EventList new addAll: list1; addAll: list2.		"sum mthe lists into 1"															"open a notation editor on the event list"	HauerSteffensView openOnEventList: mix label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 12/2/2022 05:55:25'!randomExample
	"Open a HSView on two merged random event lists."
	"HauerSteffensView randomExample"
	"HauerSteffensView openOnEventList: EventList new"
		
	HauerSteffensView openOnEventList:
		((EventList newNamed: #pRand)
				addAll: (EventList randomExample);
				addAll: (EventList randomExample)) label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 12/2/2022 11:53:39'!randomExample2
	"Open a HSView on an event generator's event list."
	"HauerSteffensView randomExample2"
	
	HauerSteffensView openOnEventList: DynamicCloud focusExample label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 1/12/2023 04:48'!randomExampleQ
	"Open a HSView on two merged random event lists - quantized to 50 msec timing and a pentatonic scale."
	"HauerSteffensView randomExampleQ"
		
	| list1 list2 mix |	list1 := EventList randomExample: 64 				"create 2 event lists in different pitch ranges"			properties: #(duration: loudness: voice: pitch:)			ranges: { (300 to: 800) . (40 to: 100) . (1 to: 1) . (44 to: 56) }.	list2 := EventList randomExample: 64 			properties: #(duration: loudness: voice: pitch:)			ranges: { (150 to: 450) . (40 to: 100) . (1 to: 1) . (66 to: 80) }.															"justify the start times and durations to a grid"	list1 roundStartTimesTo: 200 msec.	list1 roundDurationsTo: 100 msec.	list1 patchTimes.										"make the notes non-overlapping and remove rests"	list2 roundStartTimesTo: 200 msec.	list2 roundDurationsTo: 100 msec.	list2 patchTimes.	mix := EventList new addAll: list1; addAll: list2.		"sum mthe lists into 1"															"open a notation editor on the event list"	HauerSteffensView openOnEventList: mix label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'examples'!randomSWSSExample
	"Open a HSView on two merged random event lists."
	"HauerSteffensView randomSWSSExample"

	HauerSteffensView openOnEventList:
		((EventList newNamed: #sRand)
			addAll: (EventList randomSWSSExample2);
			addAll: (EventList randomSWSSExample2);
			addAll: (EventList randomSWSSExample2))! !!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 1/12/2023 04:34'!scaleExample	"Open a HSView on a simple chromatic scale event list."	"HauerSteffensView scaleExample"		HauerSteffensView		openOnEventList: (EventList				scaleFrom: 60				to: 72				in: 4.0)		label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'examples' stamp: 'stp 1/12/2023 04:44'!scaleExample2	"Open a HSView on a simple major scale event lists."	"HauerSteffensView scaleExample2"		| scale |	scale := (N mi majorScale octave: 3) collect: [ :no | no semiToneCount].	scale addAllLast: ((N mi majorScale octave: 4) collect: [ :no | no semiToneCount]).	scale addLast: 88.					"add the final E by hand."		HauerSteffensView		openOnEventList: (EventList fromPitches: scale duration: 280 msec)		label: 'Hauer-Steffens Music Notation'! !!HauerSteffensView class methodsFor: 'instance creation'!label
	"Answer the view label for the receiver's instances."

	^'Hauer-Steffens Event List View'! !!HauerSteffensView class methodsFor: 'instance creation' stamp: 'stp 1/11/2023 15:44'!new	^super new initialize! !!HauerSteffensView class methodsFor: 'constant access' stamp: 'stp 1/11/2023 13:22'!defaultCacheExtent
	"Answer the default extent of the receiver's instances' cache."

	^4000@600! !!HauerSteffensView class methodsFor: 'constant access'!inset
	"Answer the default display inset."

	^16@16! !PitchTimeView subclass: #PositionTimeView	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Editors'!!PositionTimeView methodsFor: 'layout manager access'!layoutManager
	"Answer a default layout manager for the receiver."

	^(PositionTimeLayoutManager new)
			timeScale: xScale;
			timeOffset: page x;
			pitchScale: yScale;
			pitchOffset: pitchOffset! !!PositionTimeView methodsFor: 'initialize release' stamp: 'stp 12/2/2022 05:39:16'!initialize
	"Initialize the instance variables of the receiver."

	super initialize.
	xScale := 15.
	yScale := 60.
	pitchOffset := 40! !