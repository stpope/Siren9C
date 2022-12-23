Object subclass: #DisplayListController	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!RectangleMorph subclass: #DisplayListMorph	instanceVariableNames: 'dList backgroundColor background page'	classVariableNames: 'Constants'	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListMorph methodsFor: 'accessing' stamp: 'stp 12/12/2022 07:47'!background	"Answer the receiver's background form or color, if any"	^background! !PluggableScrollPane subclass: #DisplayListView	instanceVariableNames: 'dList backgroundColor background page'	classVariableNames: 'Constants'	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:20:16'!background
	"Answer the receiver's background form or color, if any"

	^background! !!DisplayListView methodsFor: 'accessing'!background: anObject	"Set the receiver's 'background' to be anObject."	background := anObject! !!DisplayListView methodsFor: 'accessing'!backgroundColor	"Answer the receiver's backgroundColor or the default."	^backgroundColor == nil		ifTrue: [self class backgroundColor]		ifFalse: [backgroundColor]! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 10:23:54'!backgroundColor: back
	"Set the receiver's background color, if any"

	^backgroundColor _ back! !!DisplayListView methodsFor: 'accessing'!boundingBox	"Answer the displayList's boundingBox."	| rect |	displayList == nil		ifTrue: [^super boundingBox].	rect := displayList boundingBox.	^(rect origin) extent: (rect extent + (self class inset * 2))! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/2/2022 05:29:29'!clefForm
	"Answer the receiver's clef form."

	^nil! !!DisplayListView methodsFor: 'accessing'!displayList: aDisplayList	"Set the receiver's display list."	self list: aDisplayList! !!DisplayListView methodsFor: 'accessing'!foregroundColor	"Answer the receiver's foregroundColor or the default."	^foregroundColor == nil		ifTrue: [self class foregroundColor]		ifFalse: [foregroundColor]! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 12/12/2022 07:47'!innerMorphClass
	"Answer the class to use for the inner morph."

	^DisplayListMorph! !!DisplayListView methodsFor: 'accessing'!isColor	"Answer whether or not to use colors in displaying."	^Display depth > 1! !!DisplayListView methodsFor: 'accessing'!list	"Answer the receiver's display list."	^displayList! !!DisplayListView methodsFor: 'accessing'!list: aDisplayList	"Set the receiver's display list."	| ext |	displayList := aDisplayList.	ext := aDisplayList extent max: (self class defaultCacheExtent).	form := Form extent: ext depth: 8! !!DisplayListView methodsFor: 'accessing' stamp: 'stp 07/09/97 00:04'!model: aDisplayListEditor	"Accept the given display list editor as the model of the view."	| ext |	super model: aDisplayListEditor.	displayList := model list.	aDisplayListEditor displayList isNil		ifFalse: [ext := aDisplayListEditor displayList extent max: (self class defaultCacheExtent).			form := Form extent: ext depth: 8].	 "default backing store size--dangrous for huge lists"! !!DisplayListView methodsFor: 'accessing'!offsetX	"Answer the receiver's 'offsetX.'"	^scrollOffset x! !!DisplayListView methodsFor: 'accessing'!offsetY	"Answer the receiver's 'offsetY.'"	^scrollOffset y! !!DisplayListView methodsFor: 'accessing'!scaleX	"Answer the receiver's 'scaleX.'"	^scaleX! !!DisplayListView methodsFor: 'accessing'!scaleX: aNumber	"Set the receiver's 'scaleX' to be aNumber."	scaleX = aNumber ifTrue: [^self].	scaleX := aNumber.	self invalidate! !!DisplayListView methodsFor: 'accessing'!scaleY	"Answer the receiver's 'scaleY.'"	^scaleY! !!DisplayListView methodsFor: 'accessing'!scaleY: aNumber	"Set the receiver's 'scaleY' to be aNumber."	scaleY = aNumber ifTrue: [^self].	scaleY := aNumber.	self invalidate! !!DisplayListView methodsFor: 'accessing'!setScrollOffsetHolder: aValueHolder	scrollOffset := aValueHolder.! !!DisplayListView methodsFor: 'drawing' stamp: 'stp 11/30/2022 17:09:51'!drawOn: aCanvas
	"Iterate over my items and display them on the given canvas."

	self scroller drawOn: aCanvas
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

	scroller selection inspect! !!DisplayListView methodsFor: 'scrolling'!relativePointFor: aPoint	"Answer the transformed point representing the argument (faster than doing a full windowing transformation)"	^aPoint - self insetDisplayBox origin + scrollOffset! !!DisplayListView methodsFor: 'scrolling' stamp: 'stp 01/21/98 21:58'!scrollBy: aPoint	"Scroll the receiver's display by the given point."	scrollOffset := scrollOffset - aPoint.	scrollOffset := scrollOffset max: (0@0)."	scrollOffset := scrollOffset min: (form extent - self insetDisplayBox extent)."	self displayView! !!DisplayListView methodsFor: 'displaying'!display: box onForm: theForm	"Display the receiver's display list on the cached form."	((scaleX = 1) & (scaleY = 1))		ifTrue: [displayList displayOn: theForm at: 0@0 clippingBox: box]		ifFalse: [(displayList scaledBy: scaleX@scaleY) displayOn: theForm at: 0@0 clippingBox: box].! !!DisplayListView methodsFor: 'displaying' stamp: 'stp 01/23/98 19:17'!displaySelection: sel on: aDisplayMedium at: aPoint	"Display little white-bordered black 'handles' at the corners of the selection."	sel == nil ifTrue: [^self].	sel selectionHandles do:		[ :pt |		SelectionHandle displayOn: aDisplayMedium at: (pt + aPoint - (3@3))]! !!DisplayListView methodsFor: 'displaying'!displaySelectionOn: aDisplayMedium at: origin clippingBox: box	"Display little white-bordered black 'handles' at the corners of the selection."	| sel |	sel := model selection.	sel == nil ifTrue: [^self].	sel hasItems		ifFalse: [self displaySelection: sel on: aDisplayMedium at: origin]		ifTrue: [sel do:			[ :item | self displaySelection: item on: aDisplayMedium at: origin]]! !!DisplayListView methodsFor: 'displaying'!displayView	"Display the receiver's background form and display list on the given medium."	| box |	super displayView.	box := form boundingBox.	form fillColor: self backgroundColor.	background == nil		ifFalse: [background displayOn: form].	self display: box onForm: form.	self displaySelectionOn: form at: 0@0 clippingBox: box.	form displayOn: Display at: (self insetDisplayBox origin - scrollOffset) clippingBox: self insetDisplayBox! !!DisplayListView methodsFor: 'initialize-release'!initialize	"Set up the default values of the receiver."	super initialize.	scaleX := 1.	scaleY := 1.	scrollOffset := 0@0! !!DisplayListView methodsFor: 'initialize-release'!initializeWithGrid: gPoint color: gColor	"Initialize the receiver's instance variables--the default is to cache."	| cacheE line color |	self initialize.	cacheE := self class defaultCacheExtent.	background := Form extent: cacheE depth: Display depth.	background fillColor: DisplayListView backgroundColor.	gColor isNil		ifTrue: [color := Color black]		ifFalse: [color := gColor].	line _ BitBlt		destForm: background		sourceForm: nil		fillColor: color		combinationRule: Form over		destOrigin: 0@0		sourceOrigin: 0@0		extent: 1@1		clipRect: (0@0 extent: cacheE).	gPoint x to: cacheE x by: gPoint x do:		[ :x | line drawFrom: (x@0) to: (x @ cacheE y)].	gPoint y to: cacheE y by: gPoint y do:		[ :y | line drawFrom: (0@y) to: (cacheE x @ y)].! !!DisplayListView methodsFor: 'initialize-release'!release	"Remove the receiver as a dependent of its model."	model release.	model removeDependent: self.	model := nil.	displayList release.	super release! !!DisplayListView methodsFor: 'controller access'!defaultControllerClass	^DisplayListController! !!DisplayListView methodsFor: 'updating'!update: anAspect with: anArg	"Echo the model's changes, assuming a view is a dependent of the receiver."	anAspect == #scaleX		ifTrue: [^scaleX := scaleX * anArg].	anAspect == #scaleY		ifTrue: [^scaleY := scaleY * anArg].	anAspect == #selection		ifTrue: [^self displayView].	super update: anAspect with: anArg! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListView class	instanceVariableNames: ''!!DisplayListView class methodsFor: 'instance creation' stamp: 'stp 11/29/2022 14:08:17'!newOn: aDList
	"Create a DLV on the given DL"

	| view |
	view _ self new initialize.
	view list: aDList.
	^view.! !!DisplayListView class methodsFor: 'instance creation'!on: anEditor	| me |	me := self new initialize.	me model: anEditor.	^me! !!DisplayListView class methodsFor: 'instance creation'!onList: dlist	"DisplayListView example"	^self onList: dlist size: 400@300! !!DisplayListView class methodsFor: 'instance creation'!onList: dlist size: extent	"Open a display list view on the given list."	"DisplayListView example"	^self onList: dlist size: extent withGrid: nil inColor: nil! !!DisplayListView class methodsFor: 'instance creation'!onList: dlist size: extent withGrid: gPoint inColor: gColor 	"Open a DLView's window with a background grid"	"DisplayListView onList: (DisplayList exampleHierarchical) 			size: 400@300 withGrid: 32@32 inColor: Color lightGray"	| topView container me wrapper editor |	editor := DisplayListEditor on: dlist.	me := self on: editor.	gPoint == nil		ifFalse: [me initializeWithGrid: gPoint color: gColor].	topView := StandardSystemView new.	topView model: editor.	topView label: 'Display List Editor'.	topView minimumSize: extent.	topView borderWidth: 2.	topView addSubView: me.	topView controller open! !!DisplayListView class methodsFor: 'instance creation'!openOnList: dlist	"IndentedListLayoutManager listExample display"	"DisplayListView openOnList: (DisplayList linesX: 600 byY: 400)"	| topView me editor |	editor := DisplayListEditor on: dlist.	me := self on: editor.	topView := StandardSystemView new.	topView model: editor.	topView label: 'Display List Editor'.	topView minimumSize: 300@200.	topView maximumSize: 640@480.	topView borderWidth: 2.	topView addSubView: me.	topView controller open! !!DisplayListView class methodsFor: 'instance creation'!openOnList: dlist size: extent withGrid: gPoint inColor: gColor 	"Open a DLView's window with a background grid"	"DisplayListView openOnList: (DisplayLine linesX: 800 byY: 800) 			size: 400@300 withGrid: 64@32 inColor: Color lightGray"	| topView container me wrapper editor |	editor := DisplayListEditor on: dlist.	me := self on: editor.	gPoint == nil		ifFalse: [me initializeWithGrid: gPoint color: gColor].	topView := StandardSystemView new.	topView model: editor.	topView label: 'Display List Editor'.	topView minimumSize: extent.	topView borderWidth: 2.	topView addSubView: me.	topView controller open! !!DisplayListView class methodsFor: 'examples'!classListExample	"DisplayListView classListExample"	IndentedListLayoutManager listExample display! !!DisplayListView class methodsFor: 'examples'!classTreeExample	"DisplayListView classTreeExample"	(IndentedTreeLayoutManager listExample) display! !!DisplayListView class methodsFor: 'examples'!colorClassListExample	"DisplayListView colorClassListExample"	IndentedListLayoutManager colorListExample! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 10/14/97 02:02'!example1	"Display lines"	"DisplayListView example1"	DisplayListView openOnList: (DisplayLine linesX: 600 byY: 600)! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 10/14/97 02:03'!example2	"Display rectangles."	"DisplayListView example2"	DisplayListView openOnList: (DisplayRectangle rectanglesX: 800 byY: 800)! !!DisplayListView class methodsFor: 'examples'!exampleGridded	"Open an example of a gridded-background display list view on the example hierarchical display list."	"DisplayListView exampleGridded"	DisplayListView onList: (DisplayList exampleHierarchical)			size: 400@300			withGrid: 32@32			inColor: Color lightGray! !!DisplayListView class methodsFor: 'examples'!exampleHierarchical	"DisplayListView exampleHierarchical"	self onList: (DisplayList exampleHierarchical) size: 400@400! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:22:15'!exampleLines
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
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 12/12/2022 07:55'!exampleRectangles
	"Test/example of using a DLV."
	"DisplayListView exampleRectangles"
	
	| list view win |
	list := DisplayList rectanglesX: 2000 byY: 2000. 
	view := self newOn: list.
	win := DisplayListWindow labelled: 'DisplayListView'.	win addMorph: view.	win openInWorldExtent: (600 @ 400)! !!DisplayListView class methodsFor: 'examples' stamp: 'stp 11/29/2022 16:39:58'!exampleStrings
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
	DisplayListWindow open: view label: 'DisplayListView'! !!DisplayListView class methodsFor: 'class variables' stamp: 'stp 12/1/2022 11:16:11'!constants
	"Answer the class' dictionary of images"
	"DisplayListView constants"
	
	Constants ifNil: [Constants _ Dictionary new.].
	^Constants! !!DisplayListView class methodsFor: 'constant access'!backgroundColor	"Answer the default background color for display list views."	"ColorValue constantNames"	^Display depth > 1		ifTrue: [Color gray]		ifFalse: [Color white]! !!DisplayListView class methodsFor: 'constant access'!defaultCacheExtent	"Answer the default extent of the receiver's instances' cache."	^600@600! !!DisplayListView class methodsFor: 'constant access'!foregroundColor	"Answer the default foreground color for display list views."	^Display depth > 1		ifTrue: [Color yellow]		ifFalse: [Color black]! !!DisplayListView class methodsFor: 'constant access'!inset	"Answer the default display inset."	^0@0! !!DisplayListView class methodsFor: 'constant access'!selectionHandle	"Answer the class's selection handle pixmap"	^SelectionHandle! !!DisplayListView class methodsFor: 'documentation' stamp: 'stp 01/21/98 21:50'!README	"This is a very simple display list editor (i.e., structured graphics drawing program) that lets one scroll, select, and re-size items in a display list. It is not intended for general use, but rather to serve as the basis for more sophisticated tools that are implemented as subclasses.	The red mouse botton selects, drage, and rubber-bands items in the display list.	The yellow button brings up a simple menu of basic functions (only redraw has been tested so far).	The blue button is used for 'drag-scrolling' of the view--try it!!"	self flag: #Documentation! !!DisplayListView class methodsFor: 'documentation'!about	"Answer a string that describes the DisplayListView application."	^'The DisplayListView is a general-purpose viewer/editor for MODE structured graphics display lists.	The controller''s middle-button popup menu includes items for inspecting, redrawing, or adjusting the zoom or grid.'! !!DisplayListView class methodsFor: 'class initialization' stamp: 'stp 01/23/98 19:16'!initialize	"Set up the class variable."	"DisplayListView initialize"	SelectionHandle := Form extent: 6@6.	SelectionHandle fill: ((0@0) extent: (6@6)) fillColor: Color white.	SelectionHandle fill: ((1@1) extent: (4@4)) fillColor: Color black.! !SystemWindow subclass: #DisplayListWindow	instanceVariableNames: ''	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-DisplayListViews'!!DisplayListWindow methodsFor: 'initialization' stamp: 'stp 12/15/2022 14:01'!buildMorphicWindow
	"Set up the receiver window"
self halt.
	self layoutMorph addMorph: model layoutSpec: LayoutFrame useAll! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!DisplayListWindow class	instanceVariableNames: ''!!DisplayListWindow class methodsFor: 'instance creation' stamp: 'stp 11/29/2022 13:18:38'!open: aDLV label: lab
	"Open an instance of the window on the given view."
	"DisplayListView displayListViewExample"
	
	super open: aDLV label: lab! !DisplayListView initialize!