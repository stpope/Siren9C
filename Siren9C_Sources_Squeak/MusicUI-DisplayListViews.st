Object subclass: #DisplayListController
	"Answer the receiver's background form or color, if any"

	^background! !
	"Set the receiver's background color, if any"

	^backgroundColor _ back! !
	"Answer the receiver's clef form."

	^nil! !
	"Answer the class to use for the inner morph."

	^DisplayListMorph! !
	"Iterate over my items and display them on the given canvas."

	self scroller drawOn: aCanvas
! !
	"Clear the scroller's selection."

	scroller clearSelection! !
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu
		add: 'Inspect model' 					action: #inspectModel;
		add: 'Inspect selection' 					action: #inspectSelection;
		add: 'Clear selection' 					action: #clearSelection;
		add: 'Inspect display list' 					action: #inspectDList.
	^ aMenu! !
	"Open an inspector on the model of the DLV"

	dList inspect! !
	"Open an inspector on the model of the DLV"

	model inspect! !
	"Open an inspector on the model of the DLV"

	scroller selection inspect! !
	"Create a DLV on the given DL"

	| view |
	view _ self new initialize.
	view list: aDList.
	^view.! !
	"Test/example of using a DLV."
	"DisplayListView exampleLines"
	
	| list view |
	list _ DisplayList linesX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Test/example of using a DLV."
	"DisplayListView exampleMixed"
	
	| list view |
	list _ DisplayList mixedExample.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Test/example of using a DLV."
	"DisplayListView examplePolylines"
	
	| list view |
	list _ DisplayList polylinesX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Test/example of using a DLV."
	"DisplayListView exampleRectangles"
	
	| list view win |
	list := DisplayList rectanglesX: 2000 byY: 2000. 
	view := self newOn: list.
	win := DisplayListWindow labelled: 'DisplayListView'.
	"Test/example of using a DLV."
	"DisplayListView exampleStrings"
	
	| list view |
	list _ DisplayList stringsX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Test/example of using a DLV."
	"DisplayListView exampleVisuals"
	
	| list view |
	list _ DisplayList visualsX: 2000 byY: 2000.
	view _ self newOn: list.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Answer the class' dictionary of images"
	"DisplayListView constants"
	
	Constants ifNil: [Constants _ Dictionary new.].
	^Constants! !
	"Set up the receiver window"
self halt.
	self layoutMorph addMorph: model layoutSpec: LayoutFrame useAll! !
	"Open an instance of the window on the given view."
	"DisplayListView displayListViewExample"
	
	super open: aDLV label: lab! !