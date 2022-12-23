Object subclass: #LayoutManager
This is David's miraculous idea and the heart of the Navigator architecture.

Instance Variables:
	view		<DisplayListView> the view--May provide the displayItems
	orientation	<Symbol or nil> my 'base' symbol (e.g., #top)
	itemAccessor	<StructureAccessor> my referencing protocol translator

Look for class references to the LayoutManager classes (mostly from DisplayListView classes), for lots of examples of the use of LayoutManagers.

This implementation is Topaz 0, STEIM, Amsterdam, May/June 1990.

The entirety of this software is Copyright (c) 1990, Stephen Travis Pope, Nomad Object Design/Nomad Computer Music Research. 
All Rights Reserved.!
	"Answer the receiver's display list (generated)."

	^self generateDisplayList! !
	"Answer the receiver's display list (generated)."

	view := aView.
	^self generateDisplayList! !
	"Set the receiver's orientation."

	orientation := aSymbol! !
	"Set the receiver's view."

	view := aView! !
	"Add the given item to the display list."

	^self subclassResponsibility! !
	"Generate and answer the receiver's display list."

	| list |
	list := DisplayList new.
	view model items do:
		[ :e |
		 self addItem: e atNode: (OrderedCollection new) in: list].
	^list! !

Instance Variables:
	length		<Integer or Point> temp. depth used in layout
	xStep		<Integer> step size for x
	yStep		<Integer> step size for x
	treeAccessor	<TreeAccessor> the TreeAccessor used to access the model
!
	"Set the receiver's tree accessor to the argument."

	treeAccessor := aTA! !
	"Set the receiver's x step size to the argument."

	xStep := aNumber! !
	"Set the receiver's y step size to the argument."

	yStep := aNumber! !
	"Add the given item to the display list."

	| wrapper displayPoint item path2 displayList2 |
	displayPoint := self displayPositionFor: aPath.
	length := length + 1.
	item := treeAccessor itemFor: aModelNode.
	wrapper := TranslatingWrapper on: item at: displayPoint.
	(treeAccessor isTerminal: aModelNode)
		ifFalse: [displayList2 := DisplayList new.
			displayList2 model: aModelNode.
			(treeAccessor childrenOf: aModelNode) do: [ :i | 
				path2 := aPath copy.
				path2 addLast: aModelNode.
				self addItem: i
					atNode: path2
					in: displayList2].
			displayList2 add: wrapper.
			aDisplayList add: displayList2 atPath: aPath]
		ifTrue: [aDisplayList add: item atPath: aPath]! !
	"Add the given item to the display list."

	| displayPoint item path2 displayList2 |
	displayPoint := self displayPositionFor: aPath.
	length := length + 1.
	item := treeAccessor itemFor: aModelNode.
	item offset: displayPoint.
	(treeAccessor isTerminal: aModelNode)
		ifFalse: [displayList2 := DisplayList new.
			displayList2 model: aModelNode.
			(treeAccessor childrenOf: aModelNode) do: 
				[ :i | 
				path2 := aPath copy.
				path2 addLast: aModelNode.
				self addItem: i
					atNode: path2
					in: displayList2].
			displayList2 add: item"; offset: displayPoint".
			aDisplayList add: displayList2 atPath: aPath]
		ifTrue: [aDisplayList add: item atPath: aPath]! !
	"Answer the display position of the element at the given path."

	^self subclassResponsibility! !
	"Generate and answer the receiver's display list."

	| list |
	list := DisplayList new.
	self addItem: treeAccessor subject
			atNode: (OrderedCollection new)
			in: list.
	^list! !
	"Answer the display position of the element at the given path."

	| pt |
	length == nil ifTrue: [length := 0].
	pt := (orientation == #left or: [orientation == nil])
		ifFalse: [(length * xStep) @ (aPath size * yStep)]
		ifTrue: [(aPath size * xStep) @ (length * yStep)].
	Transcript show: aPath printString, ' ', pt printString; cr.
	^pt! !
	"Answer the display position of the element at the given path."

	length == nil ifTrue: [length := 0].
	^(orientation == #left or: [orientation == nil])
		ifFalse: [(length * xStep) @ (aPath size * yStep)]
		ifTrue: [(aPath size * xStep) @ (length * yStep)]! !
	"IndentedListLayoutManager colorListExample display"

	| acc me |
	acc := STreeAccessor subject: MusicMagnitude
			terminalQuery: [ :c | c subclasses size = 0]
			childAccessor: #sortedSubclasses
			itemGenerator: [ :c | (DisplayString new)
									model: c;
									string: c name;
									color: [c color]].
	me := (self new) accessor: acc; xStep: 16; yStep: 16.
	^me displayList display! !
	"IndentedListLayoutManager listExample display"
	"DisplayListView classListExample"

	| acc me |
	acc := STreeAccessor subject: Magnitude
			terminalQuery: [ :c | c subclasses size = 0]
			childAccessor: #sortedSubclasses
			itemGenerator: [ :c | (DisplayString new)
									model: c;
									string: c name;
									color: ColorValue darkGray].
	me := (self new) accessor: acc; xStep: 16; yStep: 16.
	^me displayList! !
	"Add the given item to the display list."

	|  displayPoint item path2 displayList2 |
	displayPoint := (self displayPositionFor: aPath) + (parentPoint x@0).
	length := length + 1.
	item := treeAccessor itemFor: aModelNode.
	item offset: displayPoint.
	(treeAccessor isTerminal: aModelNode)
		ifFalse: [displayList2 := DisplayList new.
			displayList2 model: aModelNode.
			(treeAccessor childrenOf: aModelNode) do: [ :i | 
				path2 := aPath copy.
				path2 addLast: aModelNode.
				self addItem: i
					atNode: path2
					under: item bounds rightCenter
					in: displayList2].
			displayList2 add: item.
			aDisplayList add: displayList2 atPath: aPath.
			parentPoint = (0@0)
				ifFalse: [aDisplayList add:
						((DisplayLine new)
							offset: item bounds leftCenter + (-2@4);
							corner: parentPoint + (2@4); color: ColorValue black)
					atPath: aPath]]
				ifTrue: [aDisplayList add: item atPath: aPath.
					aDisplayList add:
							((DisplayLine new)
								offset: item bounds leftCenter + (-2@4);
								corner: parentPoint + (2@4); color: ColorValue black)
						atPath: aPath]! !
	"Generate and answer the receiver's display list."

	list := DisplayList new.
	self addItem: treeAccessor subject
			atNode: (OrderedCollection new)
			under: 0@0
			in: list.
	^list! !
	"IndentedTreeLayoutManager listExample display"
	"DisplayListView classTreeExample"

	| acc me |
	acc := STreeAccessor subject: "View" Magnitude
			terminalQuery: [ :c | c subclasses size = 0]
			childAccessor: #sortedSubclasses
			itemGenerator: [ :c | (DisplayString new)
							model: c; 
							string: c name; 
							color: ColorValue black].
	me := (self new) accessor: acc; xStep: 4; yStep: 12.
	^me displayList! !
The basic display item generating protocol is: 
	(aStructureAccessor itemFor: aModelNode).

InstanceVariableNames:
	subject			<Object> the model or victim
	itemGenerator   	<Symbol or Block> the DisplayItem generator
						Model items are passed to it (if it's a Block),
						or have to perform: it (if it's a Symbol).

!
	"Answer the iG of the receiver."

	^itemGenerator! !
	"Set the iG of the receiver."

	itemGenerator := anObject! !
	"Answer the subject of the receiver."

	^subject! !
	"Set the subject of the receiver."

	subject := anObject! !
	"Answer a display item for the argument"

	(itemGenerator isMemberOf: Symbol)
		ifTrue: [^aModel perform: itemGenerator].
	(itemGenerator isMemberOf: BlockClosure)
		ifTrue: [^itemGenerator value: aModel].
	self error: 'Unknown item generator type.'! !
The basic display tree walking protocol is: 
	(aSTreeAccessor childrenOf: aModelNode); and
	(aSTreeAccessor isTerminal: aModelNode).

InstanceVariableNames:
	terminalQuery	<Symbol or Block> the isLeaf or hasChildrenNot query
	childAccessor		<Symbol or Block> accessor to get a node's children
						Model items are passed to these (if they're Blocks),
						or have to perform: them (if they're Symbols).

!
	"Set the cA of the receiver."

	childAccessor := anObject! !
	"Set the tQ of the receiver."

	terminalQuery := anObject! !
	"Answer the receiver."

	^self! !
	"Answer the children of the argument by using the child accessor."

	(childAccessor isSymbol)
		ifTrue: [^aNode perform: childAccessor].
	(childAccessor isMemberOf: BlockClosure)
		ifTrue: [^childAccessor value: aNode].
	self error: 'Unknown child accessor type.'! !
	"Answer whether the argument is terminal by using the terminal query."

	(terminalQuery isSymbol)
		ifTrue: [^aNode perform: terminalQuery].
	(terminalQuery isMemberOf: BlockClosure)
		ifTrue: [^terminalQuery value: aNode].
	self error: 'Unknown terminal query type.'! !
	"Answer the leaves of the argument by using the child accessor."

	| kids leaves |
	leaves := OrderedCollection new.
	(childAccessor isSymbol)
		ifTrue: [kids := aRoot perform: childAccessor]
		ifFalse: [(childAccessor isMemberOf: BlockClosure)
			ifTrue: [kids := childAccessor value: aRoot]
			ifFalse: [self error: 'Unknown child accessor type.']].
	kids isEmpty
		ifFalse: [kids do:
			[ :aChild |
			(self isTerminal: aChild)
				ifTrue: [leaves add: aChild]
				ifFalse: [leaves addAll: (self leavesOf: aChild)]]].
	^leaves! !
	"Answer an initialized instance."

	^(self new) subject: aS;
			terminalQuery: aTQ;
			childAccessor: aCA;
			itemGenerator: anIG! !

Instance Variables:
	timeScale		<Integer> the msec./pixel scale
	timeOffset		<Integer> the start of the layout in msec.
!
	"Add the given item to the display list."

	| path2 anE displayPoint item |
	anE := anAss value.
	anE hasItems
		ifTrue: [^anE do:  
			[ :i | 
			path2 := aPath copy.
			path2 addLast: anE.
			self addItem: anAss key + i key => i value
					atNode: path2
					in: aDisplayList]].
	displayPoint := self displayPointFor: anAss.
	item := view itemFor: anAss.
	item offset: displayPoint.
	aDisplayList add: item atPath: aPath! !
	"Answer the display point for the argument."

	| dPoint y |
	y _ 16.
	dPoint := (anAss key asMS value - timeOffset / timeScale) asInteger.
	(orientation == nil or: [orientation == #left])
		ifTrue: [dPoint := dPoint @ y]
		ifFalse: [orientation == #top
			ifTrue: [dPoint := y @ dPoint]].
	^dPoint! !
	"Set the receiver's time offset."

	timeOffset := aNumber! !
	"Set the receiver's time scale."

	timeScale := aNumber! !
The default layout is linear pitch-to-y, like piano-roll or Hauer-Steffens notation.

Instance Variables:
	pitchScale		<Integer> the key#/pixel scale
	pitchOffset		<Integer> the bottom of the layout as a key#
!
	"Set the receiver's pitch offset."

	pitchOffset := aNumber! !
	"Set the receiver's pitch scale."

	pitchScale := aNumber! !
	"Answer the display point for the argument."

	| pch |
	pch := anAss event pitch.
	pch ifNil: [^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ (60*pitchScale)].
	^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ ((60*pitchScale) 
			- ((pch asMIDI value - pitchOffset) * pitchScale))! !
!
	"Answer the display point for the argument."

	^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ ((anAss event position - pitchOffset) * pitchScale)! !