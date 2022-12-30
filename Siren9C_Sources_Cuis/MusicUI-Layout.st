!classDefinition: #LayoutManager category: 'MusicUI-Layout'!
Object subclass: #LayoutManager
	instanceVariableNames: 'view orientation itemAccessor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!LayoutManager commentStamp: '<historical>' prior: 0!
Instances of the LayoutManager classes are used to generate display lists given random data structures and possibly hints in terms of structure accessors.
This is David's miraculous idea and the heart of the Navigator architecture.

Instance Variables:
	view		<DisplayListView> the view--May provide the displayItems
	orientation	<Symbol or nil> my 'base' symbol (e.g., #top)
	itemAccessor	<StructureAccessor> my referencing protocol translator

Look for class references to the LayoutManager classes (mostly from DisplayListView classes), for lots of examples of the use of LayoutManagers.

This implementation is Topaz 0, STEIM, Amsterdam, May/June 1990.

The entirety of this software is Copyright (c) 1990, Stephen Travis Pope, Nomad Object Design/Nomad Computer Music Research. 
All Rights Reserved.!


!LayoutManager methodsFor: 'accessing'!
displayList
	"Answer the receiver's display list (generated)."

	^self generateDisplayList! !

!LayoutManager methodsFor: 'accessing'!
displayListFor: aView
	"Answer the receiver's display list (generated)."

	view := aView.
	^self generateDisplayList! !

!LayoutManager methodsFor: 'accessing'!
orientation: aSymbol
	"Set the receiver's orientation."

	orientation := aSymbol! !

!LayoutManager methodsFor: 'accessing'!
view: aView
	"Set the receiver's view."

	view := aView! !


!LayoutManager methodsFor: 'generating'!
addItem: anAss atNode: aPath in: aDisplayList
	"Add the given item to the display list."

	^self subclassResponsibility! !

!LayoutManager methodsFor: 'generating'!
generateDisplayList
	"Generate and answer the receiver's display list."

	| list |
	list := DisplayList new.
	view model items do:
		[ :e |
		 self addItem: e atNode: (OrderedCollection new) in: list].
	^list! !


!classDefinition: #HierarchyLayoutManager category: 'MusicUI-Layout'!
LayoutManager subclass: #HierarchyLayoutManager
	instanceVariableNames: 'length xStep yStep treeAccessor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!HierarchyLayoutManager commentStamp: '<historical>' prior: 0!
Instances of the HierarchyLayoutManager classes are used to generate display lists from tree-like data and tree accessors.

Instance Variables:
	length		<Integer or Point> temp. depth used in layout
	xStep		<Integer> step size for x
	yStep		<Integer> step size for x
	treeAccessor	<TreeAccessor> the TreeAccessor used to access the model
!


!HierarchyLayoutManager methodsFor: 'accessing'!
accessor: aTA
	"Set the receiver's tree accessor to the argument."

	treeAccessor := aTA! !

!HierarchyLayoutManager methodsFor: 'accessing'!
xStep: aNumber
	"Set the receiver's x step size to the argument."

	xStep := aNumber! !

!HierarchyLayoutManager methodsFor: 'accessing'!
yStep: aNumber
	"Set the receiver's y step size to the argument."

	yStep := aNumber! !


!HierarchyLayoutManager methodsFor: 'generating'!
addItem1: aModelNode atNode: aPath in: aDisplayList
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

!HierarchyLayoutManager methodsFor: 'generating'!
addItem: aModelNode atNode: aPath in: aDisplayList
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

!HierarchyLayoutManager methodsFor: 'generating'!
displayPositionFor: aPath
	"Answer the display position of the element at the given path."

	^self subclassResponsibility! !

!HierarchyLayoutManager methodsFor: 'generating'!
generateDisplayList
	"Generate and answer the receiver's display list."

	| list |
	list := DisplayList new.
	self addItem: treeAccessor subject
			atNode: (OrderedCollection new)
			in: list.
	^list! !


!classDefinition: #IndentedListLayoutManager category: 'MusicUI-Layout'!
HierarchyLayoutManager subclass: #IndentedListLayoutManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!IndentedListLayoutManager commentStamp: '<historical>' prior: 0!
Instances of IndentedListLayoutManager are used to generate display lists which mirror the hierarchy of their models (accessed using a TreeAccessor) in the indentation of a list-like outline.!


!IndentedListLayoutManager methodsFor: 'generating'!
displayPositionFor1: aPath
	"Answer the display position of the element at the given path."

	| pt |
	length == nil ifTrue: [length := 0].
	pt := (orientation == #left or: [orientation == nil])
		ifFalse: [(length * xStep) @ (aPath size * yStep)]
		ifTrue: [(aPath size * xStep) @ (length * yStep)].
	Transcript show: aPath printString, ' ', pt printString; cr.
	^pt! !

!IndentedListLayoutManager methodsFor: 'generating'!
displayPositionFor: aPath
	"Answer the display position of the element at the given path."

	length == nil ifTrue: [length := 0].
	^(orientation == #left or: [orientation == nil])
		ifFalse: [(length * xStep) @ (aPath size * yStep)]
		ifTrue: [(aPath size * xStep) @ (length * yStep)]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'IndentedListLayoutManager class' category: 'MusicUI-Layout'!
IndentedListLayoutManager class
	instanceVariableNames: ''!

!IndentedListLayoutManager class methodsFor: 'examples'!
colorListExample
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

!IndentedListLayoutManager class methodsFor: 'examples'!
listExample
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


!classDefinition: #IndentedTreeLayoutManager category: 'MusicUI-Layout'!
IndentedListLayoutManager subclass: #IndentedTreeLayoutManager
	instanceVariableNames: 'list'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!IndentedTreeLayoutManager commentStamp: '<historical>' prior: 0!
Instances of IndentedTreeLayoutManager are used to generate display lists which mirror the hierarchy of their models (accessed using a TreeAccessor) in the indentation of a list-like tree.!


!IndentedTreeLayoutManager methodsFor: 'generating'!
addItem: aModelNode atNode: aPath under: parentPoint in: aDisplayList
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

!IndentedTreeLayoutManager methodsFor: 'generating'!
generateDisplayList
	"Generate and answer the receiver's display list."

	list := DisplayList new.
	self addItem: treeAccessor subject
			atNode: (OrderedCollection new)
			under: 0@0
			in: list.
	^list! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'IndentedTreeLayoutManager class' category: 'MusicUI-Layout'!
IndentedTreeLayoutManager class
	instanceVariableNames: ''!

!IndentedTreeLayoutManager class methodsFor: 'examples'!
listExample
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


!classDefinition: #TimeSequenceLayoutManager category: 'MusicUI-Layout'!
LayoutManager subclass: #TimeSequenceLayoutManager
	instanceVariableNames: 'timeScale timeOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!TimeSequenceLayoutManager commentStamp: '<historical>' prior: 0!
Instances of the SequenceLayoutManager classes are used to generate display lists with "x = time" from event lists.

Instance Variables:
	timeScale		<Integer> the msec./pixel scale
	timeOffset		<Integer> the start of the layout in msec.
!


!TimeSequenceLayoutManager methodsFor: 'generating'!
addItem: anAss atNode: aPath in: aDisplayList 
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

!TimeSequenceLayoutManager methodsFor: 'generating' stamp: 'stp 12/2/2022 05:01:35'!
displayPointFor: anAss 
	"Answer the display point for the argument."

	| dPoint y |
	y _ 16.
	dPoint := (anAss key asMS value - timeOffset / timeScale) asInteger.
	(orientation == nil or: [orientation == #left])
		ifTrue: [dPoint := dPoint @ y]
		ifFalse: [orientation == #top
			ifTrue: [dPoint := y @ dPoint]].
	^dPoint! !


!TimeSequenceLayoutManager methodsFor: 'accessing'!
timeOffset: aNumber
	"Set the receiver's time offset."

	timeOffset := aNumber! !

!TimeSequenceLayoutManager methodsFor: 'accessing'!
timeScale: aNumber
	"Set the receiver's time scale."

	timeScale := aNumber! !


!classDefinition: #PitchTimeLayoutManager category: 'MusicUI-Layout'!
TimeSequenceLayoutManager subclass: #PitchTimeLayoutManager
	instanceVariableNames: 'pitchScale pitchOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!PitchTimeLayoutManager commentStamp: '<historical>' prior: 0!
Instances of the PitchTimeLayoutManager classes are used to generate display lists with "x = time" and "y = pitch" from event lists.
The default layout is linear pitch-to-y, like piano-roll or Hauer-Steffens notation.

Instance Variables:
	pitchScale		<Integer> the key#/pixel scale
	pitchOffset		<Integer> the bottom of the layout as a key#
!


!PitchTimeLayoutManager methodsFor: 'accessing'!
pitchOffset: aNumber
	"Set the receiver's pitch offset."

	pitchOffset := aNumber! !

!PitchTimeLayoutManager methodsFor: 'accessing'!
pitchScale: aNumber
	"Set the receiver's pitch scale."

	pitchScale := aNumber! !


!PitchTimeLayoutManager methodsFor: 'generating'!
displayPointFor: anAss
	"Answer the display point for the argument."

	| pch |
	pch := anAss event pitch.
	pch ifNil: [^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ (60*pitchScale)].
	^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ ((60*pitchScale) 
			- ((pch asMIDI value - pitchOffset) * pitchScale))! !


!classDefinition: #PositionTimeLayoutManager category: 'MusicUI-Layout'!
PitchTimeLayoutManager subclass: #PositionTimeLayoutManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!PositionTimeLayoutManager commentStamp: '<historical>' prior: 0!
Instances of PositionTimeLayoutManager are used to generate display lists with "x = time" and "y = position" from event lists.
!


!PositionTimeLayoutManager methodsFor: 'generating'!
displayPointFor: anAss
	"Answer the display point for the argument."

	^((anAss key asMS value - timeOffset / timeScale) asInteger)
		@ ((anAss event position - pitchOffset) * pitchScale)! !


!classDefinition: #StructureAccessor category: 'MusicUI-Layout'!
Object subclass: #StructureAccessor
	instanceVariableNames: 'subject itemGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!StructureAccessor commentStamp: '<historical>' prior: 0!
Instances of StructureAccessor and its subclasses are 'protocol convertors' which provide predictable interfaces (e.g., tree-speak), for diverse data structures.
The basic display item generating protocol is: 
	(aStructureAccessor itemFor: aModelNode).

InstanceVariableNames:
	subject			<Object> the model or victim
	itemGenerator   	<Symbol or Block> the DisplayItem generator
						Model items are passed to it (if it's a Block),
						or have to perform: it (if it's a Symbol).

!


!StructureAccessor methodsFor: 'accessing'!
itemGenerator
	"Answer the iG of the receiver."

	^itemGenerator! !

!StructureAccessor methodsFor: 'accessing'!
itemGenerator: anObject
	"Set the iG of the receiver."

	itemGenerator := anObject! !

!StructureAccessor methodsFor: 'accessing'!
subject
	"Answer the subject of the receiver."

	^subject! !

!StructureAccessor methodsFor: 'accessing'!
subject: anObject
	"Set the subject of the receiver."

	subject := anObject! !


!StructureAccessor methodsFor: 'item accessing'!
itemFor: aModel
	"Answer a display item for the argument"

	(itemGenerator isMemberOf: Symbol)
		ifTrue: [^aModel perform: itemGenerator].
	(itemGenerator isMemberOf: BlockClosure)
		ifTrue: [^itemGenerator value: aModel].
	self error: 'Unknown item generator type.'! !


!classDefinition: #STreeAccessor category: 'MusicUI-Layout'!
StructureAccessor subclass: #STreeAccessor
	instanceVariableNames: 'terminalQuery childAccessor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-Layout'!
!STreeAccessor commentStamp: '<historical>' prior: 0!
Instances of STreeAccessor tree-navigation interfaces to diverse data structures.
The basic display tree walking protocol is: 
	(aSTreeAccessor childrenOf: aModelNode); and
	(aSTreeAccessor isTerminal: aModelNode).

InstanceVariableNames:
	terminalQuery	<Symbol or Block> the isLeaf or hasChildrenNot query
	childAccessor		<Symbol or Block> accessor to get a node's children
						Model items are passed to these (if they're Blocks),
						or have to perform: them (if they're Symbols).

!


!STreeAccessor methodsFor: 'accessing'!
childAccessor: anObject
	"Set the cA of the receiver."

	childAccessor := anObject! !

!STreeAccessor methodsFor: 'accessing'!
terminalQuery: anObject
	"Set the tQ of the receiver."

	terminalQuery := anObject! !

!STreeAccessor methodsFor: 'accessing'!
treeAccessor
	"Answer the receiver."

	^self! !


!STreeAccessor methodsFor: 'tree accessing'!
childrenOf: aNode
	"Answer the children of the argument by using the child accessor."

	(childAccessor isSymbol)
		ifTrue: [^aNode perform: childAccessor].
	(childAccessor isMemberOf: BlockClosure)
		ifTrue: [^childAccessor value: aNode].
	self error: 'Unknown child accessor type.'! !

!STreeAccessor methodsFor: 'tree accessing'!
isTerminal: aNode
	"Answer whether the argument is terminal by using the terminal query."

	(terminalQuery isSymbol)
		ifTrue: [^aNode perform: terminalQuery].
	(terminalQuery isMemberOf: BlockClosure)
		ifTrue: [^terminalQuery value: aNode].
	self error: 'Unknown terminal query type.'! !

!STreeAccessor methodsFor: 'tree accessing'!
leavesOf: aRoot
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

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'STreeAccessor class' category: 'MusicUI-Layout'!
STreeAccessor class
	instanceVariableNames: ''!

!STreeAccessor class methodsFor: 'instance creation'!
subject: aS terminalQuery: aTQ childAccessor: aCA itemGenerator: anIG
	"Answer an initialized instance."

	^(self new) subject: aS;
			terminalQuery: aTQ;
			childAccessor: aCA;
			itemGenerator: anIG! !
