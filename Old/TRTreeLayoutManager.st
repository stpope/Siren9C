'From Squeak3.0 of 4 February 2001 [latest update: #3545] on 21 May 2007 at 9:40:12 am'!
LeafLinearTreeLayoutManager subclass: #TRTreeLayoutManager
	instanceVariableNames: 'sequenceView '
	classVariableNames: 'Dot EmptyDot '
	poolDictionaries: ''
	category: 'MusicOLD-Layout'!
!TRTreeLayoutManager !
Instances of TRTBinaryTreeLayoutManager are used to generate display lists which mirror the hierarchy of their models (tension-relaxation or prolongational reduction trees accessed using a TreeAccessor) in the tree structure of a binary tree whose leaves are adjusted to lie in a line at the same y value and whose branches are straightened according to the TRTree semantics described in "A Generative Theory of Tonal Music" by Fred Lerdahl and Ray Jackendoof.!


!TRTreeLayoutManager methodsFor: 'accessing'!
sequenceView: aView
	"Set the receiver's sequence view."

	sequenceView := aView! !


!TRTreeLayoutManager methodsFor: 'generating'!
addItem: aModelNode atNode: aPath in: aDisplayList
	"Add the given item to the display list."

	| displayPoint item path2 left right kids |
	aPath size = 0
		ifTrue: [length := aModelNode depth].
	item := treeAccessor itemFor: aModelNode.
	(treeAccessor isTerminal: aModelNode)
		ifFalse: [kids := (treeAccessor childrenOf: aModelNode).
			left := DisplayList new model: kids first.
			right := DisplayList new model: (kids at: 2).
			path2 := aPath copy.
			path2 addLast: aModelNode.
			self addItem: kids first
					atNode: path2
					in: left.
			self addItem: (kids at: 2)
					atNode: path2
					in: right.
			displayPoint :=  (((left items first offset x) 
						+ (right items first offset x) / 2)
				@ (aPath size * yStep)) truncateTo: 1.
			item offset: displayPoint.
			aDisplayList add: item.
			(left items at: 2) corner: (item center truncateTo: 1).
			(right items at: 2) corner: (item center truncateTo: 1).
			aDisplayList add:
				((DisplayLine new)
					offset: (item center truncateTo: 1);
					corner: 0@0)
				atPath: aPath.
			aDisplayList add: left.
			aDisplayList add: right]
		ifTrue: [displayPoint := 
					self displayPositionFor: aModelNode atDepth: length.
			item offset: displayPoint.
			aDisplayList add: item atPath: aPath.
			aDisplayList add:
					((DisplayLine new)
						offset: displayPoint;
						corner: 0@0)
				atPath: aPath]! !

!TRTreeLayoutManager methodsFor: 'generating'!
adjustPositionsIn: aList
	"Assign final positions in the list, straightening the intermediate nodes out according to TRTree semantics."

	| offset list | 
	list := self straightenBranchesIn: aList level: 0.
	offset := list computeBoundingBox origin.
	list offset: ((offset x negated)@(offset y negated)).
	^list! !

!TRTreeLayoutManager methodsFor: 'generating'!
displayPositionFor: aModelLeaf atDepth: aDepth
	"Answer the display position of the element at the given path."

	| key |
	sequenceView == nil
		ifTrue: [^super displayPositionFor: aModelLeaf atDepth: aDepth].
	key := (sequenceView model list events at:
			(view model leaves indexOf: aModelLeaf)) key.
	^((key asMS value / sequenceView scaleX)
		@ (aDepth * yStep)) truncateTo: 1! !

!TRTreeLayoutManager methodsFor: 'generating'!
primePathOf: aList
	"Answer the collection of nodes leading to the primt leav of the argument's tree."

	| item collection|
	item := aList model.
	item isTreeNode
		ifTrue: [item key == #left
			ifTrue: [collection := self primePathOf: (aList items at: 4).
				collection addLast: aList items]
			ifFalse: [collection := self primePathOf: (aList items at: 3).
				collection addLast: aList items]]
		ifFalse: [collection := OrderedCollection new.
			collection add: aList items].
	^collection! !

!TRTreeLayoutManager methodsFor: 'generating'!
straightenBranchesIn: aList level: aCounter
	"Assign final positions in the list, straightening the intermediate nodes out according to TRTree semantics."

	| primePath item pos x1 x2  line |
	item := aList items first.
	pos := item center.
	item offset: pos.
	3 to: 4 do:
		[ :i |
		primePath := self primePathOf: (aList items at: i).
		x1 := primePath first first offset x.
		x2 := aList items first offset x.
		2 to: primePath size do:
			[ :j |
			item := (primePath at: j) first.
			item offset: (((x1 + (j - 1 * (x2 - x1) / primePath size))
						@ (item offset y)) truncateTo: 1).
			line := (primePath at: j) at: 2.
			line offset: (item center truncateTo: 1).
			line extent: (((x2 - x1 / primePath size) 
						@ yStep negated) truncateTo: 1)]].
	item := aList model.
	item isTreeNode
		ifTrue: [(aList items at: 3) model isTreeNode
				ifTrue: [self straightenBranchesIn: (aList items at: 3)
							level: (aCounter + 1)]
				ifFalse: [line := (aList items at: 3) items at: 2.
					line corner: pos].
			(aList items at: 4) model isTreeNode
				ifTrue: [self straightenBranchesIn: (aList items at: 4)
							level: (aCounter + 1)]
				ifFalse: [	line := (aList items at: 4) items at: 2.
					line corner: pos]].
	line := aList items at: 2.
	pos := aList items first center.
	(aCounter = 0)
		ifTrue: [x1 := pos x - ((item key == #left)
				ifTrue: [((aList items at: 4) items at: 1) center x]
				ifFalse: [((aList items at: 3) items at: 1) center x]).
		line offset: pos.
		line corner: (((pos x + (x1 / 2)) @ (yStep / 2 negated)) 
						truncateTo: 1)].
	^aList! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

TRTreeLayoutManager class
	instanceVariableNames: ''!

!TRTreeLayoutManager class methodsFor: 'class initialization'!
initialize
	"Initialize the class variables."
	"TRTreeLayoutManager initialize"

"	Dot := Form extent: 5@5
			fromArray: #( 28672 63488 63488 63488 28672)
			offset: -2@-2.
	EmptyDot := Form extent: 5@5
			fromArray: #( 28672 55296 34816 55296 28672)
			offset: -2@-2
"! !


!TRTreeLayoutManager class methodsFor: 'class var access'!
dot
	"Answer the class var dot form."

	^Dot! !

!TRTreeLayoutManager class methodsFor: 'class var access'!
emptyDot
	"Answer the class var empty dot form."

	^EmptyDot! !


TRTreeLayoutManager initialize!
