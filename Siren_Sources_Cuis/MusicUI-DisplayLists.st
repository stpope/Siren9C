!classDefinition: #DisplayItem category: 'MusicUI-DisplayLists'!
Object subclass: #DisplayItem
	instanceVariableNames: 'model offset color container'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayItem commentStamp: '<historical>' prior: 0!
Instances of the subclasses of the abstract class DisplayItem are used as the items in display lists. They can generally display themselves on graphics contexts.
These are done this way because I don't think wrappers are right for use in display lists.

Instance Variables:
	offset		<Point> the object's offset relative to its container (display list)
	color		<ColorValue> the object's display color (or nil)

The subclasses add special display-related state and behavior such as an extent point and display method, or a visual item such as a string or image to display.
They generally implement displayOn: aGraphicsContext and bounds accessing methods.

This implementation is MODE 1.1, STEIM, Amsterdam, May/June 1990; updated at the Lagoon in Palo Alto, July, 1991-May, 1992.

The entirety of this software is Copyright (c) 1990, Stephen Travis Pope, Nomad Object Design/Nomad Computer Music Research. 
All Rights Reserved.
!


!DisplayItem methodsFor: 'transforming'!
asVisualComponent
	"Answer an encapsulated version of the receiver which understands
	VisualComponent protocol."

	^TranslatingWrapper on: self! !

!DisplayItem methodsFor: 'transforming'!
scaleBy: aPoint
	"Translate the receiver's offset by the argument."

	self extent: self extent * aPoint! !

!DisplayItem methodsFor: 'transforming'!
scaledBy: aPoint
	"Translate the receiver's offset by the argument."

	^self copy extent: self extent * aPoint! !

!DisplayItem methodsFor: 'transforming'!
translateBy: aPoint
	"Translate the receiver's offset by the argument."

	self offset: self offset + aPoint! !


!DisplayItem methodsFor: 'printing' stamp: 'stp 11/30/2022 17:24:28'!
printCompleteOn: aStream depth: depth
	"Print the receiver in the given Stream."

	self printOn: aStream.
	"aStream cr.
	depth timesRepeat: [aStream tab].
	aStream nextPutAll: '(a ', self class name. 
	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $).
	"! !

!DisplayItem methodsFor: 'printing' stamp: 'stp 11/30/2022 17:21:08'!
printOn: aStream
	"Print the receiver in the given Stream."

	aStream nextPutAll: '(a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $); cr.! !


!DisplayItem methodsFor: 'accessing' stamp: 'stp 11/29/2022 13:43:18'!
bounds
	"Answer the receiver's bounds." 

	| ext|
	ext _ self extent.
	^offset extent: ((ext x max: 1) @ (ext y max: 1))! !

!DisplayItem methodsFor: 'accessing'!
color
	"Answer the receiver's display color."

	^color! !

!DisplayItem methodsFor: 'accessing'!
color: aVal
	"Set the receiver's display color to the argument."

	color := ((aVal isMemberOf: BlockClosure)
		ifTrue: [aVal value]
		ifFalse: [aVal])! !

!DisplayItem methodsFor: 'accessing'!
container: aDList
	"Assign the container"

	container _ aDList! !

!DisplayItem methodsFor: 'accessing'!
extent
	"Answer the extent of the receiver (dummy in this class)."

	^1@1! !

!DisplayItem methodsFor: 'accessing'!
extent: aPoint
	"Ignored"! !

!DisplayItem methodsFor: 'accessing' stamp: 'stp 12/2/2022 03:57:10'!
model
	"Answer the model"

	^model! !

!DisplayItem methodsFor: 'accessing' stamp: 'stp 12/2/2022 03:56:58'!
model: obj
	"Assign the model"

	model _ obj! !

!DisplayItem methodsFor: 'accessing'!
offset
	"Answer the receiver's offset."

	^offset! !

!DisplayItem methodsFor: 'accessing'!
offset: aPoint
	"Set the receiver's offset to the argument."

	offset := aPoint! !


!DisplayItem methodsFor: 'testing'!
isDisplayItem
	"Answer whether the receiver is a kind of DisplayItem"

	^true! !


!DisplayItem methodsFor: 'initialize-release'!
initialize
	"Initialize the instance variables of the receiver."

	offset := 0@0! !


!DisplayItem methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"Display the receiver on the argument."

	self subclassResponsibility! !

!DisplayItem methodsFor: 'displaying'!
displayPostScriptOn: aPostScriptContext
	"Display the receiver on the argument in PostScript."

	self subclassResponsibility! !

!DisplayItem methodsFor: 'displaying' stamp: 'stp 11/29/2022 13:25:35'!
drawOn: aCanvas
	"Display the receiver on the given canvas."

	self subclassResponsibility! !

!DisplayItem methodsFor: 'displaying' stamp: 'stp 12/2/2022 00:17:25'!
drawSelectionOn: aCanvas
	"Display the receiver's selection boxes on the given canvas."

	| bound bRect wRect |
	bRect := (-3@-3) extent: (6@6).
	wRect := (-2@-2) extent: (4@4).
	bound := self bounds.
	#(topLeft topRight bottomRight bottomLeft) do:
		[ :cor | | pt |
		pt := bound perform: cor.
		aCanvas frameRectangle: (bRect translatedBy: pt) borderWidth: 2 color: Color darkGray].
"		fillRectangle: self bounds
			color: color
			borderWidth: 1
			borderStyleSymbol: #simple
			baseColorForBorder: Color darkGray
			
		aGC paint: ColorValue black.
		aGC displayRectangle: bRect at: pt.
		aGC paint: ColorValue white.
		aGC displayRectangle: wRect at: pt]
"! !


!DisplayItem methodsFor: 'copying'!
copy
	"Answer a shallow copy of the receiver."

	^self shallowCopy! !


!DisplayItem methodsFor: 'bounds accessing'!
computePreferredBounds
	"Answer the receiver's bounds--hack."

	^self bounds! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayItem class' category: 'MusicUI-DisplayLists'!
DisplayItem class
	instanceVariableNames: ''!

!DisplayItem class methodsFor: 'instance creation'!
model: aM offset: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^self new model: aM; offset: aPt! !

!DisplayItem class methodsFor: 'instance creation'!
offset: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^self new offset: aPt! !


!classDefinition: #DisplayLine category: 'MusicUI-DisplayLists'!
DisplayItem subclass: #DisplayLine
	instanceVariableNames: 'width extent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayLine commentStamp: '<historical>' prior: 0!
Instances of DisplayLine are used for visual lines in display lists.

Instance Variables:
	width		<Number> the object's line width
	extent		<Point> the object's visual extent!


!DisplayLine methodsFor: 'accessing'!
corner: aPoint
	"Set the corner of the receiver (offset + extent)."

	offset == nil
		ifFalse: [extent := aPoint - offset].
"	end := (offset == nil
			ifTrue: [aPoint]
			ifFalse: [start + aPoint])."
	^self! !

!DisplayLine methodsFor: 'accessing'!
extent
	"Answer the extent of the receiver."

	^extent! !

!DisplayLine methodsFor: 'accessing'!
extent: aPoint
	"Set the extent of the receiver."

	extent := aPoint
"	end := (offset == nil
			ifTrue: [aPoint]
			ifFalse: [start + aPoint])"! !

!DisplayLine methodsFor: 'accessing'!
width: aVal
	width := aVal! !


!DisplayLine methodsFor: 'printing' stamp: 'stp 11/30/2022 17:24:47'!
printOn: aStream
	"Print the receiver in the given Stream."

	aStream nextPutAll: ' (a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	extent == nil ifFalse: [aStream nextPutAll: ' extent: '. extent printOn: aStream].
"	color == nil ifFalse: [aStream nextPutAll: ' color: '. color printOn: aStream]."
	aStream nextPut: $); cr.! !


!DisplayLine methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"Display a line between startPoint and endPoint."

"	Transcript show: start printString, '	', end printString; cr."
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	width == nil
		ifFalse: [aGraphicsContext lineWidth: width].
	aGraphicsContext displayLineFrom: offset to: (offset + extent)! !

!DisplayLine methodsFor: 'displaying'!
displayPostScriptOn: aPostscriptContext
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext rlineto: extent	.
	aPostscriptContext translate: offset negated.! !

!DisplayLine methodsFor: 'displaying' stamp: 'stp 12/2/2022 02:39:47'!
drawOn: aCanvas
	"Display a line on the given canvas"

	| rect c w |
	rect _ self bounds.
	w _ width.
	w ifNil: [w _ 1].
	c _ color.
	c ifNil: [c _ Color black].
	aCanvas line: rect origin to: rect corner width: w color: c! !


!DisplayLine methodsFor: 'bounds accessing' stamp: 'stp 11/30/2022 17:27:14'!
bounds
	"Answer the receiver's bounds." 

	^offset extent: extent "((extent x max: 1) @ (extent y max: 1))"! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayLine class' category: 'MusicUI-DisplayLists'!
DisplayLine class
	instanceVariableNames: ''!

!DisplayLine class methodsFor: 'instance creation'!
from: oPoint to: endPoint
	"Answer an initialized instance."

	^((self new) offset: oPoint) extent: (endPoint - oPoint)! !

!DisplayLine class methodsFor: 'instance creation'!
offset: oPoint extent: ePoint	
	"Answer an initialized instance."

	^((self new) offset: oPoint) extent: ePoint! !


!classDefinition: #DisplayRectangle category: 'MusicUI-DisplayLists'!
DisplayLine subclass: #DisplayRectangle
	instanceVariableNames: 'fill stroke'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayRectangle commentStamp: '<historical>' prior: 0!
Instances of DisplayRectangle can be used to display bordered or filled rectangles in display lists. This is a subclass of DisplayLine for reasons of practicality. a "purist" (e.g., David Liebs), would create an intermediate abstract class (e.g., BoundedDisplayItem) for both DisplayLine and DisplayRectangle.

Instanve Variables:
	fill		<Boolean or nil> whether or not to fill the receiver on display (no by default)!


!DisplayRectangle methodsFor: 'accessing'!
fill: aBoolean
	"Set the receiver's filling Boolean."

	fill := aBoolean! !

!DisplayRectangle methodsFor: 'accessing'!
stroke: aNum
	"Set the receiver's stroke line thickness."

	stroke := aNum! !


!DisplayRectangle methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"Display a line between startPoint and endPoint."

"	Transcript show: start printString, '	', end printString; cr."
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	(fill == nil or: [fill not])
		ifFalse: [aGraphicsContext displayRectangle: (offset extent: extent)]
		ifTrue: [aGraphicsContext displayRectangularBorder: (offset extent: extent)]! !

!DisplayRectangle methodsFor: 'displaying'!
displayPostScriptOn: aPostscriptContext
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext box: extent y width: extent x.
	aPostscriptContext translate: offset negated.! !

!DisplayRectangle methodsFor: 'displaying' stamp: 'stp 11/29/2022 13:29:24'!
drawOn: aCanvas
	"Display a rect on the given canvas"

	aCanvas fillRectangle: self bounds
		color: color
		borderWidth: 1
		borderStyleSymbol: #simple
		baseColorForBorder: Color darkGray! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayRectangle class' category: 'MusicUI-DisplayLists'!
DisplayRectangle class
	instanceVariableNames: ''!

!DisplayRectangle class methodsFor: 'examples'!
rectangleExample
	"Open a display list view with randomly-placed random-color rectangles over the given extent."
	"DisplayRectangle rectangleExample"

	(DisplayList rectanglesX: 600 byY: 600) display! !


!classDefinition: #DisplayPolyline category: 'MusicUI-DisplayLists'!
DisplayRectangle subclass: #DisplayPolyline
	instanceVariableNames: 'vertices'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayPolyline commentStamp: '<historical>' prior: 0!
A DisplayPolyline is a poly-line-segment display object

Instance Variables:
	vertices	<OrderedCollection of Point>  my point array
!


!DisplayPolyline methodsFor: 'accessing'!
bounds
	"Answer the receiver's bounds." 

	| ext |
	ext := 0@0.
	vertices do: [ :pt | ext := ext max: pt].
	^offset extent: ext! !

!DisplayPolyline methodsFor: 'accessing'!
extent
	"Answer the receiver's extent." 

	^self bounds extent! !

!DisplayPolyline methodsFor: 'accessing'!
vertices: pointArray
	"Set the receiver's vertices."

	vertices := pointArray asArray! !


!DisplayPolyline methodsFor: 'transforming'!
scaledBy: aPoint
	"Scale all the receiver's points by the argument; answer a copy."

	|  newPts |
	newPts := vertices collect: [ :pt | (pt * aPoint) truncated].
	^self class offset: offset vertices: newPts! !


!DisplayPolyline methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"Stroke the receiver's edges on the supplied GraphicsContext."

	| cachedLW |
	color == nil
		ifTrue: [aGraphicsContext paint: ColorValue black]
		ifFalse: [aGraphicsContext paint: ColorValue black].
	stroke == nil
		ifFalse: [cachedLW := aGraphicsContext lineWidth.
			aGraphicsContext lineWidth: stroke].
	offset == nil
		ifFalse: [aGraphicsContext translateBy: offset].
	(fill == true)
		ifFalse: [aGraphicsContext displayPolyline: vertices at: Point zero]
		ifTrue: [aGraphicsContext displayPolygon: vertices at: Point zero].
	offset == nil
		ifFalse: [aGraphicsContext translateBy: offset negated].
	stroke == nil
		ifFalse: [aGraphicsContext lineWidth: cachedLW].! !

!DisplayPolyline methodsFor: 'displaying' stamp: 'stp 12/1/2022 10:34:18'!
drawOn: aCanvas
	"Display the polyline on the given canvas"

	| pt1 pt2 |
	pt2 _ vertices first + offset.
	vertices do:
		[ :pt |
		pt1 _ pt2.
		pt2 _ pt + offset.
		aCanvas line: pt1 to: pt2 width: width color: color]
! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayPolyline class' category: 'MusicUI-DisplayLists'!
DisplayPolyline class
	instanceVariableNames: ''!

!DisplayPolyline class methodsFor: 'instance creation'!
offset: offPt vertices: arrayOfPoints
	"Answer a new DisplayPolyline with the arguments as its vertices."

	^self basicNew offset: offPt; vertices: arrayOfPoints! !

!DisplayPolyline class methodsFor: 'instance creation'!
vertices: arrayOfPoints
	"Answer a new DisplayPolyline with the arguments as its vertices."

	^self basicNew vertices: arrayOfPoints! !


!DisplayPolyline class methodsFor: 'examples'!
polylineExample
	"Open a display list view with randomly-placed random-color polylines over the given extent."
	"DisplayPolyline polylineExample"

	(DisplayList polylinesX: 600 byY: 600) display! !


!classDefinition: #DisplayList category: 'MusicUI-DisplayLists'!
DisplayItem subclass: #DisplayList
	instanceVariableNames: 'components'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayList commentStamp: '<historical>' prior: 0!
Instances of DisplayList are used for representing composed structured graphics. They can be nested (see the class examples) and can display themselves and their components.

Instance Variables:
	offset		<Point> the object's offset distance relative to its container!


!DisplayList methodsFor: 'accessing'!
color
	^ColorValue black! !

!DisplayList methodsFor: 'accessing'!
flatten
	"Answer a copy of the receiver with its hierarchy flattened."

	| new |
	new := DisplayList new.
	components do:
		[ :item |
		item hasItems
			ifTrue: [ | subnew |
				subnew := item flatten components.
				subnew do: [ :subitem |
					new add: (subitem translateBy: item offset)]]
			ifFalse: [new add: item]].
	^new! !

!DisplayList methodsFor: 'accessing' stamp: 'stp 12/2/2022 02:41:28'!
initialize
	"Set up the receiver"

	components _ OrderedCollection new.
	offset _ 0 @ 0! !

!DisplayList methodsFor: 'accessing'!
itemWithin: dist of: point
	"Answer the list of items whose boundingBoxes are entirely within the given rectangle."

	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [ | item |
				(item := comp itemWithin: dist of: (point - self offset)) == nil
					ifFalse: [^item]]
			ifFalse: [((comp bounds expandedBy: dist) containsPoint: point)
					ifTrue: [^comp]]].
	^nil! !

!DisplayList methodsFor: 'accessing'!
itemsFromX: x1 toX: x2
	"Answer the list of items whose offsets are within the given X range."

	| dList |
	dList := DisplayList new.
	components do: 
		[ :i |
		i hasItems
			ifTrue:
				[ | tList |
				tList := i itemsFromX: x1 + i offset x 
							toX: x2 + i offset x.
				tList isEmpty ifFalse: [dList add: tList at: i offset]]
			ifFalse:
				[(i offset x between: x1 and: x2) ifTrue: [dList add: i]]].
	^dList! !

!DisplayList methodsFor: 'accessing'!
itemsIntersecting: rect
	"Answer the list of items whose boundingBoxes intersect the given rectangle. 
	Clip them to the box's border if necessary."

	| dList dList2 |
	dList := DisplayList new.
	components do: 
		[ :i |
		i hasItems
			ifTrue: [dList2 := i itemsIntersecting: (rect translateBy: self offset).
				dList2 == nil
					ifFalse: [dList add: (dList2 offset: i offset)]]
			ifFalse: [(((i boundingBox) translateBy: self offset) 
								intersects: rect)
					ifTrue: [dList add: (i clipTo: (rect translateBy: ((0@0) - self offset)))]]].
	^(dList components isEmpty)
		ifTrue: [nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [dList components first]
			ifFalse: [dList]]! !

!DisplayList methodsFor: 'accessing'!
itemsWithin: rect
	"Answer the list of items whose boundingBoxes are entirely within the given rectangle."

	| dList movedRect |
	movedRect := rect moveBy: self offset negated.
	dList := DisplayList new.
	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [ | items |
				items := comp itemsWithin: movedRect.
				items == nil ifFalse: [dList add: items]]
			ifFalse: [(movedRect contains: comp bounds) 
					ifTrue: [dList add: comp]]].
	(dList components isEmpty)
		ifTrue: [^nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [ | first |
				first := dList components first.
				first hasItems
					ifTrue: [first container: nil].
				^first copy translateBy: self offset]
			ifFalse: [^dList translateBy: self offset]]! !

!DisplayList methodsFor: 'accessing'!
itemsWithin: dist of: point
	"Answer the list of items whose boundingBoxes are entirely within the given rectangle."

	| dList |
	dList := DisplayList new.
	components do: 
		[ :comp |
		comp hasItems
			ifTrue: [dList add: 
					(comp itemsWithin: dist of: (point - self offset))]
			ifFalse: [((comp bounds expandBy: dist) containsPoint: point)
					ifTrue: [dList add: comp]]].
	^(dList components isEmpty)
		ifTrue: [nil]
		ifFalse: [(dList components size = 1)
			ifTrue: [dList components first]
			ifFalse: [dList]]! !

!DisplayList methodsFor: 'accessing'!
modelsFromX: x1 toX: x2
	"Answer the list of model-space-items whose offsets are within the given X range."

	| dList |
	dList := DisplayList new.
	components do: 
		[ :i |
		((i offset x >= x1) and: [i offset x <= x2])
				ifTrue: [dList add: i]].
	^dList collect: [ :i | i model]! !

!DisplayList methodsFor: 'accessing'!
nodeAt: aPath
	"Answer the element described by the collection of items in the argument."

	| next path2 |
	components isEmpty ifTrue: [^nil].
	next := components
			detect: [ :i | 
					i "component" model == aPath first]
			ifNone: [nil].
	next == nil
		ifFalse: [aPath size = 1
			ifTrue: [^next]
			ifFalse: [path2 := aPath copy.
				path2 removeFirst.
				^next nodeAt: path2]]
		ifTrue: [^nil]! !

!DisplayList methodsFor: 'accessing'!
nodeFor: aModel
	"Answer the element whose model is the argument."

	components isEmpty ifTrue: [^nil].
	^components
			detect: [ :i | 
					i model == aModel]
			ifNone: [nil].! !

!DisplayList methodsFor: 'accessing'!
offset
	"Answer the receiver's offset."

	^offset == nil
		ifTrue: [0@0]
		ifFalse: [offset]! !

!DisplayList methodsFor: 'accessing'!
offset: aPoint
	"Set the receiver's offset."

	offset := aPoint! !

!DisplayList methodsFor: 'accessing'!
wrapperClass
	"Raise an error."

	self error: 'I don''t use Wrappers!!'! !


!DisplayList methodsFor: 'transforming'!
scaledBy: aPoint
	"Scale the receiver's offset by the argument."

	| newMe |
	newMe := self class new.
	self do:
		[ :item |
		newMe add: (item scaledBy: aPoint) at: item offset * aPoint].
	^newMe! !

!DisplayList methodsFor: 'transforming'!
translateBy: aPoint
	"Translate the receiver's offset by the argument."

	self offset: self offset + aPoint! !


!DisplayList methodsFor: 'printing'!
printCompleteOn: aStream depth: depth
	"Descend the hierarchy printing on the stream."

	aStream cr.
	depth timesRepeat: [aStream tab].
	aStream nextPutAll: '(a DisplayList '.
	offset == nil
		ifFalse: [aStream nextPutAll: ' offset: ', offset printString].
"	model == nil
		ifFalse: [aStream nextPutAll: ' model: ', model printString]."
	components == nil
		ifFalse: [components do: 
			[ :e | 
			e printCompleteOn: aStream depth: depth + 1]].
	aStream nextPut: $)! !

!DisplayList methodsFor: 'printing'!
printCompletePostScriptOn: aStream depth: depth
	"Descend the hierarchy printing on the stream."

	aStream cr.
	depth timesRepeat: [aStream tab].
	aStream nextPutAll: '(a DisplayList '.
	offset == nil
		ifFalse: [aStream nextPutAll: ' offset: ', offset printString].
"	model == nil
		ifFalse: [aStream nextPutAll: ' model: ', model printString]."
	components == nil
		ifFalse: [components do: 
			[ :e | 
			e printCompleteOn: aStream depth: depth + 1]].
	aStream nextPut: $)! !

!DisplayList methodsFor: 'printing' stamp: 'stp 11/20/2022 11:08:48'!
printOn: aStream
	"Print the receiver on the argument using the recursive method."

	^Sensor shiftPressed
		ifTrue: [super printOn: aStream]
		ifFalse: [self printCompleteOn: aStream depth: 0]! !

!DisplayList methodsFor: 'printing'!
printPostScriptOn: aStream
	"Print the receiver on the argument using the recursive method."

	^self printCompletePostScriptOn: aStream depth: 0! !


!DisplayList methodsFor: 'testing'!
hasItems
	"Answer whether or not the receiver has items or components."

	^true! !

!DisplayList methodsFor: 'testing'!
isEmpty
	"Amswer whether the receiver has any items."

	^components isEmpty! !


!DisplayList methodsFor: 'bounds accessing' stamp: 'stp 11/29/2022 16:06:18'!
computePreferredBounds
	"Compute the receiver's preferredBounds"

	| preferredBounds |
	components isEmpty
		ifTrue: [preferredBounds := (0@0) extent: (0@0)]
		ifFalse: [preferredBounds := components first bounds].
	1 to: components size do:
		[:i |
		preferredBounds := preferredBounds merge: (components at: i) bounds].
	preferredBounds origin < (0@0)
		ifTrue: [preferredBounds := preferredBounds 
				translatedBy: preferredBounds origin negated].
	offset == nil
		ifTrue: [preferredBounds := preferredBounds expandBy: (64@64)]
		ifFalse: [preferredBounds := (preferredBounds translatedBy: offset) expandBy: (64@64)].
	^preferredBounds! !

!DisplayList methodsFor: 'bounds accessing' stamp: 'stp 11/29/2022 16:04:32'!
extent
	"Answer the extent of the receiver."

	^self computePreferredBounds extent! !


!DisplayList methodsFor: 'enumerating'!
do: aBlock
	"Iterate the argument block over the receiver's components."

	components do: aBlock! !


!DisplayList methodsFor: 'displaying' stamp: 'stp 12/2/2022 02:28:49'!
display
	"Open a DisplayListView on the receiver."

	| view |
	view _ DisplayListView newOn: self.
	DisplayListWindow open: view label: 'DisplayListView'! !

!DisplayList methodsFor: 'displaying'!
displayNonCached
	"Open a DisplayListView on the receiver."

	^DisplayListView openNonCached: self! !

!DisplayList methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"Display each of the receiver's components."

	| clipBox |
	clipBox := aGraphicsContext clippingBounds.
	offset ifNotNil: [aGraphicsContext translateBy: offset].
	components do:
		[ :c | (c intersects: clipBox)
			ifTrue: [c displayOn: aGraphicsContext]].
	offset ifNotNil: [aGraphicsContext translateBy: offset negated].! !

!DisplayList methodsFor: 'displaying'!
displayPostScriptOn: aPostscriptContext
	"Display each of the receiver's components."

	offset == nil
		ifFalse: [aPostscriptContext translate: offset].
	1 to: components size do:
		[ :i | (components at: i) displayPostScriptOn: aPostscriptContext].
	offset == nil
		ifFalse: [aPostscriptContext translateBy: offset negated].! !

!DisplayList methodsFor: 'displaying' stamp: 'stp 12/2/2022 02:56:11'!
drawOn: aContext
	"Display each of the receiver's components."

	| xform |
	xform := aContext currentTransformation.
	offset ifNotNil: [aContext geometryTransformation: (xform translatedBy: offset)].
	components do:
		[ :comp | comp drawOn: aContext].
	offset ifNotNil: [aContext geometryTransformation: (xform translatedBy: offset negated)].! !


!DisplayList methodsFor: 'adding-removing'!
add: aVisualComponent
	"Add the argument to the receiver."

	^self addComponent: aVisualComponent! !

!DisplayList methodsFor: 'adding-removing'!
add: aVisualComponent at: aPoint
	"Add aVisualComponent to the receiver's components with its offset set to aPoint."

	aVisualComponent offset: aPoint.
	self add: aVisualComponent! !

!DisplayList methodsFor: 'adding-removing'!
add: anItem atPath: aPath
	"Add the first argument at the node described by the second."

	| node |
	(aPath == nil or: [aPath isEmpty])
		ifTrue: [^self add: anItem].
	node := self nodeAt: aPath.
	node == nil
		ifTrue: [self addComponent: anItem]
		ifFalse: [node addComponent: anItem]! !

!DisplayList methodsFor: 'adding-removing'!
addAll: aCollection
	"Add the argument to the receiver."

	aCollection do: [ :item | self add: item]! !

!DisplayList methodsFor: 'adding-removing'!
addComponent: aVisualComponent
	"Add the argument to the receiver."

	components addLast: aVisualComponent.
	aVisualComponent container: self! !

!DisplayList methodsFor: 'adding-removing'!
addWrapper: aVisualWrapper
	"Raise an error."

	self error: 'I don''t use Wrappers!!'! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayList class' category: 'MusicUI-DisplayLists'!
DisplayList class
	instanceVariableNames: ''!

!DisplayList class methodsFor: 'instance creation' stamp: 'stp 11/20/2022 10:34:06'!
new
	"Initialize instances of the receiver."

	^super new initialize! !


!DisplayList class methodsFor: 'examples' stamp: 'stp 12/2/2022 02:58:44'!
exampleHierarchical
	"Create and answer a large display list with a lines, strings, and visuals."
	"DisplayList exampleHierarchical display"

	| dl1 dl2 dl3 dl4 dl5 dl6 rand size |
	size := 200.
	dl1 := DisplayList new.
	dl2 := DisplayList new.
	dl3 := DisplayList new.
	dl4 := DisplayList new.
	dl5 := DisplayList new.
	dl6 := DisplayList new.
	rand := Random new.
	10 timesRepeat:
		[ | pt |
		pt := (rand next * size) rounded @ ((rand next * size) rounded).
		dl1 add: ((DisplayString string: (pt printString) 
				offset: pt) color: (Color random: rand))].
	10 timesRepeat:
		[ | x y w h |
		x := (rand next * size) rounded.		y := (rand next * size) rounded.
		w := (rand next * size) rounded.		h :=  (rand next * size) rounded.
		dl2 add: ((DisplayRectangle offset: (x@y) extent: (w@h))
				color: (Color random: rand); fill: true)].
	10 timesRepeat:
		[ | x y w h |
		x := (rand next * size) rounded.		y := (rand next * size) rounded.
		w := (rand next * size) rounded.		h :=  (rand next * size) rounded.
		dl3 add: ((DisplayRectangle offset: (x@y) extent: (w@h))
				color: (Color random: rand); fill: true)].
	10 timesRepeat:
		[ | pt |
		pt := (rand next * size) rounded @ ((rand next * size) rounded).
		dl4 add: ((DisplayString string: (pt printString) 
				offset: pt) color: (Color random: rand))].
	dl5 add: dl1; 
		add: dl2 at: ((size * 2)@0); 
		add: dl3 at: (0@(size * 2)); 
		add: dl4 at: ((size * 2)@(size * 2)).
	dl6 add: dl5 at: (50@50).
	^dl6! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 12/2/2022 02:40:29'!
gridExample
	"Create and answer a large display list with lines and strings."
	"DisplayList gridExample display"
	"DisplayListView example"

	| dList rand ext step |
	ext := 1000.
	step := 50.
	dList := DisplayList new.
	0 to: (ext/step) do:
		[ :i |
		dList add: (DisplayLine offset: (i*step)@0 extent: 0@ext).
		dList add: (DisplayLine offset: 0@(i*step) extent: ext@0)].
	rand := Random new.
	1 to: 50 do:
		[ :i | | pt |
		pt := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: (DisplayString string: (pt printString) offset: pt)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
gridExampleX: x byY: y
	"Create and answer a large display list with lines and strings."
	"(DisplayList gridExampleX: 3000 byY: 500) display"
	"DisplayListView example"

	| dList rand step |
	step := 50.
	dList := DisplayList new.
	0 to: (x/step) do:
		[ :i |
		dList add: (DisplayLine offset: (i*step)@0 extent: 0@y)].
	0 to: (y/step) do:
		[ :i |
		dList add: (DisplayLine offset: 0@(i*step) extent: x@0)].
	rand := Random new.
	1 to: (x * y / 20000) do:
		[ :i | | pt |
		pt := (rand next * x) rounded @ ((rand next * y) rounded).
		dList add: (DisplayString string: (pt printString) offset: pt)].
	^dList! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 11/30/2022 12:39:00'!
linesX: x byY: y
	"Answer a display list with randomly-placed random-color rectangles over the given extent."
	"(DisplayList linesX: 2000 byY: 2000) display"

	| dList rand x0 y0 x1 y1 scale w h |
	dList := DisplayList new.
	rand := Random new.
	scale := 0.
	x0 := x - (scale * 2).
	y0 := y - (scale * 2).
	((x/10) + (y/10)) timesRepeat:
		[x1 := scale + (rand next * x0) rounded.
		y1 := scale + (rand next * y0) rounded.
		w := (((rand next - 0.5) * (x / 20) abs) rounded).
		h :=  (((rand next - 0.5) * (y / 20) abs) rounded).
		dList add: ((DisplayLine
				offset: (x1@y1)
				extent: (w@h))
				color: Color random;
				width: (rand next * 4) truncated)].
	^dList! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 12/1/2022 21:38:23'!
mixedExample
	"Open a display list view with randomly-placed random-color rects, lines, p-lines and strings over the given extent."
	"DisplayList mixedExample"
	"DisplayListView exampleMixed"
	
	| x y |
	x _ 20000.
	y _ 2000.
	^((DisplayList rectanglesX: x byY: y) 
		addAll: (DisplayList linesX: x byY: y);
		addAll: (DisplayList polylinesX: x byY: y);
		addAll: (DisplayList visualsX: x byY: y);
		addAll: (DisplayList stringsX: x byY: y))! !

!DisplayList class methodsFor: 'examples'!
polylineExample
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"DisplayList polylineExample"

	(DisplayList polylinesX: 600 byY: 600 items: 100) display! !

!DisplayList class methodsFor: 'examples'!
polylineExampleHuge
	"Answer a display list with randomly-placed random-color polylines over the given (very large) extent."
	"DisplayList polylineExampleHuge"

	(DisplayList polylinesX: 6000 byY: 6000 items: 10000) display! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 11/30/2022 17:33:08'!
polylinesX: x byY: y
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"(DisplayList polylinesX: 600 byY: 600) display"

	| dList rand inset scale x0 y0 size pointArray |
	inset := 16.
	scale := 128.
	dList := DisplayList new.
	rand := Random new.
	x0 := rand nextIntFrom: inset to: x-inset.
	y0 := rand nextIntFrom: inset to: y-inset.
	((x/20) + (y/20)) timesRepeat:
		[size := rand nextIntFrom: 3 to: 7.
		pointArray := Array new: size.
		pointArray at: 1 put: (0@0).
		pointArray at: size put: (0@0).
		x0 := y0 := 0.
		2 to: size - 1 do: [ :i |
			x0 := x0 + (rand nextIntFrom: scale negated to: scale).
			y0 := y0 + (rand nextIntFrom: scale negated to: scale).
			pointArray at: i put: (x0 @ y0)].
		dList add: ((DisplayPolyline
				offset: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				width: 5 atRandom;
				color: Color random;
				vertices: pointArray)].
	^dList! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 12/2/2022 02:31:45'!
polylinesX: x byY: y items: num
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"(DisplayList polylinesX: 1200 byY: 1200 items: 1000) display"

	| dList rand inset scale x0 y0 size pointArray |
	inset := 16.
	scale := 128.
	dList := DisplayList new.
	rand := Random new.
	x0 := rand nextIntFrom: inset to: x-inset.
	y0 := rand nextIntFrom: inset to: y-inset.
	num timesRepeat:
		[size := rand nextIntFrom: 3 to: 7.
		pointArray := Array new: size.
		pointArray at: 1 put: (0@0).
		pointArray at: size put: (0@0).
		x0 := y0 := 0.
		2 to: size-1 do: [ :i |
			x0 := x0 + (rand nextIntFrom: scale negated to: scale).
			y0 := y0 + (rand nextIntFrom: scale negated to: scale).
			pointArray at: i put: (x0 @ y0)].
		dList add: ((DisplayPolyline
				offset: ((rand nextIntFrom: inset to: x-inset)
						@(rand nextIntFrom: inset to: y-inset)))
				width: 5 atRandom;
				color: Color random;
				vertices: pointArray)].
	^dList! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 12/2/2022 03:00:59'!
randomExample
	"Create and answer a large display list with a lines, strings, and visuals."
	"DisplayList randomExample display"

	| dList rand ext num |
	ext := 3000.
	dList := DisplayList new.
	rand := Random new.
	num := 500.
	num timesRepeat:
		[ | x y w h |					"add rectangles"
		x := (rand next * ext) rounded.
		y := (rand next * ext) rounded.
		w := (rand next * ext/30) rounded.
		h :=  (rand next * ext/30) rounded.
		dList add: ((DisplayRectangle
				offset: (x@y)
				extent: (w@h))
				color: Color random; fill: true)].
	num timesRepeat:
		[ | pt1 pt2 |					"add lines"
		pt1 := (rand next * ext) rounded @ ((rand next * ext) rounded).
		pt2 := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: ((DisplayLine offset: pt2 extent: (pt1 - pt2))
				color: Color random)].
	num timesRepeat:
		[ | pt |						"add strings"
		pt := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: ((DisplayString string: (pt printString) 
				offset: pt) color: Color random)].
	num timesRepeat:
		[ | pt coll |					"add visuals"
		coll := (DisplayListView constants at: #forms) values.
		pt := (rand next * ext) rounded @ ((rand next * ext) rounded).
		dList add: ((DisplayVisual visual: (coll atRandom) 
				offset: pt) color: Color random)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
rectangleExample
	"Answer a display list with randomly-placed random-color rectangles over the given extent."
	"DisplayList rectangleExample"

	(DisplayList rectanglesX: 2000 byY: 2000) display! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 11/19/2022 10:58:32'!
rectanglesX: x byY: y
	"Answer a display list with randomly-placed random-color rectangles over the given extent."
	"(DisplayList rectanglesX: 600 byY: 600) display"

	| dList rand x0 y0 x1 y1 scale w h |
	dList := DisplayList new.
	rand := Random new.
	scale := 0.
	x0 := x - (scale * 2).
	y0 := y - (scale * 2).
	((x/10) + (y/10)) timesRepeat:
		[x1 := scale + (rand next * x0) rounded.
		y1 := scale + (rand next * y0) rounded.
		w := ((rand next * 0.5 * (x / 20) abs) rounded).
		h :=  ((rand next * 0.5 * (y / 20) abs) rounded).
		dList add: ((DisplayRectangle
				offset: (x1@y1)
				extent: (w@h))
				color: Color random; fill: true)].
	^dList! !

!DisplayList class methodsFor: 'examples'!
stringExample
	"Open a display list view with randomly-placed random-color strings over the given extent."
	"DisplayList stringExample"

	(DisplayList stringsX: 4000 byY: 4000) display! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 11/19/2022 11:01:35'!
stringsX: x byY: y
	"Answer a display list with randomly-placed random-color strings over the given extent."
	"(DisplayList stringsX: 600 byY: 600) display"

	| dList rand inset |
	inset := 16.
	dList := DisplayList new.
	rand := Random new.
	((x/5) + (y/5)) timesRepeat:
		[ | pos |
		pos := (rand nextIntFrom: inset to: x - inset) @ (rand nextIntFrom: inset to: y - inset).
		dList add: ((DisplayString
				string: pos printString
				offset: pos)
				color: Color random)].
	^dList! !

!DisplayList class methodsFor: 'examples' stamp: 'stp 12/1/2022 21:31:48'!
visualsX: x byY: y
	"Answer a display list with randomly-placed random-color visuals over the given extent."
	"(DisplayList visualsX: 2000 byY: 2000) display"
	"DisplayListView exampleVisuals"

	| dList rand inset images |
	inset := 16.
	dList := DisplayList new.
	rand := Random new.
	images _ OrderedCollection new.
"Collect forms"
	(DisplayListView constants at: #forms) valuesDo:
		[ :item | images addLast: item].

"Collect notes"
	(DisplayListView constants at: #NoteIcons) do:
		[ :item | images addLast: item image].

"Collect notes"
	(DisplayListView constants at: #StdNoteIcons) valuesDo:
		[ :item | images addLast: item image.
			item image image offset: item offset].
		
"Create display list"
	((x/10) + (y/10)) timesRepeat:
		[ | vis |
		vis _ 	images atRandom: rand.
		dList add: ((DisplayVisual
				visual: vis
				offset: ((rand nextIntFrom: inset to: x - inset)
						@(rand nextIntFrom: inset to: y - inset) "+ vis image offset")))].
	^dList! !


!classDefinition: #DisplayString category: 'MusicUI-DisplayLists'!
DisplayItem subclass: #DisplayString
	instanceVariableNames: 'string font'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayString commentStamp: '<historical>' prior: 0!
Instances of DisplayString are used for visual text items in display lists.

Instance Variables:
	string		<String or Text> the object's string/text
	font		<ImplementationFont> the object's display font!


!DisplayString methodsFor: 'accessing'!
bounds
	"Answer a rectangle that circumscribes the receiver."

	^offset extent: self extent! !

!DisplayString methodsFor: 'accessing' stamp: 'stp 11/29/2022 16:44:29'!
extent
	"Answer a rectangle that circumscribes the receiver."

	^font == nil
		ifFalse: [(font widthOfString: string) @ font pointSize]
		ifTrue: [(string size * 12) @ 12]! !

!DisplayString methodsFor: 'accessing'!
font
	"Answer the receiver's font."

	^font! !

!DisplayString methodsFor: 'accessing'!
font: aFont
	"Set the receiver's font."

	font := aFont! !

!DisplayString methodsFor: 'accessing'!
string
	"Answer the receiver's string."

	^string! !

!DisplayString methodsFor: 'accessing' stamp: 'stp 11/29/2022 16:41:49'!
string: aString
	"Set the receiver's string."

	string _ aString.
	font _ FontFamily defaultFamilyAndPointSize! !


!DisplayString methodsFor: 'printing'!
printOn: aStream
	"comment stating purpose of message"

	aStream nextPutAll: ' (a ', self class name, ' offset: '.
	offset printOn: aStream.
	aStream nextPutAll: ' string: '.
	string printOn: aStream.
"	color == nil
		ifFalse: [aStream nextPutAll: ' color: '. color printOn: aStream].
"	aStream nextPut: $); cr.! !


!DisplayString methodsFor: 'displaying'!
displayOn: aGraphicsContext
	"display the receiver on the argument."

"	Transcript show: (aGraphicsContext translation  + offset) printString; space."
	font == nil
		ifFalse: [aGraphicsContext font: font].
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	aGraphicsContext displayString: string at: (offset + (0@14))! !

!DisplayString methodsFor: 'displaying'!
displayPostScriptOn: aPostscriptContext
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext show: string.
	aPostscriptContext translate: offset negated.! !

!DisplayString methodsFor: 'displaying' stamp: 'stp 12/1/2022 23:37:37'!
drawOn: aCanvas
	"Display a string on the given canvas"

	aCanvas drawString: string from: 1 to: string size 
		at: offset 
		font: font
		color: (color ifNil: [Color black] ifNotNil: [color])
! !


!DisplayString methodsFor: 'initialize-release' stamp: 'stp 11/20/2022 16:54:33'!
initialize
	"Initialize the instance variables of the receiver."

	super initialize.
	font := FontFamily defaultFamilyAndPointSize! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayString class' category: 'MusicUI-DisplayLists'!
DisplayString class
	instanceVariableNames: ''!

!DisplayString class methodsFor: 'examples'!
stringExample
	"Open a display list view with randomly-placed random-color strings over the given extent."
	"DisplayString stringExample"

	(DisplayList stringsX: 600 byY: 600) display! !


!DisplayString class methodsFor: 'instance creation'!
new
	"Answer a new initialized instance."

	^super new initialize! !

!DisplayString class methodsFor: 'instance creation'!
string: aString
	"Answer a new instance with the argument as its string."

	^self new string: aString! !

!DisplayString class methodsFor: 'instance creation'!
string: aString offset: oPoint
	"Answer a new instance with the arguments as its string and offset point."

	^self new string: aString; offset: oPoint! !


!classDefinition: #DisplayVisual category: 'MusicUI-DisplayLists'!
DisplayItem subclass: #DisplayVisual
	instanceVariableNames: 'image'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!
!DisplayVisual commentStamp: '<historical>' prior: 0!
Instances of DisplayVisual are used for displaying arbitrary visual objects (e.g., cached images) in display lists.

Instance Variables:
	visual		<VisualComponent> the object's visual object, typically an image!


!DisplayVisual methodsFor: 'accessing' stamp: 'stp 12/4/2022 11:44:38'!
bounds
	"Answer the visual's bounds translated by the receiver's translation."

"	^0@0 extent: image image extent
"	^offset == nil
		ifTrue: [(0@0 extent: image image extent) expandBy: 4]
		ifFalse: [(offset extent: image image extent) expandBy: 4]! !

!DisplayVisual methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:01:47'!
image
	"Answer the receiver's visual."

	^image! !

!DisplayVisual methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:01:57'!
image: aVisual
	"Set the receiver's visual."

	image := aVisual.
"	aVisual palette == nil
		ifTrue: [aVisual palette: MappedPalette colorDefault]
"! !

!DisplayVisual methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:02:10'!
visual
	"Answer the receiver's visual."

	^image! !

!DisplayVisual methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:02:15'!
visual: aVisual
	"Set the receiver's visual."

	image := aVisual.
"	aVisual palette == nil
		ifTrue: [aVisual palette: MappedPalette colorDefault]
"! !


!DisplayVisual methodsFor: 'printing' stamp: 'stp 12/1/2022 11:03:37'!
printOn: aStream
	"Print the receiver in the given Stream."

	aStream nextPutAll: '(a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	image == nil ifFalse: [aStream nextPutAll: ' image: '. image printOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $); cr.! !

!DisplayVisual methodsFor: 'printing' stamp: 'stp 12/1/2022 11:03:51'!
storeOn: aStream
	"Print the receiver in the given Stream."

	aStream nextPutAll: '('; store: self class. 
	image == nil ifFalse: [aStream nextPutAll: ' image: '. image storeOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $)! !


!DisplayVisual methodsFor: 'displaying' stamp: 'stp 12/1/2022 11:03:04'!
displayOn: aGraphicsContext
	"Display the receiver's visual on the given graphics context."

	color == nil
		ifFalse: [aGraphicsContext paint: color].
	aGraphicsContext translateBy: offset.
	image displayOn: aGraphicsContext.
	aGraphicsContext translateBy: offset negated.! !

!DisplayVisual methodsFor: 'displaying' stamp: 'stp 12/1/2022 11:04:11'!
displayPostScriptOn: aPostscriptContext
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext imageWidth: image width 
			height: image height 
			bits: image bits.
	aPostscriptContext translate: offset negated! !

!DisplayVisual methodsFor: 'displaying' stamp: 'stp 12/1/2022 23:19:10'!
drawOn: aCanvas
	"Display a rect on the given canvas"

	| form |
	form _ image image.
"	self halt.
	aCanvas image: (ColorForm mappingWhiteToTransparentFrom: image mask) at: offset.
"	aCanvas image: form at: offset "- form offset" "sourceRect: (form boundingBox expandBy: 16)"! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'DisplayVisual class' category: 'MusicUI-DisplayLists'!
DisplayVisual class
	instanceVariableNames: ''!

!DisplayVisual class methodsFor: 'examples'!
convertMusicConstants
	"Convert all the forms to opaque forms"
	"DisplayVisual convertMusicConstants. DisplayVisual displayMusicConstants"

	| all |
	all := DisplayListView constants.
	all keysAndValuesDo: [ :key :group |
		Transcript show: key printString; cr.
		(group isMemberOf: Dictionary)
			ifTrue: [group keysAndValuesDo:
				[ :str :item | | newI |
				(item isKindOf: DisplayVisual)
					ifTrue: [newI := item.
						item visual: (OpaqueImage figure: item visual 
							shape: (item visual convertToCoverageWithOpaquePixel: 1))]
					ifFalse: [(item isMemberOf: Depth1Image)
							ifTrue: [newI := OpaqueImage figure: item 
												shape: (item convertToCoverageWithOpaquePixel: 1)]].
				group at: str put: newI]].
		((group isMemberOf: Array) or: [(group isMemberOf: OrderedCollection)])
			ifTrue: [group do:
					[ :item |
					(item isMemberOf: OpaqueImage)
						ifFalse: [item visual: (OpaqueImage figure: item visual 
								shape: (item visual convertToCoverageWithOpaquePixel: 1))]]]]! !

!DisplayVisual class methodsFor: 'examples' stamp: 'stp 12/2/2022 10:24:17'!
displayMusicConstants
	"Draw a nice table of the hierarchical image dictionary."
	"DisplayVisual displayMusicConstants"

	| xstep y i ystep dl dlv |
	xstep := 80.
	ystep := 64.
	y := 0.
	dl _ DisplayList new.
	DisplayListView constants keysAndValuesDo: [ :title :dict |
		i := 0.
		dl add: (DisplayString string: title asString) at: (xstep@y).
		y := y + (ystep/2).
		dict keysAndValuesDo:
			[ :str :item |
			dl add: (DisplayString string: (str isNumber 
					ifTrue: [str printString] ifFalse: [str asString clipTo: 9]))
				 at: ((i*xstep+8)@y).
			dl add: (DisplayString string: (item isDisplayItem
					ifTrue: [item image image extent printString]
					ifFalse: [item image extent printString]))
				 at: ((i*xstep+8)@(y + 16)).
			item isDisplayItem
				ifTrue: [dl add: item at: ((i * xstep + 8) @ (y + 40))]
				ifFalse: [dl add: (DisplayVisual image: item) at: ((i * xstep + 8) @ (y + 40))].
			i := i + 1.
			i = 14 ifTrue: [i := 0. y := y + (ystep * 1.5)]].
		y := y + ystep + 24].
	dlv _ DisplayListView newOn: dl.
	dlv backgroundColor: Color gray.
	DisplayListWindow open: dlv label: 'Music Constants'! !

!DisplayVisual class methodsFor: 'examples'!
storeMusicConstants
	"Write out all the forms to opaque forms"
	"DisplayVisual storeMusicConstants"

	| fil all |
	fil := 'MusicConstants.st' asFilename writeStream.
	all := DisplayListView constants.
	all keysAndValuesDo: [ :key :group |
		Transcript show: key printString; cr.
		(group isMemberOf: Dictionary)
			ifTrue: [fil cr; nextPutAll: 'DisplayListView constants at: ', key printString,
						 ' put: IdentityDictionary new.'; cr.
					group keysAndValuesDo:
					[ :str :item | 
					fil cr; nextPutAll: '(DisplayListView constants at: ', key printString, 
						') at: ''', str, ''' asSymbol put: '.
					item storeOn: fil.
					fil nextPut: $!! ]].
		((group isMemberOf: Array) or: [(group isMemberOf: OrderedCollection)])
			ifTrue: [fil cr; nextPutAll: 'DisplayListView constants at: ', key printString,
						 ' put: OrderedCollection new.'; cr.
				group do:
					[ :item |
					fil cr; nextPutAll: '(DisplayListView constants at: ', key printString, ') add: '.
					item storeOn: fil.
					fil nextPut: $!! ]]].
	fil close! !

!DisplayVisual class methodsFor: 'examples'!
visualExample
	"Open a display list view with randomly-placed random-color visuals over the given extent."
	"DisplayVisual visualExample"

	(DisplayList visualsX: 600 byY: 600) display! !


!DisplayVisual class methodsFor: 'instance creation' stamp: 'stp 12/1/2022 11:01:38'!
image: aV
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) image: aV! !

!DisplayVisual class methodsFor: 'instance creation' stamp: 'stp 12/1/2022 11:16:52'!
image: aV offset: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !

!DisplayVisual class methodsFor: 'instance creation' stamp: 'stp 12/1/2022 11:16:57'!
image: aV origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !

!DisplayVisual class methodsFor: 'instance creation'!
model: aM visual: aV offset: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^self new model: aM; visual: aV; offset: aPt! !

!DisplayVisual class methodsFor: 'instance creation'!
visual: aV
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV! !

!DisplayVisual class methodsFor: 'instance creation'!
visual: aV offset: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !

!DisplayVisual class methodsFor: 'instance creation'!
visual: aV origin: aPt
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !


!classDefinition: #OpaqueImage category: 'MusicUI-DisplayLists'!
Object subclass: #OpaqueImage
	instanceVariableNames: 'image mask'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicUI-DisplayLists'!

!OpaqueImage methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:29:59'!
image
	"Answer the receiver's image form"

	^image! !

!OpaqueImage methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:29:54'!
image: aForm
	"Set the receiver's image form"

	image _ aForm! !

!OpaqueImage methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:30:05'!
mask
	"Answer the receiver's mask form"

	^mask! !

!OpaqueImage methodsFor: 'accessing' stamp: 'stp 12/1/2022 11:30:19'!
mask: aForm
	"Set the receiver's mask form"

	mask _ aForm! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'OpaqueImage class' category: 'MusicUI-DisplayLists'!
OpaqueImage class
	instanceVariableNames: ''!

!OpaqueImage class methodsFor: 'instance creation' stamp: 'stp 12/1/2022 18:18:09'!
image: im mask: ma
	"Answer an OpaqueImage with the given image and mask."

	| oi |
	oi _ self new.
	oi image: im. 
	oi mask: ma.
	^oi! !
