Object subclass: #DisplayItem
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
	"Answer an encapsulated version of the receiver which understands
	VisualComponent protocol."

	^TranslatingWrapper on: self! !
	"Translate the receiver's offset by the argument."

	self extent: self extent * aPoint! !
	"Translate the receiver's offset by the argument."

	^self copy extent: self extent * aPoint! !
	"Translate the receiver's offset by the argument."

	self offset: self offset + aPoint! !
	"Print the receiver in the given Stream."

	self printOn: aStream.
	"aStream cr.
	depth timesRepeat: [aStream tab].
	aStream nextPutAll: '(a ', self class name. 
	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $).
	"! !
	"Print the receiver in the given Stream."

	aStream nextPutAll: '(a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $); cr.! !
	"Answer the receiver's bounds." 

	| ext|
	ext _ self extent.
	^offset extent: ((ext x max: 1) @ (ext y max: 1))! !
	"Answer the receiver's display color."

	^color! !
	"Set the receiver's display color to the argument."

	color := ((aVal isMemberOf: BlockClosure)
		ifTrue: [aVal value]
		ifFalse: [aVal])! !
	"Assign the container"

	container _ aDList! !
	"Answer the extent of the receiver (dummy in this class)."

	^1@1! !
	"Ignored"! !
	"Answer the model"

	^model! !
	"Assign the model"

	model _ obj! !
	"Answer the receiver's offset."

	^offset! !
	"Set the receiver's offset to the argument."

	offset := aPoint! !
	"Answer whether the receiver is a kind of DisplayItem"

	^true! !
	"Initialize the instance variables of the receiver."

	offset := 0@0! !
	"Display the receiver on the argument."

	self subclassResponsibility! !
	"Display the receiver on the argument in PostScript."

	self subclassResponsibility! !
	"Display the receiver on the given canvas."

	self subclassResponsibility! !
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
	"Answer a shallow copy of the receiver."

	^self shallowCopy! !
	"Answer the receiver's bounds--hack."

	^self bounds! !
	"Answer an instance of DisplayItem with the given instance variables."

	^self new model: aM; offset: aPt! !
	"Answer an instance of DisplayItem with the given instance variables."

	^self new offset: aPt! !

Instance Variables:
	width		<Number> the object's line width
	extent		<Point> the object's visual extent!
	"Set the corner of the receiver (offset + extent)."

	offset == nil
		ifFalse: [extent := aPoint - offset].
"	end := (offset == nil
			ifTrue: [aPoint]
			ifFalse: [start + aPoint])."
	^self! !
	"Answer the extent of the receiver."

	^extent! !
	"Set the extent of the receiver."

	extent := aPoint
"	end := (offset == nil
			ifTrue: [aPoint]
			ifFalse: [start + aPoint])"! !
	width := aVal! !
	"Print the receiver in the given Stream."

	aStream nextPutAll: ' (a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	extent == nil ifFalse: [aStream nextPutAll: ' extent: '. extent printOn: aStream].
"	color == nil ifFalse: [aStream nextPutAll: ' color: '. color printOn: aStream]."
	aStream nextPut: $); cr.! !
	"Display a line between startPoint and endPoint."

"	Transcript show: start printString, '	', end printString; cr."
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	width == nil
		ifFalse: [aGraphicsContext lineWidth: width].
	aGraphicsContext displayLineFrom: offset to: (offset + extent)! !
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext rlineto: extent	.
	aPostscriptContext translate: offset negated.! !
	"Display a line on the given canvas"

	| rect c w |
	rect _ self bounds.
	w _ width.
	w ifNil: [w _ 1].
	c _ color.
	c ifNil: [c _ Color black].
	aCanvas line: rect origin to: rect corner width: w color: c! !
	"Answer the receiver's bounds." 

	^offset extent: extent "((extent x max: 1) @ (extent y max: 1))"! !
	"Answer an initialized instance."

	^((self new) offset: oPoint) extent: (endPoint - oPoint)! !
	"Answer an initialized instance."

	^((self new) offset: oPoint) extent: ePoint! !

Instance Variables:
	offset		<Point> the object's offset distance relative to its container!
	^ColorValue black! !
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
	"Set up the receiver"

	components _ OrderedCollection new.
	offset _ 0 @ 0! !
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
	"Answer the list of model-space-items whose offsets are within the given X range."

	| dList |
	dList := DisplayList new.
	components do: 
		[ :i |
		((i offset x >= x1) and: [i offset x <= x2])
				ifTrue: [dList add: i]].
	^dList collect: [ :i | i model]! !
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
	"Answer the element whose model is the argument."

	components isEmpty ifTrue: [^nil].
	^components
			detect: [ :i | 
					i model == aModel]
			ifNone: [nil].! !
	"Answer the receiver's offset."

	^offset == nil
		ifTrue: [0@0]
		ifFalse: [offset]! !
	"Set the receiver's offset."

	offset := aPoint! !
	"Raise an error."

	self error: 'I don''t use Wrappers!!'! !
	"Scale the receiver's offset by the argument."

	| newMe |
	newMe := self class new.
	self do:
		[ :item |
		newMe add: (item scaledBy: aPoint) at: item offset * aPoint].
	^newMe! !
	"Translate the receiver's offset by the argument."

	self offset: self offset + aPoint! !
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
	"Print the receiver on the argument using the recursive method."

	^Sensor shiftPressed
		ifTrue: [super printOn: aStream]
		ifFalse: [self printCompleteOn: aStream depth: 0]! !
	"Print the receiver on the argument using the recursive method."

	^self printCompletePostScriptOn: aStream depth: 0! !
	"Answer whether or not the receiver has items or components."

	^true! !
	"Amswer whether the receiver has any items."

	^components isEmpty! !
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
		ifFalse: [preferredBounds := (preferredBounds translateBy: offset) expandBy: (64@64)].
	^preferredBounds! !
	"Answer the extent of the receiver."

	^self computePreferredBounds extent! !
	"Iterate the argument block over the receiver's components."

	components do: aBlock! !
	"Open a DisplayListView on the receiver."

	| view |
	view _ DisplayListView newOn: self.
	DisplayListWindow open: view label: 'DisplayListView'! !
	"Open a DisplayListView on the receiver."

	^DisplayListView openNonCached: self! !
	"Display each of the receiver's components."

	| clipBox |
	clipBox := aGraphicsContext clippingBounds.
	offset ifNotNil: [aGraphicsContext translateBy: offset].
	components do:
		[ :c | (c intersects: clipBox)
			ifTrue: [c displayOn: aGraphicsContext]].
	offset ifNotNil: [aGraphicsContext translateBy: offset negated].! !
	"Display each of the receiver's components."

	offset == nil
		ifFalse: [aPostscriptContext translate: offset].
	1 to: components size do:
		[ :i | (components at: i) displayPostScriptOn: aPostscriptContext].
	offset == nil
		ifFalse: [aPostscriptContext translateBy: offset negated].! !
	"Display each of the receiver's components."

	| xform |
	xform := aContext currentTransformation.
	offset ifNotNil: [aContext geometryTransformation: (xform translatedBy: offset)].
	components do:
		[ :comp | comp drawOn: aContext].
	offset ifNotNil: [aContext geometryTransformation: (xform translatedBy: offset negated)].! !
	"Add the argument to the receiver."

	^self addComponent: aVisualComponent! !
	"Add aVisualComponent to the receiver's components with its offset set to aPoint."

	aVisualComponent offset: aPoint.
	self add: aVisualComponent! !
	"Add the first argument at the node described by the second."

	| node |
	(aPath == nil or: [aPath isEmpty])
		ifTrue: [^self add: anItem].
	node := self nodeAt: aPath.
	node == nil
		ifTrue: [self addComponent: anItem]
		ifFalse: [node addComponent: anItem]! !
	"Add the argument to the receiver."

	aCollection do: [ :item | self add: item]! !
	"Add the argument to the receiver."

	components addLast: aVisualComponent.
	aVisualComponent container: self! !
	"Raise an error."

	self error: 'I don''t use Wrappers!!'! !
	"Initialize instances of the receiver."

	^super new initialize! !
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
	"Answer a display list with randomly-placed random-color polylines over the given extent."
	"DisplayList polylineExample"

	(DisplayList polylinesX: 600 byY: 600 items: 100) display! !
	"Answer a display list with randomly-placed random-color polylines over the given (very large) extent."
	"DisplayList polylineExampleHuge"

	(DisplayList polylinesX: 6000 byY: 6000 items: 10000) display! !
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
	"Answer a display list with randomly-placed random-color rectangles over the given extent."
	"DisplayList rectangleExample"

	(DisplayList rectanglesX: 2000 byY: 2000) display! !
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
	"Open a display list view with randomly-placed random-color strings over the given extent."
	"DisplayList stringExample"

	(DisplayList stringsX: 4000 byY: 4000) display! !
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

Instanve Variables:
	fill		<Boolean or nil> whether or not to fill the receiver on display (no by default)!
	"Set the receiver's filling Boolean."

	fill := aBoolean! !
	"Set the receiver's stroke line thickness."

	stroke := aNum! !
	"Display a line between startPoint and endPoint."

"	Transcript show: start printString, '	', end printString; cr."
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	(fill == nil or: [fill not])
		ifFalse: [aGraphicsContext displayRectangle: (offset extent: extent)]
		ifTrue: [aGraphicsContext displayRectangularBorder: (offset extent: extent)]! !
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext box: extent y width: extent x.
	aPostscriptContext translate: offset negated.! !
	"Display a rect on the given canvas"

	aCanvas fillRectangle: self bounds
		color: color
		borderWidth: 1
		borderStyleSymbol: #simple
		baseColorForBorder: Color darkGray! !
	"Open a display list view with randomly-placed random-color rectangles over the given extent."
	"DisplayRectangle rectangleExample"

	(DisplayList rectanglesX: 600 byY: 600) display! !

Instance Variables:
	vertices	<OrderedCollection of Point>  my point array
!
	"Answer the receiver's bounds." 

	| ext |
	ext := 0@0.
	vertices do: [ :pt | ext := ext max: pt].
	^offset extent: ext! !
	"Answer the receiver's extent." 

	^self bounds extent! !
	"Set the receiver's vertices."

	vertices := pointArray asArray! !
	"Scale all the receiver's points by the argument; answer a copy."

	|  newPts |
	newPts := vertices collect: [ :pt | (pt * aPoint) truncated].
	^self class offset: offset vertices: newPts! !
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
	"Display the polyline on the given canvas"

	| pt1 pt2 |
	pt2 _ vertices first + offset.
	vertices do:
		[ :pt |
		pt1 _ pt2.
		pt2 _ pt + offset.
		aCanvas line: pt1 to: pt2 width: width color: color]
! !
	"Answer a new DisplayPolyline with the arguments as its vertices."

	^self basicNew offset: offPt; vertices: arrayOfPoints! !
	"Answer a new DisplayPolyline with the arguments as its vertices."

	^self basicNew vertices: arrayOfPoints! !
	"Open a display list view with randomly-placed random-color polylines over the given extent."
	"DisplayPolyline polylineExample"

	(DisplayList polylinesX: 600 byY: 600) display! !

Instance Variables:
	string		<String or Text> the object's string/text
	font		<ImplementationFont> the object's display font!
	"Answer a rectangle that circumscribes the receiver."

	^offset extent: self extent! !
	"Answer a rectangle that circumscribes the receiver."

	^font == nil
		ifFalse: [(font widthOfString: string) @ font pointSize]
		ifTrue: [(string size * 12) @ 12]! !
	"Answer the receiver's font."

	^font! !
	"Set the receiver's font."

	font := aFont! !
	"Answer the receiver's string."

	^string! !
	"Set the receiver's string."

	string _ aString.
	font _ FontFamily defaultFamilyAndPointSize! !
	"comment stating purpose of message"

	aStream nextPutAll: ' (a ', self class name, ' offset: '.
	offset printOn: aStream.
	aStream nextPutAll: ' string: '.
	string printOn: aStream.
"	color == nil
		ifFalse: [aStream nextPutAll: ' color: '. color printOn: aStream].
"	aStream nextPut: $); cr.! !
	"display the receiver on the argument."

"	Transcript show: (aGraphicsContext translation  + offset) printString; space."
	font == nil
		ifFalse: [aGraphicsContext font: font].
	color == nil
		ifFalse: [aGraphicsContext paint: color].
	aGraphicsContext displayString: string at: (offset + (0@14))! !
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext show: string.
	aPostscriptContext translate: offset negated.! !
	"Display a string on the given canvas"

	aCanvas drawString: string from: 1 to: string size 
		at: offset 
		font: font
		color: (color ifNil: [Color black] ifNotNil: [color])
! !
	"Initialize the instance variables of the receiver."

	super initialize.
	font := FontFamily defaultFamilyAndPointSize! !
	"Open a display list view with randomly-placed random-color strings over the given extent."
	"DisplayString stringExample"

	(DisplayList stringsX: 600 byY: 600) display! !
	"Answer a new initialized instance."

	^super new initialize! !
	"Answer a new instance with the argument as its string."

	^self new string: aString! !
	"Answer a new instance with the arguments as its string and offset point."

	^self new string: aString; offset: oPoint! !

Instance Variables:
	visual		<VisualComponent> the object's visual object, typically an image!
	"Answer the visual's bounds translated by the receiver's translation."

"	^0@0 extent: image image extent
"	^offset == nil
		ifTrue: [(0@0 extent: image image extent) expandBy: 8]
		ifFalse: [(offset extent: image image extent) expandBy: 32]! !
	"Answer the receiver's visual."

	^image! !
	"Set the receiver's visual."

	image := aVisual.
"	aVisual palette == nil
		ifTrue: [aVisual palette: MappedPalette colorDefault]
"! !
	"Answer the receiver's visual."

	^image! !
	"Set the receiver's visual."

	image := aVisual.
"	aVisual palette == nil
		ifTrue: [aVisual palette: MappedPalette colorDefault]
"! !
	"Print the receiver in the given Stream."

	aStream nextPutAll: '(a ', self class name. 
"	model == nil ifFalse: [aStream nextPutAll: ' model: '. model printOn: aStream].
"	image == nil ifFalse: [aStream nextPutAll: ' image: '. image printOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $); cr.! !
	"Print the receiver in the given Stream."

	aStream nextPutAll: '('; store: self class. 
	image == nil ifFalse: [aStream nextPutAll: ' image: '. image storeOn: aStream].
	offset == nil ifFalse: [aStream nextPutAll: ' offset: '. offset printOn: aStream].
	aStream nextPut: $)! !
	"Display the receiver's visual on the given graphics context."

	color == nil
		ifFalse: [aGraphicsContext paint: color].
	aGraphicsContext translateBy: offset.
	image displayOn: aGraphicsContext.
	aGraphicsContext translateBy: offset negated.! !
	"Display the receiver on the argument as a PostScript item."

	aPostscriptContext translate: offset.
	aPostscriptContext imageWidth: image width 
			height: image height 
			bits: image bits.
	aPostscriptContext translate: offset negated! !
	"Display a rect on the given canvas"

	| form |
	form _ image image.
"	self halt.
	aCanvas image: (ColorForm mappingWhiteToTransparentFrom: image mask) at: offset.
"	aCanvas image: form at: offset "- form offset" "sourceRect: (form boundingBox expandBy: 16)"! !
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
	"Open a display list view with randomly-placed random-color visuals over the given extent."
	"DisplayVisual visualExample"

	(DisplayList visualsX: 600 byY: 600) display! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) image: aV! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !
	"Answer an instance of DisplayItem with the given instance variables."

	^self new model: aM; visual: aV; offset: aPt! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !
	"Answer an instance of DisplayItem with the given instance variables."

	^(super new initialize) visual: aV; offset: aPt! !
	"Answer the receiver's image form"

	^image! !
	"Set the receiver's image form"

	image _ aForm! !
	"Answer the receiver's mask form"

	^mask! !
	"Set the receiver's mask form"

	mask _ aForm! !
	"Answer an OpaqueImage with the given image and mask."

	| oi |
	oi _ self new.
	oi image: im. 
	oi mask: ma.
	^oi! !