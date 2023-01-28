DisplayListController subclass: #FunctionController
	"Hit-detect the model for the given pt."

	| value transPt |
	transPt := view transformPoint: aPoint.
	value := self model at: (transPt x / view hRange) asFloat.
	((value * (view vRange)) - (view vRange - transPt y)) abs < 16
		ifTrue: [(self model hasPointNear: (transPt x / view hRange))
			ifTrue: [self dragPoint: transPt]
			ifFalse: [self addPoint: transPt]]! !
	"Answer the view's model at the receiver's model index."

	^view modelAt: modelIndex! !
	"Add a new point to the model near the current point."

	| x0 p1 p2 p3 index newFcn mod newPoint scalePoint |
	mod := self model.
	x0 := cPoint x / view hRange.
	index := mod indexOfPointNearestX: x0.
	(mod pointAt: index) x > x0
		ifTrue: [index := index - 1].
	p1 := ((mod pointAt: index) * view displayExtent) truncated.
	p2 := cPoint.
	p3 := ((mod pointAt: (index + 1)) * view displayExtent) truncated.
	p2 := self rubberBandLinesBetween: p1 and: p2 and: p3.
	newFcn := mod class ofSize: mod size + 1.
	1 to: index do:
		[ :i |
		newFcn add: (mod realPointAt: i)].
	newPoint := (mod realPointAt: index) copy.
	scalePoint := (p2 / view displayExtent) float.
	newPoint x: scalePoint x; y: scalePoint y.	"this is for handling ExpSegs"
	newFcn add: newPoint.
	index + 1 to: mod size do:
		[ :i |
		newFcn add: (mod realPointAt: i)].
	view modelAt: modelIndex put: newFcn.
	view invalidate! !
	"Drag the current point."

	|  p1 p2 p3 index mod oldPt |
	mod := self model.
	index := mod indexOfPointNearestX: (cPoint x / view hRange).
	index = 1
		ifFalse: [p1 := ((mod pointAt: (index - 1)) * view displayExtent) truncated].
	p2 := ((mod pointAt: index) * view displayExtent) truncated.
	index = mod size
		ifFalse: [p3 := ((mod pointAt: (index + 1)) * view displayExtent) truncated].
	p2 := self rubberBandLinesBetween: p1 and: p2 and: p3.
	oldPt := (mod realPointAt: index) copy.
	oldPt x: (p2 x / view hRange) asFloat; 
		y: ((1 - ((view vRange - p2 y) / view vRange) min: 1) max: 0) asFloat.
	mod pointAt: index put: oldPt.
	view invalidate! !
	answer the
	"Set the model index of the receiver to the argument."

	modelIndex := index! !
	"Clear the current function to the view's model array."

	view modelAt: modelIndex put: nil.
	view invalidate! !
	"Redraw the view and clear the selection."

	view invalidate! !

	super initialize.
	modelIndex := 1.! !
	"Mouse dragging"

	anEvent redButtonPressed ifFalse: [^self].
	model selection == nil
		ifTrue: [self selectEvent: anEvent]
		ifFalse: [self dragSelection: anEvent]! !
They can display up to 4 model functions on a fixed-size non-scrolling field.

Instance variables:
	models	 <Array of 1 to 4 Functions> the function array
	aScale <Number> the y-scale of the view
	vRange <Number> the vertical range, normally = aScale
	hRange <Number> the horizontal range
	colors <Array of 1 to 4 ColorValues> the colors for the 4 functions
	normalize <<Array of 1 to 4 Booleans> whether to normalize (to +- 1) the functions!
	"Set the receiver's model to be aModel"

	super setModel: aModel.
	models at: 1 put: aModel! !
	"Answer the receiver's 'aScale'."

	^aScale! !
	"Set the receiver's instance variable 'aScale' to be anObject."

	aScale := anObject! !
	"Answer the receiver's color array."

	^colors! !
	"Set the receiver's color array to the argument."

	colors := anArrayOfSize4! !
	"Answer the receiver's visual extent."

	^hRange@vRange! !

	normalize at: aNumber put: false.! !
	"Answer the receiver's 'hRange'."

	^hRange! !
	"Set the receiver's horizontal extent to aNumber."

	hRange := aNumber! !
	"Set the receiver's line width to the argument."

	lineWidth := aNumber! !
	"Answer the fisrt element from receiver's array of models."

	^models at: 1! !
	"Answer the receiver's model at the given index."

	^models at: anIndex! !
	"Set the receiver's model at the given index to the given function."

	models at: anIndex put: aFunction.
	controller model: aFunction.
	anIndex == 1
		ifTrue: [self model: aFunction]! !
	"Answer the receiver's first model."

	^models! !
	"Set the receiver's array of models to the argument."

	models := anArray.
	self model: (anArray at: 1)! !

	normalize at: aNumber put: true.! !
	"Set the receiver's array of normalization switches."

	normalize := arrayOfBooleans! !
	"Answer the receiver's 'vRange'."

	^vRange! !
	"Set the receiver's vertical extent to aNumber."

	vRange := aNumber! !
	"Set the receiver's bounds."

	super bounds: aRectangle.
	vRange := aRectangle height - 8.
	aScale := vRange.
	hRange := aRectangle width - 8.
	self setPixmap.
"	self invalidate"! !

	^FunctionController! !

"	Transcript show: 'FcnView size: ', hRange printString, ' @ ', vRange printString; cr."

	cache ifFalse: [aGraphicsContext paint: backgroundColor.
				aGraphicsContext displayRectangle: aGraphicsContext clippingBounds].
	self class inset == nil
		ifFalse: [aGraphicsContext translateBy: self class inset].
	self displayOnImage.
	pixmap displayOn: aGraphicsContext.
	self class inset == nil
		ifFalse: [aGraphicsContext translateBy: self class inset negated].! !
	"Display the receiver on its cached form."
	"FunctionView example"

	| x1 x2 y1 y2 ins |
	ins := self class inset x.
	x1 := (p1 x + ins) truncated. y1 := (vRange - p1 y + ins) truncated.
	x2 := (p2 x + ins) truncated. y2 := (vRange - p2 y + ins) truncated.
	gc paint: color.
	gc displayLineFrom: (x1@y1) to: (x2@y2).! !
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := y.
	hRange := x.
	aScale := y! !
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := 256.
	hRange := 512.
	aScale := 256! !
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := 192.
	hRange := 384.
	aScale := 192! !
	"Reset the receiver's pixmap display cache after changing its size."

	pixmap := Form extent: (hRange+1)@(vRange+1).
	pixmap fill: pixmap boundingBox
	"FunctionView example"

	FunctionView multiFunctionExample! !
	"Demonstrate an ExpSeg function with different weights."
	"FunctionView expSegExample"

	FunctionView onFunction: (ExponentialFunction from: #((0 0 3) (0.3 1 5) (0.6 0.3 0) (1 0)))! !
	"Read an extracted envelope as floating-point data from a file."
	"FunctionView fileExample"

	FunctionView onFunctions: (Function fromFile: 'G1.2a1.env') normalize: #(false true true false)! !
	"FunctionView linSegExample"

	FunctionView onFunction: (LinearFunction from: #((0 0) (0.15 1.0) (0.25 0.75) (0.75 0.5) (1 0)))! !
	"Read an extracted envelope as floating-point data from a file."
	"FunctionView smallFileExample"

	FunctionView smallOnFunction: (Function readFloatsFrom: 'snd/yy/ec/syl/y2.env')
"	FunctionView smallOnFunction: (Function readFloatsFrom: 'snd/yy/ec/syl/y2.2.env')"! !
	"Open a function editor on 4 different functions."
	"FunctionView smallMultiFunctionExample"

	 
	FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0 0) (0.12 1) (0.17 0.74) 
					(0.35 0.5) (0.9 0.4) (1 0)))
			with: (LinearFunction from: #((0 0.1) (0.3 0.5) (0.7 0.3) (1 0)))
			with: (ExponentialFunction from: #((0 0 5) (0.05 1 -5) (0.2 0.25 -1) (1 0)))
			with: (SplineFunction default))
		withColors: self defaultColors
		x: 384 y: 192! !
	"Answer the default line width for fnuction plots."

	^LineWidth! !
	"Set the default line width for fnuction plots."
	"FunctionView lineWidth: 1"

	LineWidth := anInteger! !
	"Set up the defaults"
	"FunctionView initialize"
	"FunctionView lineWidth: 1"

	LineWidth := 2! !
	"FunctionView onFunction: (LinearFunction from: #((0@0) (0.1@1.0)
								(0.15@0.75) (0.75@0.5) (1@0))) 
		x: 256 y: 128"

	^function range size > 1
		ifTrue: [self onFunctions: (Array with: function with: nil with: nil with: nil)
			withColors: self defaultColors
			normalize: #(true false false false) 
			x: x y: y]
		ifFalse: [self onFunctions: (Array with: function with: nil with: nil with: nil)
			withColors: self defaultColors
			normalize: #(false false false false) 
			x: x y: y]! !
			with: (LinearFunction from:
			with:
			with:
			with:
		withColors:
				with: ColorValue cyan
		normalize: #(false false false false)
		x: 512 y:
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.7@0.3) (1@0.7))))"
			with: (LinearFunction from:
			with:
			with:
			with:
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors: (Array with: Color blue with: Color red
				with: Color cyan with: Color green)"
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors: (Array with: Color blue with: Color red
				with: Color cyan with: Color green)
		normalize: #(false false false false)"
			with: (LinearFunction from:
			with:
			with:
			with:
		withColors:
				with: Color cyan
		normalize: #(false false false false)
		x: 512 y:
	"FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors: (Array with: ColorValue blue with: ColorValue red
				with: ColorValue cyan with: ColorValue green)
		x: 512 y: 256"

	self onFunctions: functionArray
		withColors: colorArray
		normalize: #(true true true true) 
		x: x y: y! !
	"FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		x: 512 y: 256"

	^self onFunctions: functionArray
		withColors: self defaultColors
		x: x y: y! !

	^self onFunction: function! !
	"FunctionView example"

	^self onFunctions: (Array with: function with: nil with: nil with: nil)
			x: 384 y: 192! !