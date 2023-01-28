DisplayListController subclass: #FunctionController	instanceVariableNames: 'modelIndex'	classVariableNames: ''	poolDictionaries: ''	category: 'MusicUI-Functions'!!FunctionController commentStamp: '<historical>' prior: 0!Missing comment!!FunctionController methodsFor: 'control defaults' stamp: 'stp 12/31/2022 11:31'!redButtonActivity	"Hit-detect items from the display list or drag a selection."	| cPoint value |	self model == nil		ifTrue: [^ nil].	cPoint := Sensor cursorPoint - view class inset max: 0 @ 0.	value := self model at: (cPoint x / view hRange) asFloat.	"If you're clicking near the function"	(value * view vRange - (view vRange - cPoint y)) abs < 16		ifTrue: [(self model hasPointNear: cPoint x / view hRange)				ifTrue: [self dragPoint: cPoint]				ifFalse: [self addPoint: cPoint]]! !!FunctionController methodsFor: 'control defaults'!selectAtPoint: aPoint
	"Hit-detect the model for the given pt."

	| value transPt |
	transPt := view transformPoint: aPoint.
	value := self model at: (transPt x / view hRange) asFloat.
	((value * (view vRange)) - (view vRange - transPt y)) abs < 16
		ifTrue: [(self model hasPointNear: (transPt x / view hRange))
			ifTrue: [self dragPoint: transPt]
			ifFalse: [self addPoint: transPt]]! !!FunctionController methodsFor: 'accessing'!model
	"Answer the view's model at the receiver's model index."

	^view modelAt: modelIndex! !!FunctionController methodsFor: 'private'!addPoint: cPoint
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
	view invalidate! !!FunctionController methodsFor: 'private'!dragPoint: cPoint
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
	view invalidate! !!FunctionController methodsFor: 'private' stamp: 'stp 12/31/2022 11:32'!rubberBandLinesBetween: p1 and: p2 and: p3 	"Drag two lines between the given end- and middle-points; 
	answer the	final mid-point"	| nPoint gc back fore |	gc := view graphicsContext.	back := DisplayListView backgroundColor.	fore := view colors at: modelIndex.	p1 == nil		ifTrue: ["if dragging the first point"			nPoint := p2.			[self sensor anyButtonPressed]				whileTrue: [view						updateBetween: nPoint						and: p3						with: back						on: gc.					nPoint := Sensor cursorPoint - view class inset.					nPoint y: view vRange - nPoint y.					nPoint						x: (nPoint x min: p3 x - 1).					view						updateBetween: nPoint						and: p3						with: fore						on: gc].			^ 0 @ nPoint y].	p3 == nil		ifTrue: ["if dragging the last point"			nPoint := p2.			[self sensor anyButtonPressed]				whileTrue: [view						updateBetween: p1						and: nPoint						with: back						on: gc.					nPoint := Sensor cursorPoint - view class inset.					nPoint y: view vRange - nPoint y.					nPoint						x: (nPoint x max: p1 x + 1).					view						updateBetween: p1						and: nPoint						with: fore						on: gc].			^ view hRange @ nPoint y].	"else--dragging internal point"	nPoint := p2.	[self sensor anyButtonPressed]		whileTrue: [view				updateBetween: p1				and: nPoint				with: back				on: gc.			view				updateBetween: nPoint				and: p3				with: back				on: gc.			nPoint := Sensor cursorPoint - view class inset.			nPoint y: view vRange - nPoint y.			nPoint				x: ((nPoint x min: p3 x - 1)						max: p1 x + 1).			view				updateBetween: p1				and: nPoint				with: fore				on: gc.			view				updateBetween: nPoint				and: p3				with: fore				on: gc].	^ nPoint! !!FunctionController methodsFor: 'private'!selectModel: index
	"Set the model index of the receiver to the argument."

	modelIndex := index! !!FunctionController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 11:39'!addFunction	"Add a new function to the view's model array."	| class new nameArray |	(class := (PopUpMenu labels: 'LinSeg\ExpSeg\SplineSeg' withCRs) startUp) = 0		ifTrue: [^ self].	nameArray := Array				with: LinearFunction				with: ExponentialFunction				with: SplineFunction.	new := (nameArray at: class)				perform: #default.	view models at: modelIndex put: new.	view invalidate! !!FunctionController methodsFor: 'menu messages'!clearFunction
	"Clear the current function to the view's model array."

	view modelAt: modelIndex put: nil.
	view invalidate! !!FunctionController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 11:31'!doInspect	"Inspect the receiver's array of models"	Sensor shiftPressed		ifTrue: [self model inspect]		ifFalse: [view models inspect]! !!FunctionController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 11:28'!enterFunction	"Add a new function to the view's model array after typing in its	breakpoints. "	| class new nameArray wgc done block points view |	(class := (PopUpMenu labels: 'LinearFunction\ExpSeg\SplineSeg' withCRs) startUp) = 0		ifTrue: [^ self].	nameArray := Array				with: LinearFunction				with: ExponentialFunction				with: SplineFunction.	Cursor crossHair		showWhile: [wgc := view graphicsContext.			done := false.			block := [| p |					self sensor waitButton.					p := Sensor cursorPoint.					done := self sensor redButtonPressed not.					self sensor waitNoButton.					ScheduledControllers checkForEvents.					p].			wgc flush.			points := Array new writeStream.			[done]				whileFalse: [points nextPut: block value]].	points := points contents				collect: [:pt | (pt x / view hRange @ (view vRange - pt x / view vRange)) float].	points first x: 0.	points last x: 1.	new := (nameArray at: class)				from: points.	view models at: modelIndex put: new.	view invalidate! !!FunctionController methodsFor: 'menu messages'!redraw
	"Redraw the view and clear the selection."

	view invalidate! !!FunctionController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 11:09'!setAScale	"comment stating purpose of message"	| num view |	num := Number				readFromString: (FillInTheBlank request: 'set scale:' initialAnswer: view aScale printString).	num = 0		ifTrue: [^ self].	view aScale: num.	view invalidate! !!FunctionController methodsFor: 'menu messages' stamp: 'stp 12/31/2022 07:10'!setAScale: view	"comment stating purpose of message"	| num |	num := Number				readFromString: (FillInTheBlank request: 'set scale:' initialAnswer: view aScale printString).	num = 0		ifTrue: [^ self].	view aScale: num.	view invalidate! !!FunctionController methodsFor: 'initialize-release'!initialize

	super initialize.
	modelIndex := 1.! !!FunctionController methodsFor: 'initialize-release' stamp: 'stp 12/31/2022 11:39'!initializeMenu	"Set up the receiver's YellowButtonMenu"	self		menuHolder: (ValueHolder				with: (PopUpMenu labelList: #(#('inspect') #('redraw' 'set scale') #('add' 'clear' 'enter')) values: #(#doInspect #redraw #setAScale #addFunction #clearFunction #enterFunction)))! !!FunctionController methodsFor: 'events'!mouseMovedEvent: anEvent
	"Mouse dragging"

	anEvent redButtonPressed ifFalse: [^self].
	model selection == nil
		ifTrue: [self selectEvent: anEvent]
		ifFalse: [self dragSelection: anEvent]! !DisplayListView subclass: #FunctionView	instanceVariableNames: 'modelIndex vRange hRange aScale models colors normalize lineWidth pixmap'	classVariableNames: 'LineWidth'	poolDictionaries: ''	category: 'MusicUI-Functions'!!FunctionView commentStamp: '<historical>' prior: 0!Instances of class FunctionView are views on breakpoint or sampled data functions.
They can display up to 4 model functions on a fixed-size non-scrolling field.

Instance variables:
	models	 <Array of 1 to 4 Functions> the function array
	aScale <Number> the y-scale of the view
	vRange <Number> the vertical range, normally = aScale
	hRange <Number> the horizontal range
	colors <Array of 1 to 4 ColorValues> the colors for the 4 functions
	normalize <<Array of 1 to 4 Booleans> whether to normalize (to +- 1) the functions!!FunctionView methodsFor: 'private'!setModel: aModel
	"Set the receiver's model to be aModel"

	super setModel: aModel.
	models at: 1 put: aModel! !!FunctionView methodsFor: 'private'!source unavailable! !!FunctionView methodsFor: 'accessing'!aScale
	"Answer the receiver's 'aScale'."

	^aScale! !!FunctionView methodsFor: 'accessing'!aScale: anObject
	"Set the receiver's instance variable 'aScale' to be anObject."

	aScale := anObject! !!FunctionView methodsFor: 'accessing'!colors
	"Answer the receiver's color array."

	^colors! !!FunctionView methodsFor: 'accessing'!colors: anArrayOfSize4
	"Set the receiver's color array to the argument."

	colors := anArrayOfSize4! !!FunctionView methodsFor: 'accessing'!displayExtent
	"Answer the receiver's visual extent."

	^hRange@vRange! !!FunctionView methodsFor: 'accessing'!dontNormalize: aNumber

	normalize at: aNumber put: false.! !!FunctionView methodsFor: 'accessing'!hRange
	"Answer the receiver's 'hRange'."

	^hRange! !!FunctionView methodsFor: 'accessing'!hRange: aNumber
	"Set the receiver's horizontal extent to aNumber."

	hRange := aNumber! !!FunctionView methodsFor: 'accessing'!lineWidth: aNumber
	"Set the receiver's line width to the argument."

	lineWidth := aNumber! !!FunctionView methodsFor: 'accessing'!model
	"Answer the fisrt element from receiver's array of models."

	^models at: 1! !!FunctionView methodsFor: 'accessing'!modelAt: anIndex
	"Answer the receiver's model at the given index."

	^models at: anIndex! !!FunctionView methodsFor: 'accessing'!modelAt: anIndex put: aFunction
	"Set the receiver's model at the given index to the given function."

	models at: anIndex put: aFunction.
	controller model: aFunction.
	anIndex == 1
		ifTrue: [self model: aFunction]! !!FunctionView methodsFor: 'accessing'!models
	"Answer the receiver's first model."

	^models! !!FunctionView methodsFor: 'accessing'!models: anArray
	"Set the receiver's array of models to the argument."

	models := anArray.
	self model: (anArray at: 1)! !!FunctionView methodsFor: 'accessing'!normalize: aNumber

	normalize at: aNumber put: true.! !!FunctionView methodsFor: 'accessing'!normalizeArray: arrayOfBooleans
	"Set the receiver's array of normalization switches."

	normalize := arrayOfBooleans! !!FunctionView methodsFor: 'accessing'!vRange
	"Answer the receiver's 'vRange'."

	^vRange! !!FunctionView methodsFor: 'accessing'!vRange: aNumber
	"Set the receiver's vertical extent to aNumber."

	vRange := aNumber! !!FunctionView methodsFor: 'controller access'!bounds: aRectangle
	"Set the receiver's bounds."

	super bounds: aRectangle.
	vRange := aRectangle height - 8.
	aScale := vRange.
	hRange := aRectangle width - 8.
	self setPixmap.
"	self invalidate"! !!FunctionView methodsFor: 'controller access'!defaultControllerClass

	^FunctionController! !!FunctionView methodsFor: 'displaying' stamp: 'stp 12/30/2022 11:26'!displayGridsOn: gc 	"Display the gridding for the receiver on its cached form."	0		to: hRange		by: 40		do: [:xVal | gc				line: xVal @ 0				to: xVal @ vRange				width: 1				color: Color gray].	0		to: vRange		by: 40		do: [:yVal | gc				line: 0 @ yVal				to: hRange @ yVal				width: 1				color: Color gray]! !!FunctionView methodsFor: 'displaying'!displayOn: aGraphicsContext

"	Transcript show: 'FcnView size: ', hRange printString, ' @ ', vRange printString; cr."

	cache ifFalse: [aGraphicsContext paint: backgroundColor.
				aGraphicsContext displayRectangle: aGraphicsContext clippingBounds].
	self class inset == nil
		ifFalse: [aGraphicsContext translateBy: self class inset].
	self displayOnImage.
	pixmap displayOn: aGraphicsContext.
	self class inset == nil
		ifFalse: [aGraphicsContext translateBy: self class inset negated].! !!FunctionView methodsFor: 'displaying' stamp: 'stp 1/27/2023 07:21'!displayOnCanvas: aCanvas 	"Display the receiver view on its cached form."	"FunctionView example"	| oldPt mod |	aCanvas fillColor: DisplayListView backgroundColor.	aCanvas		translateBy: inset @ inset		during: [:canv | 			self displayGridsOn: canv.			1 to: models size do: 					[ :k | (mod := models at: k) == nil						ifFalse: [ | rng rmin rmax dom dmin dmax hScale vScale | 							(mod isKindOf: SplineFunction)								ifTrue: [(mod scaleBy: self class defaultSize) spline										displayOn: aCanvas										at: inset @ inset										clippingBox: bounds										rule: Form paint										fillColor: (colors at: k)]								ifFalse: [rng := mod range.									rmin := rng first.									rmax := rng stop.									"- rmin"									rmax = rmin										ifTrue: [^ self].									(normalize at: k)										ifTrue: [vScale := aScale / (rmax - rmin)											"auto-scale"]										ifFalse: [vScale := aScale].									"no auto-scale"									dom := mod domain.									dmin := dom first.									dmax := dom stop.									"- dmin"									dmax = dmin										ifTrue: [hScale := hRange]										ifFalse: [hScale := hRange asFloat / (dmax - dmin)].									oldPt := 0 @ (vRange - ((mod at: 0 + dmin) - rmin * vScale)) truncated.									1 to: hRange do: 											[ :i | | val |											val := mod at: (i asFloat / hScale + dmin) asFloat.											val := vRange - (val - rmin * vScale) truncated.											canv line: oldPt												to: i @ val												width: lineWidth												color: (colors at: k).											oldPt := i @ val]]]]]! !!FunctionView methodsFor: 'displaying' stamp: 'stp 12/30/2022 07:56'!displayOnImage	"Display the receiver view on its cached form."	"FunctionView example"	| gc oldPt mod |	gc := pixmap.	self displayGridsOn: gc.	gc lineWidth: lineWidth.	1		to: models size		do: [:k | (mod := models at: k) == nil				ifFalse: [| rng rmin rmax dom dmin dmax hScale vScale |					gc						paint: (colors at: k).					rng := mod range.					rmin := rng first.					rmax := rng stop.					"- rmin"					rmax = rmin						ifTrue: [^ self].					(normalize at: k)						ifTrue: [vScale := aScale / (rmax - rmin)							"auto-scale"]						ifFalse: [vScale := aScale].					"no auto-scale"					dom := mod domain.					dmin := dom first.					dmax := dom stop.					"- dmin"					dmax = dmin						ifTrue: [hScale := hRange]						ifFalse: [hScale := hRange asFloat / (dmax - dmin)].					oldPt := 0 @ (vRange - ((mod at: 0 + dmin)										- rmin * vScale)) truncated.					1						to: hRange						do: [:i | 							| val |							val := mod at: (i asFloat / hScale + dmin) asFloat.							val := vRange - (val - rmin * vScale) truncated.							gc displayLineFrom: oldPt to: i @ val.							oldPt := i @ val]]]! !!FunctionView methodsFor: 'displaying' stamp: 'stp 1/21/2023 06:38'!drawOn: aCanvas	self displayOnCanvas: aCanvas! !!FunctionView methodsFor: 'displaying'!updateBetween: p1 and: p2 with: color on: gc
	"Display the receiver on its cached form."
	"FunctionView example"

	| x1 x2 y1 y2 ins |
	ins := self class inset x.
	x1 := (p1 x + ins) truncated. y1 := (vRange - p1 y + ins) truncated.
	x2 := (p2 x + ins) truncated. y2 := (vRange - p2 y + ins) truncated.
	gc paint: color.
	gc displayLineFrom: (x1@y1) to: (x2@y2).! !!FunctionView methodsFor: 'initialize-release' stamp: 'stp 12/30/2022 11:30'!initialize	"Initialize the receiver's instance variables for cacheing the pixmap	during display."	super initialize.	vRange := 400.	hRange := 800.	aScale := 400.	models := Array new: 4.	colors := Array				with: DisplayListView foregroundColor				with: Color red				with: Color cyan				with: Color green.	normalize := #(false false false false).	lineWidth := LineWidth.	inset := self class inset! !!FunctionView methodsFor: 'initialize-release'!initializeCacheSized: x by: y
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := y.
	hRange := x.
	aScale := y! !!FunctionView methodsFor: 'initialize-release'!initializeCacheing
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := 256.
	hRange := 512.
	aScale := 256! !!FunctionView methodsFor: 'initialize-release'!initializeCacheingSmall
	"Initialize the receiver's instance variables for cacheing the pixmap during display."

	self initialize.
	vRange := 192.
	hRange := 384.
	aScale := 192! !!FunctionView methodsFor: 'initialize-release' stamp: 'stp 12/30/2022 07:54'!setPixmap
	"Reset the receiver's pixmap display cache after changing its size."

	pixmap := Form extent: (hRange+1)@(vRange+1).
	pixmap fill: pixmap boundingBox		fillColor: DisplayListView backgroundColor! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!FunctionView class	instanceVariableNames: ''!!FunctionView class methodsFor: 'examples'!example
	"FunctionView example"

	FunctionView multiFunctionExample! !!FunctionView class methodsFor: 'examples'!expSegExample
	"Demonstrate an ExpSeg function with different weights."
	"FunctionView expSegExample"

	FunctionView onFunction: (ExponentialFunction from: #((0 0 3) (0.3 1 5) (0.6 0.3 0) (1 0)))! !!FunctionView class methodsFor: 'examples'!fileExample
	"Read an extracted envelope as floating-point data from a file."
	"FunctionView fileExample"

	FunctionView onFunctions: (Function fromFile: 'G1.2a1.env') normalize: #(false true true false)! !!FunctionView class methodsFor: 'examples'!linSegExample
	"FunctionView linSegExample"

	FunctionView onFunction: (LinearFunction from: #((0 0) (0.15 1.0) (0.25 0.75) (0.75 0.5) (1 0)))! !!FunctionView class methodsFor: 'examples' stamp: 'stp 1/21/2023 06:39'!multiFunctionExample	"Open a function editor on 4 different functions."	"FunctionView multiFunctionExample"		FunctionView		onFunctions: (Array				with: (LinearFunction from: #(#(0 0) #(0.12 1) #(0.17 0.74) #(0.35 0.5) #(0.95 0.2) #(1 0)))				with: (FourierSummation from: #(#(1 0.5 0) #(3 0.15 0) #(5 0.1 0) #(7 0.075 0) #(9 0.055 0) #(11 0.05 0)))				with: (ExponentialFunction from: #(#(0 0 5) #(0.02 1 -5) #(0.2 0.25 -1) #(1 0)))				with: (SplineFunction from: #(#(0 0.5) #(0.3 0.4) #(0.6 0.5) #(0.85 0.2) #(1 0.6))))		withColors: self defaultColors		normalize: #(false true false false)		x: self defaultSize x		y: self defaultSize y! !!FunctionView class methodsFor: 'examples'!smallFileExample
	"Read an extracted envelope as floating-point data from a file."
	"FunctionView smallFileExample"

	FunctionView smallOnFunction: (Function readFloatsFrom: 'snd/yy/ec/syl/y2.env')
"	FunctionView smallOnFunction: (Function readFloatsFrom: 'snd/yy/ec/syl/y2.2.env')"! !!FunctionView class methodsFor: 'examples'!smallMultiFunctionExample
	"Open a function editor on 4 different functions."
	"FunctionView smallMultiFunctionExample"

	 
	FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0 0) (0.12 1) (0.17 0.74) 
					(0.35 0.5) (0.9 0.4) (1 0)))
			with: (LinearFunction from: #((0 0.1) (0.3 0.5) (0.7 0.3) (1 0)))
			with: (ExponentialFunction from: #((0 0 5) (0.05 1 -5) (0.2 0.25 -1) (1 0)))
			with: (SplineFunction default))
		withColors: self defaultColors
		x: 384 y: 192! !!FunctionView class methodsFor: 'constant access' stamp: 'stp 1/21/2023 06:40'!defaultColors	"Answer the default color array for the 4 functions."		^ Array		with: Color blue		with: Color red		with: Color green muchDarker		with: Color gray! !!FunctionView class methodsFor: 'constant access' stamp: 'stp 12/30/2022 11:04'!defaultSize	"Answer the default extent of function views."	^ 800 @ 400! !!FunctionView class methodsFor: 'constant access' stamp: 'stp 12/30/2022 11:30'!inset	"Answer the default display inset."	^ 8! !!FunctionView class methodsFor: 'constant access'!lineWidth
	"Answer the default line width for fnuction plots."

	^LineWidth! !!FunctionView class methodsFor: 'constant access'!lineWidth: anInteger
	"Set the default line width for fnuction plots."
	"FunctionView lineWidth: 1"

	LineWidth := anInteger! !!FunctionView class methodsFor: 'class initialization'!initialize
	"Set up the defaults"
	"FunctionView initialize"
	"FunctionView lineWidth: 1"

	LineWidth := 2! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 10:34'!onFunction: function 	"FunctionView example"	"FunctionView onFunction: (Function randomOfSize: 512 from: 0.1 to:	0.9) normalize: true"	^ function range size > 1		ifTrue: [self				onFunctions: (Array						with: function						with: nil						with: nil						with: nil)				withColors: self defaultColors				normalize: #(true false false false)				x: self defaultSize x				y:  self defaultSize y]		ifFalse: [self				onFunctions: (Array						with: function						with: nil						with: nil						with: nil)				withColors: self defaultColors				normalize: #(false false false false)				x:  self defaultSize x				y:  self defaultSize y]! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 10:34'!onFunction: function normalize: norm 	"FunctionView example"	self		onFunctions: (Array				with: function				with: nil				with: nil				with: nil)		withColors: self defaultColors		normalize: (Array				with: norm				with: false				with: false				with: false)		x: self defaultSize x		y: self defaultSize y! !!FunctionView class methodsFor: 'instance creation'!onFunction: function x: x y: y
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
			x: x y: y]! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 07:37'!onFunctions0: functionArray withColors: colorArray normalize: normArray x: x y: y 	"FunctionView onFunctions: (Array 
			with: (LinearFunction from:	#((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with:	(LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with:	(LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with:	(LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors:	(Array with: ColorValue blue with: ColorValue red
				with: ColorValue cyan	with: ColorValue green)
		normalize: #(false false false false)
		x: 512 y:	256 ""	| window container me wrapper |	window := ScheduledWindow new.	window label: 'Function Editor'.	window minimumSize: (x * 1.05 + 22) truncated @ (y + 20).	window maximumSize: (x * 1.05 + 22) truncated @ (y + 20).	container := CompositePart new.	me := self new initialize.	me initializeCacheSized: x by: y.	me hRange: x;		 vRange: y;		 aScale: y;		 setPixmap.	me models: functionArray.	me colors: colorArray.	me normalizeArray: normArray.	wrapper := LookPreferences edgeDecorator on: me.	wrapper noVerticalScrollBar.	container		add: wrapper		borderedIn: (0 @ 0 extent: 1 @ 1).	window component: container.	window open"! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 1/21/2023 07:23'!onFunctions: functionArray 	"FunctionView onFunctions: 		(Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.7@0.3) (1@0.7))))"				^ self		onFunctions: functionArray		withColors: self defaultColors		normalize: #(false false false false)		x: self defaultSize x		y: self defaultSize y! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 10:35'!onFunctions: functionArray normalize: normArray 	"FunctionView onFunctions: (Array 
			with: (LinearFunction from:	#((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with:	(LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with:	(LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with:	(LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))"	^ self		onFunctions: functionArray		withColors: self defaultColors		normalize: normArray		x: self defaultSize x		y: self defaultSize y! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 1/20/2023 07:36'!onFunctions: functionArray withColors: colorArray	"FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors: (Array with: Color blue with: Color red
				with: Color cyan with: Color green)"	self onFunctions: functionArray 			withColors: colorArray 			normalize: #(false false false false) 			x: FunctionView defaultSize x			y: FunctionView defaultSize y	! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 1/20/2023 07:35'!onFunctions: functionArray withColors: colorArray normalize: normArray	"FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors: (Array with: Color blue with: Color red
				with: Color cyan with: Color green)
		normalize: #(false false false false)"	self onFunctions: functionArray 			withColors: colorArray 			normalize: normArray 			x: FunctionView defaultSize x			y: FunctionView defaultSize y	! !!FunctionView class methodsFor: 'instance creation' stamp: 'stp 12/30/2022 11:34'!onFunctions: functionArray withColors: colorArray normalize: normArray x: x y: y 	"FunctionView onFunctions: (Array 
			with: (LinearFunction from:	#((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with:	(LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with:	(LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with:	(LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		withColors:	(Array with: Color blue with: Color red
				with: Color cyan	with: Color green)
		normalize: #(false false false false)
		x: 512 y:	256"	| me ins |	me := self new initialize.	me initializeCacheSized: x by: y.	me hRange: x;		 vRange: y;		 aScale: y;		 setPixmap.	me models: functionArray.	me colors: colorArray.	me normalizeArray: normArray.	ins := self inset * 4.	DisplayListWindow		open: me		label: 'FunctionView'		extent: self defaultSize + (ins @ ins) + (0@16)		scrollBars: false! !!FunctionView class methodsFor: 'instance creation'!onFunctions: functionArray withColors: colorArray x: x y: y
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
		x: x y: y! !!FunctionView class methodsFor: 'instance creation'!onFunctions: functionArray x: x y: y
	"FunctionView onFunctions: (Array 
			with: (LinearFunction from: #((0@0) (0.15@1.0) (0.25@0.75) (0.75@0.5) (1@0))) 
			with: (LinearFunction from: #((0@0.1) (0.3@0.7) (0.7@0.45) (1@0))) 
			with: (LinearFunction from: #((0@0) (0.05@1) (0.2@0.35) (1@0))) 
			with: (LinearFunction from: #((0@0.2) (0.6@0.3) (1@0.3))))
		x: 512 y: 256"

	^self onFunctions: functionArray
		withColors: self defaultColors
		x: x y: y! !!FunctionView class methodsFor: 'instance creation'!openOn: function

	^self onFunction: function! !!FunctionView class methodsFor: 'instance creation'!smallOnFunction: function
	"FunctionView example"

	^self onFunctions: (Array with: function with: nil with: nil with: nil)
			x: 384 y: 192! !FunctionView initialize!