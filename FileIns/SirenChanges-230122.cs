'From Squeak6.1alpha of 18 January 2023 [latest update: #22377] on 23 January 2023 at 1:12 pm'!!Object methodsFor: 'converting' stamp: 'stp 1/18/2023 12:57'!asText	"Answer a text for the receiver"	^ self asString asText! !!Class methodsFor: 'accessing class hierarchy' stamp: 'stp 1/21/2023 07:46'!sortedSubclasses	"Answer the receiver's subclasses in a sorted collection by name with abstract classes first."	 | coll coll2 coll3 allClasses |	subclasses == nil		ifTrue: [^Set new].	coll := subclasses collect: [ :e | e name ].	coll := coll asSortedCollection.	allClasses := OrderedCollection new: 1024.	Smalltalk allClassesDo: [ :cls | allClasses add: cls].	coll := coll collect: [ :e | allClasses detect: [ :cls | cls name == e]].	coll2 := coll select: [ :e | e subclasses isEmpty].	coll3 := coll select: [ :e | e subclasses isEmpty not].	coll2 isEmpty ifTrue: [^coll3].	coll3 isEmpty ifTrue: [^coll2].	coll := OrderedCollection withAll: coll2.	coll addAll: coll3.	^coll! !!Debugger methodsFor: 'initialize-release' stamp: 'stp 1/22/2023 12:33'!initialExtent	"Initial extent for the full debugger. For the notifier's extent see #initialExtentForNotifier."		^ 700@900! !!Float methodsFor: 'printing' stamp: 'stp 1/18/2023 12:40'!printString: digits
	"Answer a String whose characters are a description of the receiver."
	"5.2376923461109 printString: 4 "	
	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream maxDecimalPlaces: digits.
	^aStream contents! !!Line methodsFor: 'displaying' stamp: 'stp 1/19/2023 10:21'!displayOn: aDisplayMedium at: aPoint clippingBox: clipRect rule: anInteger fillColor: aForm 	"The form associated with this Path will be displayed, according  	to one of the sixteen functions of two logical variables (rule), at  	each point on the Line. Also the source form will be first anded  	with aForm as a mask. Does not effect the state of the Path."	collectionOfPoints size < 2 ifTrue: [self error: 'a line must have two points'].	(aDisplayMedium isKindOf: DisplayMedium)		ifTrue: [aDisplayMedium				drawLine: self form				from: self beginPoint + aPoint				to: self endPoint + aPoint				clippingBox: clipRect				rule: anInteger				fillColor: aForm]		ifFalse: [aDisplayMedium 		"else it's a canvas"				line: self beginPoint + aPoint				to: self endPoint + aPoint				width: form width				color: aForm]	! !!Morph methodsFor: '*Etoys-geometry'!move: aMorph toPosition: aPointOrNumber	"Support for e-toy demo. Move the given submorph to the given position. Allows the morph's owner to determine the policy for motion. For example, moving forward through a table might mean motion only in the x-axis with wrapping modulo the table size."	aMorph position: aPointOrNumber asPoint.! !!OrderedCollection methodsFor: 'removing' stamp: 'stp 1/18/2023 15:22'!removeRandom: threshold	"Remove a given percentage of the items in the receiver. 	e.g., [aColl removeRandom: 0.25] would drop 25% of the elements."	"((1 to: 20) asOrderedCollection) removeRandom: 0.7"		| rand ind |	rand := Random new.	ind := 1.	[ind < self size] whileTrue: 		[rand next > threshold			ifFalse: [self removeIndex: ind]			ifTrue: [ind := ind + 1]]! !!SampledSound methodsFor: 'accessing' stamp: 'stp 1/21/2023 14:31'!count	^ count! !!SoundMorph methodsFor: 'accessing' stamp: 'jm 11/14/97 11:21'!buildImage	| scale env h imageColor |	owner ifNil: [scale := 128@128]  "Default is 128 pix/second, 128 pix fullscale"		ifNotNil: [scale := owner soundScale].	env := sound volumeEnvelopeScaledTo: scale.	self image: (ColorForm extent: env size @ env max).	1 to: image width do:		[:x | h := env at: x.		image fillBlack: ((x-1)@(image height-h//2) extent: 1@h)].	imageColor := #(black red orange green blue) atPin:						(sound pitch / 110.0) rounded highBit.	image colors: (Array with: Color transparent with: (Color perform: imageColor)).! !!SoundPlayer class methodsFor: 'initialization' stamp: 'stp 1/21/2023 14:23'!initialize	"SoundPlayer initialize; shutDown; startUp"	"Details: BufferMSecs represents a tradeoff between latency and quality. If BufferMSecs is too low, the sound will not play smoothly, especially during long-running primitives such as large BitBlts. If BufferMSecs is too high, there will be a long time lag between when a sound buffer is submitted to be played and when that sound is actually heard. BufferMSecs is typically in the range 50-200."	SamplingRate := 44100.	BufferMSecs := 120.	Stereo := true.	UseReverb ifNil: [UseReverb := true].! !!Spline methodsFor: 'displaying' stamp: 'stp 1/21/2023 07:28'!displayOn: aDisplayMedium at: aPoint clippingBox: clipRect rule: anInteger fillColor: aForm 	"Display the receiver, a spline curve, approximated by straight line	segments."	| n line t x y x1 x2 x3 y1 y2 y3 |	collectionOfPoints size < 1 ifTrue: [self error: 'a spline must have at least one point'].	line := Line new.	line form: ((Form extent: 2 @ 2) fillBlack).	line beginPoint: 		(x := (coefficients at: 1) at: 1) rounded @ (y := (coefficients at: 5) at: 1) rounded.	1 to: (coefficients at: 1) size - 1 do: 		[:i | 		"taylor series coefficients"		x1 := (coefficients at: 2) at: i.		y1 := (coefficients at: 6) at: i.		x2 := ((coefficients at: 3) at: i) / 2.0.		y2 := ((coefficients at: 7) at: i) / 2.0.		x3 := ((coefficients at: 4) at: i) / 6.0.		y3 := ((coefficients at: 8) at: i) / 6.0.		"guess n"		n := 5 max: (x2 abs + y2 abs * 2.0 + ((coefficients at: 3)							at: i + 1) abs + ((coefficients at: 7)							at: i + 1) abs / 100.0) rounded.		1 to: n - 1 do: 			[:j | 			t := j asFloat / n.			line endPoint: 				(x3 * t + x2 * t + x1 * t + x) rounded 							@ (y3 * t + y2 * t + y1 * t + y) rounded.			line				displayOn: aDisplayMedium				at: aPoint				clippingBox: clipRect				rule: anInteger				fillColor: aForm.			line beginPoint: line endPoint].		line beginPoint: 				(x := (coefficients at: 1) at: i + 1) rounded 					@ (y := (coefficients at: 5) at: i + 1) rounded.		line			displayOn: aDisplayMedium			at: aPoint			clippingBox: clipRect			rule: anInteger			fillColor: aForm]! !!TextEditor methodsFor: 'menu messages' stamp: 'stp 1/19/2023 07:47'!browseClassFromIt	"Launch a hierarchy browser for the class indicated by the current selection.  	If multiple classes matching the selection exist, let the user choose among them.	If there's exactly 1 space in the selection, see if it's a class/selector pair"		| str aClass |	self lineSelectAndEmptyCheck: [ ^ self ].	str := self selection string withBlanksTrimmed.	((str count: [ :c | c == $ ]) = 1)		ifTrue: [ | ind cls sel | 					"parse expr like [DynamicCloud focusExample]"			ind := str indexOf: $ .			cls := (str copyFrom: 1 to: ind -1) asSymbol.			sel := (str copyFrom: ind + 1 to: str size) asSymbol.			((Smalltalk includesKey: cls) and: [(Smalltalk classNamed: cls) class includesSelector: sel])				ifTrue: [^ self systemNavigation								spawnHierarchyForClass: (Smalltalk classNamed: cls) class								selector: sel asSymbol]].	aClass := UIManager default		classFromPattern: str		withCaption: 'choose a class to browse...'		in: model environment.	aClass ifNil: [ ^ morph flash ].	self systemNavigation		spawnHierarchyForClass: aClass		selector: nil! !!TheWorldMenu methodsFor: 'construction' stamp: 'stp 1/17/2023 17:14'!buildWorldMenu	"Build the menu that is put up when the screen-desktop is clicked on"	| menu |	menu := MenuMorph new defaultTarget: self.	menu commandKeyHandler: self.	self colorForDebugging: menu.	menu addStayUpItem.	self fillIn: menu		from: {{'Restore display'. { Project current. #restoreDisplay }. 'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.' }.				{ 'Save image'. { Smalltalk. #saveSession }. 'save the current version of the image on disk' } . nil}.					"Add the instant-open windows - in-line or as a submenu"	(self class convenientWorldMenu)				"system pref as class var"		ifTrue: [self makeConvenient: menu.		"conv menu in-line"				menu addLine]		ifFalse: [menu addItem: [ :item | 			"conv sub-menu"							item contents: 'Tools'; subMenuUpdater: self selector: #makeConvenient: ]].					"Add the utilities"	menu addItem: [ :item | item contents: 'Open'; subMenuUpdater: self selector: #openMenu: ].	menu addItem: [ :item | item contents: 'Windows'; subMenuUpdater: self selector: #windowsMenuOn: ].	menu addItem: [ :item | item contents: 'Changes'; subMenuUpdater: self selector: #changesMenuOn: ].	Smalltalk at: #ServiceGUI ifPresent:[:sgui|		sgui worldMenu: menu.		sgui onlyServices ifTrue: [^ menu].	].	menu addItem: [ :item | item contents: 'Do...'; subMenuUpdater: self selector: #doMenuOn: ].	menu addItem: [ :item | item contents: 'Projects'; subMenuUpdater: self selector: #projectMenuOn: ].	menu addLine.	menu addItem: [ :item | item contents: 'Authoring'; subMenuUpdater: self selector: #authoringMenuOn: ].	menu addItem: [ :item | item contents: 'Misc'; subMenuUpdater: self selector: #addMiscOn: ].		menu addItem: [ :item | item contents: 'Save/Quit'; subMenuUpdater: self selector: #addSaveAndQuit: ].	^ menu! !!WaveEditor methodsFor: 'menu' stamp: 'stp 1/21/2023 14:23'!play	graph data size < 2 ifTrue: [^ self].	(SampledSound samples: graph data samplingRate: samplingRate) play.! !