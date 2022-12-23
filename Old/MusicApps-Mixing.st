<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 8, 2007 at 4:44:17 pm</time-stamp>


<class>
<name>MixSliderPanel</name>
<environment>Siren</environment>
<super>UI.ApplicationModel</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>colorArray mainView maxVal grid </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	"Initialize the instance variables of the receiver."

	super initialize.
	maxVal := 2.0.
	self model: mainView model.
	self scaleY: mainView scaleY.	"pixels/strip"
	grid := 84@600.
	self initForm</body>
</methods>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>accessing</category>

<body package="Siren" selector="colorArray">colorArray
	"Answer the receiver's 'colorArray'."

	^colorArray</body>

<body package="Siren" selector="colorArray:">colorArray: anObject
	"Set the receiver's 'colorArray' to be anObject."

	colorArray := anObject</body>

<body package="Siren" selector="mainView">mainView
	"Answer the receiver's 'mainView.'"

	^mainView</body>

<body package="Siren" selector="mainView:">mainView: anObject
	"Set the receiver's 'mainView' to be anObject."

	mainView := anObject</body>

<body package="Siren" selector="maxVal">maxVal
	"Answer the receiver's 'maxVal'."

	^maxVal</body>

<body package="Siren" selector="width">width
	"Answer the 'width' of the receiver's slider."

	^grid x</body>
</methods>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>controller access</category>

<body package="Siren" selector="defaultControllerClass">defaultControllerClass
	"Answer the class of the default controller for the receiver."

	^MixSliderController</body>
</methods>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>displaying</category>

<body package="Siren" selector="clearAmplOf:">clearAmplOf: line
	"Display the amplitude slider for the given event element."

	| top aGC x |
	top := line - 1 * scale y + 1.
	aGC := self graphicsContext.
	aGC paint: self backgroundColor.
	aGC displayRectangle: ((0@top) extent: (self width @ (scale y / 2 - 1))).
	x := grid x / 2.
	aGC paint: self clefColor.
	aGC displayLineFrom:(x @ top) to: (x @ (top * (scale y / 2 - 1))).
	aGC paint: self foregroundColor.</body>

<body package="Siren" selector="clearPosOf:">clearPosOf: line
	"Display the position slider for the given event element."

	| top aGC x |
	top := line - 1 * scale y + (scale y / 2) + 1.
	aGC := self graphicsContext.
	aGC paint: self backgroundColor.
	aGC displayRectangle: ((0@top) extent: (self width @ (scale y / 2 - 1))).
	x := grid x / 2.
	aGC paint: self clefColor.
	aGC displayLineFrom:(x @ top) to: (x @ (top * (scale y / 2 - 1))).
	aGC paint: self foregroundColor.
	aGC paint: self foregroundColor.</body>

<body package="Siren" selector="displayAmplOf:value:on:">displayAmplOf: line value: value on: aGC
	"Display the amplitude slider for the given event element."

	| width top |
	width := (self width * value / maxVal) truncated.
	top := line - 1 * scale y + 3.
	aGC paint: self clefColor.
	aGC displayRectangle: ((0@top) extent: (width @ (scale y / 2 - 5))).
	aGC paint: self foregroundColor</body>

<body package="Siren" selector="displayOn:">displayOn: pGC
	"Display the receiver on its cached form."

	| events stereo evt |
	self container setOrigin: (0@(self container translation y)).	"KLUDJ!"
	background displayOn: pGC.
	pGC paint: self foregroundColor.
	events := model eventList events.
	stereo := model eventList channels = 2.
	1 to: events size do:
		[ :i |
		evt := (events at: i) event.
		stereo
			ifTrue: [self displayPosOf: i value: evt position on: pGC].
		evt sound channels = 1
			ifTrue: [self displayAmplOf: i value: evt loudness value on: pGC]
			ifFalse: [self displayStereoAmplOf: i 
							value1: evt loudness value value2: (evt at: #rightL) on: pGC]]</body>

<body package="Siren" selector="displayPosOf:value:on:">displayPosOf: line value: value on: aGC
	"Display the position slider for the given event element."

	| x top |
	x := (self width * value - 1) truncated.
	top := line - 1 * scale y + (scale y / 2) + 2.
	aGC displayRectangle: ((x@top) extent: (3 @ (scale y / 2 - 3)))</body>

<body package="Siren" selector="displayStereoAmplOf:value1:value2:on:">displayStereoAmplOf: line value1: val1 value2: val2 on: aGC
	"Display the amplitude slider for the given event element."

	| width top |
	width := (self width * val1 / maxVal) truncated.
	top := line - 1 * scale y.
	aGC displayRectangle: ((0@top) extent: (width @ (scale y / 4))).
	width := (self width * val2 / maxVal) truncated.
	top := line - 1 * scale y + (scale y / 2).
	aGC displayRectangle: ((0@top) extent: (width @ (scale y / 4))).</body>
</methods>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>updating</category>

<body package="Siren" selector="update:">update: aspectSymbol
	"Some aspect of the model has changed.
	By default, repaint the receiver."

	aspectSymbol == #text
		ifTrue: [^self].
	aspectSymbol == #selection
		ifTrue: [^self].
	self invalidate</body>
</methods>

<methods>
<class-id>Siren.MixSliderPanel</class-id> <category>private</category>

<body package="Siren" selector="drawBackgroundGrid">drawBackgroundGrid
	"Display the gridding of the receiver."

	| box pt1 pt2 lineBlock aGC x |
	aGC := background graphicsContext.
	aGC paint: self backgroundColor.
	aGC displayRectangle: background bounds.
	aGC paint: self clefColor.
	box := 0@0 extent: grid.
	pt1 := box left @ 0.
	pt2 := box right @ 0.
	lineBlock :=
			[ :y | 
			pt1 y: y.
			pt2 y: y.
			aGC displayLineFrom: pt1 to: pt2].
	0 to: box height by: (scale y / 2) do:
		[ :i | lineBlock value: i].
	x := grid x / 2.
	aGC displayLineFrom:(x @ 0) to: (x @ grid y)</body>

<body package="Siren" selector="initForm">initForm
	"Draw the panel's default grid background."

	background := Form extent: grid.
	background background: DisplayListView backgroundColor.
	self scaleY == nil
		ifFalse: [self drawBackgroundGrid]</body>
</methods>


<class>
<name>MixLayoutManager</name>
<environment>Siren</environment>
<super>Siren.LayoutManager</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<methods>
<class-id>Siren.MixLayoutManager</class-id> <category>generating</category>

<body package="Siren" selector="displayPointFor:">displayPointFor: anAss
	"Answer the display point for the argument."

	^((anAss key asSec value - timeOffset * timeScale) asInteger)
		@ (((view model eventList events indexOf: anAss) - 1 * pitchScale) + 3)</body>
</methods>


<class>
<name>MixController</name>
<environment>Siren</environment>
<super>UI.ControllerWithMenu</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<methods>
<class-id>Siren.MixController</class-id> <category>initialize-release</category>

<body package="Siren" selector="initializeMenu">initializeMenu
	"Set up the receiver's YellowButtonMenu"

	self menuHolder: (ValueHolder with: (PopUpMenu
			labelList: #((redraw sort) (add cut copy paste))
			values: #(redraw sort add cut copy paste)))</body>
</methods>

<methods>
<class-id>Siren.MixController</class-id> <category>control defaults</category>

<body package="Siren" selector="controlActivity">controlActivity
	"Handle keyboard events."

	self sensor keyboardPressed &amp; self viewHasCursor 
		ifTrue: [^self processKeyboardEvent: sensor keyboard"Event"].
	super controlActivity</body>

<body package="Siren" selector="processKeyboardEvent:">processKeyboardEvent: anEvent
	"Respond to keyboard events."

	| char shift|
	char := anEvent "keyValue".
	char == Character space			"&lt;SPACE&gt;  --&gt;  play"
		ifTrue: [^model play].
	shift := sensor shiftDown.
	(char == #Left or: [char asLowercase == $h])
		ifTrue: [shift
			ifTrue: [^self moveEvent: -0.1]
			ifFalse: [^self moveEvent: -0.01]].
	(char == #Right or: [char asLowercase == $l])
		ifTrue: [shift
			ifTrue: [^self moveEvent: 0.1]
			ifFalse: [^self moveEvent: 0.01]].
	(char == #Up or: [char asLowercase == $k])
		ifTrue: [selection == nil
			ifFalse: [selection = 0 ifTrue: [^self].
				selection := selection - 1
				"do the rest here"]].
	(char == #Down or: [char asLowercase == $j])
		ifTrue: [selection == nil
			ifFalse: [selection = model eventList size ifTrue: [^self].
				selection := selection + 1
				"do the rest here"]].
	self halt</body>

<body package="Siren" selector="redButtonActivity">redButtonActivity
	"Hit-detect items from the display list or drag a selection."

	| pt line |
	model == nil ifTrue: [^nil].
	sensor shiftDown
		ifTrue: [^self dragMarker].
	pt := sensor cursorPoint.
	line := (pt y / view scaleY) truncated + 1.
"	Transcript show: 'line ', line printString; cr."
	line &gt; model eventList events size
		ifTrue: [sensor waitNoButton.
			(model selection == nil) ifFalse: [view deselect].
			model selectItem: nil.
			selection := nil.
			^view invalidate].
	((model selection == nil) or: [(model eventList events at: line) ~~ model selection])
		ifTrue: [sensor waitNoButton.
			(model selection == nil) ifFalse: [view deselect].
			model selectItem: (model eventList events at: line).
			selection := line.
			view invalidate]
		ifFalse: [sensor ctrlDown
			ifTrue: [^self dragCorner: pt]
			ifFalse: [self dragItem: pt]]</body>
</methods>

<methods>
<class-id>Siren.MixController</class-id> <category>selecting</category>

<body package="Siren" selector="dragCorner:">dragCorner: point
	"Drag an item's corner (fade-in or fade-out) in the receiver's view"

	| p1 p2 line ass element gc field start stop scaleX scaleY top bottom dur |
	scaleY := view scaleY.
	scaleX := view scaleX.
	line := (point y / scaleY) truncated + 1.
	ass := (model eventList events at: line).
	element := ass value.
	dur := element duration asSec value.
	start := ass start value + (dur * element fadeIn).
	stop := ass start value + dur - (dur * element fadeIn).
	((point x - (start * scaleX)) abs) &lt; ((point x - (stop * scaleX)) abs)
		ifTrue: [field := #fadeIn:]
		ifFalse: [field := #fadeOut:].
	gc := view graphicsContext.
	top := line -1 * scaleY - 4.
	bottom := line -1 * scaleY + 4.
	p1 := 0@0.
	[sensor anyButtonPressed]
		whileTrue:
			[p2 := sensor cursorPoint.
			p2 ~= p1
				ifTrue: [gc displayLineFrom: (p2 x@top) to: (p2 x@bottom)].
			p1 := p2].
	(field == #fadeIn:)
		ifTrue: [stop := ((p1 x / scaleX - ass start value / dur) max: 0) min: (1 - element fadeOut)]
		ifFalse: [stop := ((ass start value + dur - (p1 x / scaleX) / dur) max: 0) min: (1 - element fadeIn)].
"	Transcript show: field, ' = ', stop printString; cr."
	element perform: field with: stop.
	model changed: #text.
	model changed: #contents</body>

<body package="Siren" selector="dragItem:">dragItem: point
	"Drag an item in the receiver's view"

	| p1 p2 line item element gc back color x0 x1 ix time border |
	line := (point y / view scaleY) truncated + 1.
	element := model eventList events at: line.
	item := (view list nodeFor: element) copy.
	item == nil ifTrue: [self error: 'no item'].
	gc := view graphicsContext.
	back := DisplayListView backgroundColor.
	color := item color.
	border := item border.
	x0 := point x.
	ix := item offset x.
	p1 := sensor cursorPoint.
	[sensor anyButtonPressed]
		whileTrue:
			[p2 := sensor cursorPoint.
			x1 := p2 x - x0.
			p2 ~= p1
				ifTrue: [item color: back.
				item border: nil.
				item displayOn: gc.
				item color: color.
				item offset x: ((ix + x1) max: 0).
				item border: border.
				item displayOn: gc.
				time :=  (element key value  +  ((p2 x - p1 x / view scaleX) asFloat)) max: 0.
				element start: (Duration value: time).
				view textView newText: model text.
				view textView invalidateRectangle: view textView bounds repairNow: true].
			p1 := p2].
	item offset: (view layoutManager displayPointFor: element).
	model eventList recomputeDuration.
	model changed: #text</body>

<body package="Siren" selector="dragMarker">dragMarker
	"Drag the start/stop markers around."

	| pt field xS x |
	model == nil ifTrue: [^nil].
	pt := sensor cursorPoint.
	xS := view scaleX.
	x := pt x / xS.
	(((x - model eventList start) abs) &gt; ((x - model eventList stop) abs))
		ifTrue: [field := #stop:]
		ifFalse: [field := #start:].
	sensor waitNoButton.
	pt := sensor cursorPoint.
	model eventList perform: field with: ((pt x / xS) max: 0) asFloat.
	view invalidate</body>

<body package="Siren" selector="moveEvent:">moveEvent: delta
	"Move the current selection by the given delta (in sec)."

	| element item start |
	model selection == nil ifTrue: [^self].
	element := model selection.
	item := view list nodeFor: element.
	item == nil ifTrue: [self error: 'no item'].
	start := (element start asSec value + delta) max: 0.
	element start: start asFloat.
	view erase: element.
	item offset: (view layoutManager displayPointFor: element).
	model eventList recomputeDuration.
	model changed: #text.
	view draw: item</body>
</methods>

<methods>
<class-id>Siren.MixController</class-id> <category>menu messages</category>

<body package="Siren" selector="add">add
	"Add a new mix element to the mix."

	model add</body>

<body package="Siren" selector="copy">copy
	"Copy the current mix element."

	model copy</body>

<body package="Siren" selector="cut">cut
	"Cut the current mix element from the mix."

	model cut</body>

<body package="Siren" selector="editSelection">editSelection
	"Open a sound view on the selection's sound."

	selection == nil
		ifFalse: [SoundView openOn: selection event sound]</body>

<body package="Siren" selector="paste">paste
	"Paste in the currently-selected mix element."

	model paste</body>

<body package="Siren" selector="sort">sort
	"Sort the receiver's mix elements."

	model sort</body>
</methods>


<class>
<name>MixElement</name>
<environment>Siren</environment>
<super>Siren.MusicEvent</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MixElement class</class-id> <category>instance creation</category>

<body package="Siren" selector="dur:ampl:">dur: aD ampl: anA
	"Answer a MixElement instance initialized with the arguments."

	^self new duration: aD;
			loudness: anA</body>

<body package="Siren" selector="dur:ampl:name:">dur: aD ampl: anA name: aName
	"Answer a MixElement instance initialized with the arguments."

	^self new duration: aD;
			at: #name put: aName; 
			loudness: anA</body>

<body package="Siren" selector="dur:ampl:name:fadeIn:fadeOut:">dur: aD ampl: anA name: aName fadeIn: in fadeOut: out
	"Answer a MixElement instance initialized with the arguments."
	"MixElement dur: 1.5 ampl: 0.8 name: 'default' voice: nil"

	^self new duration: aD;
			at: #name put: aName; 
			loudness: anA;
			fadeIn: in;
			fadeOut: out</body>

<body package="Siren" selector="dur:ampl:name:voice:">dur: aD ampl: anA name: aName voice: aVoice
	"Answer a MixElement instance initialized with the arguments."
	"MixElement dur: 1.5 ampl: 0.8 name: 'default' voice: nil"

	^self new duration: aD;
			at: #name put: aName; 
			loudness: anA;
			voice: aVoice</body>

<body package="Siren" selector="new">new
	"Answer an initialized instance of the receiver."

	^super new initialize</body>

<body package="Siren" selector="sound:ampl:fadeIn:fadeOut:pos:">sound: snd ampl: anA fadeIn: in fadeOut: out pos: pos 
	"Answer a MixElement instance initialized with the arguments."

	^self new duration: snd duration;
			at: #name put: snd name; 
			loudness: anA;
			fadeIn: in;
			fadeOut: out;
			position: pos;
			sound: snd</body>

<body package="Siren" selector="sound:ampl:name:fadeIn:fadeOut:pos:">sound: snd ampl: anA name: aName fadeIn: in fadeOut: out pos: pos 
	"Answer a MixElement instance initialized with the arguments."

	^self new duration: snd duration;
			at: #name put: aName; 
			loudness: anA;
			fadeIn: in;
			fadeOut: out;
			position: pos;
			sound: snd</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MixElement</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	"Set up the default state of the receiver."

	super initialize.
	self at: #on put: true.
	self at: #solo put: false</body>
</methods>

<methods>
<class-id>Siren.MixElement</class-id> <category>accessing</category>

<body package="Siren" selector="amplitudeAt:">amplitudeAt: aTime
	"Answer the relative amplitude at the given time (a float), taking fade-in/out into account."

	| d in out |
	(aTime &lt; 0) ifTrue: [^0].
	d := self duration asSec value.
	(aTime &gt; d) ifTrue: [^0].
	in := d * self fadeIn.
	out := d * self fadeOut.
	(aTime &lt; in)
		ifTrue: [^self loudness value * (aTime / in)].
	((aTime &gt;= in) and: [aTime &lt;= (d - out)])
		ifTrue: [^self loudness value].
	^self loudness value * ((d - aTime) / (out))</body>

<body package="Siren" selector="envelope">envelope
	"Answer the receiver's 'envelope'."

	^properties at: #envelope ifAbsent: [nil]</body>

<body package="Siren" selector="envelope:">envelope: anObject
	"Set the receiver's property 'envelope' to be anObject."

	properties at: #envelope put: anObject</body>

<body package="Siren" selector="fadeIn">fadeIn
	"Answer the receiver's 'fadeIn'."

	^properties at: #fadeIn ifAbsent: [0.0]</body>

<body package="Siren" selector="fadeIn:">fadeIn: aNumber
	"Set the receiver's property 'fadeIn' to be aNumber."

	properties at: #fadeIn put: aNumber asFloat</body>

<body package="Siren" selector="fadeOut">fadeOut
	"Answer the receiver's 'fadeOut'."

	^properties at: #fadeOut ifAbsent: [0.0]</body>

<body package="Siren" selector="fadeOut:">fadeOut: aNumber
	"Set the receiver's property 'fadeOut' to be aNumber."

	properties at: #fadeOut put: aNumber asFloat</body>

<body package="Siren" selector="name">name
	"Answer the receiver's 'name'."

	^properties at: #name ifAbsent: [nil]</body>

<body package="Siren" selector="name:">name: anObject
	"Set the receiver's property 'name' to be anObject."

	properties at: #name put: anObject</body>

<body package="Siren" selector="on">on
	"Answer the receiver's 'on'."

	^properties at: #on ifAbsent: [0.0]</body>

<body package="Siren" selector="on:">on: anObject
	"Set the receiver's property 'on' to be anObject."

	properties at: #on put: anObject</body>

<body package="Siren" selector="pos">pos
	"Answer the receiver's property 'pos'."

	^properties at: #position ifAbsent: [nil]</body>

<body package="Siren" selector="pos:">pos: anObject
	"Set the receiver's property 'pos' to be anObject."

	properties at: #position put: anObject</body>

<body package="Siren" selector="position">position
	"Answer the receiver's property 'pos'."

	^self pos</body>

<body package="Siren" selector="position:">position: anObject
	"Set the receiver's property 'pos' to be anObject."

	self pos: anObject</body>

<body package="Siren" selector="solo">solo
	"Answer the receiver's 'solo'."

	^properties at: #solo ifAbsent: [0.0]</body>

<body package="Siren" selector="solo:">solo: anObject
	"Set the receiver's property 'solo' to be anObject."

	properties at: #solo put: anObject</body>

<body package="Siren" selector="sound">sound
	"Answer the receiver's 'sound'."

	^properties at: #sound ifAbsent: [nil]</body>

<body package="Siren" selector="sound:">sound: anObject
	"Set the receiver's property 'sound' to be anObject."

	properties at: #sound put: anObject</body>
</methods>

<methods>
<class-id>Siren.MixElement</class-id> <category>printing</category>

<body package="Siren" selector="printTerseOn:">printTerseOn: aStream 
	"Format and print the receiver on the argument as tersely as possible."

	aStream nextPut: $(.
	duration == nil
		ifFalse: [aStream nextPutAll: duration printString]. 
	loudness == nil
		ifFalse: [aStream nextPutAll: ' (l: ', loudness value printString, ')'].
	self fadeIn == nil
		ifFalse: [aStream nextPutAll: ' (fI: ', self fadeIn printString, ')'].
	self fadeOut == nil
		ifFalse: [aStream nextPutAll: ' (fO: ', self fadeOut printString, ')'].
	self position == nil
		ifFalse: [aStream nextPutAll: ' (p: ', self position printString, ')'].
	self sound == nil
		ifFalse: [aStream nextPutAll: ' (s: ', self sound name printString, ' ', self sound channels printString, ' ch)'].
	aStream nextPut: $); cr</body>

<body package="Siren" selector="storeOn:">storeOn: aStream 
	"Format and store the source the receiver on the argument."

	aStream nextPutAll: (properties == nil
			ifTrue: ['(', self class name, '']
			ifFalse: ['((', self class name, '']).
	duration == nil
		ifFalse: [aStream nextPutAll: ' dur: ', duration value storeString]. 
	loudness == nil
		ifFalse: [aStream nextPutAll: ' ampl: ', loudness value storeString].
	properties == nil
		ifFalse: [aStream nextPutAll: ')'; cr.
			properties associationsDo: [ :ass |
				ass key == #sound
					ifFalse: [aStream nextPutAll: '	at: '.
						ass key storeOn: aStream.
						aStream nextPutAll: ' 	put: '.
						ass value storeOn: aStream.
						aStream nextPut: $;; cr.]]].
	self sound == nil
		ifFalse: [aStream nextPutAll: '	at: #sound 	put: (SampledSound fromFile: ',
				self sound name printString,')'].
	aStream nextPutAll: ')'</body>
</methods>


<class>
<name>MixButtonController</name>
<environment>Siren</environment>
<super>Siren.MixController</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<methods>
<class-id>Siren.MixButtonController</class-id> <category>control defaults</category>

<body package="Siren" selector="redButtonActivity">redButtonActivity
	"Hit-detect items from the display list or drag a selection."

	| pt line field |
	model == nil ifTrue: [^nil].
	pt := sensor cursorPoint.
	line := (pt y / view mainView scaleY) truncated + 1.
	line &gt; model eventList events size
		ifTrue: [^self].
	field := view buttonForX: pt x.
	field == nil
		ifTrue: [^self].
	field == #name
		ifTrue: [model addSelection: (model eventList events at: line). 
				view invalidate.
				^sensor waitNoButton].
	model eventList toggle: field forEvent: line.
	view invalidate.
	sensor waitNoButton</body>
</methods>


<class>
<name>MixButtonPanel</name>
<environment>Siren</environment>
<super>UI.ApplicationModel</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>buttonOffsets buttonFields mainView </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	"Initialize the instance variables of the receiver."

	super initialize.
	self model: mainView model.
	self scaleY: mainView scaleY.	"pixels/strip"
	buttonOffsets := #(0 8 96 104).
	buttonFields := #(on name solo). 
	self initForm</body>
</methods>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>accessing</category>

<body package="Siren" selector="buttonForX:">buttonForX: x
	"Answer the symbolic label for the give x in the receiver's 'buttonOffsets'."

	1 to: (buttonOffsets size) do:
		[ :cnt |
		(x &lt;= (buttonOffsets at: cnt))
			ifTrue: [^buttonFields at: cnt - 1]].
	^nil</body>

<body package="Siren" selector="buttonOffsets">buttonOffsets
	"Answer the receiver's 'buttonOffsets'."

	^buttonOffsets</body>

<body package="Siren" selector="buttonOffsets:">buttonOffsets: anObject
	"Set the receiver's 'buttonOffsets' to be anObject."

	buttonOffsets := anObject</body>

<body package="Siren" selector="mainView">mainView
	"Answer the receiver's 'mainView'."

	^mainView</body>

<body package="Siren" selector="mainView:">mainView: anObject
	"Set the receiver's 'mainView' to be anObject."

	mainView := anObject</body>
</methods>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>controller access</category>

<body package="Siren" selector="defaultControllerClass">defaultControllerClass
	"Answer the class of the default controller for the receiver."

	^MixButtonController</body>
</methods>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>displaying</category>

<body package="Siren" selector="displayOn:">displayOn: pGC
	"Display the receiver on its cached form."

	self container setOrigin: (0@(self container translation y)).	"KLUDJ!"
	background displayOn: pGC.
	pGC paint: self foregroundColor.
	self displaySwitchesOn: pGC</body>

<body package="Siren" selector="displaySwitch:on:">displaySwitch: point on: aGC
	"Display the given switch as 'on.'"

	| bWidth bYInset x y |
	bWidth := 3.
	bYInset := 7.
	x := point x.
	y := point y.
	aGC displayRectangle: ((((buttonOffsets at: x)+3) @(y - 1 * scale y + bYInset)) 
					extent: (bWidth@(scale y - (bYInset * 2) + 1)))</body>

<body package="Siren" selector="displaySwitchesOn:">displaySwitchesOn: aGC
	"Update the panel with the switch values."

	| events evt text |
	events := model eventList events.
	1 to: events size do:
		[ :i |
		evt := (events at: i) event.
		((evt at: #on) == true)
			ifTrue: [self displaySwitch: 1@i on: aGC].
		((evt at: #solo) == true)
			ifTrue: [self displaySwitch: 3@i on: aGC].
		text := evt name asFilename tail.
		text := (text clipTo: 13) asText.
		(model selections includes: (events at: i))
			ifTrue: [text allBold].
		aGC display: text at: (12@(i - 1 * scale y + 16))]</body>
</methods>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>updating</category>

<body package="Siren" selector="update:">update: aspectSymbol
	"Some aspect of the model has changed.
	By default, repaint the receiver."

	aspectSymbol == #text
		ifTrue: [^self].
	aspectSymbol == #selection
		ifTrue: [^self].
	self invalidate</body>
</methods>

<methods>
<class-id>Siren.MixButtonPanel</class-id> <category>private</category>

<body package="Siren" selector="drawBackgroundGrid">drawBackgroundGrid
	"Display the gridding of the receiver."

	| box pt1 pt2 lineBlock aGC |
	aGC := background graphicsContext.
	aGC paint: self backgroundColor.
	aGC displayRectangle: background bounds.
	aGC paint: self clefColor.
	box := aGC clippingBounds.
	pt1 := 0 @ box top.
	pt2 := 0 @ box bottom.
	lineBlock := [ :x | 
			pt1 x: x.
			pt2 x: x.
			aGC displayLineFrom: pt1 to: pt2].
	buttonOffsets do:	 [ :i | lineBlock value: i].
	lineBlock value: box width.
	pt1 := box left @ 0.
	pt2 := box right @ 0.
	lineBlock := [ :y | 
			pt1 y: y.
			pt2 y: y.
			aGC displayLineFrom: pt1 to: pt2].
	0 to: box height by: scale y do: [ :i | lineBlock value: i]</body>

<body package="Siren" selector="initForm">initForm
	"Draw the panel's default grid background."

	| width |
	width := 105.
	background := Form extent: width@600.
	background background: DisplayListView backgroundColor.
	scale y == nil
		ifFalse: [self drawBackgroundGrid]</body>
</methods>


<class>
<name>Mix</name>
<environment>Siren</environment>
<super>Siren.EventList</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars>output clipped rate channels </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.Mix class</class-id> <category>instance creation</category>

<body package="Siren" selector="fromFile:">fromFile: aName
	"Read a mix sequence from the given file and answer it."

	| stream me |
	stream := aName asFilename readStream.
	me := Compiler evaluate: stream contentsOfEntireFile.
	stream close.
	^me</body>

<body package="Siren" selector="newStereo">newStereo
	"Answer a new instance of the receiver that is set to 2 channels."

	| inst |
	inst := self new.
	inst channels: 2.
	^inst</body>
</methods>

<methods>
<class-id>Siren.Mix class</class-id> <category>examples</category>

<body package="Siren" selector="randomExample">randomExample
	"Create an event list with random data."
	"Mix randomExample"
	"MixView randomExample"

	| rand size arrArray sound |
	rand := Random new.
	size := 16.
	arrArray := Array new: 10.
	sound := SampledSound sawtooth.
	1 to: 10 do: [ :i | arrArray at: i put: (Array new: size)].
	1 to: size do: [ :i | (arrArray at: 1) at: i put: (rand next * 8.0)].
	1 to: size do: [ :i | (arrArray at: 2) at: i put: (rand nextFrom: 1.0 to: 4.0)].
	1 to: size do: [ :i | (arrArray at: 3) at: i put: (rand nextFrom: 0.05 to: 1.0)].
	1 to: size do: [ :i | (arrArray at: 4) at: i put: (rand nextIntFrom: 1 to: 7)].
	1 to: size do: [ :i | (arrArray at: 5) at: i put: (rand nextFrom: 0.1 to: 0.3)].
	1 to: size do: [ :i | (arrArray at: 6) at: i put: (rand nextFrom: 0.1 to: 0.4)].
	1 to: size do: [ :i | (arrArray at: 7) at: i put: 
		(#('y1.2c4.snd' 'y1.4a3b.snd' 'y1.4a1.snd' 'y1.2b2b.snd' 'y1.2b1.snd' 
						'y1.2c2a.snd' 'y1.2a2.snd' 'y1.4b2.snd') 
				atRandom: rand)].
	1 to: size do: [ :i | (arrArray at: 8) at: i put: (rand nextFrom: 0.1 to: 0.9)].
	1 to: size do: [ :i | (arrArray at: 9) at: i put: sound].
	1 to: size do: [ :i | (arrArray at: 10) at: i put: (rand next &gt; 0.5)].
	^(Mix named: 'random1'  
		fromSelectors: #(start: duration: loudness: voice: fadeIn: fadeOut: name: pos: sound: solo:) 
		values: arrArray) channels: 2; start: 1.5; stop: 4.5</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.Mix</class-id>
<body>&lt;historical&gt;</body>
</comment>

<methods>
<class-id>Siren.Mix</class-id> <category>initialize-release</category>

<body package="Siren" selector="initializeAnonymous">initializeAnonymous
	"Set the start-up values of the receiver."

	super initializeAnonymous.
	duration := Duration value: 0.0.
	self fileName: 'temp.mix'.
	self channels: 1.
	self start: 0.
	self stop: 2.0</body>
</methods>

<methods>
<class-id>Siren.Mix</class-id> <category>accessing</category>

<body package="Siren" selector="channels">channels
	"Answer the number of channels of the receiver."

	channels == nil
		ifTrue: [channels := events first event sound channels].
	^channels</body>

<body package="Siren" selector="channels:">channels: aNumber
	"Set the number of channels of the receiver."

	channels := aNumber</body>

<body package="Siren" selector="defaultEventClass">defaultEventClass
	"Answer the default note event class."

	^MixElement</body>

<body package="Siren" selector="fileName">fileName
	"Answer the receiver's 'fileName' property."

	^properties at: #fileName</body>

<body package="Siren" selector="fileName:">fileName: aString
	"Set the receiver's property 'fileName' to be aString."

	self propCheck.
	properties at: #fileName put: aString</body>

<body package="Siren" selector="name">name
	"Answer the receiver's name."

	^properties at: #fileName ifAbsent: ['temp']</body>

<body package="Siren" selector="rate">rate
	"Answer the sample rate of the receiver."

	rate == nil
		ifTrue: [rate := events first value sound rate].
	^rate</body>

<body package="Siren" selector="rate:">rate: aNumber
	"Set the sample rate of the receiver."

	rate := aNumber</body>
</methods>

<methods>
<class-id>Siren.Mix</class-id> <category>markers</category>

<body package="Siren" selector="start">start
	"Answer the start marker."

	^properties at: #start ifAbsent: [0]</body>

<body package="Siren" selector="start:">start: aDuration
	"Set the start marker."

	properties at: #start put: aDuration</body>

<body package="Siren" selector="stop">stop
	"Answer the stop marker."

	^properties at: #stop ifAbsent: [0]</body>

<body package="Siren" selector="stop:">stop: aDuration
	"Set the stop marker."

	properties at: #stop put: aDuration</body>

<body package="Siren" selector="toggle:forEvent:">toggle: field forEvent: line
	"Handle the 'toggle' event from the controller, changing the given field of the given event."

	| evt val |
	evt := (events at: line) event.
	val := evt at: field.
	evt at: field put: val not.
"	Transcript show: 'event ', line printString, ' ', field, ' set to ', val not printString; cr."</body>
</methods>

<methods>
<class-id>Siren.Mix</class-id> <category>mixing</category>

<body package="Siren" selector="breakPointsFor:from:to:">breakPointsFor: evs from: aStart to: aStop
	"Answer the sorted collection of envelope breakpoints for the given events."

	| bpoints rejects ev start dur |
	bpoints := SortedCollection new.
	bpoints add: aStart.
	evs do:
		[ :ass | 
		start := ass key asSec value.
		bpoints add: start.
		ev := ass value.
		dur := ev duration asSec value.
		ev fadeIn == nil
			ifFalse: [bpoints add: start + (dur * ev fadeIn)].
		ev fadeOut == nil
			ifFalse: [bpoints add: start + dur - (dur * ev fadeOut)].
		bpoints add: start + dur].
	bpoints add: aStop.
	bpoints := bpoints select: [ :p | ((p &gt;= aStart) and: [p &lt;= aStop])].
	rejects := OrderedCollection new.
	1 to: (bpoints size - 1) do:
		[ :i |
		(bpoints at: i) = (bpoints at: (i + 1))
			ifTrue: [rejects add: (bpoints at: i)]].
	bpoints removeAll: rejects.
	^bpoints</body>

<body package="Siren" selector="mix">mix
	"Perform the mix of the enabled elements, creating a new sound."

	clipped := 0.
	Transcript cr.
	Cursor wait showWhile:
		[self mixFrom: (self at: #start) to: (self at: #stop)].
	Transcript show: 'Mix done; ', ((self at: #stop) - (self at: #start)) printString, ' secs. ',
				clipped printString, ' clipped samples.'; cr.</body>

<body package="Siren" selector="mixCollection:from:to:">mixCollection: evs from: aStart to: aStop
	"Perform the mix of the given elements, creating a new sound."

"	&lt;primitive: 17090&gt;"
	| bpoints p1 p2 |
	bpoints := self breakPointsFor: evs from: aStart to: aStop.
"	Transcript show: bpoints printString; cr."
"	Transcript show: evs size printString, ' events'; cr."
	1 to: (bpoints size - 1) do:
		[ :count | 
		p1 := (bpoints at: count).
		p2 := (bpoints at: (count + 1)).
		evs do:
			[ :ev |
			((ev start asSec value &lt; p2) and: [(ev stop asSec value) &gt; p1])
				ifTrue: [ev event sound channels = 1
					ifTrue: [self channels = 1
						ifTrue: [self mixMtoMEvent: ev frombreakPointsFor: evs from: p1 to: p2]
						ifFalse: [self mixMtoSEvent: ev frombreakPointsFor: evs from: p1 to: p2]]
					ifFalse: [self mixStoSEvent: ev frombreakPointsFor: evs from: p1 to: p2]]]]</body>

<body package="Siren" selector="mixFrom:to:">mixFrom: aStart to: aStop
	"Perform the mix of the enabled elements, creating a new sound."

	| collection size |
	size := ((aStop - aStart) * self rate * channels) truncated + 1.
	output := SampledSound named: self name size: size asInteger format: #lin16Bit.
	output rate: self rate.
	output channels: self channels.
	output duration: (aStop - aStart).
	collection := events select:
		[ :e | 
		(((e event at: #on) ~~ false) and: [(e start asSec value &lt; aStop) or: 
			[(e start asSec value + e event duration value) &gt; aStart]])].
	collection isEmpty
		ifTrue: [Transcript show: 'empty mix.'; cr. 
				^self].
	self mixCollection: collection from: aStart to: aStop</body>

<body package="Siren" selector="mixMtoMEvent:frombreakPointsFor:from:to:">mixMtoMEvent: evt frombreakPointsFor: evs from: start to: stop
	"Sum the given (mono) event into the (mono) output sound for the given range of samples."
	"MixView  simpleExample"

	| event s1 s2 iindex estart sound amp amp1 amp2 sample step |
	event := evt event.
	Transcript show: 'Mixing "', event name, '" from ', start printString, ' to ', stop printString.
	sound := event sound.
	s1 := (start - self start * self rate) truncated + 1.
	s2 := (stop - self start * self rate) truncated.
	estart := evt start asSec value.
	iindex := (self start - estart * self rate) truncated.
	amp := event loudness value.
	amp1 := event amplitudeAt: (start - estart).
	amp2 := event amplitudeAt: (stop - estart).
"	Transcript crtab; show: 's1: ', s1 printString, ' s2: ', s2 printString, ' ii: ', iindex printString; cr."
	(amp1 = amp2)				"fading in or out?"
		ifTrue: [amp1 = 0
			ifTrue: [Transcript show: '(zero)'; cr]
			ifFalse: [Transcript cr.
				s1 to: s2 do:
					[ :count | 
					sample := (output sampleAt: count) + ((sound sampleAt: count + iindex) * amp).
					(sample abs &gt; 32767) ifTrue: [clipped := clipped + 1].
					output sampleAt: count put: sample]]]
		ifFalse: [Transcript show: '		fade from ', amp1 printString, ' to ', amp2 printString; cr.
			step := (amp2 - amp1) / (s2 - s1).
			s1 to: s2 do:
				[ :count |
				sample := (output sampleAt: count)
						+ ((sound sampleAt: count + iindex) * (amp1 + (count  - s1 * step))).
				(sample abs &gt; 32767) ifTrue: [clipped := clipped + 1].
				output sampleAt: count put: sample]]</body>

<body package="Siren" selector="mixMtoSEvent:frombreakPointsFor:from:to:">mixMtoSEvent: evt frombreakPointsFor: evs from: start to: stop
	"Sum the given (mono) event into the (stereo) output sound for the 
	given range of samples."
	"MixView  simpleExample"

	| event s1 s2 iindex estart sound amp1 amp2 lVal rVal sample osample 
		ocount minS maxS step |
	event := evt event.
	sound := event sound.
	Transcript show: 'Mixing "', event name, '" from ', start printString, 
			' to ', stop printString.
	s1 := (start - self start * self rate) + 1.
	s2 := (stop - self start * self rate).
	estart := evt start asSec value.
	iindex := (self start - estart * self rate) truncated.
	lVal := event position.
	rVal := 1.0 - lVal.
	minS := output minSample asFloat.
	maxS := output maxSample asFloat.
"	Transcript crtab; show: 's1: ', s1 printString, ' s2: ', s2 printString, 
			' ii: ', iindex printString, 
			 '  	l: ', lVal printString, ' r: ', rVal printString; cr."
	amp1 := event amplitudeAt: (start - estart).
	amp2 := event amplitudeAt: (stop - estart).
	(amp1 = amp2)			"fading in or out?"
		ifTrue: [Transcript cr.				"no fade"
			lVal := lVal * amp1.
			rVal := rVal * amp1.
			0.0 = amp1
				ifFalse: [s1 to: s2 do:
					[ :count |
					sample := (sound sampleAt: count truncated + iindex) asFloat.
					ocount := (count * 2.0 - 1.0) truncated.
					0.0 = lVal
						ifFalse: [osample := (output sampleAt: ocount) asFloat 
											+ (sample * lVal).
							(osample &lt; minS) ifTrue: [clipped := clipped + 1].
							(osample &gt; maxS) ifTrue: [clipped := clipped + 1].
							output sampleAt: ocount put: osample].
					ocount := ocount + 1.
					0.0 = rVal
						ifFalse: [osample := (output sampleAt: ocount) asFloat 
											+ (sample * rVal).
							(osample &lt; minS) ifTrue: [clipped := clipped + 1].
							(osample &gt; maxS) ifTrue: [clipped := clipped + 1].
							output sampleAt: ocount put: osample]]]]
		ifFalse: [				"do fade"
			Transcript show: '		fade from ', amp1 printString, 
							' to ', amp2 printString; cr.
			step := (amp2 - amp1) / (s2 - s1) asFloat.
			s1 to: s2 do:
				[ :count |
				sample := (sound sampleAt: count truncated + iindex) asFloat.
				ocount := (count * 2.0 - 1.0) truncated.
				lVal = 0.0
					ifFalse: [osample := ((output sampleAt: ocount) asFloat 
									+ (sample * lVal * (amp1 + (count - s1 * step)))).
							(osample &lt; minS) ifTrue: [clipped := clipped + 1].
							(osample &gt; maxS) ifTrue: [clipped := clipped + 1].
						output sampleAt: ocount put: osample].
				ocount := ocount + 1.
				rVal = 0.0
					ifFalse: [osample := ((output sampleAt: ocount) asFloat 
									+ (sample * rVal * (amp1 + (count - s1 * step)))).
							(osample &lt; minS) ifTrue: [clipped := clipped + 1].
							(osample &gt; maxS) ifTrue: [clipped := clipped + 1].
						output sampleAt: ocount put: osample]]]</body>

<body package="Siren" selector="mixStoSEvent:frombreakPointsFor:from:to:">mixStoSEvent: evt frombreakPointsFor: evs from: start to: stop
	"Sum the given (stereo) event into the (stereo) output sound for the given range of samples."</body>

<body package="Siren" selector="play">play
	"Perform the mix and play the result."

"	TimeProfiler profile: [self mix]."
	self mix.
"	output play"
	SoundView openOn: output</body>
</methods>

<methods>
<class-id>Siren.Mix</class-id> <category>printing</category>

<body package="Siren" selector="printOn:">printOn: aStream
	"Print out the receiver's events on the argument."

	| anEvent |
	aStream nextPutAll: '(Mix ', (self name printString), ' (d: ', 
			duration value printString, ' sec, ', 
			events size printString, ' events, ',
			channels printString, ' channels)'.
	aStream cr.
	1 to: events size do: [ :ind | 
		anEvent := events at: ind.
		aStream tab; nextPutAll: anEvent key printString, ' =&gt; '.
		anEvent value printOn: aStream]</body>
</methods>


<class>
<name>MixView</name>
<environment>Siren</environment>
<super>Siren.DisplayListView</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>colorArray textView </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MixView class</class-id> <category>instance creation</category>

<body package="Siren" selector="open">open
	"Open a MixView on an empty model"
	"MixView open"

	| chans |
	chans := DialogView
		choose: 'Mono or stereo mix?'
		labels: #('mono' 'stereo')
		values: #(1 2)
		default: nil.
	chans = 1 ifTrue: [^self openOn: ((MixEditor new) eventList: Mix new)].
	chans = 2 ifTrue: [^self openOn: ((MixEditor new) eventList: Mix newStereo)].
	self error: 'Illegal number of channels.'</body>

<body package="Siren" selector="openOn:">openOn: aMixEditor
	"Open a MixView taking the mix editor as the model"
	"MixView openOn: ((MixEditor new) eventList: Mix randomExample)"

	| me window container wrapper buttons sliders bWidth sWidth buttonWrapper mainWrapper sliderWrapper textView |
	bWidth := 118.
	sWidth := -90.
	window := ScheduledWindow new.
	window label: 'MODE Mix View'.
	window minimumSize: Screen inset10percent extent.
	container := ViewWithResizableSubviews new.
	me := self new initialize.
	me model: aMixEditor.
	me displayList: (me layoutManager displayListFor: me).
	me list model: aMixEditor eventList.
	self addButtonsOn: me for: aMixEditor to: container in: ((0@0) extent: (1.0@0.08)).
	buttons := MixButtonPanel basicNew mainView: me; initialize.
	buttonWrapper := LookPreferences edgeDecorator on: buttons.
	container add: buttonWrapper in: (LayoutFrame new
				leftFraction: 0; topFraction: 0.08;
				rightFraction: 0; rightOffset: bWidth; bottomFraction: 1).
	wrapper := SlaveScrollWrapper new.
	wrapper setComponent: me 
			scrollOffsetHolder: buttonWrapper scrollerComponent scrollOffsetHolder.
	mainWrapper := LookPreferences edgeDecorator on: wrapper.
	mainWrapper useHorizontalScrollBar; noVerticalScrollBar.
	container add: mainWrapper in: (LayoutFrame new
				leftFraction: 0; leftOffset: bWidth; 
				topFraction: 0.08;
				rightFraction: 1; rightOffset: sWidth; 
				bottomFraction: 1; bottomOffset: -16).
	sliders := MixSliderPanel basicNew mainView: me; initialize.
	wrapper := SlaveScrollWrapper new.
	wrapper setComponent: sliders
			scrollOffsetHolder: buttonWrapper scrollerComponent scrollOffsetHolder.
	sliderWrapper := LookPreferences edgeDecorator on: wrapper.
	sliderWrapper noVerticalScrollBar.
	container add: sliderWrapper in: (LayoutFrame new
				leftFraction: 1; leftOffset: sWidth; topFraction: 0.08;
				rightFraction: 1; bottomFraction: 1).
	textView := TextView on: aMixEditor aspect: #text change: nil menu: nil initialSelection: nil.
	textView controller: NoController new.
	textView displaySelection: false.
	textView textStyle: (TextAttributes styleNamed: #small).
	me textView: textView.
	container add: textView in: (LayoutFrame new
				leftFraction: 0; leftOffset: bWidth; 
				topFraction: 1; topOffset: -16;
				rightFraction: 1; rightOffset: sWidth; 
				bottomFraction: 1).
	window component: container.
	window open</body>
</methods>

<methods>
<class-id>Siren.MixView class</class-id> <category>subview creation</category>

<body package="Siren" selector="addButtonsOn:for:to:in:">addButtonsOn: me for: dled to: container in: rect
	"Add the default button panel to the instance in the given rectangle."

	| x0 y0 x1 y1 width |
	x0 := rect left.	x1 := rect right.
	y0 := rect top.	y1 := rect bottom.
	width := 0.14 * (x1 - x0).
	self addButtonOn: me labeled: 'Play'
		doing: [:model :value | dled play] 
		in: (LayoutFrame new leftFraction: x0; topFraction: y0; 
							rightFraction: width;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'File'
		doing: [:model :value | dled doFile] 
		in: (LayoutFrame new leftFraction: width; topFraction: y0; 
							rightFraction: width * 2;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Edit'
		doing: [:model :value | dled doEdit] 
		in: (LayoutFrame new leftFraction: width * 2;	topFraction: y0; 
							rightFraction: width * 3;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Inspect'
		doing: [:model :value | dled doInspect] 
		in: (LayoutFrame new leftFraction: width * 3;	topFraction: 0.0; 
							rightFraction: width * 4;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Add'
		doing: [:model :value | dled add] 
		in: (LayoutFrame new leftFraction: width * 4;	topFraction: y0; 
							rightFraction: width * 5;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Zoom'
		doing: [:model :value | dled doZoom] 
		in: (LayoutFrame new leftFraction: width * 5;	topFraction: y0; 
							rightFraction: width * 6;	bottomFraction: y1) 
		to: container.
	self addButtonOn: me labeled: 'Properties'
		doing: [:model :value | dled doPropEdit] 
		in: (LayoutFrame new leftFraction: width * 6;	topFraction: y0; 
							rightFraction: x1;	bottomFraction: y1) 
		to: container.</body>
</methods>

<methods>
<class-id>Siren.MixView class</class-id> <category>examples</category>

<body package="Siren" selector="buzzExample">buzzExample
	"Open a MixView on a mix event list with three pulse trains."
	"MixView buzzExample"

	| elem mix |
	mix := Mix new.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 8000 freq: 150 
					width: 0.0005 chans: 1 zero: 0)
		ampl: 0.4 name: 'buzz1' fadeIn: 0 fadeOut: 0.2 pos: 0.5.
	mix add: elem at: 0.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 8000 freq: 380 
					width: 0.0005 chans: 1 zero: 0)
		ampl: 0.5 name: 'buzz2' fadeIn: 0.2 fadeOut: 0.2 pos: 0.8.
	mix add: elem at: 0.213.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 8000 freq: 777 
					width: 0.0005 chans: 1 zero: 0)
		ampl: 0.3 name: 'buzz3' fadeIn: 0.0 fadeOut: 0.2 pos: 0.2.
	mix add: elem at: 0.4421.
	mix start: 0.65.
	mix stop: 1.55.
	mix channels: 2.
	MixView openOn: ((MixEditor new) eventList: mix)</body>

<body package="Siren" selector="buzzExample2">buzzExample2
	"Open a MixView on a mix event list with three pulse trains."
	"MixView buzzExample2"

	| elem mix |
	mix := Mix new.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 44100 freq: 150 
					width: 0.005 chans: 1 zero: 0)
		ampl: 0.4 name: 'buzz1' fadeIn: 0 fadeOut: 0.2 pos: 0.5.
	mix add: elem at: 0.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 44100 freq: 380 
					width: 0.001 chans: 1 zero: 0)
		ampl: 0.5 name: 'buzz2' fadeIn: 0.2 fadeOut: 0.2 pos: 0.8.
	mix add: elem at: 0.213.
	elem := MixElement 
		sound: (SampledSound pulseTrainDur: 2.0 rate: 44100 freq: 477 
					width: 0.001 chans: 1 zero: 0)
		ampl: 0.3 name: 'buzz3' fadeIn: 0.0 fadeOut: 0.2 pos: 0.2.
	mix add: elem at: 0.4421.
	mix start: 0.65.
	mix stop: 1.55.
	mix channels: 2.
	MixView openOn: ((MixEditor new) eventList: mix)</body>

<body package="Siren" selector="example">example
	"Open a MixView on the random mix event list."
	"MixView example"

	| mix |
	mix := (Mix new"Named: #mRand")
					addAll: (Mix randomExample);
					addAll: (Mix randomExample).
	mix start: 1.7.
	mix stop: 4.2.
	mix channels: 2.
	MixView openOn:
			((MixEditor new) eventList: mix)</body>

<body package="Siren" selector="randomExample">randomExample
	"Open a MixView on the random mix event list."
	"MixView randomExample"

	MixView openOn: ((MixEditor new) eventList: Mix randomExample)</body>

<body package="Siren" selector="sineExample">sineExample
	"Open a MixView on a mix event list with three sine waves."
	"MixView sineExample"

	| elem mix |
	mix := Mix new.
	elem := MixElement 
		sound: (SampledSound sineDur: 2.0 rate: 44100 freq: 100 chans: 1)
		ampl: 0.5 name: 'sine1' fadeIn: 0.5 fadeOut: 0.2 pos: 0.5.
	mix add: elem at: 0.
	elem := MixElement 
		sound: (SampledSound sineDur: 2.0 rate: 44100 freq: 150 chans: 1)
		ampl: 1.0 name: 'sine2' fadeIn: 0.5 fadeOut: 0.2 pos: 0.8.
	mix add: elem at: 0.813.
	elem := MixElement 
		sound: (SampledSound sineDur: 2.0 rate: 44100 freq: 200 chans: 1)
		ampl: 0.5 name: 'sine3' fadeIn: 0.5 fadeOut: 0.2 pos: 0.2.
	mix add: elem at: 1.8421.
	mix start: 0.65.
	mix stop: 1.55.
	mix channels: 2.
	MixView openOn: ((MixEditor new) eventList: mix)</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MixView</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	"Initialize the instance variables of the receiver."

	| col |
	super initialize.
	DisplayScreen default colorDepth = 1
		ifTrue: [colorArray := Array with: Color gray]
		ifFalse: [colorArray := (Color constantNames select:
			[ :n |
			col := Color perform: n.
			((col brightness &gt; 0.6) "and: [col brightness &lt; 0.99]")]) collect:
				[ :n | Color perform: n]].
	scale := 64@64.	"pixels/sec" "pixels/strip"
	self initForm</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>accessing</category>

<body package="Siren" selector="colorArray">colorArray
	"Answer the receiver's 'colorArray'."

	^colorArray</body>

<body package="Siren" selector="preferredBounds">preferredBounds
	"Answer the extent of the receiver's display object bounding box."

	| list |
	list := self model eventList.
	^0@0 extent: ((list duration asSec value * scale x + 128) @ (list events size * scale y))</body>

<body package="Siren" selector="textView">textView
	"Answer the receiver's 'textView'."

	^textView</body>

<body package="Siren" selector="textView:">textView: aView
	"Set the receiver's 'textView'."

	^textView := aView</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>layout manager access</category>

<body package="Siren" selector="colorFor:">colorFor: event
	"Answer the correct display color for the given assoc."

	^(event voice == nil)
		ifTrue: [colorArray first]
		ifFalse: [self colorArray at: ((event voice max: 1) 
					min: colorArray size)].</body>

<body package="Siren" selector="itemFor:">itemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

"	^self rectangleItemFor: anAss"
"	^self barItemFor: anAss"
	^self trapezoidItemFor: anAss</body>

<body package="Siren" selector="layoutManager">layoutManager
	"Answer a default layout manager for the receiver."

	^(MixLayoutManager new)
			timeScale: scale x;
			timeOffset: 0;
			pitchScale: scale y;
			pitchOffset: 0;
			view: self</body>

<body package="Siren" selector="trapezoidItemFor:">trapezoidItemFor: anAss
	"Answer the correct visual item for the given assoc. (sent from layoutManagers)."

	| event width yOff yIns vArray color |
	event := anAss value.
	width := event duration asSec value * scale x.
	yOff := scale y - 3.
	yIns := 0.
	color := self colorFor:event.
	vArray := Array new: 5.
	vArray at: 1 put: 0@yOff.
	vArray at: 2 put: (((width*event fadeIn) truncated)@yIns).
	vArray at: 3 put:(((width*(1-event fadeOut)) truncated)@yIns).
	vArray at: 4 put: (width@yOff).
	vArray at: 5 put: 0@yOff.
	^((DisplayPolyline model: anAss offset: 0@0)
		color: color;
		vertices: vArray) fill: true; border: Color black</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>controller access</category>

<body package="Siren" selector="defaultControllerClass">defaultControllerClass
	"Answer the class of the default controller for the receiver."

	^MixController</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>displaying</category>

<body package="Siren" selector="deselect">deselect
	"Show the de-selection of the current item."

	(displayList nodeFor: model selection) 
			color: (self colorFor: (model selection) event); fill: true</body>

<body package="Siren" selector="displayMarkerAt:on:">displayMarkerAt: pos on: pGC
	"Draw the vertical line marker at the given point."

	| box y1 y2 |
	pos == nil ifTrue: [^self].
	box := pGC clippingBounds.
	y1 := box top.
	y2 := box bottom.
	pGC paint: Color black.
	pGC displayLineFrom: (pos@y1) to: (pos@y2).</body>

<body package="Siren" selector="displayOn:">displayOn: aGC
	"Display the receiver on its cached form."

	aGC paint: DisplayListView backgroundColor.
	aGC displayRectangle: aGC clippingBounds.
	background == nil ifFalse: [background displayOn: aGC].
	model selection == nil ifFalse: [self select].
	displayList displayOn: aGC.
	self displayMarkerAt: model eventList start * scale x on: aGC.
	self displayMarkerAt: model eventList stop * scale x on: aGC.</body>

<body package="Siren" selector="displaySelection">displaySelection
	"Display the model's selection in the view"

	| gc |
	model selection == nil
		ifFalse: [gc := self graphicsContext.
			self isColor
				ifTrue: [gc paint: Color red]
				ifFalse: [gc lineWidth: 2].
			gc displayRectangularBorder: model selection computePreferredBounds.
			self isColor
				ifFalse: [gc lineWidth: 1]]</body>

<body package="Siren" selector="draw:">draw: item
	"Draw the given item (assuming it's selected)."

	item displayOn: self graphicsContext.</body>

<body package="Siren" selector="erase:">erase: element
	"Erase the given item (assuminbg it's selected)."

	| item |
	item := (self list nodeFor: element) copy.
	item border: self backgroundColor.
	item displayOn: self graphicsContext.</body>

<body package="Siren" selector="select">select
	"Show the selection of the current item."

	(displayList nodeFor: model selection) color: Color black; fill: false</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>updating</category>

<body package="Siren" selector="update:">update: aspectSymbol
	"Some aspect of the model has changed.
	By default, repaint the receiver."

	aspectSymbol == #contents
		ifTrue: [self flushDisplayList].
	aspectSymbol == #zoomIn
		ifTrue: [self scaleX: scale x * 1.5.
			self flushDisplayList].
	aspectSymbol == #zoomOut
		ifTrue: [self scaleX: scale x * 0.75.
			self flushDisplayList].
	aspectSymbol == #list
		ifTrue: [self flushDisplayList].
	aspectSymbol == #text
		ifTrue: [^self].
	self changedPreferredBounds: nil.
	self invalidate</body>
</methods>

<methods>
<class-id>Siren.MixView</class-id> <category>private</category>

<body package="Siren" selector="drawBackgroundGrid">drawBackgroundGrid
	"Display the gridding of the receiver."
	"MixView randomExample2"

	| box pt1 pt2 lineBlock aGC |
	aGC := background graphicsContext.
	aGC paint: self backgroundColor.
	aGC displayRectangle: background bounds.
	aGC paint: self clefColor.
	box := aGC clippingBounds.
	pt1 := 0 @ box top.
	pt2 := 0 @ box bottom.
	lineBlock := [ :x | 
			pt1 x: x.
			pt2 x: x.
			aGC displayLineFrom: pt1 to: pt2].
	0 to: box width by: (scale x * 2) do:
		[ :i | lineBlock value: i].
	pt1 := box left @ 0.
	pt2 := box right @ 0.
	lineBlock := [ :y | 
			pt1 y: y.
			pt2 y: y.
			aGC displayLineFrom: pt1 to: pt2].
	0 to: box height by: scale y do:
		[ :i | lineBlock value: i].</body>

<body package="Siren" selector="initForm">initForm
	"Copy the default backgroundForm and draw a grid (lines * 2-sec) on it."

	background := Form extent: 600@600.
	background background: Color lightGray "DisplayListView backgroundColor".
	self scaleY == nil
		ifFalse: [self drawBackgroundGrid]</body>
</methods>


<class>
<name>MixEditor</name>
<environment>Siren</environment>
<super>Siren.EventListEditor</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>selections </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<shared-variable>
<name>LastFileSelected</name>
<environment>Siren.MixEditor</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<methods>
<class-id>Siren.MixEditor</class-id> <category>accessing</category>

<body package="Siren" selector="selections">selections
	"Answer the set of selections."

	selections == nil
		ifTrue: [selections := OrderedCollection new].
	^selections</body>

<body package="Siren" selector="text">text
	"Answer the message to appear in the mix view's text field."

	| evt name start len |
	^(selection == nil)
		ifTrue: ['']
		ifFalse: [evt := selection event.
			start := selection key asSec value.
			len := (SoundBrowser rootName, Filename separatorString) size + 1.
			name := evt sound name.
			len &lt; name size
				ifTrue: [name := name copyFrom: len to: (name size - 4)].
			('(', (start printString: 3), ' - ',
			((evt duration asSec value + start) printString: 3), ') ',
			name, ' in: ', (evt fadeIn printString: 2), ' out: ', (evt fadeOut printString: 2),
			' am: ', (evt loudness value printString: 3), 
			(eventList channels = 2 ifTrue: [' po: ', (evt position printString: 3)] ifFalse: ['']))]</body>
</methods>

<methods>
<class-id>Siren.MixEditor</class-id> <category>menu messages</category>

<body package="Siren" selector="add">add
	"Add a new mix element to the mix."

	| dir def name snd elem |
	LastFileSelected == nil
		ifTrue: [dir := SoundBrowser rootName, Filename separatorString.
			def := '']
		ifFalse: [dir := LastFileSelected asFilename directory asString, 
					Filename separatorString.
			def := LastFileSelected asFilename tail].
	name := FilenameDialog getFilenameIn: dir default: def 
					labeled: 'Pick a Sound File Name'.
	name = '' ifTrue: [^self].
	snd := SampledSound fromFile: name.
	LastFileSelected := name.
"	snd name: (snd name copyFrom: (dir size + 1) to: (snd name size - 4))."
	elem := MixElement sound: snd ampl: 1.0 fadeIn: 0.0 fadeOut: 0.0 pos: 0.5.
	eventList add: elem at: eventList start.
	self changed: #contents</body>

<body package="Siren" selector="addSelection:">addSelection: item
	"Add the given item to the list of selections."

	selections == nil
		ifTrue: [selections := OrderedCollection new].
	(selections includes: item)
		ifTrue: [selections remove: item]
		ifFalse: [selections add: item]</body>

<body package="Siren" selector="cut">cut
	"Cut the current mix element from the mix."

	eventList remove: selection.
	selection := nil.
	self changed: #contents</body>

<body package="Siren" selector="paste">paste
	"Paste in the currently-selected mix element."

	eventList add: copyBuffer copy.
	self changed: #contents</body>

<body package="Siren" selector="sort">sort
	"Sort the receiver's mix elements."

	eventList events reSort.
	self changed: #contents</body>
</methods>

<methods>
<class-id>Siren.MixEditor</class-id> <category>editing</category>

<body package="Siren" selector="doEdit">doEdit
	"Edit button--open a sound view on the currently-selected sound."

	selection == nil
		ifFalse: [SoundView openOn: (selection event sound)]</body>

<body package="Siren" selector="doFile">doFile
	"File I/O button--prompt the user to read/write a mix file (read if shift down)."

	| name dir stream newMix |
	LastFileSelected == nil
		ifTrue: [dir := SoundBrowser rootName, FileDirectory separatorString]
		ifFalse: [dir := LastFileSelected asFilename directory asString, 
							FileDirectory separatorString].
	Sensor leftShiftDown
		ifFalse: [name := FillInTheBlank getFilenameIn: dir 
					default: (eventList at: #fileName) 
					labeled: 'Pick a File Name to Write Mix'.
			name = '' ifTrue: [^self].
			stream := name asFilename writeStream.
			eventList storeOn: stream.
			stream close]
		ifTrue: [name := FillInTheBlank getFilenameIn: dir 
					default: (eventList at: #fileName) 
					labeled: 'Pick a File Name to Write Mix'.
			name = '' ifTrue: [^self].
			stream := name asFilename readStream.
			newMix := Compiler evaluate: stream contentsOfEntireFile.
			self eventList: newMix.
			stream close]</body>

<body package="Siren" selector="doZoom">doZoom
	"Zoom in by 50% (or out if shift-down)."

	Sensor leftShiftDown
		ifFalse: [self changed: #zoomIn]
		ifTrue: [self changed: #zoomOut]</body>
</methods>

<methods>
<class-id>Siren.MixEditor</class-id> <category>links and groups</category>

<body package="Siren" selector="selectItem:">selectItem: anAorEL
	"Set the given association as the receiver's selection."

	selection := anAorEL.
	self changed: #text.
"	self changed: #selection"</body>
</methods>


<class>
<name>MixSliderController</name>
<environment>Siren</environment>
<super>Siren.MixController</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Mixing</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.MixSliderController</class-id>
<body>&lt;historical&gt;</body>
</comment>

<methods>
<class-id>Siren.MixSliderController</class-id> <category>control defaults</category>

<body package="Siren" selector="dragAmplitudeFor:">dragAmplitudeFor: line
	"Drag the slider for the position field of the given event line."

	| value evt width pt p2 stereo tView |
	evt := (model eventList events at: line) value.
	value := evt loudness.
	width := view width.
	stereo := (evt sound channels = 2).
	pt := 0@0.
	tView := view mainView textView.
	stereo
		ifFalse: [[sensor redButtonPressed] whileTrue:
			[p2 := sensor cursorPoint.
			p2 ~= pt
				ifTrue: [pt := p2.
					value := pt x / width * view maxVal.
					view clearAmplOf: line.
					view displayAmplOf: line value: value on: view graphicsContext.
					evt loudness: (((value asFloat) max: 0.0) min: 5.0).
					tView newText: model text.
					tView invalidateRectangle: tView bounds repairNow: true]]]
		ifTrue: [self halt].</body>

<body package="Siren" selector="dragPositionFor:">dragPositionFor: line
	"Drag the slider for the position field of the given event line."

	| value evt width pt p2 tView |
	evt := (model eventList events at: line) value.
	value := evt position.
	width := view width.
	pt := 0@0.
	tView := view mainView textView.
	[sensor redButtonPressed] whileTrue:
		[p2 := sensor cursorPoint.
		p2 ~= pt
			ifTrue: [pt := p2.
				value := ((pt x asFloat / width) max: 0.0) min: 1.0.
				view clearPosOf: line.
				view displayPosOf: line value: value on: view graphicsContext.
				evt position: (((value asFloat) max: 0.0) min: 1.0).
				tView newText: model text.
				tView invalidateRectangle: tView bounds repairNow: true]]</body>

<body package="Siren" selector="redButtonActivity">redButtonActivity
	"Hit-detect items from the display list or drag a selection."

	| pt line field yS |
	model == nil ifTrue: [^nil].
	pt := sensor cursorPoint.
	yS := view scaleY.
	line := (pt y / yS) truncated.
	field := (pt y - (line * yS) / (yS / 2)) truncated.
	line := line + 1.
	line &gt; model eventList events size
		ifTrue: [sensor waitNoButton. ^self].
	view mainView deselect.
	model selectItem: (model eventList events at: line).
	view mainView invalidate.
	field = 0
		ifTrue: [self dragAmplitudeFor: line]
		ifFalse: [self dragPositionFor: line]</body>
</methods>



</st-source>
