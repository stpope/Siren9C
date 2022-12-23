View subclass: #HATView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-UI-HAT'!

!HATView methodsFor: 'window access'!
defaultWindow

	^0@0 extent: minimumSize! !


!HATView methodsFor: 'subview creation'!
addKeyboardView
	| keyboard | 

	keyboard := KeyboardView new.
	keyboard model: self model.
	keyboard addOctaves: 5.

	self addSubView: keyboard
				in: ( 0.05@0.75 extent: 0.9@0.16 )
				borderWidth: 0! !

!HATView methodsFor: 'subview creation'!
addScoreView
	| scores | 

	scores := BarChartView newOn: self model
						aspect: #keyScore
						bars: #keyScore:
						labels: #keyLabel:
						count: #numberOfKeys
						max: #maxKeyScore
						menu: nil.

	self addSubView: scores
				in: ( 0.0@0.4 extent: 1.0@0.3 )
				borderWidth: 1! !

!HATView methodsFor: 'subview creation'!
addTranscript
	| collector transcript | 

	collector := TextCollector new initialize.
	transcript := TextCollectorView new initialize.
	transcript model: collector.

	model transcript: collector.

	self addSubView: transcript
				in: ( 0.0@0.0 extent: 0.5@0.4 )
				borderWidth: 1! !

!HATView methodsFor: 'subview creation'!
addTreeView
	| trees | 

	trees := TreeView newOn: self model
						aspect: #chordTree
						tree: #chordTree
						node: #bestNode
						change: #inspectionNode:
						menu: #treeMenu.

	self addSubView: trees
				in: ( 0.5@0.0 extent: 0.5@0.4 )
				borderWidth: 1! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

HATView class
	instanceVariableNames: ''!

!HATView class methodsFor: 'instance creation'!
newOn: aModel
	"HATView new"

	| topView keyboard trees | 

	topView := super new.
	topView model: aModel.
	topView label:  'Harmonic Analysis Tool'.
	topView borderWidth: 1.
	topView minimumSize: 800@600.

	topView addTranscript.
	topView addScoreView.
	topView addTreeView.
	topView addKeyboardView.

	topView controller open! !
