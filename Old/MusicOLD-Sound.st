FileList subclass: #PlayList
	instanceVariableNames: 'longList autoRead lastModified '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-Sound'!

!PlayList methodsFor: 'initialize-release'!
initialize
	"Set upo the receiver for no-auto-read and no long listing."

	autoRead := ValueHolder with: false.
	longList := false! !


!PlayList methodsFor: 'file name list'!
commentFile
	"Add a comment to a sound file."

	| name comment |
	fileName == nil ifTrue: [^self].
	((fileName findString: '.snd' startingAt: 1) == 0)
		ifTrue: [^self].
	name := fileName copyUpTo: $ .
	comment := DialogView 
			request: '             Sound file comment                  ' 
			initialAnswer: ''.
	comment = '' ifTrue: [^self].
"	Transcript show:  'sfcomment ', '"', comment, '" ', name; cr."
	UnixProcess spawn: 'sfcomment ', '"', comment, '" ', name.! !

!PlayList methodsFor: 'file name list'!
fileListMenu
	"Answer the menu."

	"Evaluate this when you change this method:
		FileBrowser flushMenus"

	fileName == nil
		ifTrue: [^PopUpMenu
					labels: 'make directory...\long list' withCRs
					lines: #()
					values: #(makeDir showLongList)].

	"If fileName matches *.snd, return the special menu"
	('*.snd' match: fileName)
		ifTrue: [^PopUpMenu
				labels: ('play\copy name\add comment\rename as...\copy file to...\remove...\spawn') withCRs
				lines: #(1 2 5)
				values: #(playFile copyName commentFile renameFile
						copyFile deleteFile spawnSoundView)].

	"else return the inherited menu"
	^super fileListMenu! !

!PlayList methodsFor: 'file name list'!
fileName: selection
	"If selection is not nil, it is either the name of a file to be viewed, or a directory"

	| file fn |
	file := nil.
	lastModified := nil.
	selection == nil
		ifTrue:
			[selectionState := nil]
		 ifFalse:
			[file := selection copyUpTo: $ .
			selectionState := #fileInfo.
			Filename errorReporter errorSignal
				handle: [:ex | ex return]
				do: [fn := file asFilename.
					fn isDirectory
						ifTrue: [selectionState := #directory]
						ifFalse: [(autoRead value and:
									[fn exists and: [fn fileSize <= MaxAutoReadLength
											or: [DialogView choose: 'The file is large: ', fn fileSize printString, ' bytes', '
What would you like to do?'
												labels: #('get contents' 'get info')
												values: #(true false)
												default: true]]])
									ifTrue: [selectionState := #fileEditing]
									ifFalse: [selectionState := #fileInfo]]]].
	fileName := selection == nil
		ifTrue: [selection]
		ifFalse: [selection copyUpTo: $ ].
	self changed: #pattern.	"Reset the pattern, in case it has changed"
	self changed: #text! !

!PlayList methodsFor: 'file name list'!
playFile
	"Play the selected sound/MIDI/code file.
	 Edit your I/O utility pathnames or methods in here."

	| name type tempo |
	fileName == nil ifTrue: [^self].
	name := fileName copyUpTo: $ .	"skip anything after name"
	type := self fileType.
	type == #sound ifTrue:
		[SoundPort default playFileNamed: name].
	type == #MIDI  ifTrue:
		["Transcript show: 'playing MIDI file ', name, ' ... '."
		tempo := DialogView request: 'MIDI usec/clock tick'
							initialAnswer: '1000'.
		tempo = '' ifTrue: [^nil].
		UnixProcess spawn: (MusicMagnitude modeBinDir, 'mplay ',
				'-r ', tempo printString, ' ', name)].
"	Transcript show: 'done.'; cr"! !

!PlayList methodsFor: 'file name list'!
showLongList
	"Display the long version of the sound file list."

	longList := true.
	self changed: #text.
	longList := false! !

!PlayList methodsFor: 'file name list'!
spawnSoundView
	"Open a FileList if the current selection is a directory otherwise open a soundfile editor."

	selectionState = #directory
		ifFalse: [((fileName findString: '.snd' startingAt: 1) == 0)
			ifTrue: [self spawnFileList]
			ifFalse: [SoundView openOn:
				(SampledSound fromFile: (fileName copyUpTo: $ ))]]! !


!PlayList methodsFor: 'text'!
text
	"Return the text for the text subview."

	^longList
		ifTrue: [self listSoundFiles]
		ifFalse: [super text]! !


!PlayList methodsFor: 'private'!
fileType
	"Answer a symbol denoting whether the receiver's file is sound, MIDI, or CODEC."

	((fileName findString: '.snd' startingAt: 1) == 0)
		ifFalse: [^#sound]
		ifTrue: [((fileName findString: '.midi' startingAt: 1) == 0)
			ifFalse: [^#MIDI]
			ifTrue: [((fileName findString: '.au' startingAt: 1) == 0)
				ifFalse: [^#sound]
				ifTrue: [^nil]]]! !

!PlayList methodsFor: 'private'!
infoText
	"Returns aText with proper emphasis information 
	 from the file system information on fileName"
	"Uses space-padding so it looks best with fixed-width fonts"

	| fn dates stream mod  |
	(self fileType == #sound)
		ifFalse: [^super infoText].
	fn := fileName asFilename.
	dates := fn dates.
	mod := dates at: #modified.
	mod == nil
		ifFalse: [mod := mod first printString,'   ',mod last printString].
	stream := TextStream on: String new.
	stream emphasis: #italic;
		nextPutAll: 'sound file'; cr.
	stream emphasis: nil.
	self soundFileTextOn: stream.
	stream nextPutAll: ' size:     ';
		emphasis: nil;
		nextPutAll: fn fileSize printString, ' bytes';
		cr.
	mod == nil
		ifFalse:
			[stream nextPutAll: ' modified: ';
				emphasis: nil;
				nextPutAll: mod;
				cr].
	^stream contents! !

!PlayList methodsFor: 'private'!
list: aList
	"Set my list of files."

	| dirs files soundFiles name |
	dirs := (aList select: [ :file |
			name := Filename named: file. 
					(name exists and: [name isDirectory])]) asSortedCollection.
	files := (aList select: [ :file |
			name := Filename named: file. 
					(name exists and: [name isDirectory not])]).
	soundFiles := (files select: 
			[ :nme |
			(nme findString: '.snd' startingAt: 1) ~= 0]).
	files removeAll: soundFiles.
	list := OrderedCollection new.
	dirs do: [ :dir | list addLast: dir].
	soundFiles asSortedCollection do: [ :file | list addLast: file].
	files asSortedCollection do: [ :file | list addLast: file].
	selectionState := nil! !

!PlayList methodsFor: 'private'!
listSoundFiles
	"write a long list of the soundfiles using the 'lsf' utility."

	| match cmd proc reply connection files array |
	match := (fileName == nil 
		ifTrue: [myPattern string]
		ifFalse: [fileName asFilename isDirectory
			ifTrue: [fileName, '/*']
			ifFalse: [^self]]).
	files := (self filesMatching: (match, '.snd')) 
				asSortedCollection.
	files isEmpty ifTrue: [^self].
	array := Array new: (files size + 1).
	array at: 1 put: '-l'.
	1 to: files size do:
		[ :i |
		array at: i+1 put: (files at: i)].
	connection := UnixProcess pipeConnectionFor: 
				(MusicMagnitude modeBinDir, 'lsf')
			arguments: array
			setProcessDescriptor: [:pd | proc := pd].
	cmd := connection readStream.
	[reply := cmd contents.
	proc wait]
		valueNowOrOnUnwindDo: [cmd close].
	proc release.
	^reply! !

!PlayList methodsFor: 'private'!
soundFileTextOn: aStream
	"Answer the default text for a soundfile."

	| sound |
	sound := SampledSound headerFromFile: (fileName copyUpTo: $ ).
	sound duration == nil
		ifFalse: [aStream nextPutAll: ' duration: ', 
				sound duration asSeconds value printString, ' sec'; cr].
	sound properties == nil
	ifFalse: [sound properties associationsDo:
			[ :ass |
			aStream nextPutAll: ' ', ((ass key, ':') padTo: 10), 
				ass value printString; cr]].! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

PlayList class
	instanceVariableNames: ''!

!PlayList class methodsFor: 'instance creation'!
openOnPattern: aPattern
	"Create and schedule a view of a new instance of the receiver
	using the pattern aPattern.  For example, evaluate:"
		"PlayList open"

	| topView aFileList patternView label topWindow patternHeight codeView button1 button2 |
	patternHeight := [LookPreferences menuBarHeight 
					+ TextAttributes defaultLineGrid + 6].
	aFileList := self new.
	aPattern = '' 
		ifFalse: [Cursor read showWhile:
					[aFileList list: (SortedCollection new addAll: 
						(aFileList filesMatching: aPattern))].
				label := 'Play List on ', aPattern asString ]
		ifTrue: [label := 'Play List'].
	aFileList pattern: aPattern.
	topWindow := ScheduledWindow model: aFileList 
				label: label minimumSize: 400@400.
	topView := ViewWithResizableSubviews "CompositePart" new.
	topWindow component: topView.
	button1 := Button trigger
			model: ((PluggableAdaptor on: aFileList) 
						getBlock: [:m | true]
						putBlock: [:m :v | m perform: #directoryPattern]
						updateBlock: [:m :a :p | false]);
			label: 'enter'.
	button2 := Button trigger
			model: ((PluggableAdaptor on: aFileList) 
						getBlock: [:m | true]
						putBlock: [:m :v | m perform: #playFile]
						updateBlock: [:m :a :p | false]);
			label: 'play'.
	patternView := TextView
		 on: aFileList
		 aspect: #pattern 
		 change: #acceptPattern:from:
		 menu: #patternMenu
		 initialSelection: nil.
	patternView controller dispatchOn: Character cr to: #alwaysAcceptKey:.
	patternView controller dispatchOn: #Enter to: #alwaysAcceptKey:.
	patternView := LookPreferences edgeDecorator on: patternView.
	patternView noVerticalScrollBar.
	codeView := TextView
				 on: aFileList
				 aspect: #text
				 change: #acceptText:from:
				 menu: #textEdMenu
				 initialSelection: nil.
	topView
		 add: patternView
		 in: (LayoutFrame new
				leftOffset: 0;
				topOffset: 0;
				rightFraction: 1;
				rightOffset: -128;
				bottomOffset: patternHeight).
	topView
		 add: button1
		 in: (LayoutFrame new
				leftFraction: 1;
				leftOffset: -128;
				topOffset: 0;
				rightFraction: 1;
				rightOffset: -64;
				bottomOffset: patternHeight).
	topView
		 add: button2
		 in: (LayoutFrame new
				leftFraction: 1;
				leftOffset: -64;
				topOffset: 0;
				rightFraction: 1;
				bottomOffset: patternHeight).
	topView add:
		(LookPreferences edgeDecorator on: (SelectionInListView
			 on: aFileList
			 aspect: #fileNameList
			 change: #fileName:
			 list: #fileNameList
			 menu: #fileListMenu
			 initialSelection: #fileName))
		in: (LayoutFrame new
				leftOffset: 0;
				topOffset: patternHeight;
				rightFraction: 1;
				bottomFraction: 0.4).
	topView add:
		(LookPreferences edgeDecorator on: codeView)
		in: (0@0.4 extent: 1@0.6).
	topWindow icon: (Icon constantNamed: #file).
	topWindow open! !


!PlayList class methodsFor: 'default pattern access'!
defaultPattern
	"Answer the default file pattern."
	"PlayList open"

"	^MusicMagnitude modeRootName, '*'"
	^SoundBrowser rootName, Filename separatorString, '*'! !


Browser subclass: #SoundBrowser
	instanceVariableNames: 'sound soundView showAll '
	classVariableNames: 'RootName SoundLibrary '
	poolDictionaries: ''
	category: 'MusicOLD-Sound'!
!SoundBrowser commentStamp: '<historical>' prior: 0!
The SoundBrowser is a new Browser for managing the System's SoundLibrary with class SoundManager.
!


!SoundBrowser methodsFor: 'initialize-release'!
initialize
	"Set up the default state of the receiver."

	showAll := false! !


!SoundBrowser methodsFor: 'accessing'!
sound: aSound
	"Set the receiver's sound."

	sound := aSound! !

!SoundBrowser methodsFor: 'accessing'!
soundName
	"Answer the name of the selection."

	| name |
	name := RootName.
	category isNil ifFalse: [name  :=  name, Filename separatorString, category].
	className isNil ifFalse: [name  :=  name, Filename separatorString, className].
	protocol isNil ifFalse: [name  :=  name, Filename separatorString, protocol].
	selector isNil ifFalse: [name  :=  name, Filename separatorString, selector].
	^name! !

!SoundBrowser methodsFor: 'accessing'!
soundView: aSoundView
	"Set the receiver's view."

	soundView := aSoundView! !


!SoundBrowser methodsFor: 'category list'!
category: selection
	"Set the receiver's currently selected category to be selection and 
	update the class list."

	className := nil.
	protocol := nil.
	selector := nil.
	^super category: selection! !

!SoundBrowser methodsFor: 'category list'!
categoryList
	"Answer the sequenceable collection containing the class categories that 
	the receiver accesses."

	| length |
	length := RootName size + 2.
	^(self sortFileNames: SoundLibrary)
		collect:
			[ :item | item copyFrom: length to: item size]! !

!SoundBrowser methodsFor: 'category list'!
categoryMenu
	"Answer a Menu of operations on class categories to be 
	displayed when the operate menu button is pressed."

	| label |
	label := self showMsg.
	^category == nil
		ifTrue: [PopUpMenu
				labels: ('update\', label) withCRs
				values: #(#updateCategoryList showAll)]
		ifFalse: [PopUpMenu
				labels: ('edit\spawn\', label) withCRs
				values: #(editSelection spawnPlayList showAll)]! !

!SoundBrowser methodsFor: 'category list'!
updateCategoryList
	"Update the category list's file names"

	self class initialize.
	self changed: #categoryList.
	self changed: #category! !


!SoundBrowser methodsFor: 'class list'!
classList
	"Answer the sequenceable collection containing the classes that 
	are accessed from the currently selected category."

	| name file list length |
	category == nil ifTrue: [^ nil].
	name := RootName, Filename separatorString, category.
	file := Filename named: name.
	file exists ifFalse: [^nil].
	file isDirectory ifFalse: [^nil].
	list := self fileNamesForDirectory: name.
	length := name size + 2.
	^list collect:
			[ :item |
			item copyFrom: length to: item size]! !

!SoundBrowser methodsFor: 'class list'!
classMenu
	"Answer a Menu of operations on classes to be 
	displayed when the operate menu button is pressed."

	| label |
	label := self showMsg.
	^className == nil
		ifTrue: [nil]
		ifFalse: [PopUpMenu
				labels: ('edit\spawn\remove\', label) withCRs
				values: #(editSelection spawnPlayList removeSelection showAll)]! !

!SoundBrowser methodsFor: 'class list'!
className: selection
	"Set the receiver's currently selected class to be selection and 
	update the message category list.  If this class no longer exists,
	print a message to that effect in the system transcript, if it is open."

	| name file |
	className := selection.
	selector := nil.
	protocol := nil.
	self newProtocolList: nil.
	selection == nil 
		ifFalse: [name := RootName, '/', category, '/', className.
			file := Filename named: name.
			file exists ifFalse: [^nil].
			file isDirectory
				ifFalse: [self openFile: name]]
		ifTrue: [self openFile: nil]! !


!SoundBrowser methodsFor: 'protocol list'!
protocol: selection
	"Set the receiver's currently selected class to be selection and 
	update the message category list.  If this class no longer exists,
	print a message to that effect in the system transcript, if it is open."

	| name file |
	protocol := selection.
	selector := nil.
	self newSelectorList: nil.
	selection == nil 
		ifFalse: [name := RootName, '/', category, '/', className, '/', protocol.
			file := Filename named: name.
			file exists ifFalse: [^nil].
			file isDirectory
				ifFalse: [self openFile: name]]! !

!SoundBrowser methodsFor: 'protocol list'!
protocolList
	"Answer the sequenceable collection containing the message categories that 
	the receiver accesses from the currently selected class."

	| name file list length |
	className == nil ifTrue: [^ nil].
	name := RootName, Filename separatorString, category, Filename separatorString, className.
	file := Filename named: name.
	file exists ifFalse: [^nil].
	file isDirectory ifFalse: [^nil].
	list := self sortFileNames: (Filename filesMatching: (name, Filename separatorString, '*')).
	length := name size + 2.
	^list collect:
			[ :item |
			item copyFrom: length to: item size]! !

!SoundBrowser methodsFor: 'protocol list'!
protocolMenu
	"Answer a Menu of operations on protocol to be 
	displayed when the operate menu button is pressed."

	| label |
	label := self showMsg.
	^protocol == nil
		ifTrue: [nil]
		ifFalse: [PopUpMenu
				labels: ('edit\spawn\remove\', label) withCRs
				values: #(editSelection spawnPlayList removeSelection showAll)]! !


!SoundBrowser methodsFor: 'selector list'!
selector: selection
	"Set the receiver's currently selected class to be selection and 
	update the message category list.  If this class no longer exists,
	print a message to that effect in the system transcript, if it is open."

	| name file |
	selector := selection.
	selection == nil 
		ifFalse: [name := RootName, '/', category, '/', className, '/', protocol, '/', selector.
			file := Filename named: name.
			file exists ifFalse: [^nil].
			file isDirectory
				ifFalse: [self openFile: name]]
		ifTrue: [soundView == nil ifFalse: [soundView sound: nil]]! !

!SoundBrowser methodsFor: 'selector list'!
selectorList
	"Answer the sequenceable collection containing the message selectors that 
	the receiver accesses via the currently selected class and message category."

	| name file list length |
	protocol == nil ifTrue: [^ nil].
	name := RootName, Filename separatorString, category, Filename separatorString, 
			className, Filename separatorString, protocol.
	file := Filename named: name.
	file exists ifFalse: [^nil].
	file isDirectory ifFalse: [^nil].
	list := self sortFileNames: (Filename filesMatching: (name, Filename separatorString, '*')).
	length := name size + 2.
	^list collect:
			[ :item |
			item copyFrom: length to: item size]! !

!SoundBrowser methodsFor: 'selector list'!
selectorMenu
	"Answer a Menu of operations on selector to be 
	displayed when the operate menu button is pressed."

	| label |
	label := self showMsg.
	^selector == nil
		ifTrue: [nil]
		ifFalse: [PopUpMenu
				labels: ('edit\spawn\remove\', label) withCRs
				values: #(editSelection spawnPlayList removeSelection showAll)]! !


!SoundBrowser methodsFor: 'text'!
text
	"Answer a text for the inspector view."

	^sound == nil
		ifTrue: ['']
		ifFalse: [sound printString]! !


!SoundBrowser methodsFor: 'edit functions'!
editSelection
	"Spawn an editor on the current selection"

	self soundName asFilename edit! !

!SoundBrowser methodsFor: 'edit functions'!
openFile: filename
	"Open the file given as the argument in the sound browser view"

	"InputState default shiftDown
		ifTrue: [TimeProfiler profile: [5 timesRepeat:
			[sound := SampledSound fromFile: filename.
			soundView sound: sound]]]
		ifFalse: [sound := SampledSound fromFile: filename.
	soundView sound: sound]"

	| file |
	filename == nil
		ifTrue: [soundView == nil
			ifFalse: [soundView sound: nil].
				^self].
	file := Filename named: filename.
	file exists ifFalse: [^self].
	file isDirectory ifTrue: [^self].
	(('*.snd' match: filename) | ('*.au' match: filename)
			| ('*.aiff' match: filename) | ('*.aifc' match: filename))
		ifTrue: [Cursor read showWhile:
				[sound := SampledSound fromFile: filename].
			Cursor wait showWhile:
				[soundView sound: sound]]! !

!SoundBrowser methodsFor: 'edit functions'!
removeSelection
	"Delete the current selection."

	(DialogView confirm: 
			('Are you sure you want to delete\  ', self soundName) withCRs)
		ifFalse: [^self].
	self soundName asFilename delete.
	self changed: #categoryList.
	self changed: #category.
	self changed: #protocol.
	self changed: #selector! !

!SoundBrowser methodsFor: 'edit functions'!
spawnPlayList
	"Spawn a play list view."

	| name file |
	name := self soundName.
	file := name asFilename.
	file exists ifFalse: [^self].
	file isDirectory
		ifFalse: [name := file directory asString].
	^PlayList openOnPattern: 
		(name, (String with: Filename separator), '*')! !


!SoundBrowser methodsFor: 'private'!
fileNamesForDirectory: name
	"Answer the sorted list of file names in the given directory; remove .env and .pk if showAll is false"

	| list |
	list := Filename filesMatching: (name, '/*').
	showAll
		ifFalse: [list := list select: [ :item | ('*.env' match: item) not].
				list := list select: [ :item | ('*.pk' match: item) not]].
	^self sortFileNames: list! !

!SoundBrowser methodsFor: 'private'!
showAll
	"Toggle the showAll instance variable; it controls whether envelope files are displayed."

	| cla pro sel |
	cla := className.
	pro := protocol.
	sel := selector.
	showAll := showAll not.
	self changed: #categoryList.
	self changed: #category.
	self newClassList: cla.
	self newProtocolList: pro.
	self newSelectorList: sel! !

!SoundBrowser methodsFor: 'private'!
showMsg
	"Answer the menu item label for show all."

	^showAll ifTrue: ['show sounds'] ifFalse: ['show all']! !

!SoundBrowser methodsFor: 'private'!
sortFileNames: aList
	"Sort the argument list placing directories at the start."

	| dirs files newList fn |
	dirs := (aList select: [ :file |
			fn := (FileStream named: file).
			fn exists and: [fn isDirectory]]) asSortedCollection.
	files := (aList select: [ :file | 
			fn := (FileStream named: file).
			fn exists and: [fn isDirectory not]]) asSortedCollection.
	newList := OrderedCollection new.
	dirs do: [ :dir | newList addLast: dir].
	files do: [ :file | newList addLast: file].
	^newList! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SoundBrowser class
	instanceVariableNames: ''!

!SoundBrowser class methodsFor: 'instance creation'!
open
	"Create and schedule a browser on a collection of organized Sounds. "
	"SoundBrowser open"

	SoundBrowserView openOn: (SoundBrowser new initialize)! !


!SoundBrowser class methodsFor: 'class instance variable access'!
rootName
	"Answer the receiver class's root file directory name."
	"SoundBrowser rootName"

	^RootName! !


!SoundBrowser class methodsFor: 'class initialization' stamp: 'stp 08/17/1999 21:51'!
initialize
	"SoundBrowser initialize"

	RootName := FillInTheBlank request: 'Where are your sound files?' initialAnswer: 'snd'.
	RootName = '' ifTrue: [^nil].
	SoundLibrary := FileDirectory default fileNamesMatching:
		(RootName, (String with: FileDirectory pathNameDelimiter),  '*').! !


Browser subclass: #SoundBrowserView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-Sound'!
!SoundBrowserView commentStamp: '<historical>' prior: 0!
The SoundBrowserView is a new BrowserView with special relevance to the SoundManager.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SoundBrowserView class
	instanceVariableNames: ''!

!SoundBrowserView class methodsFor: 'instance creation'!
open
	"Create and schedule a browser on a collection of organized Sounds. "
	"SoundBrowserView open"

	SoundBrowserView openOn: (SoundBrowser new initialize)! !

!SoundBrowserView class methodsFor: 'instance creation'!
open4ListsOn: aBrowser
	"Create and schedule a sound browser on a hierarchy of sound file directories."
	"SoundBrowserView open4ListsOn: (SoundBrowser new)"

	| topView divider topWindow height x1 x2 x3 soundWrapper soundView |
	height := 320.
	topWindow := (ScheduledWindow model: aBrowser 
			label: ('Sound Browser on ', aBrowser class rootName)
			minimumSize: 600@height)
			maximumSize: 1100@height*2.
	divider := 0.45.
	x1 := 0.2.
	x2 := 0.4.
	x3 := 0.7.
	topView := ViewWithResizableSubviews new.
	self addCategoryViewTo: topView in: (0@0 extent: x1@divider) 
		on: aBrowser readOnly: false;
		addClassViewTo: topView in: (LayoutFrame new 
				leftFraction: x1;
				topOffset: 0;
				rightFraction: x2;
				bottomFraction: divider) on: aBrowser readOnly: false;
		addProtocolViewTo: topView in: (x2@0 extent: (x3 - x2)@divider) 
			on: aBrowser readOnly: false;
		addSelectorViewTo: topView in: (LayoutFrame new 
				leftFraction: x3;
				topFraction: 0.0;
				rightFraction: 1.0;
				bottomFraction: divider)
			on: aBrowser readOnly: false.
"Sound View"
	soundWrapper := ViewWithResizableSubviews new.
	soundView := SoundView 
				soundViewOn: (SampledSound default) 
				height: (height * (1 - divider)) truncated
				in: soundWrapper.
	topView
		add: soundWrapper
		in: (0@divider corner: 1@1).
	aBrowser soundView: soundView.
"Open"
	topWindow component: topView.
	topWindow icon: (Icon constantNamed: #systemBrowser).
	topWindow open! !

!SoundBrowserView class methodsFor: 'instance creation'!
openOn: aBrowser
	"Create and schedule a sound browser on a hierarchy of sound file directories."
	"SoundBrowserView openOn: (SoundBrowser new)"

	| topView soundView soundWrapper divider topWindow height x1 x2 x3 editor |
	height := 320.
	topWindow := (ScheduledWindow model: aBrowser 
			label: ('Sound Browser on ', aBrowser class rootName)
			minimumSize: 700@height)
			maximumSize: 1100@height*2.
	divider := 0.35.
	x1 := 0.25.
	x2 := 0.5.
	x3 := -160.
	topView := ViewWithResizableSubviews new.
"List Views"
	self addCategoryViewTo: topView in: (0@0 extent: x1@divider) 
				on: aBrowser readOnly: false;
		addClassViewTo: topView in: (x1@0 extent: (x2 - x1)@divider) 
				on: aBrowser readOnly: false;
		addProtocolViewTo: topView in: (LayoutFrame new 
				leftFraction: x2; topFraction: 0; 
				rightFraction: 1 offset: x3; bottomFraction: divider) 
			on: aBrowser readOnly: false.
"Sound View"
	soundView := SoundView new initialize.
	editor := SoundEditor new sound: nil.
	soundView model: editor.
	editor view: soundView.
	soundView height: 240.
	soundWrapper := LookPreferences edgeDecorator on: soundView.
	soundWrapper useVerticalScrollBar; useHorizontalScrollBar; noMenuBar.
	topView add: soundWrapper
			in: (LayoutFrame new
				leftFraction: 0; topFraction: divider;
				rightFraction: 1 offset: x3;  bottomFraction: 1).
	SoundView addButtonsOn: soundView for: editor to: topView 
			in: (LayoutFrame new
				leftFraction: 1 offset: x3; topFraction: 0;
				rightFraction: 1;  bottomFraction: 1).
	aBrowser soundView: soundView.

"Open"
	topWindow component: topView.
	topWindow icon: (Icon constantNamed: #systemBrowser).
	topWindow open! !


Model subclass: #SoundEditor
	instanceVariableNames: 'copyBuffer sound markers selection1 selection2 lastSelection buffer view changed mode '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MusicOLD-Sound'!

!SoundEditor methodsFor: 'accessing'!
displayList
	^nil! !

!SoundEditor methodsFor: 'accessing'!
list
	^nil! !

!SoundEditor methodsFor: 'accessing'!
mark: aSampleNumber
	"Set the receiver's model"

	markers == nil ifTrue: [markers := Dictionary new].
	markers at: aSampleNumber printString asSymbol put: aSampleNumber! !

!SoundEditor methodsFor: 'accessing'!
markChanged: aBoolean
	"Set the flag to tell the receiver that the model's been changed."

	changed := aBoolean! !

!SoundEditor methodsFor: 'accessing'!
markers
	"Answer the receiver's markers"

	^markers! !

!SoundEditor methodsFor: 'accessing'!
markers: aCollection
	"Set the receiver's marker collection."

	markers := aCollection! !

!SoundEditor methodsFor: 'accessing'!
mode
	"Answer the receiver's mode (#envelope or #samples)."

	^mode! !

!SoundEditor methodsFor: 'accessing'!
mode: aSymbol
	"Set the receiver's mode to #envelope or #samples."

	mode := aSymbol! !

!SoundEditor methodsFor: 'accessing'!
selection1
	"Answer the receiver's primary selection."

	^selection1! !

!SoundEditor methodsFor: 'accessing'!
selection1: anInteger
	"Set the receiver's primary selection."

	selection1 := anInteger! !

!SoundEditor methodsFor: 'accessing'!
selection2
	"Answer the receiver's secondary selection."

	^selection2! !

!SoundEditor methodsFor: 'accessing'!
selection2: anInteger
	"Set the receiver's secondary selection."

	selection2 := anInteger! !

!SoundEditor methodsFor: 'accessing'!
setModel: aModel
	"Set the receiver's model."

	self reinitialize.
	super setModel: aModel.
	self changed: #text! !

!SoundEditor methodsFor: 'accessing'!
size
	^sound size! !

!SoundEditor methodsFor: 'accessing'!
sound
	"Answer the receiver's model"

	^sound! !

!SoundEditor methodsFor: 'accessing'!
sound: aSound
	"Set the receiver's model to be the given sound"

	sound == nil ifFalse: [sound release].
	self reinitialize.
	sound := aSound.
	sound == nil ifTrue: [^self changed].
	(mode == #peak)
		ifTrue: [(sound readEnvelope: #peak)
			ifFalse: [sound computePeakEnvelope]].
	(mode == #env)
		ifTrue: [(sound readEnvelope: #RMS)
			ifFalse: [sound computeRMSEnvelope]].
	sound cueList == nil
		ifFalse: [sound cueList do:
			[ :q |
			markers add: q value first.
			markers add: q value last]].
	self changed! !

!SoundEditor methodsFor: 'accessing'!
view: aView
	"Set the receiver's view to be the given view"

	view := aView! !


!SoundEditor methodsFor: 'initialize release'!
initialize
	"Set up the proper default values for the receiver."

	markers := OrderedCollection new.
	mode := #samples.
	changed := false! !

!SoundEditor methodsFor: 'initialize release'!
reinitialize
	"Set up the proper default values for the receiver."

	markers := OrderedCollection new.
	sound := selection1 := selection2 := lastSelection := copyBuffer := nil.
	changed := false! !


!SoundEditor methodsFor: 'selection'!
select: cursorX
	"Take a mouse point and place a marker on the sound sample there."

	|selection |
	selection := (view sampleForX: cursorX).
	view showMarkerAt: cursorX.
"	Transcript show: cursorX printString, ' --> ', selection printString; cr."
	markers add: selection.
	selection1 == nil
		ifTrue: [selection1 := selection]
		ifFalse: [selection2 := selection]! !

!SoundEditor methodsFor: 'selection'!
selectCue
	"Select a named cue region in the model's sound."

	| cues choice array theQ |
	cues := sound cueList.
	(cues == nil) | (cues isEmpty)
		ifTrue: [^self].
	array := Array new: cues size.
	1 to: array size do:
		[ :ind |
		array at: ind put: (cues at: ind) key asString].
	choice := (PopUpMenu labelArray: array) startUp.
	choice = 0 ifTrue: [^self].
	theQ := cues at: choice.
	selection1 := theQ value first.
	selection2 := theQ value last.
	view displaySelection! !

!SoundEditor methodsFor: 'selection'!
selectedSound
	"Answer a sound from the receiver view's selection."

	| s |
	((selection1 == nil) or: [selection2 == nil])
		ifTrue: [^nil].
	Cursor wait showWhile:
		[s := sound fromSample: selection1 toSample: selection2].
	^s! !

!SoundEditor methodsFor: 'selection'!
setCue
	"Select the current selection as a cue region in the model's sound."

	| name |
	((selection1 == nil) | (selection2 == nil))
		ifTrue: [^self].
	name := DialogView request: 'Name of cue?' initialAnswer: 'cue1'.
	name = '' ifTrue: [^self].
	Transcript show: 'Adding cue named ', name, ' from ', selection1 printString, 
					' to ', selection2 printString, '.'; cr.
	sound addCueNamed: name from: selection1 to: selection2! !

!SoundEditor methodsFor: 'selection'!
setSelection: aMarker
	"Set the primary selection to be the given marker."

	| temp |
	lastSelection = selection2
		ifFalse: [selection2 := aMarker.
				lastSelection := aMarker]
		ifTrue: [selection1 := aMarker.
				lastSelection := aMarker].
	((selection1 == nil) or: [selection2 == nil])
		ifTrue: [^self].
	selection1 > selection2
		ifTrue: [temp := selection1.
			selection1 := selection2.
			selection2 := temp].
	view displaySelection: selection1 and: selection2! !


!SoundEditor methodsFor: 'interaction'!
comment: ignored
	"Add a comment to a sound file."

	| name comment fileName|
	fileName := sound name.
	fileName == nil ifTrue: [^self].
	((fileName findString: '.snd' startingAt: 1) == 0)
		ifTrue: [^self].
	name := fileName copyUpTo: $ .
	comment := DialogView 
			request: '                    Sound file comment                         ' 
			initialAnswer: ''.
	comment = '' ifTrue: [^self].
"	Transcript show:  'sfcomment ', '"', comment, '" ', name; cr."
	UnixProcess spawn: 'sfcomment ', '"', comment, '" ', name.! !

!SoundEditor methodsFor: 'interaction'!
copy
	"Copy the current selection into the temporary copy buffer."

	| selSnd |
	selSnd := self selectedSound.
	selSnd == nil
		ifFalse: [copyBuffer := selSnd]! !

!SoundEditor methodsFor: 'interaction'!
cut
	"Cut out the current selection."

	| newSound |
	newSound := sound cutFrom: (selection1 min: selection2) to: (selection1 max: selection2).
	newSound class == sound class
		ifFalse: [self sound: newSound].
	self changed.! !

!SoundEditor methodsFor: 'interaction'!
file
	"Save the receiver's sound to a file."

	| name which |
	((copyBuffer == nil) | ((selection1 ~~ nil)  & (selection2 ~~ nil)))
		ifTrue: [which := DialogView	
			choose: 'Which sound do you want to save?'
			labels: #('model sound' 'copy buffer' 'current selection')
			values: #(1 2 3)
			default: 1].
	name := sound name.
	(SoundFile isSoundFileName: name)
		ifFalse: [name := name, '.snd'].
	name := DialogView
		request: 'Save sound to:' 
		initialAnswer: name.
	name = '' ifTrue: [^nil].
	Cursor write showWhile:
		[((1 -> [Transcript show: 'Saving model to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: sound]),
		 (2 -> [Transcript show: 'Saving copy buffer to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: copyBuffer]),
		 (3 -> [Transcript show: 'Saving selection to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: self selectedSound])) case: which]! !

!SoundEditor methodsFor: 'interaction'!
file: ignored
	"Save the receiver's sound to a file."

	| name which |
	((copyBuffer == nil) | ((selection1 ~~ nil)  & (selection2 ~~ nil)))
		ifTrue: [which := DialogView	
			choose: 'Which sound do you want to save?'
			labels: #('model sound' 'copy buffer' 'current selection')
			values: #(1 2 3)
			default: 1].
	name := sound name.
	(SoundFile isSoundFileName: name)
		ifFalse: [name := name, '.snd'].
	name := DialogView
		request: 'Save sound to:' 
		initialAnswer: name.
	name = '' ifTrue: [^nil].
	Cursor write showWhile:
		[((1 -> [Transcript show: 'Saving model to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: sound]),
		 (2 -> [Transcript show: 'Saving copy buffer to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: copyBuffer]),
		 (3 -> [Transcript show: 'Saving selection to file ', name, '.'; cr.
			(SoundFile named: name) saveSound: self selectedSound])) case: which]! !

!SoundEditor methodsFor: 'interaction' stamp: 'stp 12/05/1999 06:57'!
inspect: ignored
	"Inspect the receiver or its sound."

	InputSensor default leftShiftDown
		ifFalse: [self sound inspect]
		ifTrue: [self inspect]! !

!SoundEditor methodsFor: 'interaction'!
newCopy
	"Make a fresh copy of the receiver's model's sound."

	self setModel: sound copy! !

!SoundEditor methodsFor: 'interaction'!
paste
	"Paste the current selection into the model at the primary selection."

	| newSound |
	newSound := sound paste: copyBuffer from: 1 to: copyBuffer size at: selection1.
	newSound class == sound class
		ifFalse: [self sound: newSound].
	self changed.! !

!SoundEditor methodsFor: 'interaction'!
play: ignored
	"Play the receiver's sound"

	((selection1 ~~ nil) and: [selection2 ~~ nil])
		ifTrue: [SoundPort default playSamples: sound samples
				rate: sound rate chans: sound channels 
				from: (sound mapSampleIndex: selection1) * sound sampleSize - 1
				to: (sound mapSampleIndex: selection2) * sound sampleSize - 1]
		ifFalse: [changed 
			ifTrue: [SoundPort default playSamples: sound samples
					rate: sound rate chans: sound channels 
					from: 1
					to: sound samples sizeInBytes]
			ifFalse: [sound play]]! !

!SoundEditor methodsFor: 'interaction'!
setEnvMode
	"Set the mode of the editor to be RMS envelope display."

	self setMode: #env! !

!SoundEditor methodsFor: 'interaction'!
setMode: newMode
	"Set the sample/envelope/peak mode of the editor."

	mode := newMode.
	(mode == #peak)
		ifTrue: [(sound readEnvelope: #peak)
			ifFalse: [sound computePeakEnvelope]].
	(mode == #env)
		ifTrue: [(sound readEnvelope: #RMS)
			ifFalse: [sound computeRMSEnvelope]].
	self changed: #mode with: mode.! !

!SoundEditor methodsFor: 'interaction'!
setPeakMode
	"Set the mode of the editor to be peak display."

	self setMode: #peak! !

!SoundEditor methodsFor: 'interaction'!
setSampleMode
	"Set the mode of the editor to be samples."

	self setMode: #samples! !

!SoundEditor methodsFor: 'interaction'!
spawn
	"Open a soundfile editor."

	SoundView openOn: sound! !

!SoundEditor methodsFor: 'interaction'!
toggleMode: ignored
	"Toggle the sample/envelope mode of the editor."
self safeHalt.
	((#samples -> [mode := #env.
				(sound readEnvelope: #RMS)
					ifFalse: [sound computeRMSEnvelope]]),
	 (#env -> [mode := #peak.
				(sound readEnvelope: #peak)
					ifFalse: [sound computePeakEnvelope]]),
	 (#peak -> [mode := #samples]))
			case: mode.
	self changed: #mode with: mode.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SoundEditor class
	instanceVariableNames: ''!

!SoundEditor class methodsFor: 'instance creation'!
new
	"Answer an initialized instance."

	^super new initialize! !


DisplayListView subclass: #SoundView
	instanceVariableNames: 'height envButton alternateColor '
	classVariableNames: 'SelectionGlyph '
	poolDictionaries: ''
	category: 'MusicOLD-Sound'!
!SoundView commentStamp: '<historical>' prior: 0!
Instances of SoundView are used for displaying samples sound objects in R4.
They can do horizontal scrolling and scaling.

Instance variables:
	displayContents	<GraphicsContext> the cached display
	marker1, 2		<Number> the offset into the sound in samples for 2 markers
	step				<Number> the step size for displaying--show every n'th sample
	scale			<Number> the vertical scale for multiplying the sample's value
!


!SoundView methodsFor: 'initialize-release'!
initialize
 
	super initialize.
	scale := 2 @ 100.
	height := 240.
	Display depth = 1
		ifTrue: [alternateColor := Color gray]
		ifFalse: [alternateColor := Color red].
	^self! !


!SoundView methodsFor: 'accessing' stamp: 'stp 11/30/1998 14:46'!
dataExtent
	"Answer the extent of the receiver's display object bounding box."

	Transcript show: 'DataExtent ', (self model size / scale x) printString; cr.
	^(self model size / scale x) @ 0! !

!SoundView methodsFor: 'accessing'!
envButton: aView
	"Set the receiver's envelope toggle button."

	envButton := aView! !

!SoundView methodsFor: 'accessing'!
height
	"Answer the receiver's pixmap height."

	^height! !

!SoundView methodsFor: 'accessing'!
height: aNumber
	"Set the receiver's pixmap height."

	height := aNumber! !

!SoundView methodsFor: 'accessing' stamp: 'stp 11/30/1998 14:47'!
preferredBounds
	"Answer the extent of the receiver's display object bounding box."

"	Transcript show: (self model size / scale x) printString; cr."
	^0@0 extent: ((self model size / scale x) @ 240)! !

!SoundView methodsFor: 'accessing'!
sound
	"Answer the model's sound."

	^model sound! !

!SoundView methodsFor: 'accessing'!
sound: aSound
	"Set the model's sound."

	model sound: aSound.
"	Transcript show: 'width: ', width printString; cr."
	self changedPreferredBounds: nil.
	self invalidate.
	self changed: #text.! !


!SoundView methodsFor: 'controller access'!
defaultControllerClass
	"Answer the default controller class for use with the receiver."

	^SoundController! !


!SoundView methodsFor: 'selection'!
displayMarkersOn: aGC in: bRect
	"Show the cue markers of the model on the receiver."

	| cues x |
	model == nil ifTrue: [^self].
	cues := model markers.
	cues == nil ifTrue: [^self].
	cues do: 
		[ :q |
		x := self xForSample: q.
		((x >= bRect left) & (x <= bRect right))
			ifTrue: [self showMarkerOn: aGC at: x in: bRect]]! !

!SoundView methodsFor: 'selection' stamp: 'stp 11/30/1998 14:46'!
displaySelection
	"Scale and scroll the view as necessary to display the current selection."

	| x1 x2 width |
	model == nil ifTrue: [^self].
	x1 := model selection1.
	x2 := model selection2.
	(x1 == nil) | (x2 == nil)
		ifTrue: [^self].
	width := self graphicsContext clippingBounds width asFloat.
	(x2 - x1) > width
		ifTrue: [self scaleX: (((x2 - x1) * 1.1) / width).
			self scrollTo: x1 - ((x2 - x1) * 0.95)]! !

!SoundView methodsFor: 'selection'!
displaySelection: s1 and: s2
	"Show the selections (up to 2 markers) in the current sound"

	| pGC |
	model == nil ifTrue: [^self].
	pGC := self graphicsContext.
	self displaySelection: s1 and: s2 on: pGC! !

!SoundView methodsFor: 'selection'!
displaySelection: s1 and: s2 on: pGC
	"Show the selections (up to 2 markers) in the current sound"

	| box |
	box := pGC clippingBounds.
	pGC paint: self backgroundColor.
	pGC displayRectangle: ((0@0) extent: (box width @ 16)).
	pGC paint: self foregroundColor.
	(s1 ~~ nil) & (model markers includes: s1)
		ifTrue: [Cursor caret displayOn: pGC at: (((self xForSample: s1)- 8)@6)].
	(s2 ~~ nil) & (model markers includes: s2)
		ifTrue: [Cursor caret displayOn: pGC at: (((self xForSample: s2) - 8)@6)]! !

!SoundView methodsFor: 'selection'!
eraseMarkerOn: pGC at: xValue in: box
	"Take a mouse point and erase the marker on the sound sample there"

	| p1 p2 |
	pGC paint: self backgroundColor.
	p1 := xValue @ box top.
	p2 := xValue @ box bottom.
	pGC displayLineFrom: p1 to: p2! !

!SoundView methodsFor: 'selection'!
showMarkerAt: xValue
	"Take a mouse point and place a marker on the sound sample there"

	| pGC box |
	pGC := self graphicsContext.
	box := pGC clippingBounds.
	self showMarkerOn: pGC at: xValue in: box! !

!SoundView methodsFor: 'selection'!
showMarkerOn: pGC at: xValue in: box
	"Take a mouse point and place a marker on the sound sample there"

	| p1 p2 pbox |
	box == nil
		ifTrue: [pbox := pGC clippingBounds]
		ifFalse: [pbox := box].
	pGC paint: self foregroundColor.
	p1 := xValue @ pbox top.
	p2 := xValue @ pbox bottom.
"	Transcript show: 'M: ', xValue printString,
				' (', pbox left printString, ' ', pbox right printString, ')'; cr."
	pGC displayLineFrom: p1 to: p2! !


!SoundView methodsFor: 'converting indeces' stamp: 'stp 11/30/1998 14:47'!
sampleForX: xValue
	"Answer the sample under the point with x = xValue."

	^(xValue - scrollOffset value x * scale x)! !

!SoundView methodsFor: 'converting indeces' stamp: 'stp 11/30/1998 14:47'!
xForSample: sample
	"Answer the xValue of the sample with the given index."

	^(sample / scale x + scrollOffset value x)! !


!SoundView methodsFor: 'displaying'!
display: box onForm: display at: offset
	"Display the receiver's display list on the cached form."

	self sound == nil ifTrue: [^nil].
	model mode == #samples
		ifTrue: [self sound channels = 1
				ifTrue: [self displayMonoSamplesOn: display in: box]
				ifFalse: [self displayStereoSamplesOnGC: display]]
		ifFalse: [self sound channels = 1
				ifTrue: [self displayMonoEnvelopeOnGC: display]
				ifFalse: [self displayStereoEnvelopeOnGC: display]].
"	self displayMarkersOn: display in: box.
	self displaySelection: model selection1
		and: model selection2
		on: display
"! !

!SoundView methodsFor: 'displaying' stamp: 'stp 11/30/1998 14:47'!
displayMonoEnvelopeOnGC: aGC
	"Show the receiver's envelope on the GC."

	| sound env box centerY scaledYScale setPoint value index oldPt ssize pt |
	model == nil ifTrue: [^self].
	self sound == nil ifTrue: [^self].
	box := aGC clippingBounds.
	aGC paint: DisplayListView foregroundColor.
	sound := self sound.
	setPoint := box left.
	scaledYScale := scale y asFloat / -500.0.
	centerY := height - 32.
	env := sound envelope.
	ssize := env size.
	oldPt := setPoint @ centerY.
	(box left + 1) to: box right do:
		[ :counter |
		index := counter.
		index > ssize ifTrue: [^self]. 
		value := ((env at: index) * scaledYScale) rounded.
		value := ((value + centerY) min: height - 1) max: 0.
		pt := (setPoint + counter) @ (value asInteger).
		aGC displayLineFrom: oldPt to: pt.
		oldPt := pt.
		((counter \\ 3) = 0)
				ifTrue: [pt := (setPoint + counter) @ centerY.
						aGC displayLineFrom: pt to: pt]]! !

!SoundView methodsFor: 'displaying' stamp: 'stgp 11/24/1998 12:55'!
displayMonoSamplesOn: display in: box
	"Show the receiver's sound on the display."

	| sound centerY scaledYScale xScale value index oldPt ssize pt dot left blt |
	model ifNil: [^self].
	self sound ifNil: [^self].
	dot := Form extent: 1 @ 1.
	dot fillBlack.
	blt := BitBlt destForm: display		sourceForm: dot
		fillColor: Color black			combinationRule: Form paint
		destOrigin: 0@0				sourceOrigin: 0 @ 0
		extent: Display extent			clipRect: box.
	sound := self sound.
	scaledYScale := (sound maxSample asFloat / scale y * zoom y) negated.
	centerY := box center y truncated.
	xScale := scale x asFloat * zoom x.
	blt drawFrom: (box left @ centerY) to: (box right @ centerY).
	ssize := sound size / sound sampleSize.
	oldPt := nil.
	left := box left.
"	Transcript show: scale printString, ' -- ', zoom printString; cr.
	Transcript show: 'b: ', (box left + 1) printString, ' ', box right printString, 
				' x ', scale x printString; cr.
"	value := ((sound intSampleAt: 1) asFloat / scaledYScale + centerY) truncated.
	oldPt := left @ value.
	2 to: box width do:
		[ :counter |
		index := (counter * xScale) truncated.
		index > ssize ifTrue: [^self]. 
		index < 1 ifTrue: [index := 1].
		value := ((sound intSampleAt: index) asFloat / scaledYScale + centerY) truncated.
		pt := (left + counter - 1) @ value.
		blt drawFrom: oldPt to: pt.
		oldPt := pt.
"		((counter \\ 3) = 0)
				ifTrue: [pt := counter @ centerY.
					display drawLine: dot from: pt to: pt clippingBox: box 
						rule: Form paint fillColor: nil]"]! !

!SoundView methodsFor: 'displaying' stamp: 'stp 11/30/1998 14:46'!
displayMonoSamplesOnGC: aGC
	"Show the receiver's sound on the GC."

	| sound box centerY scaledYScale value index oldPt ssize pt |
	model == nil ifTrue: [^self].
	self sound == nil ifTrue: [^self].
	box := aGC clippingBounds.
	aGC paint: DisplayListView foregroundColor.
	sound := self sound.
	scaledYScale := SampledSound maxSample / scale y.
	centerY := height / 2.
	ssize := sound size.
	oldPt := nil.
"	Transcript show: 'b: ', (box left + 1) printString, ' ', box right printString, 
				' x ', scale x printString; cr."
	(box left + 1) to: box right do:
		[ :counter |
		index := counter * scale x.
		index > ssize ifTrue: [^self]. 
		value := (sound intSampleAt: index truncated) / scaledYScale negated.
		value := ((value + centerY) min: height - 1) max: 0.
		pt := (counter - 1) @ (value asInteger).
		oldPt == nil ifFalse: [aGC displayLineFrom: oldPt to: pt].
		oldPt := pt.
		((counter \\ 3) = 0)
				ifTrue: [pt := counter @ centerY.
						aGC displayLineFrom: pt to: pt]]! !

!SoundView methodsFor: 'displaying'!
displayOn: aGC
	"Show the receiver's sound on the GC."

	self sound == nil ifTrue: [^nil].
	aGC clear.
	model mode == #samples
		ifTrue: [(self sound channels = 1)
			ifTrue: [self displayMonoSamplesOnGC: aGC]
			ifFalse: [self displayStereoSamplesOnGC: aGC]]
		ifFalse: [(self sound channels = 1)
			ifTrue: [self displayMonoEnvelopeOnGC: aGC]
			ifFalse: [self displayStereoEnvelopeOnGC: aGC]].
	self displayMarkersOn: aGC in: aGC clippingBounds.
	self displaySelection: model selection1 and: model selection2 on: aGC! !

!SoundView methodsFor: 'displaying' stamp: 'stp 11/30/1998 14:47'!
displayStereoEnvelopeOnGC: aGC
	"Show the receiver's envelope on the GC."

	| sound box centerY scaledScale setPoint value index oldPt ssize pt |
	model == nil ifTrue: [^self].
	self sound == nil ifTrue: [^self].
	box := aGC clippingBounds.
	aGC paint: DisplayListView foregroundColor.
	sound := self sound.
	setPoint := box left.
	model mode == #env
		ifTrue: [scaledScale := scale y asFloat / -1000.0]
		ifFalse: [scaledScale := scale y asFloat / -15000.0].
	centerY := height - 32.
	ssize := sound envelope size.
	oldPt := setPoint @ centerY.
	(box left + 1) to: box right do:
		[ :counter |
		index := counter.
		index > ssize ifTrue: [^self]. 
		value := ((sound envelope at: index) * scaledScale) rounded.
		value := ((value + centerY) min: height - 1) max: 0.
		pt := counter @ (value asInteger).
		aGC displayLineFrom: oldPt to: pt.
		oldPt := pt.
		((counter \\ 3) = 0)
				ifTrue: [pt := (setPoint + counter) @ centerY.
						aGC displayLineFrom: pt to: pt]]! !

!SoundView methodsFor: 'displaying' stamp: 'stp 11/30/1998 14:47'!
displayStereoSamplesOnGC: aGC
	"Show the receiver's sound on the GC."

	| sound box centerY scaledYScale value index oldLPt oldRPt ssize color1 color2 pt |
	model == nil ifTrue: [^self].
	self sound == nil ifTrue: [^self].
	box := aGC clippingBounds.
	sound := self sound.
	scaledYScale := sound class maxSample asFloat / scale y.
	centerY := height / 2.
	ssize := sound size.
	oldLPt := box left @ centerY.
	oldRPt := box left @ centerY.
	color1 := DisplayListView foregroundColor.
	color2 := alternateColor.
	(box left + 1) to: box right by: 2 do:
		[ :counter |
		aGC paint: color1.		"left channel"
		index := (counter * scale x) truncated.
		index > ssize ifTrue: [^self]. 
		value := (sound intSampleAt: index) // scaledYScale negated.
		value := ((value + centerY) min: height - 1) max: 0.
		pt := counter @ (value asInteger).
		aGC displayLineFrom: oldLPt to: pt.
		oldLPt := pt.
		aGC paint: color2.		"right channel"
		index := index + 1.
		index > ssize ifTrue: [^self]. 
		value := (sound sampleAt: index) // scaledYScale negated.
		value := ((value + centerY) min: height - 1) max: 0.
		pt :=counter @ (value asInteger).
		aGC displayLineFrom: oldRPt to: pt.
		oldRPt := pt.
		((counter \\ 6) = 0)
				ifTrue: [pt := counter @ centerY.
						aGC displayLineFrom: pt to: pt]]! !

!SoundView methodsFor: 'displaying' stamp: 'stgp 11/24/1998 12:56'!
displayView
	"Display the receiver's background form and display list *directly* on the Display."
	"SoundView openOn: (StoredSound fromFile: 'unbelichtet.snd')"
 
	| box |
	box := self insetDisplayBox.
	Display fill: box fillColor: self backgroundColor.
	background ifNotNil: [background displayOn: Display].

	self display: box onForm: Display at: 0@0

"	Transcript show: (Time millisecondsToRun:
		[self display: box onForm: Display at: 0@0]) printString; cr..
"
"	MessageTally spyOn:
		[10 timesRepeat: [self display: box onForm: Display at: 0@0]]
"
"	self displaySelectionOn: Display at: 0@0 clippingBox: box"! !

!SoundView methodsFor: 'displaying'!
redraw
	"Redisplay the receiver's sound"

	Cursor wait showWhile:
		[self invalidate]! !

!SoundView methodsFor: 'displaying'!
text
	"Answer the default display text for the receiver's sound."

	| stream snd sName |
	stream := WriteStream on: (String new: 64).
	snd := self sound.
	snd == nil ifTrue: [^''].
	sName := snd name.
	sName size > (SoundBrowser rootName size + 2)
		ifTrue: [stream nextPutAll: 'name: ', sName asFilename tail asString; cr].
	snd duration == nil
		ifFalse: [stream nextPutAll: 'duration: ', 
						((self sound duration asSec value printString) clipTo: 7); cr].
	stream nextPutAll: 'sample rate: ', snd rate printString, ' Hz'; cr.
	stream nextPutAll: 'format: ', snd longFormat; cr.
	stream nextPutAll:  'channels: ', snd channels printString; cr.
	stream nextPutAll: 'samples: ', snd size rounded printString; cr.
	((snd at: #comment) == nil)
		ifFalse: [stream nextPutAll: 'comment: ', (snd at: #comment) printString; cr].
	((snd cueList) == nil)
		ifFalse: [stream nextPutAll: 'cues:	'.
			snd cueList do: 
				[ :q |stream nextPutAll: q printString; crtab]].
	^stream contents! !


!SoundView methodsFor: 'updating'!
update: aspect with: newValue
	"Toggle the sample/env/peak button on mode change."

	| label |
	aspect == #mode
		ifTrue: [label := newValue asString.
			label at: 1 put: label first asUppercase.
			envButton label: label; invalidate.
	"		newValue == #env ifTrue: [scaleY := scaleY / 50].
			newValue == #peak ifTrue: [scaleY := scaleY / 2].
			newValue == #samples ifTrue: [scaleY := scaleY * 20]"].
	self displayView! !


!SoundView methodsFor: 'zooming'!
zoomIn
	"Zoom in by 25%."

	Sensor leftShiftDown
		ifTrue: [zoom := zoom * (1 @ 0.75)]
		ifFalse: [zoom := zoom / (0.75 @ 1)].
	self displayView! !

!SoundView methodsFor: 'zooming'!
zoomOut
	"Zoom out by 25%."

	Sensor leftShiftDown
		ifTrue: [zoom := zoom / (1 @ 0.75)]
		ifFalse: [zoom := zoom * (0.75 @ 1)].
	self displayView! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

SoundView class
	instanceVariableNames: ''!

!SoundView class methodsFor: 'instance creation'!
addButtonsOn: aView for: anEditor to: aPart in: rect
	"Take the sound as model and open a SoundView"
	"SoundBrowser open"
	"SoundView openOn: SampledSound sawtooth"

	| x1 x2 inspView |
	x1 := rect leftOffset.
	x2 := x1 / 2.
"Play/Stop Button"
	self addButtonNamed: 'Play'  on: anEditor
			sending: #play to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x1; topFraction: 0;
				rightFraction: 1 offset: x2; bottomFraction: 0 offset: 24).
"File Button"
	self addButtonNamed: 'File'  on: anEditor
			sending: #file to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x2; topFraction: 0;
				rightFraction: 1; bottomFraction: 0 offset: 24).
"Inspect Button"
	self addButtonNamed: 'Inspect'  on: anEditor
			sending: #inspect to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x1; topFraction: 0 offset: 24;
				rightFraction: 1 offset: x2; bottomFraction: 0 offset: 48).
"Env Toggle Button--store it for toggling"
	aView envButton: (self addButtonNamed: 'Samples'  on: anEditor
			sending: #toggleMode to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x2; topFraction: 0 offset: 24;
				rightFraction: 1; bottomFraction: 0 offset: 48)).	"48 pixels"
"Comment Button"
	self addButtonNamed: 'Comment'  on: anEditor
			sending: #comment to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x1; topFraction: 0 offset: 48;
				rightFraction: 1 offset: x2; bottomFraction: 0 offset: 72).
"Clear Button"
	self addButtonNamed: 'Clear'  on: aView controller
			sending: #clear to: aPart in: (LayoutFrame new
				leftFraction: 1 offset: x2; topFraction: 0 offset: 48;
				rightFraction: 1; bottomFraction: 0 offset: 72).
"step slider"
	self addSliderOn: aView sending: #scaleX range: (1 to: 100) axis: #beHorizontal 
			marker: [Rectangle origin: 0@0 extent: 4@2 * (0@1) transpose + (4@2)]
			in: (LayoutFrame new
					leftFraction: 1 offset: x1; topFraction: 0 offset: 72;
					rightFraction: 1; bottomFraction: 0 offset: 88) 
			to: aPart.
"scale slider"
	self addSliderOn: aView sending: #scaleY range: (1 to: 400) axis: #beHorizontal 
			marker: [Rectangle origin: 0@0 extent: 2@4 * (1@0) transpose + (2@4)]
			in: (LayoutFrame new
					leftFraction: 1 offset: x1; topFraction: 0 offset: 88;
					rightFraction: 1; bottomFraction: 0 offset: 104) 
			to: aPart.

"InspectorView"
	inspView := TextView
				on: aView aspect: #text change: #acceptText:from:
				menu: #textMenu initialSelection: nil.
	inspView textStyle: (TextAttributes styleNamed: #small).
	inspView controller: NoController new.
	aPart add: ((LookPreferences edgeDecorator on: inspView) noMenuBar)
				in: (LayoutFrame new
					leftFraction: 1 offset: x1; topFraction: 0 offset: 104;
					rightFraction: 1; bottomFraction: 1)! !

!SoundView class methodsFor: 'instance creation' stamp: 'stp 10/04/1999 22:43'!
newMorphOn: snd
	"Open a sound view."

	| me ed |
	me := self new initialize.
	ed := SoundEditor new sound: snd.
	me model: ed.
	ed view: me.
	^me! !

!SoundView class methodsFor: 'instance creation'!
open
	"Open a SoundView with no model."
	"SoundView open"
	"SoundView openOn: (SampledSound sawtooth)"

	^SoundView openOn: nil! !

!SoundView class methodsFor: 'instance creation' stamp: 'stp 10/04/1999 22:44'!
openAsMorphOn: model 
	"Open a sound view"
	"SoundView example1."

	| window |
	window _ (SystemWindow labelled: 'later') model: self.
	window addMorph: (self newMorphOn: model).
	window openInWorld
! !

!SoundView class methodsFor: 'instance creation' stamp: 'stp 10/04/1999 22:33'!
openOn: model 
	"Open a sound view"

	| win heit label |
	World ifNotNil: [^self openAsMorphOn: model].
	heit := 280.
	label := (model == nil or: [model name isEmpty])
				ifTrue: ['SoundView']
				ifFalse: ['Sound View on: ' , model name].
	win := StandardSystemView
				model: model
				label: label
				minimumSize: 500 @ heit.
	self soundViewOn: model
		height: heit
		in: win.
	win controller open ! !

!SoundView class methodsFor: 'instance creation' stamp: 'stgp 11/19/1998 22:21'!
soundViewOn: snd height: heit in: view 
	"Open a sound view."

	| me ed offset |
	offset := -160.
	me := self new initialize.
	ed := SoundEditor new sound: snd.
	me model: ed.
	ed view: me.
	me height: heit.
	view addSubView: me in: view boundingBox borderWidth: 1.
"	self addButtonsOn: me
		for: ed
		to: view
		in: ((LayoutFrame new) leftFraction: 1 offset: ofset; topFraction: 0; rightFraction: 1; bottomFraction: 1).
"	^me! !


!SoundView class methodsFor: 'examples' stamp: 'stp 10/04/1999 22:27'!
example1 
	"Open a snd view on a sound file."
	"SoundView example1."

	SoundView openOn: (StoredSound fileExample).
	^self! !

!SoundView class methodsFor: 'examples'!
example2
	"	***This is decompiled code.*** 
	This may reflect a problem with the configuration of your image 
	and its sources and changes files. 
	Please refer to the documentation and the settings tool for help in 
	setting up the proper source code files."

	SoundView openOn: StoredSound sawtooth.
	^self! !


!SoundView class methodsFor: 'constant access'!
backgroundColor
	"Answer the default background color for sound views."
	"ColorValue constantNames"

	^Display depth > 1
		ifTrue: [Color veryLightGray]
		ifFalse: [Color white]! !

SoundBrowser initialize!