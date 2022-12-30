'From Cuis 6.0 [latest update: #5542] on 18 November 2022 at 10:01:09 pm'!
!classDefinition: #SixPanedBrowserWindow category: 'Morphic-Tool Windows'!
BrowserWindow subclass: #SixPanedBrowserWindow
	instanceVariableNames: 'packagePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
!SixPanedBrowserWindow commentStamp: '<historical>' prior: 0!
Traditional 6-paned browser!


!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'STP 4/27/2022 13:32:12'!
buildMorphicPackageList
	^ PluggableListMorph
		model: model
		listGetter: #packageList
		indexGetter: #packageIndex
		indexSetter: #packageIndex:
		mainView: self
		menuGetter: #systemCategoryMenu
		keystrokeAction: #systemCatListKey:from:! !

!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'STP 4/27/2022 16:28:18'!
buildMorphicSwitches
	| instanceSwitch commentSwitch classSwitch row buttonColor |
	instanceSwitch _ self createInstanceButton.
	commentSwitch _ self createCommentButton.
	classSwitch _ self createClassButton.
	
	row _ LayoutMorph newRow.
	row
		doAdoptWidgetsColor;
		addMorph: instanceSwitch proportionalWidth: 0.60;
		addMorph: commentSwitch proportionalWidth: 0.10;
		addMorph: classSwitch proportionalWidth: 0.30.
	buttonColor _ self buttonColor.
	row color: buttonColor.

	{ 	instanceSwitch.
		commentSwitch.
		classSwitch} do: [:m |  m color: buttonColor ].
		
	^row! !

!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'STP 4/27/2022 12:18:29'!
buildMorphicSystemCategoryList
	^ PluggableListMorph
		model: model
		listGetter: #systemCategoryList
		indexGetter: #systemCategoryIndex
		indexSetter: #systemCategoryIndex:
		mainView: self
		menuGetter: #systemCategoryMenu
		keystrokeAction: #systemCatListKey:from:! !

!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'STP 4/27/2022 12:18:29'!
buildMorphicWindow
	"Create a pluggable version of all the morphs for a Browser in Morphic"
	| upperPanes messageCatList messageList systemCatList classList packageList |
	packageList _ self buildMorphicPackageList.
	systemCatList _ self buildMorphicSystemCategoryList.
	classList _ self buildMorphicClassList.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
"	messageCatList rightSibling: packageList.
	classList 
		rightSibling: messageCatList.
	messageCatList
		leftSibling: classList
		rightSibling: messageList.
	messageList leftSibling: messageCatList.
	classList makeItemsDraggable.
	systemCatList 
		acceptDropsFrom: classList 
		performing: #categorizeUnderCategoryAt:class:
		whenOutsideList: #categorizeUnderNewCategoryClass:.
"
	messageList allowItemDrag: true..
	messageCatList
		acceptDropsFrom: messageList
		performing: #categorizeUnderCategoryAt:selector:.
	upperPanes _ LayoutMorph newRow.
	upperPanes
			addMorph: packageList
			proportionalWidth: 0.12;
		
			addAdjusterAndMorph: systemCatList
			proportionalWidth: 0.15;
		
			addAdjusterAndMorph: (self buildMorphicClassColumnWith: classList)
			proportionalWidth: 0.2;
		
			addAdjusterAndMorph: messageCatList
			proportionalWidth: 0.2;
		
			addAdjusterAndMorph: messageList
			proportionalWidth: 0.38.
			
	self layoutMorph
			addMorph: upperPanes
			proportionalHeight: 0.3;
		
			addAdjusterAndMorph: self buildLowerPanes
			proportionalHeight: 0.7.
			
	model changed: #editSelection! !

!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'stp 5/3/2022 16:27:35'!
createClassButton
	| classSwitch |
	classSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #classMessagesIndicated
		action: #indicateClassMessages.
	
	classSwitch
		label: 'class' font: (FontFamily familyName: FontFamily defaultFamilyName pointSize: (Preferences standardButtonFont pointSize - 2));
		setBalloonText: 'show class methods'.
		
	^classSwitch! !

!SixPanedBrowserWindow methodsFor: 'GUI building' stamp: 'stp 5/3/2022 16:26:24'!
createInstanceButton

	| instanceSwitch |
	instanceSwitch := PluggableButtonMorph 
		model: model
		stateGetter: #instanceMessagesIndicated
		action: #indicateInstanceMessages.

	instanceSwitch 
		label: 'instance' font: (FontFamily familyName: FontFamily defaultFamilyName pointSize: (Preferences standardButtonFont pointSize - 2));
		setBalloonText: 'show instance methods'.

	^instanceSwitch ! !


!SixPanedBrowserWindow methodsFor: 'commands' stamp: 'STP 4/27/2022 13:29:37'!
findClass

	| scopedClassNames |
	scopedClassNames _ model potentialClassNames asOrderedCollection.
	self class 
		findClassFrom: scopedClassNames 
		ifFound: [ :foundClass | | cat | 
			cat := foundClass category.
			model selectedPackage: (cat upTo: $-).
			model packageIndex: (model packageIndex).
			model selectedSystemCategory: (cat copyAfter: $-).
			model selectClass: foundClass ]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'SixPanedBrowserWindow class' category: 'Morphic-Tool Windows'!
SixPanedBrowserWindow class
	instanceVariableNames: ''!

!SixPanedBrowserWindow class methodsFor: 'instance creation' stamp: 'STP 4/26/2022 12:47:40'!
openBrowser
	"SixPanedBrowserWindow openBrowser"
	
	| browser |
	browser _ SixPanedBrowser new.
	^ self
		open: browser
		label: browser defaultBrowserTitle! !


!SixPanedBrowserWindow class methodsFor: 'menu-world' stamp: 'STP 4/26/2022 18:35:06'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		10.
			#itemOrder 		-> 		28.
			#label 			->			'6-Paned Browser'.
			#object 			-> 		SixPanedBrowserWindow.
			#selector 		-> 		#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk 6-paned code browser, for studying and modifying the system'.
		} asDictionary}`! !

!SixPanedBrowserWindow class methodsFor: 'menu-world' stamp: 'stp 11/18/2022 22:00:48'!
worldMenuOptions
	^ `{{
			#submenuOf -> TheWorldMenu openLabel.
			#itemGroup 		-> 			10.
			#itemOrder 		-> 			28.
			#label 			->		'6-Paned Browser'.
			#object 			-> 		SixPanedBrowserWindow.
			#selector 		-> 			#openBrowser.
			#icon 			-> 		#editFindReplaceIcon.
			#balloonText 	-> 		'A Smalltalk 6-paned code browser, for studying and modifying the system'.
		} asDictionary}`! !
