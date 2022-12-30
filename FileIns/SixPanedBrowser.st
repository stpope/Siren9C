'From Cuis 6.0 [latest update: #5115] on 3 May 2022 at 4:29:50 pm'!
!classDefinition: #SixPanedBrowser category: #'Tools-Browser'!
Browser subclass: #SixPanedBrowser
	instanceVariableNames: 'packageOrganizer selectedPackage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:30:38'!
packageIndex
	"Set the selected system category index to be anInteger. Update all other
	selections to be deselected."

	selectedPackage ifNil: [^0].
	^ self packageList findFirst: [ :pkgName |
		pkgName match: selectedPackage ].! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:18:24'!
packageIndex: anInteger
	"Set the selected system category index to be anInteger. Update all other
	selections to be deselected."
	| index packageList |
	packageList _ self packageList.
	index _ packageList
		ifInBounds: anInteger
		ifNot: 0. 
	selectedPackage _ index = 0 ifFalse: [ packageList at: index ].
	self selectedSystemCategory: nil.! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:21:29'!
packageList
	"Generate the top-level list of category headers or packages."
	| pkgs |
	pkgs _ OrderedCollection new.
	systemOrganizer categories do: [ :cat | | head |
		head := cat upTo: $-.
		(pkgs includes: head) ifFalse: [ pkgs addLast: head ]].
	^ pkgs.! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:22:14'!
packageListIndex
	"Set the selected system category index to be anInteger. Update all other
	selections to be deselected."
self halt.
	^ self packageList findFirst: [ :pkgName |
		pkgName includes: selectedPackage ].! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:10:22'!
packageListIndex: anInteger
	"Set the selected system category index to be anInteger. Update all other
	selections to be deselected."
	| index packageList |
	packageList _ self packageList.
	index _ packageList
		ifInBounds: anInteger
		ifNot: 0. 
	selectedPackage _ index = 0 ifFalse: [ packageList at: index ].
	self selectedSystemCategory: nil.! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/26/2022 13:00:05'!
selectedPackage
	"Set the selected package category and update all other selections to be deselected."
	^selectedPackage ! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:08:33'!
selectedPackage: aString
	"Set the selected package category and update all other selections to be deselected."
	selectedPackage _ aString.
	self selectedSystemCategory: nil! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/26/2022 13:25:22'!
selectedPackageIndex
	"Set the selected system category index to be anInteger. Update all other
	selections to be deselected."
	^ self packageList indexOf: selectedPackage ifAbsent: 1.! !

!SixPanedBrowser methodsFor: 'package-list' stamp: 'STP 4/27/2022 13:12:10'!
selectedPackageIndex: anInteger
	"Set the selected package category and update all other selections to be deselected."
	| index packageList | 
	packageList _ self packageList.
	index _ packageList
		ifInBounds: anInteger
		ifNot: 0.
	index = 0
		ifTrue: [ selectedPackage _ nil ]
		ifFalse: [ selectedPackage _ packageList at: index ].
	self selectedSystemCategory: nil.! !


!SixPanedBrowser methodsFor: 'system category list' stamp: 'STP 4/27/2022 16:14:12'!
selectedSystemCategory: aString
	"Set the selected system category and update all other selections to be deselected."
	aString ifNil: [ selectedSystemCategory _ nil ].
	aString ifNotNil: [ selectedSystemCategory _ selectedPackage , '-', aString ].
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection:
		(aString isNil
			ifTrue: [ #none ]
			ifFalse: [ #newClass ]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #selectedSystemCategory.
	self changed: #systemCategoryList.
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged.! !

!SixPanedBrowser methodsFor: 'system category list' stamp: 'STP 4/27/2022 12:19:51'!
setSelectedSystemCategory: aString
	"Set the selected system category and update all other selections to be deselected."
	self selectedSystemCategory: aString
! !

!SixPanedBrowser methodsFor: 'system category list' stamp: 'stp 5/3/2022 15:15:24'!
systemCategoryIndex

	| foot |
	selectedPackage ifNil: [ ^ 0 ].
	selectedSystemCategory ifNil: [ ^ 0 ].
	foot := selectedSystemCategory allButFirst: (selectedSystemCategory indexOf: $-).
	^ self systemCategoryList findFirst: [ :cat |
		cat match: foot ].! !

!SixPanedBrowser methodsFor: 'system category list' stamp: 'stp 5/3/2022 15:13:27'!
systemCategoryIndex: anInteger
	"Set the selected system category and update all other selections to be deselected."
	anInteger = 0 ifFalse: [ selectedSystemCategory _ selectedPackage, '-',  (self systemCategoryList at: anInteger)]
		ifTrue: [selectedSystemCategory _ nil].
"	Transcript show: selectedSystemCategory; cr."
	selectedClassName _ nil.
	selectedMessageCategory _ nil.
	selectedMessage _ nil.
	self editSelection:
		(selectedSystemCategory isNil
			ifTrue: [ #none ]
			ifFalse: [ #newClass ]).
	metaClassIndicated _ false.
	self setClassOrganizer.
	self changed: #selectedSystemCategory.
	self changed: #systemCategoryList.
	self changed: #classList.
	self changed: #messageCategoryList.
	self changed: #messageList.
	self changed: #relabel.
	self changed: #instanceMessagesIndicated.
	self changed: #classCommentIndicated.
	self changed: #classMessagesIndicated.
	self acceptedContentsChanged.! !

!SixPanedBrowser methodsFor: 'system category list' stamp: 'stp 5/3/2022 14:53:55'!
systemCategoryList
	"Answer the class categories modelled by the receiver."
	selectedPackage ifNil: [ ^ OrderedCollection new ].
	^ (systemOrganizer categories select: [ :item |
		(item upTo: $-) = selectedPackage ]) collect: [ :item |
		item allButFirst: (item indexOf: $-) ].! !

!SixPanedBrowser methodsFor: 'system category list' stamp: 'stp 5/3/2022 15:14:45'!
systemCategoryListIndex

	selectedPackage ifNil: [ ^ 0 ].
	selectedSystemCategory ifNil: [ ^ 0 ].
	^ self systemCategoryList findFirst: [ :cat |
		(cat upTo: $-) = selectedPackage and:
		[cat endsWith: selectedSystemCategory ]].! !
