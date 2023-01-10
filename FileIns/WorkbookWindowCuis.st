'From Cuis 6.0 [latest update: #5625] on 9 January 2023 at 4:38:06 pm'!
!classDefinition: #WorkbookWindow category: #'Morphic-Tool Windows'!
WorkspaceWindow subclass: #WorkbookWindow
	instanceVariableNames: 'folder pages list textM index'
	classVariableNames: 'Workbook'
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 11/20/2022 18:11:02'!
initializeOn: fol named: nam
	"Set up the receiver."

	folder _ fol.
	pages _ OrderedCollection new.
	index _ 1.
	self setLabel: nam! !

!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 11/29/2022 12:26:15'!
open
	"Open the workbook window."
	"WorkbookWindow openFolder: './Siren_Cuis/Workbook3' named: 'Siren Workbook' "

	| layout listM |
	layout _ LayoutMorph newColumn.
	textM _ TextModelMorph withModel: Workspace new.
	textM acceptOnAny: true.
	textM askBeforeDiscardingEdits: false.
	textM model toggleStyling.
	
	listM _ PluggableListMorph
			model: self 
			listGetter: #pageList
			indexGetter: #pageListIndex
			indexSetter: #pageListIndex: 
			mainView: self
			menuGetter: #pageListMenu
			keystrokeAction: #pageListKey:from: .
			
	layout addMorph: listM proportionalHeight: 0.2.
	layout addAdjusterAndMorph: textM proportionalHeight: 0.8.
	self layoutMorph addMorph: layout.
	textM model actualContents: (Text fromString: (pages at: 1) value asString).
	^self openInWorld! !


!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/19/2022 10:27:36'!
accept: text
	"Plug the text into the receiver's current page."

	(index = 0) ifTrue: [^nil].
	textM scroller hasUnacceptedEdits: false.
	(pages at: index) value: text! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:18:00'!
addPage: pag named: name
	"Add the given page to the receiver's list."

	pages addLast: (name -> pag)! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 21:01:44'!
contentsSelection
	"Answer the selected page's contents"

	^1 to: 0! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:33:43'!
pageContents
	"Answer the list of topics for the list view."

	^(pages at: index) value! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 1/7/2023 09:57:05'!
pageList
	"Answer the list of topics for the list view."

	| coll |
	coll _ OrderedCollection new.
	1 to: pages size do:
		[ :ind |
		coll addLast: (ind printString, ': ', (pages at: ind) key)].
	^coll! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:30:38'!
pageListIndex
	"Answer the index into the list of topics for the list view."

	^index! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/29/2022 12:17:16'!
pageListIndex: ind
	"Answer the index into the list of topics for the list view."

	ind > 0 ifFalse: [^self].
	(pages at: index) value: textM model actualContents.
	index _ ind.
	self changed: #pageContents.
	textM model actualContents: (Text fromString: (pages at: index) value asString).
! !

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 1/9/2023 16:19:41'!
pageListMenu
	"Answer the menu for the workbook page list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Page List Menu'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Add page'.
				#selector 			-> 		#addPage.
			} asDictionary.
			{
				#label 			-> 		'Remove page'.
				#selector 			-> 		#removePage.
			} asDictionary.
			{
				#label 			-> 		'Rename page'.
				#selector 			-> 		#renamePage.
			} asDictionary.
			{
				#label 			-> 		'Save workbook as text'.
				#selector 			-> 		#save.
			} asDictionary.
			{
				#label 			-> 		'Save workbook as HTML'.
				#selector 			-> 		#saveToHTML.
			} asDictionary.
		}`.
	^ aMenu! !


!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 11/20/2022 18:12:01'!
addPage
	"Add a page to the workbook"

	self request: 'New pagename' 
		initialAnswer: 'New page'
		verifying: [ :aString | aString notEmpty]
		do: [ :aString |
			pages add: (aString -> '') afterIndex: index.
			self changed: #pageList]! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 11/20/2022 18:09:12'!
removePage
	"Remove a page from the workbook"

	(self confirm: 'Remove page?') ifFalse: [^nil].
	pages remove: (pages at: index).
	self changed: #pageList! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 11/20/2022 18:05:24'!
renamePage
	"Rename a page in the workbook"

	| pag |
	pag _ pages at: index.
	self request: 'New name' 
		initialAnswer: pag key 
		verifying: [ :aString | aString notEmpty and: [aString ~= pag key]]
		do: [ :aString |
			pag key: aString.
		self changed: #pageList]! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 1/9/2023 16:21:32'!
save
	"Save the workbook to a folder."

	self request: 'Folder name' 
		initialAnswer: folder
		verifying: [ :aString | aString notEmpty]
		do: [ :aString | | nam fold toc |
			nam _ (DirectoryEntry withPathName: aString) pathName.
			fold _ DirectoryEntry withPathName: nam.
			fold exists ifFalse: [fold fileAccessor createDirectory: nam].
			toc _ WriteStream on: (String new: 1024).
			pages do: [ :pag | | title |
				title _ (pag key copyReplaceAll: ' ' with: '_') , '.txt'.
				fold at: title put: pag value.
				toc nextPutAll: title; cr].
			fold at: 'TableOfContents.txt' put: toc contents.].
		! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 1/9/2023 16:37:04'!
saveToHTML
	"Save the workbook to a folder as 3 HTML pages (left, main, index)."

	self request: 'Folder name' 
		initialAnswer: folder
		verifying: [ :aString | aString notEmpty]
		do: [ :aString | | nam fold toc	 body ind |
			nam _ (DirectoryEntry withPathName: aString) pathName.
			fold _ DirectoryEntry withPathName: nam.
			fold exists ifFalse: [fold fileAccessor createDirectory: nam].

			toc := WriteStream on: (String new: 4096).
			body := WriteStream on: (String new: 200000).
			toc nextPutAll: '<H3>Siren 9C Workbook</H3><HR>'; cr; cr.		"write headers"
			body nextPutAll: '<H3>Siren 9C Workbook</H3>'; cr; cr.
			ind := 1.
			pages do: [ :pag | | title tag txt |			"loop over pages"
				title  := pag key.
				tag := title copyWithout: $ .
				toc nextPutAll: ind printString, ' - ', '<a target="main" href="./main.html#', 
						tag, '">', title, '</a><p>'; cr.
			body cr; nextPutAll: '<hr><h3 id="', tag, '">', ind printString, ' - ', title, '</h3>'; cr.
														"clean up page text: replace CR, LF and tabs"
			txt := (((pag value copyReplaceAll: String lf with: '</br>', String lf) 
					copyReplaceAll: String cr with: '</br>', String lf)
					copyReplaceAll: String tab with: '&emsp;').
			body cr; nextPutAll: txt, '</p>'.
			ind := ind +1].
			body cr; nextPutAll: '</p><HR><p>'; cr.	"write footers"
			toc cr; nextPutAll: '</p><HR><p>'; cr.
			fold at: 'main.html' put: body contents.
			fold at: 'left.html' put: toc contents.
			fold at: 'index.html' put: 
'<HTML>
<HEAD>
<TITLE>Siren 9C Workbook</TITLE>
</HEAD>
<FRAMESET cols="20%, 80%">
<FRAME name="left" src="left.html">
<FRAME name="main" src="main.html">
<NOFRAMES>
<P>This frameset document contains:
<A href="main.html">this main page</A>
</NOFRAMES>
</FRAMESET>
</HTML>
'.
	Transcript cr; show: 'Workbook saved to HTML ', nam; cr]
! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 1/9/2023 16:23:38'!
saveToText
	"Save the workbook to a folder."

	self request: 'Folder name' 
		initialAnswer: folder
		verifying: [ :aString | aString notEmpty]
		do: [ :aString | | nam fold toc |
			nam _ (DirectoryEntry withPathName: aString) pathName.
			fold _ DirectoryEntry withPathName: nam.
			fold exists ifFalse: [fold fileAccessor createDirectory: nam].
			toc _ WriteStream on: (String new: 1024).
			pages do: [ :pag | | title |
				title _ (pag key copyReplaceAll: ' ' with: '_') , '.txt'.
				fold at: title put: pag value.
				toc nextPutAll: title; cr].
			fold at: 'TableOfContents.txt' put: toc contents.].
		! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'WorkbookWindow class' category: #'Morphic-Tool Windows'!
WorkbookWindow class
	instanceVariableNames: ''!

!WorkbookWindow class methodsFor: 'instance creation' stamp: 'stp 1/9/2023 16:31:14'!
open
	"Create a workbook by reading in files from a folder."
	"WorkbookWindow open"
	Workbook ifNotNil: [^Workbook  open].
	self error: 'No workbook loaded.'! !

!WorkbookWindow class methodsFor: 'instance creation' stamp: 'stp 1/9/2023 16:32:58'!
openFolder: folder named: nam
	"Create a workbook by reading in files from a folder."
	"WorkbookWindow openFolder: './Siren9C/Workbook2' named: 'Siren Workbook'"

	| inst toc |
	inst _ self new initializeOn: folder named: nam.
	toc _ (folder, '/TableOfContents.txt') fileContents.
	toc lines do:
		[ :fil | | txt title |
		fil isEmpty ifFalse:
			[txt _ (folder, '/', fil) fileContents.
			txt _ txt copyReplaceAll: (String with: Character cr) with: (String with: Character lf).
			title _ fil copyReplaceAll: '_' with: ' '.
			title _ title upTo: $. .
			inst addPage: txt named: title]].
	Workbook _ inst.
	inst open.! !
