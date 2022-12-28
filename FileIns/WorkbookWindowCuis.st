'From Cuis 6.0 [latest update: #5569] on 27 December 2022 at 5:23:23 pm'!
!classDefinition: #WorkbookWindow category: #'Morphic-Tool Windows'!
WorkspaceWindow subclass: #WorkbookWindow
	instanceVariableNames: 'folder pages list textM index'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
!WorkbookWindow commentStamp: 'stp 12/26/2022 11:12:57' prior: 0!
A WorkbookWindow is a simple list of text views used for Siren's documentation and demo scripts.

Instance Variables
	folder:		<String> the folder I was read in from
	index:		<Integer> the selected page index
	label:		<String> the window label
	list:		<OrderedCollection of Strings> the list of page names
	pages:		<OrderedCollection of (String -> String)> the contents of the pages

This app exists in versions for VisualWorks and Squeak as well.
!


!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 11/20/2022 18:11:02'!
initializeOn: fol named: nam
	"Set up the receiver."

	folder _ fol.
	pages _ OrderedCollection new.
	index _ 1.
	self setLabel: nam! !

!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 12/26/2022 11:15:43'!
open
	"Open the workbook window."
	"WorkbookWindow openFolder: './Siren9C/Workbook' named: 'Siren Workbook' "

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

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/26/2022 11:14:34'!
pageList
	"Answer the list of topics for the list view, with page numbers."

	| coll |
	coll _ OrderedCollection new.
	1 to: pages size do:
 		[ :in |
 		coll addLast: (in printString, ': ', ((pages at: in) key))].
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

!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/26/2022 11:14:51'!
pageListMenu
	"Answer the menu for the workbook page list"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addTitle: 'Page List Menu'.
	aMenu 
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'add page'.
				#selector 			-> 		#addPage.
			} asDictionary.
			{
				#label 			-> 		'remove page'.
				#selector 			-> 		#removePage.
			} asDictionary.
			{
				#label 			-> 		'rename page'.
				#selector 			-> 		#renamePage.
			} asDictionary.
			{
				#label 			-> 		'save workbook as text'.
				#selector 			-> 		#saveToText.
			} asDictionary.
			{
				#label 			-> 		'save workbook as HTML'.
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

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 12/26/2022 11:20:11'!
saveToHTML
	"Save the workbook to an HTML file with left-hand index. This method creates 3 files: left, main and index"

	self request: 'Folder name to save Workbook' 
		initialAnswer: folder
		verifying: [ :aString | aString notEmpty]
		do: [ :aString | | nam  fold toc body ind |
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
		toc nextPutAll: ind printString, ' - ', '<a target="main" href="./main.html#', tag, '">', title, '</a><p>'; cr.
		body cr; nextPutAll: '<hr><h3 id="', tag, '">', ind printString, ' - ', title, '</h3>'; cr.
													"clean up page text: replace CR, LF and tabs"
		txt := (((pag value copyReplaceAll: String lf with: '</br>', String lf) 
				copyReplaceAll: String cr with: '</br>', String lf)
				copyReplaceAll: String tab with: '&emsp;').
		body cr; nextPutAll: txt, '</p>'.
		ind := ind +1].
	body cr; nextPutAll: '</p><HR><p>'; cr.	"write footers"
	toc cr; nextPutAll: '</p><HR><p>'; cr.
		
	ind := WriteStream on: (String new: 256).
	ind nextPutAll: '<HTML>
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
</HTML>'; cr.
													"store 3 files"
	fold at: 'main.html' put: body contents.
	fold at: 'left.html' put: toc contents.
	fold at: 'index.html' put: ind contents.

	Transcript cr; show: 'Workbook saved to HTML folder ', nam; cr]! !

!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 12/26/2022 11:20:19'!
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
				title  _ pag key, '.txt'.
				fold at: title put: pag value.
				toc nextPutAll: title; cr].
			fold at: 'TableOfContents.txt' put: toc contents.
		Transcript cr; show: 'Workbook saved to HTML ', nam; cr]! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: 'WorkbookWindow class' category: #'Morphic-Tool Windows'!
WorkbookWindow class
	instanceVariableNames: ''!

!WorkbookWindow class methodsFor: 'instance creation' stamp: 'stp 12/26/2022 11:16:01'!
openFolder: folder named: wnam
	"Create a workbook by reading in files from a folder."
	"WorkbookWindow openFolder: './Siren9C/Workbook/' named: 'Siren Workbook'"

	| inst toc fnam |
	inst _ self new initializeOn: folder named: wnam.
	fnam _ folder.
	fnam last == FileDirectory slash first
		ifFalse: [fnam _ fnam, FileDirectory slash].
	toc _ (fnam, 'TableOfContents.txt') fileContents.
	toc lines do:
		[ :fil | | txt title |
		fil isEmpty ifFalse:
			[txt _ (fnam, fil) fileContents.
			title _ fil copyReplaceAll: '_' with: ' '.
			title _ title upTo: $. .
			inst addPage: txt named: title]].
	inst open! !
