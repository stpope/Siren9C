'From Squeak6.0 of 21 October 2022 [latest update: #22114] on 22 December 2022 at 7:13:44 am'!Object subclass: #WorkbookWindow	instanceVariableNames: 'folder label pages list textM index'	classVariableNames: ''	poolDictionaries: ''	category: 'Tools-Base'!!WorkbookWindow commentStamp: 'stp 12/10/2022 06:55' prior: 0!A WorkbookWindow is a simple list of text views used for Siren's documentation and demo scripts.Instance Variables	folder:		<String> the folder I was read in from	index:		<Integer> the selected page index	label:		<String> the window label	list:		<OrderedCollection of Strings> the list of page names	pages:		<OrderedCollection of (String -> String)> the contents of the pagesThis app exists in versions on VisualWorks and Cuis as well.!!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 12/6/2022 17:41'!initializeOn: fol named: nam
	"Set up the receiver."

	folder := fol.
	pages := OrderedCollection new.
	index := 1.
	label := nam! !!WorkbookWindow methodsFor: 'initialization' stamp: 'stp 12/6/2022 18:02'!open
	"Open the workbook window."
	"WorkbookWindow openFolder: '../../../Siren9C/Workbook/' named: 'Siren Workbook' "

	| window listM |	window := (SystemWindow labelled: 'Siren Workspace') model: self.
	textM := PluggableTextMorph 		on: self 		text: #getPage 		accept: #accept:		readSelection: nil 		menu: #textMenu: .
	textM askBeforeDiscardingEdits: true.
	textM wrapFlag: true.
	
	listM := PluggableListMorph		on: self 		list: #pageList 		selected: #pageListIndex 		changeSelected: #pageListIndex: 		menu: #pageListMenu: .
	{		{listM. 0@0     corner: 1.0@0.25.  0@0 corner: 0@0.}.		{textM. 0@0.25 corner: 1.0@1.0.  0@0 corner: 0@0.}.	} do: [ :each | | frame |		frame := LayoutFrame 			fractions: each second 			offsets: each third.		window addMorph: each first fullFrame: frame.	].	^ window openInWorld! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 17:58'!accept
	"Plug the text into the receiver's current page."

	(index = 0) ifTrue: [^nil].
"	textM scroller hasUnacceptedEdits: false."
	(pages at: index) value: textM text! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 17:58'!accept: text
	"Plug the text into the receiver's current page."

	(index = 0) ifTrue: [^nil].
"	textM scroller hasUnacceptedEdits: false."
	(pages at: index) value: text! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:18:00'!addPage: pag named: name
	"Add the given page to the receiver's list."

	pages addLast: (name -> pag)! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 21:01:44'!contentsSelection
	"Answer the selected page's contents"

	^1 to: 0! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 17:44'!getPage
	"Answer the list of topics for the list view."

	^(pages at: index) value! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 17:44'!getText
	"Answer the list of topics for the list view."

	^(pages at: index) value! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:33:43'!pageContents
	"Answer the list of topics for the list view."

	^(pages at: index) value! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/10/2022 06:46'!pageList
	"Answer the list of topics for the list view."

	| coll |
	coll := OrderedCollection new.
	1 to: pages size do:
		[ :in |
		coll addLast: in printString, ': ', ((pages at: in) key)].
	^coll! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 11/18/2022 20:30:38'!pageListIndex
	"Answer the index into the list of topics for the list view."

	^index! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 18:01'!pageListIndex: ind
	"Answer the index into the list of topics for the list view."

	ind > 0 ifFalse: [^self].	textM accept.
	(pages at: index) value: textM getText.
	index := ind.
	self changed: #getPage.	self changed: #pageListIndex.! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/21/2022 18:37'!pageListMenu: aMenu	"Answer the menu for the workbook page list"	^ aMenu 		labels: 'add pageremove pagerename pagesave workbook as textsave workbook as HTML'		lines: #(3)		selections: #(addPage removePage renamePage saveToText saveToHTML)
! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 18:05'!pageListMenu: aMenu shifted: shifted	"Answer the menu for the workbook page list"	^ aMenu 		labels: 'add pageremove pagerename pagesave workbook'		lines: #(1)		selections: #(addPage removePage renamePage save)
! !!WorkbookWindow methodsFor: 'accessing' stamp: 'stp 12/6/2022 18:18'!textMenu: aMenu	"Answer the menu for the workbook text pane"	^StringHolder basicNew codePaneMenu: aMenu shifted: false! !!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 12/9/2022 13:35'!addPage
	"Add a page to the workbook"

	| pagNam |	pagNam := FillInTheBlank request: 'New pagename' initialAnswer: 'New page'.	pagNam isEmpty ifTrue: [^nil].	pages add: (pagNam -> '') afterIndex: index.
	self changed: #pageList! !!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 11/20/2022 18:09:12'!removePage
	"Remove a page from the workbook"

	(self confirm: 'Remove page?') ifFalse: [^nil].
	pages remove: (pages at: index).
	self changed: #pageList! !!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 11/20/2022 18:05:24'!renamePage
	"Rename a page in the workbook"

	| pag |
	pag _ pages at: index.
	self request: 'New name' 
		initialAnswer: pag key 
		verifying: [ :aString | aString notEmpty and: [aString ~= pag key]]
		do: [ :aString |
			pag key: aString.
		self changed: #pageList]! !!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 12/22/2022 06:33'!saveToHTML
	"Save the workbook to an HTML file with left-hand index. This method creates 3 files: left, main and index"	| fold toc body ind |
	fold := FillInTheBlank request: 'Folder name to save Workbook' initialAnswer: folder.	(FileDirectory default directoryExists: fold)		ifFalse: [FileDirectory default createDirectory: fold].	toc := WriteStream on: (String new: 4096).	body := WriteStream on: (String new: 200000).	toc nextPutAll: '<H3>Siren 9C Workbook</H3><HR>'; cr; cr.		"write headers"	body nextPutAll: '<H3>Siren 9C Workbook</H3>'; cr; cr.	ind := 1.	pages do: [ :pag | | title tag txt |			"loop over pages"
		title  := pag key.		tag := title copyWithout: $ .		toc nextPutAll: ind printString, ' - ', '<a target="main" href="./main.html#', tag, '">', title, '</a><p>'; cr.		body cr; nextPutAll: '<hr><h3 id="', tag, '">', ind printString, ' - ', title, '</h3>'; cr.													"clean up page text: replace CR, LF and tabs"		txt := (((pag value copyReplaceAll: String lf with: '</br>', String lf) 				copyReplaceAll: String cr with: '</br>', String lf)				copyReplaceAll: String tab with: '&emsp;').		body cr; nextPutAll: txt, '</p>'.		ind := ind +1].	body cr; nextPutAll: '</p><HR><p>'; cr.	"write footers"	toc cr; nextPutAll: '</p><HR><p>'; cr.													"store 3 files"	FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, 'main.html')			do: [ :strm | strm nextPutAll: body contents].				FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, 'left.html')			do: [ :strm | strm nextPutAll: toc contents].				FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, 'index.html')			do: [ :strm | strm nextPutAll: '<HTML><HEAD><TITLE>Siren 9C Workbook</TITLE></HEAD><FRAMESET cols="20%, 80%"><FRAME name="left" src="left.html"><FRAME name="main" src="main.html"><NOFRAMES><P>This frameset document contains:<A href="main.html">this main page</A></NOFRAMES></FRAMESET></HTML>'; cr].	Transcript cr; show: 'Workbook saved to HTML ', fold; cr! !!WorkbookWindow methodsFor: 'menu items' stamp: 'stp 12/22/2022 06:34'!saveToText
	"Save the workbook to a folder of text files with a table of contents file."
	| fold toc |
	fold := FillInTheBlank request: 'Folder name to save Workbook' initialAnswer: folder.	(FileDirectory default directoryExists: fold)		ifFalse: [FileDirectory default createDirectory: fold].	toc := WriteStream on: (String new: 1024).
	pages do: [ :pag | | title |
		title  := pag key, '.txt'.		FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, title)			do: [ :strm |				strm nextPutAll: pag value.
				toc nextPutAll: title; cr]].
	FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, 'TableOfContents.txt')			do: [ :strm |				strm nextPutAll: toc contents].	Transcript cr; show: 'Workbook saved to folder ', fold; cr! !"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!WorkbookWindow class	instanceVariableNames: ''!!WorkbookWindow class methodsFor: 'instance creation' stamp: 'stp 12/10/2022 06:51'!openFolder: folder named: nam 	"Create a workbook by reading in files from a folder."	"WorkbookWindow openFolder: 'Workbook5' named: 'Siren Workbook'"		| fol bk toc |	bk := self new initializeOn: folder named: nam.	fol := folder, FileDirectory pathNameDelimiter asString.	toc := FileStream readOnlyFileNamed: fol, 'TableOfContents.txt'.	[toc atEnd]		whileFalse: [| lin txt |			lin := toc upTo: Character cr."			Transcript show: lin; cr."			txt := (FileStream readOnlyFileNamed: fol, lin) contentsOfEntireFile.			bk addPage: txt named: (lin copyUpTo: $.)].	bk open! !