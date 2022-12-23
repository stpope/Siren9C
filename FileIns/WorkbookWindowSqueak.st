﻿'From Squeak6.0 of 21 October 2022 [latest update: #22114] on 22 December 2022 at 7:13:44 am'!
	"Set up the receiver."

	folder := fol.
	pages := OrderedCollection new.
	index := 1.
	label := nam! !
	"Open the workbook window."
	"WorkbookWindow openFolder: '../../../Siren9C/Workbook/' named: 'Siren Workbook' "

	| window listM |
	textM := PluggableTextMorph 
	textM askBeforeDiscardingEdits: true.
	textM wrapFlag: true.
	
	listM := PluggableListMorph

	"Plug the text into the receiver's current page."

	(index = 0) ifTrue: [^nil].
"	textM scroller hasUnacceptedEdits: false."
	(pages at: index) value: textM text! !
	"Plug the text into the receiver's current page."

	(index = 0) ifTrue: [^nil].
"	textM scroller hasUnacceptedEdits: false."
	(pages at: index) value: text! !
	"Add the given page to the receiver's list."

	pages addLast: (name -> pag)! !
	"Answer the selected page's contents"

	^1 to: 0! !
	"Answer the list of topics for the list view."

	^(pages at: index) value! !
	"Answer the list of topics for the list view."

	^(pages at: index) value! !
	"Answer the list of topics for the list view."

	^(pages at: index) value! !
	"Answer the list of topics for the list view."

	| coll |
	coll := OrderedCollection new.
	1 to: pages size do:
		[ :in |
		coll addLast: in printString, ': ', ((pages at: in) key)].
	^coll! !
	"Answer the index into the list of topics for the list view."

	^index! !
	"Answer the index into the list of topics for the list view."

	ind > 0 ifFalse: [^self].
	(pages at: index) value: textM getText.
	index := ind.
	self changed: #getPage.
! !
! !
	"Add a page to the workbook"

	| pagNam |
	self changed: #pageList! !
	"Remove a page from the workbook"

	(self confirm: 'Remove page?') ifFalse: [^nil].
	pages remove: (pages at: index).
	self changed: #pageList! !
	"Rename a page in the workbook"

	| pag |
	pag _ pages at: index.
	self request: 'New name' 
		initialAnswer: pag key 
		verifying: [ :aString | aString notEmpty and: [aString ~= pag key]]
		do: [ :aString |
			pag key: aString.
		self changed: #pageList]! !
	"Save the workbook to an HTML file with left-hand index. This method creates 3 files: left, main and index"
	fold := FillInTheBlank request: 'Folder name to save Workbook' initialAnswer: folder.
		title  := pag key.
	"Save the workbook to a folder of text files with a table of contents file."

	fold := FillInTheBlank request: 'Folder name to save Workbook' initialAnswer: folder.
	pages do: [ :pag | | title |
		title  := pag key, '.txt'.
				toc nextPutAll: title; cr]].
	FileStream fileNamed: (fold, FileDirectory pathNameDelimiter asString, 'TableOfContents.txt')