'From Cuis 6.0 [latest update: #5625] on 7 January 2023 at 7:30:20 am'!
!classDefinition: #STPTheme category: 'Graphics-Themes'!
Theme subclass: #STPTheme
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Themes'!

!STPTheme methodsFor: 'colors' stamp: 'stp 1/7/2023 07:28:40'!
defaultWindowColor
	^ `Color lightGray`! !

!STPTheme methodsFor: 'colors' stamp: 'stp 1/7/2023 07:29:58'!
paneBackgroundFrom: aColor
	^ Color lightYellow muchLighter! !

!STPTheme methodsFor: 'colors' stamp: 'stp 1/7/2023 07:27:02'!
useUniformColors
	^true! !
