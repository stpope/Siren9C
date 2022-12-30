<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 9, 2007 at 9:58:42 am</time-stamp>


<class>
<name>HATRule</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>name condition action confidence explanation context </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Kernel</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.HATRule class</class-id> <category>instance creation</category>

<body package="Siren" selector="if:then:">if: conditionBlock then: actionBlock

	^self new initialize
		condition: conditionBlock;
		action: actionBlock</body>

<body package="Siren" selector="if:then:because:">if: conditionBlock then: actionBlock because: aString

	^self new initialize
		condition: conditionBlock;
		action: actionBlock;
		explanation: aString</body>

<body package="Siren" selector="if:then:because:cf:">if: conditionBlock then: actionBlock because: aString cf: cf

	^self new initialize
		condition: conditionBlock;
		action: actionBlock;
		explanation: aString;
		confidence: cf</body>

<body package="Siren" selector="if:then:cf:">if: conditionBlock then: actionBlock cf: cf

	^self new initialize
		condition: conditionBlock;
		action: actionBlock;
		confidence: cf</body>

<body package="Siren" selector="name:if:then:">name: ruleName if: conditionBlock then: actionBlock

	^self new initialize
		name: ruleName;
		condition: conditionBlock;
		action: actionBlock</body>

<body package="Siren" selector="name:if:then:because:">name: ruleName if: conditionBlock then: actionBlock because: aString

	^self new initialize
		name: ruleName;
		condition: conditionBlock;
		action: actionBlock;
		explanation: aString</body>

<body package="Siren" selector="name:if:then:because:cf:">name: ruleName if: conditionBlock then: actionBlock because: aString cf: cf

	^self new initialize
		name: ruleName;
		condition: conditionBlock;
		action: actionBlock;
		explanation: aString;
		confidence: cf</body>

<body package="Siren" selector="name:if:then:cf:">name: ruleName if: conditionBlock then: actionBlock cf: cf

	^self new initialize
		name: ruleName;
		condition: conditionBlock;
		action: actionBlock;
		confidence: cf</body>

<body package="Siren" selector="newRule:condition:action:confidence:explanation:context:">newRule: ruleName condition: conditionBlock action: actionBlock confidence: cf explanation: aString context: anObject

	^self new
		name: ruleName;
		condition: conditionBlock;
		action: actionBlock;
		confidence: cf;
		explanation: aString;
		context: anObject</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.HATRule</class-id>
<body>HATRules

	HATRules have a condition, an action, an explanation, a confidence factor, and the
	context in which they are invoked.

	The condition and action are blocks which are evaluated with two arguments:
	the rule they are a part of, and the rules context.

	HATRules respond to three primary messages: wantsToFire, fire, and explain.</body>
</comment>

<methods>
<class-id>Siren.HATRule</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize

	confidence := 0.0</body>

<body package="Siren" selector="release">release

	"Break all circular references for garbage collection."

	condition release.
	condition := nil.
	action release.
	action := nil.
	context := nil</body>
</methods>

<methods>
<class-id>Siren.HATRule</class-id> <category>accessing</category>

<body package="Siren" selector="action">action

	^action</body>

<body package="Siren" selector="action:">action: aBlock

	action := aBlock</body>

<body package="Siren" selector="condition">condition

	^condition</body>

<body package="Siren" selector="condition:">condition: aBlock

	condition := aBlock</body>

<body package="Siren" selector="confidence">confidence

	^confidence</body>

<body package="Siren" selector="confidence:">confidence: aFloat

	confidence := aFloat</body>

<body package="Siren" selector="context">context

	^context</body>

<body package="Siren" selector="context:">context: anObject

	context := anObject</body>

<body package="Siren" selector="explanation">explanation

	^explanation</body>

<body package="Siren" selector="explanation:">explanation: aString

	explanation := aString</body>

<body package="Siren" selector="name">name

	^name</body>

<body package="Siren" selector="name:">name: aString

	name := aString</body>
</methods>

<methods>
<class-id>Siren.HATRule</class-id> <category>copying</category>

<body package="Siren" selector="copy">copy

	"Only allow a shallow copy, so that condition and action blocks are
		not duplicated."

	^self shallowCopy</body>
</methods>

<methods>
<class-id>Siren.HATRule</class-id> <category>processing</category>

<body package="Siren" selector="explain">explain

	"Offer an explanation for this rule.  Add a prefix to
		the string to indicate which rule this is."

	context show: 'Rule ', name, ' - ', explanation</body>

<body package="Siren" selector="fire">fire
	"Evaluate the action block with two arguments: this rule and
		this rules context.  Notify the context that we are firing."

	action value: self value: context.
	context show: 'Firing Rule: ', name</body>

<body package="Siren" selector="wantsToFire">wantsToFire

	"Evaluate the condition block with two arguments: this rule and
		this rules context.  Return the results, presumably a boolean."

	^condition value: self value: context</body>
</methods>

<methods>
<class-id>Siren.HATRule</class-id> <category>printing</category>

<body package="Siren" selector="printString">printString

	"Print a simple desciption of this rule when asked."

	^ 'HATRule: ', name</body>
</methods>


<class>
<name>HATKey</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>name scale quality score </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Kernel</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<shared-variable>
<name>BestScore</name>
<environment>Siren.HATKey</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<shared-variable>
<name>ChordValues</name>
<environment>Siren.HATKey</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<shared-variable>
<name>ProgressionValues</name>
<environment>Siren.HATKey</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.HATKey class</class-id> <category>instance creation</category>

<body package="Siren" selector="named:quality:scale:">named: aString quality: aSymbol scale: aCollection
	| key | 

	key := self new.
	key name: aString;
		quality: aSymbol;
		scale: aCollection.

	^key</body>
</methods>

<methods>
<class-id>Siren.HATKey class</class-id> <category>class initialization</category>

<body package="Siren" selector="initialize">initialize
	"Key initialize"

	BestScore := 0.

	ChordValues		:= #( 30 3 2 12 20 2 10 ).

								"		 1	 2	 3	 4	 5	 6	 7"
	ProgressionValues	:= #(	"1"(		50	10	10	20	15	10	10	)
								"2"(		10	10	10	 5	10	 5	10	)
								"3"(		10	10	10	10	 5	10	 5	)
								"4"(		20	 5	10	10	10	 5	10	)
								"5"(		30	 5	 5	10	20	10	10	)
								"6"(		10	10	 5	 5	10	10	10	)
								"7"(		15	10	 5	 5	10	10	10	))</body>
</methods>

<methods>
<class-id>Siren.HATKey class</class-id> <category>class variable reference</category>

<body package="Siren" selector="resetBestScore">resetBestScore

	BestScore := 0</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.HATKey</class-id>
<body>Keys

	Keys represent musical keys and a score indicating the strength of the key
	within the current composition.  Keys are made up of a scale, a quality, and a score.

	This class keeps two class variables, ChordValues and ProgressionValues,
	which are used to update a key's score, depending on the chords under consideration.</body>
</comment>

<methods>
<class-id>Siren.HATKey</class-id> <category>accessing</category>

<body package="Siren" selector="name">name

	^name</body>

<body package="Siren" selector="name:">name: aString

	name := aString</body>

<body package="Siren" selector="quality">quality

	^quality</body>

<body package="Siren" selector="quality:">quality: aSymbol

	quality := aSymbol</body>

<body package="Siren" selector="scale">scale

	^scale</body>

<body package="Siren" selector="scale:">scale: aCollection

	scale := aCollection</body>

<body package="Siren" selector="score">score

	(score isNil)
		ifTrue:	[^0]
		ifFalse:	[^score]</body>

<body package="Siren" selector="score:">score: newValue

	score := newValue asInteger.
	(score &gt; BestScore)
		ifTrue: [BestScore := score]</body>
</methods>

<methods>
<class-id>Siren.HATKey</class-id> <category>testing</category>

<body package="Siren" selector="containsChord:">containsChord: aChord
	| rawNotes cf rawScale | 

	"Answer whether or not the argument, aChord, exists in this key.
		Use the number of notes found in this key over the total number
		of notes in the chord as an indication."

	rawNotes := aChord notes collect: [ :aNote | aNote \\ 12].
	rawScale := scale collect: [ :aNote | aNote \\ 12].
	cf := ((rawNotes asSet intersection: rawScale asSet) size asFloat) / (rawNotes asSet size asFloat).

	"If the chord, as identified, exists in this key, or if some rule has told
		this chord that it does exist in this key, then answer true.  Otherwise false."
	^(((cf &gt;= aChord confidence) &amp; (rawScale includes: aChord root)) | (aChord key = self))</body>

<body package="Siren" selector="isMajor">isMajor

	"answer whether or not this key is major."

	^quality == #major</body>

<body package="Siren" selector="isMinor">isMinor

	"answer whether or not this key is minor."

	^quality == #minor</body>

<body package="Siren" selector="positionOfChord:">positionOfChord: aChord
	| note | 

	"Assuming the argument, aChord, exists in this key,
		answer the index of the root into this scale."

	note := aChord root.
	[note &lt; scale first]
		whileTrue: [note := note + 12].

	^scale indexOf: note</body>
</methods>

<methods>
<class-id>Siren.HATKey</class-id> <category>keeping score</category>

<body package="Siren" selector="strength">strength

	"Key strength is determined as follows:  my score / best key score.
		This is always between 0.0 and 1.0."

	^(self score / BestScore) asFloat</body>

<body package="Siren" selector="updateScore:">updateScore: value

	"Update my score using a weighted average."

	self score: ((0.10 * value) + (0.90 * self score)) rounded</body>

<body package="Siren" selector="updateScoreForChord:">updateScoreForChord: aChord
	| index cf | 

	"If 'aChord' exists in this key, update my score according
		to the value found in the ChordValues table."

	(self containsChord: aChord)
		ifTrue:	[
					index := (self positionOfChord: aChord).
					self updateScore: (aChord confidence * (ChordValues at: index)).
				]
		ifFalse:	[	self updateScore: 0]</body>

<body package="Siren" selector="updateScoreForProgression:and:">updateScoreForProgression: aChord and: prevChord
	| i j |

	"If both arguments exists in this key, update my score according
		to the value found in the ProgressionValues table." 

	(self containsChord: aChord) &amp;
	(self containsChord: prevChord)
		ifTrue:	[
					i := self positionOfChord: prevChord.
					j := self positionOfChord: aChord.
					(( i ~= 0 ) &amp; ( j ~= 0))
						ifTrue: [self updateScore: (( ProgressionValues at: i ) at: j )].
				]
		ifFalse:	[	self updateScore: 0]</body>
</methods>

<methods>
<class-id>Siren.HATKey</class-id> <category>printing</category>

<body package="Siren" selector="printScale">printScale
	| s |

	"Return a string containing my scale."

	s := WriteStream on: ( String new: 16).
	s nextPutAll: name, ': '.
	scale do: [ :aNote | s nextPutAll: (HATEngine noteNames at: ((aNote + 12 \\ 12) +1)) printString, ' '].

	^s contents</body>
</methods>


<class>
<name>HATEngine</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>progression previousChord currentChord chordExperts keyExperts majorKeys minorKeys chordTree transcript log snap </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Kernel</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<shared-variable>
<name>NoteNames</name>
<environment>Siren.HATEngine</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.HATEngine class</class-id> <category>class initialization</category>

<body package="Siren" selector="initialize">initialize

	"HAT initialize"

	NoteNames := #( C Db D Eb E F Gb G Ab A Bb B )</body>

<body package="Siren" selector="noteNames">noteNames

	^NoteNames</body>
</methods>

<methods>
<class-id>Siren.HATEngine class</class-id> <category>instance creation</category>

<body package="Siren" selector="open">open

	"HAT open."

	| model view | 

	model := self new initialize.
	view := HATView newOn: model.
	view controller open</body>
</methods>

<methods>
<class-id>Siren.HATEngine class</class-id> <category>triad expert rules</category>

<body package="Siren" selector="augmentedTriadRules">augmentedTriadRules
	| rules chord | 

	rules := OrderedCollection new: 3.

	rules add: (	ChordRule	name:	'aug. triad, root position' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 4 8 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #Aug) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #Aug.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A augmented triad is made up of two major thirds.  ',
										'It is in root position if the ',
										'root is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'aug. triad, 1st inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 4 8 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 8)
													withQuality: #Aug) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 8).
										chord quality: #Aug.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A augmented triad is made up of two major thirds.  ',
										'It is in 1st inversion if the ',
										'third is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'aug. triad, 2nd inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 4 8 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 4)
													withQuality: #Aug) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 4).
										chord quality: #Aug.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A augmented triad is made up of two major thirds.  ',
										'It is in 2nd inversion if the ',
										'fifth is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="diminishedTriadRules">diminishedTriadRules
	| rules chord | 

	rules := OrderedCollection new: 3.

	rules add: (	ChordRule	name:	'dim. triad, root position' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 3 6 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #dim) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #dim.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished triad is made up of two minor thirds.  ',
										'It is in root position if the ',
										'root is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dim. triad, 1st inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 3 9 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #dim) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #dim.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished triad is made up of two minor thirds.  ',
										'It is in 1st inversion if the ',
										'third is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dim. triad, 2nd inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 6 9 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 6)
													withQuality: #dim) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 6).
										chord quality: #dim.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished triad is made up of two minor thirds.  ',
										'It is in 2nd inversion if the ',
										'fifth is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="majorTriadRules">majorTriadRules
	| rules chord | 

	rules := OrderedCollection new: 3.

	rules add: (	ChordRule	name:	'major triad, root position' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 4 7 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #Maj) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #Maj.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major triad is made up of a major third ',
										'and a minor third.  It is in root position if the ',
										'root is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'major triad, 1st inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 3 8 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 8)
													withQuality: #Maj) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 8).
										chord quality: #Maj.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major triad is made up of a major third ',
										'and a minor third.  It is in 1st inversion if the ',
										'third is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'major triad, 2nd inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 5 9 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #Maj) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #Maj.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major triad is made up of a major third ',
										'and a minor third.  It is in 2nd inversion if the ',
										'fifth is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="minorTriadRules">minorTriadRules
	| rules chord | 

	rules := OrderedCollection new: 3.

	rules add: (	ChordRule	name:	'minor triad, root position' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 3 7 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #min) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #min.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor triad is made up of a minor third ',
										'and a major third.  It is in root position if the ',
										'root is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'minor triad, 1st inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 4 9 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #min) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #min.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor triad is made up of a minor third ',
										'and a major third.  It is in 1st inversion if the ',
										'third is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'minor triad, 2nd inversion' 
							if:		[	:thisRule :context |
										thisRule computeCF: #( 5 8 ).
										((thisRule confidence &gt; 0.2) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #min) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #min.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor triad is made up of a minor third ',
										'and a major third.  It is in 2nd inversion if the ',
										'fifth is the lowest sounding note.'
				).


	^rules</body>
</methods>

<methods>
<class-id>Siren.HATEngine class</class-id> <category>seventh expert rules</category>

<body package="Siren" selector="dimDimSeventhRules">dimDimSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'dd 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 9 )) includes: 9) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #dd7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #dd7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-diminished seventh chord is made up of ',
										'a diminished triad and a diminished seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dd 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 9 )) includes: 6) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #dd7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #dd7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-diminished seventh chord is made up of ',
										'a diminished triad and a diminished seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dd 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 9 )) includes: 3) &amp;
										((context resultsIncludeRoot: (context data notes first + 6)
													withQuality: #dd7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 6).
										chord quality: #dd7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-diminished seventh chord is made up of ',
										'a diminished triad and a diminished seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dd 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 9 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 3)
													withQuality: #dd7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 3).
										chord quality: #dd7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-diminished seventh chord is made up of ',
										'a diminished triad and a diminished seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="dimMajorSeventhRules">dimMajorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'dM 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 11 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 11 )) includes: 11) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #dM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #dM7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-major seventh chord is made up of ',
										'a diminished triad and a major seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dM 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 8 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 8 9 )) includes: 8) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #dM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #dM7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-major seventh chord is made up of ',
										'a diminished triad and a major seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dM 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 5 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 5 6 9 )) includes: 5) &amp;
										((context resultsIncludeRoot: (context data notes first + 6)
													withQuality: #dM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 6).
										chord quality: #dM7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-major seventh chord is made up of ',
										'a diminished triad and a major seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dM 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 1 4 7 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 1)
													withQuality: #dM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 1).
										chord quality: #dM7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-major seventh chord is made up of ',
										'a diminished triad and a major seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="dimMinorSeventhRules">dimMinorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'dm 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 10 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 10 )) includes: 10) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #dm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #dm7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-minor seventh chord is made up of ',
										'a diminished triad and a minor seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dm 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 7 9 )) includes: 7) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #dm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #dm7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-minor seventh chord is made up of ',
										'a diminished triad and a minor seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dm 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 6 9 )) includes: 4) &amp;
										((context resultsIncludeRoot: (context data notes first + 6)
													withQuality: #dm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 6).
										chord quality: #dm7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-minor seventh chord is made up of ',
										'a diminished triad and a minor seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'dm 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 2 5 8 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 2)
													withQuality: #dm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 2).
										chord quality: #dm7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A diminished-minor seventh chord is made up of ',
										'a diminished triad and a minor seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="majorDimSeventhRules">majorDimSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'Md 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 7 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 7 9 )) includes: 9) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #Md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #Md7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-diminished seventh chord is made up of ',
										'a major triad and a diminished seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Md 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 5 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 5 8 )) includes: 5) &amp;
										((context resultsIncludeRoot: (context data notes first + 8)
													withQuality: #Md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 8).
										chord quality: #Md7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-diminished seventh chord is made up of ',
										'a major triad and a diminished seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Md 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 2 5 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 2 5 9 )) includes: 2) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #Md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #Md7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-diminished seventh chord is made up of ',
										'a major triad and a diminished seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Md 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 10 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 3)
													withQuality: #Md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 3).
										chord quality: #Md7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-diminished seventh chord is made up of ',
										'a major triad and a diminished seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="majorMajorSeventhRules">majorMajorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'MM 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 7 11 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 7 11 )) includes: 11) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #MM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #MM7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-major seventh chord is made up of ',
										'a major triad and a major seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'MM 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 7 8 )) includes: 7) &amp;
										((context resultsIncludeRoot: (context data notes first + 8)
													withQuality: #MM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 8).
										chord quality: #MM7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-major seventh chord is made up of ',
										'a major triad and a major seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'MM 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 5 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 5 9 )) includes: 4) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #MM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #MM7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-major seventh chord is made up of ',
										'a major triad and a major seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'MM 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 1 5 8 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 1)
													withQuality: #MM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 1).
										chord quality: #MM7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-major seventh chord is made up of ',
										'a major triad and a major seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="majorMinorSeventhRules">majorMinorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'Mm 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 7 10 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 7 10 )) includes: 10) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #Mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #Mm7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-minor seventh chord is made up of ',
										'a major triad and a minor seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Mm 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 6 8 )) includes: 6) &amp;
										((context resultsIncludeRoot: (context data notes first + 8)
													withQuality: #Mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 8).
										chord quality: #Mm7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-minor seventh chord is made up of ',
										'a major triad and a minor seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Mm 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 5 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 5 9 )) includes: 3) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #Mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #Mm7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-minor seventh chord is made up of ',
										'a major triad and a minor seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'Mm 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 2 6 9 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 2)
													withQuality: #Mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 2).
										chord quality: #Mm7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A major-minor seventh chord is made up of ',
										'a major triad and a minor seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="minorDimSeventhRules">minorDimSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'md 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 7 9 )) includes: 9) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #md7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-diminished seventh chord is made up of ',
										'a minor triad and a diminished seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'md 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 6 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 6 9 )) includes: 6) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #md7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-diminished seventh chord is made up of ',
										'a minor triad and a diminished seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'md 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 2 5 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 2 5 8 )) includes: 2) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #md7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-diminished seventh chord is made up of ',
										'a minor triad and a diminished seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'md 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 6 10 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 3)
													withQuality: #md7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 3).
										chord quality: #md7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-diminished seventh chord is made up of ',
										'a minor triad and a diminished seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="minorMajorSeventhRules">minorMajorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'mM 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 11 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 7 11 )) includes: 11) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #mM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #mM7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-major seventh chord is made up of ',
										'a minor triad and a major seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mM 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 8 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 8 9 )) includes: 8) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #mM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #mM7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-major seventh chord is made up of ',
										'a minor triad and a major seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mM 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 5 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 5 8 )) includes: 4) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #mM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #mM7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-major seventh chord is made up of ',
										'a minor triad and a major seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mM 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 1 4 8 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 1)
													withQuality: #mM7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 1).
										chord quality: #mM7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-major seventh chord is made up of ',
										'a minor triad and a major seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>

<body package="Siren" selector="minorMinorSeventhRules">minorMinorSeventhRules
	| rules chord | 

	rules := OrderedCollection new: 4.

	rules add: (	ChordRule	name:	'mm 7th, root position' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 7 10 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 7 10 )) includes: 10) &amp;
										((context resultsIncludeRoot: (context data notes first)
													withQuality: #mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first).
										chord quality: #mm7.
										chord inversion: 0.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-minor seventh chord is made up of ',
										'a minor triad and a minor seventh.  ',
										'It is in root position if the root of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mm 7th, 1st inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 4 7 9 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 4 7 9 )) includes: 7) &amp;
										((context resultsIncludeRoot: (context data notes first + 9)
													withQuality: #mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 9).
										chord quality: #mm7.
										chord inversion: 1.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-minor seventh chord is made up of ',
										'a minor triad and a minor seventh.  ',
										'It is in 1st inversion if the third of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mm 7th, 2nd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 3 5 8 ).

										((thisRule confidence &gt; 0.5) &amp;
										((context data intersection: #( 3 5 8 )) includes: 3) &amp;
										((context resultsIncludeRoot: (context data notes first + 5)
													withQuality: #mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 5).
										chord quality: #mm7.
										chord inversion: 2.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-minor seventh chord is made up of ',
										'a minor triad and a minor seventh.  ',
										'It is in 2nd inversion if the fifth of the triad ',
										'is the lowest sounding note.'
				).

	rules add: (	ChordRule	name:	'mm 7th, 3rd inversion' 
							if:		[	:thisRule :context |

										thisRule computeCF: #( 2 5 9 ).
										((thisRule confidence &gt; 0.5) &amp;
										((context resultsIncludeRoot: (context data notes first + 2)
													withQuality: #mm7) not))
									]
							then:	[	:thisRule :context |

										chord := context data copy.
										chord root: (chord notes first + 2).
										chord quality: #mm7.
										chord inversion: 3.
										chord confidence: thisRule confidence.

										context results add: chord.
									] 
							because:	'A minor-minor seventh chord is made up of ',
										'a minor triad and a minor seventh.  ',
										'It is in 3rd inversion if the ',
										'seventh is the lowest sounding note.'
				).


	^rules</body>
</methods>

<methods>
<class-id>Siren.HATEngine class</class-id> <category>key expert rules</category>

<body package="Siren" selector="keyExpertRules">keyExpertRules

	^self keyExpertRulesMajor, self keyExpertRulesMinor, self keyExpertRulesMisc</body>

<body package="Siren" selector="keyExpertRulesMajor">keyExpertRulesMajor
	| rules chord | 

	rules := OrderedCollection new: 7.

	rules add: (	HATRule	name:	'key rule, major one' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 1
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ( '  I  \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								]
				).

	rules add: (	HATRule	name:	'key rule, major two' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 2
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' ii \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major three' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 3
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' iii \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major four' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 4
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' IV \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major five' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 5
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('  V  \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major six' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 6
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' vi \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major seven' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 7
										and: [context key isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('vii(dim)\in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	^rules</body>

<body package="Siren" selector="keyExpertRulesMinor">keyExpertRulesMinor
	| rules chord | 

	rules := OrderedCollection new: 7.

	rules add: (	HATRule	name:	'key rule, minor one' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 1
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('  i  \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor two' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 2
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('ii(dim)\in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor three' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 3
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' III \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor four' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 4
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' iv \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor five' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 5
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('  v  \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor six' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 6
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' VI \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, minor seven' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context keyContainsChord)
										and: [context positionOfChord = 7
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' VII \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	^rules</body>

<body package="Siren" selector="keyExpertRulesMisc">keyExpertRulesMisc
	| rules chord key | 

	rules := OrderedCollection new: 5.

	rules add: (	HATRule	name:	'key rule, picardy third' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context oppositeKeyContainsChord )
										and: [context oppositePositionOfChord = 1
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' I-picardy \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, major five borrowed from relative major' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									((context oppositeKeyContainsChord )
										and: [context oppositePositionOfChord = 5
										and: [context key isMinor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: ('  V  \in ', context key name, chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, five of five' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									key :=  context meta majorKeys
												at: ((context keyIndex + 6) \\ 12 + 1).
									((key containsChord: context data )
										and: [(key positionOfChord: context data) = 5
										and: [context data isMajor]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' V/V \in ', context key name,
															chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, seven of five' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									key :=  context meta majorKeys
												at: ((context keyIndex + 6) \\ 12 + 1).
									((key containsChord: context data )
										and: [(key positionOfChord: context data) = 7
										and: [context data isDiminished]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' vii(dim)/V \in ', context key name,
															chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	rules add: (	HATRule	name:	'key rule, seven of six' 
						if:		[	:thisRule :context |
									thisRule confidence: context key strength.
									key :=  context meta majorKeys
												at: ((context keyIndex + 8) \\ 12 + 1).
									((key containsChord: context data )
										and: [(key positionOfChord: context data) = 7
										and: [context data isDiminished]])
								]
						then:	[	:thisRule :context |
									chord := context data copy.
									chord labels add: (' vii(dim)/vi \in ', context key name,
															chord inversionString).
									((chord key isNil)
										or: [context key strength &gt; chord key strength])
										ifTrue: [chord key: context key].

									chord confidence: context key strength.
									context results add: chord.
								] 
				).

	^rules</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.HATEngine</class-id>
<body>HAT

	HAT is the heart of this rule based system.

	It contains all of the methods for setting up the experts and controls
	the communication between them and the user.

	The protocol for processing does the real work in three phases:

	The first phase takes the chord entered by the user and passes
		it through the chord experts.  Opinions make up the first level
		of the chord tree.  The chords with the highest confidence
		are then passed on to phase two.

	Phase two uses the best chords from phase one and passes them to the
		key experts.  The key experts try to fit the generic chords into the different keys.
		Key scores are updated during this phase.

	The final phase uses the results of the second phase to conclude, if possible
		the identity of the user's chord along with the key.

	Following is a description of each protocol.  Many deal with the user interface.
		These are commented very little.

		initialize-release	- setting up and cleaning up.
		accessing			- allowing acces to instance variables.
		transcript			- controls the transcript view and the log file.
		score subview		- controls the bar chart of key scores.
		keyboard subview	- controls the piano keyboard subview, used to enter the chords.
		tree subview		- controls the 'parse' tree subview.
		processing			- does the real work.</body>
</comment>

<methods>
<class-id>Siren.HATEngine</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	| labels key | 

	currentChord := HATChord new initialize.
	progression := OrderedCollection new.

	"Set up the experts, etc."
	self initializeChordExperts.
	self initializeKeys.
	self initializeKeyExperts.

	"FillInTheBlank
		request: 'Name of log file?'
		displayAt: Sensor cursorPoint
		centered: true
		action: [:fileName | fileName] 
		initialAnswer: 'log.HAT'."

	"Open the log file."
	log := (Filename named: "fileName" 'log.HAT') writeStream.

	"BinaryChoice
		message: 'Would you like snapshots taken each time?' 
		displayAt: Sensor cursorPoint 
		centered: true
		ifTrue: [snap := true] 
		ifFalse: [" snap := false "]".

	( self confirm: 'Do you know the key?' ) 
		ifTrue: [
			labels := WriteStream on: String new.
			majorKeys, minorKeys
				do: [ :each | labels nextPutAll: ' ', each name, ' '; cr].
			labels skip: -1.
			key := (Menu labels: labels contents lines: #( 12 ))
					startUp: #anyButton withHeading: 'What is the key?'.
			( key &gt; 0 )
				ifTrue:	[
					( key &lt;= 12 )
						ifTrue:	[( majorKeys at: key ) score: 25]
						ifFalse:	[( minorKeys at: ( key - 12 )) score: 25]]]</body>

<body package="Siren" selector="initializeChordExperts">initializeChordExperts
	| expert | 

	chordExperts := OrderedCollection new.

	self initializeTriadExperts.
	self initializeSeventhExperts</body>

<body package="Siren" selector="initializeKeyExperts">initializeKeyExperts

	keyExperts := OrderedCollection new.

	self initializeMajorKeyExperts.
	self initializeMinorKeyExperts</body>

<body package="Siren" selector="initializeKeys">initializeKeys

	majorKeys := Array new: 12.
	minorKeys := Array new: 12.

	self initializeMajorKeys.
	self initializeMinorKeys</body>

<body package="Siren" selector="initializeMajorKeyExperts">initializeMajorKeyExperts

	keyExperts add: (	KeyExpert
							name: 'C Major'
							keyIndex: 1
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Db Major'
							keyIndex: 2
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'D Major'
							keyIndex: 3
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Eb Major'
							keyIndex: 4
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'E Major'
							keyIndex: 5
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'F Major'
							keyIndex: 6
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Gb Major'
							keyIndex: 7
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'G Major'
							keyIndex: 8
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Ab Major'
							keyIndex: 9
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'A Major'
							keyIndex: 10
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Bb Major'
							keyIndex: 11
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'B Major'
							keyIndex: 12
							keyQuality: #major
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self )</body>

<body package="Siren" selector="initializeMajorKeys">initializeMajorKeys

	majorKeys at: 1
		put: (	HATKey named: 'C'
					quality: #major
					scale: #( 0 2 4 5 7 9 11 )).

	majorKeys at: 2
		put: (	HATKey named: 'Db'
					quality: #major
					scale: #( 1 3 5 6 8 10 12 )).

	majorKeys at: 3
		put: (	HATKey named: 'D'
					quality: #major
					scale: #( 2 4 6 7 9 11 13 )).

	majorKeys at: 4
		put: (	HATKey named: 'Eb'
					quality: #major
					scale: #( 3 5 7 8 10 12 14 )).

	majorKeys at: 5
		put: (	HATKey named: 'E'
					quality: #major
					scale: #( 4 6 8 9 11 13 15 )).

	majorKeys at: 6
		put: (	HATKey named: 'F'
					quality: #major
					scale: #( 5 7 9 10 12 14 16 )).

	majorKeys at: 7
		put: (	HATKey named: 'Gb'
					quality: #major
					scale: #( 6 8 10 11 13 15 17 )).

	majorKeys at: 8
		put: (	HATKey named: 'G'
					quality: #major
					scale: #( 7 9 11 12 14 16 18 )).

	majorKeys at: 9
		put: (	HATKey named: 'Ab'
					quality: #major
					scale: #( 8 10 12 13 15 17 19 )).

	majorKeys at: 10
		put: (	HATKey named: 'A'
					quality: #major
					scale: #( 9 11 13 14 16 18 20 )).

	majorKeys at: 11
		put: (	HATKey named: 'Bb'
					quality: #major
					scale: #( 10 12 14 15 17 19 21 )).

	majorKeys at: 12
		put: (	HATKey named: 'B'
					quality: #major
					scale: #( 11 13 15 16 18 20 22 ))</body>

<body package="Siren" selector="initializeMinorKeyExperts">initializeMinorKeyExperts

	keyExperts add: (	KeyExpert
							name: 'C Minor'
							keyIndex: 1
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Db Minor'
							keyIndex: 2
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'D Minor'
							keyIndex: 3
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Eb Minor'
							keyIndex: 4
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'E Minor'
							keyIndex: 5
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'F Minor'
							keyIndex: 6
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Gb Minor'
							keyIndex: 7
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'G Minor'
							keyIndex: 8
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Ab Minor'
							keyIndex: 9
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'A Minor'
							keyIndex: 10
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'Bb Minor'
							keyIndex: 11
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self ).

	keyExperts add: (	KeyExpert
							name: 'B Minor'
							keyIndex: 12
							keyQuality: #minor
							ruleSet: HATEngine keyExpertRules
							explanationNeeded: false
							meta: self )</body>

<body package="Siren" selector="initializeMinorKeys">initializeMinorKeys

	minorKeys at: 1
		put: (	HATKey named: 'Cm'
					quality: #minor
					scale: #( 0 2 3 5 7 8 10 )).

	minorKeys at: 2
		put: (	HATKey named: 'Dbm'
					quality: #minor
					scale: #( 1 3 4 6 8 9 11 )).

	minorKeys at: 3
		put: (	HATKey named: 'Dm'
					quality: #minor
					scale: #( 2 4 5 7 9 10 12 )).

	minorKeys at: 4
		put: (	HATKey named: 'Ebm'
					quality: #minor
					scale: #( 3 5 6 8 10 11 13 )).

	minorKeys at: 5
		put: (	HATKey named: 'Em'
					quality: #minor
					scale: #( 4 6 7 9 11 12 14 )).

	minorKeys at: 6
		put: (	HATKey named: 'Fm'
					quality: #minor
					scale: #( 5 7 8 10 12 13 15 )).

	minorKeys at: 7
		put: (	HATKey named: 'Gbm'
					quality: #minor
					scale: #( 6 8 9 11 13 14 16 )).

	minorKeys at: 8
		put: (	HATKey named: 'Gm'
					quality: #minor
					scale: #( 7 9 10 12 14 15 17 )).

	minorKeys at: 9
		put: (	HATKey named: 'Abm'
					quality: #minor
					scale: #( 8 10 11 13 15 16 18 )).

	minorKeys at: 10
		put: (	HATKey named: 'Am'
					quality: #minor
					scale: #( 9 11 12 14 16 17 19 )).

	minorKeys at: 11
		put: (	HATKey named: 'Bbm'
					quality: #minor
					scale: #( 10 12 13 15 17 18 20 )).

	minorKeys at: 12
		put: (	HATKey named: 'Bm'
					quality: #minor
					scale: #( 11 13 14 16 18 19 21 ))</body>

<body package="Siren" selector="initializeSeventhExperts">initializeSeventhExperts
	| expert | 

	chordExperts add: (		ChordExpert
								name: 'MM 7th'
								ruleSet: HATEngine majorMajorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'Mm 7th'
								ruleSet: HATEngine majorMinorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'Md 7th'
								ruleSet: HATEngine majorDimSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'mM 7th'
								ruleSet: HATEngine minorMajorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'mm 7th'
								ruleSet: HATEngine minorMinorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'md 7th'
								ruleSet: HATEngine minorDimSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'dM 7th'
								ruleSet: HATEngine dimMajorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'dm 7th'
								ruleSet: HATEngine dimMinorSeventhRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'dd 7th'
								ruleSet: HATEngine dimDimSeventhRules
								explanationNeeded: false
								meta: self )</body>

<body package="Siren" selector="initializeTriadExperts">initializeTriadExperts
	| expert | 

	chordExperts add: (		ChordExpert
								name: 'Major Triad'
								ruleSet: HATEngine majorTriadRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'Minor Triad'
								ruleSet: HATEngine minorTriadRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'Dim. Triad'
								ruleSet: HATEngine diminishedTriadRules
								explanationNeeded: false
								meta: self ).

	chordExperts add: (		ChordExpert
								name: 'Aug. Triad'
								ruleSet: HATEngine augmentedTriadRules
								explanationNeeded: false
								meta: self )</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>accessing</category>

<body package="Siren" selector="currentChord">currentChord

	^currentChord</body>

<body package="Siren" selector="majorKeys">majorKeys

	^majorKeys</body>

<body package="Siren" selector="minorKeys">minorKeys

	^minorKeys</body>

<body package="Siren" selector="transcript">transcript

	^transcript</body>

<body package="Siren" selector="transcript:">transcript: aTextCollector

	transcript := aTextCollector</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>transcript</category>

<body package="Siren" selector="cr">cr

	transcript cr.
	log cr</body>

<body package="Siren" selector="show:">show: aString

	transcript show: aString.
	log nextPutAll: aString</body>

<body package="Siren" selector="showNotes">showNotes


	self show: '		=  =  =  =  =  =  =  =  =  ='; cr; cr.	
	self show: '		The notes are:   '.

	currentChord notes
			do:	[	:aNote |
					self show: (NoteNames at: (aNote + 12 \\ 12) + 1) asString, ' '.
				].

	self cr; cr</body>

<body package="Siren" selector="space">space

	transcript space.
	log space</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>score subview</category>

<body package="Siren" selector="bestKey">bestKey
	| bestKey | 

	bestKey := majorKeys first.

	majorKeys, minorKeys
		do:	[	:aKey	|

				(aKey score &gt; bestKey score)
					ifTrue: [bestKey := aKey].

			].

	^bestKey</body>

<body package="Siren" selector="keyLabel:">keyLabel: index

	(index &gt; 12)
		ifTrue:	[^(minorKeys at: index - 12) name]
		ifFalse:	[^(majorKeys at: index) name]</body>

<body package="Siren" selector="keyScore:">keyScore: index

	( index &gt; 12 )
		ifTrue:	[^( minorKeys at: ( index - 12 )) score]
		ifFalse:	[^( majorKeys at: index ) score]</body>

<body package="Siren" selector="maxKeyScore">maxKeyScore

	^(self bestKey score max: 1)</body>

<body package="Siren" selector="numberOfKeys">numberOfKeys

	^(majorKeys size + minorKeys size)</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>keyboard subview</category>

<body package="Siren" selector="accept">accept

	Cursor execute showWhile: [self process]</body>

<body package="Siren" selector="cancel">cancel

	currentChord := HATChord new initialize.
	chordTree := nil.

	self changed: #currentChord.
	self changed: #chordTree</body>

<body package="Siren" selector="keyboardMenu">keyboardMenu

	^PopUpMenu labels: ' accept \ cancel \ stop ' withCRs
				selectors: #( accept cancel stop )</body>

<body package="Siren" selector="pressKey:">pressKey: aNote

"	Transcript show: aNote printString; cr."
	(currentChord notes includes: aNote)
		ifTrue:	[currentChord notes remove: aNote]
		ifFalse:	[currentChord notes add: aNote]</body>

<body package="Siren" selector="stop">stop

	log close.
	self cancel</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>tree subview</category>

<body package="Siren" selector="bestNode">bestNode
	| aNode | 

	aNode := chordTree.

	[aNode isLeaf]
		whileFalse:
			[aNode := aNode first].

	(aNode == chordTree)
		ifTrue:	[^nil]
		ifFalse:	[^aNode]</body>

<body package="Siren" selector="chordTree">chordTree

	^chordTree</body>

<body package="Siren" selector="inspectionNode:">inspectionNode: aNode

	(aNode notNil)
		ifTrue:	[aNode data inspect]</body>
</methods>

<methods>
<class-id>Siren.HATEngine</class-id> <category>processing</category>

<body package="Siren" selector="keyScoreUpdatePartOne:">keyScoreUpdatePartOne: goodNodes

	(goodNodes isNil)
		ifFalse:	[
			goodNodes do:
				[	:aNode |
						majorKeys, minorKeys
							do:	[ :aKey | aKey updateScoreForChord: aNode data]]].

	self changed: #keyScore</body>

<body package="Siren" selector="keyScoreUpdatePartTwo:">keyScoreUpdatePartTwo: newChords

	newChords do:	[	:aChord |
						majorKeys, minorKeys
							do:	[	:aKey |
									aKey updateScoreForProgression: aChord and: previousChord.
								].
					].

	self changed: #keyScore</body>

<body package="Siren" selector="phaseOne">phaseOne
	| chords notes opinions |

	"initialize variables and then send the user's chord the the chord experts."

	chordTree	:= HatTreeNode new label: 'current chord'.
	chords		:= SortedCollection sortBlock: [ :x :y | x confidence &gt; y confidence].
	notes		:= currentChord notes asSortedCollection asOrderedCollection.

	currentChord notes: notes.
	HATKey resetBestScore.

	self showNotes.
	self show: '- - - Begin Chord Expert Processing - - -'; cr.

	chordExperts do:	[	:anExpert |

							"Ask each expert his opinions."
							opinions		:= anExpert consider: currentChord.

							"If he has some, add them to the chord tree."
							(opinions notNil)
								ifTrue:	[	chordTree	:= HatTreeNode new label: 'current chord'.
											chords addAll: opinions.
											chordTree addChordNodes: chords.
											self changed: #chordTree.
										].
						]</body>

<body package="Siren" selector="phaseThree">phaseThree
	| best ws | 

	"Output the results."

	self cr; show: '		= = = Conclusion = = ='; cr.

	best := self bestNode data.

	(( previousChord isNil ) or: [previousChord key isNil])
		ifTrue:	[self show: '		The Key is ', best key name; cr]
		ifFalse:	[
			(( previousChord key ~= best key ) and: [best key notNil])
				ifTrue:	[self show: '		The Key has changed to ', best key name; cr]]. 

	( best key isNil )
		ifTrue:	[	self show: '		This chord is ', best printString; cr.
					self show: '		This chord does not fit into any key!'; cr; cr; cr.
				]
		ifFalse:	[	self show: '		This chord is ',
						(( best labels first copyReplaceAll: '\' with: ' ' )
						copyReplaceAll: '  ' with: ' ' ); cr.
				].

	"Remember the best chord."
	previousChord := self bestNode data.
	progression addLast: previousChord.

	( progression size &gt; 1 )
		ifTrue: [
			self cr.
			ws := WriteStream on: String new.
			ws nextPutAll: '		'.
			ws nextPutAll: progression first key name, ': '.
			ws nextPutAll: ( progression first labels first removeLeadingSpaces copyUpTo: Character space ).
			( 2 to: progression size )
				do: [ :i |
					ws nextPutAll: ' '.
					((( progression at: i ) key ~= ( progression at: ( i - 1 )) key )
					and: [(( progression at: i ) key notNil ) 
					and: [( progression at: ( i - 1 )) key notNil]])
						ifTrue: [ws nextPutAll: ( progression at: i ) key name, ': '].
					(( progression at: i ) labels isEmpty not )
						ifTrue: [ws nextPutAll: (( progression at: i ) labels first removeLeadingSpaces copyUpTo: Character space )]].

			self show: ws contents].

	self cr; cr.

	currentChord := HATChord new initialize.
	self changed: #currentChord.
	Smalltalk primGarbageCollect</body>

<body package="Siren" selector="phaseTwo">phaseTwo
	| goodNodes chords opinions tmpChords |

	"Select only those chords with the highest confidence."
	goodNodes	:= chordTree childrenSelect: [ :aNode | aNode weight = chordTree first weight].
	chords		:= SortedCollection sortBlock: [ :x :y | x confidence &gt; y confidence].

	"Update the key scores based on the 'good' chords."
	self keyScoreUpdatePartOne: goodNodes.

	self cr; show: '- - - Begin Key Expert Processing - - -'; cr.

	goodNodes									"For each 'good' chord..."
		do:	[	:aNode |

				tmpChords := SortedCollection sortBlock: [ :x :y | x confidence &gt; y confidence].
				keyExperts
					do:	[	:anExpert |			"For each key expert..."

							"ask each expert his opinion."
							opinions := anExpert consider: aNode data.

							"If he has one, add it to the chord tree."
							(opinions isEmpty not)
								ifTrue:	[	aNode removeAll.
											tmpChords addAll: opinions.
											aNode addChordNodes: tmpChords.
											self changed: #chordTree.
										].
						].
				chords addAll: tmpChords.
			].
	"Update the key scores based on the progression of the last two chord."
	(previousChord notNil)
		ifTrue:	[self keyScoreUpdatePartTwo: chords].

	"Recompute the chord confidence factors."
	chords		do:	[ :aChord |	aChord computeFinalCF].

	"Update the chord tree with new weights."
	goodNodes	do:	[ :aNode |	aNode childrenDo: [ :sub | sub updateWeight].
								aNode sortChildren.
					].
	self changed: #chordTree</body>

<body package="Siren" selector="process">process

	self phaseOne.
	self phaseTwo.
	self phaseThree</body>
</methods>


<class>
<name>Expert</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>name ruleSet data triggeredRules firedRules explanationNeeded meta results </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Kernel</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.Expert class</class-id> <category>instance creation</category>

<body package="Siren" selector="name:ruleSet:explanationNeeded:meta:">name: aString ruleSet: aCollection explanationNeeded: aBoolean meta: anObject
	| expert | 

	expert := self new.
	expert name: aString;
			ruleSet: aCollection;
			explanationNeeded: aBoolean;
			meta: anObject.

	^expert</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.Expert</class-id>
<body>Expert

	An expert represents a very small domain of knowledge.  Each expert consists
	a local data area and a local rule set.  Experts provide a context for evaluating
	the local rule set, based on the local data.  Experts also have a meta-object
	of some sort, to whom the may refer for information, and by whom they
	are started.  Each expert also keeps a flag indicating whether or not an explanation
	is needed for each rule that fires.

	The expert is essentially the inference engine which drives the rules.  This process
	is divided into three steps:  selecting the rules which have been triggered; resolving
	conficts between the triggered rules, and finally firing the selected rules.

	Experts only keep a copy of each rule they are given.  This is because each rule
	must be initialized to recognize a given expert as their context.  Since rules are sometimes
	shared between experts, a copy is made to avoid confusion.</body>
</comment>

<methods>
<class-id>Siren.Expert</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize

	"Set some instance variables to their default values."

	ruleSet := OrderedCollection new</body>

<body package="Siren" selector="release">release

	"Break all circular references as an aid to garbage collection."

	meta := nil.
	self releaseRuleSet</body>

<body package="Siren" selector="releaseRuleSet">releaseRuleSet

	"Tell each rule to release."

	ruleSet do: [ :aRule | aRule release].
	ruleSet := nil</body>
</methods>

<methods>
<class-id>Siren.Expert</class-id> <category>accessing</category>

<body package="Siren" selector="data">data

	^data</body>

<body package="Siren" selector="explanationNeeded:">explanationNeeded: aBoolean

	explanationNeeded := aBoolean</body>

<body package="Siren" selector="meta">meta

	^meta</body>

<body package="Siren" selector="meta:">meta: anObject

	meta := anObject</body>

<body package="Siren" selector="name">name

	^name</body>

<body package="Siren" selector="name:">name: aString

	name := aString</body>

<body package="Siren" selector="results">results

	^results</body>

<body package="Siren" selector="ruleSet">ruleSet

	^ruleSet</body>

<body package="Siren" selector="ruleSet:">ruleSet: aCollection

	"If we already have a ruleSet, release the old one before
		accepting the new one."

	(ruleSet notNil)
		ifTrue: [self releaseRuleSet].

	"Make a copy of the rules, and set each to
		use this expert as its context."

	ruleSet := aCollection copy.
	ruleSet do: [ :aRule | aRule context: self]</body>
</methods>

<methods>
<class-id>Siren.Expert</class-id> <category>rule access</category>

<body package="Siren" selector="addRule:">addRule: aRule
	| newRule |

	"Make a copy of the rule, and make this
		expert the rules context."

	newRule := aRule copy.
	newRule context: self.
	ruleSet add: newRule</body>

<body package="Siren" selector="addRules:">addRules: aCollection

	"add the collection of rules to the current rule set."

	aCollection do: [ :aRule | self addRule: aRule]</body>

<body package="Siren" selector="removeRule:">removeRule: aRule
	| removedRule | 

	"Remove a given rule, releasing it."

	removedRule := ruleSet remove: aRule ifAbsent: [self error: 'Rule not found'].
	removedRule release</body>
</methods>

<methods>
<class-id>Siren.Expert</class-id> <category>processing</category>

<body package="Siren" selector="consider:">consider: newData

	"Set the local data to 'newData'.
		Initalize the collection for results.
		And process."

	data := newData.
	results := OrderedCollection new.

	self preProcess.		"Allow preprocessing, if necessary."
	self process.
	self postProcess.		"Allow postprocessing, if necessary."

	^results</body>

<body package="Siren" selector="fireRules">fireRules

	"Fire all of the rules surviving conflict resolution.
		If an explanation is needed, ask for one."

	firedRules do:	[ :aRule |	aRule fire.
								(explanationNeeded) ifTrue: [aRule explain]
					]</body>

<body package="Siren" selector="postProcess">postProcess

	"Do nothing.  This method allows subclasses to do
		preprocessing if they need to."</body>

<body package="Siren" selector="preProcess">preProcess

	"Do nothing.  This method allows subclasses to do
		postprocessing if they need to."</body>

<body package="Siren" selector="process">process

	self selectRules.
	self resolveConflicts.
	self fireRules</body>

<body package="Siren" selector="resolveConflicts">resolveConflicts
	| best | 

	"Allow only the rule with the highest confidence
		to be fired.  This is the default.  Subclasses
		will over-ride this method, as needed."

	(triggeredRules isEmpty)
		ifTrue:	[	firedRules := OrderedCollection new.]
		ifFalse:	[	best := triggeredRules first.
					triggeredRules do:	[	: aRule |
											( aRule confidence &gt; best confidence )
												ifTrue: [best := aRule]
										].
					firedRules := OrderedCollection with: best.
				]</body>

<body package="Siren" selector="selectRules">selectRules

	"Gather all of the rules that 'want to fire'."

	triggeredRules := ruleSet select: [ :aRule | aRule wantsToFire]</body>
</methods>

<methods>
<class-id>Siren.Expert</class-id> <category>private</category>

<body package="Siren" selector="request:">request: aMessage

	"Ask the meta-object for something."

	^meta perform: aMessage</body>

<body package="Siren" selector="request:with:">request: aMessage with: args

	"Ask the meta-object for something."

	^meta perform: aMessage with: args</body>

<body package="Siren" selector="show:">show: aString

	"Ask the meta-object to show 'aString' to the user.
		Add a prefix indicating who is doing the sending."

	meta show: 'Expert: ', name, '; ', aString; cr</body>
</methods>


<initialize>
<class-id>Siren.HATKey</class-id>
</initialize>

<initialize>
<class-id>Siren.HATEngine</class-id>
</initialize>


</st-source>
