<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 9, 2007 at 9:58:36 am</time-stamp>


<class>
<name>ChordRule</name>
<environment>Siren</environment>
<super>Siren.HATRule</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Harmony</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.ChordRule</class-id>
<body>Chord Rule

	Chord Rules are just like generic rules except for the following:


		additional		additional methods have been added as needed
		methods:		to simplify the condition parts of these rules.</body>
</comment>

<methods>
<class-id>Siren.ChordRule</class-id> <category>private</category>

<body package="Siren" selector="computeCF:">computeCF: aPattern
	| s ch top bottom |

	"Compute a confidence value based on the 'exactness
		of match' between the current chord and the argument.
		The confidence is basically the intersection over the union."

	s	:= aPattern asSet.
	ch	:= context data.

	top := 1.0 + (ch intersection: s) size.
	bottom := 1.0 + (ch union: s) size.

	confidence := ( top / bottom asFloat)</body>
</methods>


<class>
<name>HATChord</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>notes root quality inversion confidence labels key </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Harmony</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.HATChord</class-id>
<body>Chords

	A chord, in addition to being a collection of notes, contains additional
	information indicating the root of the chord, the quality of the chord,
	the inversion, if any, etc.  If the chord has been identified as belonging
	to a particular key, then it will also indicate the key, and will have a
	label, identifying it within the key.</body>
</comment>

<methods>
<class-id>Siren.HATChord</class-id> <category>initialize</category>

<body package="Siren" selector="initialize">initialize

	"Set some instance variables to their default values."

	notes := OrderedCollection new.
	labels := OrderedCollection new</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>accessing</category>

<body package="Siren" selector="confidence">confidence

	^confidence</body>

<body package="Siren" selector="confidence:">confidence: aFloat

	confidence := aFloat</body>

<body package="Siren" selector="inversion">inversion

	^inversion</body>

<body package="Siren" selector="inversion:">inversion: anInteger

	inversion := anInteger</body>

<body package="Siren" selector="key">key

	^key</body>

<body package="Siren" selector="key:">key: aKey

	key := aKey</body>

<body package="Siren" selector="labels">labels

	^labels</body>

<body package="Siren" selector="labels:">labels: aCollection

	labels := aCollection</body>

<body package="Siren" selector="notes">notes

	^notes</body>

<body package="Siren" selector="notes:">notes: aCollection

	notes := aCollection</body>

<body package="Siren" selector="quality">quality

	^quality</body>

<body package="Siren" selector="quality:">quality: aString

	quality := aString</body>

<body package="Siren" selector="root">root

	^root</body>

<body package="Siren" selector="root:">root: anInteger

	"Be sure the root has been reduced."

	root := anInteger + 12 \\ 12</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>testing</category>

<body package="Siren" selector="isDiminished">isDiminished

	"Answer whether or not this chord is of a diminished quality."

	^ ((quality == #dim)
		or: [quality == #dm7])</body>

<body package="Siren" selector="isMajor">isMajor

	"Answer whether or not this chord is of a major quality."

	^ ((quality == #Maj)
		or: [quality == #Mm7])</body>

<body package="Siren" selector="isMinor">isMinor

	"Answer whether or not this chord is of a minor quality."

	^(quality == #min)</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>computing</category>

<body package="Siren" selector="asVector">asVector
	| vec first | 

	"Return a vector indicating the distance between each note
		in this chord and the lowest sounding note."

	vec		:= self reduced.		"reduce it"
	first	:= vec first.			"get the lowest sounding note."
	vec remove: first.

	vec		:= vec collect: [ :aNote | aNote - first].	"build the vector"

	"Return the vector,
		after duplicates have been removed,
		and it has been sorted."

	^vec asSet asSortedCollection asOrderedCollection</body>

<body package="Siren" selector="computeFinalCF">computeFinalCF

	"Adjust the confidence of this chord to reflect the
		strength of the key in which it belongs."

	(key isNil)
		ifTrue:	[confidence := 0.0]
		ifFalse:	[confidence := key strength].

	^confidence</body>

<body package="Siren" selector="reduced">reduced
	| oldNotes newNotes first tmp |

	"Return a copy of the notes of this chord, after they have been reduced.
		The only consideration, when reducing a chord, is the requirement that the
		lowest sounding note be preserved as the lowest sounding."

	oldNotes	:= notes copy.			"copy"
	first		:= oldNotes first.		"get the lowest sounding note."
	oldNotes remove: first.
	first		:= first \\ 12.			"reduce it."

	"gather all the remaining notes, reducing each of them,
		but being sure that none become lower than the original
		lowest sounding note."

	newNotes := oldNotes collect:	[	:aNote |
										tmp := aNote \\ 12.
										(tmp &lt; first)
											ifTrue:	[tmp + 12]
											ifFalse:	[tmp]
								].

	"Replace the lowest sounding note."
	newNotes add: first.

	"Return this new collection,
		after duplicates have been removed,
		and it has been sorted."

	^newNotes asSet asSortedCollection asOrderedCollection</body>

<body package="Siren" selector="shiftNotes">shiftNotes
	| low high | 

	"Rotate the notes in this chord so that
		the lowest sounding note becomes the highest."

	low := notes first.		"Get the lowest note."
	high := notes last.		"Get the highest."
	notes remove: low.

	"Make the lowest higher than the highest."
	[low &lt; high] whileTrue: [low := low + 12].

	notes addLast: low</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>set functions</category>

<body package="Siren" selector="intersection:">intersection: aSet

	"Return the intersection of the distance vector for this chord
		and the argument, aSet."

	^self asVector asSet intersection: aSet</body>

<body package="Siren" selector="union:">union: aSet

	"Return the union of the distance vector for this chord
		and the argument, aSet."

	^self asVector asSet union: aSet</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>copying</category>

<body package="Siren" selector="copy">copy

	"Return a copy of this chord
		whose labels and confidence have been reset."

	^self deepCopy labels: OrderedCollection new;
					confidence: 0.0</body>
</methods>

<methods>
<class-id>Siren.HATChord</class-id> <category>printing</category>

<body package="Siren" selector="genericNodeLabel">genericNodeLabel

	| names string | 

	"Return the generic label for this chord; i.e. 'C Major'."

	(root isNil) ifTrue: [^'a Chord'].

	string := (HATEngine noteNames at: (root \\ 12) + 1) asString, ' ', quality asString.

	(inversion = 1) ifTrue:	[	string := string, '\1st inv.' withCRs].
	(inversion = 2) ifTrue:	[	string := string, '\2nd inv.' withCRs].
	(inversion = 3) ifTrue:	[	string := string, '\3rd inv.' withCRs].

	^string</body>

<body package="Siren" selector="inversionString">inversionString

	"Return a string indicating the inversion of this chord."

	(inversion isNil) ifTrue:	[	^''].
	(inversion = 1) ifTrue:	[	^', 1st inv.'].
	(inversion = 2) ifTrue:	[	^', 2nd inv.'].
	(inversion = 3) ifTrue:	[	^', 3rd inv.'].

	^''</body>

<body package="Siren" selector="nodeLabel">nodeLabel
	| names string | 

	"Return the label to be used in the chord tree
		for this chord."

	(root isNil) ifTrue: [^'a Chord'].

	"If this chord has not yet been identified in any key,
		return the generic label.
		Otherwise return the specific label."

	(labels isEmpty)
		ifTrue:	[^self genericNodeLabel]
		ifFalse:	[^self specificNodeLabel]</body>

<body package="Siren" selector="printString">printString

	| names string | 

	"Return a string describing this chord."

	(root isNil) ifTrue: [^'a Chord'].

	string := (HATEngine noteNames at: (root + 12 \\ 12) + 1) asString, ' ', quality asString.

	(inversion = 1) ifTrue:	[	string := string, ', 1st inv.'].
	(inversion = 2) ifTrue:	[	string := string, ', 2nd inv.'].
	(inversion = 3) ifTrue:	[	string := string, ', 3rd inv.'].

	^string</body>

<body package="Siren" selector="specificNodeLabel">specificNodeLabel
	| string | 

	"Return the first label for this chord, modified for the chord tree."

	string := labels first asString copyReplaceAll: ', ' with: '\'.

	^string withCRs</body>
</methods>


<class>
<name>ChordExpert</name>
<environment>Siren</environment>
<super>Siren.Expert</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Harmony</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.ChordExpert</class-id>
<body>Chord Experts

	Chord Experts are just like generic Experts, except for the following:

		processing:		each Chord Expert when considering a chord,
						considers permutations of the chord by rotating the
						notes within the chord.

		conflict			all rules with confidence &gt; 70% are fired.
		resolution:

		additional		additional methods have been added as needed
		methods:		to simplify some of the rules.</body>
</comment>

<methods>
<class-id>Siren.ChordExpert</class-id> <category>processing</category>

<body package="Siren" selector="process">process

	"Consider the chord in each of it inversions."

	(data notes size)
		timesRepeat:	[	self selectRules.
							self resolveConflicts.
							self fireRules.
							data := data deepCopy shiftNotes.
						]</body>

<body package="Siren" selector="resolveConflicts">resolveConflicts

	"Allow all rules with &gt; 70% confidence to fire."

	firedRules := triggeredRules select: [ :aRule | aRule confidence &gt; 0.7]</body>
</methods>

<methods>
<class-id>Siren.ChordExpert</class-id> <category>testing</category>

<body package="Siren" selector="resultsIncludeRoot:withQuality:">resultsIncludeRoot: aNote withQuality: aSymbol

	"Answer whether or not a chord whose root is 'aNote' and whose
		quality is 'aSymbol' has been added to the collection of
		candidates."

	results do:	[	:aChord |
					(aChord root = (aNote + 12 \\ 12)) &amp; (aChord quality = aSymbol)
						ifTrue:	[^true]
				].

	^false</body>
</methods>


<class>
<name>KeyExpert</name>
<environment>Siren</environment>
<super>Siren.Expert</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>keyIndex keyQuality </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-HAT-Harmony</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.KeyExpert class</class-id> <category>instance creation</category>

<body package="Siren" selector="name:keyIndex:keyQuality:ruleSet:explanationNeeded:meta:">name: aString keyIndex: anInteger keyQuality: aSymbol ruleSet: aCollection explanationNeeded: aBoolean meta: anObject
	| expert | 

	expert := self new.
	expert name: aString;
			keyIndex: anInteger;
			keyQuality: aSymbol;
			ruleSet: aCollection;
			explanationNeeded: aBoolean;
			meta: anObject.

	^expert</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.KeyExpert</class-id>
<body>Key Experts

	Key Experts are just like generic Experts, except for the following:

		key:			each key expert has an associated key. 

		conflict			all rules are fired, if their confidence is in the top 50% of
		resolution:		rules wanting to be fired.

		additional		additional methods have been added as needed
		methods:		to simplify some of the rules.

Instance Variables:
	keyIndex	&lt;?type?&gt;  comment
	keyQuality	&lt;?type?&gt;  comment
</body>
</comment>

<methods>
<class-id>Siren.KeyExpert</class-id> <category>accessing</category>

<body package="Siren" selector="keyIndex">keyIndex

	^keyIndex</body>

<body package="Siren" selector="keyIndex:">keyIndex: anInteger

	keyIndex := anInteger</body>

<body package="Siren" selector="keyQuality">keyQuality

	^keyQuality</body>

<body package="Siren" selector="keyQuality:">keyQuality: aSymbol

	keyQuality := aSymbol</body>
</methods>

<methods>
<class-id>Siren.KeyExpert</class-id> <category>key access</category>

<body package="Siren" selector="key">key

	"Return my associated key."

	^self keyAt: keyIndex</body>

<body package="Siren" selector="keyAt:">keyAt: anInteger
	| index | 

	"Return a key whose quality is the same as mine,
		and whose index is 'anInteger'."

	index := anInteger.

	[index &gt; 12]			"reduce the index to keep it in bounds."
		whileTrue:
			[index := index - 12].

	(keyQuality == #major)
		ifTrue:	[^meta majorKeys at: index]
		ifFalse:	[^meta minorKeys at: index]</body>

<body package="Siren" selector="oppositeKey">oppositeKey

	"Return the key with the same index but opposite
		quality as my associated key."

	^self oppositeKeyAt: keyIndex</body>

<body package="Siren" selector="oppositeKeyAt:">oppositeKeyAt: anInteger
	| index | 

	"Return a key whose quality is opposite mine,
		and whose index is 'anInteger'."

	index := anInteger.

	[index &gt; 12]			"reduce the index to keep it in bounds."
		whileTrue:
			[index := index - 12].

	(keyQuality == #minor)
		ifTrue:	[^meta majorKeys at: index]
		ifFalse:	[^meta minorKeys at: index]</body>
</methods>

<methods>
<class-id>Siren.KeyExpert</class-id> <category>testing</category>

<body package="Siren" selector="keyContainsChord">keyContainsChord

	"Answer whether or not my associated key contains
		the chord currently under consideration."

	^self key containsChord: data</body>

<body package="Siren" selector="oppositeKeyContainsChord">oppositeKeyContainsChord

	"Answer whether or not my opposite key contains
		the chord currently under consideration."

	^self oppositeKey containsChord: data</body>

<body package="Siren" selector="oppositePositionOfChord">oppositePositionOfChord

	"Return the position of the chord currently under
		consideration, in my opposite key."

	^self oppositeKey positionOfChord: data</body>

<body package="Siren" selector="positionOfChord">positionOfChord

	"Return the position of the chord currently under
		consideration, in my associated key."

	^self key positionOfChord: data</body>
</methods>

<methods>
<class-id>Siren.KeyExpert</class-id> <category>processing</category>

<body package="Siren" selector="resolveConflicts">resolveConflicts

	"Allow only above-average rules to fired."

	firedRules := triggeredRules select: [ :aRule | aRule confidence &gt; (meta bestKey strength * 0.5)]</body>
</methods>



</st-source>
