<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 8, 2007 at 4:44:39 pm</time-stamp>


<class>
<name>PaleoUtil</name>
<environment>Siren</environment>
<super>UI.Model</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars>db debug </class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<shared-variable>
<name>DirName</name>
<environment>Siren.PaleoUtil</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<shared-variable>
<name>DB</name>
<environment>Siren.PaleoUtil</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<shared-variable>
<name>Formats</name>
<environment>Siren.PaleoUtil</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<shared-variable>
<name>Debug</name>
<environment>Siren.PaleoUtil</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.PaleoUtil class</class-id> <category>class initialization</category>

<body package="Siren" selector="initialize">initialize
	"PaleoUtil withAllSubclassesDo: [ :c | c initialize]"

	self DB: nil.
	debug := false
</body>
</methods>

<methods>
<class-id>Siren.PaleoUtil class</class-id> <category>accessing</category>

<body package="Siren" selector="DB">DB
	"Answer the DataBase root."
	"YYYJD setupDB"
	"YYYJD DB"

	^DB			"or db"</body>

<body package="Siren" selector="DB:">DB: aDB
	"Set the DataBase root."
	"YYYJD setupDB"
	"YYYJD DB"

	DB := aDB
"or use the class inst var.
	db := aDB"</body>
</methods>

<methods>
<class-id>Siren.PaleoUtil class</class-id> <category>database</category>

<body package="Siren" selector="browseDB">browseDB
	"YYYJD browseDB"

	| names sizes dict |
	names := (self DB objectSetsCollect: [ :os | os name]) asSortedCollection.
	sizes := names collect: [ :na | (self osNamed: na) getCount; 
			where: #name ne: nil; execute].
	dict := IdentityDictionary new.
	1 to: names size do:
		[ :in |
		Transcript cr; show: '	OS: ', (names at: in), ' -- ', 
					(sizes at: in) printString, ' elements'.
		dict at: (names at: in) asSymbol put: (sizes at: in)].
	Transcript cr</body>

<body package="Siren" selector="freeDB">freeDB
	"PaleoUtil withAllSubclasses do: [ :c | c freeDB]"
	"MinneStoreDB instanceCount"
	"YYYJD setupDB"

	DB := nil.			"clear class var and class inst var."
	db := nil.
	Object initializePropertiesFields.
	Smalltalk garbageCollect</body>

<body package="Siren" selector="objectSetNamed:">objectSetNamed: nam
	"Answer the named YYYJD DataBase object set."
	"YYYJD setupDB"
	"YYYJD osNamed: #LPC"

	^self DB objectSetNamed: nam asSymbol ifAbsent: [Siren objectSetNamed: nam]</body>

<body package="Siren" selector="objectSetNamed:ifAbsent:">objectSetNamed: nam ifAbsent: blok
	"Answer the named YYYJD DataBase object set."
	"YYYJD setupDB"
	"YYYJD osNamed: #LPC ifAbsent: [nil]"
	"YYYJD osNamed: #zz ifAbsent: [nil]"

	| os |
	os := self DB objectSetNamed: nam asSymbol ifAbsent: [nil].
	os ifNotNil: [^os].
	^Siren osNamed: nam.</body>

<body package="Siren" selector="osContents:">osContents: nam
	"Answer the entire contents of the named YYYJD DataBase object set."
	"YYYJD setupDB"
	"YYYJD osContents: #VoiceSounds"

	^self from: nam where: #name ne: nil</body>

<body package="Siren" selector="osNamed:">osNamed: nam
	"Answer the named YYYJD DataBase object set."
	"YYYJD setupDB"
	"YYYJD osNamed: #LPC"

	^self DB objectSetNamed: nam asSymbol ifAbsent: [Siren objectSetNamed: nam]</body>

<body package="Siren" selector="osNamed:ifAbsent:">osNamed: nam ifAbsent: blok
	"Answer the named YYYJD DataBase object set."
	"YYYJD setupDB"
	"YYYJD osNamed: #LPC ifAbsent: [nil]"
	"YYYJD osNamed: #zz ifAbsent: [nil]"

	| os |
	os := self DB objectSetNamed: nam asSymbol ifAbsent: [nil].
	os ifNotNil: [^os].
	^Siren osNamed: nam.</body>

<body package="Siren" selector="osNames">osNames
	"Answer all the YYYJD DataBase &amp; object set names."
	"YYYJD osNames"

	| coll |
	coll := (self DB objectSetsCollect: [ :os | os name]).
	coll addAll: Siren objectSets keys.
	^coll asSortedCollection.
</body>

<body package="Siren" selector="osSize:">osSize: nam
	"Answer the size of the named YYYJD DataBase object set."
	"YYYJD osSize: #VoiceSounds"

	^(self osNamed: nam) getCount; where: #name ne: nil; execute
</body>

<body package="Siren" selector="printDBsizes">printDBsizes
	"YYYJD printDBsizes"

	| names sizes "dict" |
	names := self osNames.
	sizes := names collect: [ :nam | self osSize: nam].
	Transcript cr; show: 'Database ', self DB name, 
			' -- ', sizes size printString, ' object sets, dumped on ',
			DateAndTime now printString; cr.
"	dict := IdentityDictionary new."
	1 to: names size do:
		[ :in |
		Transcript show: '	ObjSet: ', ((names at: in) asString padTo: 12),
				'	-- ', (sizes at: in) printString, ' elements.'; cr.
	"	dict at: (names at: in) asSymbol put: (sizes at: in)"].
	Transcript cr; cr.
"	^dict"</body>

<body package="Siren" selector="save">save
	self DB save.
</body>
</methods>

<methods>
<class-id>Siren.PaleoUtil class</class-id> <category>queries</category>

<body package="Siren" selector="from:named:">from: osName named: val
	"Answer the set of objects with the given name."

	^(self DB objectSetNamed: osName asSymbol) getAll; 
			where: #name eq: val; execute</body>

<body package="Siren" selector="from:where:eq:">from: osName where: prop eq: val
	"Answer the set of objects that have the given prop = the given value."

	^(self DB objectSetNamed: osName asSymbol) getAll; 
			where: prop eq: val; execute</body>

<body package="Siren" selector="from:where:gt:">from: osName where: prop gt: val
	"Answer the set of objects that have the given prop &gt; the given value."

	^(self DB objectSetNamed: osName asSymbol) getAll; 
			where: prop gt: val; execute</body>

<body package="Siren" selector="from:where:lt:">from: osName where: prop lt: val
	"Answer the set of objects that have the given prop &lt; the given value."

	^(self DB objectSetNamed: osName asSymbol) getAll; 
			where: prop lt: val; execute</body>

<body package="Siren" selector="from:where:ne:">from: osName where: prop ne: val
	"Answer the set of objects that have the given prop ~= the given value."

	^(self objectSetNamed: osName asSymbol) getAll; 
			where: prop ne: val; execute</body>
</methods>

<methods>
<class-id>Siren.PaleoUtil class</class-id> <category>enumerating</category>

<body package="Siren" selector="do:on:">do: blok on: nam
	"Iterate over the members of the object set"
	"OperaDB do: [ :item | Transcript show: item name; cr] on: #Wozzeck"

	(self osContents: nam) do: blok
</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->



<class>
<name>YYYJD</name>
<environment>Siren</environment>
<super>Siren.PaleoUtil</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.YYYJD class</class-id> <category>setup</category>

<body package="Siren" selector="setupAll">setupAll
	"Build the Paleo database for YYYJD"
	"YYYJD setupAll"
	"YYYJD setupAll. PaleoDemo setupAll"
	"YYYJD freeDB"
"	EventListExplorer open: (YYYJD someInstance)."
"	SMSBrowser openOn: (YYYJD DB) "

	debug := true.
										"Build empty DB and object sets"
	YYYJD setupDB.
	YYYJD setupObjectSets.				"Add the local cached object sets."
										"Create a framework event list"
	YYYJD setupScore.
										"Load some voice snd files"
	YYYJD loadSndFiles: 'Content:Sound:YYYJD:EC:r3:src:#.snd' into: #VoiceSounds.
										"or load all of it"
										"Load some bell sco files"
	YYYJD loadScoFiles: 'Content:Sound:YYYJD:Bells:B3:*.sc' into: #BellScores.
										"Do LPC analysis on loaded sounds"
	YYYJD doLPCOn: (YYYJD osContents: #VoiceSounds) intoOSet: #LPC.
										"Do PVoc analysis on loaded sounds"
	YYYJD doPVocOnSounds: (YYYJD osContents: #VoiceSounds) 
									intoOSet: #VoiceSpectra.
										"Print DB stats."
	YYYJD printDBsizes</body>
</methods>

<methods>
<class-id>Siren.YYYJD class</class-id> <category>lpc mgmnt</category>

<body package="Siren" selector="doLPCOn:intoOSet:">doLPCOn: snds intoOSet: lpcOS
	"Read in a hierarchy of sound files from the given ditrectory 
	 into the named DB object sets."

	"YYYJD doLPCFolder: 'Content:Sound:YYYJD:ec:read3' intoOSet: #LPC"
	"YYYJD setupDB"
	"YYYJD printDBsizes"

	| os |
	os := self osNamed: lpcOS.		"LPC object set"
	snds do: [ :snd | self doLPC: snd storeTo: os].
	debug ifTrue: [Transcript show: 'Saving LPC DB...'].
	os save.							"save object set"
	debug ifTrue: [Transcript cr; cr].
</body>
</methods>

<methods>
<class-id>Siren.YYYJD class</class-id> <category>voice pvoc</category>

<body package="Siren" selector="doPVoc:size:dec:storeTo:">doPVoc: snd size: n dec: d storeTo: os
	"Execute the PhaseVocoder on the given sound object, storing the resulting spectrum."

	| pv sp |
	debug ifTrue: [Transcript cr; show: 'PVoc on sound ', snd name; cr].
	pv := PhaseVocoder new.
	pv sound: snd.
	pv date: DateAndTime now.
	pv version: '1.0.0'.
	pv fftLen: n.
	pv winSize: n.
	pv decim: d.
	pv analyzeFFT.
	sp := pv spectrum.
	sp sound freeData.
	sp date: DateAndTime now.
	sp version: #'1.0.0'.
	os ifNotNil:
		[debug ifTrue: [Transcript show: 'Storing spectrum to DB'; cr].
		os store: sp]</body>

<body package="Siren" selector="doPVocOnFiles:into:">doPVocOnFiles: pat into: spectOS
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."
	"YYYJD doPVocOnFiles: 'Content:Sound:YYYJD:EC:read3:#.snd' 
					into: #VoiceSpectra"

	| os1 fils bas di |
	os1 := self osNamed: spectOS.		"spectrum object set"
	bas := pat asDirectoryName.
	di := bas asDirectoryName.
	fils := (FileDirectory on: bas) fileNamesMatching: pat asFilename.
	bas := bas asFilename.
	fils ifEmpty: [^Transcript show: 'No files found.'; cr].
	fils do:
		[ :fi | | snd |
		snd := self loadSndFile: fi in: di dir: bas.
		self doPVoc: snd size: 512 dec: 1.0 storeTo: os1].

	debug ifTrue: [Transcript show: 'Saving snd DB...'].
	os1 save.
	debug ifTrue: [Transcript cr; cr]</body>

<body package="Siren" selector="doPVocOnSounds:intoOSet:">doPVocOnSounds: snds intoOSet: osName
	"Analyze the sounds in the given set of objects; store spectra to DB."
	"YYYJD doPVocOnSounds:
		(YYYJD from: #VoiceSounds where: #folder eq: #read3 ) 
				intoOSet: #VoiceSpectra"
	"YYYJD setupDB"
	"YYYJD printDBsizes"

	| os |
	os := self osNamed: osName.				"PVoc object set"
											"Iterate over snds"
	snds do: [ :snd | self doPVoc: snd size: 512 dec: 1.0 storeTo: os].

	debug ifTrue: [Transcript show: 'Saving LPC DB...'].
	os save.									"save object set"
	debug ifTrue: [Transcript cr; cr].
</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->



<class>
<name>FourMagicSentences</name>
<environment>Siren</environment>
<super>Siren.PaleoUtil</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.FourMagicSentences class</class-id> <category>setup</category>

<body package="Siren" selector="createAGAOsounds">createAGAOsounds
	"Set up a collection of 5000 sounds based on the AGAO samples"
	"FourMagicSentences createAGAOsounds"
	"(Siren osNamed: #NSMSSAGAOSounds) explore"

	| dir fils snds samps durs sum num ran rat sos |
	Transcript cr; show: 'Creating AGAO Sounds obj set.'; cr.
	sos := PaleoObjectSet new name: #NSMSSAGAOSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.
	ran := Random new.
	rat := 44.1.
	num := 5000.
	dir := 'Content:Sound:4 Magic Sentences:'.
	fils := #('agao.3a.2m' 'agao.3b.2m').
	snds := OrderedCollection new: fils size.
	fils do: [ :fnam |
		snds add: (StoredSound headerFromFile: dir, fnam)].
	durs := snds collect: [ :sn | sn duration asMsec value].
	sum := durs sum asFloat.
	durs := durs collect: [ :d | d asFloat / sum].
	samps := OrderedCollection new: num.
	1 to: num do: [ :in | | start stop snd comp d |
		snd := snds atRandom: ran.
		d := snd duration asMsec value.
		start := ran next * (d - 200).					"starting msec"
		stop := ran next * 100 + 100 + start.			"dur = 100 - 200 msec"
		stop := stop min: d.
		comp := ComponentSound on: snd 
						from: (start * rat) truncated 
						to: (stop * rat) truncated.
		comp name: in printString.
		samps add: comp].
	(self osNamed: #NSMSSAGAOSounds) storeAll: samps; save.
	Transcript show: 'done'; cr.
	^samps</body>

<body package="Siren" selector="createSTsounds">createSTsounds
	"Set up a collection of 5000 sounds based on the ST samples"
	"FourMagicSentences createSTsounds"
	"(Siren osNamed: #NSMSSSTSounds) inspect"

	| sos dir fils snds samps durs sum num ran rat |
	Transcript cr; show: 'Creating ST Sounds obj set.'; cr.
	sos := PaleoObjectSet new name: #NSMSSSTSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.
	ran := Random new.
	rat := 44.1.
	num := 5000.
	dir := 'Content:Sound:SimpleTruths:justin_b:'.
	fils := (FileDirectory on: dir) fileNamesMatching: '*.snd'.
	snds := OrderedCollection new: fils size.
	fils do: [ :fnam |
		snds add: (StoredSound headerFromFile: dir, fnam)].
	durs := snds collect: [ :sn | sn duration asMsec value].
	sum := durs sum asFloat.
	durs := durs collect: [ :d | d asFloat / sum].
	samps := OrderedCollection new: num.
	1 to: num do: [ :in | | start stop snd comp d |
		snd := snds atRandom: ran.
		d := snd duration asMsec value.
		start := ran next * (d / 1.5).					"starting msec"
		stop := ran next * 500 + 200 + start.			"dur =200 - 700 msec"
		stop := stop min: d.
		comp := ComponentSound on: snd 
						from: (start * rat) truncated 
						to: (stop * rat) truncated.
		comp name: in printString.
		samps add: comp].
	(self osNamed: #NSMSSSTSounds) storeAll: samps; save.
	Transcript cr; show: '4MS sounds created'; cr.
	^samps</body>

<body package="Siren" selector="createTMYBsounds">createTMYBsounds
	"Set up a collection of 5000 sounds based on the AGAO samples"
	"FourMagicSentences createTMYBsounds"
	"(Siren osNamed: #NSMSSTMYBSounds) explore"

	| dir fils snds samps durs sum num ran rat sos |
	Transcript cr; show: 'Creating TMYB Sounds obj set.'; cr.
	sos := PaleoObjectSet new name: #NSMSSTMYBSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.
	ran := Random new.
	rat := 44.1.
	num := 5000.
	dir := 'Content:Sound:TMYB:src:'.
	fils := (FileDirectory on: dir) fileNamesMatching: 's#.#.snd'.
	snds := OrderedCollection new: fils size.
	fils do: [ :fnam |
		snds add: (StoredSound headerFromFile: dir, fnam)].
	durs := snds collect: [ :sn | sn duration asMsec value].
	sum := durs sum asFloat.
	durs := durs collect: [ :d | d asFloat / sum].
	samps := OrderedCollection new: num.
	1 to: num do: [ :in | | start stop snd comp d |
		snd := snds atRandom: ran.
		d := snd duration asMsec value.
		start := ran next * (d - 300).					"starting msec"
		stop := ran next * 200 + 200 + start.			"dur = 200 - 400 msec"
		stop := stop min: d.
		comp := ComponentSound on: snd 
						from: (start * rat) truncated 
						to: (stop * rat) truncated.
		comp name: in printString.
		samps add: comp].
	(self osNamed: #NSMSSTMYBSounds) storeAll: samps; save.
	Transcript show: 'done'; cr.
	^samps</body>

<body package="Siren" selector="createYYYJDsounds">createYYYJDsounds
	"Set up a collection of 5000 sounds based on the YYYJD samples"
	"FourMagicSentences createYYYJDsounds"
	"(Siren osNamed: #NSMSSYYYJDSounds) inspect"

	| sos samps num ran rat dir fils snds durs sum |
	Transcript cr; show: 'Creating YYYJD Sounds obj set.'; cr.
	sos := PaleoObjectSet new name: #NSMSSYYYJDSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.
	ran := Random new.
	rat := 44.1.
	num := 5000.
	dir := 'Content:Sound:YYYJD:ec:r3:src:'.
	fils := (FileDirectory on: dir) fileNamesMatching: '#.snd'.
	snds := OrderedCollection new: fils size.
	fils do: [ :fnam |
		snds add: (StoredSound headerFromFile: dir, fnam)].
	durs := snds collect: [ :sn | sn duration asMsec value].
	sum := durs sum asFloat.
	durs := durs collect: [ :d | d asFloat / sum].
	samps := OrderedCollection new: num.
	1 to: num do: [ :in | | start stop snd comp d |
		snd := snds atRandom: ran.
		d := snd duration asMsec value.
		start := ran next * (d / 1.5).					"starting msec"
		stop := ran next * 500 + 200 + start.			"dur =200 - 700 msec"
		stop := stop min: d.
		comp := ComponentSound on: snd 
						from: (start * rat) truncated 
						to: (stop * rat) truncated.
		comp name: in printString.
		samps add: comp].
	(self osNamed: #NSMSSYYYJDSounds) storeAll: samps; save.
	Transcript cr; show: '4MS sounds created'; cr.
	^samps</body>

<body package="Siren" selector="setupDB">setupDB
	"Initialize the FoMaSen database. Add obj set for the score."
	"FourMagicSentences setupDB"

	| fileSize branchSize |
	fileSize := 1 "1024" .
	branchSize := 1024 "20".
	debug := true.
"Add 4MaSen Score"
	debug ifTrue: [Transcript cr; show: 'Creating score object set.'; cr].
	(self DB addObjectSetNamed: #FoMaSen) cache;
		objectsPerFile: fileSize;
		storesClass: EventList;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #size domain: SmallInteger.

"Save All"
	debug ifTrue: [Transcript show: 'Saving all'; cr].
	self DB save.
	debug ifTrue: [Transcript show: 'Done'; cr; cr].</body>

<body package="Siren" selector="setupObjectSets">setupObjectSets
	"Set up the Siren cached object sets for NSMSS sounds and generators."
	"FourMagicSentences setupObjectSets"
	"(Siren osNamed: #NSMSSSounds) explore"

	| sos |
	debug := true.
"Sounds Cached Obj Set"
	debug ifTrue: [Transcript cr; show: 'Creating Sounds obj set.'; cr].
	sos := PaleoObjectSet new name: #NSMSSAGAOSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.

	Transcript show: 'Object sets cached.'; cr.

</body>
</methods>

<methods>
<class-id>Siren.FourMagicSentences class</class-id> <category>enumerating</category>

<body package="Siren" selector="cloud1">cloud1
	"Create a selection cloud on a FMSen sound set."
	"FourMagicSentences cloud1"
	"FourMagicSentences saveCloud"

	| snds egen elist ran |
	Cursor read showWhile:
		[snds := self osContents: #NSMSSSounds].
	egen := SelectionCloud dur: 12 sec
		voice: snds 
		density: (LinearFunction from: #((0 10) (0.7 20) (1 2))) .
	Cursor execute showWhile:
		[elist := egen eventList].
	ran := Random new.
	ran next.
	elist eventsDo: [ :ev | ev at: #position put: (ran next * 2.0) - 1.0].

	^elist "inspect"		"rather than explore since it may be very big."</body>

<body package="Siren" selector="cloudFrom:fcn:">cloudFrom: set fcn: fcn
	"Create a selection cloud on a FMSen sound set."
	"FourMagicSentences cloudFrom: #NSMSSSTSounds 
					fcn: (LinearFunction from: #((0 10) (0.7 20) (1 2)))"

	| snds egen elist ran |
	Cursor read showWhile:
		[snds := self osContents: set].
	egen := SelectionCloud dur: 12 sec voice: snds density: fcn.
	Cursor execute showWhile:
		[elist := egen eventList].
	ran := Random new.
	ran next.
	elist eventsDo: [ :ev | ev at: #position put: (ran next * 2.0) - 1.0].
	^elist</body>

<body package="Siren" selector="cloudParameterMap">cloudParameterMap
	"Answer up the parameter map for the default instance."
	"Instrument format
		[ delT, \sampler, \fname, from, to, ampl, pos ] "
	"FourMagicSentences saveCloud"

	| map |
	map := OrderedCollection new: 8.
	map add: [ :ass :li | '	[ ', 
			((((li nextTimeFor: ass) asSec value roundTo: 0.00001) 
					printPlaces: 7) padTo: 8)].
	map add: '\sampler'.
	map add: [ :e | (#('agao.3a.2m' 'agao.3b.2m') indexOf: (e voice sound name))
						printString].
	map add: [ :e | ((e voice start printString) padTo: 8)].
	map add: [ :e | ((e voice stop printString) padTo: 8)].
	map add: [ :e | ((e ampl value printPlaces: 6) padTo: 8)].
	map add: [ :e | ((e position roundTo: 0.001) printPlaces: 4)].
	^map</body>

<body package="Siren" selector="cloudParameterMap:">cloudParameterMap: snds
	"Answer up the parameter map for the default instance."
	"Instrument format
		[ delT, \sampler, \fname, from, to, ampl, pos ] "
	"FourMagicSentences saveCloud"

	| map |
	map _ OrderedCollection new: 8.
"	map add: [ :ass :li | '	[ ', ((((li nextTime: ass) asSec value roundTo: 0.00001) 
					printPlaces: 7) padTo: 8)].
"	map add: [ :ass :li | '	[ ', (((ass key asSec value roundTo: 0.00001) 
					printPlaces: 7) padTo: 8)].
	map add: '\sampler'.
	map add: [ :e | (snds indexOf: (e voice sound name)) printString].
	map add: [ :e | ((e voice start printString) padTo: 8)].
	map add: [ :e | ((e voice stop printString) padTo: 8)].
	map add: [ :e | ((e ampl value printPlaces: 6) padTo: 8)].
	map add: [ :e | ((e position roundTo: 0.001) printPlaces: 4)].
	^map</body>

<body package="Siren" selector="filesForSet:">filesForSet: set
	"Answer the list of sound files used in the given object set (KLUDJ)"
	"FourMagicSentences filesForSet: #NSMSSSTSounds"

	| dir |
	set == #NSMSSSTSounds ifTrue:
		[dir := 'Content:Sound:SimpleTruths:justin_b:'.
		^(FileDirectory on: dir) fileNamesMatching: '*.snd'].
	set == #NSMSSYYYJDSounds ifTrue:
		[dir := 'Content:Sound:YYYJD:ec:r3:src:'.
		^(FileDirectory on: dir) fileNamesMatching: '#.snd'].
	set == #NSMSSAGAOSounds ifTrue:
		[dir := 'Content:Sound:4 Magic Sentences:'.
		^(FileDirectory on: dir) fileNamesMatching: 'agao*.2m'].
	set == #NSMSSTMYBSounds ifTrue:
		[dir := 'Content:Sound:TMYB:src:'.
		^(FileDirectory on: dir) fileNamesMatching: 's#.#.snd'].</body>

<body package="Siren" selector="saveCloud">saveCloud
	"Store a selection cloud to a SC score file."
	"FourMagicSentences saveCloud"

	| l s n |
	l := self cloud1.
	n := 'cloud1.sc'.
	FileDirectory deleteFilePath: n.
	s := SuperColliderVoice onFileNamed: n.
	s parameterMap: (self cloudParameterMap).
	s header: ('// SC2 Score for file sample noise clouds; generated on ',
		Date today printString, ' at ',  Time now printString, '

// Instrument format [ delT, \sampler, \fname, fromSamp toSamp, ampl, pos ]

score = #[
').
	Cursor write showWhile:
		[s play: l; close].
	(FileStream named: n) edit</body>

<body package="Siren" selector="saveCloudFrom:fcn:to:">saveCloudFrom: set fcn: fcn to: n
	"Store a selection cloud to a SC score file."
	"TimeProfileBrowser onBlock: [FourMagicSentences saveCloudFrom: #NSMSSYYYJDSounds 
					fcn: (LinearFunction from: #((0 200) (0.2 600) (0.8 100) (1 20))) 
					to: 'cloud5.sc' ]"

	| l s fils |
	fils := FourMagicSentences filesForSet: set.
	l := self cloudFrom: set fcn: fcn.
	FileDirectory deleteFilePath: n.
	s := SuperColliderVoice onFileNamed: n.
	s parameterMap: (self cloudParameterMap: fils).
	s header: ('// SC2 Score for file sample noise clouds; generated on ',
		Date today printString, ' at ',  Time now printString, '
// Instrument format [ delT, \sampler, \fname, fromSamp toSamp, ampl, pos ]
#[
').
	Cursor write showWhile:
		[s play: l; close].
"	(FileStream named: n) edit"</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->



<class>
<name>RotatingBellCluster</name>
<environment>Siren</environment>
<super>Siren.Cluster</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars>partials att dec velocityFcn spectralFcn densityFcn </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.RotatingBellCluster class</class-id> <category>examples</category>

<body package="Siren" selector="b2aFunctions">b2aFunctions
	"Answer an array of 3 functions for the bell cluster"
	" | arr |
	arr := self b2aFunctions.
	FunctionView openOnFunctions: 
			(Array with: (arr first) normalize with: (arr at: 2) normalize with: (arr last) with: (nil))
		withColors: (Array with: Color blue darker with: Color red with: Color black with: Color green) 
		x: 512 y: 256"

		^Array 			"Spectral weighting function"
		with: (LinearFunction from: 
				#((0 1.5) (0.4 0.8) (1 0.5)))
					"Time/density function"
		with: (ExponentialFunction from: 
					"Mapping of pitch to angular velocity"
				#((0 20 -2) (0.3 60 -5) (0.7 50 2) (1 30 0)))
		with: (ExponentialFunction from: 
				#((0 0 3) (1 1 0)))</body>

<body package="Siren" selector="b2Data">b2Data
	"Answer the data for the b2 series of bell partials."

	"	Name	Freq		Ampl "
	^#(	'2ba'	1.6180		15599
		'2bx'	1.0000		21007
		'2b1'	0.6180		8560 
		'2b2'	0.3819		14415
		'2b3'	0.2361		17874
		'2b4'	0.1459		21573
		'2b5'	0.0901		22103
		'2b6'	0.0557		23614
		'2b7'	0.0344		23269
		'2b8'	0.0212		23582
		'2b9'	0.0131		21869
		'2b0'	0.0081		21063 )</body>

<body package="Siren" selector="run">run
	"Create, process, and write out a rotating bell cluster from the stored data."
	"RotatingBellCluster run"
	"TemplateMorph on: (self setup: #b2Data functions: #b2aFunctions)"

	| l s n |
	l := (self setup: #b2Data functions: #b2aFunctions) eventList.
	n := 'panner2.sc'.
	s := SuperColliderVoice onFileNamed: n.
	s play: l; close.
	(FileStream named: n) edit</body>

<body package="Siren" selector="setup:functions:">setup: dataSelector functions: fcnSelector
	"(RotatingBellCluster setup: #b2Data functions: #b2aFunctions) eventList explore"

	| i dat parts clust fcns |
	parts := OrderedCollection new: 16.
"Load partial data"
	dat := self perform: dataSelector.
	i := 1.
	[i &lt; dat size] whileTrue:
		[parts addFirst: (BellPartial named: (dat at: i) freq: (dat at: i + 1) 
				ampl: (dat at: i + 2) dur: 10.7).
		i := i + 3].
	clust := self new partials: parts.
"Load function data"
	fcns := self perform: fcnSelector.
	clust spectralFcn: (fcns at: 1).	"Spectral weighting function"
	clust densityFcn: (fcns at: 2).	"Time/density function"
	clust velocityFcn: (fcns at: 3).	"Mapping of pitch to angular velocity"
"Set bell cluster parameters"
	clust att: 2.0 sec.
	clust dec: 2.0 sec.
	clust duration: 90.0.
	clust pitch: 14.07 Hz.
	clust ampl: -7.2 dB.
	clust voice: #panner2.
	clust name: #'rBell.b2.14c'.
"Answer bell cluster"
	^clust
</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.RotatingBellCluster</class-id> <category>accessing</category>

<body package="Siren" selector="att">att
	"Answer the receiver's 'att'."

	^att</body>

<body package="Siren" selector="att:">att: anObject
	"Set the receiver's instance variable 'att' to be anObject."

	att := anObject</body>

<body package="Siren" selector="dec">dec
	"Answer the receiver's 'dec'."

	^dec</body>

<body package="Siren" selector="dec:">dec: anObject
	"Set the receiver's instance variable 'dec' to be anObject."

	dec := anObject</body>

<body package="Siren" selector="densityFcn">densityFcn
	"Answer the receiver's 'densityFcn'."

	^densityFcn</body>

<body package="Siren" selector="densityFcn:">densityFcn: anObject
	"Set the receiver's instance variable 'densityFcn' to be anObject."

	densityFcn := anObject</body>

<body package="Siren" selector="partials">partials
	"Answer the receiver's 'partials'."

	^partials</body>

<body package="Siren" selector="partials:">partials: anObject
	"Set the receiver's instance variable 'partials' to be anObject."

	partials := anObject</body>

<body package="Siren" selector="spectralFcn">spectralFcn
	"Answer the receiver's 'spectralFcn'."

	^spectralFcn</body>

<body package="Siren" selector="spectralFcn:">spectralFcn: anObject
	"Set the receiver's instance variable 'spectralFcn' to be anObject."

	spectralFcn := anObject</body>

<body package="Siren" selector="velocityFcn">velocityFcn
	"Answer the receiver's 'velocityFcn'."

	^velocityFcn</body>

<body package="Siren" selector="velocityFcn:">velocityFcn: anObject
	"Set the receiver's instance variable 'velocityFcn' to be anObject."

	velocityFcn := anObject</body>
</methods>

<methods>
<class-id>Siren.RotatingBellCluster</class-id> <category>scheduling</category>

<body package="Siren" selector="dump:">dump: fname
	"Dump the receiver as a SC2 score to the file with the given name."
	"RotatingBellCluster setup: #b2Data"
	"Instrument format
		[ delT, 'panner', fname, dur, att, dec, ampl, angVel ] "

	| fi co ti vel pos amp den par nPar pDur delay ran ind |
	fi := FileStream named: fname.	"output file"
	co := ' ,	'.
	ti := 0.							"timer"
	ran := Random new.
	pDur := partials first dur.		"duration of partials"
	nPar := partials size.			"number of partials"
"Loop thrugh time."
	[ti &lt; duration value] whileTrue: 
"Select partial to add"
		[ind := ran next.
		amp := spectralFcn at: ind.	"random pos in spectral function"
		par := partials at: ((ind * nPar) truncated + 1).
"Set partial parameters"
		vel  := velocityFcn at: ind.
"Increment time"
		pos := ti / duration value.
		den := densityFcn at: pos.
		delay := pDur / den.
"Write out event."
		fi nextPutAll: '[ ', (delay printString truncateTo: 5), co, 
			'\panner', co, par name, co,
			pDur printString, co, 
			att printString, co, dec printString, co, 
			(amp printString truncateTo: 6), co, 
			(vel printString truncateTo: 6), '],'; cr.
		ti := ti + delay].
	fi close.
	fi edit.</body>

<body package="Siren" selector="eventList">eventList
	"Answer the event list for the receiver."
	"(RotatingBellCluster setup: #b2Data functions: #b2aFunctions) eventList explore"

	| pattern ti vel pos amp den par nPar pDur delay ran ind |
	pattern _ EventList newNamed: #B2Cluster.
	ti := 0.							"timer"
	ran := Random new.
	pDur := partials first dur.		"duration of partials"
	nPar := partials size.			"number of partials"
"Loop through time."
	[ti &lt; duration value] whileTrue: 
"Select partial to add"
		[ind := ran next.
		amp := spectralFcn at: ind.	"random pos in spectral function"
		par := partials at: ((ind * nPar) truncated + 1).
"Set partial parameters"
		vel  := velocityFcn at: ind.
"Increment time"
		pos := ti / duration value.
		den := densityFcn at: pos.
		delay := pDur / den.
"Add event to list."
		pattern add:
			(MusicEvent new
				dur: pDur;
				instrument: '\panner';
				partial: par name;
				ampl: amp * (32000 / par ampl);
				attack: att; 
				decay: dec; 
				angularVelocity: vel)
			at: ti.
		ti := ti + delay].
	^pattern</body>
</methods>


<class>
<name>NSMSS</name>
<environment>Siren</environment>
<super>Siren.PaleoUtil</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.NSMSS class</class-id> <category>setup</category>

<body package="Siren" selector="setupObjectSets">setupObjectSets
	"Set up the Siren cached object sets for NSMSS sounds and generators."
	"NSMSS setupObjectSets"
	"(Siren osNamed: #NSMSSSounds) explore"

	| sos |
	debug := true.
"Sounds Cached Obj Set"
	debug ifTrue: [Transcript cr; show: 'Creating Sounds obj set.'; cr].
	sos := PaleoObjectSet new name: #NSMSSSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.

	Transcript show: 'Object sets cached.'; cr.

</body>
</methods>

<methods>
<class-id>Siren.NSMSS class</class-id> <category>sounds</category>

<body package="Siren" selector="createAGAOsounds">createAGAOsounds
	"Set up a collection of 5000 sounds based on the AGAO samples"
	"NSMSS setupObjectSets. NSMSS createAGAOsounds"
	"(Siren osNamed: #NSMSSSounds) explore"

	| dir fils snds samps durs sum num ran rat |
	ran := Random new.
	rat := 44.1.
	num := 5000.
	dir := 'Content:Sound:4 Magic Sentences:'.
	fils := #('agao.3a.2m' 'agao.3b.2m').
	snds := OrderedCollection new: fils size.
	fils do: [ :fnam |
		snds add: (StoredSound headerFromFile: dir, fnam)].
	durs := snds collect: [ :sn | sn duration asMsec value].
	sum := durs sum asFloat.
	durs := durs collect: [ :d | d asFloat / sum].
	samps := OrderedCollection new: num.
	1 to: num do: [ :in | | start stop snd comp d |
		snd := snds atRandom: ran.
		d := snd duration asMsec value.
		start := ran next * (d - 2000).					"starting msec"
		stop := ran next * 2500 + 500 + start.				"stop msec"
		stop := stop min: d.
		comp := ComponentSound on: snd 
						from: (start * rat) truncated 
						to: (stop * rat) truncated.
		comp name: in printString.
		samps add: comp].
	(self osNamed: #NSMSSSounds) storeAll: samps; save.
	^samps</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->



<class>
<name>PaleoDemo</name>
<environment>Siren</environment>
<super>Siren.PaleoUtil</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>setup</category>

<body package="Siren" selector="setupAll">setupAll
	"Load the test data sets"
	"PaleoDemo setupAll"

	PaleoDemo setupDB.
	PaleoDemo loadMikrokosmos.
	PaleoDemo addSHARC.
	PaleoDemo addFunctions.
	PaleoDemo addGeneratorsTo: (PaleoDemo osNamed: #Generators).
	PaleoDemo addGeneratorsTo: (PaleoDemo osNamed: #Generators2).
	PaleoDemo printDBsizes
</body>

<body package="Siren" selector="setupDB">setupDB
	"YYYJD setupDB. 
		PaleoDemo setupDB.
		PaleoDemo loadMikrokosmos.
		PaleoDemo addSHARC.
		PaleoDemo addGenerators.
		PaleoDemo addFunctions.
		PaleoDemo printDBsizes.
		SMSBrowser openOn: (PaleoDemo DB)"

	| fileSize branchSize sos |
	fileSize := 1 "1024" .
	branchSize := 1024 "20".
"Add Mikro Score"
	debug ifTrue: [Transcript show: 'Creating mikro object set.'; cr].
	(self DB addObjectSetNamed: #Mikro) cache;
		objectsPerFile: fileSize;
		storesClass: EventList;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #number domain: SmallInteger getter: 'item at: #number';
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #size domain: SmallInteger.
"Add SHARC spectra"
	debug ifTrue: [Transcript show: 'Creating spectrum object set.'; cr].
	(self DB addObjectSetNamed: #InstrumentSpectra)
		objectsPerFile: fileSize;
		storesClass: SHARCInstrument;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol.
"Add EventGenerators"
	debug ifTrue: [Transcript show: 'Creating generator object set.'; cr].
	(self DB addObjectSetNamed: #Generators) cache;
		objectsPerFile: fileSize;
		storesClass: EventList;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #events domain: SmallInteger getter: 'item events size';
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #size domain: SmallInteger.
"Add envelope function set"
	debug ifTrue: [Transcript show: 'Creating envelope function object set.'; cr].
	sos := PaleoObjectSet new name: #Functions; 
					fields: #(name domain range size date version).
	Siren objectSets at: sos name put: sos.

	debug ifTrue: [Transcript show: 'Saving all'; cr].
	self DB save.
	debug ifTrue: [Transcript show: 'Done'; cr; cr].

</body>
</methods>

<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>local DB</category>

<body package="Siren" selector="dbDir">dbDir
	"Answer the Paleo score DB directory."

	^'RAM:Database:Scores'
"	^'Content:Paleo:Scores'"</body>
</methods>

<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>timbres</category>

<body package="Siren" selector="addSHARC">addSHARC
	"Load the 1377 spectra of the SHARC database into the DB."
	"PaleoDemo addSHARC"
	"SMSBrowser openOn: (YYYJD DB)"

	| orch os |
	SHARCInstrument loadOrchDir: 'Content:Sound:sharc'.
	orch := SHARCInstrument orchestra.
	os := PaleoDemo osNamed: #InstrumentSpectra.
	os storeAll: orch values.
	debug ifTrue: [Transcript show: 'Saving spect DB...'].
	os save.
	debug ifTrue: [Transcript show: '  done.'; cr; cr].
</body>
</methods>

<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>MIDI</category>

<body package="Siren" selector="loadMIDIFile:in:dir:to:">loadMIDIFile: fi in: bas dir: di to: os
	"Load a MIDI file into an object set"

	| sco |
	[Cursor read showWhile:
		[sco := Sound headerFromFile: (bas, Filename separatorString, di, Filename separatorString, fi)]]
			ifError: [ :err :rcvr |
				debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				^ " self  "self halt].
	sco at: #folder put: di.
	sco at: #name: put: fi sansLastPeriodSuffix.
	sco at: #version put: (fi betweenPeriodsIfEmpty: '1.0.0').
	Cursor normal show.
	[Cursor write showWhile: 
		[os store: sco]]
			ifError: [ :err :rcvr | 
				debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				self halt].</body>

<body package="Siren" selector="loadMikrokosmos">loadMikrokosmos
	"Load a few selections from Mikrokosmos as MIDI files"
	"PaleoDemo setupDB"
	"PaleoDemo loadMikrokosmos"
	"PaleoDemo printDBsizes"
	"SMSBrowser openOn: (YYYJD DB)"

	| dict di os |
	dict := self mikrokosmosFiles.
	di := 'Content:Scores:Bartok:MIKRO'.
	os := PaleoDemo osNamed: #Mikro.
	dict associationsDo:
		[ :ass | | nu ti na msc sco |
		nu := ass key.
		ti := ass value.
		nu &lt; 100 ifTrue: [na := di, '0', nu printString, '.MID']
				ifFalse: [na := di, nu printString, '.MID'].
		Transcript show: '	Reading score "', ti, '":  '.
		Cursor read showWhile:
			[[msc := MIDIFileReader scoreFromFileNamed: na.
				sco := msc asEventList]
			ifError: [ :err :rcvr | 
				Debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				self halt]].
		Transcript show: sco events size printString, ' events'; cr.
		sco name: ti.
		sco number: nu.
		sco at: #MIDIscore put: msc.
		Cursor write showWhile: 
		[[os store: sco]
			ifError: [ :err :rcvr | 
				Debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				self halt]]].
	debug ifTrue: [Transcript show: 'Saving sco DB...'].
	os save.
	debug ifTrue: [Transcript show: '  done.'; cr; cr].
</body>

<body package="Siren" selector="mikrokosmosFiles">mikrokosmosFiles
	"Answer the list of files in mikrokosmos"

	^((37 -&gt; 'In Lydian mode'),
		(40 -&gt; 'In Yugoslavian mode'),
		(73 -&gt; 'Sixths and triads'),
		(82 -&gt; 'Scherzo'),
		(83 -&gt; 'Melody with interruptions'),
		(97 -&gt; 'Notturno'),
		(101 -&gt; 'Diminished fifth'),
		(113 -&gt; 'Bulgarian rhythm'),
		(122 -&gt; 'Chords together and in opposition'),
		(123 -&gt; 'Staccato and legato (a, b)'),
		(124 -&gt; 'Staccato'),
		(125 -&gt; 'Boating'),
		(126 -&gt; 'Change of time'),
		(127 -&gt; 'New Hungarian folk song'),
		(130 -&gt; 'Village joke'),
		(131 -&gt; 'Fourths'),
		(133 -&gt; 'Syncopation'),
		(136 -&gt; 'Whole-tone scales'),
		(140 -&gt; 'Free variations'),
		(141 -&gt; 'Subject and reflection'),
		(142 -&gt; 'From the diary of a fly'),
		(144 -&gt; 'Minor seconds, major sevenths'),
		(145 -&gt; 'Chromatic invention (a, b, c)'),
		(147 -&gt; 'March'),
		(148 -&gt; 'Dance in Bulgarian rhythm n. 1'),
		(149 -&gt; 'Dance in Bulgarian rhythm n. 2'),
		(152 -&gt; 'Dance in Bulgarian rhythm n. 5'),
		(153 -&gt; 'Dance in Bulgarian rhythm n. 6'))
</body>
</methods>

<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>functions</category>

<body package="Siren" selector="addFunctions">addFunctions
	"Add EventFunctions"
	"PaleoDemo addFunctions."
	"YYYJD setupDB. PaleoDemo setupDB. PaleoDemo addFunctions. 
			PaleoDemo printDBsizes."
	"SMSBrowser openOn: (YYYJD DB)"

	| ra os b1 b2 |
	os := PaleoDemo osNamed: #Functions.
	ra := Random new.
	Transcript show: 'Creating functions.'; cr.
	b1 := [ra next * 0.15 + 0.01].
	b2 := [ra next * 0.3 + 0.4].
	1 to: 8 do:					"Make 8 random ADSR envelopes"
		[ :i | | fcn |
		fcn := ExponentialFunction a: (b1 value) d:  (b1 value) 
								s:(b2 value) r: (b1 value * 3).
		fcn name: 'Exp', i printString.
		[Cursor write showWhile: [os store: fcn]]
			ifError: [ :err :rcvr | Cursor normal show
				debug ifTrue: [Transcript show: '		', err; cr]]].

	b1 := [ra next * 0.2].
	b2 := [ra next * 0.3 + 0.4].
	1 to: 8 do:				"Make 8 random linear wanders"
		[ :i | | fcn x y pts |
		pts := OrderedCollection new.
		x := 0.0.
		y := ra next.
		[x &lt; 1.0] whileTrue:
			[pts add: x @ y.
			x := x + (ra next * 0.3) + 0.05.
			y := y + (ra next * 0.3) + 0.05.
			y &gt; 1.0 ifTrue: [y := 1.0 / y].
			y &lt; 0.0 ifTrue: [y := y negated]].
		pts last x: 1.0.
		fcn := LinearFunction from: pts.
		fcn name: 'Lin', i printString.
		[Cursor write showWhile: [os store: fcn]]
			ifError: [ :err :rcvr | Cursor normal show
				debug ifTrue: [Transcript show: '		', err; cr]]].

	b1 := [ra next * 0.2].
	b2 := [ra next * 0.3 + 0.4].
	1 to: 8 do:				"Make 8 random splines wanders"
		[ :i | | fcn x y pts |
		pts := OrderedCollection new.
		x := 0.0.
		y := ra next.
		[x &lt; 1.0] whileTrue:
			[pts add: x @ y.
			x := x + (ra next * 0.3) + 0.05.
			y := y + (ra next * 0.6) - 0.3.
			y &gt; 1.0 ifTrue: [y := 1.0 / y].
			y &lt; 0.0 ifTrue: [y := y negated]].
		pts last x: 1.0.
		fcn := SplineFunction from: pts.
		fcn name: 'Spline', i printString.
		[Cursor write showWhile: [os store: fcn]]
			ifError: [ :err :rcvr | Cursor normal show
				debug ifTrue: [Transcript show: '		', err; cr]]].

	debug ifTrue: [Transcript show: 'Saving gen DB...'].
	os save.
	debug ifTrue: [Transcript show: '  done.'; cr; cr].
</body>
</methods>

<methods>
<class-id>Siren.PaleoDemo class</class-id> <category>generators</category>

<body package="Siren" selector="addGeneratorsTo:">addGeneratorsTo: os
	"Add EventGenerators"
	"PaleoDemo addGeneratorsTo: (PaleoDemo osNamed: #Generators)"
	"YYYJD setupDB. PaleoDemo setupAll."
	"SMSBrowser openOn: (YYYJD DB)"

	| ra |
	ra := Random new.
	#(vito plang abrupt equal lower breath wrist vidi apostle) do: 
		[ :na | | b1 b2 ge |
		Transcript show: '	Creating generator ', na; cr.
		b1 := [ra nextInt: 24 + 60].
		b2 := [ra nextInt: 30 + 90].
		ge := DynamicCloud  dur: 6  
"				pitch: {(b1 value to: b1 value). (b1 value to: b1 value)}
				ampl: ({(b2 value to: b2 value). (b2 value to: b2 value)})
"				voice: (1 to: 4).
		ge density: (ra nextIntFrom: 6 to: 24).
		ge eventList.
		ge name: na.
		[Cursor write showWhile: [os store: ge]]
			ifError: [ :err :rcvr | 
				debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				self halt]].
	debug ifTrue: [Transcript show: 'Saving gen DB...'].
	os save.
	debug ifTrue: [Transcript show: '  done.'; cr; cr].
</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->



<class>
<name>BellPartial</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>objects</indexed-type>
<inst-vars>name freq ampl dur </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicApps-Pieces</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.BellPartial class</class-id> <category>as yet unclassified</category>

<body package="Siren" selector="named:freq:ampl:">named: na freq: fr ampl: am
	"Answer an instance with the given instance variables."

	^self new name: na; freq: fr; ampl: am</body>

<body package="Siren" selector="named:freq:ampl:dur:">named: na freq: fr ampl: am dur: du
	"Answer an instance with the given instance variables."

	^self new name: na; freq: fr; ampl: am; dur: du</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.BellPartial</class-id> <category>accessing</category>

<body package="Siren" selector="ampl">ampl
	"Answer the receiver's 'ampl'."

	^ampl</body>

<body package="Siren" selector="ampl:">ampl: anObject
	"Set the receiver's instance variable 'ampl' to be anObject."

	ampl := anObject</body>

<body package="Siren" selector="dur">dur
	"Answer the receiver's 'dur'."

	^dur</body>

<body package="Siren" selector="dur:">dur: anObject
	"Set the receiver's instance variable 'dur' to be anObject."

	dur := anObject</body>

<body package="Siren" selector="freq">freq
	"Answer the receiver's 'freq'."

	^freq</body>

<body package="Siren" selector="freq:">freq: anObject
	"Set the receiver's instance variable 'freq' to be anObject."

	freq := anObject</body>

<body package="Siren" selector="name">name
	"Answer the receiver's 'name'."

	^name</body>

<body package="Siren" selector="name:">name: anObject
	"Set the receiver's instance variable 'name' to be anObject."

	name := anObject</body>
</methods>

<methods>
<class-id>Siren.BellPartial</class-id> <category>printing</category>

<body package="Siren" selector="printOn:">printOn: str

	str nextPutAll: '(', name, ': ', freq printString, ', ', ampl printString, ')'</body>
</methods>


<initialize>
<class-id>Siren.PaleoUtil</class-id>
</initialize>


</st-source>
