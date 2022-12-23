'From VisualWorks® NonCommercial, Release 5i.2 of July 14, 2000 on November 3, 2001 at 12:07:10 am'!


Smalltalk defineClass: #YYYJD
	superclass: #{Smalltalk.EventList}
	indexedType: #objects
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: 'MusicApps-Celebration'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!YYYJD class methodsFor: 'setup'!

setupAll
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
	YYYJD printDBsizes!

setupObjectSets
	"Set up the Siren cached object sets for Gen2, VoiceSounds, LPC, and PVoc"
	"YYYJD setupDB. YYYJD setupObjectSets"
	"(Siren osNamed: #Generators2) explore"

	| sos |
	debug ifTrue: [Transcript cr; show: 'Flushing old obj sets.'; cr].
	Siren initializeObjectSets.
	Smalltalk garbageCollect.
"Score Cached Obj Set"
	debug ifTrue: [Transcript cr; show: 'Creating Score obj set.'; cr].
	sos := PaleoObjectSet new name: #YYYJDScore; fields: #(name size voice duration).
	Siren objectSets at: sos name put: sos.
"Gen2 Cached Obj Set"
	debug ifTrue: [Transcript cr; show: 'Creating Gen2 obj set.'; cr].
	sos := PaleoObjectSet new name: #Generators2; fields: #(name size duration).
	Siren objectSets at: sos name put: sos.
"Add cached voice snd files"
	debug ifTrue: [Transcript show: 'Creating voice snd object set.'; cr].
	sos := PaleoObjectSet new name: #VoiceSounds; 
				fields: #(name channels duration mtime ctime version size rate).
	Siren objectSets at: sos name put: sos.
"Cached LPC Obj Set"
	debug ifTrue: [Transcript show: 'Creating LPC object set.'; cr].
	sos := PaleoObjectSet new name: #LPC; fields: #(name version data size N I D).
	Siren objectSets at: sos name put: sos.
"Add voice FFT spectrum set"
	debug ifTrue: [Transcript show: 'Creating PVoc object set.'; cr].
	sos := PaleoObjectSet new name: #VoiceSpectra; 
					fields: #(name version date domain N D folder).
	Siren objectSets at: sos name put: sos.

	Transcript show: 'Object sets cached.'; cr.!

setupScore
	"YYYJD setupScore"
	"YYYJD instanceCount"
	"YYYJD setupScore. EventListExplorer open: (EventList named: 'Ywe Ye, Yi Jr Di' asSymbol)"

	| sco sect |
	Transcript cr; show: 'Flushing old score.'; cr.
	Siren eventLists removeKey: #'Ywe Ye, Yi Jr Di' ifAbsent: [nil].
	Smalltalk garbageCollect.
"Create top-level list."
	Transcript show: 'Creating base score.'; cr.
	sco := EventList newNamed: 'Ywe Ye, Yi Jr Di' asSymbol.
	sco description: 'Based on the text by Du Fu'.
	Transcript show: 'Adding sections to score.'; cr.
"Create prelude."
	self addPreludeTo: sco.
"Create introduction."
	self addIntroTo: sco.
"Add recitation."
	sect := EventList newNamed: #Recitation.
	sect duration: 90 sec.
	sect description: 'Recitation of the text'.
	sco add: sect.
"Add development."
	sect := EventList newNamed: #Development.
	sect duration: 270 sec.
	sect description: 'Development of text to a song'.
	sco add: sect.
"Add conclusion."
	sect := EventList newNamed: #Conclusion.
	sect duration: 60 sec.
	sect description: 'Conclusion and disappearance'.
	sco add: sect.
	Transcript show: 'Storing score to DB'; cr; cr.
	(self osNamed: #YYYJDScore) store: sco; save! !

!YYYJD class methodsFor: 'As yet unclassified'!

setupDB
	"Initialize the YYYJD database. Add obj sets for the score, 
		voice snd/sco, bell snd/sco, LPC, PV spectra"
	"Object initializePropertiesFields. Smalltalk garbageCollect"
	"YYYJD setupDB"

	| fileSize branchSize |
	YYYJD freeDB.								"flush old DB objects"
	self DB: (MinneStoreDB newOn: self dbDir).	"Create DB"
	fileSize := 1 "1024" .
	branchSize := 1024 "20".
"Add YYYJD Score"
	debug ifTrue: [Transcript cr; show: 'Creating score object set.'; cr].
"	(self DB addObjectSetNamed: #YYYJDScore) cache;
		objectsPerFile: 1;
		storesClass: EventList;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #voice domain: IOVoice;
		indexOn: #size domain: SmallInteger."
"		aspect: #root holdsOne: #EventLists;
		aspect: #parts holdsMany: #EventLists;
"
"Add voice snd files"
"	debug ifTrue: [Transcript show: 'Creating voice snd object set.'; cr].
	(self DB addObjectSetNamed: #VoiceSounds) cache;
		objectsPerFile: fileSize;
		storesClass: StoredSound;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #channels domain: SmallInteger;
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #folder domain: Symbol getter: 'item at: #folder';
		indexOn: #version domain: Symbol getter: 'item at: #version';
		indexOn: #ctime domain: DateAndTime;
		indexOn: #mtime domain: DateAndTime. "
"Add voice dsp script files"
	debug ifTrue: [Transcript show: 'Creating voice score object set.'; cr].
	(self DB addObjectSetNamed: #VoiceScores) cache;
		objectsPerFile: fileSize;
		storesClass: AbstractEvent;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #folder domain: Symbol;
		indexOn: #version domain: Symbol;
		indexOn: #folder domain: Symbol getter: 'item at: #folder';
		indexOn: #version domain: Symbol getter: 'item at: #version';
		indexOn: #date domain: SmallInteger getter: 'item at: #date'.
"Add bell files"
"	debug ifTrue: [Transcript show: 'Creating bell snd object set.'; cr].
	(self DB addObjectSetNamed: #BellSounds)
		objectsPerFile: fileSize;
		storesClass: StoredSound;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #duration domain: MusicMagnitude;
		indexOn: #folder domain: Symbol getter: 'item at: #folder';
		indexOn: #version domain: Symbol getter: 'item at: #version';
		indexOn: #date domain: SmallInteger getter: 'item at: #date'."
"Add bell dsp script files"
	debug ifTrue: [Transcript show: 'Creating bell score object set.'; cr].
	(self DB addObjectSetNamed: #BellScores) cache;
		objectsPerFile: fileSize;
		storesClass: EventList;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #folder domain: Symbol getter: 'item at: #folder';
		indexOn: #version domain: Symbol getter: 'item at: #version';
		indexOn: #date domain: SmallInteger getter: 'item at: #date'.
"Add voice LPC analysis set"
"	debug ifTrue: [Transcript show: 'Creating LPC object set.'; cr].
	(self DB addObjectSetNamed: #LPC)
		objectsPerFile: fileSize;
		storesClass: LinearPredictor;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #version domain: Symbol;
		indexOn: #date domain: DateAndTime ;
		aspect: #sound holdsOne: #VoiceSounds ."
"Add voice FFT spectrum set"
"	debug ifTrue: [Transcript show: 'Creating LPC object set.'; cr].
	(self DB addObjectSetNamed: #VoiceSpectra)
		objectsPerFile: fileSize;
		storesClass: Spectrum;
		maximumLeavesPerBranch: branchSize;
		indexOn: #name domain: Symbol;
		indexOn: #version domain: Symbol;
		indexOn: #date domain: DateAndTime;
		indexOn: #N domain: Integer;
		indexOn: #domain domain: Integer;
		indexOn: #D domain: Float;
		indexOn: #folder domain: Symbol getter: 'item at: #folder'. "
"		aspect: #sound holdsOne: #VoiceSounds ."
"Save All"
	debug ifTrue: [Transcript show: 'Saving all'; cr].
	self DB save.
	debug ifTrue: [Transcript show: 'Done'; cr; cr].! !

!YYYJD class methodsFor: 'local DB'!

bells
	"Answer the database of bell sounds"

	^YYYJD osContents: #BellSounds!

dbDir
	"Answer the YYYJD DB directory."

	^'RAM:Database:YYYJD'
"	^'Content:Paleo:YYYJD'"!

lpcs
	"Answer the database of lpc objects"

	^YYYJD osContents: #LPC!

voices
	"Answer the database of voice sounds"

	^YYYJD osContents: #VoiceSounds! !

!YYYJD class methodsFor: 'snd/sco files'!

loadBellSamples
	"Read in a hierarchy of sound files from the given ditrectory 
	 into the named DB object sets."

	"YYYJD loadVoiceSamples"
	"Object initializePropertiesFields. Smalltalk garbageCollect"
	"YYYJD setupDB"

	^YYYJD loadDataFolder: 'Content:Sound:YYYJD:Bells' 
			intoSnd: #BellSounds 
			andSco: #BellScores!

loadDataFolder: bas intoSnd: sndOS andSco: scoOS
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."

	"YYYJD loadDataFolder: 'Content:Sound:YYYJD:EC' 
			intoSnd: #VoiceSounds 
			andSco: #VoiceScores"
	"YYYJD loadDataFolder: 'Content:Sound:YYYJD:EC:read3' 
			intoSnd: #VoiceSounds andSco: #VoiceScores"
	"YYYJD printDBsizes"

	| dir dirs os1 os2 |
"Set up object sets"
	os1 := self osNamed: sndOS.		"snd object set"
	os2 := self osNamed: scoOS.		"sco object set"

	dir := FileDirectory on: bas.				"select subdirs that end in a digit"
	dirs := dir directoryNames select: [ :fn | fn last isDigit].
	debug ifTrue: [Transcript show: 'Load snd/sco ', dirs printString, ' in folder ', bas; cr].
	dirs isEmpty ifTrue: [ | rt up |
			rt := bas asLocalFilename.
			up := FileDirectory dirPathFor: bas.
			self loadSndDirectory: rt in: up into: os1.
			self loadScoDirectory: rt in: up into: os2].
	dirs	do:						"take 2nd folder for testing"
		[ :di |					"iterate through directories"
		self loadSndDirectory: di in: bas into: os1.
		self loadScoDirectory: di in: bas into: os2].

	debug ifTrue: [Transcript show: 'Saving snd/sco DBs...'].
	os1 save.
	os2 save.
	debug ifTrue: [Transcript cr; cr].!

loadScoDirectory: di in: bas into: os
	"Load the sco files from the given directory."

	| di2 fils |
	debug ifTrue: [Transcript cr; show: 'Load dir: ', di; cr].
	di2 := FileDirectory on: bas, ':', di.
										"Select/process sco files"
	fils := di2 fileNames select: [ :fn | '*.sc' match: fn].
	fils do:
		[ :fi |
		debug ifTrue: [Transcript show: '		Store fil: ', fi; cr].
		self loadScoFile: fi in: bas dir: di to: os].!

loadScoFile: fi in: bas dir: di to: os
	"Load a single score file as text.."
	"YYYJD loadScoFiles: 'Content:Sound:YYYJD:Bells:B3:*.sc' into: #BellScores"

	| sco fst fna |
	debug ifTrue: [Transcript show: '		Store fil: ', fi; cr].
	fna := di, FileSep, bas, FileSep, fi.
	fst := FileStream textFileNamed: fna.
	fst ifNil: [Transcript show: '			not found.'; cr. ^nil].
	sco := EventList newNamed: fi sansLastPeriodSuffix.
	sco at: #text put: fst ascii contentsOfEntireFile.
	sco at: #folder put: bas.
	sco at: #version put: fi betweenPeriods.
	sco at: #date put: (DateAndTime from: fna asFileDirEntry modificationTime).
	os store: sco!

loadScoFiles: pat into: scoOS
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."
	"YYYJD loadScoFiles: 'Content:Sound:YYYJD:Bells:B3:*.sc' into: #BellScores"

	| os1 fils bas di |
	os1 := self osNamed: scoOS.		"snd object set"
	bas := pat asDirectoryName.
	di := bas asDirectoryName.
	fils := (FileDirectory on: bas) fileNamesMatching: pat asLocalFilename.
	bas := bas asLocalFilename.
	fils ifEmpty: [^Transcript show: 'No files found.'; cr].
	fils do:
		[ :fi |
		debug ifTrue: [Transcript show: '		Store fil: ', fi; cr].
		self loadScoFile: fi in: bas dir: di to: os1].

	debug ifTrue: [Transcript show: 'Saving sco DB...'].
	os1 save.
	debug ifTrue: [Transcript cr; cr].!

loadSndDirectory: di in: bas into: os
	"Load the snd files from the given directory."

	| di2 fils |
		debug ifTrue: [Transcript cr; show: 'Load dir: ', di; cr].
		di2 := FileDirectory on: bas, FileSep, di.
											"Select/process snd files"
		fils := di2 fileNames select: [ :fn | '*.*snd' match: fn].
		fils do: [ :fi |
			debug ifTrue: [Transcript show: '	Load fil: ', fi; cr].
			self store: (self loadSndFile: fi in: di dir: bas) to: os].!

loadSndFile: fi in: bas dir: di
	"Load a sound file and answer it"
	"YYYJD loadVoiceSamples"

	| fname snd |
	fname := bas, FileSep, di, FileSep, fi.
	[Cursor read showWhile:
		[snd := StoredSound headerFromFile: fname]]
			ifError: [ :err :rcvr |
				debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				^ " self  "self halt].
	snd at: #folder put: di asSymbol.
	snd at: #name: put: fi sansLastPeriodSuffix asSymbol.
	snd at: #fullName put: fname.
	snd at: #version put: (fi betweenPeriodsIfEmpty: '1.0.0') asSymbol.
	snd freeData.
	^snd!

loadSndFiles: pat into: sndOS
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."
	"YYYJD loadSndFiles: 'Content:Sound:YYYJD:EC:r3:src:#.snd' into: #VoiceSounds"

	| os1 fils bas di |
	os1 := self osNamed: sndOS.		"snd object set"
	bas := pat asDirectoryName.
	di := bas asDirectoryName.
	fils := (FileDirectory on: bas) fileNamesMatching: pat asLocalFilename.
	bas := bas asLocalFilename.
	fils ifEmpty: [^Transcript show: 'No files found.'; cr].
	fils do:
		[ :fi | 
		debug ifTrue: [Transcript show: '	Load fil: ', fi; cr].
		self store: (self loadSndFile: fi in: di dir: bas) to: os1].

	debug ifTrue: [Transcript show: 'Saving snd DB...'].
	os1 save.
	debug ifTrue: [Transcript cr; cr]!

loadVoiceSamples
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."

	"YYYJD loadVoiceSamples"
	"Debug := true.
	YYYJD setupDB.
	YYYJD loadVoiceSamples.
	YYYJD printDBsizes"
	"YYYJD osContents: #VoiceSounds"

	" | fn |
	Debug := false.
	fn _ 'loadVox.spy'.
	FileDirectory default deleteFileNamed: fn ifAbsent: [].
	MessageTally
		spyEvery: 2
		on: [YYYJD loadVoiceSamples]
		toFileNamed: fn.
	fn asFileStream edit"

	^YYYJD loadDataFolder: 'Content:Sound:YYYJD:EC' 
			intoSnd: #VoiceSounds 
			andSco: #VoiceScores!

store: obj to: os
	"Store an into an object set"

	[Cursor write showWhile: 
		[os store: obj]]
			ifError: [ :err :rcvr | 
				Debug ifTrue: [Transcript show: '		', err; cr].
				Cursor normal show.
				self halt].! !

!YYYJD class methodsFor: 'score creation'!

addIntroTo: sco
	"YYYJD setup"
	"YYYJD instanceCount"
	"EventListExplorer open: (YYYJD someInstance)"

	| sect |
"Create introduction."
	sect := EventList newNamed: #Introduction.
	sect description: 'Introduction with bell peals and first word'.
	sect comment: 'Opening bell peals on the word "ywe" develop into clock-wise rotating thick golden-mean texture over which the poem''s title and author are heard.'.
"Add first 4 words."
	sect add: (EventList named: 'Moon'  
		fromSelectors: #(duration: loudness: phoneme:) 
		values: (Array with: #(6.0 5.0 4.0 4.0) 
				with: #(0.3 0.5 0.7 0.8) 
				with: #(ywe ywe ywe ywe))).
"Add title."
	sect add: (EventList named: 'Title'  
		fromSelectors: #(duration: loudness: phoneme:) 
		values: (Array with: #(0.4 1.2 0.3 0.3 1.2 0.2 0.3) 
				with: #(0.8 0.6 0.4 0.3 0.4 0.5 0.2) 
				with: #(ywe ye yi jr di du fu)))
		at: 22.0.
"Add intro. rotating bell cluster."
	sect add: ((RotatingBellCluster setup: #b2Data functions: #b2aFunctions) eventList)
		at: 0.
	sect duration: 120 sec.
	sco add: sect.!

addPreludeTo: sco
	"Add the YYYJD prelude to the given score"

	| pipes sect |
"Create introduction."
	sect := EventList newNamed: #Prelude.
	sect description: 'Prelude with spatial pipes'.
	sect comment: 'Piece starts with a spatialized "pan pipe" sound'.
"Add first 4 words."
	pipes := ((DynamicSelectionCloud dur: 15 sec
			pitch: #( #(48 50 53 55 57) #(60 62 65 67 69) )
			ampl: #(0.8)
			voice: #(pipe0b))
			density: 24) eventList.
	pipes eventsDo: [ :ev | ev duration: 6.0 sec].
	sect addAll: pipes.
	sco add: sect.!

saveScore
	"YYYJD saveScore"

	(self osNamed: #YYYJD) store: YYYJD someInstance; save.! !

!YYYJD class methodsFor: 'lpc mgmnt'!

doLPC: file
	"YYYJD doLPC: 'Kombination.snd'"

	self doLPC: file save: true!

doLPC: file save: doSave
	"YYYJD doLPC: 'Kombination.snd' save: true"

	| sndin data lpc |
	Debug ifTrue: [Transcript cr; show: 'Open sound file ', file; cr].
	sndin := StoredSound fromFile: file.
	lpc := LinearPredictor on: sndin.
	lpc npoles: 24.
	lpc framesize: 256.
	lpc date: Time millisecondClockValue.
	lpc version: (file betweenPeriodsIfEmpty: '1.0.0').

	Debug ifTrue: [Transcript cr; show: 'LPC analysis...'; cr].
	lpc analyze.
	lpc stabilize.
	lpc packLPCData.
	data := lpc pitchTrackLow: 70 high: 500.
	lpc packPitchData: data.
	lpc freeSound.

	doSave ifTrue:
		[Debug ifTrue: [Transcript cr; show: 'Storing LPC to DB'; cr].
		(self osNamed: #LPC) store: lpc; save].
"	lpc explore "!

doLPC: file storeTo: os
	"YYYJD doLPC: 'Kombination.snd' save: true"

	| sndin data lpc |
"	debug ifTrue: [Transcript cr; show: 'LPC on sound ', file name; cr]."
	file isString 
		ifTrue: [sndin := StoredSound fromFile: file]
		ifFalse: [sndin := file].
	lpc := LinearPredictor on: sndin.
	lpc npoles: 24.
	lpc framesize: 256.
	lpc date: DateAndTime now.
	lpc version: (file betweenPeriodsIfEmpty: '1.0.0').

	debug ifTrue: [Transcript cr; show: 'LPC analysis...'; cr].
	lpc analyze.
	lpc stabilize.
	lpc packLPCData.
	data := lpc pitchTrackLow: 70 high: 500.
	lpc packPitchData: data.
	lpc freeSound.

	os ifNotNil:
		[debug ifTrue: [Transcript cr; show: 'Storing LPC to DB'; cr].
		os store: lpc].
"	lpc explore "!

doLPCFolder: bas intoOSet: lpcOS
	"Read in a hierarchy of sound files from the given ditrectory 
	 into the named DB object sets."

	"YYYJD doLPCFolder: 'Content:Sound:YYYJD:ec:read3' intoOSet: #LPC"
	"YYYJD setupDB"
	"YYYJD printDBsizes"

	| di fils os |
	os := self osNamed: lpcOS.		"LPC object set"
	debug ifTrue: [Transcript show: 'LPC in dir: ', bas; cr].
	di := FileDirectory on: bas.
									"Select/process snd files named ?.snd"
	fils := di fileNames select: [ :fn | '#.snd' match: fn].
	fils do: [ :fi | self doLPC: (bas, ':', fi) storeTo: os].
	debug ifTrue: [Transcript show: 'Saving LPC DB...'].
	os save.							"save object set"
	debug ifTrue: [Transcript cr; cr].!

doLPCOn: snds intoOSet: lpcOS
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
	debug ifTrue: [Transcript cr; cr].! !

!YYYJD class methodsFor: 'voice pvoc'!

doPVoc: snd size: n dec: d storeTo: os
	"Executen the PhaseVocoder on the given sound object, storing the resulting spectrum."

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
		os store: sp]!

doPVocOnFiles: pat into: spectOS
	"Read in a hierarchy of sound files from the given directory 
	 into the named DB object sets."
	"YYYJD doPVocOnFiles: 'Content:Sound:YYYJD:EC:read3:#.snd' 
					into: #VoiceSpectra"

	| os1 fils bas di |
	os1 := self osNamed: spectOS.		"spectrum object set"
	bas := pat asDirectoryName.
	di := bas asDirectoryName.
	fils := (FileDirectory on: bas) fileNamesMatching: pat asLocalFilename.
	bas := bas asLocalFilename.
	fils ifEmpty: [^Transcript show: 'No files found.'; cr].
	fils do:
		[ :fi | | snd |
		snd := self loadSndFile: fi in: di dir: bas.
		self doPVoc: snd size: 512 dec: 1.0 storeTo: os1].

	debug ifTrue: [Transcript show: 'Saving snd DB...'].
	os1 save.
	debug ifTrue: [Transcript cr; cr]!

doPVocOnSounds: snds intoOSet: osName
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
	debug ifTrue: [Transcript cr; cr].! !

!YYYJD class methodsFor: 'b3a scripts'!

b3a_1
	"Make b3a.m1 stereo layered mixes."
	"YYYJD b3a_1"

	| rand stream num |
	rand := Random new.
	num := 8.
	stream := WriteStream on: (String new: 1024).

	#(3 6 9 12 15 18 21) do:
		[ :bstart |
		2 to: num do:
			[ :i |
			stream nextPutAll: 'NST('; 
				nextPutAll: (((bstart + rand next) printString) clipTo: 4);
				nextPutAll: ', gs', i printString;
				nextPutAll: ((i <= 4) ifTrue: [', 1.0, ']
						ifFalse: [(i = 5) ifTrue: [', 0.5, ']
							ifFalse: [(i <= 7) ifTrue: [', 0.2, ']
								ifFalse: [', 0.1, ']]]);
				nextPutAll: ((rand nextFrom: 0.1 to: 0.9) printString clipTo: 4),
							 ');'; cr].
			stream cr].
	^stream contents! !

!YYYJD class methodsFor: 'b5b scripts'!

b5b_Stereo1
	"Make b5b.m2 stereo layered mixes."
	"YYYJD b5b_Stereo1"

	| rand stream num |
	rand := Random new.
	num := 8.
	stream := WriteStream on: (String new: 1024).

	#(3 6 9 12 15 18 21) do:
		[ :bstart |
		2 to: num do:
			[ :i |
			stream nextPutAll: 'NST('; 
				nextPutAll: (((bstart + rand next) printString) clipTo: 4);
				nextPutAll: ', gs', i printString;
				nextPutAll: ((i <= 4) ifTrue: [', 1.0, ']
						ifFalse: [(i = 5) ifTrue: [', 0.5, ']
							ifFalse: [(i <= 7) ifTrue: [', 0.2, ']
								ifFalse: [', 0.1, ']]]);
				nextPutAll: ((rand nextFrom: 0.1 to: 0.9) printString clipTo: 4),
							 ');'; cr].
			stream cr].
	^stream contents!

b5b_Stereo2
	"Make b5b.m2 stereo layered mixes."
	"note start voice DUR2 ampl ATT DEC 2 WHT pos;"
	"YYYJD b5b_Stereo2"

	| rand num params data list voice file |
	rand := Random new.
	num := 8.
	params := 9.
	data := Array new: params.
	file := 'b5bm2b' asFilename writeStream.
	1 to: params do: [ :i | data at: i put: (OrderedCollection new)].
	#(0 3 6 9 12 15 18 21 24 27 30 33 36) do: [ :bstart |
		2 to: num do: [ :i |
				(data at: 1) add: (((bstart + (rand next * 1.2)) printString) clipTo: 5).
				(data at: 2) add: 'gs', i printString.
				(data at: 3) add: 16.73.
				(data at: 4) add: ((i <= 4) ifTrue: [1.0]
						ifFalse: [(i = 5) ifTrue: [0.5]
							ifFalse: [(i <= 7) ifTrue: [0.2]
								ifFalse: [0.1]]]).
				(data at: 5) add: 2.5.		"att"
				(data at: 6) add: 2.5.		"dec"
				(data at: 7) add: 2.			"fcn"
				(data at: 8) add: 0.7.		"weight"
				(data at: 9) add: (rand nextFrom: 0.1 to: 0.9)]].
	list := EventList named: #b5bm2b
		fromSelectors: #(start: voice: duration: loudness: attack: decay: 
				function: weight: position:) 
		values: data.
	voice := CmusicVoice newNamed: 'violin' onStream: file.
	voice parameterMap at: 3 put: [ :e | e voice].
	voice parameterMap at: 5 put: [ :e | e loudness value printString].
	voice addMap: #attack.
	voice addMap: #decay.
	voice addMap: #function.
	voice addMap: #weight.
	voice addMap: [ :e | (e at: #position) printString clipTo: 4].
	voice playEventList: list.
	file close.
	(Filename named: 'b5bm2b') edit!

b5b_Stereo2b
	"Make b5b.m2 stereo layered mixes."
	"note start voice DUR2 ampl ATT DEC 2 WHT pos;"
	"YYYJD b5b_Stereo2"

	| rand num params data list voice file |
	rand := Random new.
	num := 8.
	params := 9.
	data := Array new: params.
	file := 'b5bm2b' asFilename writeStream.
	1 to: params do: [ :i | data at: i put: (OrderedCollection new)].
	#(0 3 6 9 12 15 18 21 24 27 30 33 36) do: [ :bstart |
		2 to: num do: [ :i |
				(data at: 1) add: (((bstart + (rand next * 1.2)) 
						printString) clipTo: 5).
				(data at: 2) add: 'gs', i printString.
				(data at: 3) add: 16.73.
				(data at: 4) add: ((i <= 4) ifTrue: [1.0]
						ifFalse: [(i = 5) ifTrue: [0.5]
							ifFalse: [(i <= 7) ifTrue: [0.2]
								ifFalse: [0.1]]]).
				(data at: 5) add: 2.5.		"att"
				(data at: 6) add: 2.5.		"dec"
				(data at: 7) add: 2.			"fcn"
				(data at: 8) add: 0.7.		"weight"
				(data at: 9) add: (rand nextFrom: 0.1 to: 0.9)]].
	list := EventList named: #b5bm2b
		fromSelectors: #(start: voice: duration: loudness: attack: decay: 
				function: weight: position:) 
		values: data.
	voice := CmusicVoice newNamed: 'violin' onStream: file.
	voice parameterMap at: 3 put: [ :e | e voice].
	voice parameterMap at: 5 put: [ :e | e loudness value printString].
	voice addMap: #attack.
	voice addMap: #decay.
	voice addMap: #function.
	voice addMap: #weight.
	voice addMap: [ :e | (e at: #position) printString clipTo: 4].
	voice playEventList: list.
	file close.
	(Filename named: 'b5bm2b') edit! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!



Smalltalk defineClass: #NSMSS
	superclass: #{Smalltalk.EventList}
	indexedType: #objects
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: 'SpeechSounds '
	imports: ''
	category: 'MusicApps-Celebration'!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!


!NSMSS class methodsFor: 'setup'!

setupObjectSets
	"Set up the Siren cached object sets for NSMSS sounds and generators."
	"NSMSS setupObjectSets"
	"(Siren osNamed: #NSMSSSounds) explore"

	| sos |
	debug := true.
"Sounds Cached Obj Set"
	debug ifTrue: [Transcript cr; show: 'Creating Sounds obj set.'; cr].
	sos := PaleoObjectSet new name: #NSMSSSounds; fields: #(name duration).
	Siren objectSets at: sos name put: sos.

	Transcript show: 'Object sets cached.'; cr.! !

!NSMSS class methodsFor: 'sounds'!

createAGAOsounds
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
	^samps! !

!NSMSS class methodsFor: 'database'!

loadSpeech
	"Load all the speech examples"
	"NSMSS loadSpeech. SpeechSounds inspect"

	| base files folders snd |
	base := '/snd/'.
	folders := #(brother "truth").
	SpeechSounds := OrderedCollection new.
	folders do: 
		[ :fo |
		files := Filename filesMatching: (base, fo, '/*.snd').
		files do:
			[ :fi |
			Transcript show: 'Loading file ', fi; cr.
			snd :=SoundSegmenter processFile: fi.
			snd cueList do:
				[ :q |
				SpeechSounds add: (VirtualSound from: snd cue: q key)]]].! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!



Smalltalk defineClass: #TMYB
	superclass: #{Smalltalk.EventList}
	indexedType: #objects
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: 'MusicApps-Celebration'!


Smalltalk defineClass: #KombinationXI
	superclass: #{Smalltalk.EventList}
	indexedType: #objects
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: 'MusicApps-Celebration'!


Smalltalk defineClass: #SimpleTruths
	superclass: #{Smalltalk.EventList}
	indexedType: #objects
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: 'MusicApps-Celebration'!


