'From Squeak6.1alpha of 23 January 2023 [latest update: #22394] on 23 January 2023 at 5:29:45 pm'!!EventList class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:54'!randomExample: length from: data
	"Create an event list of length items with random data."
	"(EventList randomExample: 64 
		from: ((#duration: -> (0.1 to: 0.2)), (#pitch: -> (36 to: 60)),
				(#ampl: -> (48 to: 64)), (#voice: -> (1 to: 4)))) edit"

	| rand arr sels i a |
	rand := Random new.
	arr := Array new: data size.
	sels := Array new: data size.
	i := 1.
	data associationsDo: [ :item | | int |
		int := item value.
		a := Array new: length.
		(int isMemberOf: Interval)
			ifTrue: [int first isInteger
				ifTrue: [1 to: length do: 					[ :c | a at: c put: (rand nextIntFrom: int first to: int stop)]]
				ifFalse: [1 to: length do: 					[ :c | a at: c put: (rand nextFrom: int first to: int stop)]]]
			ifFalse: [int isString				ifTrue: [1 to: length do: 						[ :c | a at: c put: int]]				ifFalse: [int hasItems
					ifTrue: [1 to: length do: 						[ :c | a at: c put: (int atRandom: rand)]]
					ifFalse: [1 to: length do: 						[ :c | a at: c put: int]]]].
		arr at: i put: a.
		sels at: i put: item key.
		i := i + 1].
	^EventList named: EventList nextName
		fromSelectors: sels 
		values: arr! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:57'!fmExample1
	"Play a few random notes on the CSL FM instrument"
	"OSCVoice fmExample1"
	"EventScheduler interrupt; flush"
	"Startup up the CSL OSC demo (or dumpOSC) and run these"
	"CSL FMInstrument has args: dur, ampl, c_fr, m_fr, ind, pos, att, dec, sus, rel, i_att, i_dec, i_sus, i_rel"

	| evList voice  |
	evList := (EventList randomExample: 15 			"create the event list"
		from: ((#duration: -> (0.15 to: 0.3)), 
			(#pitch: -> (110.1 to: 220.1)), 
			(#ratio: -> (0.8 to: 6.0)),
			(#ampl: -> (0.1 to: 0.4)), 
			(#modIndex: -> (0.0 to: 4.0)), 
			(#pos: -> (-0.7 to: 0.7)),
			(#att: -> (0.03 to: 0.05)),
			(#dec: -> (0.03 to: 0.05)),
			(#sus: -> (0.5 to: 0.7)),
			(#rel: -> (0.05 to: 0.06)),
			(#iatt: -> (0.05 to: 0.1)),
			(#idec: -> 0.005),
			(#isus: -> (0.3 to: 0.9)),
			(#irel: -> 0.1),
			(#inst: -> '/i1/pn'))).	1 to: evList events size do: 		[ :ind |											"plug in instruments"		((evList events at: ind) event) inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
	voice := self parameterMap: (self pMapForCSLFM: (11 to: 20)).	"plug in the parameter map"
	evList voice: voice.
	evList play.! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:27'!functionExample "BROKEN"
	"Play a function out to OSC"
	"OSCVoice functionExample"

	| event voice map |							"create the function and event"
	event := FunctionEvent new function: (LinearFunction default scaleBy: 5@1).
	event interval: 0.25 sec.						"4 per sec"
	map :=  [ :e | TypedOSCMessage for: '/osc/1/ampl' with: (Array with: e value)].
	voice := self default.
	voice parameterMap: map.					"plug in the parameter map"
	event voice: voice.
	event play! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:20'!granulatorExample1
	"Play on the CSL granulator1 instrument - short bursts of garbled speak'n'spell "
	"OSCVoice granulatorExample1"		"EventScheduler flush"
	"Startup up the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice |	
	evList := (20 sec, -18 dB) asEventList.								"create the event list with 1 note"	
	voice := self onPort: OSCPort default.								"edit this to taste."
	voice parameterMap: (self pMapForCSLGranulator: (62 to: 63)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"															"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:20'!granulatorExample2
	"Play on the CSL granulator2 instrument until interrupted - long low drones"
	"OSCVoice granulatorExample2"		"EventScheduler flush"
	"Startup up the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice |	
	evList := (30 sec, -12 dB) asEventList.								"create the event list with 1 note"	
	voice := self onPort: OSCPort default.								"edit this to taste."
	voice parameterMap: (self pMapForCSLGranulator: (64 to: 67)).	"plug in the parameter map"
	evList voice: voice.	
	evList play.
"	evList loop"															"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:06'!granulatorExample2: index
	"Play on the CSL granulator2 instrument"
	"OSCVoice granulatorExample2: 1"	"OSCVoice granulatorExample2: 2"	"OSCVoice granulatorExample2: 3"
	"Startup up the CSL OSC demo and run these"
	"CSL Instrument has args: ampl, dur"

	| evList voice osc |		osc := '/i', index printString, '/pn'.
	evList := (30 sec, -12 dB, (#inst -> osc)) asEventList.					"create the event list with 1 note"	
	voice := self onPort: OSCPort default.								"edit this to taste."
	voice parameterMap: (self pMapForCSLGranulator: (64 to: 67)).	"plug in the parameter map"
	evList voice: voice.	
	evList play.
"	evList loop"															"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:47'!pluckExample1
	"Play notes on 10 CSL plucked string instruments - high fast notes"
	"OSCVoice pluckExample1"
	"EventScheduler interrupt; flush"
	"Startup up the CSL OSC demo and run these"
	"CSL plucked string has args: ampl, freq, pos"

	| evList voice |												"create the event list"
	evList := (EventList randomExample: 120
		from: ((#duration: -> 0.07),
				(#ampl: -> (0.25 to: 0.5)), 
				(#pitch: -> (80.0 to: 600.0)), 
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |											"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
											"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"												"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:46'!pluckExample2
	"Play notes on 10 CSL plucked string instruments - slow low notes"
	"OSCVoice pluckExample2"
	"EventScheduler interrupt; flush"
	"Startup up the CSL OSC demo and run these"
	"CSL plucked string has args: ampl, freq, pos"

	| evList voice |												"create the event list"
	evList := (EventList randomExample: 12
		from: ((#duration: -> 3.0),
				(#ampl: -> (0.25 to: 0.5)), 
				(#pitch: -> (40.0 to: 100.0)), 
				(#pos: -> (-1.0 to: 1.0)))).
	1 to: evList events size do: 
		[ :ind | | ev |												"plug in instruments and expand durations (molto legato)"
		ev := (evList events at: ind) event.
		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].
																	"justify the pitches to the D Pentatonic scale"
	evList applyBlock: [ :p | (PentatonicScale root: N re) nearestNoteTo: p asHz value] toProp: #pitch.
	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"
	evList voice: voice.
	evList play.
"	evList loop"														"loop until you stop the scheduler"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 16:45'!scaleExample	"Play a scale on the CSL plucked string voice."
	"OSCVoice scaleExample"

	 | num list voice |
	list := EventList scaleFrom: 48 to: 60 in: 2.0.	num := list events size.	1 to: num do: 		[ :ind | | ev |											"plug in missing parameters"		ev := (list events at: ind) event.		ev ampl: 0.5.											"amplitude"		ev pos:  (ind / num * 2 - 1.0).							"stereo pan L to R"		ev inst: ('/i', (ind - 1 \\ 10 + 1) printString, '/pn')].	"cycle through the 10 plucked string instruments"	voice := self parameterMap: (self pMapForCSLPluck: (1 to: 10)).	"plug in the parameter map"	list voice: voice.
	list play													"...and play"! !!OSCVoice class methodsFor: 'examples' stamp: 'stp 1/23/2023 17:01'!vSOSExample3
	"Play a long note on the SHARC VSOS instrument"
	"OSCVoice vSOSExample3"		
	"EventScheduler flush"
	"Startup up the CSL OSC demo and run these"
	

	| evt voice |						"create the event"
	evt := (4.0 sec, 50.1 Hz, -8 dB, (#pos -> 0.0), (#inst -> '/i3/pn')).
	voice := self parameterMap: (self pMapForCSLSHARC1: (56 to: 61)).	"plug in the parameter map"
	evt voice: voice.
	evt play.
! !!OSCVoice class methodsFor: 'parameter maps' stamp: 'stp 1/23/2023 17:17'!pMapForCSLGranulator: range 	"Answer the default parameterMap for use with the CSL granulator instrument. "	"Instrument has args: dur, ampl"		^ [ :e | 	| arr inst inst2 sta sto num msg |	inst := e inst.	(inst isNil or: [inst isEmpty]) ifTrue: [inst := '/i1/pn'].	sta := inst findString: '/i'.							"fix instr num"	sto := inst findString: '/' startingAt: sta + 2.	num := (inst copyFrom: sta + 2 to: sto - 1) asInteger.	num := num + range start - 1.					"map inst num to range"	num > range last		ifTrue: [num := range last].	inst2 := '/i' , num printString				, (inst copyFrom: sto to: inst size).	arr := Array new: 2.								"set up the parameter map"	arr at: 1 put: e duration asSec value asFloat.	arr at: 2 put: e ampl asRatio value asFloat.	msg := TypedOSCMessage for: inst2 with: arr.	OSCVoice verbose		ifTrue: [Transcript show: msg printString; cr].	msg]! !!String methodsFor: 'accessing' stamp: 'stp 1/18/2023 12:36'!embeddedMorphs	"return the list of morphs embedded in me"	^ IdentitySet new! !