<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 8, 2007 at 4:44:10 pm</time-stamp>


<class>
<name>MIDIPatch</name>
<environment>Siren</environment>
<super>UI.Model</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>name instruments channelMap </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicIO-MIDI-Patches</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<shared-variable>
<name>Patches</name>
<environment>Siren.MIDIPatch</environment>
<private>false</private>
<constant>false</constant>
<category>As yet unclassified</category>
<attributes>
<package>Siren</package>
</attributes>
</shared-variable>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MIDIPatch class</class-id> <category>class initialization</category>

<body package="Siren" selector="initialize">initialize
	"MIDIPatch initialize"

	Patches := Dictionary new</body>
</methods>

<methods>
<class-id>Siren.MIDIPatch class</class-id> <category>instance creation</category>

<body package="Siren" selector="default">default
	^Patches at: #Default ifAbsent: 
		[Patches at: #TestPatch ifAbsent: [self new]]</body>
</methods>

<methods>
<class-id>Siren.MIDIPatch class</class-id> <category>examples</category>

<body package="Siren" selector="patchExample">patchExample
	"Create and answer a simple MIDI patch with random contents."
	"MIDIPatch patchExample"

	| inst rand |
	inst := self new initialize.
	inst name: 'TestPatch'.
	rand := Random new.
"Add 32 random General MIDI instrument names."
	1 to: 32 do:
		[ :index |
		inst instruments at: index 
			put: (GeneralMIDIMap keys asOrderedCollection atRandom: rand)].
"Assign the first 32 slots in the virtual channel map to random values."
	1 to: 32 do:
		[ :index |
		inst channelMap at: index 
			put: (Array with: (#(1 2) atRandom: rand) 
					with: (rand nextIntFrom: 0 to: 16))].
	(Patches at: #TestPatch) inspect</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.MIDIPatch</class-id>
<body>A MIDIPatch repesents a configuration of synthesizer mappings. It can be edited and stored in Smalltalk, and down-loaded to the MIDI port driver and HW/SW synthesizer.

Instance Variables:
	name &lt;String&gt; The instance's name
	instruments &lt;Dictionary of (channel -&gt; instrument name)&gt; The map of virtual channels to General MIDI instrument names
	channelMap &lt;Dictionary of (virtual channel -&gt; (Array of MIDIMappings))&gt; The virtual-to-physical channel map, down-loadable to the MIDI driver.

Class Variables:
	Patches &lt;Dictionary of Patches&gt; The "patch library"
</body>
</comment>

<methods>
<class-id>Siren.MIDIPatch</class-id> <category>initialize-release</category>

<body package="Siren" selector="initialize">initialize
	"Set up the default state of the receiver."

	instruments := Dictionary new.
	channelMap := Dictionary new.</body>
</methods>

<methods>
<class-id>Siren.MIDIPatch</class-id> <category>accessing</category>

<body package="Siren" selector="channelMap">channelMap
	"Answer the receiver's 'channelMap'."

	^channelMap</body>

<body package="Siren" selector="channelMap:">channelMap: anObject
	"Set the receiver's instance variable 'channelMap' to be anObject."

	channelMap := anObject</body>

<body package="Siren" selector="instruments">instruments
	"Answer the receiver's 'instruments'."

	^instruments</body>

<body package="Siren" selector="instruments:">instruments: anObject
	"Set the receiver's instance variable 'instruments' to be anObject."

	instruments := anObject</body>

<body package="Siren" selector="name">name
	"Answer the receiver's 'name'."

	^name</body>

<body package="Siren" selector="name:">name: aString
	"Set the receiver's instance variable 'name' to be aString."

	name := aString.
	Patches at: aString asSymbol put: self</body>
</methods>

<methods>
<class-id>Siren.MIDIPatch</class-id> <category>instrument map</category>

<body package="Siren" selector="channelFor:">channelFor: iname

	^instruments keyAtValue: iname ifAbsent: [nil]</body>

<body package="Siren" selector="clearMapAt:">clearMapAt: chan

	instruments removeKey: chan ifAbsent: [nil]</body>

<body package="Siren" selector="mapAt:">mapAt: chan

	^instruments at: chan ifAbsent: [nil]</body>

<body package="Siren" selector="mapInstrument:toChannel:">mapInstrument: iname toChannel: chan

	instruments at: chan put: iname</body>
</methods>

<methods>
<class-id>Siren.MIDIPatch</class-id> <category>channel map</category>

<body package="Siren" selector="channelMapAt:">channelMapAt: index

	^channelMap at: index</body>

<body package="Siren" selector="mapChannel:toInterface:channel:">mapChannel: index toInterface: i channel: c

	^channelMap at: index put: (Array with: i with: c)</body>
</methods>


<class>
<name>MIDIMapping</name>
<environment>Siren</environment>
<super>Core.Object</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars>interface channel control value format </inst-vars>
<class-inst-vars></class-inst-vars>
<imports></imports>
<category>MusicIO-MIDI-Patches</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.MIDIMapping</class-id>
<body>
An instance of MIDIMapping represents a logical-to-physical MIDI channel mapping.

Instance Variables:
	interface &lt;Integer&gt; The instance's MIDI interface
	channel &lt;Integer&gt; The physical channel
	control &lt;Integer|Symbol&gt; An optional control parameter
	value &lt;Number&gt; The control value
	format &lt;Object&gt; An optional packet format for sysex or TCP packets
</body>
</comment>

<methods>
<class-id>Siren.MIDIMapping</class-id> <category>accessing</category>

<body package="Siren" selector="channel">channel
	"Answer the receiver's 'channel'."

	^channel</body>

<body package="Siren" selector="channel:">channel: anObject
	"Set the receiver's instance variable 'channel' to be anObject."

	channel := anObject</body>

<body package="Siren" selector="control">control
	"Answer the receiver's 'control'."

	^control</body>

<body package="Siren" selector="control:">control: anObject
	"Set the receiver's instance variable 'control' to be anObject."

	control := anObject</body>

<body package="Siren" selector="format">format
	"Answer the receiver's 'format'."

	^format</body>

<body package="Siren" selector="format:">format: anObject
	"Set the receiver's instance variable 'format' to be anObject."

	format := anObject</body>

<body package="Siren" selector="interface">interface
	"Answer the receiver's 'interface'."

	^interface</body>

<body package="Siren" selector="interface:">interface: anObject
	"Set the receiver's instance variable 'interface' to be anObject."

	interface := anObject</body>

<body package="Siren" selector="value">value
	"Answer the receiver's 'value'."

	^value</body>

<body package="Siren" selector="value:">value: anObject
	"Set the receiver's instance variable 'value' to be anObject."

	value := anObject</body>
</methods>


<initialize>
<class-id>Siren.MIDIPatch</class-id>
</initialize>


</st-source>
