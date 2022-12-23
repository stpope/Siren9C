<?xml version="1.0"?>

<st-source>
<time-stamp>From VisualWorks® NonCommercial, Pre-Release 7.5 (mar07.3) of March 26, 2007 on April 8, 2007 at 4:44:04 pm</time-stamp>


<class>
<name>MIDINoteOn</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MIDINoteOn class</class-id> <category>instance creation</category>

<body package="Siren" selector="chan:key:vel:">chan: chan key: key vel: vel
	"Create and answer a MIDI note-on command with the given parameters."

	^self new data: (ByteArray new: 3); status: (16r90 bitOr: chan);
				key: key; vel: vel</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.MIDINoteOn</class-id>
<body>An instance of MIDINoteOn represents a MIDI note on command event.
The command format is:
	9c kk vv whereby:
		"9" means the command status 16r9
		"c" is the channel
		"kk" is the MIDI key number and
		"vv" is the MIDI key velocity</body>
</comment>


<class>
<name>MIDIProgramChange</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MIDIProgramChange class</class-id> <category>instance creation</category>

<body package="Siren" selector="chan:prog:">chan: chan prog: prog
	"Create and answer a MIDI program change command with the given parameters."
	"MIDIProgramChange chan: 4 prog: 64"

	^self new data: (ByteArray new: 2);
			status: (progChange bitOr: chan);
			second: (prog bitAnd: 16r7F)</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.MIDIProgramChange</class-id>
<body>An instance of MIDIProgramChange represents a MIDI program change event.
The command format is:
	cx pp whereby:
		"c" means the command status 16rC
		"x" is the channel number and
		"pp" is the new program number.</body>
</comment>


<class>
<name>MIDIControl</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.MIDIControl</class-id>
<body>An instance of MIDIControl represents a MIDI control change event.
The command format is:
	bc xx vv whereby:
		"e" means the command status 16rB
		"c" is the MIDI channel
		"xx" is the controller number and
		"vv" is the control value</body>
</comment>


<class>
<name>MIDIAfterTouch</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<comment>
<class-id>Siren.MIDIAfterTouch</class-id>
<body>A MIDIAfterTouch packet represents a MIDI message for keyboard after-touch data.</body>
</comment>


<class>
<name>MIDIPitchBend</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			private GeneralMIDIDrums.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MIDIPitchBend class</class-id> <category>instance creation</category>

<body package="Siren" selector="bytesFor14BitValue:">bytesFor14BitValue: val
	"Answer a two-element array with the 14-bit values that correspond to the given value."
	"val should be +- 8192"

	| int |
	val abs &gt; 8191
		ifTrue: [self error: 'Pitch bend value out of range.'].
	int := val + 8192.
	^Array with: (int bitAnd: 16r7F) with: ((int bitShift: -7) bitAnd: 16r7F)</body>

<body package="Siren" selector="value:">value: val
	"Create and answer a MIDI pitch-bend command with the given value."
	"0 corresponds to 16r2000 = 8192 = x1000000 x0000000"
	"val should be +- 8192"
	"MIDIPitchBend value: 1"

	| int |
	val abs &gt; 8191
		ifTrue: [self error: 'Pitch bend value out of range.'].
	int := val + 8192.
	^self new data: (ByteArray new: 3); 
		status: (16rE0);
		second: (int bitAnd: 16r7F);
		third: ((int bitShift: -7) bitAnd: 16r7F)</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.MIDIPitchBend</class-id>
<body>An instance of MIDIPitchBend represents a MIDI pitch bend command event.
The command format is:
	en ll hh whereby:
		"e" means the command status 16rE
		"n" is ignored
		"ll" is the lower 7 bits of the pitch bend value and
		"hh" is the upper 7 bits of the pitch bend value.</body>
</comment>


<class>
<name>MIDINoteOff</name>
<environment>Siren</environment>
<super>Siren.MIDIPacket</super>
<private>false</private>
<indexed-type>none</indexed-type>
<inst-vars></inst-vars>
<class-inst-vars></class-inst-vars>
<imports>
			private MIDICommands.*
			</imports>
<category>MusicIO-MIDI-Commands</category>
<attributes>
<package>Siren</package>
</attributes>
</class>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<methods>
<class-id>Siren.MIDINoteOff class</class-id> <category>instance creation</category>

<body package="Siren" selector="chan:key:">chan: chan key: key
	"Create and answer a MIDI note-off command with the given parameters."

	^self new data: (ByteArray new: 3); status: (16r80 bitOr: chan);
				key: key; vel: 0</body>
</methods>

<!-- -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -   -->


<comment>
<class-id>Siren.MIDINoteOff</class-id>
<body>A MIDINoteOff packet represents a MIDI message for the note-off command.</body>
</comment>



</st-source>
