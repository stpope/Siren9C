How to compile and use the MIDI for Squeak primitives

Uncompress the portmidi-master.zip file or grab the latest from https://github.com/PortMidi/portmidi.

Put the 2 portmidi_Squeak.[ch] files found here in the pm_common folder of the portMIDI distribution.

Edit the file pm_common/CMakeLists.txt and add the line,

	${PMDIR}/pm_common/Squeak_MIDI.c

to the list of PM_LIB_PUBLIC_SRC around line 38 -- before ${PMDIR}/porttime/porttime.c, for example.

Follow the rest of the instructions, e.g.,

	cd portmidi-master
	mkdir build
	cd build
	cmake ..
	make

This *should* leave you with a compiled library with a name like libportmidi.2.0.3.dylib, or whatever they're called on your platform (DLL, so, etc.).

Take this file and put in the resources directory of your Squeak virtual machine, where the other plug-in bundles are found; on the Mac, that's Squeak6.0-64bit.app/Contents/Resources

Edit the Smalltalk file with the primitives, PortMIDILibrary.st, so that all the primitive files point to your library, e.g., change all the lines like,

	<cdecl: int32_t pm_close(uint32_t) module: 'libportmidi.2.0.3.dylib' >"

to put the name of your library in the place of 'libportmidi.2.0.3.dylib'

Now fire up Squeak, file in this file of the primitives and test them!

Start with

	PortMIDILibrary pm_count_devices
and then, assuming you've loaded Siren, try,

	PortMIDIPort listDeviceTableand check the Transcript.

stephen@heaveneverywhere.com - 230124
